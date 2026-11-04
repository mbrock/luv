(in-package #:luft.terrain)

;;; The sequencer gives a proposed change its place in a scene's journal
;;; (#3F0SCZ).  It checks every change's BEFORE against the value the site
;;; has once every sequenced event is applied -- the journal's head -- and
;;; records the whole transaction or none of it.  A recorded event has
;;; happened: the scene queues it and applies it at the next frame boundary
;;; with no cohort in flight, so it is never refused (#QQT86P).
;;;
;;; Undo belongs to an actor.  It proposes the inverse of that actor's own
;;; latest transaction, whose BEFORE values are the original AFTER values, so
;;; it applies only while nobody has changed those sites since.

(defun world-site-resident-chunk (scene site)
  "SITE's resident source chunk in SCENE, or NIL."
  (let ((value (gethash (luft:site-chunk-key site) (streaming-scene-store scene))))
    (and (resident-cell-chunk-p value) value)))

(defun world-cell-value (scene site)
  "SITE's value at SCENE's journal head, and whether it is known.

An authored site has its latest event's value.  Any other site still has its
baseline, which a finite scene always knows and a source scene knows only
while SITE's chunk is resident."
  (multiple-value-bind (value present-p)
      (gethash site (world-journal-edits (streaming-scene-journal scene)))
    (cond
      (present-p (values value t))
      ((null (streaming-scene-source scene))
       (values (scene-material-placement-at scene site) t))
      (t
       (let ((resident (world-site-resident-chunk scene site)))
         (if resident
             (multiple-value-bind (offset occupied-p)
                 (gethash site (resident-cell-chunk-material-cells resident))
               (values (and occupied-p
                            (domains:identity-vocabulary-member
                             (scene-material-vocabulary scene) offset))
                       t))
             (values nil nil)))))))

(defun world-visible-value (scene site)
  "The value SCENE shows at SITE now: before any of its queued events there."
  (let ((pending (find site (streaming-scene-pending-events scene)
                       :key #'world-event-site)))
    (if pending
        (values (world-event-before pending) t)
        (world-cell-value scene site))))

(defun world-attachment-conflict-p (scene site)
  "Whether changing SITE would disturb a surface attachment, made or queued."
  (or (scene-edit-attachment-conflict-p scene site)
      (let ((resident (world-site-resident-chunk scene site)))
        (and resident
             (some (lambda (attachment)
                     (or (= site (surface-attachment-support-cell attachment))
                         (= site (surface-attachment-clearance-cell attachment))))
                   (resident-cell-chunk-attachments resident))))
      ;; A queued fixture is not an attachment yet, but will be one.
      (let ((domain (scene-domain scene)))
        (some (lambda (event)
                (let ((placement (world-value-placement (world-event-after event)))
                      (fixture (world-event-site event)))
                  (and placement
                       (material-kind-attachment-form
                        (material-placement-kind placement))
                       (or (= site fixture)
                           (eql site (luft:step-site domain fixture :z -1))
                           (eql site (luft:step-site domain fixture :z 1))))))
              (streaming-scene-pending-events scene)))))

(defun world-change-refusal (scene site before after head known-p)
  "Why SCENE refuses changing SITE from BEFORE to AFTER at value HEAD, or NIL."
  (cond
    ((not known-p) :nonresident)
    ((eql before after) :unchanged)
    ((not (eql before head)) :stale-before)
    ((eq after :unrecorded) :unrecorded)
    ((let ((placement (world-value-placement after)))
       (and placement
            (null (domains:identity-vocabulary-offset
                   (scene-material-vocabulary scene) placement nil))))
     :unknown-material)
    ((world-attachment-conflict-p scene site) :attachment)))

(defun sequence-world-events (scene changes &key actor tick cause)
  "Check and record CHANGES, each (SITE BEFORE AFTER), as one transaction.

Return the recorded events and :SEQUENCED; the scene applies them at its next
frame boundary.  Otherwise record nothing and return NIL, the reason, and the
refused change.  A later change in CHANGES sees the earlier ones' values."
  (check-type scene streaming-scene)
  (let ((domain (scene-domain scene))
        (proposed (make-hash-table :test #'eql)))
    (dolist (change changes)
      (destructuring-bind (site before after) change
        (luft:checked-site domain site)
        (unless (and (= (luft:site-extent site) luft:+cell-extent+)
                     (luft:site-positive-p site))
          (error "A world event needs one positive cell in the scene domain, not ~S."
                 site))
        (let ((refusal
                (multiple-value-bind (head known-p)
                    (multiple-value-bind (value present-p) (gethash site proposed)
                      (if present-p
                          (values value t)
                          (world-cell-value scene site)))
                  (world-change-refusal scene site before after head known-p))))
          (when refusal
            (return-from sequence-world-events (values nil refusal change))))
        (setf (gethash site proposed) after)))
    (let ((events (record-world-events (streaming-scene-journal scene) changes
                                       :actor actor :tick tick :cause cause)))
      (setf (streaming-scene-pending-events scene)
            (append (streaming-scene-pending-events scene) events))
      (values events :sequenced))))

(defun apply-world-event-to-scene (scene production-system event)
  "Materialize EVENT in SCENE; return its chunk key when the scene changed there."
  (let* ((site (world-event-site event))
         (key (luft:site-chunk-key site))
         (before (world-value-placement (world-event-before event)))
         (after (world-value-placement (world-event-after event))))
    (cond
      ((and (streaming-scene-source scene)
            (null (gethash key (material-store-chunks
                                (scene-material-cells scene)))))
       ;; Not visible.  The journal reaches the chunk whenever it is next
       ;; materialized, but a value resident or loading now predates EVENT.
       (let ((token (gethash key (streaming-scene-desired scene))))
         (when (and token production-system
                    (or (world-site-resident-chunk scene site)
                        (gethash key (streaming-scene-load-outstanding scene))))
           (schedule-authored-world-chunk-load
            scene production-system key token 0)))
       nil)
      (t
       (let ((current (scene-material-placement-at scene site)))
         (cond
           ;; A chunk materialized after EVENT was sequenced already has it.
           ((eq current after) nil)
           ((eq current before)
            (flet ((edit (placement)
                     (let ((status (nth-value 1 (edit-streaming-scene-cell
                                                 scene site placement))))
                       (unless (eq status :edited)
                         (error "Sequenced event ~S could not be applied: ~S."
                                event status)))))
              (when (and before after) (edit nil))
              (edit after))
            key)
           (t (error "Scene shows ~S at ~S, but sequenced event ~S expects ~S."
                     current site event before))))))))

(defun apply-queued-world-events (scene production-system bevel-width)
  "At a frame boundary, apply SCENE's queued events and remesh them as one cohort.

While a cohort is in flight the events wait, and this returns NIL; otherwise
it returns how many were applied.  With no PRODUCTION-SYSTEM the caller
meshes the changed chunks."
  (when (and (streaming-scene-pending-events scene)
             (null (streaming-scene-replacement scene)))
    (let ((changed nil)
          (count 0))
      (loop while (streaming-scene-pending-events scene)
            do (let ((key (apply-world-event-to-scene
                           scene production-system
                           (first (streaming-scene-pending-events scene)))))
                 (when key (pushnew key changed))
                 (pop (streaming-scene-pending-events scene))
                 (incf count)))
      (when (and changed production-system)
        (schedule-streaming-scene-edit
         scene production-system (sort changed #'<) bevel-width))
      count)))

(defun settle-world-events (scene events production-system bevel-width)
  "Apply the queue now if it may, and say whether EVENTS were :EDITED or :QUEUED."
  (apply-queued-world-events scene production-system bevel-width)
  (if (intersection events (streaming-scene-pending-events scene))
      :queued
      :edited))

(defun edit-world-cell
    (scene site after &key actor tick (cause :edit) production-system bevel-width)
  "Sequence ACTOR's change of SITE to AFTER from the value SCENE shows there.

Return the event and :EDITED once applied, or :QUEUED while a cohort is in
flight; otherwise NIL and the refusal, such as :STALE-BEFORE when a queued
event has already changed what ACTOR saw."
  (multiple-value-bind (before known-p) (world-visible-value scene site)
    (multiple-value-bind (events status)
        (if known-p
            (sequence-world-events scene (list (list site before after))
                                   :actor actor :tick tick :cause cause)
            (values nil :nonresident))
      (if events
          (values (first events)
                  (settle-world-events scene events production-system bevel-width))
          (values nil status)))))

(defun repeat-world-transaction
    (scene actor tick transaction cause changes production-system bevel-width)
  (multiple-value-bind (events status)
      (sequence-world-events scene changes :actor actor :tick tick :cause cause)
    (cond
      (events
       (values events
               (settle-world-events scene events production-system bevel-width)))
      (t
       ;; Someone else has changed these sites since: this can never apply.
       (when (eq status :stale-before)
         (forget-world-history (streaming-scene-journal scene) actor transaction))
       (values nil status)))))

(defun undo-world-events (scene actor &key tick production-system bevel-width)
  "Append the inverse of ACTOR's latest transaction while its after values hold.

Return the inverse events and :EDITED or :QUEUED; NIL and :NOTHING when ACTOR
has nothing to undo; or NIL and the refusal.  A transaction made stale by
someone else's later edit leaves ACTOR's history."
  (let* ((journal (streaming-scene-journal scene))
         (transaction (world-journal-undoable-transaction journal actor)))
    (if transaction
        (repeat-world-transaction
         scene actor tick transaction (list :undo transaction)
         (loop for event in (reverse (world-journal-transaction-events
                                      journal transaction))
               collect (list (world-event-site event) (world-event-after event)
                             (world-event-before event)))
         production-system bevel-width)
        (values nil :nothing))))

(defun redo-world-events (scene actor &key tick production-system bevel-width)
  "Repeat ACTOR's latest undone transaction while its before values hold again."
  (let* ((journal (streaming-scene-journal scene))
         (transaction (world-journal-redoable-transaction journal actor)))
    (if transaction
        (repeat-world-transaction
         scene actor tick transaction (list :redo transaction)
         (loop for event in (world-journal-transaction-events journal transaction)
               collect (list (world-event-site event) (world-event-before event)
                             (world-event-after event)))
         production-system bevel-width)
        (values nil :nothing))))
