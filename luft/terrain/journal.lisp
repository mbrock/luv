(in-package #:luft.terrain)

;;; One journal of events is the authority for authored change (#3F0SCZ).
;;; An event says that at SEQUENCE, in TICK, ACTOR's TRANSACTION changed SITE
;;; from BEFORE to AFTER, because of CAUSE (#OVEP9A).  A value is NIL for an
;;; empty cell or anything WORLD-VALUE-PLACEMENT can paint: today a material
;;; placement, later a block identity (#FBU3TF).  Values are compared with
;;; EQL, so a value with more inside it must be interned.  An import whose
;;; format kept no prior value records its before as :UNRECORDED, which
;;; claims no earlier state, and which no inverse can restore.
;;;
;;; The journal only records.  Checking a proposal against the world, and
;;; applying recorded events to a scene, is the sequencer's work
;;; (sequencer.lisp).  What the journal keeps besides its events are views
;;; derived from them: the latest value at every authored site, which a world
;;; source replays over its generator, who authored it and when, and each
;;; actor's undo history.
;;;
;;; A save keeps the journal as an edit set compacted at a watermark and the
;;; tail of events after it (#4D3QFJ).  Compaction forgets the events, so
;;; undo reaches only into the tail, but not who last authored each cell:
;;; the compacted set keeps that as its provenance (#OVEP9A).

(defstruct (world-event
             (:constructor %make-world-event
                 (sequence tick actor transaction site before after cause
                  &optional (time (get-universal-time))))
             (:constructor make-world-event
                 (&key sequence tick actor transaction site before after cause time))
             (:copier nil))
  "One authored cell change in its place in a world journal.
TIME is the universal time it was recorded here."
  (sequence 0 :type (integer 1 *) :read-only t)
  (tick nil :read-only t)
  (actor nil :read-only t)
  (transaction 0 :type (integer 1 *) :read-only t)
  (site 0 :type luft:site :read-only t)
  (before nil :read-only t)
  (after nil :read-only t)
  (cause nil :read-only t)
  (time nil :read-only t))

(defstruct (cell-provenance
             (:constructor make-cell-provenance (&key sequence actor tick time))
             (:copier nil))
  "Who last authored a cell, and when: the event at SEQUENCE, by ACTOR, at
simulation TICK and universal TIME, either of which may be unknown."
  (sequence nil :read-only t)
  (actor nil :read-only t)
  (tick nil :read-only t)
  (time nil :read-only t))

(defun world-event-provenance (event)
  (make-cell-provenance :sequence (world-event-sequence event)
                        :actor (world-event-actor event)
                        :tick (world-event-tick event)
                        :time (world-event-time event)))

(defgeneric world-value-placement (value)
  (:documentation
   "The material placement that paints authored VALUE, or NIL for an empty cell.")
  (:method ((value null)) nil)
  (:method ((value material-placement)) value))

(defclass world-journal ()
  ((watermark :initform 0 :reader world-journal-watermark
              :documentation "The sequence its installed edit set is compacted at.")
   ;; The events after WATERMARK, in sequence order.
   (events :initform (make-array 16 :adjustable t :fill-pointer 0)
           :reader world-journal-events)
   (next-transaction :initform 1 :accessor world-journal-next-transaction)
   ;; Presence is meaningful: NIL is an authored removal, while absence means
   ;; that no event has touched the site and its baseline still owns it.
   (edits :initform (make-hash-table :test #'eql) :reader world-journal-edits)
   ;; Site -> its CELL-PROVENANCE, for every authored site whose author is
   ;; known: a disposable materialization of the events (#OVEP9A).
   (provenance :initform (make-hash-table :test #'eql)
               :reader world-journal-provenance)
   ;; Site -> (VALUE . PROVENANCE) as installed at WATERMARK, from which the
   ;; edit set at any later watermark is replayed.
   (base :initform (make-hash-table :test #'eql) :reader world-journal-base)
   (transactions :initform (make-hash-table :test #'eql)
                 :reader world-journal-transactions)
   ;; Actor -> (undoable . redoable) transaction ids, newest first.
   (histories :initform (make-hash-table :test #'equal)
              :reader world-journal-histories))
  (:documentation
   "An append-only sequence of world events and the views derived from it."))

(defun make-world-journal ()
  (make-instance 'world-journal))

(defun world-journal-head (journal)
  "The sequence number of JOURNAL's latest event, or its watermark before any."
  (+ (world-journal-watermark journal)
     (fill-pointer (world-journal-events journal))))

(defun install-world-edit-set (journal edits watermark)
  "Start empty JOURNAL from EDITS, a list of (SITE VALUE [PROVENANCE]),
compacted at WATERMARK.

This is what a save or a host's catch-up hands over (#8DQ18B): the world as
of sequence WATERMARK, with no events of its own.  A PROVENANCE, a
CELL-PROVENANCE, says who authored SITE's VALUE.  Events recorded next
follow WATERMARK; a save's tail of them is replayed by REPLAY-WORLD-EVENTS."
  (unless (and (zerop (world-journal-head journal))
               (zerop (hash-table-count (world-journal-edits journal))))
    (error "~S already has events or edits; install an edit set first." journal))
  (check-type watermark (integer 0 *))
  (loop for (site value provenance) in edits
        do (setf (gethash site (world-journal-edits journal)) value
                 (gethash site (world-journal-base journal)) (cons value provenance))
           (when provenance
             (setf (gethash site (world-journal-provenance journal)) provenance)))
  (setf (slot-value journal 'watermark) watermark)
  journal)

(defun world-cell-provenance (journal site)
  "The CELL-PROVENANCE of SITE's latest value in JOURNAL, or NIL when it is
not authored or its author is unknown."
  (values (gethash site (world-journal-provenance journal))))

(defun world-journal-transaction-events (journal transaction)
  "TRANSACTION's events in sequence order."
  (gethash transaction (world-journal-transactions journal)))

(defun world-actor-history (journal actor)
  (or (gethash actor (world-journal-histories journal))
      (setf (gethash actor (world-journal-histories journal))
            (cons nil nil))))

(defun world-journal-undoable-transaction (journal actor)
  "The id of ACTOR's latest transaction that undo would invert, or NIL."
  (first (car (world-actor-history journal actor))))

(defun world-journal-redoable-transaction (journal actor)
  "The id of ACTOR's latest undone transaction that redo would repeat, or NIL."
  (first (cdr (world-actor-history journal actor))))

(defun forget-world-history (journal actor transaction)
  "Drop TRANSACTION from ACTOR's undo and redo history; no event changes."
  (let ((history (world-actor-history journal actor)))
    (setf (car history) (remove transaction (car history))
          (cdr history) (remove transaction (cdr history)))))

(defun note-world-transaction (journal actor transaction cause)
  "Advance ACTOR's history for its TRANSACTION.

(:UNDO T) moves T to the redoable stack and (:REDO T) back; any other cause
is new work, which makes T undoable and forgets what was redoable."
  (let ((history (world-actor-history journal actor))
        (target (and (consp cause) (second cause))))
    (case (and (consp cause) (first cause))
      (:undo (setf (car history) (remove target (car history)))
             (push target (cdr history)))
      (:redo (setf (cdr history) (remove target (cdr history)))
             (push target (car history)))
      (t (push transaction (car history))
         (setf (cdr history) nil)))))

(defun append-world-event (journal event)
  (let ((site (world-event-site event)))
    (vector-push-extend event (world-journal-events journal))
    (setf (gethash site (world-journal-edits journal)) (world-event-after event)
          (gethash site (world-journal-provenance journal)) (world-event-provenance event))))

(defun record-world-events (journal changes &key actor tick cause)
  "Append CHANGES, each a list (SITE BEFORE AFTER), as one transaction.

Nothing is checked: the sequencer checks a proposal before it records one,
and an import or a peer's already-sequenced events are recorded as they
are.  Return the new events in order."
  (unless changes
    (error "A world transaction needs at least one change."))
  (let ((transaction (world-journal-next-transaction journal))
        (time (get-universal-time))
        (events nil))
    (incf (world-journal-next-transaction journal))
    (loop for (site before after) in changes
          for event = (%make-world-event
                       (1+ (world-journal-head journal)) tick actor
                       transaction site before after cause time)
          do (append-world-event journal event)
             (push event events))
    (setf events (nreverse events)
          (gethash transaction (world-journal-transactions journal)) events)
    (note-world-transaction journal actor transaction cause)
    events))

(defun replay-world-events (journal events)
  "Append EVENTS, sequenced and recorded elsewhere, exactly as they are: a
save's tail after the edit set JOURNAL was installed from.

They must continue JOURNAL's head without a gap, a transaction's events
together.  Transaction ids and causes are kept, so undo and redo find what
they name; history naming a transaction compacted away is dropped."
  (let ((journal-transactions (world-journal-transactions journal))
        (batch nil))
    (flet ((finish-batch ()
             (when batch
               (let* ((events (nreverse batch))
                      (first (first events))
                      (transaction (world-event-transaction first)))
                 (when (gethash transaction journal-transactions)
                   (error "Transaction ~D appears twice in a replayed tail." transaction))
                 (setf (gethash transaction journal-transactions) events
                       (world-journal-next-transaction journal)
                       (max (world-journal-next-transaction journal) (1+ transaction)))
                 (note-world-transaction journal (world-event-actor first) transaction
                                         (world-event-cause first)))
               (setf batch nil))))
      (dolist (event events)
        (unless (= (world-event-sequence event) (1+ (world-journal-head journal)))
          (error "Replayed event ~D does not follow head ~D."
                 (world-event-sequence event) (world-journal-head journal)))
        (when (and batch (/= (world-event-transaction event)
                             (world-event-transaction (first batch))))
          (finish-batch))
        (append-world-event journal event)
        (push event batch))
      (finish-batch))
    (maphash (lambda (actor history)
               (declare (ignore actor))
               (flet ((here (transactions)
                        (remove-if-not (lambda (transaction)
                                         (gethash transaction journal-transactions))
                                       transactions)))
                 (setf (car history) (here (car history))
                       (cdr history) (here (cdr history)))))
             (world-journal-histories journal))
    journal))

;;; Compaction.  A save keeps at most *JOURNAL-TAIL-EVENTS* of the newest
;;; events, cut at a transaction boundary so no transaction is half kept.

(defparameter *journal-tail-events* 4096
  "The most recent events a saved journal keeps after its compacted edit set.")

(defun world-journal-compaction-watermark (journal &optional (limit *journal-tail-events*))
  "The sequence to compact JOURNAL's edit set at: the earliest transaction
boundary that leaves at most LIMIT of its events after it.  Never before
its own watermark, since earlier events are gone already."
  (let* ((events (world-journal-events journal))
         (count (length events))
         (watermark (world-journal-watermark journal)))
    (loop for index from (max 0 (- count limit)) below count
          when (or (zerop index)
                   (/= (world-event-transaction (aref events index))
                       (world-event-transaction (aref events (1- index)))))
            return (+ watermark index)
          finally (return (+ watermark count)))))

(defun world-journal-compacted (journal watermark)
  "JOURNAL as a save keeps it compacted at WATERMARK: the edit set then, a
hash table of site -> value; the provenance then, site -> CELL-PROVENANCE;
and the list of events after WATERMARK."
  (let ((edits (make-hash-table :test #'eql))
        (provenance (make-hash-table :test #'eql))
        (events (world-journal-events journal))
        (offset (- watermark (world-journal-watermark journal))))
    (unless (<= 0 offset (length events))
      (error "~S cannot be compacted at ~D; it holds ~D through ~D." journal watermark
             (world-journal-watermark journal) (world-journal-head journal)))
    (maphash (lambda (site entry)
               (setf (gethash site edits) (car entry))
               (when (cdr entry)
                 (setf (gethash site provenance) (cdr entry))))
             (world-journal-base journal))
    (loop for index below offset
          for event = (aref events index)
          do (setf (gethash (world-event-site event) edits) (world-event-after event)
                   (gethash (world-event-site event) provenance)
                   (world-event-provenance event)))
    (values edits provenance (coerce (subseq events offset) 'list))))
