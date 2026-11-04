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
;;; source replays over its generator, and each actor's undo history.

(defstruct (world-event
             (:constructor %make-world-event
                 (sequence tick actor transaction site before after cause))
             (:copier nil))
  "One authored cell change in its place in a world journal."
  (sequence 0 :type (integer 1 *) :read-only t)
  (tick nil :read-only t)
  (actor nil :read-only t)
  (transaction 0 :type (integer 1 *) :read-only t)
  (site 0 :type luft:site :read-only t)
  (before nil :read-only t)
  (after nil :read-only t)
  (cause nil :read-only t))

(defgeneric world-value-placement (value)
  (:documentation
   "The material placement that paints authored VALUE, or NIL for an empty cell.")
  (:method ((value null)) nil)
  (:method ((value material-placement)) value))

(defclass world-journal ()
  ((events :initform (make-array 16 :adjustable t :fill-pointer 0)
           :reader world-journal-events)
   (next-transaction :initform 1 :accessor world-journal-next-transaction)
   ;; Presence is meaningful: NIL is an authored removal, while absence means
   ;; that no event has touched the site and its baseline still owns it.
   (edits :initform (make-hash-table :test #'eql) :reader world-journal-edits)
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
  "The sequence number of JOURNAL's latest event, zero when it has none."
  (fill-pointer (world-journal-events journal)))

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

(defun record-world-events (journal changes &key actor tick cause)
  "Append CHANGES, each a list (SITE BEFORE AFTER), as one transaction.

Nothing is checked: the sequencer checks a proposal before it records one,
and an import or a peer's already-sequenced events are recorded as they
are.  Return the new events in order."
  (unless changes
    (error "A world transaction needs at least one change."))
  (let ((transaction (world-journal-next-transaction journal))
        (events nil))
    (incf (world-journal-next-transaction journal))
    (loop for (site before after) in changes
          for event = (%make-world-event
                       (1+ (world-journal-head journal)) tick actor
                       transaction site before after cause)
          do (vector-push-extend event (world-journal-events journal))
             (setf (gethash site (world-journal-edits journal)) after)
             (push event events))
    (setf events (nreverse events)
          (gethash transaction (world-journal-transactions journal)) events)
    (note-world-transaction journal actor transaction cause)
    events))
