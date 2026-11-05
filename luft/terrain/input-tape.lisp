(in-package #:luft.terrain)

;;; An input tape is one player's input: one INPUT-FRAME per simulation tick,
;;; and the other things the player did, such as edits, each placed before
;;; the frame of its tick.  Replayed from the same start, the frames walk the
;;; same walk; a bot or a soak replays a saved tape against a live world.
;;;
;;; On disk a tape is one readable form, its frames run-length coded:
;;;
;;;   (:luft-input-tape 1
;;;    :ticks 3
;;;    :frames ((2 1.0 0.0 0.0 0.0 0.0) (1 1.0 0.0 0.1 0.0 0.0 :jump))
;;;    :actions ((1 :edit 1030 1040 12 (:block :name :stone))))
;;;
;;; A run is (COUNT FORWARD RIGHT YAW-DELTA PITCH-DELTA VERTICAL . EDGES),
;;; EDGES among :JUMP and :FLIGHT; an action is (INDEX . DATA), before frame
;;; INDEX, with DATA a list of numbers, strings, keywords, and such lists.
;;; Numbers keep their exact type, so a replay steers by the same values.

(defconstant +input-tape-version+ 1)

(defstruct (input-tape (:constructor make-input-tape ()) (:copier nil))
  "One player's input frames, one per tick, and its ACTIONS between them:
(INDEX . DATA) in order, DATA done before frame INDEX."
  (frames (make-array 0 :adjustable t :fill-pointer t) :type vector)
  (actions (make-array 0 :adjustable t :fill-pointer t) :type vector))

(defun input-tape-length (tape)
  "How many ticks TAPE steers."
  (length (input-tape-frames tape)))

(defun input-tape-frame (tape index)
  (aref (input-tape-frames tape) index))

(defun record-input-frame (tape frame)
  "Append FRAME, the next tick's input, to TAPE."
  (vector-push-extend frame (input-tape-frames tape))
  frame)

(defun record-input-action (tape data)
  "Note DATA as done before TAPE's next frame."
  (vector-push-extend (cons (input-tape-length tape) data) (input-tape-actions tape))
  data)

(defun input-tape-actions-at (tape index)
  "The actions done before frame INDEX of TAPE, oldest first."
  (let* ((actions (input-tape-actions tape))
         (start (let ((low 0) (high (length actions)))
                  ;; The first action whose index is not below INDEX.
                  (loop while (< low high)
                        do (let ((middle (floor (+ low high) 2)))
                             (if (< (car (aref actions middle)) index)
                                 (setf low (1+ middle))
                                 (setf high middle))))
                  low)))
    (loop for position from start below (length actions)
          for (at . data) = (aref actions position)
          while (= at index)
          collect data)))

;;; The file.

(defun frame-run-key (frame)
  (list* (input-frame-forward frame) (input-frame-right frame)
         (input-frame-yaw-delta frame) (input-frame-pitch-delta frame)
         (input-frame-vertical frame)
         (append (and (input-frame-jump-p frame) '(:jump))
                 (and (input-frame-flight-p frame) '(:flight)))))

(defun input-tape-runs (tape)
  "TAPE's frames as runs of identical frames, (COUNT . KEY)."
  (let ((runs nil))
    (loop for frame across (input-tape-frames tape)
          for key = (frame-run-key frame)
          do (if (and runs (equal key (cdr (first runs))))
                 (incf (car (first runs)))
                 (push (cons 1 key) runs)))
    (nreverse runs)))

(defun write-input-tape (tape pathname)
  "Write TAPE to PATHNAME, replacing it, and return the pathname."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname :direction :output :if-exists :supersede
                                   :external-format :utf-8)
    (with-standard-io-syntax
      (let ((*read-default-float-format* 'single-float)
            (*print-readably* t))
        (format stream ";;;; A Luft input tape: one input frame per simulation tick.~%")
        (format stream "(:luft-input-tape ~D~% :ticks ~D~% :frames (" +input-tape-version+
                (input-tape-length tape))
        (loop for run in (input-tape-runs tape)
              for first = t then nil
              do (unless first (format stream "~%          "))
                 (prin1 run stream))
        (format stream ")~% :actions (")
        (loop for action across (input-tape-actions tape)
              for first = t then nil
              do (unless first (format stream "~%           "))
                 (prin1 action stream))
        (format stream "))~%"))))
  pathname)

(define-condition invalid-input-tape (error)
  ((pathname :initarg :pathname :reader invalid-input-tape-pathname)
   (reason :initarg :reason :reader invalid-input-tape-reason))
  (:report (lambda (condition stream)
             (format stream "~A is not a Luft input tape: ~A"
                     (invalid-input-tape-pathname condition)
                     (invalid-input-tape-reason condition)))))

(defun tape-datum-p (datum)
  "Whether DATUM is plain data an action may carry."
  (typecase datum
    (null t)
    (cons (and (tape-datum-p (car datum)) (tape-datum-p (cdr datum))))
    (keyword t)
    (symbol nil)
    (float (and (= datum datum) (< (abs datum) most-positive-double-float)))
    ((or rational string) t)
    (t nil)))

(defun finite-real-p (value)
  (and (realp value)
       (or (rationalp value)
           (and (= value value) (<= (abs value) most-positive-double-float)))))

(defun read-input-tape (pathname)
  "The tape written to PATHNAME, checked whole: anything else is refused
with an INVALID-INPUT-TAPE."
  (flet ((refuse (control &rest arguments)
           (error 'invalid-input-tape :pathname pathname
                                      :reason (apply #'format nil control arguments))))
    (let ((form (handler-case
                    (with-open-file (stream pathname :external-format :utf-8)
                      (with-standard-io-syntax
                        (let ((*read-eval* nil)
                              (*read-default-float-format* 'single-float)
                              (*package* (find-package '#:keyword)))
                          (prog1 (read stream)
                            (unless (eq :end (read stream nil :end))
                              (refuse "more than one form"))))))
                  (invalid-input-tape (condition) (error condition))
                  (error (condition) (refuse "~A" condition)))))
      (unless (and (consp form) (eq (first form) :luft-input-tape))
        (refuse "it does not begin with :LUFT-INPUT-TAPE"))
      (unless (eql (second form) +input-tape-version+)
        (refuse "version ~S, not ~D" (second form) +input-tape-version+))
      (destructuring-bind (&key ticks frames actions) (cddr form)
        (unless (typep ticks '(integer 0)) (refuse "its tick count is ~S" ticks))
        (let ((tape (make-input-tape)))
          (dolist (run frames)
            (unless (and (consp run) (typep (first run) '(integer 1))
                         (<= 6 (length run))
                         (every #'finite-real-p (subseq run 1 6))
                         (<= (abs (second run)) 1) (<= (abs (third run)) 1)
                         (<= (abs (sixth run)) 1)
                         (null (set-difference (nthcdr 6 run) '(:jump :flight))))
              (refuse "the frame run ~S" run))
            (destructuring-bind (count forward right yaw-delta pitch-delta vertical &rest edges) run
              (dotimes (i count)
                (record-input-frame
                 tape (make-input-frame :forward forward :right right
                                        :yaw-delta yaw-delta :pitch-delta pitch-delta
                                        :vertical vertical
                                        :jump-p (and (member :jump edges) t)
                                        :flight-p (and (member :flight edges) t))))))
          (unless (= ticks (input-tape-length tape))
            (refuse "it promises ~D ticks but its frames make ~D" ticks (input-tape-length tape)))
          (let ((last -1))
            (dolist (action actions)
              (unless (and (consp action) (typep (car action) '(integer 0))
                           (<= last (car action) ticks)
                           (consp (cdr action)) (tape-datum-p (cdr action)))
                (refuse "the action ~S" action))
              (setf last (car action))
              (vector-push-extend action (input-tape-actions tape))))
          tape)))))
