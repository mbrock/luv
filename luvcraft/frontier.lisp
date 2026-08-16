;;; Inspectable frontier programs over packed, domain-addressed work.

(in-package #:luvcraft.frontier)

(defclass frontier-program-definition ()
  ((name :initarg :name :reader frontier-program-definition-name)
   (family :initarg :family :reader frontier-program-definition-family)
   (frontier-layout
    :initarg :frontier-layout
    :reader frontier-program-definition-frontier-layout)
   (neighborhood
    :initarg :neighborhood
    :reader frontier-program-definition-neighborhood)
   (materialization
    :initarg :materialization
    :reader frontier-program-definition-materialization)
   (source-form
    :initarg :source-form
    :reader frontier-program-definition-source-form)
   (revision
    :initform (gensym "FRONTIER-PROGRAM-")
    :reader frontier-program-definition-revision))
  (:documentation
   "One live semantic account of frontier-shaped materialization work. #X7Q90E

The definition names the dynamic family, physical frontier layout,
neighborhood, and result materialization at an aggregate boundary.  Concrete
methods on EXECUTE-FRONTIER-PROGRAM lower that account to closed loops."))

(defgeneric frontier-program-definition-for (name)
  (:documentation "Return the live frontier program definition named by NAME."))

(defmethod frontier-program-definition-for (name)
  (declare (ignore name))
  nil)

(defmacro define-frontier-program
    (name &key family frontier-layout neighborhood materialization)
  "Define an inspectable frontier program at one EQL-specialized name."
  (let ((query (gensym "PROGRAM-NAME"))
        (source-form
          `(define-frontier-program ,name
             :family ,family
             :frontier-layout ,frontier-layout
             :neighborhood ,neighborhood
             :materialization ,materialization)))
    `(progn
       (defmethod frontier-program-definition-for ((,query (eql ',name)))
         (declare (ignore ,query))
         (load-time-value
          (make-instance
           'frontier-program-definition
           :name ',name
           :family ',family
           :frontier-layout ',frontier-layout
           :neighborhood ',neighborhood
           :materialization ',materialization
           :source-form ',source-form)))
       ',name)))

(defgeneric execute-frontier-program
    (program input &rest arguments &key &allow-other-keys)
  (:documentation
   "Execute PROGRAM over INPUT after selecting its concrete realization once."))

(records:define-columnar-buffer frontier-site-buffer
  (materialization nil :type t :clear-on-remove t)
  (offset 0 :type (unsigned-byte 32)))

(defclass bucket-frontier ()
  ((maximum-priority
    :initarg :maximum-priority
    :reader bucket-frontier-maximum-priority)
   (priority-meaning
    :initarg :priority-meaning
    :initform nil
    :reader bucket-frontier-priority-meaning)
   (buckets :initarg :buckets :reader bucket-frontier-buckets)
   (current-priority
    :initform -1
    :accessor bucket-frontier-current-priority)
   (count :initform 0 :accessor bucket-frontier-count)
   (pushes :initform 0 :accessor bucket-frontier-pushes)
   (pops :initform 0 :accessor bucket-frontier-pops)
   (peak-count :initform 0 :accessor bucket-frontier-peak-count))
  (:documentation
   "A finite highest-priority-first frontier of packed materialization sites.

Each bucket is a generated columnar buffer of aggregate identity plus dense
offset.  PRIORITY-MEANING retains the field or client declaration which gives
the otherwise raw bucket number its meaning."))

(defun make-bucket-frontier
    (&key maximum-priority priority-meaning (initial-capacity 256))
  (check-type maximum-priority (integer 0 #.most-positive-fixnum))
  (check-type initial-capacity (integer 0 #.most-positive-fixnum))
  (let ((buckets (make-array (1+ maximum-priority))))
    (dotimes (priority (length buckets))
      (setf (aref buckets priority)
            (make-frontier-site-buffer :capacity initial-capacity)))
    (make-instance
     'bucket-frontier
     :maximum-priority maximum-priority
     :priority-meaning priority-meaning
     :buckets buckets)))

(declaim (inline bucket-frontier-empty-p bucket-frontier-push))

(defun bucket-frontier-empty-p (frontier)
  (zerop (bucket-frontier-count frontier)))

(defun bucket-frontier-push (frontier materialization offset priority)
  "Admit one MATERIALIZATION/OFFSET site at finite PRIORITY."
  (check-type offset (unsigned-byte 32))
  (unless (typep priority
                 `(integer 0 ,(bucket-frontier-maximum-priority frontier)))
    (error "Frontier priority ~S is outside 0..~D."
           priority (bucket-frontier-maximum-priority frontier)))
  (frontier-site-buffer-push
   (aref (bucket-frontier-buckets frontier) priority)
   materialization offset)
  (incf (bucket-frontier-count frontier))
  (incf (bucket-frontier-pushes frontier))
  (setf (bucket-frontier-current-priority frontier)
        (max priority (bucket-frontier-current-priority frontier))
        (bucket-frontier-peak-count frontier)
        (max (bucket-frontier-count frontier)
             (bucket-frontier-peak-count frontier)))
  frontier)

(defun bucket-frontier-pop (frontier)
  "Return MATERIALIZATION, OFFSET, PRIORITY, and PRESENT-P for the next site."
  (when (bucket-frontier-empty-p frontier)
    (return-from bucket-frontier-pop (values nil nil nil nil)))
  (let* ((priority (bucket-frontier-current-priority frontier))
         (bucket (aref (bucket-frontier-buckets frontier) priority)))
    (multiple-value-bind (materialization offset present-p)
        (frontier-site-buffer-pop bucket)
      (unless present-p
        (error "Frontier count disagrees with priority bucket ~D." priority))
      (decf (bucket-frontier-count frontier))
      (incf (bucket-frontier-pops frontier))
      (when (zerop (frontier-site-buffer-length bucket))
        (loop for candidate downfrom (1- priority) to 0
              when (plusp
                    (frontier-site-buffer-length
                     (aref (bucket-frontier-buckets frontier) candidate)))
                do (setf (bucket-frontier-current-priority frontier) candidate)
                   (return)
              finally (setf (bucket-frontier-current-priority frontier) -1)))
      (values materialization offset priority t))))

(defclass frontier-execution ()
  ((program :initarg :program :reader frontier-execution-program)
   (input :initarg :input :reader frontier-execution-input)
   (frontier :initarg :frontier :reader frontier-execution-frontier)
   (visits :initform 0 :accessor frontier-execution-visits)
   (relations :initform 0 :accessor frontier-execution-relations)
   (admissions :initform 0 :accessor frontier-execution-admissions)
   (crossings :initform 0 :accessor frontier-execution-crossings)
   (unavailable :initform 0 :accessor frontier-execution-unavailable)
   (elapsed-seconds
    :initform 0d0
    :type double-float
    :accessor frontier-execution-elapsed-seconds))
  (:documentation
   "Inspectable work and timing evidence from one frontier execution."))

(defun make-frontier-execution (program input frontier)
  (make-instance
   'frontier-execution
   :program (or (frontier-program-definition-for program) program)
   :input input
   :frontier frontier))

(defun admit-frontier-site
    (execution materialization offset priority)
  "Record one admitted relation and push its destination site."
  (incf (frontier-execution-admissions execution))
  (bucket-frontier-push
   (frontier-execution-frontier execution)
   materialization offset priority))

(defmacro do-voxel-frontier-relations
    ((source source-offset priority
      target target-offset direction destination crossing availability
      frontier window domain directions
      &key execution result)
     &body body)
  "Drain FRONTIER and execute BODY once for every spatial relation it exposes.

SOURCE is a retained aggregate materialization and SOURCE-OFFSET is its dense
site identity.  TARGET is the local SOURCE or a materialization selected by
WINDOW at a crossing.  DESTINATION has dynamic extent.  The client body owns
admission, transfer, mutation, and unavailable-neighbor semantics; the macro
keeps traversal and value lifetimes visible to the compiler. #X7Q90E"
  (let ((present-p (gensym "PRESENT-P"))
        (local (gensym "LOCAL"))
        (resolved (gensym "RESOLVED"))
        (execution-value (gensym "EXECUTION")))
    `(let ((,execution-value ,execution))
       (loop until (bucket-frontier-empty-p ,frontier)
             do (multiple-value-bind
                    (,source ,source-offset ,priority ,present-p)
                    (bucket-frontier-pop ,frontier)
                  (declare (ignore ,present-p))
                  (let ((,local
                          (chunk-domain-local-coordinate
                           ,domain ,source-offset)))
                    (declare (dynamic-extent ,local))
                    (when ,execution-value
                      (incf (frontier-execution-visits ,execution-value)))
                    (do-chunk-window-neighbors
                        (,target-offset ,destination ,crossing ,direction
                         ,resolved ,availability
                         ,window ,domain ,local ,directions)
                      (when ,execution-value
                        (incf (frontier-execution-relations ,execution-value))
                        (when ,crossing
                          (incf (frontier-execution-crossings ,execution-value)))
                        (when (eq ,availability :unavailable)
                          (incf
                           (frontier-execution-unavailable ,execution-value))))
                      (let ((,target
                              (ecase ,availability
                                (:local ,source)
                                (:available ,resolved)
                                (:unavailable nil))))
                        ,@body))))
             finally (return ,result)))))
