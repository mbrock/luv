;;; Inspectable frontier programs over packed, domain-addressed work, and the
;;; compiler which closes one program over bound fields into a scalar loop.
;;;
;;; Two languages meet here.  The frontier language owns work-generating
;;; effects: pop a site, expose its relations, read and commit fields, admit
;;; another site, count evidence.  The arithmetic language owns pure value
;;; laws: transfer, admission tests, and priorities, quantity-checked against
;;; the fields they read.  A FRONTIER-PROGRAM-DEFINITION is the open,
;;; redefinable source; a FRONTIER-REALIZATION is one compilation of it over
;;; particular field bindings, retaining the checked expressions, the emitted
;;; form, and the compiled function. #53Q1II

(in-package #:luvcraft.frontier)

;;; ------------------------------------------------------------------------
;;; Program definitions

(defun frontier-word-p (form name)
  "Whether FORM is the frontier language word NAME, in any package.

Programs and bindings are written in their client packages, so the language
recognizes its few reserved words by name rather than by symbol identity."
  (and (symbolp form) form (not (keywordp form)) (string= form name)))

(defclass frontier-field-role ()
  ((name :initarg :name :reader frontier-field-role-name)
   (relaxed-p :initarg :relaxed-p :initform nil
              :reader frontier-field-role-relaxed-p)
   (memo-p :initarg :memo-p :initform nil :reader frontier-field-role-memo-p)
   (invalidated-p :initarg :invalidated-p :initform nil
                  :reader frontier-field-role-invalidated-p)
   (source-form :initarg :source-form :reader frontier-field-role-source-form))
  (:documentation
   "One field a program reads or writes by role name.  RELAXED-P marks the
best-known-value field of a monotone program; MEMO-P marks the visited
identity of a discover-once program; INVALIDATED-P marks the field an
invalidation program clears, whose source value is the level a cleared site
had.  Storage is bound at realization."))

(defclass frontier-relation-predicate ()
  ((name :initarg :name :reader frontier-relation-predicate-name)
   (kind :initarg :kind :reader frontier-relation-predicate-kind)
   (argument :initarg :argument :initform nil
             :reader frontier-relation-predicate-argument)
   (source-form :initarg :source-form
                :reader frontier-relation-predicate-source-form))
  (:documentation
   "A raw truth value about the relation being exposed: (DIRECTION= constant)
or CROSSING.  Predicates enter arithmetic laws as unchecked flags."))

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
   (fields :initarg :fields :initform nil
           :reader frontier-program-definition-fields)
   (constants :initarg :constants :initform nil
              :reader frontier-program-definition-constants)
   (predicates :initarg :predicates :initform nil
               :reader frontier-program-definition-predicates)
   (transfer :initarg :transfer :initform nil
             :reader frontier-program-definition-transfer)
   (admission :initarg :admission :initform nil
              :reader frontier-program-definition-admission)
   (priority :initarg :priority :initform nil
             :reader frontier-program-definition-priority)
   (retain-admissions-p :initarg :retain-admissions-p :initform nil
                        :reader frontier-program-definition-retain-admissions-p)
   (source-form
    :initarg :source-form
    :reader frontier-program-definition-source-form)
   (revision
    :initform (gensym "FRONTIER-PROGRAM-")
    :reader frontier-program-definition-revision))
  (:documentation
   "One live semantic account of frontier-shaped materialization work. #X7Q90E

The definition names the dynamic family, physical frontier layout,
neighborhood, and result materialization at an aggregate boundary, and states
its local law: field roles, realization constants, relation predicates, and
the arithmetic TRANSFER, ADMISSION, and PRIORITY forms over them.  Concrete
methods on EXECUTE-FRONTIER-PROGRAM lower that account by hand;
COMPILE-FRONTIER-PROGRAM lowers it mechanically over bound fields."))

(defgeneric frontier-program-definition-for (name)
  (:documentation "Return the live frontier program definition named by NAME."))

(defmethod frontier-program-definition-for (name)
  (declare (ignore name))
  nil)

(defun parse-frontier-field-role (form)
  (destructuring-bind (name &key relaxed memo invalidated)
      (if (consp form) form (list form))
    (check-type name symbol)
    (make-instance 'frontier-field-role
                   :name name :relaxed-p relaxed :memo-p memo
                   :invalidated-p invalidated
                   :source-form form)))

(defun parse-frontier-relation-predicate (form)
  (destructuring-bind (name definition) form
    (check-type name symbol)
    (cond ((frontier-word-p definition "CROSSING")
           (make-instance 'frontier-relation-predicate
                          :name name :kind :crossing :source-form form))
          ((and (consp definition)
                (frontier-word-p (first definition) "DIRECTION=")
                (= (length definition) 2) (symbolp (second definition)))
           (make-instance 'frontier-relation-predicate
                          :name name :kind :direction=
                          :argument (second definition) :source-form form))
          (t (error "Unknown frontier relation predicate ~S." form)))))

(defun make-frontier-program-definition
    (name &key family frontier-layout neighborhood materialization
            fields constants predicates transfer admission priority
            retain-admissions source-form)
  (let ((definition
          (make-instance
           'frontier-program-definition
           :name name
           :family family
           :frontier-layout frontier-layout
           :neighborhood neighborhood
           :materialization materialization
           :fields (mapcar #'parse-frontier-field-role fields)
           :constants constants
           :predicates (mapcar #'parse-frontier-relation-predicate predicates)
           :transfer transfer
           :admission admission
           :priority priority
           :retain-admissions-p retain-admissions
           :source-form source-form)))
    (frontier-family-check-definition family definition)
    definition))

(defmacro define-frontier-program
    (name &rest options
     &key family frontier-layout neighborhood materialization
       fields constants predicates transfer admission priority
       retain-admissions)
  "Define an inspectable frontier program at one EQL-specialized name."
  (declare (ignore family frontier-layout neighborhood materialization
                   fields constants predicates transfer admission priority
                   retain-admissions))
  (let ((query (gensym "PROGRAM-NAME"))
        (source-form `(define-frontier-program ,name ,@options)))
    `(progn
       (defmethod frontier-program-definition-for ((,query (eql ',name)))
         (declare (ignore ,query))
         (load-time-value
          (make-frontier-program-definition
           ',name ,@(loop for (key value) on options by #'cddr
                          append (list key `',value))
           :source-form ',source-form)))
       (note-frontier-program-redefinition ',name)
       ',name)))

(defgeneric note-frontier-program-redefinition (name)
  (:documentation "Notify retained realizations that program NAME changed."))

(defmethod note-frontier-program-redefinition (name)
  (declare (ignore name))
  nil)

(defgeneric execute-frontier-program
    (program input &rest arguments &key &allow-other-keys)
  (:documentation
   "Execute PROGRAM over INPUT after selecting its concrete realization once."))

(defun frontier-program-relaxed-field (definition)
  (find-if #'frontier-field-role-relaxed-p
           (frontier-program-definition-fields definition)))

(defun frontier-program-memo-field (definition)
  (find-if #'frontier-field-role-memo-p
           (frontier-program-definition-fields definition)))

(defun frontier-program-invalidated-field (definition)
  (find-if #'frontier-field-role-invalidated-p
           (frontier-program-definition-fields definition)))

;;; Families are the semantic dynamics of #DURBKN.  Each family knows which
;;; roles a program must declare and what its default admission, commit, and
;;; priority laws are.  A definition is checked against its family at
;;; definition time so a wrong program fails before any realization.

(defgeneric frontier-family-check-definition (family definition)
  (:documentation "Signal an error unless DEFINITION is well formed for FAMILY."))

(defmethod frontier-family-check-definition (family definition)
  (error "Unknown frontier program family ~S in ~S."
         family (frontier-program-definition-name definition)))

(defmethod frontier-family-check-definition
    ((family (eql :monotone-max-fixpoint)) definition)
  (declare (ignore family))
  (unless (= 1 (count-if #'frontier-field-role-relaxed-p
                         (frontier-program-definition-fields definition)))
    (error "A monotone program needs exactly one :RELAXED field: ~S"
           (frontier-program-definition-name definition)))
  (unless (frontier-program-definition-transfer definition)
    (error "A monotone program needs a :TRANSFER law: ~S"
           (frontier-program-definition-name definition))))

(defmethod frontier-family-check-definition
    ((family (eql :invalidation)) definition)
  (declare (ignore family))
  (unless (= 1 (count-if #'frontier-field-role-invalidated-p
                         (frontier-program-definition-fields definition)))
    (error "An invalidation program needs exactly one :INVALIDATED field: ~S"
           (frontier-program-definition-name definition)))
  (unless (frontier-program-definition-admission definition)
    (error "An invalidation program needs an :ADMISSION (dependency) test: ~S"
           (frontier-program-definition-name definition))))

(defmethod frontier-family-check-definition
    ((family (eql :discover-once)) definition)
  (declare (ignore family))
  (unless (= 1 (count-if #'frontier-field-role-memo-p
                         (frontier-program-definition-fields definition)))
    (error "A discover-once program needs exactly one :MEMO field: ~S"
           (frontier-program-definition-name definition)))
  (unless (frontier-program-definition-admission definition)
    (error "A discover-once program needs an :ADMISSION test: ~S"
           (frontier-program-definition-name definition))))

;;; ------------------------------------------------------------------------
;;; Packed frontier storage and execution evidence

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
  (let ((maximum-priority (bucket-frontier-maximum-priority frontier)))
    ;; A runtime-constructed `(INTEGER 0 ,maximum-priority) type specifier
    ;; conses once per admission.  Spell the same open-protocol check as
    ;; primitive predicates so the packed frontier remains allocation-free.
    (unless (and (integerp priority)
                 (<= 0 priority maximum-priority))
      (error "Frontier priority ~S is outside 0..~D."
             priority maximum-priority)))
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
   (emissions :initform 0 :accessor frontier-execution-emissions)
   (admitted-sites :initarg :admitted-sites :initform nil
                   :reader frontier-execution-admitted-sites))
  (:documentation
   "Inspectable semantic work evidence from one frontier execution.

ADMITTED-SITES is a packed FRONTIER-SITE-BUFFER of every admitted site when
the program retains admissions, so a discover-once execution can hand its
component to the client without a cons per member, and an invalidation
execution its cleared set.  EMISSIONS counts sites handed to a companion
frontier, such as the surviving sources an invalidation discovers."))

(defun make-frontier-execution (program input frontier &key retain-admissions)
  (make-instance
   'frontier-execution
   :program (or (and (symbolp program) (frontier-program-definition-for program))
                program)
   :input input
   :frontier frontier
   :admitted-sites (and retain-admissions
                        (make-frontier-site-buffer :capacity 64))))

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
keeps traversal and value lifetimes visible to the compiler.  This is the
manually staged lowering which COMPILE-FRONTIER-PROGRAM now generates. #X7Q90E"
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

;;; ------------------------------------------------------------------------
;;; Field bindings: how one program role reaches physical storage

(defclass frontier-field-binding ()
  ((name :initarg :name :reader frontier-field-binding-name)
   (declaration :initarg :declaration :initform nil
                :reader frontier-field-binding-declaration)
   (lanes :initarg :lanes :initform nil :reader frontier-field-binding-lanes)
   (read-template :initarg :read :reader frontier-field-binding-read-template)
   (write-template :initarg :write :initform nil
                   :reader frontier-field-binding-write-template)
   (lazy-p :initarg :lazy :initform nil :reader frontier-field-binding-lazy-p))
  (:documentation
   "One program field role bound to storage reachable from a materialization.

DECLARATION is the represented-value declaration (often a voxel field
definition) giving the field its quantity and Lisp representation.  LANES are
(NAME FORM &key TYPE): storage borrowed once per materialization, where FORM
mentions the template variable MATERIALIZATION.  READ and WRITE are forms
over lane names, MATERIALIZATION, OFFSET, WINDOW, and (for WRITE) VALUE.  A
LAZY binding is read where the law mentions it rather than once up front, so
a short-circuiting admission can skip an expensive probe.  Templates are
ordinary inspectable data; the compiler substitutes them into the emitted
loop."))

(defun make-frontier-field-binding (name &key declaration lanes read write lazy)
  (check-type name symbol)
  (make-instance 'frontier-field-binding
                 :name name :declaration declaration :lanes lanes
                 :read read :write write :lazy lazy))

(defun frontier-field-binding-quantity-specification (binding)
  (let ((declaration (frontier-field-binding-declaration binding)))
    (and declaration
         (math:declaration-quantity-specification declaration))))

(defun frontier-field-binding-representation-type (binding)
  (let ((declaration (frontier-field-binding-declaration binding)))
    (and declaration
         (math:declaration-representation-type declaration))))

(defgeneric frontier-declaration-maximum-value (declaration)
  (:documentation
   "Return the finite integer maximum a declaration's legal values allow."))

(defmethod frontier-declaration-maximum-value (declaration)
  (declare (ignore declaration))
  nil)

(defmethod frontier-declaration-maximum-value
    ((declaration luvcraft.world.fields:voxel-field-definition))
  (let ((legal (luvcraft.world.fields:voxel-field-definition-legal-value-type
                declaration)))
    (and (consp legal) (eq (first legal) 'integer)
         (integerp (third legal))
         (third legal))))

;;; ------------------------------------------------------------------------
;;; Realizations

(defclass frontier-realization ()
  ((definition :initarg :definition :reader frontier-realization-definition)
   (revision :initarg :revision :reader frontier-realization-revision)
   (bindings :initarg :bindings :reader frontier-realization-bindings)
   (site-domain :initarg :site-domain :reader frontier-realization-site-domain)
   (maximum-priority :initarg :maximum-priority
                     :reader frontier-realization-maximum-priority)
   (priority-meaning :initarg :priority-meaning
                     :reader frontier-realization-priority-meaning)
   (transfer :initarg :transfer :reader frontier-realization-transfer)
   (admission :initarg :admission :reader frontier-realization-admission)
   (priority :initarg :priority :reader frontier-realization-priority)
   (drain-form :initarg :drain-form :reader frontier-realization-drain-form)
   (admit-form :initarg :admit-form :reader frontier-realization-admit-form)
   (relate-form :initarg :relate-form :reader frontier-realization-relate-form)
   (drain-function :initarg :drain-function
                   :reader frontier-realization-drain-function)
   (admit-function :initarg :admit-function
                   :reader frontier-realization-admit-function)
   (relate-function :initarg :relate-function
                    :reader frontier-realization-relate-function))
  (:documentation
   "One program compiled over bound fields into closed scalar Lisp.

TRANSFER, ADMISSION, and PRIORITY are the checked arithmetic expression
graphs; DRAIN-FORM and ADMIT-FORM are the emitted lambda forms; the two
functions are their compiled realizations.  ADMIT-FUNCTION seeds one site
through the same admission and commit law as a relation, or is NIL when the
law needs a source.  RELATE-FUNCTION exposes one relation from a virtual
source whose field values are supplied as arguments, so a boundary such as
open sky is the program's own transfer law rather than client arithmetic.
#53Q1II #581ZQP"))

(defmethod print-object ((realization frontier-realization) stream)
  (print-unreadable-object (realization stream :type t)
    (format stream "~S over ~{~S~^ ~}"
            (frontier-program-definition-name
             (frontier-realization-definition realization))
            (mapcar #'frontier-field-binding-name
                    (frontier-realization-bindings realization)))))

(defun frontier-realization-current-p (realization)
  "Whether REALIZATION still reflects its program's live definition."
  (let ((live (frontier-program-definition-for
               (frontier-program-definition-name
                (frontier-realization-definition realization)))))
    (and live
         (eq (frontier-program-definition-revision live)
             (frontier-realization-revision realization)))))

(defun make-realization-frontier (realization &key (initial-capacity 256))
  "Make the packed frontier the realization's layout requires."
  (make-bucket-frontier
   :maximum-priority (frontier-realization-maximum-priority realization)
   :priority-meaning (frontier-realization-priority-meaning realization)
   :initial-capacity initial-capacity))

(defun make-realization-execution (realization input frontier)
  (make-frontier-execution
   (frontier-realization-definition realization) input frontier
   :retain-admissions
   (frontier-program-definition-retain-admissions-p
    (frontier-realization-definition realization))))

(defun drain-frontier-realization
    (realization window frontier execution &rest arguments)
  "Run the compiled program until FRONTIER is empty; return EXECUTION.

ARGUMENTS are the program's constants as keywords.  A program whose family
hands sites to a companion frontier, such as invalidation's surviving
sources, takes that frontier as the first argument before the keywords."
  (apply (frontier-realization-drain-function realization)
         window frontier execution arguments))

(defun admit-frontier-realization-site
    (realization window frontier execution materialization offset value
     &rest constants)
  "Seed one site through the program's admission and commit law.

For a monotone program VALUE joins the relaxed field; for a discover-once
program VALUE is ignored.  Return whether the site was admitted."
  (let ((admit (frontier-realization-admit-function realization)))
    (unless admit
      (error "~S cannot admit a site without a source." realization))
    (apply admit window frontier execution materialization offset value
           constants)))

(defun schedule-frontier-realization-site
    (realization frontier execution materialization offset priority)
  "Push MATERIALIZATION/OFFSET at PRIORITY for reconsideration, without law.

This is the frontier word for a site whose value already stands but whose
relations must be exposed again, such as a resident chunk's face when a
neighbour arrives.  It is not an admission and is not retained."
  (declare (ignore realization execution))
  (bucket-frontier-push frontier materialization offset priority))

(defun relate-frontier-realization-site
    (realization window frontier execution materialization offset direction
     &rest arguments)
  "Expose one relation into MATERIALIZATION/OFFSET from a virtual source.

ARGUMENTS supply the program's constants and, keyed by role name, the source
field values the transfer reads; DIRECTION is the relation's direction as
seen from that source.  The target is tested, committed, and admitted by the
same law as an ordinary relation.  Return whether it was admitted. #581ZQP"
  (apply (frontier-realization-relate-function realization)
         window frontier execution materialization offset direction
         arguments))

;;; ------------------------------------------------------------------------
;;; The compiler

;;; Template variables.  Binding authors write these symbols; the compiler
;;; substitutes the emitted loop's own variables for them.
(defparameter *frontier-template-variables* '(materialization offset value))

(defvar *frontier-compilation-fields* nil
  "Alist of field role name to quantity specification while parsing laws.")

(defclass frontier-lowered-field ()
  ((binding :initarg :binding :reader lowered-field-binding)
   (source-parameter :initarg :source-parameter
                     :reader lowered-field-source-parameter)
   (target-parameter :initarg :target-parameter
                     :reader lowered-field-target-parameter)
   (source-lanes :initarg :source-lanes :reader lowered-field-source-lanes)
   (target-lanes :initarg :target-lanes :reader lowered-field-target-lanes)
   (source-variable :initarg :source-variable
                    :reader lowered-field-source-variable)
   (target-variable :initarg :target-variable
                    :reader lowered-field-target-variable))
  (:documentation "Compiler bookkeeping for one bound field role."))

(defun frontier-role-symbol (name role)
  (make-symbol (format nil "~A/~A" name role)))

(defun make-frontier-parameter (name specification)
  (make-instance 'lang:arithmetic-parameter
                 :name name :quantity-specification specification
                 :source-form name))

(lang:define-arithmetic-operator as-field-quantity
  "Assert that a raw or foreign value is measured in a program field's quantity.")

(defmethod lang:parse-arithmetic-operator-call
    ((operator (eql 'as-field-quantity)) form environment)
  (declare (ignore operator))
  (destructuring-bind (name field-name operand-form) form
    (declare (ignore name))
    (let* ((entry (assoc field-name *frontier-compilation-fields*))
           (specification (cdr entry))
           (operand (lang:parse-arithmetic-expression operand-form environment)))
      (unless entry
        (error 'lang:arithmetic-language-error
               :form form :reason :unknown-frontier-field :details field-name))
      (unless specification
        (error 'lang:arithmetic-language-error
               :form form :reason :field-has-no-quantity :details field-name))
      (when (lang:arithmetic-expression-quantity-checked-p operand)
        (setf operand (make-instance 'lang:arithmetic-representation
                                     :operand operand :source-form form)))
      (make-instance 'lang:arithmetic-quantity-assumption
                     :operand operand
                     :quantity-specification specification
                     :source-form form))))

(defun rewrite-frontier-field-references (form fields)
  "Replace (FIELD ROLE) reads with the compiler's role symbols."
  (flet ((lowered-for (name)
           (find name fields
                 :key (lambda (lowered)
                        (frontier-field-binding-name
                         (lowered-field-binding lowered))))))
    (cond ((atom form) form)
          ((frontier-word-p (first form) "AS-FIELD-QUANTITY")
           (list* 'as-field-quantity (second form)
                  (mapcar (lambda (subform)
                            (rewrite-frontier-field-references subform fields))
                          (cddr form))))
          ((and (symbolp (first form))
                (= (length form) 2)
                (or (frontier-word-p (second form) "SOURCE")
                    (frontier-word-p (second form) "TARGET"))
                (lowered-for (first form)))
           (let ((lowered (lowered-for (first form))))
             (if (frontier-word-p (second form) "SOURCE")
                 (lang:arithmetic-object-name
                  (lowered-field-source-parameter lowered))
                 (lang:arithmetic-object-name
                  (lowered-field-target-parameter lowered)))))
          (t (mapcar (lambda (subform)
                       (rewrite-frontier-field-references subform fields))
                     form)))))

(defun expression-reference-targets (expression)
  "Every arithmetic reference target reachable from EXPRESSION."
  (let ((seen (make-hash-table :test #'eq))
        (targets nil))
    (labels ((visit (expression)
               (unless (gethash expression seen)
                 (setf (gethash expression seen) t)
                 (when (typep expression 'lang:arithmetic-reference)
                   (pushnew (lang:arithmetic-reference-target expression)
                            targets))
                 (mapc #'visit (lang:arithmetic-expression-children expression)))))
      (visit expression))
    targets))

(defun substitute-template (template substitutions)
  "Substitute SUBSTITUTIONS (name . form) into TEMPLATE, matching by name."
  (cond ((consp template)
         (cons (substitute-template (car template) substitutions)
               (substitute-template (cdr template) substitutions)))
        ((and (symbolp template) template (not (keywordp template)))
         (let ((entry (assoc template substitutions
                             :test (lambda (form key)
                                     (frontier-word-p form (symbol-name key))))))
           (if entry (cdr entry) template)))
        (t template)))

(defun lane-let-bindings (binding lanes-alist materialization-variable)
  (loop for (lane-name form) in (frontier-field-binding-lanes binding)
        collect `(,(cdr (assoc lane-name lanes-alist))
                  ,(substitute-template
                    form `((materialization . ,materialization-variable))))))

(defun lane-type-declarations (binding lanes-alist)
  (loop for (lane-name nil . options) in (frontier-field-binding-lanes binding)
        for type = (getf options :type)
        when type
          collect `(type ,type ,(cdr (assoc lane-name lanes-alist)))))

(defun role-materialization-variable (role)
  (ecase role (:source 'source) (:target 'target)))

(defun field-read-form (lowered role offset-variable)
  (let* ((binding (lowered-field-binding lowered))
         (lanes (ecase role
                  (:source (lowered-field-source-lanes lowered))
                  (:target (lowered-field-target-lanes lowered)))))
    (substitute-template
     (frontier-field-binding-read-template binding)
     (append lanes
             `((offset . ,offset-variable)
               (materialization . ,(role-materialization-variable role))
               (window . window))))))

(defun field-write-form (lowered offset-variable value-form)
  (let* ((binding (lowered-field-binding lowered))
         (template (frontier-field-binding-write-template binding)))
    (unless template
      (error "Field ~S has no write template but the program commits to it."
             (frontier-field-binding-name binding)))
    (substitute-template
     template
     (append (lowered-field-target-lanes lowered)
             `((offset . ,offset-variable) (value . ,value-form)
               (materialization . target) (window . window))))))

(defclass frontier-compilation ()
  ((definition :initarg :definition :reader compilation-definition)
   (fields :initarg :fields :reader compilation-fields)
   (constants :initarg :constants :reader compilation-constants)
   (predicates :initarg :predicates :reader compilation-predicates)
   (environment :initarg :environment :accessor compilation-environment)
   (transfer :initform nil :accessor compilation-transfer)
   (admission :initform nil :accessor compilation-admission)
   (priority :initform nil :accessor compilation-priority)
   (site-domain :initarg :site-domain :reader compilation-site-domain))
  (:documentation "Working state for one COMPILE-FRONTIER-PROGRAM call."))

(defun compilation-lowered-field (compilation name)
  (or (find name (compilation-fields compilation)
            :key (lambda (lowered)
                   (frontier-field-binding-name (lowered-field-binding lowered))))
      (error "Program ~S has no field ~S."
             (frontier-program-definition-name
              (compilation-definition compilation))
             name)))

(defun compilation-relaxed-field (compilation)
  (let ((role (frontier-program-relaxed-field
               (compilation-definition compilation))))
    (and role (compilation-lowered-field
               compilation (frontier-field-role-name role)))))

(defun compilation-invalidated-field (compilation)
  (let ((role (frontier-program-invalidated-field
               (compilation-definition compilation))))
    (and role (compilation-lowered-field
               compilation (frontier-field-role-name role)))))

(defun compilation-memo-field (compilation)
  (let ((role (frontier-program-memo-field
               (compilation-definition compilation))))
    (and role (compilation-lowered-field
               compilation (frontier-field-role-name role)))))

(defun parse-frontier-law (compilation form)
  "Parse one arithmetic law of the program in the compilation environment."
  (and form
       (let ((*frontier-compilation-fields*
               (mapcar (lambda (lowered)
                         (cons (frontier-field-binding-name
                                (lowered-field-binding lowered))
                               (frontier-field-binding-quantity-specification
                                (lowered-field-binding lowered))))
                       (compilation-fields compilation))))
         (lang:parse-arithmetic-expression
          (rewrite-frontier-field-references
           form (compilation-fields compilation))
          (compilation-environment compilation)))))

(defun ensure-law-fits-field (expression lowered what)
  "Require EXPRESSION's quantity to be interpretable as LOWERED's field."
  (let ((field-specification
          (frontier-field-binding-quantity-specification
           (lowered-field-binding lowered))))
    (when (and expression field-specification)
      (let ((derived
              (and (lang:arithmetic-expression-quantity-checked-p expression)
                   (lang:arithmetic-expression-quantity-specification
                    expression))))
        (unless derived
          (error "The ~A law ~S has no quantity but field ~S is measured in ~S."
                 what (lang:arithmetic-expression-form expression)
                 (frontier-field-binding-name (lowered-field-binding lowered))
                 field-specification))
        (handler-case
            (math:interpret-quantity-specification derived field-specification)
          (math:quantity-operation-error (condition)
            (error "The ~A law ~S yields ~S, which is not the ~S field's ~S: ~A"
                   what (lang:arithmetic-expression-form expression)
                   derived
                   (frontier-field-binding-name (lowered-field-binding lowered))
                   field-specification condition)))))))

(defun lower-law (compilation expression lowering-environment)
  (declare (ignore compilation))
  (let ((lisp:*lisp-arithmetic-lowering* :scalar))
    (lisp:lower-lisp-arithmetic-expression expression lowering-environment)))

(defgeneric frontier-family-law-forms (family compilation candidate-variable
                                       lowering-environment &key seed-p)
  (:documentation
   "Return (VALUES TEST-FORM COMMIT-FORMS PRIORITY-FORM OTHERWISE-FORMS) for
one exposed target under FAMILY, given the lowering environment of bound
field values.  CANDIDATE-VARIABLE names the transfer result for monotone
families, or the supplied value when SEED-P says the target is being seeded
rather than reached through a relation.  OTHERWISE-FORMS run when the test
fails, for families with a secondary effect. #FE0O5R"))

(defmethod frontier-family-law-forms
    ((family (eql :monotone-max-fixpoint)) compilation candidate
     lowering-environment &key seed-p)
  (declare (ignore family seed-p))
  (let* ((relaxed (compilation-relaxed-field compilation))
         (current (lowered-field-target-variable relaxed))
         (admission (compilation-admission compilation))
         (priority (compilation-priority compilation)))
    (values
     (if admission
         `(and (> ,candidate ,current)
               ,(lower-law compilation admission lowering-environment))
         `(> ,candidate ,current))
     (list (field-write-form relaxed 'target-offset candidate))
     (if priority
         (lower-law compilation priority lowering-environment)
         candidate))))

(defmethod frontier-family-law-forms
    ((family (eql :discover-once)) compilation candidate lowering-environment
     &key seed-p)
  (declare (ignore family candidate seed-p))
  (let* ((memo (compilation-memo-field compilation))
         (visited (lowered-field-target-variable memo))
         (priority (compilation-priority compilation)))
    (values
     `(and (not ,visited)
           ,(lower-law compilation (compilation-admission compilation)
                       lowering-environment))
     (list (field-write-form memo 'target-offset t))
     (if priority
         (lower-law compilation priority lowering-environment)
         0))))

(defmethod frontier-family-law-forms
    ((family (eql :invalidation)) compilation candidate lowering-environment
     &key seed-p)
  "Clear a dependent target and re-admit it at the level it had; hand an
independent lit target to the companion frontier as a surviving source.

The invalidated field's source value is the popped priority: the level the
cleared source had.  A seed clears the site unconditionally and admits it at
the supplied value."
  (declare (ignore family))
  (let* ((invalidated (compilation-invalidated-field compilation))
         (current (lowered-field-target-variable invalidated)))
    (if seed-p
        (values t
                (list (field-write-form invalidated 'target-offset 0))
                candidate
                nil)
        (values
         `(and (plusp ,current)
               ,(lower-law compilation (compilation-admission compilation)
                           lowering-environment))
         (list (field-write-form invalidated 'target-offset 0))
         current
         `((when (plusp ,current)
             (survive target target-offset ,current)))))))

(defgeneric frontier-family-companion-p (family)
  (:documentation
   "Whether FAMILY's drain hands sites to a second, companion frontier."))

(defmethod frontier-family-companion-p (family)
  (declare (ignore family))
  nil)

(defmethod frontier-family-companion-p ((family (eql :invalidation)))
  (declare (ignore family))
  t)

(defun frontier-directions-form (definition)
  (let ((neighborhood (frontier-program-definition-neighborhood definition)))
    (cond ((eq neighborhood :voxel-face-relations)
           '*voxel-face-directions*)
          ((and (consp neighborhood)
                (eq (first neighborhood) :voxel-relations)
                (member (second neighborhood)
                        (frontier-program-definition-constants definition)))
           (second neighborhood))
          (t (error "Unknown frontier neighborhood ~S." neighborhood)))))

(defun frontier-layout-maximum-priority (definition relaxed-binding)
  "The finite bucket range: from the relaxed or invalidated field's legal values."
  (let ((layout (frontier-program-definition-frontier-layout definition)))
    (cond ((eq layout :brightest-first-buckets)
           (or (and relaxed-binding
                    (frontier-declaration-maximum-value
                     (frontier-field-binding-declaration relaxed-binding)))
               (error "~S needs a finite legal-value maximum on its relaxed field."
                      layout)))
          ((eq layout :lifo-stack) 0)
          ((and (consp layout) (eq (first layout) :buckets))
           (second layout))
          (t (error "Unknown frontier layout ~S." layout)))))

(defun predicate-form (predicate)
  (ecase (frontier-relation-predicate-kind predicate)
    (:crossing '(and crossing t))
    (:direction= `(eq direction ,(frontier-relation-predicate-argument predicate)))))

(defun compile-frontier-program
    (program &key bindings site-domain (compile t))
  "Close PROGRAM over field BINDINGS and emit its scalar realization.

SITE-DOMAIN is a form over MATERIALIZATION yielding the chunk domain that
gives a site's dense offset spatial meaning.  Each field role of the program
must have one FRONTIER-FIELD-BINDING.  The laws are parsed and quantity
checked against the bound fields, lowered with scalar arithmetic, and spliced
into one closed loop over raw buckets and lanes.  With COMPILE NIL the forms
are emitted but not compiled, for inspection. #53Q1II #T2G95K #716UN6"
  (let* ((definition (if (symbolp program)
                         (or (frontier-program-definition-for program)
                             (error "No frontier program named ~S." program))
                         program))
         (roles (frontier-program-definition-fields definition))
         (constants (frontier-program-definition-constants definition))
         (fields
           (mapcar
            (lambda (role)
              (let* ((name (frontier-field-role-name role))
                     (binding (or (find name bindings
                                        :key #'frontier-field-binding-name)
                                  (error "Program ~S field ~S is unbound."
                                         (frontier-program-definition-name
                                          definition)
                                         name)))
                     (specification
                       (frontier-field-binding-quantity-specification binding)))
                (flet ((lanes (which)
                         (loop for (lane-name) in (frontier-field-binding-lanes
                                                   binding)
                               collect (cons lane-name
                                             (make-symbol
                                              (format nil "~A/~A/~A"
                                                      which name lane-name))))))
                  (make-instance
                   'frontier-lowered-field
                   :binding binding
                   :source-parameter
                   (make-frontier-parameter
                    (frontier-role-symbol name 'source) specification)
                   :target-parameter
                   (make-frontier-parameter
                    (frontier-role-symbol name 'target) specification)
                   :source-lanes (lanes 'source)
                   :target-lanes (lanes 'target)
                   :source-variable (frontier-role-symbol name 'source)
                   :target-variable (frontier-role-symbol name 'target)))))
            roles))
         (predicates (frontier-program-definition-predicates definition))
         (constant-parameters
           (mapcar (lambda (name) (make-frontier-parameter name nil)) constants))
         (predicate-parameters
           (mapcar (lambda (predicate)
                     (make-frontier-parameter
                      (frontier-relation-predicate-name predicate) nil))
                   predicates))
         (environment
           (append
            (loop for lowered in fields
                  collect (cons (lang:arithmetic-object-name
                                 (lowered-field-source-parameter lowered))
                                (lowered-field-source-parameter lowered))
                  collect (cons (lang:arithmetic-object-name
                                 (lowered-field-target-parameter lowered))
                                (lowered-field-target-parameter lowered)))
            (mapcar (lambda (parameter)
                      (cons (lang:arithmetic-object-name parameter) parameter))
                    (append constant-parameters predicate-parameters))))
         (compilation
           (make-instance 'frontier-compilation
                          :definition definition
                          :fields fields
                          :constants constant-parameters
                          :predicates predicate-parameters
                          :environment environment
                          :site-domain site-domain)))
    (unless site-domain
      (error "COMPILE-FRONTIER-PROGRAM needs a :SITE-DOMAIN template."))
    (dolist (lowered fields)
      (let ((role (find (frontier-field-binding-name
                         (lowered-field-binding lowered))
                        roles :key #'frontier-field-role-name)))
        (when (and (frontier-field-binding-lazy-p (lowered-field-binding lowered))
                   (or (frontier-field-role-relaxed-p role)
                       (frontier-field-role-memo-p role)))
          (error "The ~S field is relaxed or a memo and cannot be bound lazily."
                 (frontier-field-role-name role)))))
    (dolist (predicate predicates)
      (when (and (eq (frontier-relation-predicate-kind predicate) :direction=)
                 (not (member (frontier-relation-predicate-argument predicate)
                              constants)))
        (error "Predicate ~S names ~S, which is not a program constant."
               (frontier-relation-predicate-name predicate)
               (frontier-relation-predicate-argument predicate))))
    ;; Parse and check the laws.
    (setf (compilation-transfer compilation)
          (parse-frontier-law
           compilation (frontier-program-definition-transfer definition))
          (compilation-admission compilation)
          (parse-frontier-law
           compilation (frontier-program-definition-admission definition))
          (compilation-priority compilation)
          (parse-frontier-law
           compilation (frontier-program-definition-priority definition)))
    (let ((relaxed (compilation-relaxed-field compilation)))
      (when relaxed
        (ensure-law-fits-field (compilation-transfer compilation) relaxed
                               "transfer")
        (ensure-law-fits-field (compilation-priority compilation) relaxed
                               "priority")))
    (let* ((relaxed (or (compilation-relaxed-field compilation)
                        (compilation-invalidated-field compilation)))
           (relaxed-binding (and relaxed (lowered-field-binding relaxed)))
           (maximum-priority
             (frontier-layout-maximum-priority definition relaxed-binding))
           (drain-form (emit-frontier-drain-form compilation))
           (admit-form (emit-frontier-admit-form compilation))
           (relate-form (and (not (frontier-family-companion-p
                                   (frontier-program-definition-family
                                    definition)))
                             (emit-frontier-relate-form compilation))))
      (make-instance
       'frontier-realization
       :definition definition
       :revision (frontier-program-definition-revision definition)
       :bindings bindings
       :site-domain site-domain
       :maximum-priority maximum-priority
       :priority-meaning (and relaxed-binding
                              (frontier-field-binding-declaration
                               relaxed-binding))
       :transfer (compilation-transfer compilation)
       :admission (compilation-admission compilation)
       :priority (compilation-priority compilation)
       :drain-form drain-form
       :admit-form admit-form
       :relate-form relate-form
       :drain-function (and compile (compile nil drain-form))
       :admit-function (and compile admit-form (compile nil admit-form))
       :relate-function (and compile relate-form (compile nil relate-form))))))

;;; Emission.  The generated loop is deliberately plain: raw bucket vectors
;;; and counters are bound once at entry and written back at exit; the source
;;; site's lanes and field values are bound once per pop; the popped site is
;;; validated once and its six relations are primitive fixnum steps
;;; (DO-CHUNK-SITE-NEIGHBORS, #FGT96H); a target's lanes are the source's own
;;; for a local step and are borrowed from the crossing materialization
;;; otherwise.  Nothing per relation is generic, checked twice, or allocated.

(defun compilation-lowering-environment (compilation &key (source-p t))
  "Map every arithmetic parameter to the emitted form holding its value.

Eager fields are read once into a variable; lazy fields lower to their read
form wherever the law mentions them."
  (append
   (loop for lowered in (compilation-fields compilation)
         for lazy = (frontier-field-binding-lazy-p
                     (lowered-field-binding lowered))
         when source-p
           collect (cons (lowered-field-source-parameter lowered)
                         (if lazy
                             (field-read-form lowered :source 'source-offset)
                             (lowered-field-source-variable lowered)))
         collect (cons (lowered-field-target-parameter lowered)
                       (if lazy
                           (field-read-form lowered :target 'target-offset)
                           (lowered-field-target-variable lowered))))
   (mapcar (lambda (parameter)
             (cons parameter (lang:arithmetic-object-name parameter)))
           (compilation-constants compilation))
   (when source-p
     (mapcar (lambda (parameter)
               (cons parameter (lang:arithmetic-object-name parameter)))
             (compilation-predicates compilation)))))

(defun compilation-referenced-parameters (compilation)
  (let ((targets nil))
    (dolist (expression (list (compilation-transfer compilation)
                              (compilation-admission compilation)
                              (compilation-priority compilation)))
      (when expression
        (setf targets (union targets (expression-reference-targets expression)))))
    targets))

(defun field-value-bindings (compilation role offset-variable referenced)
  "LET* bindings and declarations reading the ROLE values the laws need."
  (let ((bindings nil) (declarations nil) (needed nil))
    (dolist (lowered (compilation-fields compilation))
      (let* ((parameter (ecase role
                          (:source (lowered-field-source-parameter lowered))
                          (:target (lowered-field-target-parameter lowered))))
             (variable (ecase role
                         (:source (lowered-field-source-variable lowered))
                         (:target (lowered-field-target-variable lowered))))
             (binding (lowered-field-binding lowered))
             (implied (and (eq role :target)
                           (or (frontier-field-role-relaxed-p
                                (find (frontier-field-binding-name binding)
                                      (frontier-program-definition-fields
                                       (compilation-definition compilation))
                                      :key #'frontier-field-role-name))
                               (frontier-field-role-memo-p
                                (find (frontier-field-binding-name binding)
                                      (frontier-program-definition-fields
                                       (compilation-definition compilation))
                                      :key #'frontier-field-role-name))
                               (frontier-field-role-invalidated-p
                                (find (frontier-field-binding-name binding)
                                      (frontier-program-definition-fields
                                       (compilation-definition compilation))
                                      :key #'frontier-field-role-name))))))
        (when (or implied (member parameter referenced))
          (push lowered needed)
          (unless (frontier-field-binding-lazy-p binding)
            (push `(,variable
                    ,(if (and (eq role :source)
                              (eq lowered (compilation-invalidated-field
                                           compilation)))
                         ;; A cleared source's value is the level it had,
                         ;; which is the priority it was admitted at.
                         'source-priority
                         (field-read-form lowered role offset-variable)))
                  bindings)
            (let ((type (frontier-field-binding-representation-type binding)))
              (when type
                (push `(type ,type ,variable) declarations)))))))
    (values (nreverse bindings) (nreverse declarations) (nreverse needed))))

(defun emit-target-law (compilation candidate-form lowering-environment
                        &key admit-form seed-p)
  "Emit the test, commits, and admission for one exposed target site."
  (let ((candidate (make-symbol "CANDIDATE")))
    (multiple-value-bind (test commits priority otherwise)
        (frontier-family-law-forms
         (frontier-program-definition-family (compilation-definition compilation))
         compilation candidate lowering-environment :seed-p seed-p)
      (let ((body (if otherwise
                      `(if ,test
                           (progn ,@commits ,(funcall admit-form priority))
                           (progn ,@otherwise))
                      `(when ,test
                         ,@commits
                         ,(funcall admit-form priority)))))
        (if candidate-form
            `(let ((,candidate ,candidate-form)) ,body)
            body)))))

(defun counter-write-back-forms ()
  `((setf (bucket-frontier-count frontier) count
          (bucket-frontier-current-priority frontier) current-priority
          (bucket-frontier-pushes frontier) pushes
          (bucket-frontier-pops frontier) pops
          (bucket-frontier-peak-count frontier) peak-count)
    (incf (frontier-execution-visits execution) visits)
    (incf (frontier-execution-relations execution) relations)
    (incf (frontier-execution-admissions execution) admissions)
    (incf (frontier-execution-crossings execution) crossings)
    (incf (frontier-execution-unavailable execution) unavailable)))

(defun companion-parameters (compilation)
  (and (frontier-family-companion-p
        (frontier-program-definition-family (compilation-definition compilation)))
       '(companion)))

(defun companion-bindings (compilation)
  (and (companion-parameters compilation)
       `((companion-buckets (bucket-frontier-buckets companion))
         (companion-count (bucket-frontier-count companion))
         (companion-current-priority
          (bucket-frontier-current-priority companion))
         (companion-pushes (bucket-frontier-pushes companion))
         (companion-peak-count (bucket-frontier-peak-count companion))
         (emissions 0))))

(defun companion-declarations (compilation)
  (and (companion-parameters compilation)
       `((type simple-vector companion-buckets)
         (type fixnum companion-count companion-current-priority
               companion-pushes companion-peak-count emissions))))

(defun companion-flets (compilation)
  (and (companion-parameters compilation)
       `((survive (materialization offset priority)
           (declare (type fixnum offset priority))
           (frontier-site-buffer-push (svref companion-buckets priority)
                                      materialization offset)
           (incf companion-count)
           (incf companion-pushes)
           (incf emissions)
           (when (> priority companion-current-priority)
             (setf companion-current-priority priority))
           (when (> companion-count companion-peak-count)
             (setf companion-peak-count companion-count))))))

(defun companion-write-back-forms (compilation)
  (and (companion-parameters compilation)
       `((setf (bucket-frontier-count companion) companion-count
               (bucket-frontier-current-priority companion)
               companion-current-priority
               (bucket-frontier-pushes companion) companion-pushes
               (bucket-frontier-peak-count companion) companion-peak-count)
         (incf (frontier-execution-emissions execution) emissions))))

(defun raw-frontier-bindings ()
  `((buckets (bucket-frontier-buckets frontier))
    (count (bucket-frontier-count frontier))
    (current-priority (bucket-frontier-current-priority frontier))
    (pushes (bucket-frontier-pushes frontier))
    (pops (bucket-frontier-pops frontier))
    (peak-count (bucket-frontier-peak-count frontier))
    (admitted-sites (frontier-execution-admitted-sites execution))
    (visits 0) (relations 0) (admissions 0) (crossings 0) (unavailable 0)))

(defun raw-frontier-declarations ()
  `(declare (type simple-vector buckets)
            (type fixnum count current-priority pushes pops peak-count
                  visits relations admissions crossings unavailable)
            (ignorable admitted-sites)))

(defun admit-flet ()
  `(admit (materialization offset priority)
     (declare (type fixnum offset priority))
     (frontier-site-buffer-push (svref buckets priority) materialization offset)
     (when admitted-sites
       (frontier-site-buffer-push admitted-sites materialization offset))
     (incf count)
     (incf pushes)
     (incf admissions)
     (when (> priority current-priority) (setf current-priority priority))
     (when (> count peak-count) (setf peak-count count))))

(defun emit-frontier-drain-form (compilation)
  (let* ((definition (compilation-definition compilation))
         (constants (mapcar #'lang:arithmetic-object-name
                            (compilation-constants compilation)))
         (referenced (compilation-referenced-parameters compilation))
         (lowering (compilation-lowering-environment compilation))
         (transfer (compilation-transfer compilation))
         (candidate-form (and transfer (lower-law compilation transfer lowering)))
         (directions (frontier-directions-form definition)))
    (multiple-value-bind (source-values source-declarations source-needed)
        (field-value-bindings compilation :source 'source-offset referenced)
      (multiple-value-bind (target-values target-declarations target-needed)
          (field-value-bindings compilation :target 'target-offset referenced)
        ;; A local step's target lanes are the source's own, so every lane a
        ;; target reads is also borrowed at the source.
        (setf source-needed
              (remove-if-not (lambda (lowered)
                               (or (member lowered source-needed)
                                   (member lowered target-needed)))
                             (compilation-fields compilation)))
        `(lambda (window frontier execution
                  ,@(companion-parameters compilation) &key ,@constants)
           (declare (ignorable window ,@constants))
           (let* (,@(raw-frontier-bindings)
                  ,@(companion-bindings compilation)
                  (directions ,directions))
             ,(raw-frontier-declarations)
             (declare ,@(companion-declarations compilation))
             (flet (,(admit-flet) ,@(companion-flets compilation))
               (declare (inline admit))
               (loop
                 (when (zerop count) (return))
                 (let ((bucket (svref buckets current-priority))
                       (source-priority current-priority))
                   (declare (type fixnum source-priority) (ignorable source-priority))
                   (multiple-value-bind (source source-offset present-p)
                       (frontier-site-buffer-pop bucket)
                     (declare (type fixnum source-offset))
                     (unless present-p
                       (error "Frontier count disagrees with priority bucket ~D."
                              current-priority))
                     (decf count)
                     (incf pops)
                     (when (zerop (frontier-site-buffer-length bucket))
                       (setf current-priority
                             (loop for candidate downfrom (1- current-priority) to 0
                                   when (plusp (frontier-site-buffer-length
                                                (svref buckets candidate)))
                                     return candidate
                                   finally (return -1))))
                     (incf visits)
                     (let* ((source-domain
                              ,(substitute-template
                                (compilation-site-domain compilation)
                                '((materialization . source))))
                            ,@(loop for lowered in source-needed
                                    append (lane-let-bindings
                                            (lowered-field-binding lowered)
                                            (lowered-field-source-lanes lowered)
                                            'source))
                            ,@source-values)
                       (declare ,@(loop for lowered in source-needed
                                        append (lane-type-declarations
                                                (lowered-field-binding lowered)
                                                (lowered-field-source-lanes lowered)))
                                ,@source-declarations)
                       (do-chunk-site-neighbors
                           (target-offset crossing direction
                            materialization availability
                            window source-domain source-offset directions)
                         (incf relations)
                         (when crossing (incf crossings))
                         (if (eq availability :unavailable)
                             (incf unavailable)
                             (let* ((target (if crossing materialization source))
                                    ,@(loop for lowered in target-needed
                                            append
                                            (loop for (lane . target-variable)
                                                    in (lowered-field-target-lanes lowered)
                                                  for source-variable
                                                    = (cdr (assoc lane (lowered-field-source-lanes lowered)))
                                                  for form
                                                    = (second (assoc lane (frontier-field-binding-lanes
                                                                           (lowered-field-binding lowered))))
                                                  collect
                                                  `(,target-variable
                                                    (if crossing
                                                        ,(substitute-template
                                                          form '((materialization . target)))
                                                        ,source-variable))))
                                    ,@(loop for predicate
                                              in (frontier-program-definition-predicates definition)
                                            collect `(,(frontier-relation-predicate-name predicate)
                                                      ,(predicate-form predicate)))
                                    ,@target-values)
                               (declare
                                ,@(loop for lowered in target-needed
                                        append (lane-type-declarations
                                                (lowered-field-binding lowered)
                                                (lowered-field-target-lanes lowered)))
                                ,@target-declarations
                                (ignorable target
                                           ,@(mapcar #'frontier-relation-predicate-name
                                                     (frontier-program-definition-predicates
                                                      definition))))
                               ,(emit-target-law
                                 compilation candidate-form lowering
                                 :admit-form
                                 (lambda (priority)
                                   `(admit target target-offset ,priority))))))))))
               ,@(counter-write-back-forms)
               ,@(companion-write-back-forms compilation)
               execution)))))))

(defun emit-frontier-admit-form (compilation)
  "Emit the seed entry point, or NIL when the law needs a source or relation."
  (let* ((constants (mapcar #'lang:arithmetic-object-name
                            (compilation-constants compilation)))
         (referenced
           (let ((targets nil))
             (dolist (expression (list (compilation-admission compilation)
                                       (compilation-priority compilation))
                              targets)
               (when expression
                 (setf targets
                       (union targets
                              (expression-reference-targets expression)))))))
         (relaxed (compilation-relaxed-field compilation))
         (source-parameters
           (append (mapcar #'lowered-field-source-parameter
                           (compilation-fields compilation))
                   (compilation-predicates compilation)))
         (law-expressions (list (compilation-admission compilation)
                                (compilation-priority compilation))))
    (when (and (not (eq (frontier-program-definition-family
                         (compilation-definition compilation))
                        :invalidation))
               (some (lambda (expression)
                       (and expression
                            (intersection (expression-reference-targets expression)
                                          source-parameters)))
                     law-expressions))
      (return-from emit-frontier-admit-form nil))
    (multiple-value-bind (target-values target-declarations target-needed)
        (field-value-bindings compilation :target 'target-offset referenced)
      `(lambda (window frontier execution materialization target-offset value
                &key ,@constants)
         (declare (ignorable window value ,@constants)
                  (type fixnum target-offset))
         (let* (,@(raw-frontier-bindings)
                (target materialization)
                ,@(loop for lowered in target-needed
                        append (lane-let-bindings
                                (lowered-field-binding lowered)
                                (lowered-field-target-lanes lowered)
                                'target))
                ,@target-values
                (admitted-p nil))
           ,(raw-frontier-declarations)
           (declare ,@(loop for lowered in target-needed
                            append (lane-type-declarations
                                    (lowered-field-binding lowered)
                                    (lowered-field-target-lanes lowered)))
                    ,@target-declarations)
           (flet (,(admit-flet))
             (declare (inline admit))
             ,(emit-target-law
               compilation
               (and (or relaxed (compilation-invalidated-field compilation))
                    'value)
               (compilation-lowering-environment compilation :source-p nil)
               :seed-p t
               :admit-form
               (lambda (priority)
                 `(progn (admit target target-offset ,priority)
                         (setf admitted-p t)))))
           ,@(counter-write-back-forms)
           admitted-p)))))

(defun emit-frontier-relate-form (compilation)
  "Emit the virtual-source relation entry point.

The source role's field values arrive as keyword arguments named by role, the
relation direction as an argument, and the crossing predicate is true: a
virtual source is by definition outside the target's materialization."
  (let* ((definition (compilation-definition compilation))
         (constants (mapcar #'lang:arithmetic-object-name
                            (compilation-constants compilation)))
         (referenced (compilation-referenced-parameters compilation))
         (lowering (compilation-lowering-environment compilation))
         (transfer (compilation-transfer compilation))
         (candidate-form (and transfer (lower-law compilation transfer lowering)))
         (source-fields
           (remove-if-not
            (lambda (lowered)
              (member (lowered-field-source-parameter lowered) referenced))
            (compilation-fields compilation)))
         (source-arguments
           (mapcar (lambda (lowered)
                     (frontier-field-binding-name (lowered-field-binding lowered)))
                   source-fields)))
    (multiple-value-bind (target-values target-declarations target-needed)
        (field-value-bindings compilation :target 'target-offset referenced)
      `(lambda (window frontier execution materialization target-offset direction
                &key ,@constants ,@source-arguments)
         (declare (ignorable window direction ,@constants)
                  (type fixnum target-offset))
         (let* (,@(raw-frontier-bindings)
                (target materialization)
                ,@(loop for lowered in source-fields
                        for argument in source-arguments
                        collect `(,(lowered-field-source-variable lowered)
                                  ,argument))
                ,@(loop for lowered in target-needed
                        append (lane-let-bindings
                                (lowered-field-binding lowered)
                                (lowered-field-target-lanes lowered)
                                'target))
                ,@(loop for predicate
                          in (frontier-program-definition-predicates definition)
                        collect `(,(frontier-relation-predicate-name predicate)
                                  ,(ecase (frontier-relation-predicate-kind
                                           predicate)
                                     (:crossing t)
                                     (:direction=
                                      (predicate-form predicate)))))
                ,@target-values
                (admitted-p nil))
           ,(raw-frontier-declarations)
           (declare ,@(loop for lowered in source-fields
                            for type = (frontier-field-binding-representation-type
                                        (lowered-field-binding lowered))
                            when type
                              collect `(type ,type
                                             ,(lowered-field-source-variable
                                               lowered)))
                    ,@(loop for lowered in target-needed
                            append (lane-type-declarations
                                    (lowered-field-binding lowered)
                                    (lowered-field-target-lanes lowered)))
                    ,@target-declarations
                    (ignorable ,@(mapcar #'frontier-relation-predicate-name
                                         (frontier-program-definition-predicates
                                          definition))))
           (flet (,(admit-flet))
             (declare (inline admit))
             ,(emit-target-law
               compilation candidate-form lowering
               :admit-form
               (lambda (priority)
                 `(progn (admit target target-offset ,priority)
                         (setf admitted-p t)))))
           ,@(counter-write-back-forms)
           admitted-p)))))
