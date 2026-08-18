;;; Generated structure-of-arrays storage with inspectable quantity meaning.

(in-package #:luv.arithmetic.records)

(define-condition columnar-declaration-error (error)
  ((definition
    :initarg :definition
    :reader columnar-declaration-error-definition)
   (lane-name
    :initarg :lane-name
    :reader columnar-declaration-error-lane-name)
   (declaration
    :initarg :declaration
    :reader columnar-declaration-error-declaration)
   (reason
    :initarg :reason
    :reader columnar-declaration-error-reason))
  (:report
   (lambda (condition stream)
     (format stream "Columnar declaration for ~S lane ~S is invalid: ~A."
             (columnar-layout-definition-name
              (columnar-declaration-error-definition condition))
             (columnar-declaration-error-lane-name condition)
             (columnar-declaration-error-reason condition)))))

(defclass columnar-lane-definition (math:represented-value-declaration)
  ((name
    :initarg :name
    :reader columnar-lane-definition-name)
   (initial-element
    :initarg :initial-element
    :reader columnar-lane-definition-initial-element)
   (clear-on-remove-p
    :initarg :clear-on-remove-p
    :reader columnar-lane-definition-clear-on-remove-p))
  (:documentation
   "One generated column's name, element representation, and retention policy."))

(defmethod initialize-instance :after
    ((lane columnar-lane-definition) &key)
  (unless (symbolp (columnar-lane-definition-name lane))
    (error "A columnar lane needs a symbolic name, not ~S."
           (columnar-lane-definition-name lane)))
  (unless (typep (columnar-lane-definition-initial-element lane)
                 (math:declaration-representation-type lane))
    (error "Columnar lane ~S initial element ~S is not of type ~S."
           (columnar-lane-definition-name lane)
           (columnar-lane-definition-initial-element lane)
           (math:declaration-representation-type lane))))

(defclass columnar-layout-definition ()
  ((name
    :initarg :name
    :reader columnar-layout-definition-name)
   (lanes
    :initarg :lanes
    :reader columnar-layout-definition-lanes)
   (quantity-layout
    :initarg :quantity-layout
    :reader columnar-layout-definition-quantity-layout)
   (source-form
    :initarg :source-form
    :reader columnar-layout-definition-source-form))
  (:documentation
   "The physical lanes and fixed row meaning generated for a columnar type."))

(defmethod initialize-instance :after
    ((definition columnar-layout-definition) &key)
  (let ((lanes (columnar-layout-definition-lanes definition))
        (layout (columnar-layout-definition-quantity-layout definition)))
    (unless (and (consp lanes)
                 (every (lambda (lane)
                          (typep lane 'columnar-lane-definition))
                        lanes)
                 (= (length lanes)
                    (length
                     (remove-duplicates
                      lanes :key #'columnar-lane-definition-name :test #'eq))))
      (error "Columnar definition ~S needs distinct physical lanes."
             (columnar-layout-definition-name definition)))
    (when (and layout
               (/= (length lanes) (math:quantity-layout-extent layout)))
      (error "Columnar definition ~S has ~D lanes but a ~D-lane quantity layout."
             (columnar-layout-definition-name definition)
             (length lanes) (math:quantity-layout-extent layout)))))

(defgeneric columnar-layout-definition-for (name)
  (:documentation "Return the inspectable physical row layout named by NAME."))

(defmethod columnar-layout-definition-for (name)
  (declare (ignore name))
  nil)

(defun columnar-layout-lane-definition (definition lane-name)
  "Return DEFINITION's physical LANE-NAME description, or NIL."
  (find lane-name (columnar-layout-definition-lanes definition)
        :key #'columnar-lane-definition-name :test #'eq))

(defclass columnar-row-declaration ()
  ((layout-definition
    :initarg :layout-definition
    :reader columnar-row-declaration-layout-definition)
   (lane-declarations
    :initarg :lane-declarations
    :reader columnar-row-declaration-lane-declarations)
   (quantity-layout
    :initarg :quantity-layout
    :reader columnar-row-declaration-quantity-layout)
   (revision
    :initform (gensym "COLUMNAR-ROW-")
    :reader columnar-row-declaration-revision))
  (:documentation
   "Concrete per-lane declarations retained once by a columnar materialization.

Rows remain raw array elements; this object carries their checked meaning at
the aggregate boundary. #327W2B"))

(defun columnar-row-lane-declaration (row lane-name)
  "Return ROW's concrete declaration for LANE-NAME, or NIL."
  (cdr (assoc lane-name
              (columnar-row-declaration-lane-declarations row)
              :test #'eq)))

(defun signal-columnar-declaration-error (definition lane-name declaration reason)
  (error 'columnar-declaration-error
         :definition definition :lane-name lane-name
         :declaration declaration :reason reason))

(defun ensure-columnar-lane-representation
    (definition lane-name declaration physical-lane)
  (let ((actual (math:declaration-representation-type declaration))
        (expected (math:declaration-representation-type physical-lane)))
    (unless actual
      (signal-columnar-declaration-error
       definition lane-name declaration :missing-representation-type))
    (multiple-value-bind (subtype-p known-p) (subtypep actual expected)
      (unless (and known-p subtype-p)
        (signal-columnar-declaration-error
         definition lane-name declaration :incompatible-representation-type))))
  declaration)

(defun fixed-columnar-lane-quantity-p (definition lane-name)
  (let ((layout (columnar-layout-definition-quantity-layout definition))
        (position
          (position lane-name (columnar-layout-definition-lanes definition)
                    :key #'columnar-lane-definition-name :test #'eq)))
    (and layout position
         (find-if
          (lambda (projection)
            (member position
                    (math:quantity-projection-positions projection)))
          (math:quantity-layout-projections layout)))))

(defun make-columnar-row-declaration (definition &optional declarations)
  "Bind concrete represented DECLARATIONS to DEFINITION's physical lanes.

DECLARATIONS is an alist from lane names to represented-value declarations.
Representation compatibility and duplicate semantic ownership are checked
once; returned rows retain the concrete declarations without wrapping values."
  (check-type definition columnar-layout-definition)
  (let ((seen nil)
        (bindings nil))
    (dolist (binding declarations)
      (destructuring-bind (lane-name . declaration) binding
        (let ((physical
                (columnar-layout-lane-definition definition lane-name)))
          (unless physical
            (signal-columnar-declaration-error
             definition lane-name declaration :unknown-lane))
          (when (member lane-name seen :test #'eq)
            (signal-columnar-declaration-error
             definition lane-name declaration :duplicate-lane))
          (unless (typep declaration 'math:represented-value-declaration)
            (signal-columnar-declaration-error
             definition lane-name declaration :not-a-represented-value))
          (when (and (fixed-columnar-lane-quantity-p definition lane-name)
                     (math:declaration-quantity-checked-p declaration))
            (signal-columnar-declaration-error
             definition lane-name declaration :quantity-already-fixed))
          (ensure-columnar-lane-representation
           definition lane-name declaration physical)
          (push lane-name seen)
          (push (cons lane-name declaration) bindings))))
    (dolist (lane (columnar-layout-definition-lanes definition))
      (unless (member (columnar-lane-definition-name lane) seen :test #'eq)
        (push (cons (columnar-lane-definition-name lane) lane) bindings)))
    (make-instance
     'columnar-row-declaration
     :layout-definition definition
     :lane-declarations (nreverse bindings)
     :quantity-layout (columnar-layout-definition-quantity-layout definition))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun columnar-generated-symbol (name control &rest arguments)
    (intern (apply #'format nil control
                   (mapcar (lambda (argument)
                             (if (symbolp argument)
                                 (symbol-name argument)
                                 argument))
                           arguments))
            (symbol-package name)))

  (defun parse-columnar-lane (description)
    (destructuring-bind (name initial-element &key type clear-on-remove)
        description
      (unless (and (symbolp name) type)
        (error "A columnar lane needs a symbolic name and :TYPE: ~S"
               description))
      (list :name name :initial-element initial-element :type type
            :clear-on-remove-p (not (null clear-on-remove))
            :source-form description)))

  (defun parse-columnar-name-and-quantities (name-and-options)
    (if (symbolp name-and-options)
        (values name-and-options nil)
        (destructuring-bind (name &key quantities) name-and-options
          (values name quantities))))

  (defun columnar-lane-position (lane-name lanes)
    (or (position lane-name lanes :key (lambda (lane) (getf lane :name))
                  :test #'eq)
        (error "Unknown columnar quantity lane ~S." lane-name)))

  (defun columnar-quantity-layout-form (quantities lanes)
    (when quantities
      `(math:make-quantity-layout
        ,(length lanes)
        (list
         ,@(loop for (lane-names options) in quantities
                 collect
                 `(math:make-quantity-projection
                   ',(mapcar (lambda (lane-name)
                               (columnar-lane-position lane-name lanes))
                             lane-names)
                   (or (math:make-declared-quantity-specification ',options)
                       (error "Columnar projection needs quantity meaning: ~S"
                              ',(list lane-names options))))))))))

(defmacro define-columnar-buffer (name-and-options &body lane-descriptions)
  "Define a concrete synchronized structure-of-arrays buffer.

Each lane is (NAME INITIAL-ELEMENT :TYPE TYPE [:CLEAR-ON-REMOVE T]).  Optional
:QUANTITIES on NAME-AND-OPTIONS groups named physical lanes into fixed
quantity projections.  The generated MAKE-, -PUSH, -POP, and -RESET functions
operate on raw specialized arrays with one shared length and capacity. #LDP5UR"
  (multiple-value-bind (name quantities)
      (parse-columnar-name-and-quantities name-and-options)
    (let* ((lanes (mapcar #'parse-columnar-lane lane-descriptions))
           (lane-names (mapcar (lambda (lane) (getf lane :name)) lanes))
           (internal-constructor
             (columnar-generated-symbol name "%%MAKE-~A" name))
           (constructor (columnar-generated-symbol name "MAKE-~A" name))
           (grow (columnar-generated-symbol name "%~A-GROW" name))
           (push-name (columnar-generated-symbol name "~A-PUSH" name))
           (pop-name (columnar-generated-symbol name "~A-POP" name))
           (reset-name (columnar-generated-symbol name "~A-RESET" name))
           (length-slot (columnar-generated-symbol name "LENGTH"))
           (capacity-slot (columnar-generated-symbol name "CAPACITY"))
           (row-slot (columnar-generated-symbol name "ROW-DECLARATION"))
           (length-reader (columnar-generated-symbol name "~A-LENGTH" name))
           (capacity-reader
             (columnar-generated-symbol name "~A-CAPACITY" name))
           (lane-slots
             (mapcar (lambda (lane)
                       (columnar-generated-symbol
                        name "~A-LANE" (getf lane :name)))
                     lanes))
           (lane-readers
             (mapcar (lambda (slot)
                       (columnar-generated-symbol name "~A-~A" name slot))
                     lane-slots))
           (new-lane-variables
             (loop repeat (length lanes) collect (gensym "NEW-LANE")))
           (source-form
             `(define-columnar-buffer ,name-and-options ,@lane-descriptions))
           (layout-form (columnar-quantity-layout-form quantities lanes)))
      (unless (= (length lane-names)
                 (length (remove-duplicates lane-names :test #'eq)))
        (error "Columnar lane names must be distinct: ~S" lane-names))
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (defmethod columnar-layout-definition-for ((layout-name (eql ',name)))
             (declare (ignore layout-name))
             (load-time-value
              (make-instance
               'columnar-layout-definition
               :name ',name
               :lanes
               (list
                ,@(loop for lane in lanes
                        collect
                        `(make-instance
                          'columnar-lane-definition
                          :name ',(getf lane :name)
                          :representation-type ',(getf lane :type)
                          :initial-element ,(getf lane :initial-element)
                          :clear-on-remove-p ,(getf lane :clear-on-remove-p)
                          :source-form ',(getf lane :source-form))))
               :quantity-layout ,layout-form
               :source-form ',source-form))))

         (defstruct
             (,name
              (:constructor ,internal-constructor
                  (,length-slot ,capacity-slot ,row-slot ,@lane-slots))
              (:copier nil))
           (,length-slot 0 :type fixnum)
           (,capacity-slot 0 :type fixnum)
           (,row-slot nil :type columnar-row-declaration :read-only t)
           ;; Each lane slot carries its precise specialized array type, so
           ;; the generated push and every direct lane access store and
           ;; load raw elements without boxing them.
           ,@(loop for slot in lane-slots
                   for lane in lanes
                   collect `(,slot (make-array 0 :element-type ',(getf lane :type))
                            :type (simple-array
                                   ,(upgraded-array-element-type (getf lane :type))
                                   (*)))))

         (defun ,constructor
             (&key (capacity 16) declarations row-declaration)
           (check-type capacity (integer 0 #.most-positive-fixnum))
           (when (and declarations row-declaration)
             (error "Supply DECLARATIONS or ROW-DECLARATION, not both."))
           (let* ((definition (columnar-layout-definition-for ',name))
                  (row
                    (or row-declaration
                        (make-columnar-row-declaration definition declarations))))
             (unless (eq definition
                         (columnar-row-declaration-layout-definition row))
               (signal-columnar-declaration-error
                definition nil row :foreign-row-declaration))
             (,internal-constructor
              0 capacity row
              ,@(loop for lane in lanes
                      collect
                      `(make-array
                        capacity :element-type ',(getf lane :type)
                        :initial-element ,(getf lane :initial-element))))))

         (defun ,grow (buffer minimum-capacity)
           ;; Growth is rare, and one inlined REPLACE per lane over precisely
           ;; typed arrays makes SBCL's constraint propagation take minutes on
           ;; a buffer with a few dozen lanes.  Call REPLACE instead.
           (declare (notinline replace))
           (let* ((old-capacity (,capacity-reader buffer))
                  (new-capacity
                    (max minimum-capacity 1 (* 2 old-capacity)))
                  (length (,length-reader buffer))
                  ,@(loop for lane in lanes
                          for reader in lane-readers
                          for new in new-lane-variables
                          collect
                          `(,new
                            (let ((array
                                    (make-array
                                     new-capacity
                                     :element-type ',(getf lane :type)
                                     :initial-element
                                     ,(getf lane :initial-element))))
                              (replace array (,reader buffer)
                                       :end1 length :end2 length)
                              array))))
             (setf ,@(loop for reader in lane-readers
                           for new in new-lane-variables
                           append (list `(,reader buffer) new)))
             (setf (,capacity-reader buffer) new-capacity)
             buffer))

         (declaim (inline ,push-name ,pop-name))

         (defun ,push-name (buffer ,@lane-names)
           ,@(loop for lane in lanes
                   collect `(check-type ,(getf lane :name) ,(getf lane :type)))
           (let ((position (,length-reader buffer)))
             (when (= position (,capacity-reader buffer))
               (,grow buffer (1+ position)))
             (setf ,@(loop for reader in lane-readers
                           for lane-name in lane-names
                           append `((aref (,reader buffer) position)
                                    ,lane-name)))
             (setf (,length-reader buffer) (1+ position)))
           buffer)

         (defun ,pop-name (buffer)
           (let ((position (1- (,length-reader buffer))))
             (when (minusp position)
               (return-from ,pop-name
                 (values ,@(loop repeat (length lanes) collect nil) nil)))
             (let (,@(loop for lane-name in lane-names
                           for reader in lane-readers
                           collect `(,lane-name (aref (,reader buffer) position))))
               ,@(loop for lane in lanes
                       for reader in lane-readers
                       when (getf lane :clear-on-remove-p)
                         collect
                         `(setf (aref (,reader buffer) position)
                                ,(getf lane :initial-element)))
               (setf (,length-reader buffer) position)
               (values ,@lane-names t))))

         (defun ,reset-name (buffer)
           ,@(let ((clear-lanes
                     (loop for lane in lanes
                           for reader in lane-readers
                           when (getf lane :clear-on-remove-p)
                             collect (cons lane reader))))
               (when clear-lanes
                 `((let ((length (,length-reader buffer)))
                     ,@(loop for (lane . reader) in clear-lanes
                             collect
                             `(fill (,reader buffer)
                                    ,(getf lane :initial-element)
                                    :end length))))))
           (setf (,length-reader buffer) 0)
           buffer)

         ',name))))

(defmacro define-columnar-materialization
    (name-and-options &body lane-descriptions)
  "Define fixed columnar storage whose exact extent comes from one DOMAIN.

Each lane has the same syntax as DEFINE-COLUMNAR-BUFFER.  The generated
MAKE-NAME constructor takes DOMAIN first, asks DOMAIN-CARDINALITY for its exact
extent, checks the row declaration once, and allocates one specialized array
per lane.  The domain, row meaning, and arrays then travel together."
  (multiple-value-bind (name quantities)
      (parse-columnar-name-and-quantities name-and-options)
    (let* ((lanes (mapcar #'parse-columnar-lane lane-descriptions))
           (lane-names (mapcar (lambda (lane) (getf lane :name)) lanes))
           (internal-constructor
             (columnar-generated-symbol name "%%MAKE-~A" name))
           (constructor (columnar-generated-symbol name "MAKE-~A" name))
           (domain-slot (columnar-generated-symbol name "DOMAIN"))
           (row-slot (columnar-generated-symbol name "ROW-DECLARATION"))
           (domain-reader
             (columnar-generated-symbol name "~A-DOMAIN" name))
           (lane-slots
             (mapcar (lambda (lane)
                       (columnar-generated-symbol
                        name "~A-LANE" (getf lane :name)))
                     lanes))
           (source-form
             `(define-columnar-materialization
                  ,name-and-options ,@lane-descriptions))
           (layout-form (columnar-quantity-layout-form quantities lanes)))
      (unless (= (length lane-names)
                 (length (remove-duplicates lane-names :test #'eq)))
        (error "Columnar lane names must be distinct: ~S" lane-names))
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (defmethod columnar-layout-definition-for
               ((layout-name (eql ',name)))
             (declare (ignore layout-name))
             (load-time-value
              (make-instance
               'columnar-layout-definition
               :name ',name
               :lanes
               (list
                ,@(loop for lane in lanes
                        collect
                        `(make-instance
                          'columnar-lane-definition
                          :name ',(getf lane :name)
                          :representation-type ',(getf lane :type)
                          :initial-element ,(getf lane :initial-element)
                          :clear-on-remove-p ,(getf lane :clear-on-remove-p)
                          :source-form ',(getf lane :source-form))))
               :quantity-layout ,layout-form
               :source-form ',source-form))))

         (defstruct
             (,name
              (:constructor ,internal-constructor
                  (,domain-slot ,row-slot ,@lane-slots))
              (:copier nil))
           (,domain-slot nil :type t :read-only t)
           (,row-slot nil :type columnar-row-declaration :read-only t)
           ,@(loop for slot in lane-slots
                   for lane in lanes
                   collect `(,slot (make-array 0 :element-type ',(getf lane :type))
                            :type (simple-array
                                   ,(upgraded-array-element-type (getf lane :type))
                                   (*))
                            :read-only t)))

         (defun ,constructor (domain &key declarations row-declaration)
           (when (and declarations row-declaration)
             (error "Supply DECLARATIONS or ROW-DECLARATION, not both."))
           (let* ((extent (domains:domain-cardinality domain))
                  (definition (columnar-layout-definition-for ',name))
                  (row
                    (or row-declaration
                        (make-columnar-row-declaration definition declarations))))
             (check-type extent (integer 0 #.most-positive-fixnum))
             (unless (eq definition
                         (columnar-row-declaration-layout-definition row))
               (signal-columnar-declaration-error
                definition nil row :foreign-row-declaration))
             (,internal-constructor
              domain row
              ,@(loop for lane in lanes
                      collect
                      `(make-array
                        extent :element-type ',(getf lane :type)
                        :initial-element ,(getf lane :initial-element))))))

         (declaim (inline ,domain-reader))
         ',name))))

(defmacro with-columnar-buffer-storage
    ((bindings buffer buffer-type) &body body)
  "Borrow BUFFER-TYPE's active extent, row declaration, and raw lane arrays.

BINDINGS is (LENGTH ROW-DECLARATION (ARRAY LANE-NAME) ...).  The buffer is
evaluated once, and every array receives its precise specialized array type.
This is the checked aggregate boundary for closed scalar or SIMD kernels;
the kernel traverses the borrowed arrays without row objects. #VKLLPR"
  (destructuring-bind (length-binding row-binding &rest array-bindings)
      bindings
    (let* ((definition (columnar-layout-definition-for buffer-type))
           (lanes (and definition
                       (columnar-layout-definition-lanes definition))))
      (unless definition
        (error "There is no columnar buffer definition named ~S." buffer-type))
      (let ((resolved-bindings
              (loop for binding in array-bindings
                    collect
                    (destructuring-bind (variable lane-name) binding
                      (let ((lane
                              (find lane-name lanes
                                    :key #'columnar-lane-definition-name
                                    :test #'eq)))
                        (unless lane
                          (error "There is no ~S lane in ~S."
                                 lane-name buffer-type))
                        (list variable lane))))))
        (let ((buffer-value (gensym "BUFFER")))
          `(let ((,buffer-value ,buffer))
             (let ((,length-binding
                     (,(columnar-generated-symbol
                        buffer-type "~A-LENGTH" buffer-type)
                      ,buffer-value))
                   (,row-binding
                     (,(columnar-generated-symbol
                        buffer-type "~A-ROW-DECLARATION" buffer-type)
                      ,buffer-value))
                   ,@(loop for (variable lane) in resolved-bindings
                           collect
                           `(,variable
                             (,(columnar-generated-symbol
                                buffer-type "~A-~A-LANE" buffer-type
                                (columnar-lane-definition-name lane))
                              ,buffer-value))))
               (declare (type fixnum ,length-binding)
                        (type columnar-row-declaration ,row-binding)
                        ,@(loop for (variable lane) in resolved-bindings
                                collect
                                `(type
                                  (simple-array
                                   ,(upgraded-array-element-type
                                     (math:declaration-representation-type lane))
                                   (*))
                                  ,variable)))
               ,@body)))))))

(defmacro with-columnar-materialization-storage
    ((bindings materialization materialization-type) &body body)
  "Borrow a fixed materialization's domain, extent, row, and raw lane arrays.

BINDINGS is (DOMAIN EXTENT ROW-DECLARATION (ARRAY LANE-NAME) ...).  The
materialization is evaluated once and each array receives its precise
specialized array type."
  (destructuring-bind
      (domain-binding extent-binding row-binding &rest array-bindings)
      bindings
    (let* ((definition
             (columnar-layout-definition-for materialization-type))
           (lanes (and definition
                       (columnar-layout-definition-lanes definition))))
      (unless definition
        (error "There is no columnar layout definition named ~S."
               materialization-type))
      (let ((resolved-bindings
              (loop for binding in array-bindings
                    collect
                    (destructuring-bind (variable lane-name) binding
                      (let ((lane
                              (find lane-name lanes
                                    :key #'columnar-lane-definition-name
                                    :test #'eq)))
                        (unless lane
                          (error "There is no ~S lane in ~S."
                                 lane-name materialization-type))
                        (list variable lane))))))
        (let ((materialization-value (gensym "MATERIALIZATION")))
          `(let ((,materialization-value ,materialization))
             (let* ((,domain-binding
                      (,(columnar-generated-symbol
                         materialization-type "~A-DOMAIN"
                         materialization-type)
                       ,materialization-value))
                    (,extent-binding
                      (domains:domain-cardinality ,domain-binding))
                    (,row-binding
                      (,(columnar-generated-symbol
                         materialization-type "~A-ROW-DECLARATION"
                         materialization-type)
                       ,materialization-value))
                    ,@(loop for (variable lane) in resolved-bindings
                            collect
                            `(,variable
                              (,(columnar-generated-symbol
                                 materialization-type "~A-~A-LANE"
                                 materialization-type
                                 (columnar-lane-definition-name lane))
                               ,materialization-value))))
               (declare (type fixnum ,extent-binding)
                        (type columnar-row-declaration ,row-binding)
                        ,@(loop for (variable lane) in resolved-bindings
                                collect
                                `(type
                                  (simple-array
                                   ,(upgraded-array-element-type
                                     (math:declaration-representation-type lane))
                                   (*))
                                  ,variable)))
               ,@body)))))))

(defmacro with-columnar-buffer-row
    ((bindings buffer index buffer-type) &body body)
  "Bind one BUFFER-TYPE row's raw lane values at INDEX without allocation."
  (let* ((definition (columnar-layout-definition-for buffer-type))
         (lanes (and definition
                     (columnar-layout-definition-lanes definition))))
    (unless definition
      (error "There is no columnar buffer definition named ~S." buffer-type))
    (unless (= (length bindings) (length lanes))
      (error "~S needs ~D row bindings, not ~D."
             buffer-type (length lanes) (length bindings)))
    (let ((buffer-value (gensym "BUFFER"))
          (index-value (gensym "INDEX")))
      `(let ((,buffer-value ,buffer)
             (,index-value ,index))
         (let (,@(loop for binding in bindings
                       for lane in lanes
                       for reader = (columnar-generated-symbol
                                      buffer-type "~A-~A-LANE" buffer-type
                                      (columnar-lane-definition-name lane))
                       collect `(,binding (aref (,reader ,buffer-value)
                                                ,index-value))))
           (declare
            ,@(loop for binding in bindings
                    for lane in lanes
                    collect `(type ,(math:declaration-representation-type lane)
                                   ,binding)))
           ,@body)))))

(defmacro do-columnar-buffer-rows
    ((bindings buffer buffer-type &optional result) &body body)
  "Visit BUFFER-TYPE's active rows without constructing row objects."
  (let ((buffer-value (gensym "BUFFER"))
        (index (gensym "INDEX")))
    `(let ((,buffer-value ,buffer))
       (loop for ,index fixnum below
             (,(columnar-generated-symbol buffer-type "~A-LENGTH" buffer-type)
              ,buffer-value)
             do (with-columnar-buffer-row
                    (,bindings ,buffer-value ,index ,buffer-type)
                  ,@body)
             finally (return ,result)))))
