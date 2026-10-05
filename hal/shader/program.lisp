;;; Shader programs: the stages that run together, linked by name.
;;;
;;; A specification describes one stage.  A program names the stages one
;;; pipeline runs -- vertex and fragment, vertex alone for a depth pass, or
;;; compute -- and linking checks what the stages must agree on before any
;;; ahead-of-time compiler writes them out: resource identities in the
;;; binding families Metal and Direct3D share (buffers, textures, samplers),
;;; the inter-stage interface, and which samplers compare depth.

(in-package #:luv.shader)

(defparameter *shader-program-stage-sets*
  '((:vertex) (:vertex :fragment) (:compute))
  "The stage combinations one program may name.")

(defclass shader-program (shader-named-object)
  ((stages
    :initarg :stages
    :reader shader-program-stages
    :documentation
    "A plist from stage keyword to the name of a DEFINE-SHADER function.")
   (source-pathname
    :initarg :source-pathname
    :initform nil
    :reader shader-program-source-pathname))
  (:documentation
   "A named set of stages compiled and bound together as one pipeline."))

(defmethod print-object ((program shader-program) stream)
  (print-unreadable-object (program stream :type t)
    (format stream "~S ~S" (shader-object-name program)
            (loop for (stage) on (shader-program-stages program) by #'cddr
                  collect stage))))

(defvar *shader-programs* nil
  "Every defined program, oldest first.  Redefinition keeps the position.")

(defun make-shader-program (name stages &key source-form source-pathname)
  (unless (and name (symbolp name))
    (error 'shader-language-error
           :form source-form :reason :invalid-program-name :details name))
  (unless (evenp (length stages))
    (error 'shader-language-error
           :form source-form :reason :odd-program-stage-list :details stages))
  (let ((keys (loop for (stage designator) on stages by #'cddr
                    do (unless (and designator (symbolp designator))
                         (error 'shader-language-error
                                :form source-form
                                :reason :invalid-program-stage-shader
                                :details (list stage designator)))
                    collect stage)))
    (unless (find-if (lambda (set)
                       (and (= (length set) (length keys))
                            (every (lambda (key) (member key keys)) set)))
                     *shader-program-stage-sets*)
      (error 'shader-language-error
             :form source-form :reason :invalid-program-stages
             :details keys)))
  (make-instance 'shader-program
                 :name name :stages (copy-list stages)
                 :source-form source-form :source-pathname source-pathname))

(defun register-shader-program (program)
  (let ((cell (member (shader-object-name program) *shader-programs*
                      :key #'shader-object-name)))
    (if cell
        (setf (car cell) program)
        (setf *shader-programs*
              (append *shader-programs* (list program))))
    program))

(defun find-shader-program (name &optional (errorp t))
  (or (find name *shader-programs* :key #'shader-object-name)
      (and errorp
           (error 'shader-language-error
                  :reason :undefined-shader-program :details name))))

(defmacro define-shader-program (name &rest stages)
  "Define NAME as a program of STAGES, a plist such as
\(:VERTEX TERRAIN-VERTEX :FRAGMENT TERRAIN-FRAGMENT\).  Each value names a
DEFINE-SHADER function; it is called when the program is linked, so
redefining a stage reaches the program without redefining it."
  `(progn
     (register-shader-program
      (make-shader-program ',name ',stages
                           :source-form '(define-shader-program ,name ,@stages)
                           :source-pathname
                           (or *compile-file-truename* *load-truename*)))
     ',name))

(defun shader-program-specification (program stage)
  "Return PROGRAM's current specification for STAGE, or NIL."
  (let ((designator (getf (shader-program-stages program) stage)))
    (when designator
      (unless (fboundp designator)
        (error 'shader-language-error
               :form (shader-object-source-form program)
               :reason :undefined-program-stage-shader
               :details (list stage designator)))
      (let ((specification (funcall designator)))
        (unless (typep specification 'shader-specification)
          (error 'shader-language-error
                 :form (shader-object-source-form program)
                 :reason :program-stage-not-a-shader
                 :details (list stage designator)))
        (unless (eq stage (shader-specification-stage specification))
          (error 'shader-language-error
                 :form (shader-object-source-form program)
                 :reason :program-stage-mismatch
                 :details (list stage designator
                                (shader-specification-stage specification))))
        specification))))

;;; Resource families.  Metal and Direct3D number buffers, textures, and
;;; samplers independently, so a uniform block and a texture may both be
;;; binding 0, while a uniform block and a storage buffer share one buffer
;;; index space.

(defun shader-resource-kind (resource)
  "The resource's kind keyword, before program-wide sampler analysis."
  (let ((type (shader-declaration-type resource)))
    (case (shader-type-opaque-kind type)
      (:texture-2d (shader-type-name type))
      (:storage-buffer (if (shader-storage-buffer-writable-p resource)
                           :read-write-storage-buffer
                           :storage-buffer))
      (otherwise (shader-type-opaque-kind type)))))

(defun shader-resource-family (resource)
  "Return :BUFFER, :TEXTURE, or :SAMPLER for RESOURCE's binding space."
  (ecase (shader-type-opaque-kind (shader-declaration-type resource))
    ((:uniform-block :storage-buffer) :buffer)
    (:texture-2d :texture)
    (:sampler :sampler)))

(defun shader-resource-target (expression)
  "The resource EXPRESSION denotes through references and bindings, or NIL."
  (loop
    (unless (typep expression 'shader-reference)
      (return nil))
    (let ((target (shader-reference-target expression)))
      (typecase target
        (shader-resource (return target))
        (shader-binding (setf expression (shader-binding-expression target)))
        (t (return nil))))))

(defun map-shader-specification-expressions (function specification)
  "Call FUNCTION once on every expression SPECIFICATION reaches, including
the bodies of inline functions and folds seen through their bindings."
  (let ((seen (make-hash-table :test #'eq)))
    (labels ((visit (expression)
               (unless (or (null expression) (gethash expression seen))
                 (setf (gethash expression seen) t)
                 (funcall function expression)
                 (when (typep expression 'shader-reference)
                   (let ((target (shader-reference-target expression)))
                     (when (typep target 'shader-binding)
                       (visit (shader-binding-expression target)))))
                 (mapc #'visit (lang:arithmetic-expression-children
                                expression))
                 (mapc #'visit (shader-expression-children expression)))))
      (dolist (binding (shader-specification-bindings specification))
        (visit (shader-binding-expression binding)))
      (dolist (statement (shader-specification-statements specification))
        (mapc #'visit (shader-statement-expressions statement))))))

(defun shader-sampler-uses (specification)
  "Return two lists: samplers used to sample, and samplers used to compare."
  (let ((sampling nil) (comparing nil))
    (map-shader-specification-expressions
     (lambda (expression)
       (when (typep expression 'shader-call)
         (let ((operator (shader-call-operator expression)))
           (when (member operator '(sample sample-compare))
             (let ((sampler (shader-resource-target
                             (second (shader-call-operands expression)))))
               (when sampler
                 (if (eq operator 'sample)
                     (pushnew sampler sampling)
                     (pushnew sampler comparing))))))))
     specification)
    (values sampling comparing)))

(defun shader-comparison-samplers (&rest specifications)
  "Return the keys of samplers SPECIFICATIONS use with SAMPLE-COMPARE.
One sampler cannot both filter colour and compare depth: Direct3D gives the
two uses different object types."
  (let ((sampling nil) (comparing nil))
    (dolist (specification specifications)
      (multiple-value-bind (plain comparison) (shader-sampler-uses specification)
        (dolist (sampler plain)
          (pushnew (shader-resource-key sampler) sampling))
        (dolist (sampler comparison)
          (pushnew (shader-resource-key sampler) comparing))))
    (let ((both (intersection sampling comparing)))
      (when both
        (error 'shader-language-error
               :reason :sampler-both-samples-and-compares
               :details both)))
    comparing))

;;; Linking.

(defclass shader-program-resource ()
  ((declaration
    :initarg :declaration
    :reader shader-program-resource-declaration
    :documentation "The longest compatible declaration among the stages.")
   (kind :initarg :kind :reader shader-program-resource-kind)
   (family :initarg :family :reader shader-program-resource-family)
   (stages
    :initarg :stages
    :accessor shader-program-resource-stages))
  (:documentation "One linked program input, with the stages that use it."))

(defun shader-program-resource-name (resource)
  (shader-object-name (shader-program-resource-declaration resource)))

(defun shader-program-resource-binding (resource)
  (shader-resource-binding (shader-program-resource-declaration resource)))

(defmethod print-object ((resource shader-program-resource) stream)
  (print-unreadable-object (resource stream :type t)
    (format stream "~S ~S ~D ~S"
            (shader-program-resource-name resource)
            (shader-program-resource-kind resource)
            (shader-program-resource-binding resource)
            (shader-program-resource-stages resource))))

(defclass shader-program-linkage ()
  ((program :initarg :program :reader shader-program-linkage-program)
   (specifications
    :initarg :specifications
    :reader shader-program-linkage-specifications
    :documentation "An alist from stage keyword to specification.")
   (resources
    :initarg :resources
    :reader shader-program-linkage-resources)
   (comparison-samplers
    :initarg :comparison-samplers
    :reader shader-program-linkage-comparison-samplers)
   (color-outputs
    :initarg :color-outputs
    :reader shader-program-linkage-color-outputs
    :documentation "Fragment outputs ordered by location."))
  (:documentation "A program whose stages were checked against each other."))

(defun shader-program-linkage-specification (linkage stage)
  (cdr (assoc stage (shader-program-linkage-specifications linkage))))

(defparameter *shader-family-order* '(:buffer :texture :sampler))

(defun link-shader-family-resources (stage-specifications comparison-samplers)
  (let ((names (make-hash-table :test #'eq))
        (locations (make-hash-table :test #'equal))
        (linked nil))
    (loop for (stage . specification) in stage-specifications do
      (dolist (resource (shader-specification-resources specification))
        (let* ((name (shader-resource-key resource))
               (family (shader-resource-family resource))
               (location (list family (shader-resource-binding resource)))
               (named (gethash name names))
               (located (gethash location locations)))
          (unless (zerop (shader-resource-descriptor-set resource))
            (error 'shader-language-error
                   :form (shader-object-source-form resource)
                   :reason :program-descriptor-set-not-zero
                   :details (shader-resource-descriptor-set resource)))
          (when (and located
                     (not (eq name (shader-program-resource-key located))))
            (error 'shader-language-error
                   :form (shader-object-source-form resource)
                   :reason :program-binding-collision
                   :details (list location
                                  (shader-program-resource-key located)
                                  name)))
          (cond
            ((null named)
             (let ((linked-resource
                     (make-instance
                      'shader-program-resource
                      :declaration resource
                      :kind (if (and (eq family :sampler)
                                     (member name comparison-samplers))
                                :comparison-sampler
                                (shader-resource-kind resource))
                      :family family :stages (list stage))))
               (setf (gethash name names) linked-resource
                     (gethash location locations) linked-resource)
               (push linked-resource linked)))
            (t
             (let ((declaration (shader-program-resource-declaration named)))
               (unless (equal location
                              (list (shader-program-resource-family named)
                                    (shader-resource-binding declaration)))
                 (error 'shader-language-error
                        :form (shader-object-source-form resource)
                        :reason :program-resource-moved
                        :details (list name
                                       (shader-resource-binding declaration)
                                       (shader-resource-binding resource))))
               (unless (shader-resource-compatible-p declaration resource)
                 (error 'shader-language-error
                        :form (shader-object-source-form resource)
                        :reason :incompatible-program-resource
                        :details (list name
                                       (shader-object-source-form declaration))))
               (when (and (typep resource 'shader-uniform-block)
                          (> (shader-uniform-block-byte-size resource)
                             (shader-uniform-block-byte-size declaration)))
                 (setf named
                       (make-instance
                        'shader-program-resource
                        :declaration resource
                        :kind (shader-program-resource-kind named)
                        :family family
                        :stages (shader-program-resource-stages named)))
                 (setf linked (substitute named (gethash name names) linked)
                       (gethash name names) named
                       (gethash location locations) named))
               (pushnew stage (shader-program-resource-stages named))))))))
    (dolist (resource linked)
      (setf (shader-program-resource-stages resource)
            (sort (copy-list (shader-program-resource-stages resource))
                  #'< :key (lambda (stage)
                             (position stage '(:vertex :fragment :compute))))))
    (sort linked
          (lambda (left right)
            (let ((lf (position (shader-program-resource-family left)
                                *shader-family-order*))
                  (rf (position (shader-program-resource-family right)
                                *shader-family-order*)))
              (or (< lf rf)
                  (and (= lf rf)
                       (< (shader-program-resource-binding left)
                          (shader-program-resource-binding right)))))))))

(defun shader-program-resource-key (resource)
  (shader-resource-key (shader-program-resource-declaration resource)))

(defun shader-interface-rasterized-p (declaration)
  "Whether DECLARATION flows from vertex to fragment by location."
  (and (shader-interface-location declaration)
       (null (shader-interface-built-in declaration))))

(defun check-shader-stage-interface (vertex fragment)
  "Every fragment input must be a vertex output of the same type and
interpolation at the same location."
  (dolist (input (shader-specification-inputs fragment))
    (when (shader-interface-rasterized-p input)
      (let ((output
              (find (shader-interface-location input)
                    (remove-if-not #'shader-interface-rasterized-p
                                   (shader-specification-outputs vertex))
                    :key #'shader-interface-location)))
        (unless output
          (error 'shader-language-error
                 :form (shader-object-source-form input)
                 :reason :fragment-input-without-vertex-output
                 :details (shader-interface-location input)))
        (unless (shader-type= (shader-declaration-type input)
                              (shader-declaration-type output))
          (error 'shader-language-error
                 :form (shader-object-source-form input)
                 :reason :stage-interface-type-mismatch
                 :details (list (shader-object-source-form output))))
        (unless (eq (shader-interface-interpolation input)
                    (shader-interface-interpolation output))
          (error 'shader-language-error
                 :form (shader-object-source-form input)
                 :reason :stage-interface-interpolation-mismatch
                 :details (list (shader-object-source-form output))))))))

(defun shader-color-outputs (fragment)
  (let ((outputs (sort (remove-if-not #'shader-interface-location
                                      (shader-specification-outputs fragment))
                       #'< :key #'shader-interface-location)))
    (loop for output in outputs
          for expected from 0
          unless (= expected (shader-interface-location output))
            do (error 'shader-language-error
                      :form (shader-object-source-form output)
                      :reason :sparse-color-outputs
                      :details (shader-interface-location output)))
    outputs))

(defun link-shader-program (program)
  "Check PROGRAM's stages against each other and return a linkage."
  (let* ((program (if (typep program 'shader-program)
                      program
                      (find-shader-program program)))
         (specifications
           (loop for stage in '(:vertex :fragment :compute)
                 for specification
                   = (shader-program-specification program stage)
                 when specification
                   collect (cons stage specification)))
         (vertex (cdr (assoc :vertex specifications)))
         (fragment (cdr (assoc :fragment specifications)))
         (comparison-samplers
           (apply #'shader-comparison-samplers
                  (mapcar #'cdr specifications))))
    (when (and vertex fragment)
      (check-shader-stage-interface vertex fragment))
    (make-instance 'shader-program-linkage
                   :program program
                   :specifications specifications
                   :resources (link-shader-family-resources
                               specifications comparison-samplers)
                   :comparison-samplers comparison-samplers
                   :color-outputs (and fragment
                                       (shader-color-outputs fragment)))))

(defun shader-family-binding-collisions (specification)
  "Return pairs of SPECIFICATION's resources that share a family binding."
  (let ((seen (make-hash-table :test #'equal))
        (collisions nil))
    (dolist (resource (shader-specification-resources specification))
      (let* ((location (list (shader-resource-family resource)
                             (shader-resource-binding resource)))
             (other (gethash location seen)))
        (if other
            (push (list other resource) collisions)
            (setf (gethash location seen) resource))))
    (nreverse collisions)))
