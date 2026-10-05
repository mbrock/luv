;;; Texture kinds and the operators that read and write them.
;;;
;;; A sampled texture has a dimension -- 2D, 2D array, cube, or 3D -- which
;;; fixes its coordinate type.  Every operator takes the texture first, then
;;; (for sampling) the sampler, then the coordinate; an array texture's layer
;;; index, an unsigned integer, always follows the coordinate.  What follows
;;; is the operator's own: a level of detail, a bias, two gradients, or a
;;; depth reference.
;;;
;;;   (sample albedo linear uv)                      ; vec4
;;;   (sample-level cascades linear uv layer 0.0)    ; explicit level
;;;   (sample-grad sky linear direction ddx ddy)     ; explicit gradients
;;;   (sample-compare shadows compare uv layer depth)
;;;   (gather heights nearest uv)                    ; four red texels
;;;   (texel-load volume (uvec3 x y z) level)        ; exact texel
;;;   (texture-size cascades)                        ; uvec3: w, h, layers
;;;
;;; A storage texture is declared with a texel format and read and written
;;; by exact coordinate: (TEXEL-LOAD IMAGE XY) and (SET-TEXEL IMAGE XY TEXEL).

(in-package #:luv.shader)

(defparameter *storage-texture-formats*
  '((:rgba32f :float 4) (:rgba16f :float 4) (:rgba8 :float 4)
    (:r32f :float 1) (:r32ui :uint 1))
  "(FORMAT SCALAR-KIND CHANNELS) for the storage texture formats: RGBA8 is
unsigned normalized.  A float format's texels are vec4 and R32UI's are uvec4;
missing channels read as zero, with one in alpha, and are ignored on store.")

(defvar *storage-texture-types* (make-hash-table :test #'equal)
  "Format-specific storage texture types, one per (designator . format), so
that two declarations of one format have EQ types.")

(defun storage-texture-type (declared format source-form)
  "The type of a storage texture declared as DECLARED with FORMAT."
  (let ((entry (assoc format *storage-texture-formats*)))
    (unless entry
      (error 'shader-language-error
             :form source-form :reason :invalid-storage-texture-format
             :details (list format
                            (mapcar #'first *storage-texture-formats*))))
    (let ((key (cons (shader-type-name declared) format)))
      (or (gethash key *storage-texture-types*)
          (setf (gethash key *storage-texture-types*)
                (make-instance
                 'shader-type
                 :name (shader-type-name declared)
                 :opaque-kind :storage-texture
                 :texture-dimension (shader-type-texture-dimension declared)
                 :sample-result-type (ecase (second entry)
                                       (:float :vec4)
                                       (:uint :uvec4))
                 :storage-format format))))))

(defun storage-texture-format-channels (format)
  (third (assoc format *storage-texture-formats*)))

(defun shader-texture-type-p (type)
  "Whether TYPE is a sampled (read-only) texture."
  (eq :texture (shader-type-opaque-kind (find-shader-type type))))

(defun shader-storage-texture-type-p (type)
  (eq :storage-texture (shader-type-opaque-kind (find-shader-type type))))

(defun shader-texture-arrayed-p (type)
  (eq :2d-array (shader-type-texture-dimension (find-shader-type type))))

(defun texture-coordinate-type (type)
  "The floating coordinate that samples TYPE: UV, or a direction or UVW."
  (ecase (shader-type-texture-dimension (find-shader-type type))
    ((:2d :2d-array) :vec2)
    ((:cube :3d) :vec3)))

(defun texture-texel-type (type)
  "The unsigned texel coordinate of TYPE, or NIL when it has none (a cube)."
  (ecase (shader-type-texture-dimension (find-shader-type type))
    ((:2d :2d-array) :uvec2)
    (:3d :uvec3)
    (:cube nil)))

(defun texture-size-type (type)
  "Width and height, then layers or depth for arrays and volumes."
  (ecase (shader-type-texture-dimension (find-shader-type type))
    ((:2d :cube) :uvec2)
    ((:2d-array :3d) :uvec3)))

(defun texture-gather-type (type)
  (let ((result (find-shader-type (shader-type-sample-result-type
                                   (find-shader-type type)))))
    (if (eq :uint (shader-type-scalar-kind result)) :uvec4 :vec4)))

(define-shader-operator sample-level
  "Sample a texture at an explicit level of detail.")
(define-shader-operator sample-grad
  "Sample a texture with explicit coordinate derivatives along x and y.")
(define-shader-operator sample-bias
  "Sample a texture in a fragment stage with a bias added to its level.")
(define-shader-operator gather
  "Gather the first channel of the four texels a bilinear sample would read.")
(define-shader-operator gather-compare
  "Compare a depth reference against the four texels of a bilinear footprint.")
(define-shader-operator texture-size
  "The size in texels of a texture's level: uvec2, or uvec3 with the layer
count of an array or the depth of a volume.")
(define-shader-operator set-texel
  "Store one texel of a read-write storage texture (a statement).")

(defun check-texture-operands
    (operator operands source-form &key kinds dimensions sampler
                                        float-result depth required optional)
  "Check OPERANDS: a texture of one of KINDS (:TEXTURE, :STORAGE-TEXTURE)
and DIMENSIONS, then a sampler when SAMPLER, then REQUIRED operand types and
up to OPTIONAL trailing ones.  REQUIRED and OPTIONAL are functions of the
texture's type, so a layer can appear exactly for arrays."
  (let* ((texture (first operands))
         (type (and texture (shader-expression-type texture))))
    (flet ((fail (&optional details)
             (error 'shader-language-error
                    :form source-form :reason :invalid-texture-operation
                    :details (or details
                                 (list operator
                                       (mapcar (lambda (operand)
                                                 (shader-type-name
                                                  (shader-expression-type
                                                   operand)))
                                               operands))))))
      (unless (and type (member (shader-type-opaque-kind type) kinds))
        (fail))
      (unless (member (shader-type-texture-dimension type) dimensions)
        (fail (list operator :dimension
                    (shader-type-texture-dimension type))))
      (when (and depth (not (shader-type-image-depth-p type)))
        (fail (list operator :requires-depth-texture)))
      (when (and float-result
                 (not (eq :float (shader-type-scalar-kind
                                  (find-shader-type
                                   (shader-type-sample-result-type type))))))
        (fail (list operator :requires-float-texture)))
      (let ((rest (rest operands)))
        (when sampler
          (unless (and rest (eq :sampler (shader-type-opaque-kind
                                          (shader-expression-type
                                           (first rest)))))
            (fail))
          (setf rest (rest rest)))
        (let ((required (funcall required type))
              (optional (and optional (funcall optional type))))
          (unless (<= (length required) (length rest)
                      (+ (length required) (length optional)))
            (fail))
          (loop for operand in rest
                for expected in (append required optional)
                unless (shader-type= expected (shader-expression-type operand))
                  do (fail))))
      type)))

(defun with-layer (type &rest types)
  "TYPES with the unsigned layer index inserted first, for an array TYPE."
  (if (shader-texture-arrayed-p type) (cons :uint types) types))

(defmethod infer-shader-call-type ((operator (eql 'sample)) operands source-form)
  (let ((type (check-texture-operands
               operator operands source-form
               :kinds '(:texture) :dimensions '(:2d :2d-array :cube :3d)
               :sampler t
               :required (lambda (type)
                           (cons (texture-coordinate-type type)
                                 (with-layer type))))))
    (find-shader-type (shader-type-sample-result-type type))))

(defmethod infer-shader-call-type
    ((operator (eql 'sample-level)) operands source-form)
  (let ((type (check-texture-operands
               operator operands source-form
               :kinds '(:texture) :dimensions '(:2d :2d-array :cube :3d)
               :sampler t :float-result t
               :required (lambda (type)
                           (cons (texture-coordinate-type type)
                                 (with-layer type :float))))))
    (find-shader-type (shader-type-sample-result-type type))))

(defmethod infer-shader-call-type
    ((operator (eql 'sample-bias)) operands source-form)
  (let ((type (check-texture-operands
               operator operands source-form
               :kinds '(:texture) :dimensions '(:2d :2d-array :cube :3d)
               :sampler t :float-result t
               :required (lambda (type)
                           (cons (texture-coordinate-type type)
                                 (with-layer type :float))))))
    (find-shader-type (shader-type-sample-result-type type))))

(defmethod infer-shader-call-type
    ((operator (eql 'sample-grad)) operands source-form)
  (let ((type (check-texture-operands
               operator operands source-form
               :kinds '(:texture) :dimensions '(:2d :2d-array :cube :3d)
               :sampler t :float-result t
               :required (lambda (type)
                           (let ((coordinate (texture-coordinate-type type)))
                             (cons coordinate
                                   (with-layer type coordinate
                                     coordinate)))))))
    (find-shader-type (shader-type-sample-result-type type))))

(defmethod infer-shader-call-type
    ((operator (eql 'sample-compare)) operands source-form)
  (check-texture-operands
   operator operands source-form
   :kinds '(:texture) :dimensions '(:2d :2d-array) :sampler t :depth t
   :required (lambda (type)
               (cons :vec2 (with-layer type :float))))
  (find-shader-type :float))

(defmethod infer-shader-call-type ((operator (eql 'gather)) operands source-form)
  (let ((type (check-texture-operands
               operator operands source-form
               :kinds '(:texture) :dimensions '(:2d :2d-array :cube)
               :sampler t
               :required (lambda (type)
                           (cons (texture-coordinate-type type)
                                 (with-layer type))))))
    (find-shader-type (texture-gather-type type))))

(defmethod infer-shader-call-type
    ((operator (eql 'gather-compare)) operands source-form)
  (check-texture-operands
   operator operands source-form
   :kinds '(:texture) :dimensions '(:2d :2d-array) :sampler t :depth t
   :required (lambda (type)
               (cons :vec2 (with-layer type :float))))
  (find-shader-type :vec4))

(defmethod infer-shader-call-type
    ((operator (eql 'texel-load)) operands source-form)
  (let ((type (check-texture-operands
               operator operands source-form
               :kinds '(:texture :storage-texture)
               :dimensions '(:2d :2d-array :3d)
               :required (lambda (type)
                           (cons (texture-texel-type type) (with-layer type)))
               ;; Sampled textures name a mip level; storage textures are
               ;; one level as bound.
               :optional (lambda (type)
                           (and (shader-texture-type-p type) '(:uint))))))
    (find-shader-type (shader-type-sample-result-type type))))

(defmethod infer-shader-call-type
    ((operator (eql 'texture-size)) operands source-form)
  (let ((type (check-texture-operands
               operator operands source-form
               :kinds '(:texture :storage-texture)
               :dimensions '(:2d :2d-array :cube :3d)
               :required (constantly nil)
               :optional (lambda (type)
                           (and (shader-texture-type-p type) '(:uint))))))
    (find-shader-type (texture-size-type type))))

;;; Meaning.  Sampling keeps the texture's declared sample meaning and asks
;;; for a dimensionless coordinate, as SAMPLE does; gathers and sizes are
;;; raw representations.

(defun check-sample-coordinate-quantity (operands source-form)
  (let ((coordinate (third operands)))
    (when (shader-expression-quantity-checked-p coordinate)
      (require-semantic-operands operands source-form '(2))
      (require-dimensionless-coordinate
       (shader-expression-quantity-specification coordinate) source-form))))

(defun texture-operand-resource (operands)
  (shader-resource-target (first operands)))

(macrolet ((define-sampling-quantities (&rest operators)
             `(progn
                ,@(loop for operator in operators
                        collect
                        `(defmethod infer-shader-call-quantity-specification
                             ((operator (eql ',operator)) operands source-form)
                           (check-sample-coordinate-quantity operands
                                                             source-form)
                           (let ((texture (texture-operand-resource operands)))
                             (and texture
                                  (shader-resource-sample-quantity-specification
                                   texture))))
                        collect
                        `(defmethod infer-shader-call-quantity-layout
                             ((operator (eql ',operator)) operands source-form)
                           (declare (ignore source-form))
                           (let ((texture (texture-operand-resource operands)))
                             (and texture
                                  (shader-resource-sample-quantity-layout
                                   texture))))))))
  (define-sampling-quantities sample-level sample-bias sample-grad))

(defmethod infer-shader-call-quantity-specification
    ((operator (eql 'gather)) operands source-form)
  (check-sample-coordinate-quantity operands source-form)
  nil)

(defmethod infer-shader-call-quantity-specification
    ((operator (eql 'gather-compare)) operands source-form)
  (infer-shader-call-quantity-specification
   'sample-compare operands source-form))

(defmethod infer-shader-call-quantity-specification
    ((operator (eql 'texture-size)) operands source-form)
  (declare (ignore operands source-form))
  nil)

;;; Storage texture stores.

(defmethod parse-shader-statement
    ((operator (eql 'set-texel)) stage form environment context)
  (declare (ignore context))
  (unless (member stage '(:compute :fragment))
    (error 'shader-language-error
           :form form :reason :invalid-statement-for-stage
           :details (list 'set-texel stage)))
  (unless (= (length form) 4)
    (error 'shader-language-error :form form :reason :set-texel-arity))
  (let* ((texture (shader-environment-value (second form) environment form))
         (coordinate (parse-shader-expression (third form) environment))
         (value (parse-shader-expression (fourth form) environment))
         (type (and (typep texture 'shader-resource)
                    (shader-declaration-type texture))))
    (unless (and type (shader-storage-texture-type-p type))
      (error 'shader-language-error
             :form form :reason :not-storage-texture :details (second form)))
    (unless (shader-type= (texture-texel-type type)
                          (shader-expression-type coordinate))
      (error 'shader-language-error
             :form form :reason :texel-coordinate-type
             :details (shader-type-name (shader-expression-type coordinate))))
    (unless (shader-type= (shader-type-sample-result-type type)
                          (shader-expression-type value))
      (error 'shader-language-error
             :form form :reason :texel-type-mismatch
             :details (list (shader-type-sample-result-type type)
                            (shader-type-name
                             (shader-expression-type value)))))
    (make-instance 'shader-texel-store
                   :texture texture :coordinate coordinate :value value
                   :source-form form)))
