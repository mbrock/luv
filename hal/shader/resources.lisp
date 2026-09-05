(in-package #:luv.shader)

;;; A program's inputs are the union of its stages' resource declarations.
;;; Names identify roles for the host; set/binding pairs locate them for the
;;; GPU. Linking checks both identities before any backend allocates a layout.

(defun link-shader-resources (&rest specifications)
  "Return resources ordered by set and binding, checking shared declarations.
Names are package-independent, case-insensitive roles. Uniform blocks may
expose different prefixes of one layout; retain the longest compatible view.
The returned declarations belong to the specifications and must not be mutated."
  (let ((names (make-hash-table :test #'equal))
        (locations (make-hash-table :test #'equal)))
    (dolist (specification specifications)
      (dolist (resource (shader-specification-resources specification))
        (let* ((name (shader-resource-key resource))
               (location (shader-resource-location resource))
               (named (gethash name names))
               (located (gethash location locations)))
          (when (and named (not (equal location (shader-resource-location named))))
            (error "Shader input ~S occupies both ~S and ~S."
                   name (shader-resource-location named) location))
          (when (and located (not (eq name (shader-resource-key located))))
            (error "Shader inputs ~S and ~S share set/binding ~S."
                   (shader-resource-key located) name location))
          (when (and named (not (shader-resource-compatible-p named resource)))
            (error "Incompatible declarations of shader input ~S at ~S: ~S and ~S."
                   name location (shader-object-source-form named)
                   (shader-object-source-form resource)))
          (when (or (null named)
                    (and (typep resource 'shader-uniform-block)
                         (> (shader-uniform-block-byte-size resource)
                            (shader-uniform-block-byte-size named))))
            (setf (gethash name names) resource
                  (gethash location locations) resource)))))
    (sort (loop for resource being the hash-values of names collect resource)
          #'shader-resource-location<)))

(defun bind-shader-resources (resources &rest arguments)
  "Resolve a name/value plist into (declaration . value) pairs in layout order.
Reject missing, unknown, repeated, and NIL inputs. Values remain opaque here;
resource-handle validation belongs to the host/backend, not the shader DSL."
  (unless (evenp (length arguments))
    (error "Shader inputs require name/value pairs: ~S." arguments))
  (let ((values (make-hash-table :test #'eq)))
    (loop for (name value) on arguments by #'cddr
          for key = (shader-input-key name) do
            (unless (find key resources :key #'shader-resource-key)
              (error "Unknown shader input ~S; expected ~S."
                     name (mapcar #'shader-resource-key resources)))
            (when (nth-value 1 (gethash key values))
              (error "Repeated shader input ~S." name))
            (unless value (error "Shader input ~S cannot be NIL." name))
            (setf (gethash key values) value))
    (loop for resource in resources
          for key = (shader-resource-key resource)
          collect (cons resource
                        (or (gethash key values)
                            (error "Missing shader input ~S." key))))))

;;; Identity and compatibility are independent of any backend's descriptors.

(defun shader-input-key (name)
  (check-type name symbol)
  (intern (string-upcase (symbol-name name)) '#:keyword))

(defun shader-resource-key (resource)
  "The keyword spelling of RESOURCE's semantic role, independent of package."
  (shader-input-key (shader-object-name resource)))

(defun shader-resource-location (resource)
  (list (shader-resource-descriptor-set resource) (shader-resource-binding resource)))

(defun shader-resource-location< (left right)
  (let ((ls (shader-resource-descriptor-set left))
        (rs (shader-resource-descriptor-set right)))
    (or (< ls rs)
        (and (= ls rs) (< (shader-resource-binding left) (shader-resource-binding right))))))

(defun optional-shader-semantics= (comparison left right)
  (if (and left right) (funcall comparison left right) (eq left right)))

(defgeneric shader-resource-compatible-p (left right)
  (:documentation "Whether two declarations can describe the same program input."))

(defmethod shader-resource-compatible-p ((left shader-resource) (right shader-resource))
  (and (shader-type= (shader-declaration-type left) (shader-declaration-type right))
       (eql (shader-resource-sample-transfer left) (shader-resource-sample-transfer right))
       (optional-shader-semantics=
        #'math:quantity-specification=
        (shader-resource-sample-quantity-specification left)
        (shader-resource-sample-quantity-specification right))
       (optional-shader-semantics=
        #'math:quantity-layout=
        (shader-resource-sample-quantity-layout left)
        (shader-resource-sample-quantity-layout right))))

(defmethod shader-resource-compatible-p
    ((left shader-storage-buffer) (right shader-storage-buffer))
  (and (call-next-method)
       (shader-type= (shader-storage-buffer-element-type left)
                     (shader-storage-buffer-element-type right))))

(defmethod shader-resource-compatible-p
    ((left shader-uniform-block) (right shader-uniform-block))
  (and (call-next-method)
       (loop for l in (shader-uniform-block-members left)
             for r in (shader-uniform-block-members right)
             always (and (string-equal (shader-object-name l) (shader-object-name r))
                         (= (shader-uniform-member-offset l) (shader-uniform-member-offset r))
                         (shader-type= (shader-declaration-type l) (shader-declaration-type r))
                         (optional-shader-semantics=
                          #'math:quantity-specification=
                          (shader-declaration-quantity-specification l)
                          (shader-declaration-quantity-specification r))
                         (optional-shader-semantics=
                          #'math:quantity-layout=
                          (shader-declaration-quantity-layout l)
                          (shader-declaration-quantity-layout r))))))
