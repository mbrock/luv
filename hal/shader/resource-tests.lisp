(in-package #:luvcraft.tests)

(defun resource-interface-probe (&rest resources)
  (shader:parse-shader-specification
   'resource-interface-probe
   `(:stage :fragment :outputs ((result :vec4 :location 0)) :resources ,resources)
   '((set-output result (vec4 0.0 0.0 0.0 1.0)))))

(define-test shader-resource-linking-orders-locations-and-binds-by-role
  (let* ((first
           (resource-interface-probe
            '(camera :uniform-block :binding 7 :members ((position :vec4)))
            '(image :texture-2d :binding 2)))
         (second
           (resource-interface-probe
            '(:camera :uniform-block :binding 7
              :members ((position :vec4) (direction :vec4)))
            '(sampler :sampler :set 1 :binding 0)))
         (inputs (shader:link-shader-resources first second))
         (bindings (shader:bind-shader-resources inputs :sampler :s :camera :c :image :i)))
    (true (equal '(:image :camera :sampler) (mapcar #'shader:shader-resource-key inputs)))
    (true (= 32 (shader:shader-uniform-block-byte-size (second inputs))))
    (true (equal '(:i :c :s) (mapcar #'cdr bindings)))
    (true (equal '(2 7 0) (mapcar #'shader:shader-resource-binding inputs)))
    ;; Linking neither mutates a stage's prefix nor depends on stage order.
    (true (= 16 (shader:shader-uniform-block-byte-size
                 (first (shader:shader-specification-resources first)))))
    (true (equal inputs (shader:link-shader-resources second first)))
    (fail (shader:bind-shader-resources inputs :image :i))
    (fail (shader:bind-shader-resources inputs :image :i :camera :c :sampler :s :typo :x))
    (fail (shader:bind-shader-resources inputs :image :i :camera :c :sampler :s :camera :other))
    (fail (shader:bind-shader-resources inputs :image :i :camera nil :sampler :s))
    (fail (shader:bind-shader-resources inputs :image))))

(define-test shader-resource-linking-rejects-conflicting-identities-and-representations
  (flet ((conflict (left right)
           (fail (shader:link-shader-resources (resource-interface-probe left)
                                              (resource-interface-probe right)))))
    (conflict '(image :texture-2d :binding 0) '(image :texture-2d :binding 1))
    (conflict '(image :texture-2d :binding 0) '(image :texture-2d :set 1 :binding 0))
    (conflict '(image :texture-2d :binding 0) '(other :texture-2d :binding 0))
    (conflict '(image :texture-2d :binding 0) '(image :depth-texture-2d :binding 0))
    (conflict '(data :storage-buffer :binding 0 :element :vec4)
              '(data :storage-buffer :binding 0 :element :uvec4))
    (conflict '(camera :uniform-block :binding 0 :members ((position :vec4)))
              '(camera :uniform-block :binding 0 :members ((direction :vec4))))
    (conflict '(image :texture-2d :binding 0 :sample-transfer :identity)
              '(image :texture-2d :binding 0 :sample-transfer :srgb-to-linear))))
