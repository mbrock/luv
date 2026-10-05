;;;; Executable claims for texture kinds, storage textures, fragment
;;;; built-ins, and compute effects: what the language accepts and rejects,
;;;; and their SPIR-V, checked by spirv-val when it is installed.  The MSL
;;;; and WGSL claims use the same probes.

(in-package #:luvcraft.tests)

(defun spirv-val-diagnostics (specification &key (environment "vulkan1.1"))
  "Assemble SPECIFICATION and validate it.  Return NIL on success or without
spirv-val, else its report."
  (when (ignore-errors
         (zerop (nth-value 2 (uiop:run-program '("spirv-val" "--version")
                                               :ignore-error-status t))))
    (uiop:with-temporary-file (:pathname pathname :type "spv" :keep nil)
      (spv:write-spir-v (spv:assemble-shader-specification specification)
                        pathname)
      (multiple-value-bind (output error-output status)
          (uiop:run-program (list "spirv-val" "--target-env" environment
                                  (uiop:native-namestring pathname))
                            :output :string :error-output :string
                            :ignore-error-status t)
        (unless (zerop status)
          (format nil "~A~A" output error-output))))))

(defun spir-v-instruction-names (specification)
  (mapcar #'spv:instruction-name
          (spv:lower-spir-v (spv:shader-module specification))))

(defun spir-v-forms-text (specification)
  (write-to-string
   (mapcar #'spv:instruction-form
           (spv:lower-spir-v (spv:shader-module specification)))))

(defun effect-failure-reason (thunk)
  (handler-case (progn (funcall thunk) nil)
    (shader:shader-language-error (condition)
      (shader:shader-language-error-reason condition))))

(defun effect-probe (options &rest body)
  (shader:parse-shader-specification 'effect-probe options body))

(shader:define-shader texture-kinds-fragment-probe
    (:stage :fragment
     :inputs ((uv :vec2 :location 0)
              (pixel :vec4 :built-in :frag-coord)
              (front :bool :built-in :front-facing)
              (sample-number :uint :built-in :sample-index))
     :outputs ((color :vec4 :location 0)
               (depth :float :built-in :frag-depth))
     :resources ((albedo :texture-2d :binding 0)
                 (cascades :depth-texture-2d-array :binding 1)
                 (sky :texture-cube :binding 2)
                 (volume :texture-3d :binding 3)
                 (layers :texture-2d-array :binding 4)
                 (heights :depth-texture-2d :binding 5)
                 (linear-clamp :sampler :binding 0)
                 (shadow :sampler :binding 3)))
  (let* ((layer (uint (swizzle pixel :x)))
         (direction (shader:vec3 uv 1.0))
         (base (sample albedo linear-clamp uv))
         (level (shader:sample-level layers linear-clamp uv layer 2.0))
         (biased (shader:sample-bias albedo linear-clamp uv 0.5))
         (graded (shader:sample-grad sky linear-clamp direction direction
                                     direction))
         (lit (sample-compare cascades shadow uv layer 0.5))
         (lit-gather (shader:gather-compare cascades shadow uv layer 0.5))
         (reds (shader:gather albedo linear-clamp uv))
         (depths (shader:gather heights linear-clamp uv))
         (fog (shader:sample-level volume linear-clamp direction 0.0))
         (texel (texel-load volume (shader:uvec3 layer layer layer)
                            (uint 1.0)))
         (array-texel (texel-load layers (uvec2 layer layer) layer
                                  (uint 0.0)))
         (size (shader:texture-size cascades))
         (sky-size (shader:texture-size sky (uint 1.0))))
    (when (< (swizzle base :a) 0.5)
      (shader:discard))
    (set-output
     color
     (* (+ base level biased graded lit-gather reds depths fog texel
           array-texel
           (vec4 (float (swizzle size :z)) (float (swizzle sky-size :y))
                 (float sample-number) 0.0))
        lit (if front 1.0 0.5)))
    (set-output depth (swizzle pixel :z))))

(shader:define-shader workgroup-effects-probe
    (:stage :compute
     :workgroup-size (64 1 1)
     :inputs ((cell :uvec3 :built-in :global-invocation-id)
              (local :uint :built-in :local-invocation-index)
              (lane :uint :built-in :wave-lane-index)
              (lanes :uint :built-in :wave-lane-count))
     :shared ((tile :vec4 64)
              (counts :uint 4))
     :resources ((counter :storage-buffer :binding 0 :element :uint
                          :access :read-write)
                 (values :storage-buffer :binding 1 :element :vec4
                         :access :read-write)
                 (image :read-write-texture-2d :binding 0 :format :rgba16f)
                 (mask :read-write-texture-2d :binding 1 :format :r32ui)))
  (let* ((index (swizzle cell :x))
         (zero (uint 0.0))
         (one (uint 1.0))
         (texel (uvec2 index zero))
         (loaded (texel-load image texel))
         (size (shader:texture-size image)))
    (shader:set-shared-element tile local loaded)
    (when (= local zero)
      (shader:set-shared-element counts zero zero))
    (shader:workgroup-barrier)
    (shader:atomic-add counts zero one)
    (let* ((neighbour (shader:shared-element
                       tile (mod (+ local one) (uint 64.0))))
           (slot (shader:atomic-add counter zero one))
           (low (shader:atomic-min counter one index))
           (high (shader:atomic-max counter one index))
           (bits (shader:atomic-or counter one index))
           (swapped (shader:atomic-exchange counter one index))
           (compared (shader:atomic-compare-exchange counter one zero index))
           (sum (shader:wave-active-sum (float index)))
           (prefix (shader:wave-prefix-sum index))
           (ballot (shader:wave-ballot (< index (uint 3.0)))))
      (shader:storage-barrier)
      (shader:set-buffer-element
       values slot
       (+ neighbour
          (vec4 sum (float (+ prefix lane lanes))
                (float (+ (swizzle ballot :x) low high bits))
                (float (+ swapped compared (swizzle size :y))))))
      (shader:set-texel image texel neighbour)
      (shader:set-texel mask texel (shader:uvec4 slot zero zero zero)))))

(define-test texture-kinds-take-layers-levels-and-gradients-by-type
  (let ((specification (texture-kinds-fragment-probe)))
    (true (equal '(:2d :2d-array :cube :3d :2d-array :2d nil nil)
                 (mapcar (lambda (resource)
                           (shader:shader-type-texture-dimension
                            (shader:shader-declaration-type resource)))
                         (shader:shader-specification-resources
                          specification))))
    (flet ((reason (resources &rest body)
             (effect-failure-reason
              (lambda ()
                (apply #'effect-probe
                       `(:stage :fragment
                         :inputs ((uv :vec2 :location 0))
                         :outputs ((color :vec4 :location 0))
                         :resources ,resources)
                       body)))))
      ;; An array's layer follows the coordinate; it may not be left out.
      (true (eq :invalid-texture-operation
                (reason '((layers :texture-2d-array :binding 0)
                          (filter :sampler :binding 0))
                        '(set-output color (sample layers filter uv)))))
      ;; Cubes have no texel coordinates.
      (true (eq :invalid-texture-operation
                (reason '((sky :texture-cube :binding 0))
                        '(set-output color
                          (texel-load sky (uvec2 (uint 0.0) (uint 0.0)))))))
      ;; Comparison needs a depth texture.
      (true (eq :invalid-texture-operation
                (reason '((image :texture-2d :binding 0)
                          (filter :sampler :binding 3))
                        '(set-output color
                          (vec4 (sample-compare image filter uv 0.5))))))
      ;; Only fragments have the derivatives a bias adjusts.
      (true (eq :sample-bias-outside-fragment
                (effect-failure-reason
                 (lambda ()
                   (effect-probe
                    '(:stage :vertex
                      :inputs ((index :uint :built-in :vertex-index))
                      :outputs ((position :vec4 :built-in :position))
                      :resources ((image :texture-2d :binding 0)
                                  (filter :sampler :binding 0)))
                    '(set-output position
                      (shader:sample-bias image filter
                       (vec2 (float index) 0.0) 1.0))))))))))

(define-test storage-textures-declare-formats-that-fix-their-texels
  (let* ((specification (workgroup-effects-probe))
         (resources (shader:shader-specification-resources specification))
         (image (find "IMAGE" resources :key (lambda (resource)
                                               (symbol-name
                                                (shader:shader-object-name
                                                 resource)))
                                        :test #'string=))
         (type (shader:shader-declaration-type image)))
    (true (shader:shader-storage-texture-type-p type))
    (true (eq :rgba16f (shader:shader-type-storage-format type)))
    (true (eq :read-write-texture-2d (shader:shader-resource-kind image)))
    (true (eq :storage-texture (shader:shader-resource-family image)))
    ;; One format has one type, so stages linking one texture agree.
    (true (eq type
              (shader:shader-declaration-type
               (first (shader:shader-specification-resources
                       (effect-probe
                        '(:stage :compute :workgroup-size (1 1 1)
                          :resources ((image :read-write-texture-2d
                                       :binding 0 :format :rgba16f)))
                        '(shader:set-texel image
                          (uvec2 (uint 0.0) (uint 0.0))
                          (vec4 0.0 0.0 0.0 0.0)))))))))
  (flet ((reason (resources &rest body)
           (effect-failure-reason
            (lambda ()
              (apply #'effect-probe
                     `(:stage :compute :workgroup-size (1 1 1)
                       :resources ,resources)
                     body)))))
    (true (eq :invalid-storage-texture-format
              (reason '((image :read-write-texture-2d :binding 0))
                      '(shader:set-texel image (uvec2 (uint 0.0) (uint 0.0))
                        (vec4 0.0 0.0 0.0 0.0)))))
    (true (eq :format-on-non-storage-texture
              (reason '((image :texture-2d :binding 0 :format :r32f))
                      '(shader:workgroup-barrier))))
    ;; R32UI texels are unsigned.
    (true (eq :texel-type-mismatch
              (reason '((mask :read-write-texture-2d :binding 0
                         :format :r32ui))
                      '(shader:set-texel mask (uvec2 (uint 0.0) (uint 0.0))
                        (vec4 0.0 0.0 0.0 0.0)))))
    (true (eq :not-storage-texture
              (reason '((image :texture-2d :binding 0))
                      '(shader:set-texel image (uvec2 (uint 0.0) (uint 0.0))
                        (vec4 0.0 0.0 0.0 0.0)))))))

(define-test effects-have-a-place-in-the-statement-sequence
  (flet ((compute (body &key shared resources inputs)
           (effect-failure-reason
            (lambda ()
              (effect-probe
               `(:stage :compute :workgroup-size (64 1 1)
                 :inputs ,(or inputs
                              '((cell :uvec3 :built-in
                                 :global-invocation-id)))
                 :shared ,shared
                 :resources
                 ,(or resources
                      '((counter :storage-buffer :binding 0 :element :uint
                         :access :read-write))))
               body)))))
    ;; An atomic is a binding's whole value or a statement, never a
    ;; subexpression whose evaluation the backends might repeat.
    (true (eq :effect-requires-statement-binding
              (compute '(let* ((slot (+ (uint 1.0)
                                        (shader:atomic-add counter
                                         (uint 0.0) (uint 1.0)))))
                         (shader:set-buffer-element counter slot slot)))))
    (true (null (compute '(let* ((slot (shader:atomic-add counter (uint 0.0)
                                        (uint 1.0))))
                           (shader:set-buffer-element counter slot slot)))))
    (true (eq :atomic-element-type
              (compute '(shader:atomic-add values (uint 0.0) (uint 1.0))
                       :resources '((values :storage-buffer :binding 0
                                     :element :vec4 :access :read-write)))))
    (true (eq :read-only-storage-buffer
              (compute '(shader:atomic-add counter (uint 0.0) (uint 1.0))
                       :resources '((counter :storage-buffer :binding 0
                                     :element :uint)))))
    ;; Barriers belong to uniform control flow.
    (true (eq :barrier-in-divergent-control-flow
              (compute '(when (< (swizzle cell :x) (uint 3.0))
                         (shader:workgroup-barrier)))))
    (true (null (compute '(let* ((count (shader:buffer-element
                                         counter (uint 0.0))))
                           (when (< count (uint 3.0))
                             (shader:workgroup-barrier))
                           (shader:set-buffer-element
                            counter (swizzle cell :x) count)))))
    ;; An atomic's value differs per invocation, whatever its operands.
    (true (eq :barrier-in-divergent-control-flow
              (compute '(let* ((count (shader:atomic-add
                                       counter (uint 0.0) (uint 1.0))))
                         (when (< count (uint 3.0))
                           (shader:workgroup-barrier))))))
    (true (eq :shared-array-index-out-of-bounds
              (compute '(shader:set-shared-element tile (uint 4.0) 1.0)
                       :shared '((tile :float 4)))))
    (true (eq :shared-memory-exceeds-limit
              (compute '(shader:workgroup-barrier)
                       :shared '((tile :vec4 1025)))))
    (true (eq :shared-array-requires-element
              (compute '(shader:set-buffer-element counter (uint 0.0) tile)
                       :shared '((tile :uint 4))))))
  ;; Fragment, wave, and compute words keep to their stages.
  (true (eq :invalid-statement-for-stage
            (effect-failure-reason
             (lambda ()
               (effect-probe '(:stage :compute :workgroup-size (1 1 1))
                             '(shader:discard))))))
  (true (eq :invalid-statement-for-stage
            (effect-failure-reason
             (lambda ()
               (effect-probe '(:stage :fragment
                               :outputs ((color :vec4 :location 0)))
                             '(shader:workgroup-barrier))))))
  (true (eq :shared-memory-outside-compute
            (effect-failure-reason
             (lambda ()
               (effect-probe '(:stage :fragment
                               :outputs ((color :vec4 :location 0))
                               :shared ((tile :float 4)))
                             '(set-output color (vec4 0.0 0.0 0.0 0.0)))))))
  (true (eq :built-in-not-in-stage
            (effect-failure-reason
             (lambda ()
               (effect-probe '(:stage :vertex
                               :inputs ((pixel :vec4 :built-in :frag-coord))
                               :outputs ((position :vec4 :built-in
                                          :position)))
                             '(set-output position pixel))))))
  (true (eq :built-in-type
            (effect-failure-reason
             (lambda ()
               (effect-probe '(:stage :fragment
                               :inputs ((front :float :built-in
                                         :front-facing))
                               :outputs ((color :vec4 :location 0)))
                             '(set-output color (vec4 front 0.0 0.0 0.0)))))))
  (true (eq :wave-operation-outside-wave-stage
            (effect-failure-reason
             (lambda ()
               (effect-probe '(:stage :vertex
                               :inputs ((index :uint :built-in :vertex-index))
                               :outputs ((position :vec4 :built-in
                                          :position)))
                             '(set-output position
                               (vec4 (shader:wave-active-sum (float index))
                                0.0 0.0 1.0))))))))

(define-test texture-kinds-and-fragment-built-ins-lower-to-valid-spir-v
  (let* ((specification (texture-kinds-fragment-probe))
         (names (spir-v-instruction-names specification))
         (forms (spir-v-forms-text specification)))
    (dolist (name '(spv::image-sample-explicit-lod
                    spv::image-sample-implicit-lod-with
                    spv::image-sample-dref-implicit-lod
                    spv::image-dref-gather spv::image-gather
                    spv::image-fetch-with spv::image-query-size-lod
                    spv::kill))
      (true (find name names)))
    (dolist (text '("FRAG-COORD" "FRONT-FACING" "SAMPLE-ID" "FRAG-DEPTH"
                    "DEPTH-REPLACING" "SAMPLE-RATE-SHADING" "IMAGE-QUERY"
                    "CUBE" "3D"))
      (true (search text forms)))
    (false (spirv-val-diagnostics specification
                                      :environment "vulkan1.0"))))

(define-test workgroup-effects-lower-to-valid-spir-v
  (let* ((specification (workgroup-effects-probe))
         (module (spv:shader-module specification))
         (names (spir-v-instruction-names specification))
         (forms (spir-v-forms-text specification)))
    ;; Subgroup arithmetic is SPIR-V 1.3, Vulkan 1.1.
    (true (= #x00010300 (spv:spir-v-module-version module)))
    (dolist (capability '(spv::group-non-uniform
                          spv::group-non-uniform-arithmetic
                          spv::group-non-uniform-ballot))
      (true (member capability (spv:spir-v-module-capabilities module))))
    (dolist (name '(spv::control-barrier spv::atomic-i-add spv::atomic-u-min
                    spv::atomic-u-max spv::atomic-or spv::atomic-exchange
                    spv::atomic-compare-exchange spv::image-read
                    spv::image-write spv::image-query-size
                    spv::group-non-uniform-f-add spv::group-non-uniform-i-add
                    spv::group-non-uniform-ballot))
      (true (find name names)))
    (dolist (text '("WORKGROUP" "RGBA16F" "R32UI" "EXCLUSIVE-SCAN"
                    "SUBGROUP-LOCAL-INVOCATION-ID" "SUBGROUP-SIZE"))
      (true (search text forms)))
    (false (spirv-val-diagnostics specification))))

(define-test unchanged-shaders-keep-their-spir-v-version
  ;; Nothing here should move a module that uses none of it.
  (true (= #x00010000
           (spv:spir-v-module-version
            (spv:shader-module (texture-kinds-fragment-probe))))))
