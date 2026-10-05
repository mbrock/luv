;;;; Executable claims for shader programs and luv-shaderc.
;;;;
;;;; The example program is compiled to every output; when the native tools
;;;; are present, its MSL goes through Apple's metal, its HLSL through DXC,
;;;; and its header through the C++ compiler against a copy of moppe's
;;;; reflection header.

(defpackage #:luv.shaderc.tests
  (:use #:cl)
  (:import-from #:parachute #:define-test #:true #:false #:is #:fail)
  (:local-nicknames (#:shader #:luv.shader)
                    (#:shaderc #:luv.shaderc)
                    (#:spv #:luv.spir-v)))

(in-package #:luv.shaderc.tests)

(defparameter *root* (asdf:system-source-directory "luv/shaderc"))

(defparameter *example*
  (merge-pathnames "hal/shaderc/examples/textured-instances.lisp" *root*))

(defparameter *compute-example*
  (merge-pathnames "hal/shaderc/examples/particle-advance.lisp" *root*))

(defun tool-available-p (&rest command)
  (ignore-errors
   (zerop (nth-value 2 (uiop:run-program command :ignore-error-status t)))))

(defun xcrun-command (&rest arguments)
  "Apple's xcrun with ARGUMENTS.  Nix shells put xcbuild's xcrun first and
point DEVELOPER_DIR at a bare SDK; Metal's compiler lives in Xcode's."
  (if (probe-file "/usr/bin/xcrun")
      (list* "env" "-u" "DEVELOPER_DIR" "-u" "SDKROOT" "/usr/bin/xcrun"
             arguments)
      (list* "xcrun" arguments)))

(defun run-tool (command)
  "Run COMMAND; return NIL on success, else its diagnostics."
  (multiple-value-bind (output error-output status)
      (uiop:run-program command :output :string :error-output :string
                                :ignore-error-status t)
    (unless (zerop status)
      (format nil "~{~A~^ ~}~%~A~A" command output error-output))))

(defun failure-reason (thunk)
  (handler-case (progn (funcall thunk) nil)
    (shader:shader-language-error (condition)
      (shader:shader-language-error-reason condition))))

(defmacro with-example-program ((program) &body body)
  "Run BODY with PROGRAM bound to the example's one program."
  `(let ((,program (first (shaderc:load-shader-source *example*))))
     ,@body))

(defun stage-probe (name stage &key inputs outputs resources body)
  (let ((specification
          (shader:parse-shader-specification
           name `(:stage ,stage :inputs ,inputs :outputs ,outputs
                  :resources ,resources)
           (list body))))
    (setf (fdefinition name) (lambda () specification))
    specification))

(defun probe-program (&rest stages)
  (shader:make-shader-program 'probe-program stages))

(define-test example-program-links-in-binding-families
  (with-example-program (program)
    (let* ((linkage (shader:link-shader-program program))
           (resources (shader:shader-program-linkage-resources linkage)))
      (is equal '("FRAME" "CORNERS" "INSTANCES" "ALBEDO" "SHADOW-MAP"
                  "LINEAR-CLAMP" "SHADOW-COMPARE")
          (mapcar (lambda (resource)
                    (symbol-name (shader:shader-program-resource-name
                                  resource)))
                  resources))
      (is equal '(:uniform-block :storage-buffer :storage-buffer :texture-2d
                  :depth-texture-2d :sampler :comparison-sampler)
          (mapcar #'shader:shader-program-resource-kind resources))
      ;; Buffers and textures number independently: both start at 0.
      (is equal '(0 1 2 0 1 0 3)
          (mapcar #'shader:shader-program-resource-binding resources))
      (is equal '(:vertex :fragment)
          (shader:shader-program-resource-stages (first resources)))
      ;; The fragment stage declares a prefix; the program keeps the whole.
      (is = 128 (shader:shader-uniform-block-byte-size
                 (shader:shader-program-resource-declaration
                  (first resources))))
      (is = 1 (length (shader:shader-program-linkage-color-outputs linkage))))))

(define-test program-linking-rejects-what-the-stages-disagree-on
  (let ((vertex
          (stage-probe 'probe-vertex :vertex
                       :inputs '((index :uint :built-in :vertex-index))
                       :outputs '((position :vec4 :built-in :position)
                                  (uv :vec2 :location 0))
                       :resources '((camera :uniform-block :binding 0
                                     :members ((origin :vec4)))
                                    (image :texture-2d :binding 0))
                       :body '(let* ((x (float index)))
                               (shader:set-output position
                                (shader:vec4 x 0.0 0.0 1.0))
                               (shader:set-output uv (shader:vec2 x x))))))
    (declare (ignore vertex))
    (flet ((fragment (&key (inputs '((uv :vec2 :location 0)))
                           (outputs '((color :vec4 :location 0)))
                           resources)
             (stage-probe 'probe-fragment :fragment
                          :inputs inputs :outputs outputs
                          :resources resources
                          :body '(shader:set-output color
                                  (shader:vec4 0.0 0.0 0.0 1.0))))
           (reason (&rest stages)
             (failure-reason
              (lambda () (shader:link-shader-program
                          (apply #'probe-program stages))))))
      (fragment :resources '((other :texture-2d :binding 0)))
      (is eq :program-binding-collision
          (reason :vertex 'probe-vertex :fragment 'probe-fragment))
      (fragment :resources '((image :texture-2d :binding 1)))
      (is eq :program-resource-moved
          (reason :vertex 'probe-vertex :fragment 'probe-fragment))
      (fragment :resources '((image :depth-texture-2d :binding 0)))
      (is eq :incompatible-program-resource
          (reason :vertex 'probe-vertex :fragment 'probe-fragment))
      (fragment :resources '((other :storage-buffer :binding 0
                              :element :vec4)))
      (is eq :program-binding-collision
          (reason :vertex 'probe-vertex :fragment 'probe-fragment))
      (fragment :inputs '((uv :vec2 :location 3)))
      (is eq :fragment-input-without-vertex-output
          (reason :vertex 'probe-vertex :fragment 'probe-fragment))
      (fragment :inputs '((uv :vec4 :location 0)))
      (is eq :stage-interface-type-mismatch
          (reason :vertex 'probe-vertex :fragment 'probe-fragment))
      (fragment :inputs '((uv :vec2 :location 0 :interpolation :flat)))
      (is eq :stage-interface-interpolation-mismatch
          (reason :vertex 'probe-vertex :fragment 'probe-fragment))
      (fragment :outputs '((color :vec4 :location 1)))
      (is eq :sparse-color-outputs
          (reason :vertex 'probe-vertex :fragment 'probe-fragment))
      (is eq :program-stage-mismatch
          (reason :vertex 'probe-fragment :fragment 'probe-vertex))
      ;; A sampler texture of binding 0 beside a buffer of binding 0 is
      ;; fine: the families are separate.
      (fragment :resources '((image :texture-2d :binding 0)
                             (camera :uniform-block :binding 0
                              :members ((origin :vec4)))))
      (true (shader:link-shader-program
             (probe-program :vertex 'probe-vertex :fragment 'probe-fragment)))
      (is eq :invalid-program-stages
          (failure-reason
           (lambda () (probe-program :fragment 'probe-fragment))))
      ;; A depth pass is a vertex stage alone.
      (true (shader:link-shader-program
             (probe-program :vertex 'probe-vertex))))))

(define-test the-binding-contract-is-checked-before-anything-is-written
  (flet ((contract-error (resources &key (stage-inputs '()))
           (stage-probe 'contract-vertex :vertex
                        :inputs (append
                                 '((index :uint :built-in :vertex-index))
                                 stage-inputs)
                        :outputs '((position :vec4 :built-in :position))
                        :body '(shader:set-output position
                                (shader:vec4 0.0 0.0 0.0 1.0)))
           (stage-probe 'contract-fragment :fragment
                        :outputs '((color :vec4 :location 0))
                        :resources resources
                        :body '(shader:set-output color
                                (shader:vec4 0.0 0.0 0.0 1.0)))
           (handler-case
               (progn
                 (shaderc:compile-shader-program
                  (probe-program :vertex 'contract-vertex
                                 :fragment 'contract-fragment)
                  :targets nil)
                 nil)
             (error (condition) condition))))
    (true (typep (contract-error '((image :texture-2d :binding 16)))
                 'shaderc:shaderc-error))
    (true (typep (contract-error '((filter :sampler :binding 9)))
                 'shaderc:shaderc-error))
    ;; Sampler 3 is the standard comparison sampler, and only it compares.
    (true (typep (contract-error '((filter :sampler :binding 3)))
                 'shaderc:shaderc-error))
    (true (typep (contract-error '() :stage-inputs
                                 '((position :vec4 :location 0)))
                 'shaderc:shaderc-error))))

(define-test luv-shaderc-writes-stages-manifest-and-header
  (uiop:with-temporary-file (:pathname scratch :keep nil)
    (let ((directory (uiop:ensure-directory-pathname
                      (format nil "~A.d" (uiop:native-namestring scratch)))))
      (unwind-protect
           (multiple-value-bind (programs written)
               (let ((shader:*shader-programs* nil))
                 (shaderc:compile-shader-files (list *example*)
                                               :directory directory))
             (is = 1 (length programs))
             (is equal '("textured_instances.vertex.metal"
                         "textured_instances.vertex.hlsl"
                         "textured_instances.fragment.metal"
                         "textured_instances.fragment.hlsl"
                         "textured_instances.json"
                         "textured_instances.hh")
                 (mapcar #'file-namestring written))
             (let ((json (uiop:read-file-string
                          (merge-pathnames "textured_instances.json"
                                           directory)))
                   (header (uiop:read-file-string
                            (merge-pathnames "textured_instances.hh"
                                             directory)))
                   (vertex-msl (uiop:read-file-string
                                (merge-pathnames
                                 "textured_instances.vertex.metal" directory)))
                   (fragment-hlsl (uiop:read-file-string
                                   (merge-pathnames
                                    "textured_instances.fragment.hlsl"
                                    directory))))
               (true (search "\"entry\": \"textured_instances_vertex\"" json))
               (true (search "\"hlsl_profile\": \"ps_6_0\"" json))
               (true (search "\"kind\": \"comparison_sampler\"" json))
               (true (search "\"hlsl\": \"t1, space0\"" json))
               (true (search "\"color_outputs\": 1" json))
               (true (search "namespace moppe::nhal::shaders::textured_instances {"
                             header))
               (true (search "#include <moppe/nhal/reflection.hh>" header))
               (true (search "std::array<float, 4> sun_direction;" header))
               (true (search "static_assert(sizeof(Frame) == 128);" header))
               (true (search "{\"frame\", ResourceKind::uniform_block, 0,"
                             header))
               (true (search "stage_vertex | stage_fragment, sizeof(Frame)}"
                             header))
               (true (search "ResourceKind::comparison_sampler, 3," header))
               (true (search ".vertex_entry = \"textured_instances_vertex\","
                             header))
               (true (search ".compute_entry = nullptr," header))
               ;; Both languages name the entry the same way.
               (true (search "textured_instances_vertex(" vertex-msl))
               (true (search "constant Frame& frame [[buffer(0)]]" vertex-msl))
               (true (search "textured_instances_fragment(" fragment-hlsl))
               (true (search "SamplerComparisonState shadow_compare : register(s3);"
                             fragment-hlsl)))
             (when (apply #'tool-available-p
                          (xcrun-command "-sdk" "macosx" "--find" "metal"))
               (dolist (stage '("vertex" "fragment"))
                 (is eq nil
                     (run-tool
                      (xcrun-command
                       "-sdk" "macosx" "metal" "-std=metal4.0"
                       "-c" (uiop:native-namestring
                             (merge-pathnames
                              (format nil "textured_instances.~A.metal" stage)
                              directory))
                       "-o" (uiop:native-namestring
                             (merge-pathnames (format nil "~A.air" stage)
                                              directory)))))))
             (let ((dxc (or (uiop:getenv "LUV_DXC") "dxc")))
               (when (tool-available-p dxc "--version")
                 (loop for (stage profile) in '(("vertex" "vs_6_0")
                                                ("fragment" "ps_6_0"))
                       do (is eq nil
                              (run-tool
                               (list dxc "-HV" "2021" "-WX" "-T" profile
                                     "-E" (format nil "textured_instances_~A"
                                                  stage)
                                     "-Fo" (uiop:native-namestring
                                            (merge-pathnames
                                             (format nil "~A.dxil" stage)
                                             directory))
                                     (uiop:native-namestring
                                      (merge-pathnames
                                       (format nil "textured_instances.~A.hlsl"
                                               stage)
                                       directory))))))))
             (let ((compiler (or (uiop:getenv "CXX") "c++")))
               (when (tool-available-p compiler "--version")
                 (is eq nil
                     (run-tool
                      (list compiler "-std=c++20" "-fsyntax-only" "-x" "c++"
                            "-I" (uiop:native-namestring
                                  (merge-pathnames "hal/shaderc/fixtures/"
                                                   *root*))
                            (uiop:native-namestring
                             (merge-pathnames "textured_instances.hh"
                                              directory))))))))
        (uiop:delete-directory-tree directory :validate t
                                              :if-does-not-exist :ignore)))))

(define-test names-become-cpp-identifiers
  (is string= "frame_state" (shaderc:snake-identifier 'frame-state))
  (is string= "default_" (shaderc:snake-identifier 'default))
  (is string= "FrameState" (shaderc:camel-identifier 'frame-state))
  (is string= "ProgramBlock" (shaderc:camel-identifier 'program)))

(define-test errors-name-the-file-program-and-stage
  (uiop:with-temporary-file (:pathname source :type "lisp" :keep nil
                             :stream stream :direction :output)
    (write-string "(define-shader broken-vertex
    (:stage :vertex
     :inputs ((index :uint :built-in :vertex-index))
     :outputs ((position :vec4 :built-in :position)))
  (set-output position (vec4 index 0.0 0.0)))
(define-shader-program broken :vertex broken-vertex)
" stream)
    :close-stream
    (let ((condition
            (handler-case
                (let ((shader:*shader-programs* nil))
                  (shaderc:compile-shader-files (list source)))
              (shader:shader-language-error (condition) condition))))
      (true (typep condition 'shader:shader-language-error))
      (is eq :invalid-vector-width
          (shader:shader-language-error-reason condition)))))

(define-test compute-programs-lower-to-every-target
  (uiop:with-temporary-file (:pathname scratch :keep nil)
    (let ((directory (uiop:ensure-directory-pathname
                      (format nil "~A.d" (uiop:native-namestring scratch)))))
      (unwind-protect
           (multiple-value-bind (programs written)
               (shaderc:compile-shader-files (list *compute-example*)
                                             :directory directory)
             (is equal '("particle_advance.compute.metal"
                         "particle_advance.compute.hlsl"
                         "particle_advance.json"
                         "particle_advance.hh")
                 (mapcar #'file-namestring written))
             (let* ((program (shaderc:compiled-program-linkage
                              (first programs)))
                    (specification
                      (shader:shader-program-linkage-specification
                       program :compute))
                    (header (uiop:read-file-string
                             (merge-pathnames "particle_advance.hh"
                                              directory)))
                    (json (uiop:read-file-string
                           (merge-pathnames "particle_advance.json"
                                            directory)))
                    (spir-v (merge-pathnames "particle_advance.spv"
                                             directory)))
               (true (search "ResourceKind::read_write_storage_buffer, 1,"
                             header))
               (true (search ".compute_entry = \"particle_advance_compute\","
                             header))
               (true (search "workgroup_size {64, 1, 1};" header))
               (true (search "\"hlsl\": \"u1, space0\"" json))
               (true (search "\"hlsl_profile\": \"cs_6_0\"" json))
               ;; The same specification is a GLCompute module for Vulkan.
               (spv:write-spir-v (spv:assemble-shader-specification
                                  specification)
                                 spir-v)
               (when (tool-available-p "spirv-val" "--version")
                 (is eq nil (run-tool
                             (list "spirv-val" "--target-env" "vulkan1.0"
                                   (uiop:native-namestring spir-v)))))
               (when (apply #'tool-available-p
                            (xcrun-command "-sdk" "macosx" "--find" "metal"))
                 (is eq nil
                     (run-tool
                      (xcrun-command
                       "-sdk" "macosx" "metal" "-std=metal4.0" "-c"
                       (uiop:native-namestring
                        (merge-pathnames "particle_advance.compute.metal"
                                         directory))
                       "-o" (uiop:native-namestring
                             (merge-pathnames "compute.air" directory))))))
               (let ((dxc (or (uiop:getenv "LUV_DXC") "dxc")))
                 (when (tool-available-p dxc "--version")
                   (is eq nil
                       (run-tool
                        (list dxc "-HV" "2021" "-WX" "-T" "cs_6_0"
                              "-E" "particle_advance_compute"
                              "-Fo" (uiop:native-namestring
                                     (merge-pathnames "compute.dxil"
                                                      directory))
                              (uiop:native-namestring
                               (merge-pathnames
                                "particle_advance.compute.hlsl"
                                directory)))))))))
        (uiop:delete-directory-tree directory :validate t
                                              :if-does-not-exist :ignore)))))
