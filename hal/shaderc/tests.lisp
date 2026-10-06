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
                    (#:hlsl #:luv.hlsl)
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

(defun header-diagnostics (pathname)
  "Compile the generated header PATHNAME against the copy of moppe's
reflection header; NIL on success or without a C++ compiler."
  (let ((compiler (or (uiop:getenv "CXX") "c++")))
    (when (tool-available-p compiler "--version")
      (run-tool
       (list compiler "-std=c++20" "-fsyntax-only" "-x" "c++"
             "-I" (uiop:native-namestring
                   (merge-pathnames "hal/shaderc/fixtures/" *root*))
             (uiop:native-namestring pathname))))))

(defun native-diagnostics (directory program stage)
  "Compile PROGRAM's STAGE in DIRECTORY with metal and DXC when present;
return their failures."
  (let ((base (format nil "~A.~A" program stage))
        (failures nil))
    (when (apply #'tool-available-p
                 (xcrun-command "-sdk" "macosx" "--find" "metal"))
      (push (run-tool
             (xcrun-command
              "-sdk" "macosx" "metal" "-std=metal4.0" "-c"
              (uiop:native-namestring
               (merge-pathnames (format nil "~A.metal" base) directory))
              "-o" (uiop:native-namestring
                    (merge-pathnames (format nil "~A.air" base) directory))))
            failures))
    (let ((dxc (or (uiop:getenv "LUV_DXC") "dxc")))
      (when (tool-available-p dxc "--version")
        (push (run-tool
               (list dxc "-HV" "2021" "-WX"
                     "-T" (format nil "~A_6_0"
                                  (cond ((string= stage "vertex") "vs")
                                        ((string= stage "fragment") "ps")
                                        (t "cs")))
                     "-E" (format nil "~A_~A" program stage)
                     "-Fo" (uiop:native-namestring
                            (merge-pathnames (format nil "~A.dxil" base)
                                             directory))
                     (uiop:native-namestring
                      (merge-pathnames (format nil "~A.hlsl" base)
                                       directory))))
              failures)))
    (remove nil failures)))

(defmacro with-scratch-directory ((directory) &body body)
  (let ((scratch (gensym "SCRATCH")))
    `(uiop:with-temporary-file (:pathname ,scratch :keep nil)
       (let ((,directory (uiop:ensure-directory-pathname
                          (format nil "~A.d" (uiop:native-namestring
                                              ,scratch)))))
         (unwind-protect (progn ,@body)
           (uiop:delete-directory-tree ,directory :validate t
                                                  :if-does-not-exist
                                                  :ignore))))))

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
                         "textured_instances.vertex.spv"
                         "textured_instances.fragment.metal"
                         "textured_instances.fragment.hlsl"
                         "textured_instances.fragment.spv"
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
             (is = 1 (length programs))
             (is equal '("particle_advance.compute.metal"
                         "particle_advance.compute.hlsl"
                         "particle_advance.compute.spv"
                         "particle_advance.json"
                         "particle_advance.hh")
                 (mapcar #'file-namestring written))
             (let* ((header (uiop:read-file-string
                             (merge-pathnames "particle_advance.hh"
                                              directory)))
                    (json (uiop:read-file-string
                           (merge-pathnames "particle_advance.json"
                                            directory)))
                    (spir-v (merge-pathnames "particle_advance.compute.spv"
                                             directory)))
               (true (search "ResourceKind::read_write_storage_buffer, 1,"
                             header))
               (true (search ".compute_entry = \"particle_advance_compute\","
                             header))
               (true (search ".workgroup_size = {64, 1, 1}," header))
               (is eq nil (header-diagnostics
                           (merge-pathnames "particle_advance.hh"
                                            directory)))
               (true (search "\"hlsl\": \"u1, space0\"" json))
               (true (search "\"hlsl_profile\": \"cs_6_0\"" json))
               ;; Vulkan's module folds the families into set 0: the
               ;; read-write buffer keeps buffer binding 1.
               (true (search "\"spirv\": \"set 0, binding 1\"" json))
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

;;; Every output of a program, through every native tool that is present.

(defun native-output-diagnostics (compiled directory)
  "Compile COMPILED's written MSL, HLSL, and header in DIRECTORY with
Metal, DXC, and the C++ compiler when present.  Return the failures."
  (let ((failures nil)
        (name (shaderc:compiled-program-name compiled))
        (metal-p (apply #'tool-available-p
                        (xcrun-command "-sdk" "macosx" "--find" "metal")))
        (dxc (let ((dxc (or (uiop:getenv "LUV_DXC") "dxc")))
               (and (tool-available-p dxc "--version") dxc)))
        (compiler (let ((compiler (or (uiop:getenv "CXX") "c++")))
                    (and (tool-available-p compiler "--version") compiler))))
    (flet ((check (command)
             (let ((failure (run-tool command)))
               (when failure (push failure failures))))
           (file (name) (uiop:native-namestring
                         (merge-pathnames name directory))))
      (dolist (stage (shaderc:compiled-program-stages compiled))
        (let ((stage-name (string-downcase
                           (symbol-name (shaderc:compiled-stage-stage stage)))))
          (when metal-p
            (check (xcrun-command
                    "-sdk" "macosx" "metal" "-std=metal4.0" "-c"
                    (file (format nil "~A.~A.metal" name stage-name))
                    "-o" (file (format nil "~A.~A.air" name stage-name)))))
          (when dxc
            (check (list dxc "-HV" "2021" "-WX"
                         "-T" (luv.hlsl:hlsl-profile
                               (shaderc:compiled-stage-stage stage))
                         "-E" (shaderc:compiled-stage-entry-point stage)
                         "-Fo" (file (format nil "~A.~A.dxil" name stage-name))
                         (file (format nil "~A.~A.hlsl" name stage-name)))))))
      (when compiler
        (check (list compiler "-std=c++20" "-fsyntax-only" "-x" "c++"
                     "-I" (uiop:native-namestring
                           (merge-pathnames "hal/shaderc/fixtures/" *root*))
                     (file (format nil "~A.hh" name))))))
    failures))

(defmacro with-compiled-source ((compiled directory text) &body body)
  "Compile the shader source TEXT's one program into a fresh DIRECTORY,
bind COMPILED to it, and run BODY."
  (let ((source (gensym "SOURCE")) (stream (gensym "STREAM"))
        (scratch (gensym "SCRATCH")))
    `(uiop:with-temporary-file (:pathname ,source :type "lisp" :keep nil
                                :stream ,stream :direction :output)
       (write-string ,text ,stream)
       :close-stream
       (uiop:with-temporary-file (:pathname ,scratch :keep nil)
         (let ((,directory (uiop:ensure-directory-pathname
                            (format nil "~A.d"
                                    (uiop:native-namestring ,scratch)))))
           (unwind-protect
                (let ((,compiled
                        (first (let ((shader:*shader-programs* nil))
                                 (shaderc:compile-shader-files
                                  (list ,source) :directory ,directory)))))
                  ,@body)
             (uiop:delete-directory-tree ,directory :validate t
                                                    :if-does-not-exist
                                                    :ignore)))))))

(define-test uniform-matrices-are-four-column-lanes-in-the-header
  ;; #QEHEEE
  (with-compiled-source (compiled directory "(define-shader camera-vertex
    (:stage :vertex
     :inputs ((index :uint :built-in :vertex-index))
     :outputs ((position :vec4 :built-in :position))
     :resources ((camera :uniform-block :binding 0
                  :members ((view-projection :mat4) (eye :vec4)))
                 (corners :storage-buffer :binding 1 :element :vec4)))
  (set-output position
              (* view-projection (buffer-element corners index))))
(define-shader-program camera :vertex camera-vertex)
")
    (let ((header (uiop:read-file-string
                   (merge-pathnames "camera.hh" directory)))
          (json (uiop:read-file-string
                 (merge-pathnames "camera.json" directory))))
      (true (search "std::array<float, 16> view_projection;" header))
      (true (search "std::array<float, 4> eye;" header))
      (true (search "static_assert(sizeof(Camera) == 80);" header))
      (true (search "\"type\": \"mat4\"," json))
      (true (search "\"offset\": 64" json))
      (is equal nil (native-output-diagnostics compiled directory)))))

(defparameter *struct-example*
  (merge-pathnames "hal/shaderc/examples/particle-swarm.lisp" *root*))

(define-test storage-structures-become-asserted-cpp-structures
  ;; #V16OXI
  (uiop:with-temporary-file (:pathname scratch :keep nil)
    (let ((directory (uiop:ensure-directory-pathname
                      (format nil "~A.d" (uiop:native-namestring scratch)))))
      (unwind-protect
           (let* ((compiled
                    (first (let ((shader:*shader-programs* nil))
                             (shaderc:compile-shader-files
                              (list *struct-example*)
                              :directory directory))))
                  (header (uiop:read-file-string
                           (merge-pathnames "particle_swarm.hh" directory)))
                  (json (uiop:read-file-string
                         (merge-pathnames "particle_swarm.json" directory)))
                  (specification
                    (shader:shader-program-linkage-specification
                     (shaderc:compiled-program-linkage compiled) :compute))
                  (spir-v (merge-pathnames "particle_swarm.spv" directory)))
             (true (search "#include <cstddef>" header))
             (true (search "#include <cstdint>" header))
             (true (search "  struct Particle {
    std::array<float, 4> position;
    std::array<float, 4> velocity;
    std::array<float, 16> orientation;
    std::array<std::int32_t, 2> cell;
    float age;
    std::uint32_t flags;
  };
  static_assert(sizeof(Particle) == 112);" header))
             (true (search "static_assert(offsetof(Particle, flags) == 108);"
                           header))
             (true (search "std::array<float, 16> world;" header))
             (true (search "\"element\": \"particle\"," json))
             (true (search "\"struct\": \"Particle\"," json))
             (true (search "\"stride\": 112" json))
             (true (search "\"structs\": [" json))
             (true (search "\"alignment\": 16," json))
             (is equal nil (native-output-diagnostics compiled directory))
             ;; Vulkan reads the same layout from the same source.
             (spv:write-spir-v (spv:assemble-shader-specification
                                specification)
                               spir-v)
             (when (tool-available-p "spirv-val" "--version")
               (is eq nil (run-tool
                           (list "spirv-val" "--target-env" "vulkan1.0"
                                 (uiop:native-namestring spir-v))))))
        (uiop:delete-directory-tree directory :validate t
                                              :if-does-not-exist :ignore)))))

(define-test cpp-structure-names-must-not-collide
  (is eq :struct-name
      (handler-case
          (with-compiled-source (compiled directory
                                 "(define-shader-struct frame (origin :vec4))
(define-shader frame-compute
    (:stage :compute
     :workgroup-size (1 1 1)
     :inputs ((thread :uvec3 :built-in :global-invocation-id))
     :resources ((frame :uniform-block :binding 0 :members ((scale :vec4)))
                 (frames :storage-buffer :binding 1 :element frame
                         :access :read-write)))
  (set-buffer-element frames (swizzle thread :x)
                      (make-frame :origin scale)))
(define-shader-program framed :compute frame-compute)
")
            (declare (ignore compiled directory))
            nil)
        (shader:shader-language-error () :struct-name)
        (shaderc:shaderc-error () :struct-name))))

(defparameter *culling-example*
  (merge-pathnames "hal/shaderc/examples/instance-culling.lisp" *root*))

(define-test the-culling-example-compiles-for-every-target
  (with-scratch-directory (directory)
    (let* ((programs (shaderc:compile-shader-files (list *culling-example*)
                                                   :directory directory))
           (linkage (shaderc:compiled-program-linkage (first programs)))
           (specification (shader:shader-program-linkage-specification
                           linkage :compute))
           (header (uiop:read-file-string
                    (merge-pathnames "instance_culling.hh" directory)))
           (metal (uiop:read-file-string
                   (merge-pathnames "instance_culling.compute.metal"
                                    directory)))
           (hlsl (uiop:read-file-string
                  (merge-pathnames "instance_culling.compute.hlsl"
                                   directory)))
           (spir-v (merge-pathnames "instance_culling.spv" directory)))
      (true (search ".workgroup_size = {64, 1, 1}," header))
      (true (search "{\"arguments\", ResourceKind::read_write_storage_buffer, 3,"
                    header))
      ;; The argument record is atomic in Metal, so every access to it is.
      (true (search "device atomic_uint* arguments [[buffer(3)]]" metal))
      (true (search "atomic_fetch_add_explicit(&arguments[" metal))
      (true (search "InterlockedAdd(arguments[" hlsl))
      (is eq nil (header-diagnostics
                  (merge-pathnames "instance_culling.hh" directory)))
      (is equal nil (native-diagnostics directory "instance_culling"
                                        "compute"))
      (spv:write-spir-v (spv:assemble-shader-specification specification)
                        spir-v)
      (when (tool-available-p "spirv-val" "--version")
        (is eq nil (run-tool
                    (list "spirv-val" "--target-env" "vulkan1.0"
                          (uiop:native-namestring spir-v))))))))

(define-test texture-kinds-and-storage-textures-reach-the-reflection
  (stage-probe 'kinds-vertex :vertex
               :inputs '((index :uint :built-in :vertex-index))
               :outputs '((position :vec4 :built-in :position)
                          (uv :vec2 :location 0))
               :body '(let* ((x (float index)))
                       (shader:set-output position
                        (shader:vec4 x 0.0 0.0 1.0))
                       (shader:set-output uv (shader:vec2 x x))))
  (stage-probe 'kinds-fragment :fragment
               :inputs '((uv :vec2 :location 0))
               :outputs '((color :vec4 :location 0))
               :resources '((layers :texture-2d-array :binding 0)
                            (cascades :depth-texture-2d-array :binding 1)
                            (sky :texture-cube :binding 2)
                            (volume :texture-3d :binding 3)
                            (heat :read-write-texture-2d :binding 0
                             :format :r32f)
                            (filter :sampler :binding 0))
               :body '(let* ((layer (shader:uint 1.0))
                             (direction (shader:vec3 uv 1.0)))
                       (shader:set-texel heat (shader:uvec2 layer layer)
                        (shader:vec4 1.0 0.0 0.0 0.0))
                       (shader:set-output color
                        (+ (shader:sample layers filter uv layer)
                           (shader:sample-level cascades filter uv layer 0.0)
                           (shader:sample sky filter direction)
                           (shader:sample volume filter direction)))))
  (let* ((compiled (shaderc:compile-shader-program
                    (probe-program :vertex 'kinds-vertex
                                   :fragment 'kinds-fragment)))
         (json (shaderc:program-json compiled))
         (header (shaderc:program-header compiled))
         (fragment (find :fragment (shaderc:compiled-program-stages compiled)
                         :key #'shaderc:compiled-stage-stage)))
    ;; The storage texture is binding 0 of its own family, beside texture 0.
    (dolist (text '("\"kind\": \"texture_2d_array\""
                    "\"kind\": \"depth_texture_2d_array\""
                    "\"kind\": \"texture_cube\""
                    "\"kind\": \"texture_3d\""
                    "\"kind\": \"read_write_texture_2d\""
                    "\"family\": \"storage_texture\""
                    "\"format\": \"r32f\""
                    "\"msl\": \"[[texture(16)]]\""
                    "\"hlsl\": \"u0, space1\""))
      (true (search text json)))
    (dolist (text '("ResourceKind::texture_2d_array, 0,"
                    "ResourceKind::depth_texture_2d_array, 1,"
                    "ResourceKind::texture_cube, 2,"
                    "ResourceKind::texture_3d, 3,"
                    "ResourceKind::read_write_texture_2d, 0,"))
      (true (search text header)))
    (true (search "RWTexture2D<float> heat : register(u0, space1);"
                  (hlsl:hlsl-document-source
                   (shaderc:compiled-stage-hlsl fragment))))
    (with-scratch-directory (directory)
      (shaderc:write-compiled-program compiled directory)
      (is eq nil (header-diagnostics
                  (merge-pathnames "probe_program.hh" directory)))
      (is equal nil (native-diagnostics directory "probe_program"
                                        "fragment")))))
