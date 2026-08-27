(in-package #:luvcraft.web)

;;; The body gallery is deliberately written as ParenScript rather than kept as
;;; a second, opaque browser-language source file.

(defun body-gallery-javascript ()
  (ps:ps*
   `(progn
      (defvar canvas ((@ document query-selector) "#body-canvas"))
      (defvar status ((@ document query-selector) "#status"))
      (defvar title ((@ document query-selector) "#body-title"))
      (defvar picker ((@ document query-selector) "#body-picker"))
      (defvar knobs-element ((@ document query-selector) "#knobs"))
      (defvar reset-button ((@ document query-selector) "#reset"))
      (defvar frame-floats 76)
      (defvar frame-interval (/ 1000 60))
      (defvar render-scale 1)
      (defvar quad
        (new (|Float32Array|
              (array 0 0 0 1 0 0 1 1 0 0 0 0 1 1 0 0 1 0))))
      (defvar frame-data (new (|Float32Array| frame-floats)))
      (defvar instance-data (new (|Float32Array| 4)))
      (defvar sun-length ((@ |Math| hypot) 0.55 0.82 0.28))
      (defvar sun-x (/ 0.55 sun-length))
      (defvar sun-y (/ 0.82 sun-length))
      (defvar sun-z (/ 0.28 sun-length))

      (defvar device)
      (defvar context)
      (defvar format)
      (defvar frame-buffer)
      (defvar quad-buffer)
      (defvar instance-buffer)
      (defvar facing-buffer)
      (defvar depth-texture)
      (defvar depth-view)
      (defvar pipeline)
      (defvar bind-group)
      (defvar catalog)
      (defvar selected-body)
      (defvar current-values (new (|Map|)))
      (defvar module-cache (new (|Map|)))
      (defvar yaw 0.32)
      (defvar elevation 0.12)
      (defvar distance 4.1)
      (defvar dragging false)
      (defvar previous-pointer null)
      (defvar pipeline-generation 0)
      (defvar last-frame-time (- -Infinity))
      (defvar canvas-size-dirty true)
      (defvar camera-buffer-dirty true)
      (defvar instance-buffer-dirty true)

      (defun set-status (message &optional (failed false))
        (setf (@ status text-content) message
              (@ status style color) (if failed "#ef927f" "")))

      (defun write-vec4 (array index x y z w)
        (let ((offset (* index 4)))
          (setf (aref array offset) x
                (aref array (+ offset 1)) y
                (aref array (+ offset 2)) z
                (aref array (+ offset 3)) w)))

      (defun resize-canvas ()
        (unless canvas-size-dirty (return-from resize-canvas false))
        (setf canvas-size-dirty false)
        (let ((width (max 1 (floor (* (@ canvas client-width) render-scale))))
              (height (max 1 (floor (* (@ canvas client-height) render-scale)))))
          (when (and (= (@ canvas width) width) (= (@ canvas height) height))
            (return-from resize-canvas false))
          (setf (@ canvas width) width (@ canvas height) height)
          ((@ context configure) (create :device device :format format
                                         :alpha-mode "premultiplied"))
          (when depth-texture ((@ depth-texture destroy)))
          (setf depth-texture
                ((@ device create-texture)
                 (create :size (array width height) :format "depth32float"
                         :usage (aref |GPUTextureUsage| "RENDER_ATTACHMENT")))
                depth-view ((@ depth-texture create-view))
                camera-buffer-dirty true)
          true))

      (defun stature ()
        (let ((value ((@ current-values get) (@ selected-body stature-knob))))
          (if (= value undefined) 1 value)))

      (defun update-buffers ()
        (let* ((scale (stature))
               (center (* (@ selected-body center-height) scale))
               (radius (* (@ selected-body radius) scale)))
          (when instance-buffer-dirty
            (setf (aref instance-data 0) 0 (aref instance-data 1) center
                  (aref instance-data 2) 0 (aref instance-data 3) radius)
            ((@ device queue write-buffer) instance-buffer 0 instance-data)
            (setf instance-buffer-dirty false))
          (unless camera-buffer-dirty (return-from update-buffers))
          (let* ((cos-elevation (cos elevation))
                 (camera-x (* (sin yaw) cos-elevation distance))
                 (camera-y (+ center (* (sin elevation) distance)))
                 (camera-z (* (cos yaw) cos-elevation distance))
                 (forward-x (- camera-x)) (forward-y (- center camera-y))
                 (forward-z (- camera-z))
                 (forward-length (or ((@ |Math| hypot) forward-x forward-y forward-z) 1)))
            (setf forward-x (/ forward-x forward-length)
                  forward-y (/ forward-y forward-length)
                  forward-z (/ forward-z forward-length))
            (let* ((right-x forward-z) (right-z (- forward-x))
                   (right-length (or ((@ |Math| hypot) right-x right-z) 1)))
              (setf right-x (/ right-x right-length) right-z (/ right-z right-length))
              (let ((up-x (* forward-y right-z))
                    (up-y (- (* forward-z right-x) (* forward-x right-z)))
                    (up-z (- (* forward-y right-x))) (near 0.05) (far 40)
                    (focal (/ 1 (tan (/ (* 42 pi) 180 2)))))
                (write-vec4 frame-data 0 camera-x camera-y camera-z 0)
                (write-vec4 frame-data 1 right-x 0 right-z 0)
                (write-vec4 frame-data 2 up-x up-y up-z 0)
                (write-vec4 frame-data 3 forward-x forward-y forward-z 0)
                (write-vec4 frame-data 4 (/ focal (/ (@ canvas width) (@ canvas height)))
                            focal (/ far (- far near)) (/ (* (- far) near) (- far near)))
                (write-vec4 frame-data 6 sun-x sun-y sun-z 1)
                (write-vec4 frame-data 7 2.0 1.55 1.15 0.02)
                (write-vec4 frame-data 8 0.20 0.27 0.34 (@ canvas height))
                (write-vec4 frame-data 9 0.42 0.33 0.25 (@ canvas width))
                (write-vec4 frame-data 10 0.13 0.16 0.19 1)
                ((@ device queue write-buffer) frame-buffer 0 frame-data)
                (setf camera-buffer-dirty false))))))

      (defun compilation-messages (info)
        (chain (@ info messages)
               (map (lambda (message)
                      (+ (@ message type) ": " (@ message message) " ("
                         (@ message line-num) ":" (@ message line-pos) ")")))
               (join ,(string #\Newline))))

      (luv.wiki.browser:async-defun checked-shader-module (label code)
        (let* ((module ((@ device create-shader-module) (create :label label :code code)))
               (info (luv.wiki.browser:await ((@ module get-compilation-info))))
               (errors (chain (@ info messages)
                              (filter (lambda (message) (= (@ message type) "error"))))))
          (when (@ errors length) (throw (new (|Error| (compilation-messages info)))))
          module))

      (defun stage-constants (body values stage)
        ((@ |Object| from-entries)
         (chain (@ body knobs)
                (filter (lambda (knob) ((@ knob stages includes) stage)))
                (map (lambda (knob)
                       (array (@ knob identifier) ((@ values get) (@ knob name))))))))

      (defun modules-for-body (body)
        (unless ((@ module-cache has) (@ body id))
          ((@ module-cache set) (@ body id)
           ((luv.wiki.browser:async-lambda ()
              (let ((codes
                      (luv.wiki.browser:await
                       ((@ |Promise| all)
                        (array (chain (fetch (@ body vertex-url))
                                      (then (lambda (response) ((@ response text)))))
                               (chain (fetch (@ body fragment-url))
                                      (then (lambda (response) ((@ response text))))))))))
                (let ((modules
                        (luv.wiki.browser:await
                         ((@ |Promise| all)
                          (array (checked-shader-module (+ (@ body id) " vertex") (aref codes 0))
                                 (checked-shader-module (+ (@ body id) " fragment") (aref codes 1)))))))
                  (create :vertex (aref modules 0) :fragment (aref modules 1))))))))
        ((@ module-cache get) (@ body id)))

      (luv.wiki.browser:async-defun rebuild-pipeline ()
        (let* ((generation (incf pipeline-generation)) (body selected-body)
               (values (new (|Map| current-values))))
          (set-status (if ((@ module-cache has) (@ body id))
                          "Retuning this creature…" "Compiling this creature…"))
          (let* ((modules (luv.wiki.browser:await (modules-for-body body)))
                 (next-pipeline
                   (luv.wiki.browser:await
                    ((@ device create-render-pipeline-async)
                     (create
                      :label (+ (@ body label) " body") :layout "auto"
                      :vertex
                      (create :module (@ modules vertex)
                              :entry-point (+ (@ body id) "_sdf_vertex_specification")
                              :constants (stage-constants body values "vertex")
                              :buffers
                              (array
                               (create :array-stride 12 :attributes
                                       (array (create :shader-location 0 :offset 0 :format "float32x3")))
                               (create :array-stride 16 :step-mode "instance" :attributes
                                       (array (create :shader-location 1 :offset 0 :format "float32x4")))
                               (create :array-stride 16 :step-mode "instance" :attributes
                                       (array (create :shader-location 2 :offset 0 :format "float32x4")))))
                      :fragment
                      (create :module (@ modules fragment)
                              :entry-point (+ (@ body id) "_sdf_fragment_specification")
                              :constants (stage-constants body values "fragment")
                              :targets
                              (array (create :format format :blend
                                             (create :color (create :src-factor "src-alpha" :dst-factor "one-minus-src-alpha")
                                                     :alpha (create :src-factor "one" :dst-factor "one-minus-src-alpha")))))
                      :primitive (create :topology "triangle-list" :cull-mode "none")
                      :depth-stencil (create :format "depth32float" :depth-write-enabled true
                                             :depth-compare "less"))))))
            (unless (= generation pipeline-generation) (return-from rebuild-pipeline))
            (setf pipeline next-pipeline
                  bind-group ((@ device create-bind-group)
                              (create :layout ((@ pipeline get-bind-group-layout) 0)
                                      :entries (array (create :binding 2 :resource
                                                              (create :buffer frame-buffer))))))
            (set-status (+ (@ body knobs length) " live shader knobs")))))

      (defun render (timestamp)
        (request-animation-frame render)
        (when (or (@ document hidden)
                  (< (+ (- timestamp last-frame-time) 0.25) frame-interval))
          (return-from render))
        (setf last-frame-time timestamp)
        (resize-canvas)
        (when (and pipeline selected-body)
          (update-buffers)
          (let* ((encoder ((@ device create-command-encoder)))
                 (pass ((@ encoder begin-render-pass)
                        (create
                         :color-attachments
                         (array (create :view (chain context (get-current-texture) (create-view))
                                        :clear-value (create :r 0.055 :g 0.065 :b 0.058 :a 1)
                                        :load-op "clear" :store-op "store"))
                         :depth-stencil-attachment
                         (create :view depth-view :depth-clear-value 1
                                 :depth-load-op "clear" :depth-store-op "discard")))))
            ((@ pass set-pipeline) pipeline) ((@ pass set-bind-group) 0 bind-group)
            ((@ pass set-vertex-buffer) 0 quad-buffer)
            ((@ pass set-vertex-buffer) 1 instance-buffer)
            ((@ pass set-vertex-buffer) 2 facing-buffer)
            ((@ pass draw) 6 1) ((@ pass end))
            ((@ device queue submit) (array ((@ encoder finish)))))))

      (defun format-value (knob value)
        (let ((decimals (max 0 (min 4 (@ (or (aref (chain (+ (@ knob step) "") (split ".")) 1) "") length)))))
          (+ ((@ value to-fixed) decimals) (@ knob unit))))

      (defun decimal-places (value)
        (let ((text (chain (+ value "") (to-lower-case))))
          (if ((@ text includes) "e-")
              (let ((parts ((@ text split) "e-")))
                (+ (|Number| (aref parts 1))
                   (@ (or (aref ((@ (aref parts 0) split) ".") 1) "") length)))
              (@ (or (aref ((@ text split) ".") 1) "") length))))

      (defun knob-precision (knob)
        (min 8 (max (decimal-places (@ knob minimum)) (decimal-places (@ knob maximum))
                    (decimal-places (@ knob default)) (decimal-places (@ knob step)))))

      (defun quantize-knob-value (knob raw)
        (unless ((@ |Number| is-finite) raw) (return-from quantize-knob-value (@ knob default)))
        (let* ((snapped (+ (@ knob default)
                           (* (round (/ (- raw (@ knob default)) (@ knob step))) (@ knob step))))
               (clamped (max (@ knob minimum) (min (@ knob maximum) snapped))))
          (|Number| ((@ clamped to-fixed) (knob-precision knob)))))

      (defun default-values (body)
        (new (|Map| (chain (@ body knobs)
                           (map (lambda (knob) (array (@ knob name) (@ knob default))))))))

      (defun state-from-fragment ()
        (let* ((parameters (new (|URLSearchParams| ((@ location hash slice) 1))))
               (body (or (chain (@ catalog bodies)
                                (find (lambda (candidate)
                                        (= (@ candidate id) ((@ parameters get) "body")))))
                         (aref (@ catalog bodies) 0)))
               (values
                 (new (|Map|
                       (chain (@ body knobs)
                              (map (lambda (knob)
                                     (let* ((encoded ((@ parameters get) (@ knob name)))
                                            (raw (if (or (= encoded null) (= ((@ encoded trim)) ""))
                                                     (@ knob default) (|Number| encoded))))
                                       (array (@ knob name) (quantize-knob-value knob raw))))))))))
          (create :body body :values values)))

      (defun write-fragment ()
        (let ((parameters (new (|URLSearchParams|))))
          ((@ parameters set) "body" (@ selected-body id))
          (dolist (knob (@ selected-body knobs))
            ((@ parameters set) (@ knob name) (+ ((@ current-values get) (@ knob name)) "")))
          ((@ history replace-state) null "" (+ "#" parameters))))

      (defun build-knobs ()
        ((@ knobs-element replace-children))
        (dolist (knob (@ selected-body knobs))
          (let* ((wrapper ((@ document create-element) "section"))
                 (line ((@ document create-element) "div"))
                 (label ((@ document create-element) "label"))
                 (output ((@ document create-element) "output"))
                 (input ((@ document create-element) "input"))
                 (id (+ "knob-" (@ knob name))))
            (setf (@ wrapper class-name) "knob" (@ line class-name) "knob-line"
                  (@ label html-for) id (@ label text-content) (@ knob label)
                  (@ input id) id (@ input type) "range" (@ input min) (@ knob minimum)
                  (@ input max) (@ knob maximum) (@ input step) "any"
                  (@ input value) ((@ current-values get) (@ knob name))
                  (@ output value) (format-value knob (|Number| (@ input value))))
            ((@ input add-event-listener) "input"
             (lambda ()
               (let ((value (quantize-knob-value knob (|Number| (@ input value)))))
                 (setf (@ input value) value)
                 ((@ current-values set) (@ knob name) value)
                 (when (= (@ knob name) (@ selected-body stature-knob))
                   (setf instance-buffer-dirty true camera-buffer-dirty true))
                 (setf (@ output value) (format-value knob value))
                 (write-fragment) (chain (rebuild-pipeline) (catch fail)))))
            ((@ line append) label output) ((@ wrapper append) line)
            (when (@ knob documentation)
              (let ((doc ((@ document create-element) "p")))
                (setf (@ doc text-content) (aref ((@ (@ knob documentation) split) "\n") 0))
                ((@ wrapper append) doc)))
            ((@ wrapper append) input) ((@ knobs-element append) wrapper))))

      (luv.wiki.browser:async-defun select-body (body &optional values)
        (unless values (setf values (default-values body)))
        (setf selected-body body current-values (new (|Map| values))
              instance-buffer-dirty true camera-buffer-dirty true
              (@ title text-content) (@ body label))
        (dolist (button (@ picker children))
          ((@ button set-attribute) "aria-current"
           (if (= (@ button dataset body) (@ body id)) "true" "false")))
        (build-knobs) (write-fragment)
        (luv.wiki.browser:await (rebuild-pipeline)))

      (defun build-picker ()
        (dolist (body (@ catalog bodies))
          (let ((button ((@ document create-element) "button")))
            (setf (@ button type) "button" (@ button dataset body) (@ body id)
                  (@ button text-content) (@ body label))
            ((@ button add-event-listener) "click"
             (lambda () (chain (select-body body) (catch fail))))
            ((@ picker append) button))))

      (defun fail (error)
        ((@ console error) error)
        (set-status (or (@ error message) (|String| error)) true))

      (luv.wiki.browser:async-defun start ()
        (unless (@ navigator gpu)
          (throw (new (|Error| "WebGPU is unavailable in this browser."))))
        (let ((adapter (luv.wiki.browser:await ((@ navigator gpu request-adapter)))))
          (unless adapter (throw (new (|Error| "No WebGPU adapter is available."))))
          (setf device (luv.wiki.browser:await ((@ adapter request-device))))
          (chain (@ device lost)
                 (then (lambda (info)
                         (fail (new (|Error| (+ "WebGPU device lost: " (@ info message)))))))))
        (setf context ((@ canvas get-context) "webgpu")
              format ((@ navigator gpu get-preferred-canvas-format))
              frame-buffer ((@ device create-buffer)
                            (create :size (* frame-floats 4)
                                    :usage (logior (aref |GPUBufferUsage| "UNIFORM")
                                                   (aref |GPUBufferUsage| "COPY_DST"))))
              quad-buffer ((@ device create-buffer)
                           (create :size (@ quad byte-length)
                                   :usage (logior (aref |GPUBufferUsage| "VERTEX")
                                                  (aref |GPUBufferUsage| "COPY_DST"))))
              instance-buffer ((@ device create-buffer)
                               (create :size 16 :usage (logior (aref |GPUBufferUsage| "VERTEX")
                                                               (aref |GPUBufferUsage| "COPY_DST"))))
              facing-buffer ((@ device create-buffer)
                             (create :size 16 :usage (logior (aref |GPUBufferUsage| "VERTEX")
                                                             (aref |GPUBufferUsage| "COPY_DST")))))
        ((@ device queue write-buffer) quad-buffer 0 quad)
        ((@ device queue write-buffer) facing-buffer 0
         (new (|Float32Array| (array 0 0 1 0))))
        (setf catalog
              (luv.wiki.browser:await
               (chain (fetch "/bodies/bodies.json")
                      (then (lambda (response) ((@ response json)))))))
        (build-picker) (resize-canvas)
        (let ((fragment-state (state-from-fragment)))
          (luv.wiki.browser:await
           (select-body (@ fragment-state body) (@ fragment-state values))))
        (request-animation-frame render))

      ((@ reset-button add-event-listener) "click"
       (lambda () (chain (select-body selected-body) (catch fail))))
      ((@ canvas add-event-listener) "pointerdown"
       (lambda (event)
         (setf dragging true previous-pointer (array (@ event client-x) (@ event client-y)))
         ((@ canvas set-pointer-capture) (@ event pointer-id))))
      ((@ canvas add-event-listener) "pointermove"
       (lambda (event)
         (when dragging
           (decf yaw (* (- (@ event client-x) (aref previous-pointer 0)) 0.008))
           (setf elevation
                 (max -0.5
                      (min 0.7
                           (+ elevation (* (- (@ event client-y)
                                              (aref previous-pointer 1)) 0.006))))
                 previous-pointer (array (@ event client-x) (@ event client-y))
                 camera-buffer-dirty true))))
      ((@ canvas add-event-listener) "pointerup" (lambda () (setf dragging false)))
      ((@ canvas add-event-listener) "pointercancel" (lambda () (setf dragging false)))
      ((@ canvas add-event-listener) "wheel"
       (lambda (event)
         ((@ event prevent-default))
         (setf distance (max 2.0 (min 8.0 (* distance (exp (* (@ event delta-y) 0.001)))))
               camera-buffer-dirty true))
       (create :passive false))
      (chain (new (|ResizeObserver| (lambda () (setf canvas-size-dirty true))))
             (observe canvas))
      ((@ document add-event-listener) "visibilitychange"
       (lambda () (setf last-frame-time (- -Infinity))))
      ((@ window add-event-listener) "hashchange"
       (lambda ()
         (when catalog
           (let ((fragment-state (state-from-fragment)))
             (chain (select-body (@ fragment-state body) (@ fragment-state values))
                    (catch fail))))))
      (chain (start) (catch fail)))))
