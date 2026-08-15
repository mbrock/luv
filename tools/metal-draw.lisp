;;;; Cold-process acceptance probe for a generated Metal 4 draw.

(require :asdf)

(asdf:load-asd (truename (merge-pathnames #P"../luv.asd" *load-truename*)))
(asdf:load-system :luv/canvas/metal)
(asdf:load-system :luv/luvcraft)

(luv.spir-v:define-shader-method
    luv.spir-v:shader-specification-for
    metal-draw-probe-vertex-specification
    ((role (eql :metal-draw-probe)) (stage (eql :vertex)))
    (:stage :vertex
     :inputs ((position :vec3 :location 0))
     :outputs ((clip-position :vec4 :built-in :position)))
  (let* ((clip (luv.spir-v:vec4 position 1.0)))
    (luv.spir-v:set-output clip-position clip)))

(luv.spir-v:define-shader-method
    luv.spir-v:shader-specification-for
    metal-draw-probe-fragment-specification
    ((role (eql :metal-draw-probe)) (stage (eql :fragment)))
    (:stage :fragment
     :outputs ((color :vec4 :location 0)))
  (let* ((rgba (luv.spir-v:vec4 0.85 0.25 0.75 1.0)))
    (luv.spir-v:set-output color rgba)))

(defun require-selector-sequence (selectors required)
  (let ((tail selectors))
    (dolist (selector required)
      (setf tail (member selector tail :test #'string=))
      (unless tail
        (error "Missing Metal selector ~S after ~S." selector selectors))
      (setf tail (rest tail))))
  selectors)

(defun probe-generated-metal-draw ()
  (let ((canvas
          (luv:make-sdl-canvas
           :title "generated Metal 4 triangle"
           :width 96 :height 64 :visible-p nil :presentation-api :metal))
        (provider (make-instance 'luv:metal-gpu-provider))
        (device nil)
        (context nil)
        (buffer nil)
        (artifact nil)
        (trace nil)
        (argument-table-protocol nil)
        (evidence nil))
    (unwind-protect
         (progn
           (luv:open-canvas canvas)
           (setf device (luv:request-gpu-device provider)
                 context
                 (luv:make-canvas-context
                  canvas provider
                  (luv:make-canvas-configuration :device device))
                 buffer
                 (luv:create
                  device
                  (luv:make-buffer-descriptor
                   :label "generated triangle vertices"
                   :size 36 :usage '(:vertex :copy-dst))))
           (luv:write-buffer
            buffer
            (make-array
             9 :element-type 'single-float
             :initial-contents
             '(-0.65 -0.55 0.0
                0.65 -0.55 0.0
                0.0 0.70 0.0)))
           (setf artifact
                 (luv::make-live-shader-pipeline
                  :role :metal-draw-probe
                  :vertex-role :metal-draw-probe
                  :label "generated live Metal 4 triangle"
                  :device device :layout nil
                  :vertex-buffers
                  '((:array-stride 12
                     :attributes
                     ((:shader-location 0 :offset 0 :format :float32x3))))
                  :target-format (luv:canvas-format context)
                  :primitive '(:topology :triangle-list)
                  :depth-stencil nil))
           (luv:request-canvas-frame
            canvas
            (lambda (timestamp)
              (declare (ignore timestamp))
              (luv.objective-c:with-objective-c-trace (active-trace)
                (setf trace active-trace)
                (luv:call-with-canvas-frame
                 context
                 (lambda (surface-texture encoder)
                   (let ((pass
                           (luv:begin-render-pass
                            encoder
                            (luv:make-render-pass-descriptor
                             :label "generated triangle pass"
                             :color-attachments
                             `((:view ,surface-texture
                                :load-op :clear :store-op :store
                                :clear-value #(0.04 0.05 0.08 1.0)))))))
                     (luv:set-pipeline
                      pass
                      (luv::live-shader-pipeline-native-pipeline artifact))
                     (setf argument-table-protocol
                           (luv.objective-c:objective-c-object-protocol-name
                            (luv:metal-render-pass-argument-table pass)))
                     (luv:set-vertex-buffer pass 0 buffer)
                     (luv:draw pass 3)
                     (luv:end-pass pass)))))))
           (luv:submitted-work-done (luv:device-queue device))
           (let ((selectors (luv::metal-submission-selectors trace)))
             (require-selector-sequence
              selectors
              '("beginCommandBufferWithAllocator:"
                "setRenderPipelineState:"
                "setAddress:attributeStride:atIndex:"
                "setArgumentTable:atStages:"
                "drawPrimitives:vertexStart:vertexCount:instanceCount:baseInstance:"
                "endEncoding"
                "endCommandBuffer"
                "waitForDrawable:"
                "commit:count:"
                "signalDrawable:"
                "present"
                "signalEvent:value:"))
             (setf evidence
                   (list
                    :device
                    (luv.objective-c:objective-c-string
                     (luv.metal:device-name (luv::metal-native-object device)))
                    :pipeline
                    (luv.objective-c:objective-c-object-protocol-name
                     (luv::metal-native-object
                      (luv::live-shader-pipeline-native-pipeline artifact)))
                    :argument-table argument-table-protocol
                    :vertex-buffer
                    (luv.objective-c:objective-c-object-protocol-name
                     (luv::metal-native-object buffer))
                    :submitted-value
                    (luv::metal-queue-submitted-value (luv:device-queue device))
                    :completed-value
                    (luv.metal:metal-shared-event-signaled-value
                     (luv::metal-queue-completion-event
                      (luv:device-queue device)))
                    :pending-command-allocators
                    (length
                     (luv::metal-queue-pending-submissions
                      (luv:device-queue device)))
                    :submission-selectors selectors
                    :drawn t))))
      (when artifact
        (luv::release-live-shader-pipeline artifact))
      (when buffer (luv:destroy buffer))
      (when (member (luv:canvas-state canvas) '(:opening :open))
        (luv:close-canvas canvas))
      (when device (luv:destroy device)))
    evidence))

(handler-case
    (progn
      (format t "~S~%"
              (luv:call-with-sdl-main-thread
               #'probe-generated-metal-draw))
      (finish-output))
  (error (condition)
    (format *error-output* "Metal draw probe failed: ~A~%" condition)
    (finish-output *error-output*)
    (uiop:quit 1)))
