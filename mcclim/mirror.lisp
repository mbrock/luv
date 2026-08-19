(in-package #:mcluv)

(defclass luv-mirror (luv:canvas-event-handler)
  ((sheet
    :initarg :sheet
    :reader mirror-sheet)
   (target
    :initarg :target
    :reader mirror-target)
   (context
    :initarg :context
    :initform nil
    :accessor mirror-context)
   (device
    :initarg :device
    :initform nil
    :accessor mirror-device)
   (embedded-p
    :initarg :embedded-p
    :initform nil
    :reader mirror-embedded-p))
  (:documentation
   "A McCLIM sheet's relationship with a luv presentation target.

TARGET is initially a native canvas.  It is intentionally not part of the
mirror's identity: a later target may be a texture presented on a 3D quad."))

(defvar *embedded-mirror-target* nil)
(defvar *embedded-mirror-context* nil)
(defvar *embedded-mirror-device* nil)

(defclass luv-raster-mirror (luv-mirror mcclim-render:image-mirror-mixin)
  ((texture
    :initform nil
    :accessor mirror-texture)
   (compositor
    :initform nil
    :accessor mirror-compositor))
  (:documentation
   "A luv mirror retaining McCLIM's CPU raster image for later upload."))

(defclass luv-gpu-mirror (luv-mirror)
  ((texture
    :initform nil
    :accessor mirror-texture)
   (compositor
    :initform nil
    :accessor mirror-compositor)
   (frame-states
    :initform (make-hash-table :test #'equal)
    :reader gpu-mirror-frame-states)
   (vertex-module :initform nil :accessor gpu-mirror-vertex-module)
   (fragment-module :initform nil :accessor gpu-mirror-fragment-module)
   (layout :initform nil :accessor gpu-mirror-layout)
   (uniform-buffer :initform nil :accessor gpu-mirror-uniform-buffer)
   (bind-group :initform nil :accessor gpu-mirror-bind-group)
   (pipeline :initform nil :accessor gpu-mirror-pipeline)
   (format :initform nil :accessor gpu-mirror-format)
   (analytic-vertex-module :initform nil
                           :accessor gpu-mirror-analytic-vertex-module)
   (analytic-fragment-module :initform nil
                             :accessor gpu-mirror-analytic-fragment-module)
   (analytic-pipeline :initform nil :accessor gpu-mirror-analytic-pipeline)
   (relief-vertex-module :initform nil
                         :accessor gpu-mirror-relief-vertex-module)
   (relief-fragment-module :initform nil
                           :accessor gpu-mirror-relief-fragment-module)
   (relief-pipeline :initform nil :accessor gpu-mirror-relief-pipeline)
   (gradient-analytic-vertex-module
    :initform nil :accessor gpu-mirror-gradient-analytic-vertex-module)
   (gradient-analytic-fragment-module
    :initform nil :accessor gpu-mirror-gradient-analytic-fragment-module)
   (gradient-analytic-pipeline
    :initform nil :accessor gpu-mirror-gradient-analytic-pipeline)
   (image-vertex-module
    :initform nil :accessor gpu-mirror-image-vertex-module)
   (image-fragment-module
    :initform nil :accessor gpu-mirror-image-fragment-module)
   (image-layout :initform nil :accessor gpu-mirror-image-layout)
   (image-sampler :initform nil :accessor gpu-mirror-image-sampler)
   (image-pipeline :initform nil :accessor gpu-mirror-image-pipeline)
   (image-paints
    :initform (make-hash-table :test #'eq)
    :reader gpu-mirror-image-paints)
   (lattice-vertex-module
    :initform nil :accessor gpu-mirror-lattice-vertex-module)
   (lattice-fragment-module
    :initform nil :accessor gpu-mirror-lattice-fragment-module)
   (lattice-layout :initform nil :accessor gpu-mirror-lattice-layout)
   (lattice-pipeline :initform nil :accessor gpu-mirror-lattice-pipeline)
   (lattice-paints
    :initform (make-hash-table :test #'eq)
    :reader gpu-mirror-lattice-paints)
   (slug-cache :initform nil :accessor gpu-mirror-slug-cache)
   (text-vertex-module :initform nil
                       :accessor gpu-mirror-text-vertex-module)
   (text-fragment-module :initform nil
                         :accessor gpu-mirror-text-fragment-module)
   (text-layout :initform nil :accessor gpu-mirror-text-layout)
   (text-pipeline :initform nil :accessor gpu-mirror-text-pipeline)
   (text-bind-groups
    :initform (make-hash-table :test #'eq)
    :reader gpu-mirror-text-bind-groups)
   ;; The last immutable semantic stream prepared for presentation. Embedded
   ;; compositors replay it into the game's actual scene or HUD pass.
   (prepared-commands
    :initform nil :accessor gpu-mirror-prepared-commands)
   (prepared-frame-state
    :initform nil :accessor gpu-mirror-prepared-frame-state))
  (:documentation
   "A direct GPU target for an ordered LUV-GPU-MEDIUM drawing stream.

An embedded mirror retains prepared GPU commands and buffers for the game's
final pass; a standalone mirror renders into its native canvas drawable."))

(defmethod print-object ((mirror luv-mirror) stream)
  (print-unreadable-object (mirror stream :type t :identity t)
    (format stream "~S on ~S" (mirror-sheet mirror) (mirror-target mirror))))

(defun sheet-title (sheet)
  (or (and (typep sheet 'top-level-sheet-mixin)
           (sheet-pretty-name sheet))
      "McCLIM on luv"))

(defgeneric make-luv-mirror (port sheet target region)
  (:documentation "Construct PORT's renderer-specific mirror for TARGET."))

(defmethod make-luv-mirror ((port luv-port) sheet target region)
  (declare (ignore port region))
  (make-instance 'luv-mirror :sheet sheet :target target))

(defmethod make-luv-mirror ((port luv-raster-port) sheet target region)
  (let ((mirror (make-instance 'luv-raster-mirror
                               :sheet sheet
                               :target target)))
    (mcclim-render::%set-image-region mirror region)
    mirror))

(defmethod make-luv-mirror ((port luv-gpu-port) sheet target region)
  (declare (ignore port region))
  (make-instance 'luv-gpu-mirror :sheet sheet :target target))

(defun canvas-button-to-clim-button (button)
  (ecase button
    (:left +pointer-left-button+)
    (:middle +pointer-middle-button+)
    (:right +pointer-right-button+)))

(defun update-luv-pointer-position (pointer canvas event)
  (multiple-value-bind (canvas-x canvas-y) (luv:canvas-position canvas)
    (setf (luv-pointer-x pointer)
          (+ (or canvas-x 0) (luv:canvas-pointer-event-x event))
          (luv-pointer-y pointer)
          (+ (or canvas-y 0) (luv:canvas-pointer-event-y event)))))

(defun drain-luv-frame-events (sheet)
  "Handle events for a callback-only frame while dispatching native input.

Conventional RUN-FRAME-TOP-LEVEL frames consume their own queues instead."
  (let ((queue (climi::frame-event-queue (pane-frame sheet))))
    (loop for event = (climi::queue-read-no-hang queue)
          while event
          do (handle-event (event-sheet event) event))))

(defgeneric service-luv-frame-events (mirror)
  (:documentation
   "Drain callback-only input and publish any resulting visual state."))

(defmethod service-luv-frame-events ((mirror luv-mirror))
  "Service callback-only frames without stealing a real frame loop's queue."
  (let* ((sheet (mirror-sheet mirror))
         (frame (pane-frame sheet)))
    (unless (climi::frame-process frame)
      (drain-luv-frame-events sheet)
      (present-mirror mirror))))

(defun distribute-canvas-pointer-event (mirror canvas event class
                                        &key button)
  (let* ((sheet (mirror-sheet mirror))
         (port (port sheet))
         (pointer (ensure-luv-port-pointer port)))
    (update-luv-pointer-position pointer canvas event)
    (distribute-event
     port
     (apply #'make-instance class
            :sheet sheet
            :pointer pointer
            :x (luv:canvas-pointer-event-x event)
            :y (luv:canvas-pointer-event-y event)
            :timestamp (luv:canvas-event-timestamp event)
            (when button (list :button button))))
    (service-luv-frame-events mirror)))

(defmethod luv:handle-canvas-event
    ((mirror luv-mirror) canvas (event luv:canvas-pointer-motion-event))
  (distribute-canvas-pointer-event
   mirror canvas event 'pointer-motion-event))

(defmethod luv:handle-canvas-event
    ((mirror luv-mirror) canvas (event luv:canvas-pointer-enter-event))
  (distribute-canvas-pointer-event
   mirror canvas event 'pointer-enter-event))

(defmethod luv:handle-canvas-event
    ((mirror luv-mirror) canvas (event luv:canvas-pointer-exit-event))
  (distribute-canvas-pointer-event
   mirror canvas event 'pointer-exit-event))

(defmethod luv:handle-canvas-event
    ((mirror luv-mirror) canvas
     (event luv:canvas-pointer-button-press-event))
  (when (member (luv:canvas-pointer-event-button event)
                '(:left :middle :right))
    (let* ((port (port (mirror-sheet mirror)))
           (pointer (ensure-luv-port-pointer port))
           (button
             (canvas-button-to-clim-button
              (luv:canvas-pointer-event-button event))))
      (setf (luv-pointer-button-state pointer)
            (logior (luv-pointer-button-state pointer) button))
      (distribute-canvas-pointer-event
       mirror canvas event 'pointer-button-press-event :button button))))

(defmethod luv:handle-canvas-event
    ((mirror luv-mirror) canvas
     (event luv:canvas-pointer-button-release-event))
  (when (member (luv:canvas-pointer-event-button event)
                '(:left :middle :right))
    (let* ((port (port (mirror-sheet mirror)))
           (pointer (ensure-luv-port-pointer port))
           (button
             (canvas-button-to-clim-button
              (luv:canvas-pointer-event-button event))))
      (setf (luv-pointer-button-state pointer)
            (logandc2 (luv-pointer-button-state pointer) button))
      (distribute-canvas-pointer-event
       mirror canvas event 'pointer-button-release-event :button button))))

(defun canvas-modifiers-to-clim-state (modifiers)
  (reduce #'logior
          (mapcar (lambda (modifier)
                    (case modifier
                      (:shift +shift-key+)
                      (:control +control-key+)
                      (:meta +meta-key+)
                      (:super +super-key+)
                      ;; :NUM-LOCK and :CAPS-LOCK arrive with every key
                      ;; while latched, and CLIM has no state bit for them.
                      ;; A latched lock must not error a key event -- that
                      ;; fuses whatever overlay was focused.
                      (t 0)))
                  modifiers)
          :initial-value 0))

(defun distribute-canvas-key-event (mirror event class)
  (let* ((sheet (mirror-sheet mirror))
         (port (port sheet))
         (pointer (ensure-luv-port-pointer port))
         (modifier-state
           (canvas-modifiers-to-clim-state
            (luv:canvas-key-event-modifiers event))))
    (setf (luv-port-modifier-state port) modifier-state)
    (multiple-value-bind (x y)
        (climi::sheet-pointer-position sheet pointer)
      (distribute-event
       port
       (apply #'make-instance class
              :sheet sheet
              :x x :y y
              :timestamp (luv:canvas-event-timestamp event)
              :modifier-state modifier-state
              :key-name (luv:canvas-key-event-key-name event)
              (when (luv:canvas-key-event-character event)
                (list :key-character
                      (luv:canvas-key-event-character event))))))
    (service-luv-frame-events mirror)))

(defmethod luv:handle-canvas-event
    ((mirror luv-mirror) canvas (event luv:canvas-key-press-event))
  (declare (ignore canvas))
  (distribute-canvas-key-event mirror event 'key-press-event))

(defmethod luv:handle-canvas-event
    ((mirror luv-mirror) canvas (event luv:canvas-key-release-event))
  (declare (ignore canvas))
  (distribute-canvas-key-event mirror event 'key-release-event))

(defmethod luv:handle-canvas-event
    ((mirror luv-mirror) canvas
     (event luv:canvas-window-focus-gained-event))
  (declare (ignore canvas))
  (let ((sheet (mirror-sheet mirror)))
    (distribute-event
     (port sheet)
     (make-instance 'window-manager-focus-event
                    :sheet sheet
                    :timestamp (luv:canvas-event-timestamp event)))
    (service-luv-frame-events mirror)))

(defun make-mirror-configuration-event (mirror width height timestamp)
  (let ((sheet (mirror-sheet mirror)))
    ;; SDL resize events contain dimensions but not an origin. Preserve the
    ;; native origin McCLIM already knows when constructing its region.
    (with-bounding-rectangle* (x y) (climi::sheet-mirror-geometry sheet)
      (make-instance
       'window-configuration-event
       :sheet sheet :timestamp timestamp
       :region (make-bounding-rectangle x y (+ x width) (+ y height))))))

(defmethod luv:handle-canvas-event
    ((mirror luv-mirror) canvas (event luv:canvas-window-resized-event))
  (declare (ignore canvas))
  (let ((sheet (mirror-sheet mirror))
        (width (luv:canvas-window-event-width event))
        (height (luv:canvas-window-event-height event)))
    (distribute-event
     (port sheet)
     (make-mirror-configuration-event
      mirror width height
      (luv:canvas-event-timestamp event)))
    (service-luv-frame-events mirror)))

(defmethod luv:handle-canvas-event
    ((mirror luv-raster-mirror) canvas
     (event luv:canvas-window-pixel-size-changed-event))
  (declare (ignore canvas))
  ;; Configuration events are placed ahead of repaint events by McCLIM, so a
  ;; logical resize is laid out before we redraw for the new pixel extent.
  (let ((sheet (mirror-sheet mirror)))
    (distribute-event
     (port sheet)
     (make-instance 'window-repaint-event
                    :sheet sheet
                    :timestamp (luv:canvas-event-timestamp event)
                    :region +everywhere+))
    (service-luv-frame-events mirror)))

(defmethod luv:handle-canvas-event
    ((mirror luv-gpu-mirror) canvas
     (event luv:canvas-window-pixel-size-changed-event))
  (declare (ignore canvas))
  ;; The GPU path has no dirty image to resize, but a new drawable extent still
  ;; requires McCLIM to rebuild device-coordinate geometry before presentation.
  (let ((sheet (mirror-sheet mirror)))
    (distribute-event
     (port sheet)
     (make-instance 'window-repaint-event
                    :sheet sheet
                    :timestamp (luv:canvas-event-timestamp event)
                    :region +everywhere+))
    (service-luv-frame-events mirror)))

(defmethod luv:handle-canvas-event
    ((mirror luv-mirror) canvas
     (event luv:canvas-window-focus-lost-event))
  (declare (ignore mirror canvas event))
  nil)

(defmethod luv:handle-canvas-event
    ((mirror luv-mirror) canvas
     (event luv:canvas-window-close-request-event))
  (declare (ignore canvas))
  (let* ((sheet (mirror-sheet mirror))
         (frame (pane-frame sheet)))
    ;; Callback-only demos are torn down by the canvas itself.  A conventional
    ;; frame top level needs the CLIM delete event so its unwind protocol runs.
    (when (climi::frame-process frame)
      (distribute-event
       (port sheet)
       (make-instance 'window-manager-delete-event
                      :sheet sheet
                      :timestamp (luv:canvas-event-timestamp event))))))

(defun raster-mirror-image-size (mirror)
  (let ((image (mcclim-render:image-mirror-image mirror)))
    (list (pattern-width image) (pattern-height image))))

(defun ensure-raster-mirror-context (mirror)
  (when (mirror-embedded-p mirror)
    (return-from ensure-raster-mirror-context (mirror-context mirror)))
  (let* ((target (mirror-target mirror))
         (device
           (or (mirror-device mirror)
               (setf (mirror-device mirror)
                     (luv:request-gpu-device luv:*gpu-provider*))))
         (context
           (or (mirror-context mirror)
               (setf (mirror-context mirror)
                     (luv:make-canvas-context
                      target luv:*gpu-provider*
                      (luv:make-canvas-configuration :device device))))))
    (multiple-value-bind (width height) (luv:canvas-size target)
      (unless (equal (list width height) (luv:canvas-extent context))
        (luv:configure-canvas-context
         context
         (luv:make-canvas-configuration
          :device (luv:context-device context)
          :format (luv:canvas-format context)
          :usage '(:copy-dst)))))
    context))

(defun release-mirror-device (mirror)
  (alexandria:when-let ((device (mirror-device mirror)))
    (luv:destroy device)
    (setf (mirror-device mirror) nil))
  mirror)

(defun ensure-raster-mirror-texture (mirror context size)
  (let ((texture (mirror-texture mirror))
        (format (luv:canvas-format context)))
    (unless (and texture
                 (equal (luv:gpu-texture-size texture)
                        (append size '(1)))
                 (eq (luv:gpu-texture-format texture) format))
      (let ((replacement
              (luv:create
               (luv:context-device context)
               (luv:make-texture-descriptor
                :label "McCLIM raster upload"
                :size size
                :usage '(:copy-src :copy-dst :texture-binding)
                :dimensions :2d
                :format format))))
        (when texture
          ;; A compositor may retain a view and descriptor for the old image.
          ;; Release those dependents before destroying their source.
          (release-raster-mirror-compositor (mirror-compositor mirror))
          (luv:destroy texture))
        (setf (mirror-texture mirror) replacement)))
    (mirror-texture mirror)))

(defgeneric present-raster-mirror-texture
    (mirror context texture compositor)
  (:documentation
   "Present an uploaded McCLIM TEXTURE through an optional COMPOSITOR."))

(defmethod present-raster-mirror-texture
    ((mirror luv-raster-mirror) context texture (compositor null))
  (unless (mirror-embedded-p mirror)
    (luv:present-canvas-frame
     context
     (lambda (surface encoder)
       (luv:encode
        encoder
        (luv:make-gpu-copy-texture-command
         :source texture :destination surface))))))

(defgeneric release-raster-mirror-compositor (compositor))

(defmethod release-raster-mirror-compositor ((compositor null))
  (values))

(defmethod present-mirror ((mirror luv-raster-mirror))
  "Upload and present MIRROR when McCLIM has marked its image dirty."
  (let ((target (mirror-target mirror))
        (deferred-size nil))
    (when (and (eq :open (luv:canvas-state target))
               (not (region-equal
                     (mcclim-render:image-dirty-region mirror)
                     +nowhere+)))
      ;; Acquire the image lock on the native thread.  Acquiring it before
      ;; dispatch would deadlock against native callbacks that draw.
      (luv:request-canvas-frame
       target
       (lambda (timestamp)
         (declare (ignore timestamp))
         (mcclim-render:with-image-locked (mirror)
           (unless (region-equal
                    (mcclim-render:image-dirty-region mirror)
                    +nowhere+)
             (let* ((image (mcclim-render:image-mirror-image mirror))
                    (size (raster-mirror-image-size mirror))
                    (context (ensure-raster-mirror-context mirror)))
               (if (and (not (mirror-embedded-p mirror))
                        (not (equal size (luv:canvas-extent context))))
                   ;; Swapchain creation is sometimes the first point where a
                   ;; Wayland compositor reveals its assigned size. Leave the
                   ;; raster dirty and let McCLIM lay it out before presenting.
                   (setf deferred-size (luv:canvas-extent context))
                   (let ((texture
                           (ensure-raster-mirror-texture
                            mirror context size)))
                     (luv:write-texture
                      (luv:device-queue (luv:context-device context))
                      (luv:make-texture-copy :texture texture)
                      (pattern-array image)
                      (luv:make-texture-data-layout
                       :bytes-per-row (* 4 (first size))
                       :rows-per-image (second size))
                      size)
                     (present-raster-mirror-texture
                      mirror context texture (mirror-compositor mirror))
                     (setf (mcclim-render:image-dirty-region mirror)
                           +nowhere+)))))))))
    (when deferred-size
      (destructuring-bind (width height) deferred-size
        (setf (luv:canvas-width target) width
              (luv:canvas-height target) height)
        (let ((sheet (mirror-sheet mirror)))
          (distribute-event
           (port sheet)
           (make-mirror-configuration-event mirror width height 0))
          (distribute-event
           (port sheet)
           (make-instance 'window-repaint-event
                          :sheet sheet :timestamp 0 :region +everywhere+))
          (unless (climi::frame-process (pane-frame sheet))
            (drain-luv-frame-events sheet)
            (present-mirror mirror))))))
  mirror)

(defmethod release-mirror-presentation ((mirror luv-raster-mirror))
  (release-raster-mirror-compositor (mirror-compositor mirror))
  (setf (mirror-compositor mirror) nil)
  (unless (mirror-embedded-p mirror)
    (setf (luv:canvas-clock (mirror-target mirror)) (luv:make-demand-clock)))
  (alexandria:when-let ((texture (mirror-texture mirror)))
    (luv:destroy texture)
    (setf (mirror-texture mirror) nil))
  mirror)

(defmethod realize-mirror ((port luv-port) (sheet mirrored-sheet-mixin))
  (with-bounding-rectangle* (x y :width width :height height) sheet
    (let* ((embedded-p (not (null *embedded-mirror-target*)))
           (canvas (or *embedded-mirror-target*
                       (luv:make-sdl-canvas
                        :title (sheet-title sheet)
                        :x (floor x)
                        :y (floor y)
                       :width (max 1 (ceiling width))
                       :height (max 1 (ceiling height))
                       :presentation-api
                       (luv:sdl-presentation-api-for luv:*gpu-provider*)
                       :visible-p nil)))
           (region (make-rectangle* 0 0
                                    (max 1 (ceiling width))
                                    (max 1 (ceiling height))))
           (mirror (make-luv-mirror port sheet canvas region)))
      (when embedded-p
        (setf (slot-value mirror 'embedded-p) t
              (mirror-context mirror) *embedded-mirror-context*
              (mirror-device mirror) *embedded-mirror-device*))
      (unless embedded-p
        (setf (luv:canvas-event-handler canvas) mirror))
      (handler-case
          (progn
            (unless embedded-p
              (luv:open-canvas canvas))
            ;; REALIZE-MIRROR's standard :AROUND method normally installs the
            ;; direct mirror only after this primary method returns.  Geometry
            ;; initialization needs it sooner: without a native region the
            ;; render medium clips every drawing operation to NOWHERE.
            (setf (sheet-direct-mirror sheet) mirror)
            ;; Synchronize McCLIM with whatever logical geometry is observable
            ;; now. Presentation performs a second reconciliation if Wayland
            ;; reveals the compositor-assigned extent only at swapchain time.
            (if embedded-p
                (handle-event
                 sheet
                 (make-mirror-configuration-event
                  mirror (max 1 (ceiling width))
                  (max 1 (ceiling height)) 0))
                (multiple-value-bind (actual-width actual-height)
                    (luv:canvas-logical-size canvas)
                  (setf (luv:canvas-width canvas) actual-width
                        (luv:canvas-height canvas) actual-height)
                  (handle-event
                   sheet
                   (make-mirror-configuration-event
                    mirror actual-width actual-height 0))))
            (push mirror (port-mirrors port))
            mirror)
        (error (condition)
          (when (eq (sheet-direct-mirror sheet) mirror)
            (setf (sheet-direct-mirror sheet) nil))
          (when (and (not embedded-p)
                     (member (luv:canvas-state canvas) '(:opening :open)))
            (ignore-errors (luv:close-canvas canvas)))
          (error condition))))))

(defmethod realize-mirror
    ((port luv-raster-port) (sheet mirrored-sheet-mixin))
  ;; Resolve the renderer/base-port diamond explicitly.  Renderer selection
  ;; changes the mirror class, not the native host lifecycle.
  (call-next-method))

(defmethod realize-mirror
    ((port luv-gpu-port) (sheet mirrored-sheet-mixin))
  (call-next-method))

(defmethod destroy-mirror ((port luv-port) (sheet mirrored-sheet-mixin))
  (let ((mirror (sheet-direct-mirror sheet)))
    (when mirror
      (let ((target (mirror-target mirror)))
        (release-mirror-presentation mirror)
        (unless (mirror-embedded-p mirror)
          (setf (luv:canvas-event-handler target) nil)
          (when (member (luv:canvas-state target) '(:opening :open))
            (luv:close-canvas target))
          (release-mirror-device mirror))
        (setf (mirror-context mirror) nil))
      (setf (port-mirrors port)
            (delete mirror (port-mirrors port))))))

(defmethod enable-mirror ((port luv-port) (sheet mirrored-sheet-mixin))
  (declare (ignore port))
  (alexandria:when-let ((mirror (sheet-direct-mirror sheet)))
    (let ((target (mirror-target mirror)))
      (unless (mirror-embedded-p mirror)
        (luv:show-canvas target))
      ;; There is no separate McCLIM top-level loop to provoke the first
      ;; exposure. Paint and present before OPEN-WIDGET-LAB returns instead
      ;; of waiting for the first pointer event to dirty a gadget.
      (luv:request-canvas-frame
       target
       (lambda (timestamp)
         (declare (ignore timestamp))
         (repaint-sheet sheet +everywhere+)
         (present-mirror mirror))))))

(defmethod disable-mirror ((port luv-port) (sheet mirrored-sheet-mixin))
  (declare (ignore port))
  (alexandria:when-let ((mirror (sheet-direct-mirror sheet)))
    (unless (mirror-embedded-p mirror)
      (luv:hide-canvas (mirror-target mirror)))))

(defmethod set-mirror-geometry
    ((port luv-port) (sheet mirrored-sheet-mixin) region)
  (declare (ignore port))
  (with-bounding-rectangle* (x1 y1 x2 y2 :width width :height height) region
    (alexandria:when-let ((mirror (sheet-direct-mirror sheet)))
      (unless (mirror-embedded-p mirror)
        (let ((target (mirror-target mirror)))
          (luv:move-canvas target (floor x1) (floor y1))
          (luv:resize-canvas target
                             (max 1 (ceiling width))
                             (max 1 (ceiling height))))))
    (values x1 y1 x2 y2)))

(defmethod set-mirror-name
    ((port luv-port) (sheet top-level-sheet-mixin) name)
  (declare (ignore port))
  (alexandria:when-let ((mirror (sheet-direct-mirror sheet)))
    (unless (mirror-embedded-p mirror)
      (setf (luv:canvas-title (mirror-target mirror)) name))))

(defmethod set-mirror-icon
    ((port luv-port) (sheet top-level-sheet-mixin) icon)
  (declare (ignore port sheet icon))
  nil)

(defmethod raise-mirror ((port luv-port) (sheet top-level-sheet-mixin))
  (declare (ignore port))
  (alexandria:when-let ((mirror (sheet-direct-mirror sheet)))
    (luv:raise-canvas (mirror-target mirror))))

(defmethod bury-mirror ((port luv-port) (sheet top-level-sheet-mixin))
  (declare (ignore port sheet))
  nil)

(defmethod shrink-mirror ((port luv-port) (sheet top-level-sheet-mixin))
  (declare (ignore port))
  (alexandria:when-let ((mirror (sheet-direct-mirror sheet)))
    (luv:minimize-canvas (mirror-target mirror))))

(defmethod unshrink-mirror ((port luv-port) (sheet top-level-sheet-mixin))
  (declare (ignore port))
  (alexandria:when-let ((mirror (sheet-direct-mirror sheet)))
    (luv:restore-canvas (mirror-target mirror))))
