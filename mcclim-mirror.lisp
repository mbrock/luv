(in-package #:luv.mcclim)

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
    :accessor mirror-context))
  (:documentation
   "A McCLIM sheet's relationship with a luv presentation target.

TARGET is initially a native canvas.  It is intentionally not part of the
mirror's identity: a later target may be a texture presented on a 3D quad."))

(defclass luv-raster-mirror (luv-mirror mcclim-render:image-mirror-mixin)
  ((texture
    :initform nil
    :accessor mirror-texture))
  (:documentation
   "A luv mirror retaining McCLIM's CPU raster image for later upload."))

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

(defun service-luv-frame-events (mirror)
  "Service callback-only frames without stealing a real frame loop's queue."
  (let* ((sheet (mirror-sheet mirror))
         (frame (pane-frame sheet)))
    (unless (climi::frame-process frame)
      (drain-luv-frame-events sheet))
    (present-mirror mirror)))

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
                    (ecase modifier
                      (:shift +shift-key+)
                      (:control +control-key+)
                      (:meta +meta-key+)
                      (:super +super-key+)))
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

(defun ensure-raster-mirror-context (mirror size)
  (let* ((target (mirror-target mirror))
         (context
           (or (mirror-context mirror)
               (setf (mirror-context mirror)
                     (luv:make-canvas-context target luv:*gpu-provider*)))))
    (unless (equal size (luv:canvas-extent context))
      (luv:configure-canvas-context
       context
       (luv:make-canvas-configuration
        :device (luv:context-device context)
        :format (luv:canvas-format context)
        :usage '(:copy-dst))))
    context))

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
                :usage '(:copy-src :copy-dst)
                :dimensions :2d
                :format format))))
        (when texture
          (luv:destroy texture))
        (setf (mirror-texture mirror) replacement)))
    (mirror-texture mirror)))

(defmethod present-mirror ((mirror luv-raster-mirror))
  "Upload and present MIRROR when McCLIM has marked its image dirty."
  (let ((target (mirror-target mirror)))
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
                    (pixels (pattern-array image))
                    (size (raster-mirror-image-size mirror))
                    (context (ensure-raster-mirror-context mirror size))
                    (texture
                      (ensure-raster-mirror-texture
                       mirror context size)))
               (luv:write-texture
                (luv:device-queue (luv:context-device context))
                (luv:make-texture-copy :texture texture)
                pixels
                (luv:make-texture-data-layout
                 :bytes-per-row (* 4 (first size))
                 :rows-per-image (second size))
                size)
               (luv:present-canvas-frame
                context
                (lambda (surface encoder)
                  (luv:encode
                   encoder
                   (luv:make-gpu-copy-texture-command
                    :source texture :destination surface))))
               (setf (mcclim-render:image-dirty-region mirror)
                     +nowhere+))))))))
  mirror)

(defmethod release-mirror-presentation ((mirror luv-raster-mirror))
  (alexandria:when-let ((texture (mirror-texture mirror)))
    (luv:destroy texture)
    (setf (mirror-texture mirror) nil))
  mirror)

(defmethod realize-mirror ((port luv-port) (sheet mirrored-sheet-mixin))
  (with-bounding-rectangle* (x y :width width :height height) sheet
    (let* ((canvas (luv:make-sdl-canvas
                    :title (sheet-title sheet)
                    :x (floor x)
                    :y (floor y)
                    :width (max 1 (ceiling width))
                    :height (max 1 (ceiling height))
                    :visible-p nil))
           (region (make-rectangle* 0 0
                                    (max 1 (ceiling width))
                                    (max 1 (ceiling height))))
           (mirror (make-luv-mirror port sheet canvas region)))
      (setf (luv:canvas-event-handler canvas) mirror)
      (handler-case
          (progn
            (luv:open-canvas canvas)
            ;; REALIZE-MIRROR's standard :AROUND method normally installs the
            ;; direct mirror only after this primary method returns.  Geometry
            ;; initialization needs it sooner: without a native region the
            ;; render medium clips every drawing operation to NOWHERE.
            (setf (sheet-direct-mirror sheet) mirror)
            (climi::update-mirror-geometry sheet)
            (push mirror (port-mirrors port))
            mirror)
        (error (condition)
          (when (eq (sheet-direct-mirror sheet) mirror)
            (setf (sheet-direct-mirror sheet) nil))
          (when (member (luv:canvas-state canvas) '(:opening :open))
            (ignore-errors (luv:close-canvas canvas)))
          (error condition))))))

(defmethod realize-mirror
    ((port luv-raster-port) (sheet mirrored-sheet-mixin))
  ;; Resolve the renderer/base-port diamond explicitly.  Renderer selection
  ;; changes the mirror class, not the native host lifecycle.
  (call-next-method))

(defmethod destroy-mirror ((port luv-port) (sheet mirrored-sheet-mixin))
  (let ((mirror (sheet-direct-mirror sheet)))
    (when mirror
      (let ((target (mirror-target mirror)))
        (release-mirror-presentation mirror)
        (setf (luv:canvas-event-handler target) nil)
        (when (member (luv:canvas-state target) '(:opening :open))
          (luv:close-canvas target))
        (setf (mirror-context mirror) nil))
      (setf (port-mirrors port)
            (delete mirror (port-mirrors port))))))

(defmethod enable-mirror ((port luv-port) (sheet mirrored-sheet-mixin))
  (declare (ignore port))
  (alexandria:when-let ((mirror (sheet-direct-mirror sheet)))
    (let ((target (mirror-target mirror)))
      (luv:show-canvas target)
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
    (luv:hide-canvas (mirror-target mirror))))

(defmethod set-mirror-geometry
    ((port luv-port) (sheet mirrored-sheet-mixin) region)
  (declare (ignore port))
  (with-bounding-rectangle* (x1 y1 x2 y2 :width width :height height) region
    (alexandria:when-let ((mirror (sheet-direct-mirror sheet)))
      (let ((target (mirror-target mirror)))
        (luv:move-canvas target (floor x1) (floor y1))
        (luv:resize-canvas target
                           (max 1 (ceiling width))
                           (max 1 (ceiling height)))))
    (values x1 y1 x2 y2)))

(defmethod set-mirror-name
    ((port luv-port) (sheet top-level-sheet-mixin) name)
  (declare (ignore port))
  (alexandria:when-let ((mirror (sheet-direct-mirror sheet)))
    (setf (luv:canvas-title (mirror-target mirror)) name)))

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
