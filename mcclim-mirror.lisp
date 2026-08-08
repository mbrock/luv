(in-package #:luv.mcclim)

(defclass luv-mirror ()
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
  ()
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
        (when (member (luv:canvas-state target) '(:opening :open))
          (luv:close-canvas target)))
      (setf (port-mirrors port)
            (delete mirror (port-mirrors port))))))

(defmethod enable-mirror ((port luv-port) (sheet mirrored-sheet-mixin))
  (declare (ignore port))
  (alexandria:when-let ((mirror (sheet-direct-mirror sheet)))
    (luv:show-canvas (mirror-target mirror))))

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
