(in-package #:mcluv)

(defclass luv-port (basic-port)
  ((mirrors
    :initform nil
    :accessor port-mirrors)
   (modifier-state
    :initform 0
    :accessor luv-port-modifier-state)
   (graft-width
    :initarg :graft-width
    :initform 1920
    :reader port-graft-width)
   (graft-height
    :initarg :graft-height
    :initform 1080
    :reader port-graft-height)
   (graft-dpi
    :initarg :graft-dpi
    :initform 96
    :reader port-graft-dpi))
  (:documentation
   "Renderer-independent McCLIM display connection owned by luv."))

(defclass luv-raster-port (luv-port mcclim-render:render-port-mixin)
  ()
  (:documentation
   "A luv port whose media rasterize into MCCLIM-RENDER images."))

(defclass luv-raster-medium (mcclim-render:render-medium-mixin basic-medium)
  ()
  (:documentation
   "A CPU raster medium whose image will be uploaded to a luv target."))

(defclass luv-gpu-port (luv-port)
  ()
  (:documentation
   "A luv port whose media record McCLIM drawing directly as GPU geometry."))

(defclass luv-gpu-medium (basic-medium)
  ((vertices
    :initform (make-array 1024 :element-type 'single-float
                          :adjustable t :fill-pointer 0)
    :reader gpu-medium-vertices)
   (analytic-vertices
    :initform (make-array 1024 :element-type 'single-float
                          :adjustable t :fill-pointer 0)
    :reader gpu-medium-analytic-vertices)
   (commands
    :initform (make-array 32 :adjustable t :fill-pointer 0)
    :reader gpu-medium-commands)
   (fallback-statistics
    :initform (make-hash-table :test #'eq)
    :reader gpu-medium-fallback-statistics)
   (buffering-depth
    :initform 0
    :accessor gpu-medium-buffering-depth))
  (:documentation
   "An ordered McCLIM drawing stream containing no software pixel surface."))

(defclass luv-frame-manager (standard-frame-manager)
  ()
  (:documentation
   "A McCLIM frame manager constrained by luv's current native host."))

(defmethod frame-manager-menu-choose
    ((manager luv-frame-manager) items &rest options)
  (declare (ignore manager items))
  ;; McCLIM implements menus as temporary top-level frames.  SDL's current
  ;; Cocoa host gives one canvas a durable process-main-thread event loop, so a
  ;; menu would require precisely the unsupported second native canvas that
  ;; CLAIM-SDL-CANVAS-HOST rejects.  Decline it at the semantic boundary and
  ;; leave the owning application frame responsive.
  (warn "~A is unavailable: luv's Cocoa host currently supports one native canvas."
        (or (getf options :label) "McCLIM popup menu"))
  (values nil nil nil))

(defclass luv-pointer (standard-pointer)
  ((x :initform 0 :accessor luv-pointer-x)
   (y :initform 0 :accessor luv-pointer-y)
   (button-state
    :initform 0
    :accessor luv-pointer-button-state))
  (:documentation "McCLIM pointer state cached from portable canvas events."))

(defmethod pointer-position ((pointer luv-pointer))
  (values (luv-pointer-x pointer) (luv-pointer-y pointer)))

(defmethod pointer-button-state ((pointer luv-pointer))
  (luv-pointer-button-state pointer))

(defmethod port-modifier-state ((port luv-port))
  (luv-port-modifier-state port))

(defun ensure-luv-port-pointer (port)
  (let ((pointer (port-pointer port)))
    (if (typep pointer 'luv-pointer)
        pointer
        (setf (port-pointer port)
              (make-instance 'luv-pointer :port port)))))

(defgeneric present-mirror (mirror)
  (:documentation "Synchronize MIRROR's pending output with its target."))

(defgeneric release-mirror-presentation (mirror)
  (:documentation "Release renderer-specific resources retained by MIRROR."))

(defmethod present-mirror ((mirror t))
  mirror)

(defmethod release-mirror-presentation ((mirror t))
  mirror)

(defclass luv-graft (graft)
  ((dpi
    :initarg :dpi
    :reader graft-dpi))
  (:documentation "The root of the McCLIM sheet hierarchy on a LUV-PORT."))

(defmethod find-port-type ((type (eql :luv)))
  (values 'luv-raster-port 'identity))

(defmethod find-port-type ((type (eql :luv-raster)))
  (values 'luv-raster-port 'identity))

(defmethod find-port-type ((type (eql :luv-gpu)))
  (values 'luv-gpu-port 'identity))

(defmethod initialize-instance :after ((port luv-port) &key)
  (ensure-luv-port-pointer port))

(defmethod print-object ((port luv-port) stream)
  (print-unreadable-object (port stream :type t :identity t)
    (format stream "~D mirror~:P" (length (port-mirrors port)))))

(defmethod make-graft ((port luv-port)
                       &key (orientation :default) (units :device))
  (make-instance 'luv-graft
                 :port port
                 :mirror t
                 :region (make-bounding-rectangle
                          0 0
                          (port-graft-width port)
                          (port-graft-height port))
                 :orientation orientation
                 :units units
                 :dpi (port-graft-dpi port)))

(defun graft-dimension (pixels dpi units)
  (ecase units
    (:screen-sized 1)
    (:device pixels)
    (:inches (/ pixels dpi))
    (:millimeters (* 25.4 (/ pixels dpi)))))

(defmethod graft-width ((graft luv-graft) &key (units :device))
  (graft-dimension (port-graft-width (port graft))
                   (graft-dpi graft)
                   units))

(defmethod graft-height ((graft luv-graft) &key (units :device))
  (graft-dimension (port-graft-height (port graft))
                   (graft-dpi graft)
                   units))

(defmethod process-next-event ((port luv-port)
                               &key wait-function timeout)
  "Wait cooperatively until McCLIM has another reason to run.

Concurrent McCLIM sheet queues are woken directly when luv appends translated
events.  This quiet fallback remains for single-process queue users."
  (declare (ignore port))
  (labels ((ready-p () (and wait-function (funcall wait-function))))
    (when (ready-p)
      (return-from process-next-event (values nil :wait-function)))
    (cond (timeout
           (sleep timeout)
           (if (ready-p)
               (values nil :wait-function)
               (values nil :timeout)))
          (wait-function
           (loop until (ready-p) do (sleep 0.05))
           (values nil :wait-function))
          (t
           ;; A port I/O process may call us with neither argument.  Remain
           ;; quiescent until it is interrupted or real event delivery exists.
           (loop do (sleep 3600))))))

(defmethod make-medium ((port luv-raster-port) sheet)
  ;; MCCLIM-RENDER gives us a useful, inspectable CPU image.  Finish/force
  ;; output synchronizes its dirty contents with the mirror's GPU texture.
  (make-instance 'luv-raster-medium :port port :sheet sheet))

(defmethod make-medium ((port luv-gpu-port) sheet)
  (make-instance 'luv-gpu-medium :port port :sheet sheet))

(defmethod port-force-output ((port luv-port))
  (declare (ignore port))
  nil)

(defmethod port-force-output ((port luv-raster-port))
  (dolist (mirror (port-mirrors port))
    (present-mirror mirror))
  nil)

(defmethod port-force-output ((port luv-gpu-port))
  (dolist (mirror (port-mirrors port))
    (present-mirror mirror))
  nil)

(defmethod medium-finish-output :before ((medium luv-raster-medium))
  (alexandria:when-let ((mirror (medium-drawable medium)))
    (present-mirror mirror)))

(defmethod medium-force-output :before ((medium luv-raster-medium))
  (alexandria:when-let ((mirror (medium-drawable medium)))
    (present-mirror mirror)))

(defmethod medium-finish-output :before ((medium luv-gpu-medium))
  (when (zerop (gpu-medium-buffering-depth medium))
    (alexandria:when-let ((mirror (medium-drawable medium)))
      (present-mirror mirror))))

(defmethod medium-force-output :before ((medium luv-gpu-medium))
  (when (zerop (gpu-medium-buffering-depth medium))
    (alexandria:when-let ((mirror (medium-drawable medium)))
      (present-mirror mirror))))

(defmethod destroy-port :before ((port luv-port))
  (dolist (mirror (copy-list (port-mirrors port)))
    (let ((target (mirror-target mirror)))
      (release-mirror-presentation mirror)
      (unless (mirror-embedded-p mirror)
        (setf (luv:canvas-event-handler target) nil)
        (when (member (luv:canvas-state target) '(:opening :open))
          (luv:close-canvas target))
        (release-mirror-device mirror))
      (setf (mirror-context mirror) nil)))
  (setf (port-mirrors port) nil))
