(in-package #:luv.mcclim)

(defclass luv-port (mcclim-render:render-port-mixin)
  ((mirrors
    :initform nil
    :accessor port-mirrors)
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
   "A McCLIM display connection whose presentation targets are owned by luv."))

(defclass luv-medium (mcclim-render:render-medium-mixin basic-medium)
  ()
  (:documentation
   "A CPU raster medium whose image will be uploaded to a luv target."))

(defclass luv-graft (graft)
  ((dpi
    :initarg :dpi
    :reader graft-dpi))
  (:documentation "The root of the McCLIM sheet hierarchy on a LUV-PORT."))

(defmethod find-port-type ((type (eql :luv)))
  (values 'luv-port 'identity))

(defmethod initialize-instance :after ((port luv-port) &key)
  (unless (port-pointer port)
    (setf (port-pointer port)
          (make-instance 'standard-pointer :port port))))

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

The canvas event bridge will replace this deliberately quiet starting point
with delivery of translated luv events."
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

(defmethod make-medium ((port luv-port) sheet)
  ;; MCCLIM-RENDER gives us a useful, inspectable CPU image immediately.  The
  ;; missing next step is presenting dirty portions of that image through the
  ;; mirror's luv target.
  (make-instance 'luv-medium :port port :sheet sheet))

(defmethod port-force-output ((port luv-port))
  (declare (ignore port))
  nil)

(defmethod destroy-port :before ((port luv-port))
  (dolist (mirror (copy-list (port-mirrors port)))
    (let ((target (mirror-target mirror)))
      (when (member (luv:canvas-state target) '(:opening :open))
        (luv:close-canvas target))))
  (setf (port-mirrors port) nil))
