(in-package #:luft.render)

;;; Reproducible live mounting for worksheet stage BY51K0 only.  This is an
;;; instrument because that is the current Luft attachment seam under test;
;;; it deliberately defines no shell policy and migrates no existing tool.

(defclass viewer-workbench-backend-proof ()
  ((frame :initarg :frame :reader viewer-workbench-backend-proof-frame)
   (compositor :initarg :compositor
               :reader viewer-workbench-backend-proof-compositor)
   (extent :initform nil :accessor viewer-workbench-backend-proof-extent)))

(defmethod viewer-instrument-priority
    ((instrument viewer-workbench-backend-proof))
  (declare (ignore instrument))
  10000)

(defun viewer-workbench-backend-proof-attachment (viewer)
  (find-if (lambda (instrument)
             (typep instrument 'viewer-workbench-backend-proof))
           (viewer-instruments viewer)))

(defun workbench-backend-proof-screen-state ()
  (make-array 12 :element-type 'single-float
              :initial-contents
              '(0.0 0.0 0.0 1.0
                1.0 0.0 0.0 0.0
                0.0 1.0 0.0 0.0)))

(defun resize-viewer-workbench-backend-proof (instrument viewer event)
  (declare (ignore viewer))
  (let ((extent (list (canvas-window-event-width event)
                      (canvas-window-event-height event))))
    (unless (equal extent (viewer-workbench-backend-proof-extent instrument))
      (setf (viewer-workbench-backend-proof-extent instrument) extent)
      (mcluv:dispatch-embedded-mirror-event
       (mcluv:workbench-backend-proof-mirror
        (viewer-workbench-backend-proof-frame instrument))
       event)))
  instrument)

(defmethod refresh-viewer-instrument
    ((instrument viewer-workbench-backend-proof) viewer)
  (destructuring-bind (width height) (viewer-logical-extent viewer)
    (resize-viewer-workbench-backend-proof
     instrument viewer
     (make-instance 'canvas-window-resized-event
                    :timestamp 0 :width width :height height)))
  (mcluv:prepare-gpu-mirror-compositor
   (mcluv:workbench-backend-proof-mirror
    (viewer-workbench-backend-proof-frame instrument)))
  instrument)

(defmethod encode-viewer-instrument
    ((instrument viewer-workbench-backend-proof)
     viewer pass surface-texture physical-extent)
  (declare (ignore viewer physical-extent))
  (mcluv:encode-direct-gpu-mirror
   (viewer-workbench-backend-proof-compositor instrument)
   pass surface-texture (workbench-backend-proof-screen-state))
  instrument)

(defmethod handle-viewer-instrument-event
    ((instrument viewer-workbench-backend-proof) viewer canvas
     (event canvas-window-resized-event))
  (declare (ignore canvas))
  (resize-viewer-workbench-backend-proof instrument viewer event)
  nil)

(defmethod handle-viewer-instrument-event
    ((instrument viewer-workbench-backend-proof) viewer canvas
     (event canvas-event))
  (declare (ignore viewer canvas))
  (when (typep event
               '(or canvas-pointer-motion-event canvas-pointer-button-event
                 canvas-key-event canvas-window-focus-gained-event
                 canvas-window-focus-lost-event))
    (mcluv:dispatch-embedded-mirror-event
     (mcluv:workbench-backend-proof-mirror
      (viewer-workbench-backend-proof-frame instrument))
     event)
    (typep event '(or canvas-pointer-event canvas-key-event))))

(defmethod release-viewer-instrument
    ((instrument viewer-workbench-backend-proof) viewer)
  (declare (ignore viewer))
  (mcluv:destroy-workbench-backend-proof
   (viewer-workbench-backend-proof-frame instrument)))

(defun open-viewer-workbench-backend-proof (&optional (viewer *viewer*))
  "Mount the isolated BY51K0 proof over a running Luft viewer."
  (or (viewer-workbench-backend-proof-attachment viewer)
      (destructuring-bind (width height) (viewer-logical-extent viewer)
        (let* ((frame
                 (mcluv:make-embedded-workbench-backend-proof
                  (viewer-canvas viewer) (viewer-context viewer)
                  (viewer-device viewer) width height))
               (mirror (mcluv:workbench-backend-proof-mirror frame))
               (compositor
                 (make-instance 'mcluv:direct-gpu-mirror-compositor
                                :mirror mirror))
               (instrument
                 (make-instance 'viewer-workbench-backend-proof
                                :frame frame :compositor compositor)))
          (setf (mcluv:mirror-compositor mirror) compositor
                (viewer-workbench-backend-proof-extent instrument)
                (list width height))
          (mcluv:repaint-gpu-mirror mirror)
          (add-viewer-instrument viewer instrument)))))

(defun close-viewer-workbench-backend-proof (&optional (viewer *viewer*))
  "Unmount and release the isolated BY51K0 proof."
  (alexandria:when-let
      ((instrument (viewer-workbench-backend-proof-attachment viewer)))
    (remove-viewer-instrument viewer instrument)))
