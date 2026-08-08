;;; Small live demos built out of the public canvas protocol.

(in-package #:luv)

(defclass clear-color-demo ()
  ((canvas
    :initarg :canvas
    :reader demo-canvas)
   (context
    :initarg :context
    :reader demo-context)
   (speed
    :initarg :speed
    :reader demo-speed)
   (start-time
    :initform nil
    :accessor demo-start-time))
  (:documentation "A running cadence-clock color-cycle demonstration."))

(defun clear-color-component (phase offset)
  (* 0.5 (+ 1.0 (sin (+ phase offset)))))

(defun render-clear-color-demo-frame (demo timestamp)
  (unless (demo-start-time demo)
    (setf (demo-start-time demo) timestamp))
  (let* ((tau (* 2 pi))
         (phase (* tau (demo-speed demo)
                   (- timestamp (demo-start-time demo)))))
    (render-canvas-color
     (demo-context demo)
     (clear-color-component phase 0)
     (clear-color-component phase (/ tau 3))
     (clear-color-component phase (* 2 (/ tau 3))))))

(defun start-clear-color-demo (&key
                                 (title "luv clear color demo")
                                 (width 800)
                                 (height 600)
                                 (frames-per-second 60)
                                 (speed 0.08))
  "Open and return a smoothly cycling CLEAR-COLOR-DEMO.

SPEED is the number of complete color cycles per second.  Stop the returned
object with STOP-CLEAR-COLOR-DEMO."
  (let ((canvas (make-sdl-canvas :title title :width width :height height)))
    (open-canvas canvas)
    (handler-case
        (let* ((context (make-canvas-context canvas *gpu-provider*))
               (demo (make-instance 'clear-color-demo
                                    :canvas canvas
                                    :context context
                                    :speed speed)))
          (setf (canvas-clock canvas)
                (make-cadence-clock
                 (lambda (native-canvas timestamp)
                   (declare (ignore native-canvas))
                   (render-clear-color-demo-frame demo timestamp))
                 :frames-per-second frames-per-second))
          demo)
      (error (condition)
        (close-canvas canvas)
        (error condition)))))

(defun stop-clear-color-demo (demo)
  "Stop DEMO, close its canvas, and return no values."
  (let ((canvas (demo-canvas demo)))
    (when (eq :open (canvas-state canvas))
      (setf (canvas-clock canvas) (make-demand-clock)))
    (close-canvas canvas))
  (values))
