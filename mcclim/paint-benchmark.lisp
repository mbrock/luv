;;; Equal-work Tracy sanity check for the direct McCLIM paint pipelines.

(in-package #:mcluv)

(defun make-paint-benchmark-pattern (&optional (size 32))
  (let ((pixels (make-array (list size size)
                            :element-type '(unsigned-byte 32))))
    (dotimes (y size)
      (dotimes (x size)
        (let ((checker (if (evenp (+ (floor x 8) (floor y 8)))
                           #xff3f7ee8
                           #xffea4775)))
          (setf (aref pixels y x) checker))))
    (make-pattern pixels nil)))

(define-application-frame paint-benchmark-frame ()
  ((mode :initform :solid :accessor paint-benchmark-mode)
   (shape-count :initarg :shape-count :reader paint-benchmark-shape-count)
   (pattern :initform (make-paint-benchmark-pattern)
            :reader paint-benchmark-pattern))
  (:menu-bar nil)
  (:panes
   (canvas :application
           :display-function 'display-paint-benchmark
           :scroll-bars nil))
  (:layouts (default canvas)))

(defun paint-benchmark-ink (frame mode left top right bottom row column)
  (ecase mode
    (:solid
     (if (evenp (+ row column))
         (make-rgb-color 0.18 0.48 0.92)
         (make-rgb-color 0.92 0.32 0.48)))
    (:gradient
     (make-linear-gradient
      left top right bottom
      (make-rgb-color 0.18 0.48 0.92)
      (make-rgb-color 0.92 0.32 0.48)))
    (:image
     (transform-region
      (make-transformation (/ (- right left) 32.0) 0
                           0 (/ (- bottom top) 32.0)
                           left top)
      (paint-benchmark-pattern frame)))))

(defun display-paint-benchmark (frame stream)
  (loop with columns = 16
        with width = 54
        with height = 34
        with radius = 10
        with mode = (paint-benchmark-mode frame)
        for index below (paint-benchmark-shape-count frame)
        for column = (mod index columns)
        for row = (floor index columns)
        for left = (+ 8 (* column 62))
        for top = (+ 8 (* row 44))
        for right = (+ left width)
        for bottom = (+ top height)
        do (draw-analytic-rounded-rectangle*
            stream left top right bottom :radius radius :filled t
            :ink (paint-benchmark-ink
                  frame mode left top right bottom row column))))

(defun prepare-paint-benchmark-sample (frame mirror mode)
  (setf (paint-benchmark-mode frame) mode)
  (let ((*application-frame* frame))
    (luv:with-cpu-trace-zone (:mcluv/redisplay)
      (redisplay-frame-panes frame :force-p t))
    (repaint-gpu-mirror mirror :present-p nil)))

(defun render-paint-benchmark-sample (frame mirror mode)
  (prepare-paint-benchmark-sample frame mirror mode)
  (render-gpu-mirror-frame mirror)
  (luv:tracy-frame-mark "McCLIM paint comparison"))

(defun paint-benchmark-stream-size (mirror)
  (let ((medium (sheet-medium (mirror-sheet mirror))))
    (list :commands (length (gpu-medium-commands medium))
          :solid-vertices (/ (length (gpu-medium-vertices medium)) 6)
          :analytic-vertices
          (/ (length (gpu-medium-analytic-vertices medium)) 12)
          :gradient-vertices
          (/ (length (gpu-medium-gradient-vertices medium)) 21)
          :image-vertices (/ (length (gpu-medium-image-vertices medium)) 12)
          :cached-images (hash-table-count (gpu-mirror-image-paints mirror)))))

(defun call-with-paint-sample-zone (mode shape-count function)
  (ecase mode
    (:solid
     (luv:with-cpu-trace-zone
         (:mcluv/solid-paint-sample :tracy-value shape-count)
       (funcall function)))
    (:gradient
     (luv:with-cpu-trace-zone
         (:mcluv/gradient-paint-sample :tracy-value shape-count)
       (funcall function)))
    (:image
     (luv:with-cpu-trace-zone
         (:mcluv/image-paint-sample :tracy-value shape-count)
       (funcall function)))))

(defun run-paint-tracy-benchmark
    (&key (shape-count 256) (repetitions 30) ready-function)
  "Trace equal solid, analytical-gradient, and cached-image roundrect frames."
  (let ((frame nil))
    (unwind-protect
         (let* ((port (find-port :server-path '(:luv-gpu)))
                (manager
                  (or (first (climi::frame-managers port))
                      (make-instance 'luv-frame-manager :port port))))
           (setf frame
                 (make-application-frame
                  'paint-benchmark-frame
                  :frame-manager manager :enable nil
                  :shape-count shape-count :width 1010 :height 720))
           (let* ((sheet (frame-top-level-sheet frame))
                  (mirror (sheet-direct-mirror sheet)))
             (unless (typep mirror 'luv-gpu-mirror)
               (error "Paint benchmark did not realize a GPU mirror."))
             (let ((*suppress-luv-mirror-visibility* t)
                   (modes '(:solid :gradient :image))
                   sizes)
               (setf (sheet-enabled-p sheet) t)
               ;; Compile every pipeline and populate the sole immutable image
               ;; texture before Tracy attaches.
               (dolist (mode modes)
                 (render-paint-benchmark-sample frame mirror mode)
                 (push (cons mode (paint-benchmark-stream-size mirror)) sizes))
               (unless (= 1 (hash-table-count (gpu-mirror-image-paints mirror)))
                 (error "Expected one warmed image texture, found ~D."
                        (hash-table-count (gpu-mirror-image-paints mirror))))
               (when ready-function (funcall ready-function))
               (loop repeat 300
                     until (luv:tracy-connected-p)
                     do (sleep 0.1)
                     finally
                        (unless (luv:tracy-connected-p)
                          (error "Tracy did not connect within 30 seconds.")))
               (luv:tracy-message "McCLIM paint comparison begins")
               (loop repeat repetitions
                     do (dolist (mode modes)
                          (call-with-paint-sample-zone
                           mode shape-count
                           (lambda ()
                             (render-paint-benchmark-sample
                              frame mirror mode)))))
               (luv:tracy-message "McCLIM paint comparison complete")
               (unless (= 1 (hash-table-count (gpu-mirror-image-paints mirror)))
                 (error "Image texture cache grew during steady state."))
               (setf sizes (nreverse sizes))
               (format t "Paint streams: ~S~%" sizes)
               (format t "Paint cache: 1 source texture after ~D steady image draws.~%"
                       (* shape-count repetitions))
               (force-output)
               sizes)))
      (when frame
        (destroy-frame frame)))))
