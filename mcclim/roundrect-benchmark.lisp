;;; Deterministic native-vs-decomposed rounded-rectangle performance proof.

(in-package #:mcluv)

(define-application-frame roundrect-benchmark-frame ()
  ((mode :initform :analytic :accessor roundrect-benchmark-mode)
   (shape-count :initarg :shape-count :reader roundrect-benchmark-shape-count))
  (:menu-bar nil)
  (:panes
   (canvas :application
           :display-function 'display-roundrect-benchmark
           :scroll-bars nil))
  (:layouts (default canvas)))

(defun display-roundrect-benchmark (frame stream)
  (loop with columns = 16
        with width = 54
        with height = 34
        with radius = 10
        for index below (roundrect-benchmark-shape-count frame)
        for column = (mod index columns)
        for row = (floor index columns)
        for left = (+ 8 (* column 62))
        for top = (+ 8 (* row 44))
        for right = (+ left width)
        for bottom = (+ top height)
        for ink = (if (evenp (+ row column))
                      (make-rgb-color 0.18 0.48 0.92)
                      (make-rgb-color 0.92 0.32 0.48))
        do (ecase (roundrect-benchmark-mode frame)
             (:analytic
              (draw-analytic-rounded-rectangle*
               stream left top right bottom
               :radius radius :filled t :ink ink))
             (:decomposed
              (draw-rounded-rectangle*
               stream left top right bottom
               :radius radius :filled t :ink ink)))))

(defun prepare-roundrect-benchmark-sample (frame mirror mode)
  (setf (roundrect-benchmark-mode frame) mode)
  (let ((*application-frame* frame))
    (luv:with-cpu-trace-zone (:mcluv/redisplay)
      (redisplay-frame-panes frame :force-p t))
    (repaint-gpu-mirror mirror :present-p nil)))

(defun render-roundrect-benchmark-sample (frame mirror mode)
  (prepare-roundrect-benchmark-sample frame mirror mode)
  (render-gpu-mirror-frame mirror)
  (luv:tracy-frame-mark "McCLIM roundrect A/B"))

(defun roundrect-benchmark-stream-size (mirror)
  (let ((medium (sheet-medium (mirror-sheet mirror))))
    (list :commands (length (gpu-medium-commands medium))
          :solid-vertices (/ (length (gpu-medium-vertices medium)) 6)
          :analytic-vertices
          (/ (length (gpu-medium-analytic-vertices medium)) 12))))

(defun run-roundrect-tracy-benchmark
    (&key (shape-count 256) (repetitions 30) ready-function)
  "Trace equal native and McCLIM-decomposed roundrect frames on hidden Metal."
  (let ((frame nil))
    (unwind-protect
         (let* ((port (find-port :server-path '(:luv-gpu)))
                (manager
                  (or (first (climi::frame-managers port))
                      (make-instance 'luv-frame-manager :port port))))
           (setf frame
                 (make-application-frame
                  'roundrect-benchmark-frame
                  :frame-manager manager :enable nil
                  :shape-count shape-count :width 1010 :height 720))
           (let* ((sheet (frame-top-level-sheet frame))
                  (mirror (sheet-direct-mirror sheet)))
             (unless (typep mirror 'luv-gpu-mirror)
               (error "Roundrect benchmark did not realize a GPU mirror."))
             (let ((*suppress-luv-mirror-visibility* t))
               (setf (sheet-enabled-p sheet) t)
               ;; Initialize both pipelines and grow retained buffers before
               ;; the capture. These frames are intentionally not measured.
               (render-roundrect-benchmark-sample frame mirror :analytic)
               (let ((analytic-size (roundrect-benchmark-stream-size mirror)))
                 (render-roundrect-benchmark-sample frame mirror :decomposed)
                 (let ((decomposed-size
                         (roundrect-benchmark-stream-size mirror)))
                   (when ready-function (funcall ready-function))
                   (loop repeat 300
                         until (luv:tracy-connected-p)
                         do (sleep 0.1)
                         finally
                            (unless (luv:tracy-connected-p)
                              (error "Tracy did not connect within 30 seconds.")))
                   (luv:tracy-message "McCLIM roundrect A/B begins")
                   (loop repeat repetitions
                         do (luv:with-cpu-trace-zone
                                (:mcluv/analytic-sample
                                 :tracy-value shape-count)
                              (render-roundrect-benchmark-sample
                               frame mirror :analytic))
                            (luv:with-cpu-trace-zone
                                (:mcluv/decomposed-sample
                                 :tracy-value shape-count)
                              (render-roundrect-benchmark-sample
                               frame mirror :decomposed)))
                   (luv:tracy-message "McCLIM roundrect A/B complete")
                   (format t "Analytic stream: ~S~%Decomposed stream: ~S~%"
                           analytic-size decomposed-size)
                   (force-output)
                   (list :analytic analytic-size
                         :decomposed decomposed-size)))))))
      (when frame
        (destroy-frame frame))))
