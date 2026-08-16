;;; Reproducible screenshots of upstream McCLIM examples on the direct GPU port.

(in-package #:mcluv)

(define-application-frame analytic-shape-gallery ()
  ()
  (:menu-bar nil)
  (:panes
   (canvas :application
           :display-function 'display-analytic-shape-gallery
           :scroll-bars nil))
  (:layouts (default canvas)))

(defun display-analytic-shape-gallery (frame stream)
  (declare (ignore frame))
  ;; This deliberately draws through the ordinary recording stream: the
  ;; backend extension must survive McCLIM recording and repaint as one
  ;; semantic command, not rely on reaching into the presentation medium.
  (let ((medium stream))
    (draw-rectangle* medium 0 0 720 420
                     :filled t :ink (make-rgb-color 0.075 0.085 0.12))
    (draw-analytic-rounded-rectangle*
     medium 58 58 430 206 :radius 36 :filled t
     :ink (make-linear-gradient
           58 58 430 206
           (make-rgb-color 1.0 0.30 0.48)
           (make-rgb-color 0.42 0.34 1.0)))
    (draw-analytic-rounded-rectangle*
     medium 84 82 404 182 :radius 24 :filled t
     :ink (make-rgb-color 0.12 0.14 0.20))
    (draw-text* medium "roundrect in roundrect" 116 143
                :ink +white+ :text-size 22)
    (draw-ellipse* medium 570 128 92 0 0 54
                   :filled t
                   :ink (make-radial-gradient
                         570 128 92
                         (make-rgb-color 0.72 1.0 0.84)
                         (make-rgb-color 0.04 0.54 0.31)))
    (draw-ellipse* medium 190 310 105 35 -18 54
                   :filled t :ink (make-rgb-color 0.43 0.58 1.0))
    (draw-circle* medium 450 310 58
                  :filled t :ink (make-rgb-color 1.0 0.63 0.18))
    (draw-analytic-rounded-rectangle*
     medium 548 274 680 346 :radius 36 :filled t
     :ink (make-rgb-color 0.73 0.43 0.96))))

(defun gallery-image-pixel (red green blue alpha)
  (flet ((channel-byte (value)
           (min 255 (max 0 (round (* 255 value))))))
    (logior (ash (channel-byte alpha) 24)
            (ash (channel-byte red) 16)
            (ash (channel-byte green) 8)
            (channel-byte blue))))

(defun make-gallery-image-pattern (&optional (width 192) (height 128))
  (let ((pixels
          (make-array (list height width)
                      :element-type '(unsigned-byte 32))))
    (dotimes (y height)
      (dotimes (x width)
        (let* ((u (/ x (max 1.0 (1- width))))
               (v (/ y (max 1.0 (1- height))))
               (checker (if (evenp (+ (floor x 24) (floor y 24)))
                            0.16 0.0)))
          (setf (aref pixels y x)
                (gallery-image-pixel
                 (+ 0.16 (* 0.78 u) checker)
                 (+ 0.18 (* 0.70 (- 1 u)))
                 (+ 0.36 (* 0.58 v))
                 1.0)))))
    (make-pattern pixels nil)))

(defun make-gallery-opacity-mask (&optional (width 192) (height 128))
  (let ((opacities
          (make-array (list height width)
                      :element-type 'single-float)))
    (dotimes (y height)
      (dotimes (x width)
        (let* ((nx (/ (- (* 2.0 x) width) width))
               (ny (/ (- (* 2.0 y) height) height))
               (distance (sqrt (+ (* nx nx) (* ny ny)))))
          (setf (aref opacities y x)
                (coerce (max 0.0 (min 1.0 (* 1.8 (- 1.0 distance))))
                        'single-float)))))
    (make-stencil opacities)))

(defparameter *gallery-image-pattern* (make-gallery-image-pattern))
(defparameter *gallery-masked-image-pattern*
  (compose-in *gallery-image-pattern* (make-gallery-opacity-mask)))

(define-application-frame paint-gallery ()
  ()
  (:menu-bar nil)
  (:panes
   (canvas :application
           :display-function 'display-paint-gallery
           :scroll-bars nil))
  (:layouts (default canvas)))

(defun display-paint-gallery (frame stream)
  (declare (ignore frame))
  (draw-rectangle* stream 0 0 800 480 :filled t
                   :ink (make-rgb-color 0.055 0.065 0.09))
  ;; Ordinary DRAW-PATTERN* is a single textured quad. This one also crosses
  ;; a native rectangular scissor boundary at x=178.
  (with-drawing-options
      (stream :clipping-region (make-rectangle* 40 44 178 172))
    (draw-pattern* stream *gallery-image-pattern* 40 44))
  ;; The same immutable source texture, affinely stretched and clipped by one
  ;; analytical roundrect coverage evaluation.
  (let ((paint
          (transform-region
           (make-transformation (/ 440.0 192) 0 0 (/ 180.0 128) 300 36)
           *gallery-image-pattern*)))
    (draw-analytic-rounded-rectangle*
     stream 300 36 740 216 :radius 42 :filled t :ink paint))
  ;; McCLIM's COMPOSE-IN stencil becomes texture alpha; the ellipse adds its
  ;; own analytical edge without an offscreen mask surface.
  (let ((paint
          (transform-region
           (make-transformation (/ 300.0 192) 0 0 (/ 190.0 128) 52 266)
           *gallery-masked-image-pattern*)))
    (draw-ellipse* stream 202 361 150 0 0 95 :filled t :ink paint))
  ;; A genuinely affine paint mapping, deliberately skewed inside the clip.
  (let ((paint
          (transform-region
           (make-transformation 1.75 0.45 -0.18 1.35 382 290)
           *gallery-image-pattern*)))
    (draw-analytic-rounded-rectangle*
     stream 382 260 752 446 :radius 30 :filled t :ink paint))
  (draw-text* stream "rect clip" 40 208 :ink +white+ :text-size 18)
  (draw-text* stream "rounded + affine" 300 244
              :ink +white+ :text-size 18)
  (draw-text* stream "opacity mask" 104 464
              :ink +white+ :text-size 18))

(defparameter *mcclim-gallery-scenes*
  `((:relief "Height-bearing McCLIM gadgets" widget-lab 360 180)
    (:analytic "Analytic GUI primitives" analytic-shape-gallery 720 420)
    (:paints "Image paints, masks, and affine placement"
     paint-gallery 800 480)
    (:calculator "Calculator" clim-demo.calculator:calculator-app 420 520)
    (:gadgets "Gadgets" clim-demo::gadget-test 980 760)
    (:tables "Tables and borders"
     clim-demo.tables-with-borders:tables-with-borders 1280 720)
    (:text "Text transformations"
     clim-demo.draw-text-test:draw-text-test 1280 720)
    (:borders "Border styles" clim-demo::bordered-output 800 700)
    (:wrapping "Baseline and wrapping"
     clim-demo.seos-baseline:seos-baseline 700 700))
  "Named upstream McCLIM application frames captured by the gallery script.")

(defun gallery-frame-media (frame)
  (let ((sheet (frame-top-level-sheet frame)))
    (remove-duplicates
     (remove nil
             (mapcan
              (lambda (child)
                (list (sheet-medium child)
                      (gpu-sheet-presentation-medium child)))
              (gpu-sheet-paint-order sheet)))
     :test #'eq)))

(defun clear-gallery-frame-statistics (frame)
  (dolist (medium (gallery-frame-media frame))
    (when (typep medium 'luv-gpu-medium)
      (clear-gpu-medium-fallback-statistics medium))))

(defun gallery-frame-fallback-report (frame)
  (let ((combined (make-hash-table :test #'eq)))
    (dolist (medium (gallery-frame-media frame))
      (when (typep medium 'luv-gpu-medium)
        (dolist (entry (gpu-medium-fallback-report medium))
          (let* ((primitive (getf entry :primitive))
                 (target (copy-list (gethash primitive combined))))
            (loop for (field value) on (cddr entry) by #'cddr
                  do (incf (getf target field 0) value))
            (setf (gethash primitive combined) target)))))
    (let (entries)
      (maphash (lambda (primitive statistics)
                 (push (list* :primitive primitive statistics) entries))
               combined)
      (sort entries #'string< :key (lambda (entry)
                                    (symbol-name (getf entry :primitive)))))))

(defun capture-gallery-frame (manager frame-class pathname width height)
  (let ((frame nil))
    (unwind-protect
         (progn
           ;; Adoption realizes an invisible native mirror. Deliberately leave
           ;; the frame disabled: no SHOW-CANVAS, activation, or focus change.
           (setf frame
                 (make-application-frame
                  frame-class :frame-manager manager :enable nil
                  :width width :height height))
           (let* ((sheet (frame-top-level-sheet frame))
                  (mirror (sheet-direct-mirror sheet)))
             (unless (typep mirror 'luv-gpu-mirror)
               (error "~S did not realize a direct GPU mirror." frame-class))
             (let ((*suppress-luv-mirror-visibility* t))
               ;; A disabled sheet intentionally does not repaint. Enable its
               ;; drawing lifecycle while the GPU mirror's visibility hook is
               ;; suppressed, then capture the still-hidden drawable.
               (setf (sheet-enabled-p sheet) t)
               (clear-gallery-frame-statistics frame)
               ;; RUN-FRAME-TOP-LEVEL normally supplies this binding. The
               ;; hidden harness deliberately bypasses its event loop, but
               ;; redisplay functions may still create embedded gadgets.
               (let ((*application-frame* frame))
                 (redisplay-frame-panes frame :force-p t)
                 (repaint-gpu-mirror mirror :present-p nil)
                 (capture-gpu-mirror-screenshot mirror pathname)
                 (gallery-frame-fallback-report frame)))))
      (when frame
        (destroy-frame frame)))))

(defun html-escaped-string (value)
  (with-output-to-string (stream)
    (loop for character across (princ-to-string value)
          do (write-string
              (case character
                (#\& "&amp;") (#\< "&lt;") (#\> "&gt;")
                (#\" "&quot;") (otherwise (string character)))
              stream))))

(defun write-gallery-index (pathname results)
  (with-open-file (stream pathname :direction :output
                          :if-exists :supersede :if-does-not-exist :create)
    (format stream "<!doctype html><meta charset=\"utf-8\"><title>McCLIM on luv</title>~%")
    (format stream "<style>body{font:16px system-ui;margin:2rem;background:#202124;color:#eee}main{display:grid;grid-template-columns:repeat(auto-fit,minmax(360px,1fr));gap:1.5rem}article{background:#303134;padding:1rem;border-radius:12px}img{width:100%;height:auto;background:#eee}pre{white-space:pre-wrap;font-size:12px}</style>~%")
    (format stream "<h1>McCLIM on luv: direct Metal gallery</h1><main>~%")
    (dolist (result results)
      (destructuring-bind (&key name title file report error) result
        (declare (ignore name))
        (format stream "<article><h2>~A</h2>" (html-escaped-string title))
        (if error
            (format stream "<p>Capture failed: ~A</p>" (html-escaped-string error))
            (format stream "<img src=\"~A\" alt=\"~A\"><pre>~A</pre>"
                    (html-escaped-string file)
                    (html-escaped-string title)
                    (html-escaped-string report)))
        (format stream "</article>~%")))
    (format stream "</main>~%")))

(defun capture-mcclim-gallery
    (directory &key (scenes *mcclim-gallery-scenes*))
  "Capture SCENES invisibly through the direct GPU backend into DIRECTORY.

INDEX.HTML displays the images and each scene's measured BASIC-MEDIUM fallback
activity. FALLBACKS.SEXP is the same evidence in machine-readable form."
  (let* ((directory (uiop:ensure-directory-pathname directory))
         (port (find-port :server-path '(:luv-gpu)))
         (manager (or (first (climi::frame-managers port))
                      (make-instance 'luv-frame-manager :port port)))
         results)
    (ensure-directories-exist directory)
    (dolist (scene scenes)
      (destructuring-bind (name title frame-class width height) scene
        (let* ((filename (format nil "~(~A~).png" name))
               (pathname (merge-pathnames filename directory)))
          (format t "Capturing ~A without showing its canvas...~%" title)
          (handler-case
              (let ((report
                      (capture-gallery-frame
                       manager frame-class pathname width height)))
                (push (list :name name :title title :file filename
                            :report report)
                      results))
            (error (condition)
              (push (list :name name :title title :file filename
                          :error (princ-to-string condition))
                    results))))))
    (setf results (nreverse results))
    (with-open-file
        (stream (merge-pathnames #P"fallbacks.sexp" directory)
                :direction :output :if-exists :supersede
                :if-does-not-exist :create)
      (let ((*print-pretty* t))
        (write results :stream stream)))
    (write-gallery-index (merge-pathnames #P"index.html" directory) results)
    (format t "McCLIM gallery written under ~A~%" directory)
    results))
