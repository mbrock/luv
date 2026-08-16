;;; Reproducible screenshots of upstream McCLIM examples on the direct GPU port.

(in-package #:mcluv)

(defparameter *mcclim-gallery-scenes*
  `((:calculator "Calculator" clim-demo.calculator:calculator-app 420 520)
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
