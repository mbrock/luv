;;;; Adjacent-frame comparisons in one fixed target and sustained queue stream.
(require :asdf)
(assert (uiop:getenv "BLOCK_LIBRARY"))
(load "wiki/measurements/yawl-runtime-f87a44f/blocks.lisp")
(in-package #:gpu-fps)

(defvar *interleaved-programs* nil)
(defvar *interleaved-order* nil)
(defvar *interleaved-drawing* nil)
(defvar *interleaved-index* 0)
(defvar *interleaved-mode* nil)
(defvar *interleaved-labels* nil)

(let ((original (symbol-function 'advance-fixture)))
  (setf (symbol-function 'advance-fixture)
        (lambda (kind host old-p)
          (when *interleaved-programs*
            (setf *interleaved-mode*
                  (nth (mod *interleaved-index* (length *interleaved-order*)) *interleaved-order*)
                  (r::terrain-scene-program *interleaved-drawing*)
                  (cdr (assoc *interleaved-mode* *interleaved-programs* :test #'equal)))
            (incf *interleaved-index*)
            (when *collect-block-samples* (push *interleaved-mode* *interleaved-labels*)))
          (funcall original kind host old-p))))

(let ((original (symbol-function 'r::renderer-frame-component-binding)))
  (setf (symbol-function 'r::renderer-frame-component-binding)
        (lambda (frame key make-binding)
          ;; Each variant retains bindings made against its own layout.
          ;; Serial completion still protects mutable per-frame uploads.
          (funcall original frame
                   (if *interleaved-programs* (list *interleaved-mode* key) key)
                   make-binding))))

(defun measure-interleaved (kind host old-p width height)
  (assert (not old-p))
  (assert (eq *benchmark-mode* :native-1x))
  (let* ((renderer (r:viewer-renderer host))
         (drawing (r::renderer-terrain renderer))
         (control (r::terrain-scene-program drawing))
         (modes (uiop:split-string
                 (or (uiop:getenv "BLOCK_MODES") "control,normal-off,relief-off") :separator ","))
         (programs nil)
         (result nil)
         (prefix (uiop:getenv "BLOCK_OUTPUT")))
    (assert (zerop (mod (env-integer "BENCH_WARMUP" 60) (* 2 (length modes)))))
    (assert (zerop (mod (env-integer "BENCH_FRAMES" 300) (* 2 (length modes)))))
    (unwind-protect
         (progn
           (luv::call-on-sdl-canvas-thread
            (r:viewer-canvas host)
            (lambda ()
              (dolist (mode modes) (push (cons mode (block-program renderer mode)) programs))))
           (setf *interleaved-programs* programs *interleaved-drawing* drawing
                 *interleaved-order* (append modes (reverse modes))
                 *interleaved-index* 0 *interleaved-labels* nil
                 *block-samples* nil *feedback-times* nil *collect-block-samples* t)
           (unwind-protect
                (setf result (funcall *serialized-measure* kind host old-p width height))
             (setf *collect-block-samples* nil))
           (let* ((samples (reverse *block-samples*))
                  (cpu (first samples)) (wall (second samples)) (gpu (sixth samples))
                  (labels (nthcdr (env-integer "BENCH_WARMUP" 60)
                                  (reverse *interleaved-labels*))))
             (assert (= (length cpu) (length wall) (length gpu) (length labels)
                        (length *feedback-times*) (env-integer "BENCH_FRAMES" 300)))
             (assert (equalp (summary gpu) (getf result :gpu-ms)))
             (with-open-file (stream (concatenate 'string prefix ".tsv")
                                     :direction :output :if-exists :supersede)
               (format stream "frame~Cvariant~Cgpu_ms~Ccompletion_ms~Ccpu_encode_ms~Cfeedback_unix~%"
                       #\Tab #\Tab #\Tab #\Tab #\Tab)
               (loop for c in cpu for w in wall for g in gpu for mode in labels
                     for timestamp in (reverse *feedback-times*) for frame from 0
                     do (format stream "~D~C~A~C~,9F~C~,9F~C~,9F~C~,6F~%"
                                frame #\Tab mode #\Tab g #\Tab w #\Tab c #\Tab timestamp)))
             (dolist (mode modes)
               (format t "~&ADJACENT ~A GPU ~S completion ~S~%" mode
                       (summary (loop for g in gpu for label in labels when (equal label mode) collect g))
                       (summary (loop for w in wall for label in labels when (equal label mode) collect w)))))
           ;; Optional inspection captures happen only after all timed frames.
           (when (uiop:getenv "PROBE_CAPTURE_DIR")
             (dolist (mode modes)
               (luv::call-on-sdl-canvas-thread
                (r:viewer-canvas host)
                (lambda ()
                  (setf *interleaved-mode* mode
                        (r::terrain-scene-program drawing) (cdr (assoc mode programs :test #'equal)))))
               (r:capture-viewer-frame
                (merge-pathnames (format nil "~A.png" mode)
                                 (uiop:ensure-directory-pathname (uiop:getenv "PROBE_CAPTURE_DIR")))
                host))))
      (setf *interleaved-programs* nil *interleaved-drawing* nil)
      (luv::call-on-sdl-canvas-thread
       (r:viewer-canvas host)
       (lambda ()
         (r::clear-renderer-frame-bind-groups renderer)
         (setf (r::terrain-scene-program drawing) control)
         (dolist (entry programs) (luv:destroy (cdr entry))))))
    result))

(setf (symbol-function 'measure) #'measure-interleaved)
(main)
(sb-ext:exit :code 0 :abort t)
