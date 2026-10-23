;;;; Disposable-process experiment. Keep the managed image untouched.
;;;; Uses the existing serialized benchmark, swapping only its terrain pipeline.
(require :asdf)
(require :sb-posix)
(assert (uiop:getenv "BENCH_LIBRARY"))
(load "scripts/bench-gpu-fps.lisp")
(load "wiki/measurements/yawl-fragment-a31fc0b/probe.lisp")
(in-package #:gpu-fps)

(defvar *block-samples* nil)
(defvar *collect-block-samples* nil)
(defvar *block-output* nil)
(defvar *frame-output* nil)
(defvar *feedback-times* nil)

(luv.objective-c:define-objective-c-message process-info
    ("processInfo" :object :ownership :borrowed :class "NSProcessInfo"))
(luv.objective-c:define-objective-c-message begin-user-activity
    ("beginActivityWithOptions:reason:" :object :ownership :borrowed)
  (options :uint64) (reason :object))
(luv.objective-c:define-objective-c-message end-user-activity
    ("endActivity:" :void)
  (activity :object))

(defmethod luv:queue-gpu-time-reader :around ((queue luv::metal-gpu-queue))
  (let ((reader (call-next-method)))
    (lambda (submission)
      (let ((seconds (funcall reader submission)))
        (when (and seconds *collect-block-samples*)
          (multiple-value-bind (unix microseconds) (sb-ext:get-time-of-day)
            (push (+ unix (/ microseconds 1d6)) *feedback-times*)))
        seconds))))

(let ((original (symbol-function 'summary)))
  (setf (symbol-function 'summary)
        (lambda (values)
          (when *collect-block-samples*
            (push (reverse values) *block-samples*))
          (funcall original values))))

(when (uiop:getenv "BLOCK_PUMP_EVENTS")
  ;; Diagnostic: production services Cocoa between frames; MEASURE normally
  ;; holds its event thread for a whole batch. Charge this work to simulation.
  (let ((original (symbol-function 'advance-fixture)))
    (setf (symbol-function 'advance-fixture)
          (lambda (kind host old-p)
            (sdl3:pump-events)
            (funcall original kind host old-p)))))

(defun block-program (renderer mode)
  ;; Match MAKE-LUVCRAFT-TERRAIN-DRAWING, including its atmosphere hooks.
  (sb-posix:setenv "FRAGMENT_PROBE" (if (equal mode "control-copy") "control" mode) 1)
  (let ((luft.render.shaders::*terrain-cloud-visibility-function*
          'luft.render.shaders::luvcraft-cloud-visibility)
        (luft.render.shaders::*surface-atmosphere-function*
          'luft.render.shaders::luvcraft-atmospheric-surface))
    (r::make-drawing-program
     (r::renderer-device renderer) :label (format nil "runtime probe ~A" mode)
     :vertex (luft.render.shaders:terrain-vertex-specification)
     :fragment (luft.render.shaders:star-fragment-specification)
     :targets (mapcar (lambda (format) `(:format ,format))
                      (if (r::renderer-temporal-p renderer)
                          '(:rgba16-float :rg16-float) '(:rgba16-float)))
     :sample-count (r::renderer-scene-sample-count renderer)
     :depth-stencil '(:format :depth32-float :depth-write-enabled t
                      :depth-compare :less))))

(defparameter *serialized-measure* (symbol-function 'measure))

(let ((original (symbol-function 'measure)))
  (setf (symbol-function 'measure)
        (lambda (kind host old-p width height)
          (assert (not old-p))
          (let* ((renderer (r:viewer-renderer host))
                 (drawing (r::renderer-terrain renderer))
                 (control (r::terrain-scene-program drawing))
                 (modes (uiop:split-string
                         (or (uiop:getenv "BLOCK_MODES") "control,normal-off,relief-off")
                         :separator ","))
                 (programs nil)
                 (result nil)
                 (block 0))
            (unwind-protect
                 (progn
                   (luv::call-on-sdl-canvas-thread
                    (r:viewer-canvas host)
                    (lambda ()
                      (when (uiop:getenv "BLOCK_USER_QOS")
                        (assert (zerop
                                 (cffi:foreign-funcall "pthread_set_qos_class_self_np"
                                                       :unsigned-int #x19 :int 0 :int))))
                      (cffi:with-foreign-objects ((qos :unsigned-int) (priority :int))
                        (assert (zerop
                                 (cffi:foreign-funcall "pthread_get_qos_class_np"
                                                       :pointer (cffi:foreign-funcall "pthread_self" :pointer)
                                                       :pointer qos :pointer priority :int)))
                        (format t "~&CANVAS QOS ~D priority ~D~%"
                                (cffi:mem-ref qos :unsigned-int) (cffi:mem-ref priority :int)))
                      (dolist (mode modes)
                        (push (cons mode (block-program renderer mode)) programs))))
                   ;; An ABCCBA order balances linear drift. Repeat in the same
                   ;; resident world; pipeline compilation is outside all blocks.
                   (dotimes (round (env-integer "BLOCK_ROUNDS" 3))
                     (dolist (mode (append modes (reverse modes)))
                       (incf block)
                       (luv::call-on-sdl-canvas-thread
                        (r:viewer-canvas host)
                        (lambda ()
                          ;; Layouts have identity: select the whole program
                          ;; and rebuild its bindings during untimed warmup.
                          (r::clear-renderer-frame-bind-groups renderer)
                          (setf (r::terrain-scene-program drawing)
                                (cdr (assoc mode programs :test #'equal)))))
                       (setf *block-samples* nil *feedback-times* nil
                             *collect-block-samples* t)
                       (unwind-protect
                            (setf result (funcall original kind host old-p width height))
                         (setf *collect-block-samples* nil))
                       ;; MEASURE summarizes encode, completion, CPU frame,
                       ;; simulation, completion again, then GPU, in that order.
                       (let* ((samples (reverse *block-samples*))
                              (cpu (first samples)) (wall (second samples))
                              (gpu (sixth samples)))
                         (assert (= (length cpu) (length wall) (length gpu)
                                    (length *feedback-times*)
                                    (env-integer "BENCH_FRAMES" 300)))
                         (assert (equalp (summary gpu) (getf result :gpu-ms)))
                         (loop for c in cpu for w in wall for g in gpu
                               for timestamp in (reverse *feedback-times*) for frame from 0
                               do (format *frame-output* "~D~C~A~C~D~C~,9F~C~,9F~C~,9F~C~,6F~%"
                                          block #\Tab mode #\Tab frame #\Tab g #\Tab w #\Tab c
                                          #\Tab timestamp))
                         (finish-output *frame-output*))
                       (let ((record (append (list :block block :round (1+ round) :variant mode
                                                   :pump-events (not (null (uiop:getenv "BLOCK_PUMP_EVENTS")))
                                                   :user-activity (not (null (uiop:getenv "BLOCK_USER_ACTIVITY"))))
                                             result)))
                         (print record *block-output*) (terpri *block-output*)
                         (finish-output *block-output*)
                         (format t "~&BLOCK ~D ~A GPU ~,3F completion ~,3F~%"
                                 block mode (getf (getf result :gpu-ms) :mean)
                                 (getf (getf result :completion-ms) :mean))
                         (finish-output)))))
              (luv::call-on-sdl-canvas-thread
               (r:viewer-canvas host)
               (lambda ()
                 (r::clear-renderer-frame-bind-groups renderer)
                 (setf (r::terrain-scene-program drawing) control)
                 (dolist (entry programs) (luv:destroy (cdr entry))))))
            result))))

(let ((original (symbol-function 'run-case)))
  (setf (symbol-function 'run-case)
        (lambda (kind backend width height)
          (unless (eq backend :old)
            (funcall original kind backend width height)))))

(unless (uiop:getenv "BLOCK_LIBRARY")
  (let ((prefix (or (uiop:getenv "BLOCK_OUTPUT") "build/runtime-blocks")))
    (ensure-directories-exist prefix)
    (with-open-file (blocks (concatenate 'string prefix ".sexp")
                            :direction :output :if-exists :supersede)
      (with-open-file (frames (concatenate 'string prefix ".tsv")
                              :direction :output :if-exists :supersede)
        (format frames "block~Cvariant~Cframe~Cgpu_ms~Ccompletion_ms~Ccpu_encode_ms~Cfeedback_unix~%"
                #\Tab #\Tab #\Tab #\Tab #\Tab #\Tab)
        ;; These specials must be visible on the canvas thread as well.
        (setf *block-output* blocks *frame-output* frames)
        (if (uiop:getenv "BLOCK_USER_ACTIVITY")
            (luv.objective-c:with-autorelease-pool ()
              (let ((info (process-info (luv.objective-c:find-objective-c-class "NSProcessInfo"))))
                (luv.objective-c:with-owned-objective-c-object
                    (activity
                     (luv.objective-c:retain-objective-c-object
                      (begin-user-activity
                       info #xEFFFFF
                       (luv.objective-c:lisp-string-to-objective-c "Luft runtime comparison"))))
                  ;; User initiated, allowing idle system sleep; not latency
                  ;; critical, no power-setting changes, and scoped to this run.
                  (unwind-protect (main) (end-user-activity info activity)))))
            (main)))))
  (sb-ext:exit :code 0 :abort t))
