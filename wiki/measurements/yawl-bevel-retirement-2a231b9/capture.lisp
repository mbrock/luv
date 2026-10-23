;;;; Fresh hardware-counter captures, separate from unprofiled runtime timing.
(require :asdf)
(assert (uiop:getenv "RETIREMENT_LIBRARY"))
(assert (uiop:getenv "RETIREMENT_TRACE_DIR"))
(assert (uiop:getenv "RETIREMENT_CAPTURE_DIR"))
(assert (null (uiop:getenv "RETIREMENT_MOTION")))
(load "wiki/measurements/yawl-bevel-retirement-2a231b9/run.lisp")
(in-package #:gpu-fps)

(cffi:load-foreign-library (namestring (truename "build/gpu-capture-probe.dylib")))
(cffi:defcfun ("startCapture" start-retirement-capture) :int (device :pointer) (path :string))
(cffi:defcfun ("stopCapture" stop-retirement-capture) :void)
(cffi:defcfun ("labelEncoder" label-retirement-encoder) :void (encoder :pointer) (label :string))

(defmethod luv:begin-render-pass :around
    ((encoder luv::metal-gpu-command-encoder) (descriptor luv::render-pass-descriptor))
  (let* ((start (now)) (pass (call-next-method)))
    ;; This replaces the benchmark's identically-specialized around method;
    ;; retain its CPU-pass record, consumed by its END-PASS instrumentation.
    (setf (gethash pass *pass-starts*)
          (list nil start (or (luv::gpu-descriptor-label descriptor) "unlabelled")))
    (label-retirement-encoder
     (luv.objective-c:objective-c-pointer (luv::metal-render-pass-native-encoder pass))
     (or (luv::gpu-descriptor-label descriptor) "unlabelled"))
    pass))

(let ((original (symbol-function 'retirement-capture)))
  (setf (symbol-function 'retirement-capture)
        (lambda (host pathname width height)
          (let* ((device (r::renderer-device (r:viewer-renderer host)))
                 (path (merge-pathnames (format nil "~A.gputrace" (pathname-name pathname))
                                        (uiop:ensure-directory-pathname
                                         (uiop:getenv "RETIREMENT_TRACE_DIR")))))
            (assert (not (probe-file path)))
            (assert (plusp (start-retirement-capture
                            (luv.objective-c:objective-c-pointer (luv::metal-native-object device))
                            (namestring path))))
            (unwind-protect (funcall original host pathname width height)
              (stop-retirement-capture))))))

(main)
(sb-ext:exit :code 0 :abort t)
