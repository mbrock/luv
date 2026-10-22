;;;; GPU start and end times for Metal 4 submissions.

(in-package #:luv.metal)

(defparameter *metal-gpu-timing-source-directory*
  #.(uiop:pathname-directory-pathname
     (or *compile-file-truename* *load-truename*))
  "The source checkout embedded when this Lisp file is compiled or loaded.")

(defun metal-gpu-timing-source ()
  (merge-pathnames "gpu-timing.m" *metal-gpu-timing-source-directory*))

(defun ensure-metal-gpu-timing-library ()
  "Build and load the native feedback helper when its source changed."
  (let* ((source (metal-gpu-timing-source))
         (library (merge-pathnames
                   (format nil "../../build/metal-gpu-timing-~D.dylib"
                           (file-write-date source))
                   *metal-gpu-timing-source-directory*)))
    (unless (probe-file library)
      (ensure-directories-exist library)
      (uiop:run-program
       (list "clang" "-fno-objc-arc" "-fblocks" "-Wall" "-Wextra" "-Werror"
             "-dynamiclib" "-framework" "Foundation" "-o"
             (uiop:native-namestring library)
             (uiop:native-namestring source))
       :output *standard-output*
       :error-output *error-output*))
    (cffi:load-foreign-library library)))

(defparameter *metal-gpu-timing-library* (ensure-metal-gpu-timing-library))

(defconstant +metal-gpu-timing-capacity+ 256
  "Submissions remembered; far more than are ever in flight.")

(defun make-metal-gpu-timing-ring ()
  "Allocate a zeroed foreign ring of submission times; never freed by GC."
  (let* ((bytes (* 24 +metal-gpu-timing-capacity+))
         (ring (cffi:foreign-alloc :uint8 :count bytes)))
    (dotimes (index bytes) (setf (cffi:mem-aref ring :uint8 index) 0))
    ring))

(defun metal-timed-commit-options (ring value)
  "Return +1 commit options recording submission VALUE in RING, or a null pointer."
  (cffi:foreign-funcall "luv_metal_timed_commit_options"
                        :pointer ring :uint32 +metal-gpu-timing-capacity+
                        :uint64 value :pointer))

(defun metal-gpu-time (ring value)
  "Submission VALUE's GPU seconds once its feedback has arrived, else NIL."
  (cffi:with-foreign-object (seconds :double)
    (when (= 1 (cffi:foreign-funcall "luv_metal_gpu_time"
                                     :pointer ring
                                     :uint32 +metal-gpu-timing-capacity+
                                     :uint64 value :pointer seconds :int))
      (cffi:mem-ref seconds :double))))

(defun release-metal-commit-options (options)
  (unless (cffi:null-pointer-p options)
    (cffi:foreign-funcall "luv_metal_release" :pointer options :void)))
