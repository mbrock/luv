;;;; One native catch frame around every declared Objective-C message.

(in-package #:luv.objective-c)

(defparameter *objective-c-exception-bridge-source-directory*
  #.(uiop:pathname-directory-pathname
     (or *compile-file-truename* *load-truename*))
  "The source checkout embedded when this Lisp file is compiled or loaded.")

(defun objective-c-exception-bridge-source ()
  (merge-pathnames
   "exception-bridge.m" *objective-c-exception-bridge-source-directory*))

(defun objective-c-exception-bridge-library ()
  (let ((stamp (file-write-date (objective-c-exception-bridge-source))))
    (merge-pathnames
     (format nil "../build/objective-c-exception-bridge-~D.dylib" stamp)
     *objective-c-exception-bridge-source-directory*)))

(defun ensure-objective-c-exception-bridge ()
  "Build and load the native Objective-C catch frame when its source changed."
  (let ((source (objective-c-exception-bridge-source))
        (library (objective-c-exception-bridge-library)))
    (when (or (not (probe-file library))
              (< (file-write-date library) (file-write-date source)))
      (ensure-directories-exist library)
      (uiop:run-program
       (list "clang" "-fno-objc-arc" "-fexceptions" "-fobjc-exceptions"
             "-Wall" "-Wextra" "-Werror" "-dynamiclib"
             "-framework" "Foundation" "-o"
             (uiop:native-namestring library)
             (uiop:native-namestring source))
       :output *standard-output*
       :error-output *error-output*))
    (cffi:load-foreign-library library)))

(defparameter *objective-c-exception-bridge*
  (ensure-objective-c-exception-bridge)
  "Current CFFI handle for the hot-reloadable native exception bridge.")

(cffi:defcstruct objective-c-native-failure
  (name :pointer)
  (reason :pointer)
  (call-stack :pointer))

(defun objective-c-bridge-function (name)
  (cffi:foreign-symbol-pointer
   name :library *objective-c-exception-bridge*))

(defun %invoke-objective-c-message
    (receiver selector return-value return-size argument-count arguments
     argument-sizes failure)
  (cffi:foreign-funcall-pointer
   (objective-c-bridge-function "luv_objc_invoke") ()
   :pointer receiver
   :pointer selector
   :pointer return-value
   :size return-size
   :size argument-count
   :pointer arguments
   :pointer argument-sizes
   :pointer failure
   :int))

(defun %free-objective-c-native-failure (failure)
  (cffi:foreign-funcall-pointer
   (objective-c-bridge-function "luv_objc_failure_free") ()
   :pointer failure
   :void))

(defun nullable-native-string (pointer)
  (unless (cffi:null-pointer-p pointer)
    (cffi:foreign-string-to-lisp pointer)))

(defun objective-c-native-failure-values (failure)
  (let ((type '(:struct objective-c-native-failure)))
    (values
     (nullable-native-string
      (cffi:foreign-slot-value failure type 'name))
     (nullable-native-string
      (cffi:foreign-slot-value failure type 'reason))
     (nullable-native-string
      (cffi:foreign-slot-value failure type 'call-stack)))))

(defun call-with-objective-c-exception-boundary
    (message receiver result result-size arguments argument-sizes argument-count)
  "Invoke MESSAGE natively, translating any Objective-C exception to Lisp."
  (cffi:with-foreign-object (failure '(:struct objective-c-native-failure))
    (let ((status
            (%invoke-objective-c-message
             (objective-c-pointer receiver)
             (objective-c-message-selector-pointer (class-of message))
             result result-size argument-count arguments argument-sizes
             failure)))
      (unless (zerop status)
        (multiple-value-bind (name reason call-stack)
            (objective-c-native-failure-values failure)
          (%free-objective-c-native-failure failure)
          (error (if (= status 1)
                     'objective-c-exception
                     'objective-c-bridge-error)
                 :message message
                 :receiver receiver
                 :selector
                 (objective-c-message-selector (class-of message))
                 :name name
                 :reason reason
                 :call-stack call-stack))))))
