(in-package #:luvcraft)

(defvar *macos-updates-library* nil)
(defvar *macos-update-quit-requested* nil)

#+darwin
(cffi:defcallback request-macos-update-quit :void ()
  ;; Called on Cocoa's main thread. The game worker owns save and teardown.
  (setf *macos-update-quit-requested* t))

(defun start-macos-updates (canvas)
  #+darwin
  (let ((path (merge-pathnames "../Frameworks/libgame-updates.dylib"
                               (executable-directory))))
    ;; Development images and CLI/smoke invocations never initialize Sparkle.
    (when (probe-file path)
      (setf *macos-update-quit-requested* nil
            *macos-updates-library* (cffi:load-foreign-library path))
      (luv::call-on-sdl-canvas-thread
       canvas
       (lambda ()
         (luv.objective-c:with-objective-c-native-environment
           (cffi:foreign-funcall-pointer
            (cffi:foreign-symbol-pointer "game_updates_start"
                                         :library *macos-updates-library*)
            () :pointer (cffi:callback request-macos-update-quit) :void))))))
  #-darwin (declare (ignore canvas)))

(defun finish-macos-updates ()
  #+darwin
  (when *macos-updates-library*
    (trivial-main-thread:call-in-main-thread
     (lambda ()
       (luv.objective-c:with-objective-c-native-environment
         (cffi:foreign-funcall-pointer
          (cffi:foreign-symbol-pointer "game_updates_finish"
                                       :library *macos-updates-library*)
          () :void)))
     :blocking t)))
