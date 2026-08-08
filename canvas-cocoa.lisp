;;; Cocoa application lifecycle for SDL canvases hosted by a durable Lisp.

(in-package #:luv)

(cffi:defcfun ("objc_getClass" %objc-get-class) :pointer
  (name :string))

(cffi:defcfun ("sel_registerName" %objc-selector) :pointer
  (name :string))

(cffi:defcfun ("objc_msgSend" %objc-send-object) :pointer
  (receiver :pointer)
  (selector :pointer))

(cffi:defcfun ("objc_msgSend" %objc-send-void) :void
  (receiver :pointer)
  (selector :pointer))

(cffi:defcfun ("objc_msgSend" %objc-send-policy) :bool
  (receiver :pointer)
  (selector :pointer)
  (policy :long))

(defconstant +cocoa-activation-policy-regular+ 0)
(defconstant +cocoa-activation-policy-prohibited+ 2)

(defun cocoa-application ()
  (%objc-send-object (%objc-get-class "NSApplication")
                     (%objc-selector "sharedApplication")))

(defun set-cocoa-activation-policy (application policy)
  (unless (%objc-send-policy application
                             (%objc-selector "setActivationPolicy:")
                             policy)
    (error "Cocoa refused application activation policy ~D." policy)))

(defmethod prepare-sdl-canvas-host ((canvas sdl-canvas))
  (call-next-method)
  ;; SDL normally turns a command-line process into a permanent foreground
  ;; Cocoa app.  This Lisp is a durable REPL host, so we manage that policy
  ;; around the actual lifetime of its windows instead.
  (unless (sdl3:set-hint "SDL_MAC_BACKGROUND_APP" "1")
    (error "SDL Cocoa background-app hint failed: ~A" (sdl3:get-error))))

(defmethod activate-sdl-canvas-host :before ((canvas sdl-canvas))
  (declare (ignore canvas))
  (set-cocoa-activation-policy (cocoa-application)
                               +cocoa-activation-policy-regular+))

(defmethod activate-sdl-canvas-host :after ((canvas sdl-canvas))
  (declare (ignore canvas))
  ;; ACTIVATE is cooperative on current macOS.  SDL_RaiseWindow, performed by
  ;; the primary method, separately requests that this particular window be
  ;; made key and frontmost.
  (%objc-send-void (cocoa-application) (%objc-selector "activate")))

(defmethod deactivate-sdl-canvas-host ((canvas sdl-canvas))
  (declare (ignore canvas))
  (let ((application (cocoa-application)))
    (%objc-send-void application (%objc-selector "deactivate"))
    (set-cocoa-activation-policy application
                                 +cocoa-activation-policy-prohibited+)))
