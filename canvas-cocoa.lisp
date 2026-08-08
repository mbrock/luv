;;; Cocoa application lifecycle for SDL canvases hosted by a durable Lisp.

(in-package #:luv)

(cffi:defcfun ("objc_getClass" %objc-get-class) :pointer
  (name :string))

(cffi:defcfun ("sel_registerName" %objc-selector) :pointer
  (name :string))

(cffi:defcfun ("objc_msgSend" %objc-send-object) :pointer
  (receiver :pointer)
  (selector :pointer))

(cffi:defcfun ("objc_msgSend" %objc-send-boolean) :bool
  (receiver :pointer)
  (selector :pointer))

(cffi:defcfun ("objc_msgSend" %objc-send-void) :void
  (receiver :pointer)
  (selector :pointer))

(cffi:defcfun ("objc_msgSend" %objc-send-void-boolean) :void
  (receiver :pointer)
  (selector :pointer)
  (argument :bool))

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
  ;; There is no cooperating foreground application to yield activation when
  ;; a Slynk worker opens a window.  This is an explicit user request, so use
  ;; the same forceful activation that SDL uses for command-line Cocoa apps.
  ;; A process transitioning out of background-only policy needs a few Cocoa
  ;; event turns before Launch Services will honor activation, so pump briefly
  ;; instead of leaving a regular-but-background window in another Stage.
  (let ((application (cocoa-application)))
    (loop repeat 20
          until (%objc-send-boolean application (%objc-selector "isActive"))
          do (%objc-send-void-boolean
              application (%objc-selector "activateIgnoringOtherApps:") t)
             (sdl3:pump-events)
             (sleep 0.01))
    ;; The primary method already requested this before activation; repeat it
    ;; now that the application can actually make the window key.
    (sdl3:raise-window (sdl-canvas-window canvas))))

(defmethod deactivate-sdl-canvas-host ((canvas sdl-canvas))
  (declare (ignore canvas))
  (let ((application (cocoa-application)))
    (%objc-send-void application (%objc-selector "deactivate"))
    (set-cocoa-activation-policy application
                                 +cocoa-activation-policy-prohibited+)))
