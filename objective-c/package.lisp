(defpackage #:luv.objective-c
  (:nicknames #:objc)
  (:use #:cl #:luv.invocation)
  (:documentation
   "Declared Objective-C messages, explicit native ownership, and tracing.")
  (:export #:objective-c-error
           #:objective-c-exception
           #:objective-c-exception-message
           #:objective-c-exception-receiver
           #:objective-c-exception-selector
           #:objective-c-exception-name
           #:objective-c-exception-reason
           #:objective-c-exception-call-stack
           #:objective-c-bridge-error
           #:unknown-objective-c-class
           #:unknown-objective-c-class-name
           #:released-objective-c-object
           #:released-objective-c-object-object
           #:objective-c-ownership-error
           #:objective-c-ownership-error-object
           #:objective-c-receiver
           #:objective-c-class
           #:objective-c-class-name
           #:objective-c-object
           #:objective-c-object-class-name
           #:objective-c-object-protocol-name
           #:objective-c-object-ownership
           #:objective-c-object-released-p
           #:objective-c-pointer
           #:find-objective-c-class
           #:wrap-objective-c-object
           #:retain-objective-c-object
           #:release-objective-c-object
           #:with-owned-objective-c-object
           #:objective-c-object=
           #:with-objective-c-native-environment
           #:*objective-c-exception-policy*
           #:with-unchecked-objective-c-messages
           #:with-objective-c-exception-handling
           #:objective-c-message-class
           #:objective-c-message
           #:objective-c-message-selector
           #:objective-c-message-result-type
           #:objective-c-message-result-ownership
           #:objective-c-message-result-class-name
           #:objective-c-message-consumes-receiver-p
           #:objective-c-message-description
           #:objective-c-invocation-description
           #:define-objective-c-message
           #:objective-c-runtime
           #:*objective-c-runtime*
           #:tracing-objective-c-runtime
           #:with-objective-c-trace
           #:with-autorelease-pool
           #:objective-c-string
           #:lisp-string-to-objective-c
           #:objective-c-error-description))

(defpackage #:luv.metal
  (:use #:cl)
  (:local-nicknames (#:objc #:luv.objective-c))
  (:documentation "Declared Metal and QuartzCore messages exercised through Lisp.")
  (:export #:make-system-default-device
           #:device-name
           #:device-registry-id
           #:probe-system-default-device
           #:new-metal-4-command-queue
           #:new-metal-4-compiler
           #:compile-metal-4-library
           #:new-metal-library-function
           #:metal-function-type
           #:new-command-allocator
           #:new-command-buffer
           #:begin-command-buffer
           #:end-command-buffer
           #:render-command-encoder
           #:end-encoding
           #:wait-for-drawable
           #:commit-command-buffer
           #:signal-drawable
           #:present-drawable
           #:set-layer-device
           #:set-layer-pixel-format
           #:layer-pixel-format
           #:set-layer-drawable-size
           #:layer-drawable-size
           #:next-drawable
           #:drawable-texture
           #:encode-clear-pass
           #:+language-version-4-0+
           #:+function-type-vertex+
           #:+function-type-fragment+
           #:+pixel-format-bgra8-unorm+
           #:+pixel-format-bgra8-unorm-srgb+))
