(defpackage #:luv.objective-c
  (:nicknames #:objc)
  (:use #:cl #:luv.invocation)
  (:documentation
   "Declared Objective-C messages, explicit native ownership, and tracing.")
  (:export #:objective-c-error
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
           #:objective-c-string))

(defpackage #:luv.metal
  (:use #:cl)
  (:local-nicknames (#:objc #:luv.objective-c))
  (:documentation "The first native Metal declarations exercised through Lisp.")
  (:export #:make-system-default-device
           #:device-name
           #:device-registry-id
           #:probe-system-default-device))
