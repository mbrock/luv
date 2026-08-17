(defpackage #:luv.libav
  (:nicknames #:libav)
  (:use #:cl)
  (:documentation "Explicitly owned Common Lisp access to FFmpeg's libav* libraries.")
  (:export #:load-libav
           #:libav-loaded-p
           #:libav-error
           #:libav-error-operation
           #:libav-error-code
           #:libav-error-message
           #:libav-version-mismatch
           #:libav-build
           #:libav-versions
           #:libav-configuration
           #:pixel-format
           #:hardware-device-type
           #:hardware-device-types
           #:hardware-device-type-name
           #:decoder-available-p
           #:pixel-format-name
           #:frame
           #:make-frame
           #:frame-pointer
           #:frame-live-p
           #:release-frame
           #:with-frame
           #:frame-width
           #:frame-height
           #:frame-pixel-format
           #:frame-presentation-timestamp
           #:frame-duration
           #:frame-key-p
           #:frame-plane-pointer
           #:frame-plane-pitch
           #:frame-hardware-p
           #:allocate-frame-buffer
           #:unreference-frame))
