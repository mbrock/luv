;;;; An explicitly owned AVFrame.
;;;;
;;;; AVFrame has two lifetimes stacked on top of each other: the struct
;;;; itself, allocated by av_frame_alloc and released by av_frame_free, and
;;;; the reference-counted buffers it points at, which av_frame_unref drops
;;;; without disturbing the struct.  A decode loop reuses one frame across
;;;; thousands of pictures by unreferencing between them, so the two are kept
;;;; separate here rather than folded into one "close".
;;;;
;;;; Nothing in this file copies pixels.  A hardware frame does not have any
;;;; to copy -- its `data[3]' is a CVPixelBuffer, a VASurfaceID, or an
;;;; AVVkFrame -- and the software path wants its planes uploaded straight from
;;;; the decoder's memory rather than through a Lisp array.

(in-package #:luv.libav)

(defclass frame ()
  ((pointer :initarg :pointer :reader frame-pointer))
  (:documentation "An explicitly owned AVFrame."))

(cffi:defcfun ("av_frame_clone" %av-frame-clone) :pointer
  (source :pointer))

(defun frame-live-p (frame)
  (not (cffi:null-pointer-p (frame-pointer frame))))

(defun ensure-frame-live (frame)
  (unless (frame-live-p frame)
    (error "The AVFrame ~S has been released." frame))
  frame)

(defun make-frame ()
  "Allocate an empty AVFrame.  It carries no picture until something fills it."
  (load-libav)
  (let ((pointer (%av-frame-alloc)))
    (when (cffi:null-pointer-p pointer)
      (error "av_frame_alloc could not allocate an AVFrame."))
    (make-instance 'frame :pointer pointer)))

(defun clone-frame (frame)
  "Return a new reference to FRAME and all buffers backing its picture."
  (let ((pointer (%av-frame-clone (frame-pointer (ensure-frame-live frame)))))
    (when (cffi:null-pointer-p pointer)
      (error "av_frame_clone could not retain the decoded picture."))
    (make-instance 'frame :pointer pointer)))

(defun release-frame (frame)
  "Release FRAME and any buffers it holds.  Repeated calls are harmless."
  (when (frame-live-p frame)
    ;; av_frame_free takes the address of the pointer and nulls it, so hand it
    ;; a cell rather than the pointer itself.
    (cffi:with-foreign-object (cell :pointer)
      (setf (cffi:mem-ref cell :pointer) (frame-pointer frame))
      (%av-frame-free cell))
    (setf (slot-value frame 'pointer) (cffi:null-pointer)))
  frame)

(defmacro with-frame ((variable) &body body)
  "Evaluate BODY with VARIABLE bound to a fresh frame, releasing it after."
  `(let ((,variable (make-frame)))
     (unwind-protect (progn ,@body)
       (release-frame ,variable))))

(defun unreference-frame (frame)
  "Drop FRAME's buffers, leaving the struct ready to receive another picture."
  (ensure-frame-live frame)
  (%av-frame-unref (frame-pointer frame))
  frame)

(macrolet
    ((define-frame-slot (name slot documentation)
       `(progn
          (defun ,name (frame)
            ,documentation
            (cffi:foreign-slot-value
             (frame-pointer (ensure-frame-live frame))
             '(:struct av-frame) ',slot))
          (defun (setf ,name) (value frame)
            (setf (cffi:foreign-slot-value
                   (frame-pointer (ensure-frame-live frame))
                   '(:struct av-frame) ',slot)
                  value)))))
  (define-frame-slot frame-width width
    "FRAME's width in pixels.")
  (define-frame-slot frame-height height
    "FRAME's height in pixels.")
  (define-frame-slot frame-presentation-timestamp presentation-timestamp
    "FRAME's presentation timestamp, in its stream's time base.")
  (define-frame-slot frame-duration duration
    "FRAME's duration, in its stream's time base."))

(defun frame-pixel-format (frame)
  "FRAME's pixel format as a keyword, or its raw integer when unrecognised.

A hardware format -- :VIDEOTOOLBOX, :VAAPI, :VULKAN -- means the picture never
touched the CPU and FRAME's third plane holds a platform surface."
  (let ((format (cffi:foreign-slot-value
                 (frame-pointer (ensure-frame-live frame))
                 '(:struct av-frame) 'format)))
    (or (cffi:foreign-enum-keyword 'pixel-format format :errorp nil)
        format)))

(defun (setf frame-pixel-format) (format frame)
  (setf (cffi:foreign-slot-value
         (frame-pointer (ensure-frame-live frame))
         '(:struct av-frame) 'format)
        (if (keywordp format)
            (cffi:foreign-enum-value 'pixel-format format)
            format))
  format)

(defun frame-key-p (frame)
  "True when FRAME is a key frame."
  (logtest +frame-flag-key+
           (cffi:foreign-slot-value
            (frame-pointer (ensure-frame-live frame))
            '(:struct av-frame) 'flags)))

(defun frame-hardware-p (frame)
  "True when FRAME's picture lives in a hardware frames pool, not in memory."
  (not (cffi:null-pointer-p
        (cffi:foreign-slot-value
         (frame-pointer (ensure-frame-live frame))
         '(:struct av-frame) 'hardware-frames-context))))

(defun frame-plane-pointer (frame plane)
  "Return the foreign pointer to FRAME's PLANE, counting from zero."
  (check-type plane (integer 0 7))
  (cffi:mem-aref
   (cffi:foreign-slot-pointer
    (frame-pointer (ensure-frame-live frame)) '(:struct av-frame) 'data)
   :pointer plane))

(defun frame-videotoolbox-pixel-buffer (frame)
  "Return FRAME's borrowed CVPixelBuffer, or NIL for a non-VideoToolbox frame."
  (when (eq :videotoolbox (frame-pixel-format frame))
    (let ((buffer (frame-plane-pointer frame 3)))
      (unless (cffi:null-pointer-p buffer) buffer))))

(defun frame-vulkan-frame (frame)
  "Return FRAME's borrowed AVVkFrame, or NIL for a non-Vulkan frame."
  (when (eq :vulkan (frame-pixel-format frame))
    (let ((vulkan-frame (frame-plane-pointer frame 0)))
      (unless (cffi:null-pointer-p vulkan-frame) vulkan-frame))))

(defun frame-plane-pitch (frame plane)
  "Return the byte stride of FRAME's PLANE, which exceeds its width when padded."
  (check-type plane (integer 0 7))
  (cffi:mem-aref
   (cffi:foreign-slot-pointer
    (frame-pointer (ensure-frame-live frame)) '(:struct av-frame) 'pitches)
   :int plane))

(defun allocate-frame-buffer (frame &key (alignment 0))
  "Allocate buffers for FRAME's current width, height, and pixel format.

An ALIGNMENT of zero lets FFmpeg choose the alignment its own SIMD paths want,
which is what a decoder would do."
  (ensure-frame-live frame)
  (check-code (%av-frame-get-buffer (frame-pointer frame) alignment)
              'allocate-frame-buffer)
  frame)
