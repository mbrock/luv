(in-package #:luv.ghostty)

(defclass key-encoder ()
  ((pointer :initarg :pointer :reader key-encoder-pointer)
   (event-pointer :initarg :event-pointer :reader key-encoder-event-pointer))
  (:documentation
   "An explicitly owned reusable libghostty-vt keyboard encoder and event."))

(defun key-encoder-open-p (encoder)
  (not (cffi:null-pointer-p (key-encoder-pointer encoder))))

(defun make-key-encoder ()
  "Create an owned Ghostty key encoder using its default allocator."
  (load-libghostty-vt)
  (cffi:with-foreign-objects ((encoder-output :pointer) (event-output :pointer))
    (setf (cffi:mem-ref encoder-output :pointer) (cffi:null-pointer)
          (cffi:mem-ref event-output :pointer) (cffi:null-pointer))
    (check-result
     (%key-encoder-new (cffi:null-pointer) encoder-output)
     'make-key-encoder)
    (let ((encoder (cffi:mem-ref encoder-output :pointer)))
      (handler-case
          (progn
            (check-result
             (%key-event-new (cffi:null-pointer) event-output)
             'make-key-encoder)
            (make-instance
             'key-encoder :pointer encoder
             :event-pointer (cffi:mem-ref event-output :pointer)))
        (error (condition)
          (%key-encoder-free encoder)
          (error condition))))))

(defun close-key-encoder (encoder)
  "Release ENCODER and its reusable event. Repeated calls are harmless."
  (when (key-encoder-open-p encoder)
    (%key-event-free (key-encoder-event-pointer encoder))
    (%key-encoder-free (key-encoder-pointer encoder))
    (setf (slot-value encoder 'event-pointer) (cffi:null-pointer)
          (slot-value encoder 'pointer) (cffi:null-pointer)))
  encoder)

(defmacro with-key-encoder ((variable) &body body)
  `(let ((,variable (make-key-encoder)))
     (unwind-protect (progn ,@body)
       (close-key-encoder ,variable))))

(defun modifier-bits (modifiers)
  (loop for modifier in modifiers
        for bits = (ecase modifier
                     (:shift #x001)
                     (:control #x002)
                     ((:alt :meta) #x004)
                     (:super #x008)
                     (:caps-lock #x010)
                     (:num-lock #x020)
                     (:shift-right #x041)
                     (:control-right #x082)
                     ((:alt-right :meta-right) #x104)
                     (:super-right #x208))
        for result = bits then (logior result bits)
        finally (return (or result 0))))

(defun copy-foreign-octets (pointer length)
  (let ((bytes (make-array length :element-type '(unsigned-byte 8))))
    (dotimes (index length bytes)
      (setf (aref bytes index) (cffi:mem-aref pointer :uint8 index)))))

(defun encode-prepared-key-event (encoder event)
  (cffi:with-foreign-object (length :size)
    (cffi:with-foreign-object (buffer :uint8 128)
      (setf (cffi:mem-ref length :size) 0)
      (let ((result (%key-encoder-encode
                     (key-encoder-pointer encoder) event buffer 128 length)))
        (case result
          (:success
           (copy-foreign-octets buffer (cffi:mem-ref length :size)))
          (:out-of-space
           (let* ((capacity (cffi:mem-ref length :size))
                  (large-buffer (cffi:foreign-alloc :uint8 :count capacity)))
             (unwind-protect
                  (progn
                    (setf (cffi:mem-ref length :size) 0)
                    (check-result
                     (%key-encoder-encode
                      (key-encoder-pointer encoder) event
                      large-buffer capacity length)
                     'encode-key-event)
                    (copy-foreign-octets
                     large-buffer (cffi:mem-ref length :size)))
               (cffi:foreign-free large-buffer))))
          (otherwise
           (check-result result 'encode-key-event)))))))

(defun encode-key-event
    (encoder terminal action key
     &key modifiers consumed-modifiers text unshifted-codepoint composing-p)
  "Encode one physical key fact using TERMINAL's current keyboard modes.

ACTION is :PRESS, :REPEAT, or :RELEASE. KEY is a Ghostty/W3C physical key
keyword. MODIFIERS use :SHIFT, :CONTROL, :ALT (or :META), and :SUPER. TEXT is
layout-produced printable text; UNSHIFTED-CODEPOINT is its base codepoint."
  (unless (key-encoder-open-p encoder)
    (error "The libghostty-vt key encoder ~S is closed." encoder))
  (ensure-terminal-open terminal)
  (check-type text (or null string))
  (%key-encoder-setopt-from-terminal
   (key-encoder-pointer encoder) (terminal-pointer terminal))
  (let ((event (key-encoder-event-pointer encoder)))
    (%key-event-set-action event action)
    (%key-event-set-key event key)
    (%key-event-set-mods event (modifier-bits modifiers))
    (%key-event-set-consumed-mods event (modifier-bits consumed-modifiers))
    (%key-event-set-composing event composing-p)
    (%key-event-set-unshifted-codepoint event (or unshifted-codepoint 0))
    (if text
        (cffi:with-foreign-string ((pointer length) text :encoding :utf-8)
          ;; WITH-FOREIGN-STRING reports storage including its trailing NUL;
          ;; Ghostty takes an explicit byte count and must not see that byte.
          (%key-event-set-utf8 event pointer (1- length))
          (unwind-protect
               (encode-prepared-key-event encoder event)
            (%key-event-set-utf8 event (cffi:null-pointer) 0)))
        (progn
          (%key-event-set-utf8 event (cffi:null-pointer) 0)
          (encode-prepared-key-event encoder event)))))
