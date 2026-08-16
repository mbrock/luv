(in-package #:luv.ghostty)

(define-condition ghostty-error (error)
  ((operation :initarg :operation :reader ghostty-error-operation)
   (result :initarg :result :reader ghostty-error-result))
  (:report
   (lambda (condition stream)
     (format stream "libghostty-vt operation ~S failed with ~S."
             (ghostty-error-operation condition)
             (ghostty-error-result condition)))))

(defun check-result (result operation)
  ;; cffi-libffi returns the formatter constructor's scalar result through
  ;; raw storage because that call also has a structure-by-value argument.
  (when (integerp result)
    (setf result (cffi:foreign-enum-keyword 'ghostty-result result)))
  (unless (eq result :success)
    (error 'ghostty-error :operation operation :result result))
  result)

(defclass terminal ()
  ((pointer :initarg :pointer :reader terminal-pointer)
   (response-function :initform nil :accessor terminal-response-function)
   (callback-condition :initform nil :accessor terminal-callback-condition))
  (:documentation "An explicitly owned libghostty-vt terminal."))

(defvar *terminal-callbacks* (make-hash-table :test #'eql))

(defvar *terminal-callbacks-lock*
  (sb-thread:make-mutex :name "libghostty-vt terminal callbacks"))

(defun terminal-for-native-pointer (pointer)
  (sb-thread:with-mutex (*terminal-callbacks-lock*)
    (gethash (cffi:pointer-address pointer) *terminal-callbacks*)))

(cffi:defcallback %terminal-write-pty-callback :void
    ((pointer :pointer) (userdata :pointer) (data :pointer) (length :size))
  (declare (ignore userdata))
  (let ((terminal (terminal-for-native-pointer pointer)))
    (when terminal
      (let ((function (terminal-response-function terminal)))
        (when function
          ;; Ghostty's bytes are borrowed only for this callback.  Copy before
          ;; entering application code, which may enqueue them for later IO.
          (let ((bytes (make-array length :element-type '(unsigned-byte 8))))
            (dotimes (index length)
              (setf (aref bytes index) (cffi:mem-aref data :uint8 index)))
            ;; A Lisp condition must never unwind through Ghostty's C stack.
            ;; Retain it on the terminal so the owner can surface it safely.
            (handler-case (funcall function bytes)
              (error (condition)
                (setf (terminal-callback-condition terminal) condition)))))))))

(defun terminal-open-p (terminal)
  (not (cffi:null-pointer-p (terminal-pointer terminal))))

(defun ensure-terminal-open (terminal)
  (unless (terminal-open-p terminal)
    (error "The libghostty-vt terminal ~S is closed." terminal))
  terminal)

(defun make-terminal (&key (columns 80) (rows 24))
  "Create an owned libghostty-vt terminal using its default allocator."
  (load-libghostty-vt)
  (cffi:with-foreign-object (output :pointer)
    (setf (cffi:mem-ref output :pointer) (cffi:null-pointer))
    (check-result
     (%terminal-new (cffi:null-pointer) output columns rows)
     'make-terminal)
    (make-instance 'terminal :pointer (cffi:mem-ref output :pointer))))

(defun close-terminal (terminal)
  "Release TERMINAL. Repeated calls are harmless."
  (when (terminal-open-p terminal)
    (set-terminal-response-function terminal nil)
    (%terminal-free (terminal-pointer terminal))
    (setf (slot-value terminal 'pointer) (cffi:null-pointer)))
  terminal)

(defmacro with-terminal ((variable &rest initargs) &body body)
  `(let ((,variable (make-terminal ,@initargs)))
     (unwind-protect (progn ,@body)
       (close-terminal ,variable))))

(defun write-terminal (terminal text)
  "Feed the UTF-8 encoding of TEXT to TERMINAL's VT stream parser."
  (ensure-terminal-open terminal)
  (cffi:with-foreign-string ((data length) text :encoding :utf-8)
    (%terminal-vt-write (terminal-pointer terminal) data length))
  terminal)

(defun write-terminal-bytes (terminal bytes &key (start 0) end)
  "Feed the octets in BYTES from START to END to TERMINAL's VT parser."
  (ensure-terminal-open terminal)
  (check-type bytes (simple-array (unsigned-byte 8) (*)))
  (let ((end (or end (length bytes))))
    (unless (<= 0 start end (length bytes))
      (error "Invalid terminal byte range [~D,~D) for ~D octets."
             start end (length bytes)))
    (unless (= start end)
      (cffi:with-pointer-to-vector-data (data bytes)
        (%terminal-vt-write
         (terminal-pointer terminal) (cffi:inc-pointer data start) (- end start)))))
  terminal)

(defun resize-terminal
    (terminal columns rows &key (cell-width-pixels 0) (cell-height-pixels 0))
  "Resize TERMINAL in cells and record its optional cell pixel geometry."
  (ensure-terminal-open terminal)
  (check-type columns (integer 1 65535))
  (check-type rows (integer 1 65535))
  (check-type cell-width-pixels (unsigned-byte 32))
  (check-type cell-height-pixels (unsigned-byte 32))
  (check-result
   (%terminal-resize (terminal-pointer terminal) columns rows
                     cell-width-pixels cell-height-pixels)
   'resize-terminal)
  terminal)

(defun terminal-size (terminal)
  "Return TERMINAL's column and row counts."
  (ensure-terminal-open terminal)
  (cffi:with-foreign-objects ((columns :uint16) (rows :uint16))
    (check-result
     (%terminal-get (terminal-pointer terminal) :columns columns)
     'terminal-size)
    (check-result
     (%terminal-get (terminal-pointer terminal) :rows rows)
     'terminal-size)
    (values (cffi:mem-ref columns :uint16) (cffi:mem-ref rows :uint16))))

(defun set-terminal-response-function (terminal function)
  "Route terminal-generated response octets to FUNCTION, or disable with NIL.

FUNCTION is called synchronously during WRITE-TERMINAL or
WRITE-TERMINAL-BYTES.  Its octet vector is an owned copy of Ghostty's borrowed
callback bytes.  FUNCTION must enqueue or copy promptly and must not feed the
same terminal recursively."
  (ensure-terminal-open terminal)
  (check-type function (or null function))
  (let ((key (cffi:pointer-address (terminal-pointer terminal))))
    (if function
        (progn
          (setf (terminal-response-function terminal) function
                (terminal-callback-condition terminal) nil)
          (sb-thread:with-mutex (*terminal-callbacks-lock*)
            (setf (gethash key *terminal-callbacks*) terminal))
          (handler-case
              (check-result
               (%terminal-set
                (terminal-pointer terminal) :write-pty
                (cffi:callback %terminal-write-pty-callback))
               'set-terminal-response-function)
            (error (condition)
              (sb-thread:with-mutex (*terminal-callbacks-lock*)
                (remhash key *terminal-callbacks*))
              (setf (terminal-response-function terminal) nil)
              (error condition))))
        (progn
          (check-result
           (%terminal-set (terminal-pointer terminal) :write-pty
                          (cffi:null-pointer))
           'set-terminal-response-function)
          (setf (terminal-response-function terminal) nil)
          (sb-thread:with-mutex (*terminal-callbacks-lock*)
            (remhash key *terminal-callbacks*)))))
  function)

(defun make-plain-formatter (terminal)
  (cffi:with-foreign-objects
      ((output :pointer)
       (options '(:struct formatter-terminal-options)))
    (loop for index below (cffi:foreign-type-size
                           '(:struct formatter-terminal-options))
          do (setf (cffi:mem-aref options :uint8 index) 0))
    (setf (cffi:foreign-slot-value
           options '(:struct formatter-terminal-options) 'size)
          (cffi:foreign-type-size '(:struct formatter-terminal-options))
          (cffi:foreign-slot-value
           options '(:struct formatter-terminal-options) 'emit)
          :plain
          (cffi:foreign-slot-value
           options '(:struct formatter-terminal-options) 'trim)
          t)
    (check-result
     (%formatter-terminal-new
      (cffi:null-pointer) output (terminal-pointer terminal)
      (cffi:mem-ref options '(:struct formatter-terminal-options)))
     'terminal-text)
    (cffi:mem-ref output :pointer)))

(defun terminal-text (terminal)
  "Return the active screen of TERMINAL as trimmed plain UTF-8 text."
  (ensure-terminal-open terminal)
  (let ((formatter (make-plain-formatter terminal)))
    (unwind-protect
         (cffi:with-foreign-objects ((output :pointer) (length :size))
           (setf (cffi:mem-ref output :pointer) (cffi:null-pointer)
                 (cffi:mem-ref length :size) 0)
           (check-result
            (%formatter-format-alloc formatter (cffi:null-pointer)
                                     output length)
            'terminal-text)
           (let ((pointer (cffi:mem-ref output :pointer))
                 (byte-count (cffi:mem-ref length :size)))
             (unwind-protect
                  (cffi:foreign-string-to-lisp
                   pointer :count byte-count :encoding :utf-8)
               (%ghostty-free (cffi:null-pointer) pointer byte-count))))
      (%formatter-free formatter))))
