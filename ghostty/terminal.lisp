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
  ((pointer :initarg :pointer :reader terminal-pointer))
  (:documentation "An explicitly owned libghostty-vt terminal."))

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
