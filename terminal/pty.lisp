;;; A PTY process which drives one libghostty-vt terminal from one IO owner.

(in-package #:luv.terminal)

(cffi:define-foreign-library pty-library
  (:darwin (:or "libutil.dylib" "/usr/lib/libutil.dylib"))
  (:unix (:or "libutil.so.1" "libutil.so")))

(cffi:use-foreign-library pty-library)

(cffi:defcstruct pty-window-size
  (rows :uint16)
  (columns :uint16)
  (width-pixels :uint16)
  (height-pixels :uint16))

(cffi:defcfun ("forkpty" %forkpty) :int
  (master :pointer)
  (name :pointer)
  (terminal-attributes :pointer)
  (window-size :pointer))

(cffi:defcfun ("chdir" %chdir) :int
  (path :pointer))

(cffi:defcfun ("execve" %execve) :int
  (path :pointer)
  (arguments :pointer)
  (environment :pointer))

(cffi:defcfun ("_exit" %exit) :void
  (status :int))

(defconstant +pty-set-window-size-request+
  #+darwin #x80087467
  #+linux #x5414
  #-(or darwin linux) 0)

(defclass pty-child ()
  ((pid :initarg :pid :reader pty-child-pid)
   (status :initform :running :accessor pty-child-status)
   (exit-code :initform nil :accessor pty-child-exit-code)))

(defclass pty-device ()
  ((terminal :initarg :terminal :reader pty-device-terminal)
   (process :initarg :process :reader pty-device-process)
   (stream :initarg :stream :reader pty-device-stream)
   (mailbox :initarg :mailbox :reader pty-device-mailbox)
   (lock :initarg :lock :reader pty-device-lock)
   (on-output :initarg :on-output :initform nil :reader pty-device-on-output)
   (key-encoder :initform nil :accessor pty-device-key-encoder)
   (state :initform :starting :accessor %pty-device-state)
   (exit-code :initform nil :accessor %pty-device-exit-code)
   (condition :initform nil :accessor %pty-device-condition)
   (thread :initform nil :accessor pty-device-thread))
  (:documentation
   "A child PTY and the serialized IO path which drives one Ghostty terminal.

The worker, mailbox, and terminal lock realize the single-owner flow in
#K3KFGZ; the device deliberately does not own the semantic terminal."))

(defun pty-device-state (device)
  (sb-thread:with-mutex ((pty-device-lock device))
    (%pty-device-state device)))

(defun pty-device-exit-code (device)
  (sb-thread:with-mutex ((pty-device-lock device))
    (%pty-device-exit-code device)))

(defun pty-device-condition (device)
  (sb-thread:with-mutex ((pty-device-lock device))
    (%pty-device-condition device)))

(defun copy-octets (bytes &key (start 0) end)
  (check-type bytes (array (unsigned-byte 8) (*)))
  (let ((end (or end (length bytes))))
    (unless (<= 0 start end (length bytes))
      (error "Invalid PTY byte range [~D,~D) for ~D octets."
             start end (length bytes)))
    (let ((copy (make-array (- end start) :element-type '(unsigned-byte 8))))
      (replace copy bytes :start2 start :end2 end)
      copy)))

(defun send-pty-device-bytes (device bytes &key (start 0) end)
  "Enqueue owned input octets for DEVICE's child process."
  (check-type device pty-device)
  (let ((copy (copy-octets bytes :start start :end end)))
    (unless (member (pty-device-state device) '(:starting :running))
      (error "PTY device ~S is not accepting input." device))
    (sb-concurrency:send-message
     (pty-device-mailbox device) (list :write copy)))
  device)

(defun send-pty-device-text (device text)
  "UTF-8 encode TEXT and enqueue it for DEVICE's child process."
  (send-pty-device-bytes device (sb-ext:string-to-octets text :external-format :utf-8)))

(defun send-pty-device-key
    (device action key
     &key modifiers consumed-modifiers text unshifted-codepoint composing-p)
  "Enqueue one semantic Ghostty key event for DEVICE's child process.

Encoding happens on the PTY owner against the terminal's then-current modes."
  (check-type device pty-device)
  (check-type text (or null string))
  (unless (member (pty-device-state device) '(:starting :running))
    (error "PTY device ~S is not accepting input." device))
  (sb-concurrency:send-message
   (pty-device-mailbox device)
   (list :key action key (copy-list modifiers) (copy-list consumed-modifiers)
         (and text (copy-seq text)) unshifted-codepoint composing-p))
  device)

(defun resize-pty-device
    (device columns rows &key (cell-width-pixels 0) (cell-height-pixels 0))
  "Serialize a matching Ghostty and PTY window resize on DEVICE's IO owner."
  (check-type device pty-device)
  (check-type columns (integer 1 65535))
  (check-type rows (integer 1 65535))
  (check-type cell-width-pixels (unsigned-byte 16))
  (check-type cell-height-pixels (unsigned-byte 16))
  (unless (member (pty-device-state device) '(:starting :running))
    (error "PTY device ~S cannot be resized in state ~S."
           device (pty-device-state device)))
  (sb-concurrency:send-message
   (pty-device-mailbox device)
   (list :resize columns rows cell-width-pixels cell-height-pixels))
  device)

(defun call-with-pty-device-terminal (device function)
  "Call FUNCTION with DEVICE's terminal while excluding PTY mutation."
  (check-type device pty-device)
  (check-type function function)
  (sb-thread:with-mutex ((pty-device-lock device))
    (funcall function (pty-device-terminal device))))

(defun write-pty-stream-bytes (stream bytes)
  (loop for byte across bytes do (write-byte byte stream))
  (force-output stream))

(defun read-pty-stream-bytes (stream &optional (capacity 4096))
  "Return readable PTY octets, EOF status, and stream-error status."
  (let ((bytes (make-array capacity :element-type '(unsigned-byte 8)))
        (count 0)
        (eof-p nil)
        (stream-error-p nil))
    (loop while (< count capacity)
          for byte =
            (handler-case
                (and (listen stream) (read-byte stream nil :eof))
              ;; A closed PTY master reports EIO on Linux instead of an
              ;; ordinary zero-length read.  Preserve any bytes collected
              ;; earlier in this drain before publishing EOF.
              (stream-error ()
                (setf stream-error-p t)
                :stream-error))
          do (cond
               ((null byte) (return))
               ((eq byte :eof) (setf eof-p t) (return))
               ((eq byte :stream-error) (return))
               (t
                (setf (aref bytes count) byte)
                (incf count))))
    (values (if (= count capacity) bytes (subseq bytes 0 count))
            eof-p stream-error-p)))

(defun enable-native-pty-echo (stream)
  "Restore the ordinary terminal-driver echo disabled by RUN-PROGRAM :PTY.

Interactive terminal children expect to inherit ECHO initially.  They remain
free to disable it themselves for password entry, full-screen programs, or
other private input modes."
  (let* ((file-descriptor (sb-sys:fd-stream-fd stream))
         (attributes (sb-posix:tcgetattr file-descriptor)))
    (setf (sb-posix:termios-lflag attributes)
          (logior (sb-posix:termios-lflag attributes) sb-posix:echo))
    (sb-posix:tcsetattr file-descriptor sb-posix:tcsanow attributes))
  stream)

(defun set-native-pty-size
    (stream columns rows cell-width-pixels cell-height-pixels)
  #-(or darwin linux)
  (error "PTY resizing is unsupported on this platform.")
  #+(or darwin linux)
  (cffi:with-foreign-object (size '(:struct pty-window-size))
    (setf (cffi:foreign-slot-value size '(:struct pty-window-size) 'rows) rows
          (cffi:foreign-slot-value size '(:struct pty-window-size) 'columns)
          columns
          (cffi:foreign-slot-value
           size '(:struct pty-window-size) 'width-pixels)
          (min 65535 (* columns cell-width-pixels))
          (cffi:foreign-slot-value
           size '(:struct pty-window-size) 'height-pixels)
          (min 65535 (* rows cell-height-pixels)))
    ;; SB-POSIX declares ioctl's third argument with the native variadic ABI.
    ;; Passing the CFFI pointer through its alien view matters on arm64 Darwin.
    (sb-posix:ioctl
     (sb-sys:fd-stream-fd stream) +pty-set-window-size-request+
     (sb-alien:sap-alien size (* t)))))

(defun perform-pty-device-message (device message)
  (ecase (first message)
    (:write
     (write-pty-stream-bytes (pty-device-stream device) (second message))
     :continue)
    (:key
     (destructuring-bind
         (operation action key modifiers consumed-modifiers text
          unshifted-codepoint composing-p)
         message
       (declare (ignore operation))
       (let ((bytes
               (sb-thread:with-mutex ((pty-device-lock device))
                 (let ((encoder
                         (or (pty-device-key-encoder device)
                             (setf (pty-device-key-encoder device)
                                   (ghostty:make-key-encoder)))))
                   (ghostty:encode-key-event
                    encoder (pty-device-terminal device) action key
                    :modifiers modifiers
                    :consumed-modifiers consumed-modifiers
                    :text text
                    :unshifted-codepoint unshifted-codepoint
                    :composing-p composing-p)))))
         (when (plusp (length bytes))
           (write-pty-stream-bytes (pty-device-stream device) bytes))))
     :continue)
    (:resize
     (destructuring-bind
         (operation columns rows cell-width-pixels cell-height-pixels) message
       (declare (ignore operation))
       ;; Neither operation can interleave with a read or another command.  The
       ;; child sees the kernel size before Ghostty publishes the matching grid.
       (set-native-pty-size
        (pty-device-stream device) columns rows
        cell-width-pixels cell-height-pixels)
       (sb-thread:with-mutex ((pty-device-lock device))
         (ghostty:resize-terminal
          (pty-device-terminal device) columns rows
          :cell-width-pixels cell-width-pixels
          :cell-height-pixels cell-height-pixels)))
     :continue)
    (:stop :stop)))

(defun drain-pty-device-messages (device)
  (loop
    (multiple-value-bind (message present-p)
        (sb-concurrency:receive-message-no-hang (pty-device-mailbox device))
      (unless present-p (return :continue))
      (when (eq :stop (perform-pty-device-message device message))
        (return :stop)))))

(defun update-pty-child-status (child &optional no-hang-p)
  (when (eq :running (pty-child-status child))
    (multiple-value-bind (pid status)
        (sb-posix:waitpid (pty-child-pid child)
                          (if no-hang-p sb-posix:wnohang 0))
      (when (plusp pid)
        (cond
          ((sb-posix:wifexited status)
           (setf (pty-child-status child) :exited
                 (pty-child-exit-code child) (sb-posix:wexitstatus status)))
          ((sb-posix:wifsignaled status)
           (setf (pty-child-status child) :signaled
                 (pty-child-exit-code child) (sb-posix:wtermsig status)))))))
  (pty-child-status child))

(defun process-finished-p (process)
  (not (eq :running (update-pty-child-status process t))))

(defun wait-for-process-exit (process timeout)
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout internal-time-units-per-second)))))
    (loop until (process-finished-p process)
          when (>= (get-internal-real-time) deadline) do (return nil)
          do (sleep 0.01)
          finally (return t))))

(defun release-pty-device-process (device close-requested-p)
  (let ((process (pty-device-process device))
        (stream (pty-device-stream device)))
    (ignore-errors
      (sb-thread:with-mutex ((pty-device-lock device))
        (ghostty:set-terminal-response-function
         (pty-device-terminal device) nil)))
    (when (pty-device-key-encoder device)
      (ignore-errors
        (ghostty:close-key-encoder (pty-device-key-encoder device)))
      (setf (pty-device-key-encoder device) nil))
    (ignore-errors (close stream))
    (when (eq :running (update-pty-child-status process t))
      (ignore-errors (sb-posix:kill (pty-child-pid process) sb-posix:sighup))
      (unless (wait-for-process-exit process 0.25)
        (ignore-errors (sb-posix:kill (pty-child-pid process) sb-posix:sigterm))
        (unless (wait-for-process-exit process 0.25)
          (ignore-errors (sb-posix:kill (pty-child-pid process) sb-posix:sigkill))
          (wait-for-process-exit process 0.25))))
    (let ((status (update-pty-child-status process t)))
      (sb-thread:with-mutex ((pty-device-lock device))
        (setf (%pty-device-exit-code device)
              (and (member status '(:exited :signaled))
                   (pty-child-exit-code process)))
        (unless (eq (%pty-device-state device) :failed)
          (setf (%pty-device-state device)
                (if close-requested-p :closed :exited)))))
    process))

(defun run-pty-device (device)
  (let ((close-requested-p nil))
    (unwind-protect
         (handler-case
             (progn
               (sb-thread:with-mutex ((pty-device-lock device))
                 (setf (%pty-device-state device) :running))
               (loop
                 (when (eq :stop (drain-pty-device-messages device))
                   (setf close-requested-p t)
                   (return))
                 (multiple-value-bind (bytes eof-p stream-error-p)
                     (handler-case
                         (read-pty-stream-bytes (pty-device-stream device))
                       (stream-error ()
                         (values #() nil t)))
                   (declare (ignore stream-error-p))
                   (when (plusp (length bytes))
                     (sb-thread:with-mutex ((pty-device-lock device))
                       (ghostty:write-terminal-bytes
                        (pty-device-terminal device) bytes))
                     (let ((function (pty-device-on-output device)))
                       (when function (funcall function device bytes))))
                   ;; Child exit and master EOF are not ordered: a fast child
                   ;; can be reaped just before its final output becomes
                   ;; readable.  Drain any bytes first, then accept either
                   ;; process exit or the PTY's EOF.
                   (when (and (or eof-p
                                  (process-finished-p
                                   (pty-device-process device)))
                              (zerop (length bytes)))
                     (return))
                   (when (zerop (length bytes)) (sleep 0.005)))))
           (error (condition)
             (sb-thread:with-mutex ((pty-device-lock device))
               (setf (%pty-device-condition device) condition
                     (%pty-device-state device) :failed))))
      (release-pty-device-process device close-requested-p)))
  device)

(defun environment-with-terminal-capabilities (environment term)
  (let ((result (copy-list (or environment (sb-ext:posix-environ)))))
    (labels ((set-variable (name value)
               (setf result
                     (delete-if
                      (lambda (entry)
                        (and (> (length entry) (length name))
                             (string= name entry :end2 (length name))
                             (char= #\= (char entry (length name)))))
                      result))
               (when value (push (format nil "~A=~A" name value) result))))
      (when term
        (set-variable "TERM" term)
        (set-variable "COLORTERM" "truecolor")))
    result))

(defun environment-variable (name environment)
  (loop with prefix = (concatenate 'string name "=")
        for entry in environment
        when (and (>= (length entry) (length prefix))
                  (string= prefix entry :end2 (length prefix)))
          do (return (subseq entry (length prefix)))))

(defun executable-file-p (path)
  (handler-case
      (zerop (sb-posix:access path sb-posix:x-ok))
    (sb-posix:syscall-error () nil)))

(defun resolve-pty-program (program environment directory)
  (let* ((program (namestring program))
         (working-directory
           (merge-pathnames
            (uiop:ensure-directory-pathname (or directory "."))
            (uiop:getcwd))))
    (labels ((under-working-directory (path)
               (namestring
                (if (uiop:absolute-pathname-p path)
                    path
                    (merge-pathnames path working-directory)))))
      (if (find #\/ program)
          (under-working-directory program)
          (or (loop for path in (uiop:split-string
                                 (or (environment-variable "PATH" environment)
                                     "/usr/local/bin:/usr/bin:/bin")
                                 :separator '(#\:))
                    for directory = (if (zerop (length path)) "." path)
                    for candidate = (under-working-directory
                                     (merge-pathnames
                                      program
                                      (uiop:ensure-directory-pathname directory)))
                    when (executable-file-p candidate) return candidate)
              (error "Executable ~S was not found in PATH." program))))))

(defun allocate-foreign-string-vector (strings)
  (let ((vector (cffi:foreign-alloc :pointer :count (1+ (length strings))))
        (pointers nil))
    (handler-case
        (progn
          (loop for string in strings
                for index from 0
                for pointer = (cffi:foreign-string-alloc string :encoding :utf-8)
                do (push pointer pointers)
                   (setf (cffi:mem-aref vector :pointer index) pointer))
          (setf (cffi:mem-aref vector :pointer (length strings))
                (cffi:null-pointer))
          (values vector pointers))
      (error (condition)
        (mapc #'cffi:foreign-string-free pointers)
        (cffi:foreign-free vector)
        (error condition)))))

(defun free-foreign-string-vector (vector pointers)
  (mapc #'cffi:foreign-string-free pointers)
  (cffi:foreign-free vector))

(defun launch-pty-child (program arguments directory environment columns rows)
  "Fork PROGRAM under a fresh controlling PTY and return its child and stream."
  (let* ((program-path (resolve-pty-program program environment directory))
         (directory-path (and directory (namestring directory)))
         (program-pointer (cffi:foreign-string-alloc program-path :encoding :utf-8))
         (directory-pointer
           (and directory-path
                (cffi:foreign-string-alloc directory-path :encoding :utf-8)))
         (argument-vector nil)
         (argument-pointers nil)
         (environment-vector nil)
         (environment-pointers nil))
    (unwind-protect
         (progn
           (multiple-value-setq (argument-vector argument-pointers)
             (allocate-foreign-string-vector
              (cons (namestring program) arguments)))
           (multiple-value-setq (environment-vector environment-pointers)
             (allocate-foreign-string-vector environment))
           (cffi:with-foreign-objects
               ((master :int) (size '(:struct pty-window-size)))
             (setf (cffi:foreign-slot-value
                    size '(:struct pty-window-size) 'rows) rows
                   (cffi:foreign-slot-value
                    size '(:struct pty-window-size) 'columns) columns
                   (cffi:foreign-slot-value
                    size '(:struct pty-window-size) 'width-pixels) 0
                   (cffi:foreign-slot-value
                    size '(:struct pty-window-size) 'height-pixels) 0)
             (let ((pid (%forkpty master (cffi:null-pointer)
                                  (cffi:null-pointer) size)))
               (cond
                 ((minusp pid)
                  (error "forkpty failed."))
                 ((zerop pid)
                  ;; Everything the child needs was allocated before FORKPTY;
                  ;; cross the post-fork window with only libc calls.
                  (when (and directory-pointer
                             (minusp (%chdir directory-pointer)))
                    (%exit 126))
                  (%execve program-pointer argument-vector environment-vector)
                  (%exit 127))
                 (t
                  (let ((master-fd (cffi:mem-ref master :int)))
                    (handler-case
                        (values
                         (make-instance 'pty-child :pid pid)
                         (sb-sys:make-fd-stream
                          master-fd
                          :input t :output t :dual-channel-p t
                          :element-type '(unsigned-byte 8)
                          :buffering :none :auto-close t
                          :name (format nil "PTY for ~A" program)))
                      (error (condition)
                        (ignore-errors (sb-posix:close master-fd))
                        (ignore-errors (sb-posix:kill pid sb-posix:sigterm))
                        (ignore-errors (sb-posix:waitpid pid 0))
                        (error condition)))))))))
      (when environment-vector
        (free-foreign-string-vector environment-vector environment-pointers))
      (when argument-vector
        (free-foreign-string-vector argument-vector argument-pointers))
      (when directory-pointer (cffi:foreign-string-free directory-pointer))
      (cffi:foreign-string-free program-pointer))))

(defun open-pty-device
    (terminal &key
                (program (or (uiop:getenv "SHELL") "/bin/sh"))
                arguments directory environment
                (term "xterm-256color") on-output)
  "Start PROGRAM under a PTY whose output drives TERMINAL.

The device owns its child, PTY stream, IO thread, and Ghostty response route;
it does not own TERMINAL.  All terminal mutation and PTY traffic are serialized
by the device's worker.  Use CALL-WITH-PTY-DEVICE-TERMINAL for renderer-side
snapshot work which must not race mutation."
  (check-type terminal ghostty:terminal)
  (unless (ghostty:terminal-open-p terminal)
    (error "Cannot attach a PTY to closed terminal ~S." terminal))
  (let* ((child-environment
           (environment-with-terminal-capabilities environment term))
         (process nil)
         (stream nil)
         (device
           (multiple-value-bind (columns rows) (ghostty:terminal-size terminal)
             (multiple-value-bind (child pty-stream)
                 (launch-pty-child
                  program arguments directory child-environment columns rows)
               (setf process child
                     stream pty-stream)
               (make-instance
                'pty-device
                :terminal terminal :process process :stream stream
                :mailbox (sb-concurrency:make-mailbox :name "luv PTY commands")
                :lock (sb-thread:make-mutex :name "luv PTY terminal")
                :on-output on-output))))
         (completed-p nil))
    (unwind-protect
         (progn
           (enable-native-pty-echo stream)
           (ghostty:set-terminal-response-function
            terminal
            (lambda (bytes)
              ;; Ghostty has already copied its borrowed callback data.  The
              ;; mailbox transfer keeps the callback quick and non-reentrant.
              (sb-concurrency:send-message
               (pty-device-mailbox device) (list :write bytes))))
           (setf (pty-device-thread device)
                 (sb-thread:make-thread
                  (lambda () (run-pty-device device))
                  :name "luv PTY owner"))
           (setf completed-p t)
           device)
      (unless completed-p
        (ignore-errors (ghostty:set-terminal-response-function terminal nil))
        (when stream (ignore-errors (close stream)))
        (when (and process
                   (eq :running (update-pty-child-status process t)))
          (ignore-errors
            (sb-posix:kill (pty-child-pid process) sb-posix:sigterm))
          (ignore-errors (update-pty-child-status process)))))))

(defun wait-for-pty-device (device &key (timeout 5.0))
  "Wait for DEVICE's owner to finish; return its state or :TIMEOUT."
  (check-type device pty-device)
  (multiple-value-bind (value state)
      (sb-thread:join-thread (pty-device-thread device)
                             :timeout timeout :default :timeout)
    (declare (ignore value))
    (if (eq state :timeout) :timeout (pty-device-state device))))

(defun close-pty-device (device &key (timeout 2.0))
  "Stop DEVICE cooperatively, release its child and descriptor, and join it."
  (check-type device pty-device)
  (let ((state (pty-device-state device)))
    (when (member state '(:starting :running))
      (sb-concurrency:send-message (pty-device-mailbox device) (list :stop)))
    (when (sb-thread:thread-alive-p (pty-device-thread device))
      (when (eq :timeout (wait-for-pty-device device :timeout timeout))
        (error "PTY owner did not stop within ~,2F seconds." timeout)))
    (sb-thread:with-mutex ((pty-device-lock device))
      (unless (eq (%pty-device-state device) :failed)
        (setf (%pty-device-state device) :closed))))
  device)
