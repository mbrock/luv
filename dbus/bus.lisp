;;;; Connections: authentication, calls and their replies, and handlers.

(in-package #:luv.dbus)

(defclass bus ()
  ((address :initarg :address :reader bus-address)
   (socket :initarg :socket :reader bus-socket)
   (stream :initarg :stream :reader bus-stream)
   (unique-name :initform nil :accessor bus-unique-name)
   (open-p :initform t :accessor bus-open-p)
   (serial :initform 0 :accessor bus-serial)
   (write-lock :initform (sb-thread:make-mutex :name "D-Bus write") :reader bus-write-lock)
   (lock :initform (sb-thread:make-mutex :name "D-Bus state") :reader bus-lock)
   (replied :initform (sb-thread:make-waitqueue :name "D-Bus replies") :reader bus-replied)
   (replies :initform (make-hash-table) :reader bus-replies
            :documentation "Serial of each call awaiting its reply -> the reply, or :WAITING.")
   (inbox :initform '() :accessor bus-inbox
          :documentation "Method calls and signals for the dispatcher, newest first.")
   (arrived :initform (sb-thread:make-waitqueue :name "D-Bus inbox") :reader bus-arrived)
   (handlers :initform '() :accessor bus-handlers)
   (failures :initform '() :accessor bus-failures
             :documentation "Errors handlers raised, newest first, at most 32.")
   (reader-thread :initform nil :accessor bus-reader-thread)
   (dispatch-thread :initform nil :accessor bus-dispatch-thread)))

(defmethod print-object ((bus bus) stream)
  (print-unreadable-object (bus stream :type t)
    (format stream "~A ~:[closed~;~:*~A~]" (bus-address bus)
            (and (bus-open-p bus) (bus-unique-name bus)))))

;;; Addresses.

(defun session-bus-address ()
  "The session bus's address: $DBUS_SESSION_BUS_ADDRESS, or the systemd user
bus in $XDG_RUNTIME_DIR if its socket exists."
  (or (uiop:getenv "DBUS_SESSION_BUS_ADDRESS")
      (let* ((runtime (or (uiop:getenv "XDG_RUNTIME_DIR")
                          (format nil "/run/user/~D" (sb-posix:getuid))))
             (path (format nil "~A/bus" runtime)))
        (and (probe-file path) (format nil "unix:path=~A" path)))))

(defun system-bus-address ()
  (or (uiop:getenv "DBUS_SYSTEM_BUS_ADDRESS")
      "unix:path=/run/dbus/system_bus_socket"))

(defun parse-address (address)
  "The first unix transport in ADDRESS as (:path p) or (:abstract name)."
  (dolist (transport (uiop:split-string address :separator ";"))
    (let ((colon (position #\: transport)))
      (when (and colon (string= "unix" (subseq transport 0 colon)))
        (let ((pairs (loop for pair in (uiop:split-string (subseq transport (1+ colon))
                                                          :separator ",")
                           for equals = (position #\= pair)
                           when equals
                             collect (cons (subseq pair 0 equals)
                                           (unescape-address (subseq pair (1+ equals)))))))
          (cond ((assoc "path" pairs :test #'string=)
                 (return (list :path (cdr (assoc "path" pairs :test #'string=)))))
                ((assoc "abstract" pairs :test #'string=)
                 (return (list :abstract (cdr (assoc "abstract" pairs :test #'string=)))))))))))

(defun unescape-address (value)
  (with-output-to-string (out)
    (loop with i = 0
          while (< i (length value))
          do (if (and (char= (char value i) #\%) (< (+ i 2) (length value)))
                 (progn (write-char (code-char (parse-integer value :start (1+ i) :end (+ i 3)
                                                                    :radix 16))
                                    out)
                        (incf i 3))
                 (progn (write-char (char value i) out) (incf i))))))

;;; Opening: connect, authenticate as our uid, and say hello.

(defun write-line-crlf (stream line)
  (write-sequence (sb-ext:string-to-octets (format nil "~A~C~C" line #\Return #\Newline)
                                           :external-format :latin-1)
                  stream)
  (finish-output stream))

(defun read-line-crlf (stream)
  (let ((octets (loop for byte = (read-byte stream nil nil)
                      until (or (null byte) (= byte 10))
                      unless (= byte 13) collect byte)))
    (map 'string #'code-char octets)))

(defun authenticate (stream)
  (write-byte 0 stream)
  (write-line-crlf stream (format nil "AUTH EXTERNAL ~{~(~2,'0x~)~}"
                                  (map 'list #'char-code (princ-to-string (sb-posix:getuid)))))
  (let ((answer (read-line-crlf stream)))
    (unless (uiop:string-prefix-p "OK " answer)
      (error 'dbus-error :name "org.freedesktop.DBus.Error.AuthFailed" :message answer)))
  (write-line-crlf stream "BEGIN"))

(defun open-bus (&optional (address (session-bus-address)))
  "Connect to the bus at ADDRESS, :SESSION, or :SYSTEM, and register on it."
  (let* ((address (case address
                    (:session (or (session-bus-address)
                                  (error 'dbus-error :name "luv.NoSessionBus"
                                                     :message "no session bus address")))
                    (:system (system-bus-address))
                    (t address)))
         (target (or (parse-address address)
                     (error 'dbus-error :name "luv.UnsupportedAddress" :message address)))
         (socket (if (eq (first target) :abstract)
                     (make-instance 'sb-bsd-sockets:local-abstract-socket :type :stream)
                     (make-instance 'sb-bsd-sockets:local-socket :type :stream))))
    (handler-bind ((error (lambda (condition)
                            (declare (ignore condition))
                            (ignore-errors (sb-bsd-sockets:socket-close socket)))))
      (sb-bsd-sockets:socket-connect socket (second target))
      (let* ((stream (sb-bsd-sockets:socket-make-stream socket :input t :output t
                                                               :element-type '(unsigned-byte 8)
                                                               :buffering :full))
             (bus (make-instance 'bus :address address :socket socket :stream stream)))
        (authenticate stream)
        (setf (bus-reader-thread bus)
              (sb-thread:make-thread #'read-messages :name "D-Bus reader" :arguments (list bus))
              (bus-dispatch-thread bus)
              (sb-thread:make-thread #'dispatch-messages :name "D-Bus dispatch"
                                                         :arguments (list bus)))
        (setf (bus-unique-name bus)
              (first (call bus "org.freedesktop.DBus" "/org/freedesktop/DBus"
                                  "org.freedesktop.DBus" "Hello")))
        bus))))

(defun close-bus (bus)
  (when (bus-open-p bus)
    (mark-closed bus)
    ;; Closing alone leaves the connection up while the reader is blocked in
    ;; read(2) on it; shutting it down ends that read and tells the daemon.
    (ignore-errors (sb-bsd-sockets:socket-shutdown (bus-socket bus) :direction :io))
    (ignore-errors (sb-bsd-sockets:socket-close (bus-socket bus))))
  (dolist (thread (list (bus-reader-thread bus) (bus-dispatch-thread bus)))
    (when (and thread (not (eq thread sb-thread:*current-thread*)))
      (ignore-errors (sb-thread:join-thread thread :default nil :timeout 2))))
  bus)

(defun mark-closed (bus)
  (sb-thread:with-mutex ((bus-lock bus))
    (setf (bus-open-p bus) nil)
    (sb-thread:condition-broadcast (bus-replied bus))
    (sb-thread:condition-broadcast (bus-arrived bus))))

;;; Sending.

(defun %send (bus message)
  "Give MESSAGE the next serial and write it; return the serial.  The
caller holds the write lock."
  (unless (bus-open-p bus)
    (error 'dbus-error :name "org.freedesktop.DBus.Error.Disconnected"
                       :message "the connection is closed"))
  (setf (message-serial message) (incf (bus-serial bus)))
  (write-sequence (encode-message message) (bus-stream bus))
  (finish-output (bus-stream bus))
  (message-serial message))

(defun send (bus message)
  (sb-thread:with-mutex ((bus-write-lock bus))
    (%send bus message)))

(defun call (bus destination path interface member
                    &key (signature "") arguments (timeout 10))
  "Call MEMBER and return the reply's values as a list; a D-Bus error reply
signals DBUS-ERROR.  Safe to call from handlers, never from the reader."
  (let ((message (make-message :type :method-call :destination destination :path path
                               :interface interface :member member
                               :signature signature :body arguments)))
    (let ((sent (sb-thread:with-mutex ((bus-write-lock bus))
                  ;; Wait for the reply before it can arrive.
                  (sb-thread:with-mutex ((bus-lock bus))
                    (setf (gethash (1+ (bus-serial bus)) (bus-replies bus)) :waiting))
                  (%send bus message)))
          (deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
      (let ((reply (sb-thread:with-mutex ((bus-lock bus))
                       (loop for reply = (gethash sent (bus-replies bus))
                             until (or (not (eq reply :waiting))
                                       (not (bus-open-p bus))
                                       (>= (get-internal-real-time) deadline))
                             do (sb-thread:condition-wait
                                 (bus-replied bus) (bus-lock bus)
                                 :timeout (max 0.01 (/ (- deadline (get-internal-real-time))
                                                       internal-time-units-per-second)))
                             finally (remhash sent (bus-replies bus))
                                     (return reply)))))
          (cond ((message-p reply)
                 (if (eq (message-type reply) :error)
                     (error 'dbus-error :name (message-error-name reply)
                                        :message (let ((text (first (message-body reply))))
                                                   (and (stringp text) text)))
                     (message-body reply)))
                ((not (bus-open-p bus))
                 (error 'dbus-error :name "org.freedesktop.DBus.Error.Disconnected"
                                    :message "the connection closed"))
                (t (error 'dbus-error :name "org.freedesktop.DBus.Error.NoReply"
                                      :message (format nil "~A.~A gave no reply in ~Ds"
                                                       interface member timeout))))))))

(defun emit-signal (bus path interface member &key (signature "") arguments destination)
  (send bus (make-message :type :signal :path path :interface interface :member member
                          :destination destination :signature signature :body arguments)))

(defun reply (bus call &key (signature "") arguments)
  "Answer CALL, a method-call message, with ARGUMENTS of SIGNATURE."
  (unless (logbitp 0 (message-flags call))  ; NO_REPLY_EXPECTED
    (send bus (make-message :type :method-return :reply-serial (message-serial call)
                            :destination (message-sender call)
                            :signature signature :body arguments))))

(defun reply-error (bus call name &optional (text ""))
  (unless (logbitp 0 (message-flags call))
    (send bus (make-message :type :error :reply-serial (message-serial call)
                            :destination (message-sender call) :error-name name
                            :signature "s" :body (list text)))))

;;; Receiving.  The reader hands replies to waiting callers and everything
;;; else to the dispatcher, so a handler may call methods synchronously.

(defun read-message (stream)
  (let ((prefix (make-array 16 :element-type '(unsigned-byte 8))))
    (when (< (read-sequence prefix stream) 16)
      (return-from read-message nil))
    (let ((octets (make-array (message-length prefix) :element-type '(unsigned-byte 8))))
      (replace octets prefix)
      (when (< (read-sequence octets stream :start 16) (length octets))
        (return-from read-message nil))
      (decode-message octets))))

(defun read-messages (bus)
  (unwind-protect
       (loop for message = (handler-case (read-message (bus-stream bus))
                             (error () nil))
             while message
             do (sb-thread:with-mutex ((bus-lock bus))
                  (if (and (member (message-type message) '(:method-return :error))
                           (gethash (message-reply-serial message) (bus-replies bus)))
                      (progn
                        (setf (gethash (message-reply-serial message) (bus-replies bus)) message)
                        (sb-thread:condition-broadcast (bus-replied bus)))
                      (unless (member (message-type message) '(:method-return :error))
                        (push message (bus-inbox bus))
                        (sb-thread:condition-broadcast (bus-arrived bus))))))
    (mark-closed bus)))

(defun add-handler (bus function)
  "Call FUNCTION with BUS and each incoming method call or signal, on the
dispatch thread.  For a method call, FUNCTION returns true when it replied;
when no handler does, the caller gets UnknownMethod."
  (sb-thread:with-mutex ((bus-lock bus))
    (setf (bus-handlers bus) (append (bus-handlers bus) (list function))))
  function)

(defun remove-handler (bus function)
  (sb-thread:with-mutex ((bus-lock bus))
    (setf (bus-handlers bus) (remove function (bus-handlers bus)))))

(defun note-failure (bus condition message)
  (sb-thread:with-mutex ((bus-lock bus))
    (push (list message condition) (bus-failures bus))
    (when (> (length (bus-failures bus)) 32)
      (setf (bus-failures bus) (subseq (bus-failures bus) 0 32)))))

(defun dispatch-messages (bus)
  (loop
    (let ((batch (sb-thread:with-mutex ((bus-lock bus))
                   (loop while (and (bus-open-p bus) (null (bus-inbox bus)))
                         do (sb-thread:condition-wait (bus-arrived bus) (bus-lock bus)))
                   (if (and (null (bus-inbox bus)) (not (bus-open-p bus)))
                       (return)
                       (reverse (shiftf (bus-inbox bus) '())))))
          (handlers (sb-thread:with-mutex ((bus-lock bus)) (bus-handlers bus))))
      (dolist (message batch)
        (let ((handled (loop for handler in handlers
                             thereis (handler-case (funcall handler bus message)
                                       (dbus-error (condition)
                                         (when (eq (message-type message) :method-call)
                                           (ignore-errors
                                            (reply-error bus message (dbus-error-name condition)
                                                         (or (dbus-error-message condition) ""))))
                                         t)
                                       (error (condition)
                                         (note-failure bus condition message)
                                         (when (eq (message-type message) :method-call)
                                           (ignore-errors
                                            (reply-error bus message "org.freedesktop.DBus.Error.Failed"
                                                         (princ-to-string condition))))
                                         t)))))
          (when (and (not handled) (eq (message-type message) :method-call))
            (ignore-errors
             (reply-error bus message "org.freedesktop.DBus.Error.UnknownMethod"
                          (format nil "No method ~A.~A on ~A" (message-interface message)
                                  (message-member message) (message-path message))))))))))

;;; The bus daemon and properties.

(defun bus-daemon-call (bus member &key (signature "") arguments)
  (call bus "org.freedesktop.DBus" "/org/freedesktop/DBus" "org.freedesktop.DBus"
               member :signature signature :arguments arguments))

(defun add-match (bus rule)
  "Ask the daemon to route messages matching RULE, such as
\"type='signal',interface='org.kde.StatusNotifierItem'\", to BUS."
  (bus-daemon-call bus "AddMatch" :signature "s" :arguments (list rule)))

(defun request-name (bus name &key (flags 4))
  "Ask for NAME; FLAGS default to DO_NOT_QUEUE.  Return :PRIMARY-OWNER,
:IN-QUEUE, :EXISTS, or :ALREADY-OWNER."
  (nth (1- (first (bus-daemon-call bus "RequestName" :signature "su"
                                                     :arguments (list name flags))))
       '(:primary-owner :in-queue :exists :already-owner)))

(defun get-property (bus destination path interface name)
  (first (call bus destination path "org.freedesktop.DBus.Properties" "Get"
                      :signature "ss" :arguments (list interface name))))

(defun get-all-properties (bus destination path interface)
  "INTERFACE's properties on PATH as a list of (name . value)."
  (first (call bus destination path "org.freedesktop.DBus.Properties" "GetAll"
                      :signature "s" :arguments (list interface))))
