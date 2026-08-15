;;;; Lisp entry point run by the Nix-backed ./sly launcher.

(defparameter cl-user::*sly-client-directory*
  (make-pathname :name nil :type nil :defaults *load-truename*))

(load (merge-pathnames
       #P"../parinfer.lisp"
       cl-user::*sly-client-directory*))

(defpackage #:sly-client
  (:use #:cl))

(in-package #:sly-client)

(require :sb-bsd-sockets)

(defparameter *host* (or (sb-ext:posix-getenv "LUV_SLYNK_HOST") "127.0.0.1"))
(defparameter *port*
  (parse-integer (or (sb-ext:posix-getenv "LUV_SLYNK_PORT") "4005")))
(defparameter *project-root*
  (truename
   (merge-pathnames
    #P"../"
    cl-user::*sly-client-directory*)))
(defparameter *server-pid-path*
  (merge-pathnames #P".sly-server.pid" *project-root*))
(defparameter *server-log-path*
  (merge-pathnames #P".sly-server.log" *project-root*))
(defparameter *server-start-timeout* 120)
(defparameter *default-output-limit* (* 256 1024))

(defun attach-only-p ()
  (not (null (sb-ext:posix-getenv "LUV_SLYNK_ATTACH_ONLY"))))

(defstruct output-budget
  limit
  (written 0)
  (truncated-p nil))

(defclass limited-output-stream
    (sb-gray:fundamental-character-output-stream)
  ((target :initarg :target :reader limited-output-target)
   (budget :initarg :budget :reader limited-output-budget)
   (line-column :initform 0 :accessor limited-output-line-column)))

(defun advance-line-column (stream string start end)
  (let ((last-newline (position #\Newline string
                                :start start :end end :from-end t)))
    (if last-newline
        (setf (limited-output-line-column stream)
              (- end last-newline 1))
        (incf (limited-output-line-column stream) (- end start)))))

(defun utf-8-character-length (character)
  (let ((code (char-code character)))
    (cond
      ((<= code #x7f) 1)
      ((<= code #x7ff) 2)
      ((<= code #xffff) 3)
      (t 4))))

(defmethod sb-gray:stream-write-char ((stream limited-output-stream) character)
  (let* ((budget (limited-output-budget stream))
         (width (utf-8-character-length character)))
    (if (<= (+ (output-budget-written budget) width)
            (output-budget-limit budget))
        (progn
          (incf (output-budget-written budget) width)
          (write-char character (limited-output-target stream))
          (if (char= character #\Newline)
              (setf (limited-output-line-column stream) 0)
              (incf (limited-output-line-column stream))))
        (setf (output-budget-truncated-p budget) t)))
  character)

(defmethod sb-gray:stream-write-string
    ((stream limited-output-stream) string &optional (start 0) end)
  (let* ((end (or end (length string)))
         (budget (limited-output-budget stream))
         (remaining (- (output-budget-limit budget)
                       (output-budget-written budget)))
         (cutoff start))
    (loop while (< cutoff end)
          for width = (utf-8-character-length (char string cutoff))
          while (<= width remaining)
          do (decf remaining width)
             (incf cutoff))
    (when (< start cutoff)
      (write-string string (limited-output-target stream)
                    :start start :end cutoff)
      (advance-line-column stream string start cutoff)
      (incf (output-budget-written budget)
            (- (output-budget-limit budget) remaining
               (output-budget-written budget))))
    (when (< cutoff end)
      (setf (output-budget-truncated-p budget) t))
    string))

(defmethod sb-gray:stream-line-column ((stream limited-output-stream))
  (limited-output-line-column stream))

(defmethod sb-gray:stream-force-output ((stream limited-output-stream))
  (force-output (limited-output-target stream)))

(defmethod sb-gray:stream-finish-output ((stream limited-output-stream))
  (finish-output (limited-output-target stream)))

(defmethod sb-gray:stream-clear-output ((stream limited-output-stream))
  (clear-output (limited-output-target stream)))

(defun configured-output-limit ()
  (let ((value (sb-ext:posix-getenv "LUV_SLY_MAX_OUTPUT")))
    (cond
      ((or (null value) (zerop (length value))) *default-output-limit*)
      (t
       (let ((limit (parse-integer value :junk-allowed t)))
         (unless (and limit
                      (every #'digit-char-p value)
                      (not (minusp limit)))
           (error "LUV_SLY_MAX_OUTPUT must be a non-negative byte count"))
         limit)))))

(defun string-octets (string)
  (sb-ext:string-to-octets string :external-format :utf-8))

(defun octets-string (octets)
  (sb-ext:octets-to-string octets :external-format :utf-8))

(defun read-exactly (stream length)
  (let ((octets (make-array length :element-type '(unsigned-byte 8))))
    (loop with position = 0
          while (< position length)
          for end = (read-sequence octets stream :start position)
          when (= end position)
            do (error "Slynk closed the connection")
          do (setf position end))
    octets))

(defun write-packet (stream payload)
  (let ((octets (string-octets payload)))
    (when (> (length octets) #xffffff)
      (error "Slynk packet exceeds its 24-bit length limit"))
    (write-sequence
     (sb-ext:string-to-octets
      (format nil "~6,'0X" (length octets))
      :external-format :ascii)
     stream)
    (write-sequence octets stream)
    (force-output stream)))

(defun read-packet (stream)
  (let* ((header (sb-ext:octets-to-string
                  (read-exactly stream 6)
                  :external-format :ascii))
         (length (parse-integer header :radix 16)))
    (octets-string (read-exactly stream length))))

(defun sly-secret ()
  (with-open-file (stream (merge-pathnames #P".sly-secret"
                                            (user-homedir-pathname))
                          :if-does-not-exist nil)
    (and stream (read-line stream nil ""))))

(defun host-address (host)
  (sb-bsd-sockets:host-ent-address
   (sb-bsd-sockets:get-host-by-name host)))

(defun connection-available-p ()
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream :protocol :tcp)))
    (unwind-protect
         (handler-case
             (progn
               (sb-bsd-sockets:socket-connect
                socket (host-address *host*) *port*)
               t)
           (error () nil))
      (ignore-errors (sb-bsd-sockets:socket-close socket)))))

(defun shell-quote (string)
  (with-output-to-string (output)
    (write-char #\' output)
    (loop for character across string
          do (if (char= character #\')
                 (write-string "'\\''" output)
                 (write-char character output)))
    (write-char #\' output)))

(defun run-shell (command)
  (let ((process
          (sb-ext:run-program
           "/bin/sh" (list "-c" command)
           :search nil
           :output nil
           :error nil
           :wait t)))
    (sb-ext:process-exit-code process)))

(defun pid-file-pid ()
  (with-open-file (stream *server-pid-path* :if-does-not-exist nil)
    (and stream
         (parse-integer (string-trim '(#\Space #\Tab #\Newline #\Return)
                                     (read-line stream nil ""))
                        :junk-allowed t))))

(defun pid-alive-p (pid)
  (and pid
       (zerop
        (run-shell
         (format nil "kill -0 ~D >/dev/null 2>&1" pid)))))

(defun remove-stale-pid-file ()
  (let ((pid (pid-file-pid)))
    (unless (pid-alive-p pid)
      (ignore-errors (delete-file *server-pid-path*)))))

(defun print-server-log-tail (&optional (lines 80))
  (when (probe-file *server-log-path*)
    (format *error-output* "~&--- ~A tail ---~%" *server-log-path*)
    (run-shell
     (format nil "tail -n ~D ~A >&2"
             lines
             (shell-quote (namestring *server-log-path*))))))

(defun start-server (&key quiet)
  (cond
    ((connection-available-p)
     (unless quiet
       (format t "luv Slynk is already listening on ~A:~D.~%" *host* *port*))
     t)
    (t
     (remove-stale-pid-file)
     (let* ((server-path (merge-pathnames #P"sly-server.lisp" *project-root*))
            (command
              (format nil
                      "(cd ~A && exec sbcl --noinform --disable-debugger --load ~A) > ~A 2>&1 & echo $! > ~A"
                      (shell-quote (namestring *project-root*))
                      (shell-quote (namestring server-path))
                      (shell-quote (namestring *server-log-path*))
                      (shell-quote (namestring *server-pid-path*)))))
       (unless (probe-file server-path)
         (error "Missing server bootstrap: ~A" server-path))
       (unless (zerop (run-shell command))
         (error "Could not spawn luv Slynk server"))
       (loop repeat (* 10 *server-start-timeout*)
             when (connection-available-p)
               do (progn
                    (unless quiet
                      (format t "luv Slynk is listening on ~A:~D.~%"
                              *host* *port*))
                    (return-from start-server t))
             unless (pid-alive-p (pid-file-pid))
               do (progn
                    (print-server-log-tail)
                    (error "luv Slynk server exited during startup"))
             do (sleep 0.1))
       (print-server-log-tail)
       (error "Timed out waiting for luv Slynk on ~A:~D" *host* *port*)))))

(defun ensure-server ()
  (unless (connection-available-p)
    (if (attach-only-p)
        (error "The requested external Slynk endpoint is not accepting connections on ~A:~D"
               *host* *port*)
        (start-server :quiet t))))

(defmacro with-slynk-connection ((stream) &body body)
  `(let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                :type :stream :protocol :tcp)))
     (unwind-protect
          (progn
            (sb-bsd-sockets:socket-connect
             socket (host-address *host*) *port*)
            (let ((,stream
                    (sb-bsd-sockets:socket-make-stream
                     socket :input t :output t
                     :element-type '(unsigned-byte 8)
                     :buffering :none)))
              (unwind-protect
                   (progn ,@body)
                (close ,stream))))
       (ignore-errors (sb-bsd-sockets:socket-close socket)))))

(defun eval-request (code package)
  ;; EVAL-AND-GRAB-OUTPUT captures *STANDARD-OUTPUT* itself, but a standalone
  ;; connection has no SLY listener to forward *ERROR-OUTPUT* or *TRACE-OUTPUT*.
  ;; Return those streams explicitly so compiler diagnostics reach this client.
  (format nil
          (concatenate
           'string
           "(:emacs-rex "
           "(cl:let ((diagnostic-output (cl:make-string-output-stream))) "
           "(cl:let ((cl:*error-output* diagnostic-output) "
           "(cl:*trace-output* diagnostic-output)) "
           "(cl:destructuring-bind (output value) "
           "(slynk:eval-and-grab-output ~S) "
           "(cl:list output value "
           "(cl:get-output-stream-string diagnostic-output))))) ~S t 1)")
          code package))

(defun write-diagnostic-output (output)
  (unless (zerop (length output))
    (write-string output *error-output*)
    (unless (char= (char output (1- (length output))) #\Newline)
      (terpri *error-output*))
    (force-output *error-output*)))

(defun query-request (operation package &rest arguments)
  (format nil "(:emacs-rex (~A~{ ~S~}) ~S t 1)"
          operation arguments package))

(defun inspector-request (operation package &rest arguments)
  (format nil
          "(:emacs-rex (slynk:eval-for-inspector nil nil '~A~{ ~S~}) ~S t 1)"
          operation arguments package))

(defun debugger-abort-request (thread package request-id)
  (format nil
          "(:emacs-rex (slynk:throw-to-toplevel) ~S ~S ~D)"
          package thread request-id))

(defun debugger-restart-request (thread level restart package request-id)
  (format nil
          "(:emacs-rex (slynk:invoke-nth-restart-for-emacs ~D ~D) ~S ~S ~D)"
          level restart package thread request-id))

(defun emacs-return-request (thread tag value)
  (format nil "(:emacs-return ~S ~S ~S)" thread tag value))

(defun print-debugger (condition restarts frames)
  (format *error-output* "~&~A~@[~%~A~]~%"
          (first condition) (second condition))
  (loop for (name description) in restarts
        for index from 0
        do (format *error-output* "  ~D: ~A~@[ — ~A~]~%"
                   index name description))
  (format *error-output* "  a: abort evaluation~%")
  (when frames
    (format *error-output* "~%Backtrace:~%")
    (loop for frame in frames
          do (format *error-output* "  ~D: ~A~%"
                     (first frame) (second frame)))))

(defun choose-restart (condition restarts frames)
  (print-debugger condition restarts frames)
  (loop
    (format *error-output* "Restart: ")
    (force-output *error-output*)
    (let ((choice (read-line *standard-input* nil nil)))
      (unless choice
        (format *error-output* "~&No input; aborting evaluation.~%")
        (return nil))
      (cond
        ((member choice '("a" "abort" "q" "quit") :test #'string-equal)
         (return nil))
        (t
         (let ((index (parse-integer choice :junk-allowed t)))
           (if (and index
                    (<= 0 index)
                    (< index (length restarts)))
               (return index)
               (format *error-output* "Please enter a restart number or a.~%"))))))))

(defun new-features-packet-p (payload)
  ;; Slynk sends implementation feature symbols before the return value. Some
  ;; are qualified by packages that do not exist in this tiny standalone
  ;; client, and the feature list is informational here, so do not READ it.
  (let ((prefix "(:new-features"))
    (and (<= (length prefix) (length payload))
         (string-equal prefix payload :end2 (length prefix)))))

(defun read-eval-return (stream package)
  (let ((next-request-id 2))
    (loop
      for payload = (read-packet stream)
      for message = (unless (new-features-packet-p payload)
                      (let ((*read-eval* nil)) (read-from-string payload)))
      do (cond
           ((and (consp message)
                 (eq (first message) :debug))
            (destructuring-bind
                (kind thread level condition restarts frames &rest ignored)
                message
              (declare (ignore kind ignored))
              (let ((restart (choose-restart condition restarts frames)))
                (write-packet
                 stream
                 (if restart
                     (debugger-restart-request
                      thread level restart package next-request-id)
                     (debugger-abort-request thread package next-request-id)))
                (incf next-request-id))))
           ((and (consp message)
                 (eq (first message) :read-from-minibuffer))
            (destructuring-bind (kind thread tag prompt initial-value) message
              (declare (ignore kind))
              (format *error-output* "~A" prompt)
              (when initial-value
                (format *error-output* " [~A]" initial-value))
              (force-output *error-output*)
              (write-packet
               stream
               (emacs-return-request
                thread tag (read-line *standard-input* nil nil)))))
           ((and (consp message)
                 (eq (first message) :y-or-n-p))
            (destructuring-bind (kind thread tag question) message
              (declare (ignore kind))
              (format *error-output* "~A [y/N] " question)
              (force-output *error-output*)
              (let ((answer (read-line *standard-input* nil nil)))
                (write-packet
                 stream
                 (emacs-return-request
                  thread tag
                  (and answer
                       (not (null (member answer '("y" "yes")
                                          :test #'string-equal)))))))))
           ((and (consp message)
                 (eq (first message) :return)
                 (eql (third message) 1))
            (destructuring-bind (status value) (second message)
              (ecase status
                (:ok (return value))
                (:abort
                 (error "Remote evaluation aborted~@[: ~A~]" value)))))))))

(defun authenticate (stream)
  (let ((secret (sly-secret)))
    (when secret
      (write-packet stream secret))))

(defun call-query-on (stream package operation &rest arguments)
  (write-packet stream (apply #'query-request operation package arguments))
  (read-eval-return stream package))

(defun evaluate (code package)
  (with-slynk-connection (stream)
    (authenticate stream)
    (write-packet stream (eval-request code package))
    (destructuring-bind (output value diagnostic-output)
        (read-eval-return stream package)
      (unless (zerop (length output))
        (write-string output)
        (unless (char= (char output (1- (length output))) #\Newline)
          (terpri))
        (force-output))
      (write-diagnostic-output diagnostic-output)
      (write-line value))))

(defun call-inspector (stream package operation &rest arguments)
  (write-packet stream
                (apply #'inspector-request operation package arguments))
  (read-eval-return stream package))

(defun print-inspector-ispec (ispec)
  (etypecase ispec
    (string (write-string ispec))
    (cons
     (case (first ispec)
       (:value
        (destructuring-bind (kind label id) ispec
          (declare (ignore kind))
          (format t "[~D] ~A" id label)))
       (:label
        (write-string (second ispec)))
       (:action
        (destructuring-bind (kind label id) ispec
          (declare (ignore kind))
          (format t "[a~D] ~A" id label)))))))

(defun print-inspector (state)
  (format t "~&~A~%--------------------~%" (getf state :title))
  (destructuring-bind (ispecs length start end) (getf state :content)
    (declare (ignore start))
    (mapc #'print-inspector-ispec ispecs)
    (when (< end length)
      (format t "~&… more inspector parts~%")))
  (fresh-line)
  (force-output))

(defun join-inspector-chunks (first second)
  (destructuring-bind (ispecs-1 length-1 start-1 end-1) first
    (declare (ignore length-1))
    (destructuring-bind (ispecs-2 length-2 start-2 end-2) second
      (unless (= end-1 start-2)
        (error "Non-contiguous inspector chunks: ~D and ~D" end-1 start-2))
      (list (append ispecs-1 ispecs-2) length-2 start-1 end-2))))

(defun fetch-all-inspector-parts (stream package state)
  (destructuring-bind (ispecs length start end) (getf state :content)
    (declare (ignore ispecs start))
    (if (< end length)
        (let ((copy (copy-list state)))
          (setf (getf copy :content)
                (join-inspector-chunks
                 (getf state :content)
                 (call-inspector stream package "slynk:inspector-range"
                                 end most-positive-fixnum)))
          copy)
        state)))

(defun inspector-help ()
  (format t "Commands: NUMBER inspect value, l back, n forward, g refresh,~%")
  (format t "          v verbose, > fetch all, aNUMBER run action,~%")
  (format t "          e FORM evaluate with *=object, q quit~%"))

(defun parse-number (string &optional (start 0))
  (when (< start (length string))
    (multiple-value-bind (number end)
        (parse-integer string :start start :junk-allowed t)
      (and number (= end (length string)) number))))

(defun run-inspector (code package)
  (with-slynk-connection (stream)
    (authenticate stream)
    (let ((state (call-inspector stream package "slynk:init-inspector" code)))
      (loop
        (print-inspector state)
        (format t "Inspect (? for help): ")
        (force-output)
        (let* ((line (read-line *standard-input* nil nil))
               (command (and line
                             (string-trim '(#\Space #\Tab) line))))
          (cond
            ((or (null command)
                 (member command '("q" "quit") :test #'string-equal))
             (return))
            ((string= command "?")
             (inspector-help))
            ((member command '("l" "back") :test #'string-equal)
             (let ((previous
                     (call-inspector stream package "slynk:inspector-pop")))
               (if previous
                   (setf state previous)
                   (format t "No previous object.~%"))))
            ((member command '("n" "next") :test #'string-equal)
             (let ((next
                     (call-inspector stream package "slynk:inspector-next")))
               (if next
                   (setf state next)
                   (format t "No next object.~%"))))
            ((member command '("g" "refresh") :test #'string-equal)
             (setf state
                   (call-inspector stream package "slynk:inspector-reinspect")))
            ((member command '("v" "verbose") :test #'string-equal)
             (setf state
                   (call-inspector stream package
                                   "slynk:inspector-toggle-verbose")))
            ((string= command ">")
             (setf state (fetch-all-inspector-parts stream package state)))
            ((and (> (length command) 2)
                  (char-equal (char command 0) #\e)
                  (char= (char command 1) #\Space))
             (format t "~A~%"
                     (call-inspector stream package "slynk:inspector-eval"
                                     (subseq command 2))))
            ((and (> (length command) 1)
                  (char-equal (char command 0) #\a)
                  (parse-number command 1))
             (let ((new-state
                     (call-inspector
                      stream package "slynk::inspector-call-nth-action"
                      (parse-number command 1))))
               (when new-state
                 (setf state new-state))))
            ((parse-number command)
             (setf state
                   (call-inspector stream package "slynk:inspect-nth-part"
                                   (parse-number command))))
            (t
             (format t "Unknown inspector command. Enter ? for help.~%"))))))))

(defun write-result-string (string)
  (write-string string)
  (unless (or (zerop (length string))
              (char= (char string (1- (length string))) #\Newline))
    (terpri)))

(defun evaluate-captured-output-on (stream code &optional (package "CL-USER"))
  (write-packet stream (eval-request code package))
  (destructuring-bind (output value diagnostic-output)
      (read-eval-return stream package)
    (declare (ignore value))
    (write-diagnostic-output diagnostic-output)
    output))

(defun listener-process-id ()
  (when (connection-available-p)
    (handler-case
        (with-slynk-connection (stream)
          (authenticate stream)
          (parse-integer
           (string-trim
            '(#\Space #\Tab #\Newline #\Return)
            (evaluate-captured-output-on
             stream "(princ (sb-posix:getpid))"))))
      (error () nil))))

(defun stop-server ()
  (when (attach-only-p)
    (let ((listener-pid (listener-process-id)))
      (if listener-pid
          (format t "External Slynk pid ~D is listening on ~A:~D; leaving it running.~%"
                  listener-pid *host* *port*)
          (format t "The external Slynk endpoint is not running on ~A:~D.~%"
                  *host* *port*)))
    (return-from stop-server nil))
  (let ((connection-p (connection-available-p))
        (pid (pid-file-pid))
        (listener-pid (listener-process-id)))
    (cond
      ((and listener-pid (not (eql pid listener-pid)))
       (ignore-errors (delete-file *server-pid-path*))
       (format t
               "luv Slynk pid ~D is owned by Emacs or another process; leaving it running.~%"
               listener-pid))
      ((and connection-p (null listener-pid))
       (format t
               "luv Slynk is listening on ~A:~D, but its owner could not be identified; leaving it running.~%"
               *host* *port*))
      ((not (pid-alive-p pid))
       (ignore-errors (delete-file *server-pid-path*))
       (format t "luv Slynk is not running.~%"))
      (t
       (run-shell (format nil "kill ~D >/dev/null 2>&1" pid))
       (loop repeat 100
             unless (pid-alive-p pid)
               do (progn
                    (ignore-errors (delete-file *server-pid-path*))
                    (format t "Stopped luv Slynk pid ~D.~%" pid)
                    (return-from stop-server t))
             do (sleep 0.1))
       (error "Timed out stopping luv Slynk pid ~D" pid)))))

(defun server-status ()
  (when (attach-only-p)
    (let ((listener-pid (listener-process-id)))
      (if listener-pid
          (format t "External Slynk is listening on ~A:~D (pid ~D).~%"
                  *host* *port* listener-pid)
          (format t "External Slynk is not accepting connections on ~A:~D.~%"
                  *host* *port*)))
    (return-from server-status nil))
  (let ((connection-p (connection-available-p))
        (pid (pid-file-pid))
        (listener-pid (listener-process-id)))
    (cond
      (listener-pid
       (unless (eql pid listener-pid)
         (ignore-errors (delete-file *server-pid-path*)))
       (format t "luv Slynk is listening on ~A:~D (pid ~D, ~A).~%"
               *host* *port* listener-pid
               (if (eql pid listener-pid)
                   "managed by ./sly"
                   "Emacs/external")))
      (connection-p
       (format t "luv Slynk is listening on ~A:~D (owner unavailable).~%"
               *host* *port*))
      ((pid-alive-p pid)
       (format t "luv Slynk pid ~D exists, but ~A:~D is not accepting connections.~%"
               pid *host* *port*))
      (t
       (ignore-errors (delete-file *server-pid-path*))
       (format t "luv Slynk is not running.~%")))))

(defun evaluate-output-on (stream code &optional (package "CL-USER"))
  (write-result-string (evaluate-captured-output-on stream code package)))

(defun package-description-form (name)
  (format nil
          "(let ((package (or (find-package ~S)
                              (find-package (string-upcase ~S)))))
             (unless package
               (error \"No such package: ~~A\" ~S))
             (write
              (list
              :name (package-name package)
              :nicknames (package-nicknames package)
              :documentation (documentation package t)
              :uses (mapcar #'package-name (package-use-list package))
              :used-by (mapcar #'package-name (package-used-by-list package))
              :shadowing (mapcar #'symbol-name
                                 (package-shadowing-symbols package))
              :internal-count
              (loop for symbol being the symbols of package
                    count (eq (nth-value
                               1 (find-symbol (symbol-name symbol) package))
                              :internal))
              :exports
              (sort (loop for symbol being the external-symbols of package
                          collect (symbol-name symbol))
                    #'string<))
              :stream *standard-output* :readably t :length nil :level nil))"
          name name name))

(defun first-line (string)
  (let ((newline (position #\Newline string)))
    (subseq string 0 newline)))

(defun print-name-list (label names)
  (format t "~A: " label)
  (if names
      (format t "~{~A~^, ~}~%" names)
      (format t "(none)~%")))

(defun package-export-kinds (detail)
  (or (loop for (property value) on detail by #'cddr
            when (and value
                      (not (member property
                                   '(:designator :bounds :flex-score
                                     :arglist))))
              collect (string-downcase (property-label property)))
      '("symbol")))

(defun package-export-documentation (detail)
  (loop for (property value) on detail by #'cddr
        when (and (stringp value)
                  (not (member property '(:designator :arglist))))
          return (first-line value)))

(defun print-package-export (package name detail)
  (let ((arglist (and detail (getf detail :arglist)))
        (documentation (and detail
                            (package-export-documentation detail))))
    (format t "  ~A:~A [~{~A~^, ~}]~@[ ~A~]~@[ — ~A~]~%"
            package name (package-export-kinds detail)
            arglist documentation)))

(defun print-package-description (description details)
  (let ((name (getf description :name))
        (exports (getf description :exports)))
    (format t "Package ~A~%" name)
    (print-name-list "Nicknames" (getf description :nicknames))
    (print-name-list "Uses" (getf description :uses))
    (print-name-list "Used by" (getf description :used-by))
    (print-name-list "Shadowing" (getf description :shadowing))
    (when (getf description :documentation)
      (format t "Documentation: ~A~%"
              (first-line (getf description :documentation))))
    (format t "Internal symbols: ~D~%" (getf description :internal-count))
    (format t "Exports (~D):~%" (length exports))
    (dolist (export exports)
      (print-package-export
       name export
       (find export details :test #'string=
             :key (lambda (detail)
                    (first (getf detail :designator))))))))

(defun run-describe-packages (names)
  (with-slynk-connection (stream)
    (authenticate stream)
    (loop for requested-name in names
          for firstp = t then nil
          unless firstp do (terpri)
          do (let* ((description
                      (let ((*read-eval* nil))
                        (read-from-string
                         (evaluate-captured-output-on
                          stream (package-description-form requested-name)))))
                    (name (getf description :name))
                    (details
                      (call-query-on
                       stream "CL-USER"
                       "slynk-apropos:apropos-list-for-emacs"
                       "" t nil name)))
               (print-package-description description details)))))

(defun run-describe-systems (names)
  (with-slynk-connection (stream)
    (authenticate stream)
    (loop for name in names
          for firstp = t then nil
          unless firstp do (terpri)
          do (evaluate-output-on
              stream
              (format nil
                      "(let ((name (string-downcase ~S)))
                         (unless (member name (asdf:already-loaded-systems)
                                         :test #'string=)
                           (error \"ASDF system ~~S is not loaded.\" name))
                         (describe (asdf:find-system name)))"
                      name)))))

(defun run-describe (names package functionp)
  (with-slynk-connection (stream)
    (authenticate stream)
    (loop for name in names
          for firstp = t then nil
          unless firstp do (terpri)
          do (write-result-string
              (call-query-on stream package
                             (if functionp
                                 "slynk:describe-function"
                                 "slynk:describe-symbol")
                             name)))))

(defun apropos-designator (designator)
  (destructuring-bind (name package externalp) designator
    (if package
        (format nil "~A~A~A" package (if externalp ":" "::") name)
        name)))

(defun property-label (property)
  (string-capitalize
   (substitute #\Space #\- (symbol-name property))))

(defun print-apropos-result (result)
  (format t "~A~%" (apropos-designator (getf result :designator)))
  (loop for (property value) on result by #'cddr
        unless (member property '(:designator :bounds :flex-score))
          do (format t "  ~A: ~A~%" (property-label property)
                     (if (eq value :not-documented)
                         "(not documented)"
                         value))))

(defun run-apropos (patterns package external-only case-sensitive)
  (with-slynk-connection (stream)
    (authenticate stream)
    (dolist (pattern patterns)
      (when (cdr patterns)
        (format t "~&Apropos ~S:~%~%" pattern))
      (let ((results
              (call-query-on
               stream (or package "CL-USER")
               "slynk-apropos:apropos-list-for-emacs"
               pattern external-only case-sensitive package)))
        (if results
            (mapc #'print-apropos-result results)
            (format t "No apropos matches for ~S.~%" pattern)))
      (when (cdr patterns)
        (terpri)))))

(defparameter *xref-types*
  '(("calls" . :calls)
    ("calls-who" . :calls-who)
    ("references" . :references)
    ("binds" . :binds)
    ("sets" . :sets)
    ("macroexpands" . :macroexpands)
    ("specializes" . :specializes)
    ("callers" . :callers)
    ("callees" . :callees)))

(defparameter *xref-use-types*
  '(:calls :macroexpands :binds :references :sets :specializes))

(defun location-component (location kind)
  (second (find kind (rest location) :key #'first)))

(defun file-line-and-column (file position)
  (when (and (integerp position) (plusp position))
    (ignore-errors
      (with-open-file (stream file :element-type '(unsigned-byte 8))
        (loop with line = 1
              with column = 1
              repeat (1- position)
              for byte = (read-byte stream nil nil)
              while byte
              if (= byte 10)
                do (setf line (1+ line) column 1)
              else
                do (incf column)
              finally (return (values line column)))))))

(defun format-location (location)
  (if (and (consp location) (eq (first location) :location))
      (let ((file (location-component location :file))
            (buffer (location-component location :buffer))
            (position (location-component location :position)))
        (cond
          (file
           (multiple-value-bind (line column)
               (file-line-and-column file position)
             (if line
                 (format nil "~A:~D:~D" file line column)
                 (format nil "~A~@[@~D~]" file position))))
          (buffer
           (format nil "buffer ~A~@[@~D~]" buffer position))
          (t (format nil "~S" location))))
      (format nil "~S" location)))

(defun first-snippet-line (location)
  (let ((snippet (and (consp location)
                      (location-component location :snippet))))
    (when snippet
      (let* ((trimmed
               (string-trim '(#\Space #\Tab #\Newline #\Return) snippet))
             (newline (position #\Newline trimmed)))
        (subseq trimmed 0 newline)))))

(defun print-xref (xref)
  (destructuring-bind (name location) xref
    (format t "~A~%  ~A~%" name (format-location location))
    (let ((snippet (first-snippet-line location)))
      (when (and snippet (plusp (length snippet)))
        (format t "  ~A~%" snippet)))))

(defun print-xrefs (results)
  (cond
    ((eq results :not-implemented)
     nil)
    (results
     (mapc #'print-xref results)
     t)))

(defun run-xref-for-name (stream type name package)
  (if (string-equal type "uses")
      (let ((found nil))
        (dolist (xref-type *xref-use-types*)
          (let ((results
                  (call-query-on stream package "slynk:xref"
                                 xref-type name)))
            (when (and (not (eq results :not-implemented)) results)
              (setf found t)
              (format t "~&~A:~%" (property-label xref-type))
              (print-xrefs results))))
        (unless found
          (format t "No xrefs for ~A.~%" name)))
      (let* ((entry (assoc type *xref-types* :test #'string-equal))
             (results
               (call-query-on stream package "slynk:xref"
                              (cdr entry) name)))
        (cond
          ((eq results :not-implemented)
           (format t "Xref type ~A is not implemented by this Lisp.~%" type))
          ((null results)
           (format t "No ~A xrefs for ~A.~%" type name))
          (t (print-xrefs results))))))

(defun run-xref (type names package)
  (with-slynk-connection (stream)
    (authenticate stream)
    (dolist (name names)
      (when (cdr names)
        (format t "~&~A:~%~%" name))
      (run-xref-for-name stream type name package)
      (when (cdr names)
        (terpri)))))

(defun run-edit (names package)
  (with-slynk-connection (stream)
    (authenticate stream)
    (dolist (name names)
      (when (cdr names)
        (format t "~&~A:~%~%" name))
      (let ((definitions
              (call-query-on stream package
                             "slynk:find-definitions-for-emacs" name)))
        (cond
          ((null definitions)
           (format t "No definitions for ~A.~%" name))
          ((eq (caar definitions) :error)
           (format t "~A~%" (second (first definitions))))
          (t (print-xrefs definitions))))
      (when (cdr names)
        (terpri)))))

(defun usage (&optional (stream *standard-output*))
  (format stream
          "Prefix a client command with --luvcraft to attach to the running standalone game.~%~%")
  (format stream "Usage: ./sly start|stop|status|log~%")
  (format stream "       ./sly eval CODE [--package PACKAGE]~%")
  (format stream "       ./sly parinfer [--check|--diff|--write] [--strict] [--file FILE|CODE|FILE]~%")
  (format stream "       ./sly inspect CODE [--package PACKAGE]~%")
  (format stream "       ./sly describe NAME... [--function] [--package PACKAGE]~%")
  (format stream "       ./sly describe-package PACKAGE...~%")
  (format stream "       ./sly describe-system SYSTEM...~%")
  (format stream "       ./sly apropos PATTERN... [--package PACKAGE] [--all]~%")
  (format stream "                               [--case-sensitive]~%")
  (format stream "       ./sly edit NAME... [--package PACKAGE]~%")
  (format stream "       ./sly xref TYPE NAME... [--package PACKAGE]~%")
  (format stream "~%Xref types: calls, calls-who, references, binds, sets,~%")
  (format stream "            macroexpands, specializes, callers, callees, uses~%")
  (format stream
          "~%Output is capped at ~D bytes; set LUV_SLY_MAX_OUTPUT=0 for unlimited.~%"
          *default-output-limit*))

(defun read-standard-input-to-end ()
  (with-output-to-string (output)
    (loop for character = (read-char *standard-input* nil nil)
          while character
          do (write-char character output))))

(defun read-text-file (pathname)
  (with-open-file (stream pathname :direction :input)
    (with-output-to-string (output)
      (loop for character = (read-char stream nil nil)
            while character
            do (write-char character output)))))

(defun write-text-file (pathname text)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string text stream)))

(defun temporary-parinfer-pathname (label)
  (make-pathname
   :name (format nil "luv-parinfer-~A-~D-~D"
                 label (get-universal-time) (random 1000000))
   :type "lisp"
   :defaults #P"/tmp/"))

(defun call-diff (left-label left-path right-label right-path)
  (let ((process
          (sb-ext:run-program
           "diff"
           (list "-u" "--label" left-label "--label" right-label
                 (namestring left-path) (namestring right-path))
           :search t
           :output *standard-output*
           :error *error-output*
           :wait t)))
    (sb-ext:process-exit-code process)))

(defun print-parinfer-diff (source repaired label)
  (let ((source-path (temporary-parinfer-pathname "source"))
        (repaired-path (temporary-parinfer-pathname "repaired")))
    (unwind-protect
         (progn
           (write-text-file source-path source)
           (write-text-file repaired-path repaired)
           (call-diff label source-path "parinfer" repaired-path))
      (ignore-errors (delete-file source-path))
      (ignore-errors (delete-file repaired-path)))))

(defun parse-parinfer-arguments (arguments)
  (let ((mode :print)
        (source nil)
        (file nil)
        (strict nil))
    (loop while arguments
          for argument = (pop arguments)
          do (cond
               ((string= argument "--check")
                (setf mode :check))
               ((string= argument "--diff")
                (setf mode :diff))
               ((string= argument "--write")
                (setf mode :write))
               ((string= argument "--strict")
                (setf strict t))
               ((string= argument "--file")
                (unless arguments
                  (error "--file requires a pathname"))
                (setf file (pop arguments)))
               (source
                (error "parinfer accepts at most one source argument"))
               (t
                (setf source argument))))
    (when (and file source)
      (error "parinfer accepts either --file or a source argument, not both"))
    (when (and (eq mode :write) (not (or file
                                         (and source (probe-file source)))))
      (error "parinfer --write requires a file path"))
    (when file
      (setf file (namestring (truename file))))
    (when (and (not file) source (probe-file source))
      (setf file (namestring (truename source))
            source nil))
    (values mode source file strict)))

(defun parinfer-safe-repair-p (report)
  (and (not (sly-client/parinfer:indent-mode-report-source-balanced-p report))
       (sly-client/parinfer:indent-mode-report-candidate-balanced-p report)
       (sly-client/parinfer:indent-mode-report-candidate-changed-p report)))

(defun parinfer-balanced-conflict-p (report)
  (and (sly-client/parinfer:indent-mode-report-source-balanced-p report)
       (sly-client/parinfer:indent-mode-report-candidate-changed-p report)))

(defun explain-parinfer-report (report label stream)
  (cond
    ((parinfer-balanced-conflict-p report)
     (format stream
             "parinfer: ~A is paren-balanced, but indentation suggests a different tree; use --diff to inspect the candidate.~%"
             label))
    ((parinfer-safe-repair-p report)
     (format stream
             "parinfer: ~A has a validated indentation repair candidate; use --diff or --write.~%"
             label))
    ((not (sly-client/parinfer:indent-mode-report-source-balanced-p report))
     (format stream
             "parinfer: ~A is not paren-balanced, and no validated repair candidate was found.~%"
             label))
    (t
     (format stream "parinfer: ~A unchanged.~%" label))))

(defun run-parinfer (arguments)
  (multiple-value-bind (mode source file strict)
      (parse-parinfer-arguments arguments)
    (let* ((label (or file "stdin"))
           (original
             (cond (file (read-text-file file))
                   (source source)
                   (t (read-standard-input-to-end))))
           (report
             (sly-client/parinfer:analyze-indent-mode original))
           (candidate
             (sly-client/parinfer:indent-mode-report-candidate report))
           (repaired
             (sly-client/parinfer:apply-indent-mode original))
           (changed-p (not (string= original repaired)))
           (candidate-changed-p
             (sly-client/parinfer:indent-mode-report-candidate-changed-p
              report))
           (source-balanced-p
             (sly-client/parinfer:indent-mode-report-source-balanced-p
              report)))
      (ecase mode
        (:print
         (write-string repaired)
         0)
        (:check
         (cond
           ((or changed-p (not source-balanced-p))
            (explain-parinfer-report report label *error-output*)
            1)
           ((and strict candidate-changed-p)
            (explain-parinfer-report report label *error-output*)
            1)
           (t
            (format t "parinfer: ~A unchanged.~%" label)
            0)))
        (:diff
         (cond
           (candidate-changed-p
            (when (parinfer-balanced-conflict-p report)
              (format *error-output*
                      "parinfer: ~A is paren-balanced; showing indentation candidate, not a safe rewrite.~%"
                      label))
            (print-parinfer-diff original candidate label))
           ((not source-balanced-p)
            (explain-parinfer-report report label *error-output*)
            1)
           (t
            (format t "parinfer: ~A unchanged.~%" label)
            0)))
        (:write
         (cond
           (changed-p
            (write-text-file file repaired)
            (format t "parinfer: rewrote ~A.~%" file)
            0)
           ((parinfer-balanced-conflict-p report)
            (format *error-output*
                    "parinfer: ~A is paren-balanced, but indentation suggests a different tree; refusing --write. Use --diff to inspect it.~%"
                    file)
            1)
           ((not source-balanced-p)
            (explain-parinfer-report report file *error-output*)
            1)
           (t
            (format t "parinfer: ~A unchanged.~%" file)
            0)))))))

(defun parse-code-arguments (command arguments)
  (unless arguments
    (error "~A requires Common Lisp source code" command))
  (let ((code (pop arguments))
        (package "CL-USER"))
    (loop while arguments
          for option = (pop arguments)
          do (cond
               ((string= option "--package")
                (unless arguments
                  (error "--package requires a package name"))
                (setf package (pop arguments)))
               (t (error "Unknown option: ~A" option))))
    (values code package)))

(defun parse-names (command arguments)
  (unless arguments
    (error "~A requires at least one name" command))
  arguments)

(defun parse-describe-arguments (arguments)
  (unless arguments
    (error "describe requires a symbol name"))
  (let ((names nil)
        (package "CL-USER")
        (functionp nil))
    (loop while arguments
          for argument = (pop arguments)
          do (cond
               ((string= argument "--package")
                (unless arguments
                  (error "--package requires a package name"))
                (setf package (pop arguments)))
               ((string= argument "--function")
                (setf functionp t))
               ((and (> (length argument) 1)
                     (string= argument "--" :end1 2))
                (error "Unknown describe option: ~A" argument))
               (t (push argument names))))
    (unless names
      (error "describe requires a symbol name"))
    (values (nreverse names) package functionp)))

(defun parse-apropos-arguments (arguments)
  (unless arguments
    (error "apropos requires a search pattern"))
  (let ((patterns nil)
        (package nil)
        (external-only t)
        (case-sensitive nil))
    (loop while arguments
          for argument = (pop arguments)
          do (cond
               ((string= argument "--package")
                (unless arguments
                  (error "--package requires a package name"))
                (setf package (pop arguments)))
               ((string= argument "--external-only")
                (setf external-only t))
               ((string= argument "--all")
                (setf external-only nil))
               ((string= argument "--case-sensitive")
                (setf case-sensitive t))
               ((and (> (length argument) 1)
                     (string= argument "--" :end1 2))
                (error "Unknown apropos option: ~A" argument))
               (t (push argument patterns))))
    (unless patterns
      (error "apropos requires a search pattern"))
    (values (nreverse patterns) package external-only case-sensitive)))

(defun parse-edit-arguments (arguments)
  (unless arguments
    (error "edit requires a symbol name"))
  (let ((names nil)
        (package "CL-USER"))
    (loop while arguments
          for argument = (pop arguments)
          do (cond
               ((string= argument "--package")
                (unless arguments
                  (error "--package requires a package name"))
                (setf package (pop arguments)))
               ((and (> (length argument) 1)
                     (string= argument "--" :end1 2))
                (error "Unknown edit option: ~A" argument))
               (t (push argument names))))
    (unless names
      (error "edit requires a symbol name"))
    (values (nreverse names) package)))

(defun parse-xref-arguments (arguments)
  (unless (cdr arguments)
    (error "xref requires a type and symbol name"))
  (let ((type (pop arguments))
        (names nil)
        (package "CL-USER"))
    (unless (or (string-equal type "uses")
                (assoc type *xref-types* :test #'string-equal))
      (error "Unknown xref type: ~A" type))
    (loop while arguments
          for argument = (pop arguments)
          do (cond
               ((string= argument "--package")
                (unless arguments
                  (error "--package requires a package name"))
                (setf package (pop arguments)))
               ((and (> (length argument) 1)
                     (string= argument "--" :end1 2))
                (error "Unknown xref option: ~A" argument))
               (t (push argument names))))
    (unless names
      (error "xref requires a type and symbol name"))
    (values type (nreverse names) package)))

(defun main (arguments)
  (unless arguments
    (usage *error-output*)
    (return-from main 2))
  (let ((command (pop arguments)))
    (cond
      ((member command '("-h" "--help") :test #'string=)
       (usage)
       0)
      ((string= command "start")
       (when arguments
         (error "start does not accept arguments"))
       (start-server)
       0)
      ((string= command "stop")
       (when arguments
         (error "stop does not accept arguments"))
       (stop-server)
       0)
      ((string= command "status")
       (when arguments
         (error "status does not accept arguments"))
       (server-status)
       0)
      ((string= command "log")
       (when arguments
         (error "log does not accept arguments"))
       (print-server-log-tail)
       0)
      ((string= command "eval")
       (ensure-server)
       (multiple-value-bind (code package)
           (parse-code-arguments command arguments)
         (evaluate code package))
       0)
      ((string= command "parinfer")
       (run-parinfer arguments))
      ((string= command "inspect")
       (ensure-server)
       (multiple-value-bind (code package)
           (parse-code-arguments command arguments)
         (run-inspector code package))
       0)
      ((string= command "describe")
       (ensure-server)
       (multiple-value-bind (names package functionp)
           (parse-describe-arguments arguments)
         (run-describe names package functionp))
       0)
      ((string= command "describe-package")
       (ensure-server)
       (run-describe-packages (parse-names command arguments))
       0)
      ((string= command "describe-system")
       (ensure-server)
       (run-describe-systems (parse-names command arguments))
       0)
      ((string= command "apropos")
       (ensure-server)
       (multiple-value-bind (patterns package external-only case-sensitive)
           (parse-apropos-arguments arguments)
         (run-apropos patterns package external-only case-sensitive))
       0)
      ((member command '("edit" "definition") :test #'string=)
       (ensure-server)
       (multiple-value-bind (names package)
           (parse-edit-arguments arguments)
         (run-edit names package))
       0)
      ((string= command "xref")
       (ensure-server)
       (multiple-value-bind (type names package)
           (parse-xref-arguments arguments)
         (run-xref type names package))
       0)
      (t
       (error "Unknown command: ~A" command)))))

(let* ((original-output *standard-output*)
       (original-error *error-output*)
       (limit (handler-case
                  (configured-output-limit)
                (error (condition)
                  (format original-error "sly: ~A~%" condition)
                  (sb-ext:exit :code 1))))
       (budget (and (plusp limit)
                    (make-output-budget :limit limit)))
       (exit-code
         (let ((*standard-output*
                 (if budget
                     (make-instance 'limited-output-stream
                                    :target original-output
                                    :budget budget)
                     original-output))
               (*error-output*
                 (if budget
                     (make-instance 'limited-output-stream
                                    :target original-error
                                    :budget budget)
                     original-error)))
           (handler-case
               (main (cdr sb-ext:*posix-argv*))
             (error (condition)
               (format *error-output* "sly: ~A~%" condition)
               1)))))
  (when (and budget (output-budget-truncated-p budget))
    (force-output original-output)
    (format original-error
            "~&[sly output truncated after ~D bytes; set LUV_SLY_MAX_OUTPUT=0 for unlimited]~%"
            limit)
    (force-output original-error))
  (sb-ext:exit :code exit-code))
