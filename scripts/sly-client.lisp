;;;; Lisp entry point run by the Nix-backed ./sly launcher.

(defparameter cl-user::*sly-client-directory*
  (make-pathname :name nil :type nil :defaults *load-truename*))

(load (merge-pathnames
       #P"../parinfer/implementation.lisp"
       cl-user::*sly-client-directory*))

(defpackage #:sly-client
  (:use #:cl))

(in-package #:sly-client)

(require :sb-bsd-sockets)
(require :sb-posix)

(defparameter *host* (or (sb-ext:posix-getenv "LUV_SLYNK_HOST") "127.0.0.1"))
(defparameter *port*
  (parse-integer (or (sb-ext:posix-getenv "LUV_SLYNK_PORT") "4005")))
(defparameter *expected-listener-pid*
  (let ((value (sb-ext:posix-getenv "LUV_SLYNK_PID")))
    (and value (parse-integer value :junk-allowed t))))
(defparameter *project-root*
  (truename
   (merge-pathnames
    #P"../"
    cl-user::*sly-client-directory*)))
(defparameter *server-pid-path*
  (merge-pathnames #P".sly-server.pid" *project-root*))
(defparameter *server-log-path*
  (merge-pathnames #P".sly-server.log" *project-root*))
(defparameter *server-start-lock-path*
  (merge-pathnames #P".sly-server.start.lock" *project-root*))
(defparameter *server-start-timeout* 120)
(defparameter *slynk-handshake-timeout* 3)
(defparameter *default-output-limit* (* 256 1024))

(define-condition slynk-handshake-timeout (error) ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream
             "Timed out after ~D seconds waiting for a Slynk handshake on ~A:~D. The port accepts TCP but may be occupied by a non-Slynk process."
             *slynk-handshake-timeout* *host* *port*))))

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

(defparameter *slynk-silence-notices* '(5 15 30 60)
  "Seconds of silence at which ./sly reports that the image has not answered.

After the last entry the notice repeats at that interval.  A request can take
minutes legitimately -- loading a system, meshing a world -- so waiting is not
itself an error, but waiting without a word is: silence is exactly what a
wedged image looks like, and the difference has to be visible from here.")

(defun report-slynk-silence (seconds)
  (format *error-output*
          "~&sly: no answer from ~A:~D after ~D s. It may simply be busy; ~
`./sly status` in another shell says whether its canvas is stalled.~%"
          *host* *port* seconds)
  (force-output *error-output*))

(defun await-slynk-packet (stream)
  "Wait until STREAM has something to say, reporting the silence if it lasts."
  (let* ((descriptor (sb-sys:fd-stream-fd stream))
         (interval (or (car (last *slynk-silence-notices*)) 60))
         (pending (rest *slynk-silence-notices*))
         (waited 0)
         (next (or (first *slynk-silence-notices*) interval)))
    (loop until (listen stream)
          do (when (sb-sys:wait-until-fd-usable
                    descriptor :input (max 1 (- next waited)))
               (return))
             (setf waited next)
             (report-slynk-silence waited)
             (setf next (if pending (first pending) (+ waited interval))
                   pending (rest pending)))))

(defun read-packet (stream)
  (await-slynk-packet stream)
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

(defun slynk-handshake-timeout-error ()
  (error 'slynk-handshake-timeout))

(defun connect-slynk-socket (socket)
  (handler-case
      (sb-ext:with-timeout *slynk-handshake-timeout*
        (sb-bsd-sockets:socket-connect
         socket (host-address *host*) *port*))
    (sb-ext:timeout ()
      (slynk-handshake-timeout-error))))

(defun connection-available-p ()
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream :protocol :tcp)))
    (unwind-protect
         (handler-case
             (progn
               (connect-slynk-socket socket)
               t)
           (slynk-handshake-timeout (condition)
             (error condition))
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
  "Run COMMAND under /bin/sh, letting it speak, and return its exit code.

The streams are inherited rather than discarded.  SB-EXT:RUN-PROGRAM reads
NIL as /dev/null, not as \"inherit\", so a command told to print swallowed
its own output here: `./sly log` shelled out to tail and threw the tail
away, leaving a header with nothing under it.  Every caller that wants
silence redirects for itself."
  (let ((process
          (sb-ext:run-program
           "/bin/sh" (list "-c" command)
           :search nil
           :output t
           :error t
           :wait t)))
    (sb-ext:process-exit-code process)))

(defun run-shell-output (command)
  "Run COMMAND under /bin/sh and return its standard output as a string."
  (with-output-to-string (output)
    (sb-ext:run-program
     "/bin/sh" (list "-c" command)
     :search nil
     :output output
     :error nil
     :wait t)))

(defun split-lines (text)
  (loop with start = 0
        for newline = (position #\Newline text :start start)
        collect (string-right-trim '(#\Return) (subseq text start newline))
        while newline
        do (setf start (1+ newline))))

(defstruct (port-holder (:constructor make-port-holder (pid command)))
  pid
  command)

(defun port-holders (&optional (port *port*))
  "The processes holding a listening socket on PORT, as lsof reports them.

A dead image's shell can outlive it still holding the inherited listening
socket, which is exactly the state where the port accepts TCP and answers
nothing."
  (let ((pid nil)
        (holders nil))
    (dolist (line (split-lines
                   (run-shell-output
                    (format nil "lsof -nP -iTCP:~D -sTCP:LISTEN -Fpc 2>/dev/null"
                            port)))
             (nreverse holders))
      (when (plusp (length line))
        (case (char line 0)
          (#\p (setf pid (parse-integer (subseq line 1) :junk-allowed t)))
          (#\c (when pid
                 (push (make-port-holder pid (subseq line 1)) holders)
                 (setf pid nil))))))))

(defun lisp-holder-p (holder)
  "True when HOLDER looks like a Lisp image rather than a leaked descriptor.

./sly reclaims a squatted port by killing what holds it, and a Lisp is the
one kind of holder that might be somebody's live image: it is reported, never
killed."
  (let ((command (string-downcase (port-holder-command holder))))
    (or (search "sbcl" command)
        (search "lisp" command)
        (search "emacs" command)
        (search "luvcraft" command))))

(defun describe-port-holders (holders &optional (stream *error-output*))
  (dolist (holder holders)
    (format stream "  pid ~D ~A~%"
            (port-holder-pid holder)
            (port-holder-command holder))))

(defun pid-from-file (pathname)
  (with-open-file (stream pathname :if-does-not-exist nil)
    (and stream
         (parse-integer (string-trim '(#\Space #\Tab #\Newline #\Return)
                                     (read-line stream nil ""))
                        :junk-allowed t))))

(defun pid-file-pid ()
  (pid-from-file *server-pid-path*))

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

(defun acquire-start-lock ()
  (loop repeat (* 10 *server-start-timeout*)
        do (let ((stream
                   (open *server-start-lock-path*
                         :direction :output
                         :if-exists nil
                         :if-does-not-exist :create)))
             (if stream
                 (progn
                   (unwind-protect
                        (format stream "~D~%" (sb-posix:getpid))
                     (close stream))
                   (return-from acquire-start-lock t))
                 (let ((owner (pid-from-file *server-start-lock-path*))
                       (written-at (file-write-date *server-start-lock-path*)))
                   (cond
                     ((and owner (not (pid-alive-p owner)))
                      (ignore-errors (delete-file *server-start-lock-path*)))
                     ((and (null owner) written-at
                           (> (- (get-universal-time) written-at) 5))
                      (ignore-errors (delete-file *server-start-lock-path*)))))))
           (sleep 0.1)
        finally (error "Timed out waiting for Slynk startup lock ~A"
                       *server-start-lock-path*)))

(defun release-start-lock ()
  (when (eql (pid-from-file *server-start-lock-path*) (sb-posix:getpid))
    (ignore-errors (delete-file *server-start-lock-path*))))

(defun spawn-server (&key quiet)
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
    (labels ((relay-server-output (position)
               (if (probe-file *server-log-path*)
                   (with-open-file (input *server-log-path*)
                     (file-position input (min position (file-length input)))
                     (loop for line = (read-line input nil nil)
                           while line do (format t "~A~%" line))
                     (finish-output)
                     (file-position input))
                   position)))
      (loop with log-position = 0
            repeat (* 10 *server-start-timeout*)
            do (setf log-position (relay-server-output log-position))
          when (connection-available-p)
            do (progn
                 (setf log-position (relay-server-output log-position))
                 (assert-listener-project)
                 (unless (eql (pid-file-pid) (listener-process-id))
                   (error "Slynk startup pid ~A does not own port ~D (listener pid ~A)"
                          (pid-file-pid) *port* (listener-process-id)))
                 (unless quiet
                   (format t "luv Slynk is listening on ~A:~D.~%"
                           *host* *port*))
                 (return-from spawn-server t))
          unless (pid-alive-p (pid-file-pid))
            do (progn
                 (print-server-log-tail)
                 (error "luv Slynk server exited during startup"))
          do (sleep 0.1)))
    (print-server-log-tail)
    (error "Timed out waiting for luv Slynk on ~A:~D" *host* *port*)))

(defun kill-port-holders (holders signal)
  (dolist (holder holders)
    (run-shell (format nil "kill -~A ~D >/dev/null 2>&1"
                       signal (port-holder-pid holder)))))

(defun reclaim-port (&key quiet)
  "Free *PORT* when it accepts TCP but no Slynk answers behind it.

The usual cause is a descriptor a dead image left behind: a shell it spawned
in the terminal wall inherited the listening socket and outlived it, so the
kernel keeps the port bound for a process that will never accept anything.
Nothing can close another process's descriptor, so the holders are killed --
except a Lisp, which might be somebody's live image and is only reported."
  (let* ((holders (port-holders))
         (lisps (remove-if-not #'lisp-holder-p holders))
         (leftovers (remove-if #'lisp-holder-p holders)))
    (cond
      ((null holders)
       (unless quiet
         (format *error-output*
                 "Port ~D answered no Slynk handshake and nothing holds it now.~%"
                 *port*))
       t)
      (lisps
       (format *error-output*
               "Port ~D is held by a Lisp that answers no Slynk handshake:~%"
               *port*)
       (describe-port-holders lisps)
       (format *error-output*
               "Kill it yourself, or set LUV_SLYNK_PORT to another port.~%")
       nil)
      (t
       (format *error-output*
               "Port ~D answers no Slynk handshake, so nothing is listening ~
there in any useful sense.  Killing what holds it:~%"
               *port*)
       (describe-port-holders leftovers)
       (kill-port-holders leftovers "TERM")
       (loop repeat 20
             while (port-holders)
             do (sleep 0.1))
       (when (port-holders)
         (kill-port-holders (port-holders) "KILL")
         (loop repeat 20
               while (port-holders)
               do (sleep 0.1)))
       (let ((remaining (port-holders)))
         (cond
           (remaining
            (format *error-output* "Port ~D is still held after SIGKILL:~%" *port*)
            (describe-port-holders remaining)
            nil)
           (t
            (format *error-output* "Reclaimed port ~D.~%" *port*)
            t)))))))

(defun live-listener-p (&key quiet)
  "True when a Slynk belonging to this checkout is listening on *PORT*.

A port that accepts TCP and then says nothing is not a listener at all; it is
reclaimed here so the caller can start a real image on it."
  (handler-case
      (and (connection-available-p)
           (progn (assert-listener-project) t))
    (slynk-handshake-timeout ()
      (unless (reclaim-port :quiet quiet)
        (error "Cannot start luv Slynk: port ~D is occupied and could not be reclaimed"
               *port*))
      nil)))

(defun start-server (&key quiet)
  (when (live-listener-p :quiet quiet)
    (unless quiet
      (format t "luv Slynk is already listening on ~A:~D for ~A.~%"
              *host* *port* *project-root*))
    (return-from start-server t))
  (acquire-start-lock)
  (unwind-protect
       (if (live-listener-p :quiet quiet)
           (progn
             (unless quiet
               (format t "luv Slynk is already listening on ~A:~D for ~A.~%"
                       *host* *port* *project-root*))
             t)
           (spawn-server :quiet quiet))
    (release-start-lock)))

(defun ensure-server ()
  (if (attach-only-p)
      (unless (connection-available-p)
        (error "The requested external Slynk endpoint is not accepting connections on ~A:~D"
               *host* *port*))
      (unless (live-listener-p :quiet t)
        (start-server :quiet t))))

(defmacro with-slynk-connection ((stream) &body body)
  `(let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                :type :stream :protocol :tcp)))
     (unwind-protect
          (progn
            (connect-slynk-socket socket)
            (let ((,stream
                    (sb-bsd-sockets:socket-make-stream
                     socket :input t :output t
                     :element-type '(unsigned-byte 8)
                     :buffering :none)))
              (unwind-protect
                   (progn ,@body)
                (close ,stream))))
       (ignore-errors (sb-bsd-sockets:socket-close socket)))))

(defmacro with-slynk-handshake-timeout (&body body)
  `(handler-case
       (sb-ext:with-timeout *slynk-handshake-timeout*
         ,@body)
     (sb-ext:timeout ()
       (slynk-handshake-timeout-error))))

(defmacro with-verified-slynk-connection ((stream) &body body)
  `(with-slynk-connection (,stream)
     (authenticate ,stream)
     (assert-stream-listener-project ,stream)
     ,@body))

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

(defparameter *fence-watermark-form*
  "(cl:let ((now (cl:and (cl:find-package \"LUV\")
                       (cl:find-symbol \"CANVAS-FAILURE-SERIAL-NOW\" \"LUV\"))))
     (cl:princ (cl:if (cl:and now (cl:fboundp now)) (cl:funcall now) -1)))"
  "Read out of the image before an evaluation: the canvas failure serial, so
that afterwards we can ask what failed since.  -1 when the image has no luv.")

(defparameter *fence-form*
  "(cl:let* ((luv (cl:find-package \"LUV\"))
            (fence (cl:and luv (cl:find-symbol \"FENCE-CANVASES\" \"LUV\")))
            (since (cl:and luv (cl:find-symbol \"CANVAS-FAILURES-SINCE\" \"LUV\")))
            (report (cl:and luv (cl:find-symbol \"REPORT-CANVAS-FAILURE\" \"LUV\")))
            (title (cl:and luv (cl:find-symbol \"CANVAS-TITLE\" \"LUV\"))))
     (cl:when (cl:and fence (cl:fboundp fence))
       (cl:let ((outcomes (cl:funcall fence :frames 2 :timeout 3.0))
                (failures (cl:funcall since ~D)))
         (cl:dolist (outcome outcomes)
           (cl:when (cl:eq (cl:cdr outcome) :timeout)
             (cl:format cl:t \"~~&FENCE-TIMEOUT canvas ~~S ran no frame within 3 s.~~%\"
                        (cl:funcall title (cl:car outcome)))))
         (cl:when failures
           (cl:format cl:t \"~~&FENCE-FAILURES ~~D~~%\" (cl:length failures))
           (cl:dolist (failure failures)
             (cl:funcall report failure))))))"
  "Evaluated after every evaluation: wait for two more frames of every open
canvas -- a frame already under way when the evaluation returned may finish
first; the second must have begun after it -- then print every failure
retained since the watermark, with its backtrace.  A ./sly do that broke
the next frame says so, and exits 1.")

(defvar *fence-p* t
  "Whether EVALUATE fences on the canvas loops after the evaluation.")

(defun fence-watermark-on (stream)
  (let ((text (evaluate-captured-output-on stream *fence-watermark-form*
                                           "CL-USER")))
    (or (ignore-errors (parse-integer (string-trim '(#\Space #\Newline) text)))
        -1)))

(defun fence-on (stream watermark)
  "Fence and report; true when the frames after the evaluation failed."
  (let ((text (evaluate-captured-output-on
               stream (format nil *fence-form* watermark) "CL-USER")))
    (cond ((search "FENCE-FAILURES" text)
           (format *error-output*
                   "~&The game failed after this evaluation:~%~A~%"
                   (string-trim '(#\Newline) text))
           (force-output *error-output*)
           t)
          ((search "FENCE-TIMEOUT" text)
           (format *error-output* "~&~A~%" (string-trim '(#\Newline) text))
           (force-output *error-output*)
           nil)
          (t nil))))

(defun evaluate (code package)
  "Evaluate CODE in PACKAGE, print what it printed and what it returned, then
fence: wait for the next frame of every open canvas and report any failure
it retained.  A change deposited by the evaluation and only felt on the
canvas thread a frame later still comes back to the caller.  Returns 1 when
the frames after CODE failed, else 0."
  (with-verified-slynk-connection (stream)
    (let ((watermark (if *fence-p* (fence-watermark-on stream) -1)))
      (write-packet stream (eval-request code package))
      (destructuring-bind (output value diagnostic-output)
          (read-eval-return stream package)
        (unless (zerop (length output))
          (write-string output)
          (unless (char= (char output (1- (length output))) #\Newline)
            (terpri))
          (force-output))
        (write-diagnostic-output diagnostic-output)
        (write-line value))
      (if (and *fence-p* (>= watermark 0) (fence-on stream watermark))
          1
          0))))

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
  (with-verified-slynk-connection (stream)
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

(defun listener-process-id-on (stream)
  (parse-integer
   (string-trim
    '(#\Space #\Tab #\Newline #\Return)
    (evaluate-captured-output-on stream "(princ (sb-posix:getpid))"))))

(defun listener-process-id ()
  (when (connection-available-p)
    (handler-case
        (with-slynk-connection (stream)
          (authenticate stream)
          (with-slynk-handshake-timeout
            (listener-process-id-on stream)))
      (error () nil))))

(defun canonical-directory-name (pathname)
  (namestring (truename pathname)))

(defun listener-project-root-on (stream)
  (let ((root
          (string-trim
           '(#\Space #\Tab #\Newline #\Return)
           (evaluate-captured-output-on
            stream
            "(princ (namestring (truename (or (and (boundp 'cl-user::*luv-project-root*) (symbol-value 'cl-user::*luv-project-root*)) (uiop:pathname-directory-pathname (asdf:system-source-file :luv))))))"))))
    (and (plusp (length root)) root)))

(defun listener-project-root ()
  (when (connection-available-p)
    (handler-case
        (with-slynk-connection (stream)
          (authenticate stream)
          (with-slynk-handshake-timeout
            (listener-project-root-on stream)))
      (error () nil))))

(defun listener-identity ()
  (handler-case
      (with-slynk-connection (stream)
        (authenticate stream)
        (with-slynk-handshake-timeout
          (values (listener-process-id-on stream)
                  (listener-project-root-on stream))))
    (slynk-handshake-timeout (condition) (values nil nil condition))
    (error () (values nil nil nil))))

(defun listener-for-project-p (&optional (root (listener-project-root)))
  (and root
       (string= (canonical-directory-name *project-root*)
                (canonical-directory-name root))))

(defun assert-stream-listener-project (stream)
  (let ((root (listener-project-root-on stream))
        (pid (and *expected-listener-pid* (listener-process-id-on stream))))
    (unless (listener-for-project-p root)
      (error "Slynk port ~D belongs to ~A, not this checkout ~A. Set LUV_SLYNK_PORT to an unused port if these checkout-derived ports collided."
             *port* (or root "an unidentified Lisp image") *project-root*))
    (when (and *expected-listener-pid* (not (eql pid *expected-listener-pid*)))
      (error "Slynk port ~D belongs to pid ~A, not expected luvcraft pid ~A"
             *port* pid *expected-listener-pid*))))

(defun assert-listener-project ()
  (with-slynk-connection (stream)
    (authenticate stream)
    (with-slynk-handshake-timeout
      (assert-stream-listener-project stream))))

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
        (listener-pid (listener-process-id))
        (listener-root (listener-project-root)))
    (cond
      ((and connection-p (not (listener-for-project-p listener-root)))
       (format t "Slynk port ~D belongs to ~A; leaving that checkout running.~%"
               *port* (or listener-root "an unidentified Lisp image")))
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
  (let ((pid (pid-file-pid)))
    (multiple-value-bind (listener-pid listener-root handshake-error)
        (listener-identity)
      (let ((connection-p (or listener-pid listener-root handshake-error)))
        (cond
          (handshake-error
           (format t "Port ~D accepts TCP but did not complete a Slynk handshake: ~A~%"
                   *port* handshake-error)
           (let ((holders (port-holders)))
             (when holders
               (format t "It is held by:~%")
               (describe-port-holders holders *standard-output*)))
           (format t "./sly reclaim frees the port; ./sly start does it for you.~%"))
          ((and connection-p (not (listener-for-project-p listener-root)))
           (format t "Slynk port ~D belongs to ~A, not this checkout ~A.~%"
                   *port* (or listener-root "an unidentified Lisp image") *project-root*))
          (listener-pid
           (unless (eql pid listener-pid)
             (ignore-errors (delete-file *server-pid-path*)))
           (format t "luv Slynk is listening on ~A:~D (pid ~D, ~A, checkout ~A).~%"
                   *host* *port* listener-pid
                   (if (eql pid listener-pid)
                       "managed by ./sly"
                       "Emacs/external")
                   listener-root)
           (print-game-status))
          (connection-p
           (format t "luv Slynk is listening on ~A:~D (owner unavailable).~%"
                   *host* *port*))
          ((pid-alive-p pid)
           (format t "luv Slynk pid ~D exists, but ~A:~D is not accepting connections.~%"
                   pid *host* *port*))
          (t
           (ignore-errors (delete-file *server-pid-path*))
           (format t "luv Slynk is not running.~%")))))))

(defun run-reclaim ()
  "Free the Slynk port, unless a working Slynk is the thing holding it."
  (multiple-value-bind (listener-pid listener-root handshake-error)
      (listener-identity)
    (cond
      ((and (null handshake-error) (or listener-pid listener-root))
       (format t "Port ~D is a working Slynk (pid ~A, checkout ~A); ~
nothing to reclaim.~%"
               *port* (or listener-pid "unknown") (or listener-root "unknown"))
       0)
      ((null (port-holders))
       (format t "Nothing is holding port ~D.~%" *port*)
       0)
      ((reclaim-port) 0)
      (t 1))))

(defparameter *game-status-form*
  "(let* ((session-symbol
            (and (find-package :luvcraft)
                 (find-symbol \"*SESSION*\" :luvcraft)))
          (session (and session-symbol
                        (boundp session-symbol)
                        (symbol-value session-symbol)))
          (viewer-symbol
            (and (find-package :luft.render)
                 (find-symbol \"*VIEWER*\" :luft.render)))
          (viewer (and viewer-symbol
                       (boundp viewer-symbol)
                       (symbol-value viewer-symbol)))
          (canvas
            (cond
              (session
               (funcall (find-symbol \"LUVCRAFT-SESSION-CANVAS\" :luvcraft)
                        session))
              (viewer
               (funcall (find-symbol \"VIEWER-CANVAS\" :luft.render)
                        viewer))))
          (health
            (ignore-errors
             (when canvas
               (funcall (find-symbol \"CANVAS-HEALTH\" :luv)
                        canvas)))))
     (princ (cond (session :luvcraft) (viewer :luft) (t :idle)))
     (when health (format t \" ~S\" health)))"
  "Read out of the image: which interactive target is playing and its health.

The health comes from the canvas's own loop counters rather than from asking
the canvas thread anything, so it answers even when that thread is the thing
that has stopped -- which is the only moment the question really matters.")

(defun print-canvas-health (text)
  "Print the canvas health plist embedded in the game status TEXT, if any."
  (let* ((start (position #\( text))
         (health (and start
                      (ignore-errors
                       (let ((*read-eval* nil))
                         (read-from-string (subseq text start)))))))
    (when health
      (let ((phase (getf health :phase))
            (seconds (getf health :phase-seconds))
            (ticks (getf health :ticks))
            (state (getf health :state))
            (failure (getf health :frame-failure))
            (failure-count (getf health :failure-count)))
        (cond
          ((getf health :stalled-p)
           (format t "The canvas is STALLED in ~(~A~) for ~,1F s after ~D ~
loop iterations. It is not servicing its window; the image logs the stall ~
and ends itself if it does not recover (./sly log).~%"
                   phase (or seconds 0) ticks))
          (failure
           (multiple-value-bind (second minute hour)
               (decode-universal-time (getf failure :universal-time))
             (format t "The active window's frames are PARKED: a frame failed at ~
~2,'0D:~2,'0D:~2,'0D in ~(~A~) (loop iteration ~D):~%  ~A~%The window is ~
still pumped. Fix the cause and ./sly resume; ./sly failures shows the ~
backtrace.~%"
                     hour minute second (getf failure :phase)
                     (getf failure :tick) (getf failure :report))))
          ((not (member state '(:open :opening)))
           (format t "The canvas is ~(~A~) after ~D loop iterations.~%"
                   state ticks))
          ((getf health :held-p)
           (format t "The canvas loop is pumping with frames HELD (~D ~
iterations); something is redefining the world.~%" ticks))
          (t
           (format t "The canvas loop is healthy (~(~A~), ~D iterations, ~
~D frames).~%"
                   phase ticks (or (getf health :frames) 0))))
        (when (and failure-count (plusp failure-count) (not failure))
          (format t "~D failure~:P retained on the canvas: ./sly failures ~
shows them.~%" failure-count))))))

(defun print-game-status ()
  "Say which interactive target has a live window in the image."
  (let ((answer
          (ignore-errors
           (with-slynk-connection (stream)
             (authenticate stream)
             (evaluate-captured-output-on
              stream *game-status-form* "CL-USER")))))
    (format t "~A~%"
            (cond ((null answer) "Game state unknown (image busy or not loaded).")
                  ((search "LUVCRAFT" (string-upcase (princ-to-string answer)))
                   "Luvcraft is playing: ./sly screenshot PNG; ./sly stop-playing closes it.")
                  ((search "LUFT" (string-upcase (princ-to-string answer)))
                   "LUFT is playing: ./sly screenshot PNG; ./sly stop-playing closes it.")
                  (t "Nothing is playing: ./sly play [luvcraft|luft] starts it.")))
    (when answer
      (print-canvas-health (princ-to-string answer)))))

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
  (with-verified-slynk-connection (stream)
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
  (with-verified-slynk-connection (stream)
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
  (with-verified-slynk-connection (stream)
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
  (with-verified-slynk-connection (stream)
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
  (with-verified-slynk-connection (stream)
    (dolist (name names)
      (when (cdr names)
        (format t "~&~A:~%~%" name))
      (run-xref-for-name stream type name package)
      (when (cdr names)
        (terpri)))))

(defun run-edit (names package)
  (with-verified-slynk-connection (stream)
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
  (format stream "The ordinary workflow is one live Lisp and one game:~%")
  (format stream "  ./sly play [luvcraft|luft] | status | screenshot PNG | stop-playing | restart~%")
  (format stream "PLAY starts the checkout's durable image when necessary. RESTART is the~%")
  (format stream "explicit recovery path when that image is wrecked. LUVCRAFT is PLAY's default.~%")
  (format stream "Prefix a client command with --luvcraft only to attach to a separate~%")
  (format stream "standalone build/luvcraft process.~%~%")
  (format stream "Usage: ./sly play [luvcraft|luft] [--fullscreen]|stop-playing|status|restart~%")
  (format stream "       ./sly start|stop|log|reclaim~%")
  (format stream "       ./sly screenshot PNG~%")
  (format stream "       ./sly eval CODE [--package PACKAGE]~%")
  (format stream "       ./sly do CODE [--package PACKAGE]   (synonym for eval)~%")
  (format stream "       ./sly load SYSTEM...   (frames held while loading; then fenced)~%")
  (format stream "       ./sly failures         (every error retained on the canvas, with backtraces)~%")
  (format stream "       ./sly resume           (run frames again after a frame failure parked them)~%")
  (format stream "       ./sly parinfer [--check|--diff|--write] [--strict] [--file FILE|CODE|FILE]~%")
  (format stream "       ./sly parinfer --batch --check [--strict] FILE...~%")
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
  (and (not (sly-client.parinfer:indent-mode-report-source-balanced-p report))
       (sly-client.parinfer:indent-mode-report-candidate-balanced-p report)
       (sly-client.parinfer:indent-mode-report-candidate-changed-p report)))

(defun parinfer-balanced-conflict-p (report)
  (and (sly-client.parinfer:indent-mode-report-source-balanced-p report)
       (sly-client.parinfer:indent-mode-report-candidate-changed-p report)))

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
    ((not (sly-client.parinfer:indent-mode-report-source-balanced-p report))
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
             (sly-client.parinfer:analyze-indent-mode original))
           (candidate
             (sly-client.parinfer:indent-mode-report-candidate report))
           (repaired
             (sly-client.parinfer:apply-indent-mode original))
           (changed-p (not (string= original repaired)))
           (candidate-changed-p
             (sly-client.parinfer:indent-mode-report-candidate-changed-p
              report))
           (source-balanced-p
             (sly-client.parinfer:indent-mode-report-source-balanced-p
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

(defun run-parinfer-batch (arguments)
  (let ((strict nil)
        (files nil))
    (dolist (argument arguments)
      (cond
        ((string= argument "--check"))
        ((string= argument "--strict")
         (setf strict t))
        ((and (> (length argument) 1)
              (string= argument "--" :end1 2))
         (error "Unknown parinfer batch option: ~A" argument))
        (t
         (push argument files))))
    (unless files
      (error "parinfer --batch requires at least one file"))
    (dolist (file (nreverse files) 0)
      (let ((status
              (run-parinfer
               (append '("--check")
                       (when strict '("--strict"))
                       (list file)))))
        (unless (zerop status)
          (return status))))))

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

(defun run-screenshot (arguments)
  (unless (= (length arguments) 1)
    (error "screenshot requires exactly one PNG pathname"))
  (let* ((pathname
           (merge-pathnames (pathname (first arguments)) (truename ".")))
         (code
           (format nil
                   "(progn
                      (cond
                        (luvcraft:*session*
                         (multiple-value-bind
                               (pathname pixels width height format)
                             (luvcraft:capture-luvcraft-screenshot
                              luvcraft:*session* ~S)
                           (declare (ignore pixels))
                           (list (namestring (truename pathname))
                                 width height format)))
                        ((let* ((package (find-package \"LUFT.RENDER\"))
                                (viewer-symbol
                                  (and package (find-symbol \"*VIEWER*\" package))))
                           (and viewer-symbol (boundp viewer-symbol)
                                (symbol-value viewer-symbol)))
                         (let* ((package (find-package \"LUFT.RENDER\"))
                                (viewer
                                  (symbol-value
                                   (find-symbol \"*VIEWER*\" package)))
                                (context
                                  (funcall
                                   (symbol-function
                                    (find-symbol \"VIEWER-CONTEXT\" package))
                                   viewer))
                                (extent (luv:canvas-extent context)))
                           (funcall
                            (symbol-function
                             (find-symbol \"CAPTURE-VIEWER-FRAME\" package))
                            ~S viewer)
                           (list (namestring (truename ~S))
                                 (first extent) (second extent)
                                 (luv:canvas-format context))))
                        (t
                         (error \"No luvcraft game or LUFT viewer is open.\"))))"
                   pathname pathname pathname)))
    (evaluate code "LUVCRAFT")))

(defun parse-play-arguments (arguments)
  (let ((target :luvcraft)
        (target-specified-p nil)
        (fullscreen-p nil))
    (dolist (argument arguments)
      (cond
        ((string= argument "--fullscreen")
         (setf fullscreen-p t))
        ((member argument '("luvcraft" "luft") :test #'string-equal)
         (when target-specified-p
           (error "play accepts one target, not both ~A and ~A" target argument))
         (setf target (intern (string-upcase argument) :keyword)
               target-specified-p t))
        ((and (> (length argument) 1)
              (string= argument "--" :end1 2))
         (error "Unknown play option: ~A" argument))
        (t
         (error "Unknown play target: ~A (expected luvcraft or luft)" argument))))
    (values target fullscreen-p)))

(defun run-play (arguments)
  (multiple-value-bind (target fullscreen-p)
      (parse-play-arguments arguments)
    (when (attach-only-p)
      (error "play owns the durable image; a standalone luvcraft is already playing"))
    (ensure-server)
    (ecase target
      (:luvcraft
       (evaluate
        (format nil
                "(progn
                   (when luft.render:*viewer*
                     (error \"LUFT is already playing; call STOP-PLAYING first.\"))
                   (luvcraft:play~:[~; :fullscreen-p t~]))"
                fullscreen-p)
        "CL-USER"))
      (:luft
       (evaluate
        (format nil
                "(progn
                   (when luvcraft:*session*
                     (error \"Luvcraft is already playing; call STOP-PLAYING first.\"))
                   (when luft.render:*viewer*
                     (error \"LUFT is already playing; call STOP-PLAYING first.\"))
                   (luft.render:start-viewer~:[~; :fullscreen-p t~]))"
                fullscreen-p)
        "CL-USER")))))

(defun run-stop-playing (arguments)
  (when arguments
    (error "stop-playing does not accept arguments"))
  (when (attach-only-p)
    (error "stop-playing owns the durable image; close the standalone game normally"))
  (ensure-server)
  (evaluate "(cond
               (luvcraft:*session* (luvcraft:stop-playing))
               (luft.render:*viewer* (luft.render:stop-viewer))
               (t (values)))"
            "CL-USER"))

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
      ((string= command "restart")
       (when arguments
         (error "restart does not accept arguments"))
       (when (attach-only-p)
         (error "restart manages the durable image, not a standalone luvcraft"))
       (stop-server)
       (start-server)
       0)
      ((string= command "play")
       (run-play arguments)
       0)
      ((string= command "stop-playing")
       (run-stop-playing arguments)
       0)
      ((string= command "reclaim")
       (when arguments
         (error "reclaim does not accept arguments"))
       (run-reclaim))
      ((string= command "log")
       (when arguments
         (error "log does not accept arguments"))
       (print-server-log-tail)
       0)
      ((string= command "screenshot")
       (ensure-server)
       (run-screenshot arguments)
       0)
      ((member command '("eval" "do") :test #'string=)
       (ensure-server)
       (multiple-value-bind (code package)
           (parse-code-arguments command arguments)
         (evaluate code package)))
      ((string= command "load")
       (unless arguments
         (error "load requires at least one system name"))
       (ensure-server)
       ;; The frame loop is held while the world is redefined: a class
       ;; whose reader is called mid-redefinition kills the frame that
       ;; called it, and a redefinition is nothing a frame should run
       ;; through.  The fence after it says whether the next frame lived.
       (evaluate
        (format nil "(luv:with-canvas-frames-held () ~{(asdf:load-system ~S)~^ ~})"
                arguments)
        "CL-USER"))
      ((string= command "failures")
       (when arguments
         (error "failures does not accept arguments"))
       (ensure-server)
       (let ((*fence-p* nil))
         (evaluate
          "(cl:let ((failures
                     (cl:loop for canvas in (luv:open-canvases)
                              append (cl:reverse (luv:canvas-failures canvas)))))
             (cl:if failures
                    (cl:dolist (failure failures)
                      (luv:report-canvas-failure failure))
                    (cl:format cl:t \"No failures are retained on any open canvas.~~%\"))
             (cl:length failures))"
          "CL-USER")))
      ((string= command "resume")
       (when arguments
         (error "resume does not accept arguments"))
       (ensure-server)
       (evaluate
        "(cl:mapcar (cl:function luv:resume-canvas-frames) (luv:open-canvases))"
        "CL-USER"))
      ((string= command "parinfer")
       (if (member "--batch" arguments :test #'string=)
           (run-parinfer-batch
            (remove "--batch" arguments :test #'string=))
           (run-parinfer arguments)))
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
