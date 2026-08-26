;;;; The short-lived command client for a managed Luv Lisp image. ASDF's
;;;; PROGRAM-OP freezes this file and its dependencies into build/sly-client.

(defpackage #:sly-client
  (:use #:cl)
  (:export #:entry-point))

(in-package #:sly-client)

(defparameter *host* "127.0.0.1")
(defparameter *port* 0)
(defparameter *expected-listener-pid* nil)
(defparameter *project-root*
  (asdf:system-source-directory "sly-client"))
(defparameter *swash* nil)
(defparameter *lisp-selector* nil)
(defparameter *managed-lisp* nil)
(defparameter *current-command* nil)
(defparameter *server-start-timeout* 120)
(defparameter *slynk-handshake-timeout* 3)
(defparameter *default-output-limit* (* 256 1024))

(defun configure-from-environment ()
  "Read invocation-specific state after the cached core starts."
  (let ((listener-pid (sb-ext:posix-getenv "LUV_SLYNK_PID")))
    (setf *host* (or (sb-ext:posix-getenv "LUV_SLYNK_HOST") "127.0.0.1")
          *port* (parse-integer
                  (or (sb-ext:posix-getenv "LUV_SLYNK_PORT") "0"))
          *expected-listener-pid*
          (and listener-pid
               (parse-integer listener-pid :junk-allowed t))
          *swash*
          (or (sb-ext:posix-getenv "LUV_SWASH")
              (error "LUV_SWASH is not set; refresh the luv development profile"))
          *lisp-selector* (sb-ext:posix-getenv "LUV_LISP_SELECTOR")
          *managed-lisp* nil
          *current-command* nil)))

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

(defun split-lines (text)
  (loop with start = 0
        for newline = (position #\Newline text :start start)
        collect (string-right-trim '(#\Return) (subseq text start newline))
        while newline
        do (setf start (1+ newline))))

(defstruct lisp-instance
  id name root started started-universal-time
  last-activity activity state port pid)

(defun run-swash-output (&rest arguments)
  (let ((output (make-string-output-stream))
        (errors (make-string-output-stream)))
    (let* ((process (sb-ext:run-program
                     *swash* arguments
                     :search nil :input nil :output output :error errors :wait t))
           (stdout (get-output-stream-string output))
           (stderr (get-output-stream-string errors))
           (code (sb-ext:process-exit-code process)))
      (unless (zerop code)
        (error "swash ~{~A~^ ~} failed (~D):~%~A"
               arguments code stderr))
      stdout)))

(defun run-swash (&rest arguments)
  (let ((process (sb-ext:run-program
                  *swash* arguments
                  :search nil :input nil :output t :error t :wait t)))
    (unless (zerop (sb-ext:process-exit-code process))
      (error "swash ~{~A~^ ~} failed with exit code ~D"
             arguments (sb-ext:process-exit-code process)))))

(defun json-value (name object)
  (cdr (assoc name object :test #'string=)))

(defun decode-event-lines (text)
  (loop for line in (split-lines text)
        unless (zerop (length line))
          collect (let ((json:*json-identifier-name-to-lisp* #'identity)
                        (json:*identifier-name-to-key* #'identity))
                    (json:decode-json-from-string line))))

(defun swash-events (&rest filters)
  (decode-event-lines
   (apply #'run-swash-output "events" "--json" filters)))

(defun event-fields (event)
  (json-value "fields" event))

(defun event-field (name event)
  (json-value name (event-fields event)))

(defun event-name (event)
  (or (event-field "SWASH_EVENT" event) "output"))

(defun parse-decimal (value)
  (and value (parse-integer value :junk-allowed t)))

(defun process-alive-p (pid)
  (and (integerp pid)
       (plusp pid)
       (handler-case
           (progn
             (sb-posix:kill pid 0)
             t)
         (error () nil))))

(defun endpoint-alive-p (port)
  (and (integerp port)
       (plusp port)
       (let ((*host* "127.0.0.1")
             (*port* port))
         (handler-case
             (connection-available-p)
           (slynk-handshake-timeout () nil)))))

(defvar *process-alive-probe* #'process-alive-p)
(defvar *endpoint-alive-probe* #'endpoint-alive-p)
(defvar *universal-time-provider* #'get-universal-time)
(defvar *swash-events-provider* #'swash-events)

(defun starting-lisp-current-p (instance)
  (let ((started (lisp-instance-started-universal-time instance)))
    (and started
         (let ((age (- (funcall *universal-time-provider*) started)))
           (and (not (minusp age))
                (<= age *server-start-timeout*))))))

(defun lisp-instance-live-p (instance)
  (case (lisp-instance-state instance)
    (:starting
     (starting-lisp-current-p instance))
    (:ready
     (and (lisp-instance-pid instance)
          (lisp-instance-port instance)
          (funcall *process-alive-probe* (lisp-instance-pid instance))
          (funcall *endpoint-alive-probe* (lisp-instance-port instance))))
    (otherwise nil)))

(defun reconcile-lisp-instance (instance)
  (when (and (member (lisp-instance-state instance) '(:starting :ready))
             (not (lisp-instance-live-p instance)))
    (setf (lisp-instance-state instance) :stale
          (lisp-instance-activity instance) "stale"))
  instance)

(defun lisp-instances ()
  "Reconstruct the Lisp registry from portable Swash journal events."
  (let ((by-id (make-hash-table :test #'equal)))
    (dolist (event (funcall *swash-events-provider*
                            "--field" "LUV_KIND=LISP"))
      (let* ((id (event-field "SWASH_SESSION" event))
             (kind (event-name event))
             (instance (gethash id by-id)))
        ;; Swash's portable lifecycle events do not inherit user tags. The
        ;; first tagged event may therefore be ordinary process output or the
        ;; server's SLYNK-READY event if the starting client was interrupted
        ;; before it could publish LISP-STARTING. Any LUV_KIND=LISP event is
        ;; enough to establish the session; later events fill in its identity.
        (when (and id (null instance))
          (setf instance
                (make-lisp-instance
                 :id id
                 :name (or (event-field "LUV_NAME" event) id)
                 :root (event-field "LUV_ROOT" event)
                 :started (json-value "timestamp" event)
                 :started-universal-time
                 (parse-decimal (event-field "LUV_STARTED_AT" event))
                 :state :starting)
                (gethash id by-id) instance))
        (when instance
          (setf (lisp-instance-last-activity instance) (json-value "timestamp" event)
                (lisp-instance-activity instance) kind)
          (let ((event-name (event-field "LUV_NAME" event))
                (event-root (event-field "LUV_ROOT" event))
                (started-at
                  (parse-decimal (event-field "LUV_STARTED_AT" event))))
            (when event-name
              (setf (lisp-instance-name instance) event-name))
            (when event-root
              (setf (lisp-instance-root instance) event-root))
            (when started-at
              (setf (lisp-instance-started-universal-time instance)
                    started-at)))
          (cond
            ((string= kind "slynk-ready")
             (setf (lisp-instance-port instance)
                   (parse-decimal (event-field "LUV_SLYNK_PORT" event))
                   (lisp-instance-pid instance)
                   (parse-decimal (event-field "LUV_SLYNK_PID" event))
                   (lisp-instance-state instance) :ready))
            ((member kind '("exited" "lisp-retired") :test #'string=)
             (setf (lisp-instance-state instance) :exited))))))
    ;; Sessions started by the immediately preceding Swash revision did not
    ;; copy tags onto lifecycle events. Fold untagged exits during migration;
    ;; this is also harmless insurance for imported journals.
    (dolist (event (funcall *swash-events-provider* "--event" "exited"))
      (let ((instance (gethash (event-field "SWASH_SESSION" event) by-id)))
        (when instance
          (setf (lisp-instance-state instance) :exited
                (lisp-instance-last-activity instance) (json-value "timestamp" event)
                (lisp-instance-activity instance) "exited"))))
    (mapcar
     #'reconcile-lisp-instance
     (sort (loop for instance being each hash-value of by-id collect instance)
           #'string< :key #'lisp-instance-started))))

(defun running-lisp-p (instance)
  (member (lisp-instance-state instance) '(:starting :ready)))

(defun stale-lisp-p (instance)
  (eq (lisp-instance-state instance) :stale))

(defun same-root-p (left right)
  (and left right
       (string= (string-right-trim "/" left)
                (string-right-trim "/" right))))

(defun timestamp-display (timestamp)
  (if (and timestamp (>= (length timestamp) 19))
      (let ((copy (subseq timestamp 0 19)))
        (setf (char copy 10) #\Space)
        copy)
      "-"))

(defun print-lisp-list (&key all (stream *standard-output*))
  (let ((instances (if all
                       (lisp-instances)
                       (remove-if-not #'running-lisp-p (lisp-instances)))))
    (if (null instances)
        (format stream "No ~:[running ~;~]Swash-managed Lisps.~%" all)
        (progn
          (format stream "~6A  ~16A ~8A ~7A ~5A  ~19A  ~19A  ~A~%"
                  "ID" "NAME" "STATE" "PID" "PORT" "STARTED" "ACTIVE" "ROOT")
          (dolist (instance instances)
            (format stream "~6A  ~16A ~8A ~7A ~5A  ~19A  ~19A  ~A~%"
                    (lisp-instance-id instance)
                    (lisp-instance-name instance)
                    (string-downcase (symbol-name (lisp-instance-state instance)))
                    (or (lisp-instance-pid instance) "-")
                    (or (lisp-instance-port instance) "-")
                    (timestamp-display (lisp-instance-started instance))
                    (timestamp-display (lisp-instance-last-activity instance))
                    (or (lisp-instance-root instance) "-")))))
    instances))

(defun matching-lisps (selector instances)
  (and (plusp (length selector))
       (or (let ((exact
                   (remove-if-not
                    (lambda (instance)
                      (string= selector (lisp-instance-id instance)))
                    instances)))
             (and exact exact))
           (remove-if-not
            (lambda (instance)
              (or (string= selector (lisp-instance-name instance))
                  (and (<= (length selector) (length (lisp-instance-id instance)))
                       (string= selector (lisp-instance-id instance)
                                :end2 (length selector)))))
            instances))))

(defun stale-lisp-selection-error (instances)
  (error "Stale managed Lisp~P ~{~A~^, ~} must be selected by full session ID; use ./sly --lisp ID status, log, stop, or restart"
         (length instances)
         (mapcar #'lisp-instance-id instances)))

(defun choose-lisp (&key start-if-missing allow-explicit-stale refuse-if-stale
                         (instances nil instances-supplied-p))
  (when (and *lisp-selector* (zerop (length *lisp-selector*)))
    (error "Lisp selector must not be empty"))
  (let* ((all (if instances-supplied-p instances (lisp-instances)))
         (running (remove-if-not #'running-lisp-p all))
         (exact-instance
           (and *lisp-selector*
                (find *lisp-selector* all
                      :key #'lisp-instance-id :test #'string=)))
         ;; An unhealthy session is deliberately never a work target. Lifecycle
         ;; commands may recover one only through its complete Swash identity;
         ;; names and prefixes are not precise enough for a destructive action.
         (explicit-stale
           (and allow-explicit-stale
                exact-instance
                (stale-lisp-p exact-instance)
                exact-instance))
         (eligible (if explicit-stale
                       (cons explicit-stale running)
                       running))
         (candidates
           (cond
             ((and exact-instance (running-lisp-p exact-instance))
              (list exact-instance))
             (explicit-stale (list explicit-stale))
             (*lisp-selector*
              (matching-lisps *lisp-selector* eligible))
             (t
              (remove-if-not
               (lambda (instance)
                 (same-root-p (lisp-instance-root instance)
                              (namestring *project-root*)))
               running))))
         (stale-matches
           (and *lisp-selector*
                (matching-lisps *lisp-selector*
                                (remove-if-not #'stale-lisp-p all))))
         (stale-for-root
           (remove-if-not
            (lambda (instance)
              (and (stale-lisp-p instance)
                   (same-root-p (lisp-instance-root instance)
                                (namestring *project-root*))))
            all)))
    (cond
      ((and stale-matches
            (null exact-instance))
       (stale-lisp-selection-error stale-matches))
      ((null candidates)
       (cond
         (stale-matches
          (stale-lisp-selection-error stale-matches))
         (*lisp-selector*
          (print-lisp-list)
          (error "No running Lisp matches ~S" *lisp-selector*))
         ((and stale-for-root (or start-if-missing refuse-if-stale))
          (stale-lisp-selection-error stale-for-root))
         (start-if-missing (start-server :quiet t))
         (t nil)))
      ((cdr candidates)
       (print-lisp-list)
       (if *lisp-selector*
           (error "Selector ~S matches ~D running Lisps; use ./sly --lisp ID ..."
                  *lisp-selector* (length candidates))
           (error "This checkout has ~D running Lisps; use ./sly --lisp ID ..."
                  (length candidates))))
      (t (first candidates)))))

(defun select-managed-lisp (instance)
  (unless (and (lisp-instance-port instance) (lisp-instance-pid instance))
    (error "Lisp ~A has not published a Slynk endpoint"
           (lisp-instance-id instance)))
  (setf *managed-lisp* instance
        *host* "127.0.0.1"
        *port* (lisp-instance-port instance)
        *expected-listener-pid* (lisp-instance-pid instance))
  instance)

(defun session-events (session)
  (swash-events "--session" session))

(defun ready-instance-from-event (instance event)
  (setf (lisp-instance-port instance)
        (parse-decimal (event-field "LUV_SLYNK_PORT" event))
        (lisp-instance-pid instance)
        (parse-decimal (event-field "LUV_SLYNK_PID" event))
        (lisp-instance-state instance) :ready
        (lisp-instance-last-activity instance) (json-value "timestamp" event)
        (lisp-instance-activity instance) "slynk-ready")
  instance)

(defun wait-for-lisp (instance &key follow-process quiet)
  (let ((deadline (+ (get-internal-real-time)
                     (* *server-start-timeout* internal-time-units-per-second)))
        (next-notice (+ (get-internal-real-time)
                        (* 5 internal-time-units-per-second))))
    (unwind-protect
         (loop
           (let* ((events (session-events (lisp-instance-id instance)))
                  (ready (find "slynk-ready" events
                               :key #'event-name :test #'string= :from-end t))
                  (exited (find "exited" events
                                :key #'event-name :test #'string= :from-end t)))
             (when exited
               (error "Lisp ~A exited before publishing its Slynk endpoint; run ./sly --lisp ~A log"
                      (lisp-instance-id instance) (lisp-instance-id instance)))
             (when ready
               (ready-instance-from-event instance ready)
               (select-managed-lisp instance)
               (assert-listener-project)
               (unless quiet
                 (format t "Lisp ~A (~A) is ready on ~A:~D (pid ~D).~%"
                         (lisp-instance-id instance)
                         (lisp-instance-name instance)
                         *host* *port* *expected-listener-pid*))
               (return instance)))
           (when (> (get-internal-real-time) deadline)
             (ignore-errors (run-swash "stop" (lisp-instance-id instance)))
             (error "Timed out after ~D seconds waiting for Lisp ~A"
                    *server-start-timeout* (lisp-instance-id instance)))
           (when (> (get-internal-real-time) next-notice)
             (format *error-output* "sly: Lisp ~A is still starting; waiting for slynk-ready.~%"
                     (lisp-instance-id instance))
             (force-output *error-output*)
             (incf next-notice (* 5 internal-time-units-per-second)))
           (sleep 0.5))
      (when (and follow-process (sb-ext:process-alive-p follow-process))
        (sb-ext:process-kill follow-process 15))
      (when follow-process
        (ignore-errors (sb-ext:process-wait follow-process))))))

(defun default-lisp-name ()
  (let* ((directory (pathname-directory *project-root*))
         (name (car (last directory))))
    (princ-to-string name)))

(defun valid-session-id-p (value)
  (and (= (length value) 6)
       (every #'upper-case-p (subseq value 0 3))
       (every #'digit-char-p (subseq value 3))))

(defun ensure-sly-dependency-core ()
  (let* ((builder
           (merge-pathnames #P"scripts/build-sly-dependency-core"
                            *project-root*))
         (core (merge-pathnames #P"build/sly-dependencies.core"
                                *project-root*))
         (process
           (sb-ext:run-program
            (namestring builder) nil
            :search nil :input nil :output t :error t :wait t)))
    (unless (zerop (sb-ext:process-exit-code process))
      (error "Could not build the Sly dependency core"))
    (unless (probe-file core)
      (error "Sly dependency core builder did not produce ~A" core))
    core))

(defun start-server (&key quiet (name (default-lisp-name)))
  "Start a new Lisp incarnation. Explicit START intentionally permits peers."
  (let* ((dependency-core (ensure-sly-dependency-core))
         (started-at (funcall *universal-time-provider*))
         (server-path (merge-pathnames #P"sly-server.lisp" *project-root*))
         (output
           (run-swash-output
            "start"
            "--tag" "LUV_KIND=LISP"
            "--tag" (format nil "LUV_ROOT=~A" (namestring *project-root*))
            "--tag" (format nil "LUV_NAME=~A" name)
            "--tag" (format nil "LUV_STARTED_AT=~D" started-at)
            "--" "env" (format nil "LUV_NAME=~A" name)
            "sbcl" "--core" (namestring dependency-core)
            "--noinform" "--disable-debugger"
            "--load" (namestring server-path)))
         (separator (or (position-if (lambda (character)
                                       (find character " \t\r\n"))
                                     output)
                        (length output)))
         (session (subseq output 0 separator)))
    (unless (valid-session-id-p session)
      (error "Could not read Swash session ID from: ~S" output))
    (format t "Started Lisp ~A (~A) under Swash.~%" session name)
    (force-output)
    (run-swash-output "emit" session
                      "--event" "lisp-starting"
                      "--message" (format nil "Starting Lisp ~A" name)
                      "--field" "LUV_KIND=LISP"
                      "--field" (format nil "LUV_ROOT=~A" (namestring *project-root*))
                      "--field" (format nil "LUV_NAME=~A" name)
                      "--field" (format nil "LUV_STARTED_AT=~D" started-at))
    (let* ((instance
             (make-lisp-instance
              :id session :name name :root (namestring *project-root*)
              :started-universal-time started-at :state :starting))
           (follow
             (sb-ext:run-program
              *swash* (list "follow" session)
              :search nil :input nil :output t :error t :wait nil)))
      (wait-for-lisp instance :follow-process follow :quiet quiet))))

(defun lisp-identity-fields (instance)
  (list "LUV_KIND=LISP"
        (format nil "LUV_ROOT=~A" (lisp-instance-root instance))
        (format nil "LUV_NAME=~A" (lisp-instance-name instance))))

(defun lisp-activity-fields (instance)
  (append (lisp-identity-fields instance)
          (list (format nil "LUV_COMMAND=~A" *current-command*))))

(defun emit-managed-lisp-event (instance event message &rest fields)
  (apply #'run-swash-output
         "emit" (lisp-instance-id instance)
         "--event" event
         "--message" message
         (loop for field in (append (lisp-identity-fields instance) fields)
               append (list "--field" field))))

(defun emit-lisp-activity ()
  (when (and *managed-lisp* *current-command*)
    (emit-managed-lisp-event
     *managed-lisp* "sly-activity" (format nil "./sly ~A" *current-command*)
     (format nil "LUV_COMMAND=~A" *current-command*))))

(defun ensure-server ()
  (if (attach-only-p)
      (unless (connection-available-p)
        (error "The requested external Slynk endpoint is not accepting connections on ~A:~D"
               *host* *port*))
      (let ((instance (choose-lisp :start-if-missing t)))
        (if (eq (lisp-instance-state instance) :ready)
            (select-managed-lisp instance)
            (wait-for-lisp instance :quiet nil))
        (emit-lisp-activity))))

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
    (evaluate-captured-output-on stream
                                 "(princ (slynk-backend::getpid))"))))

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
      (error "Slynk endpoint ~A:~D reports checkout ~A, not ~A"
             *host*
             *port* (or root "an unidentified Lisp image") *project-root*))
    (when (and *expected-listener-pid* (not (eql pid *expected-listener-pid*)))
      (error "Slynk port ~D belongs to pid ~A, not expected luvcraft pid ~A"
             *port* pid *expected-listener-pid*))))

(defun assert-listener-project ()
  (with-slynk-connection (stream)
    (authenticate stream)
    (with-slynk-handshake-timeout
      (assert-stream-listener-project stream))))

(defun first-whitespace-delimited-field (line)
  (let* ((trimmed (string-left-trim '(#\Space #\Tab) line))
         (end (position-if (lambda (character)
                             (member character '(#\Space #\Tab)))
                           trimmed)))
    (subseq trimmed 0 end)))

(defun swash-session-running-p (instance)
  (find (lisp-instance-id instance)
        (split-lines (run-swash-output "-a"))
        :key #'first-whitespace-delimited-field
        :test #'string=))

(defvar *swash-session-running-probe* #'swash-session-running-p)
(defvar *swash-stop-runner*
  (lambda (instance)
    (run-swash "stop" (lisp-instance-id instance))))
(defvar *managed-lisp-event-emitter* #'emit-managed-lisp-event)

(defun stop-managed-lisp (instance)
  "Stop INSTANCE when Swash still owns it, then retire its journal identity.

An unhealthy session whose host has already disappeared is already stopped;
publishing LISP-RETIRED makes that fact durable so RESTART can proceed. If its
host is still present, Swash remains the only authority allowed to stop it."
  (let ((stale-p (stale-lisp-p instance)))
    (when (or (not stale-p)
              (funcall *swash-session-running-probe* instance))
      (handler-case
          (funcall *swash-stop-runner* instance)
        (error (condition)
          ;; The host may have exited between the status query and STOP. That
          ;; is a successful stop; any still-running host remains an error.
          (when (or (not stale-p)
                    (funcall *swash-session-running-probe* instance))
            (error condition)))))
    (funcall *managed-lisp-event-emitter*
             instance "lisp-retired"
             (format nil "Retired Lisp ~A" (lisp-instance-name instance)))
    t))

(defun stop-server ()
  (when (attach-only-p)
    (let ((listener-pid (listener-process-id)))
      (if listener-pid
          (format t "External Slynk pid ~D is listening on ~A:~D; leaving it running.~%"
                  listener-pid *host* *port*)
          (format t "The external Slynk endpoint is not running on ~A:~D.~%"
                  *host* *port*)))
    (return-from stop-server nil))
  (let ((instance (choose-lisp :allow-explicit-stale t
                               :refuse-if-stale t)))
    (if (null instance)
        (progn
          (format t "This checkout has no running Lisp.~%")
          nil)
        (progn
          (stop-managed-lisp instance)
          (format t "Stopped Lisp ~A (~A).~%"
                  (lisp-instance-id instance) (lisp-instance-name instance))
          t))))

(defun restart-server ()
  (when (attach-only-p)
    (error "restart manages a Swash Lisp, not a standalone luvcraft"))
  (let ((instance (choose-lisp :allow-explicit-stale t
                               :refuse-if-stale t)))
    (if instance
        (let ((name (lisp-instance-name instance)))
          (stop-managed-lisp instance)
          (format t "Stopped Lisp ~A (~A).~%"
                  (lisp-instance-id instance) name)
          (start-server :name name))
        (start-server))))

(defun report-managed-server-status (instance)
  (cond
    ((null instance)
     (format t "This checkout has no running Lisp.~%"))
    ((eq (lisp-instance-state instance) :ready)
     (select-managed-lisp instance)
     (format t "Selected ~A (~A) for this checkout.~%"
             (lisp-instance-id instance) (lisp-instance-name instance))
     (handler-case
         (progn
           (assert-listener-project)
           (print-game-status))
       (error (condition)
         (format t "Slynk health: ~A~%" condition))))
    ((eq (lisp-instance-state instance) :starting)
     (format t "Lisp ~A (~A) is still starting.~%"
             (lisp-instance-id instance) (lisp-instance-name instance)))
    (t
     (format t "Lisp ~A (~A) is stale or unhealthy; inspect its log, then stop or restart it by full session ID.~%"
             (lisp-instance-id instance) (lisp-instance-name instance)))))

(defun server-status ()
  (when (attach-only-p)
    (let ((listener-pid (listener-process-id)))
      (if listener-pid
          (format t "External Slynk is listening on ~A:~D (pid ~D).~%"
                  *host* *port* listener-pid)
          (format t "External Slynk is not accepting connections on ~A:~D.~%"
                  *host* *port*)))
    (return-from server-status nil))
  (report-managed-server-status
   (choose-lisp :allow-explicit-stale t)))

(defun print-server-log-tail ()
  (when (attach-only-p)
    (error "log is available for Swash-managed Lisps, not standalone luvcraft"))
  (let ((instance (choose-lisp :allow-explicit-stale t
                               :refuse-if-stale t)))
    (if instance
        (run-swash "poll" (lisp-instance-id instance))
        (format t "This checkout has no running Lisp.~%"))))

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
  (format stream "Lisps are named Swash sessions, visible across all checkouts:~%")
  (format stream "  ./sly list [--all]~%")
  (format stream "  ./sly start [--name NAME]~%")
  (format stream "  ./sly --lisp ID-or-NAME COMMAND ...~%~%")
  (format stream "Without --lisp, a command selects the sole running Lisp for this checkout.~%")
  (format stream "If there is none, work commands start one; if there are several, selection~%")
  (format stream "is intentionally required. RESTART creates a new Swash incarnation.~%")
  (format stream "Prefix a client command with --luvcraft only to attach to a separate~%")
  (format stream "standalone build/luvcraft process.~%~%")
  (format stream "Usage: ./sly play [luvcraft|luft] [--fullscreen]|stop-playing|status|restart~%")
  (format stream "       ./sly list [--all]|start [--name NAME]|stop|log~%")
  (format stream "       ./sly systems [--all]|system NAME|stale~%")
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
    (when *managed-lisp*
      (format t "Opening ~A in Lisp ~A (~A).~%"
              (string-downcase (symbol-name target))
              (lisp-instance-id *managed-lisp*)
              (lisp-instance-name *managed-lisp*))
      (force-output))
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

(defun parse-start-arguments (arguments)
  (cond
    ((null arguments) (default-lisp-name))
    ((and (= (length arguments) 2)
          (string= (first arguments) "--name")
          (plusp (length (second arguments))))
     (second arguments))
    (t (error "start accepts only --name NAME"))))

(defun run-captured-report (form)
  (format *error-output* "Inspecting live ASDF state...~%")
  (force-output *error-output*)
  (ensure-server)
  (with-verified-slynk-connection (stream)
    (let ((output (evaluate-captured-output-on stream form "CL-USER")))
      (write-string output)
      (unless (or (zerop (length output))
                  (char= (char output (1- (length output))) #\Newline))
        (terpri))
      (force-output))))

(defun run-systems (arguments)
  (cond
    ((null arguments)
     (run-captured-report
      (format nil "(luv.sly.asdf:print-systems :root (pathname ~S))"
              (namestring *project-root*))))
    ((equal arguments '("--all"))
     (run-captured-report
      (format nil "(luv.sly.asdf:print-systems :root (pathname ~S) :all t)"
              (namestring *project-root*))))
    (t (error "systems accepts only --all"))))

(defun run-system-status (arguments)
  (unless (= (length arguments) 1)
    (error "system requires exactly one ASDF system name"))
  (run-captured-report
   (format nil "(luv.sly.asdf:print-system ~S :root (pathname ~S))"
           (first arguments) (namestring *project-root*))))

(defun run-stale-systems (arguments)
  (when arguments
    (error "stale does not accept arguments"))
  (run-captured-report
   (format nil "(luv.sly.asdf:print-stale-systems :root (pathname ~S))"
           (namestring *project-root*))))

(defun load-systems-form (systems)
  ;; The slim Luft image has no LUV package until a rendering system is
  ;; loaded.  Resolve the frame-holding function at evaluation time so the
  ;; reader can still accept this form in that image.
  (format nil
          "(cl:let* ((luv (cl:find-package ~S))
                     (hold (cl:and luv (cl:find-symbol ~S luv))))
             (cl:if (cl:and hold (cl:fboundp hold))
                    (cl:funcall hold (cl:lambda () ~{(asdf:load-system ~S)~^ ~}))
                    (cl:progn ~{(asdf:load-system ~S)~^ ~})))"
          "LUV" "CALL-WITH-CANVAS-FRAMES-HELD" systems systems))

(defun main (arguments)
  (unless arguments
    (usage *error-output*)
    (return-from main 2))
  (let ((command (pop arguments)))
    (setf *current-command* command)
    (cond
      ((member command '("-h" "--help") :test #'string=)
       (usage)
       0)
      ((string= command "start")
       (start-server :name (parse-start-arguments arguments))
       0)
      ((string= command "list")
       (cond
         ((null arguments) (print-lisp-list))
         ((equal arguments '("--all")) (print-lisp-list :all t))
         (t (error "list accepts only --all")))
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
      ((string= command "systems")
       (run-systems arguments)
       0)
      ((string= command "system")
       (run-system-status arguments)
       0)
      ((string= command "stale")
       (run-stale-systems arguments)
       0)
      ((string= command "restart")
       (when arguments
         (error "restart does not accept arguments"))
       (restart-server)
       0)
      ((string= command "play")
       (run-play arguments)
       0)
      ((string= command "stop-playing")
       (run-stop-playing arguments)
       0)
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
        (load-systems-form arguments)
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

(defun entry-point ()
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
                 (progn
                   (configure-from-environment)
                   (main (cdr sb-ext:*posix-argv*)))
               (error (condition)
                 (format *error-output* "sly: ~A~%" condition)
                 1)))))
    (when (and budget (output-budget-truncated-p budget))
      (force-output original-output)
      (format original-error
              "~&[sly output truncated after ~D bytes; set LUV_SLY_MAX_OUTPUT=0 for unlimited]~%"
              limit)
      (force-output original-error))
    (sb-ext:exit :code exit-code)))
