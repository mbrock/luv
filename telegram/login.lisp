;;;; Credentials, stored sessions, and logging in.
;;;;
;;;; Three things stand between an authorization key and being someone on
;;;; Telegram.  An application identity, which Telegram issues per developer
;;;; and which lives in the environment or a dotenv file.  A login, which is
;;;; a phone number, a code Telegram sends to it, and -- if the account has
;;;; one -- a password proved by SRP rather than sent.  And somewhere to keep
;;;; the result, because an authorization key that is thrown away costs
;;;; another login and another code.
;;;;
;;;; The awkward part is that Telegram may answer the very first request with
;;;; PHONE_MIGRATE_4, meaning "this number belongs to another data centre,
;;;; start again over there".  So LOG-IN owns its connection: it has to be
;;;; able to throw one away and open another mid-flow.

(in-package #:telegram.client)

(define-condition login-failed (mt:mtproto-error)
  ((detail :initarg :detail :reader login-failed-detail))
  (:report (lambda (condition stream)
             (format stream "Login failed: ~A." (login-failed-detail condition))))
  (:documentation "The login could not be completed."))

;;;; Credentials

(defparameter *credential-files*
  '("./.env" "~/.telegram.env" "~/.env")
  "Where APPLICATION-FROM-ENVIRONMENT looks when the environment itself is
bare, in order.  TELEGRAM_ENV_FILE overrides the list entirely.")

(defun read-dotenv (pathname)
  "Parse a dotenv file into an alist, or NIL if it is not there.  Understands
the `export' prefix, `#' comments, and quoted values, because that is what
these files have in them."
  (let ((path (probe-file (merge-pathnames pathname))))
    (when path
      (with-open-file (stream path :external-format :utf-8)
        (loop for line = (read-line stream nil nil)
              while line
              for trimmed = (string-trim '(#\Space #\Tab #\Return) line)
              for content = (if (and (>= (length trimmed) 7)
                                     (string= "export " trimmed :end2 7))
                                (string-left-trim '(#\Space) (subseq trimmed 7))
                                trimmed)
              for equals = (position #\= content)
              unless (or (zerop (length content))
                         (char= #\# (char content 0))
                         (null equals))
                collect (cons (string-trim '(#\Space) (subseq content 0 equals))
                              (unquote-dotenv-value
                               (string-trim '(#\Space)
                                            (subseq content (1+ equals))))))))))

(defun unquote-dotenv-value (value)
  (if (and (>= (length value) 2)
           (member (char value 0) '(#\" #\'))
           (char= (char value 0) (char value (1- (length value)))))
      (subseq value 1 (1- (length value)))
      value))

(defun credential (names &key file)
  "The first of NAMES set in the environment, or failing that in a dotenv
file.  The environment wins so that one run can override what is on disk."
  (or (loop for name in names
            thereis (let ((value (sb-ext:posix-getenv name)))
                      (when (and value (plusp (length value))) value)))
      (loop for candidate in (if file
                                 (list file)
                                 (let ((named (sb-ext:posix-getenv
                                               "TELEGRAM_ENV_FILE")))
                                   (if named (list named) *credential-files*)))
            for bindings = (read-dotenv candidate)
            thereis (loop for name in names
                          for found = (cdr (assoc name bindings :test #'string=))
                          when (and found (plusp (length found)))
                            return found))))

(defun application-from-environment (&rest initargs &key file &allow-other-keys)
  "An application identity from TELEGRAM_API_ID and TELEGRAM_API_HASH, or the
TDLIB_ names other clients use, taken from the environment or from a dotenv
file.  FILE names one explicitly; otherwise TELEGRAM_ENV_FILE and then
*CREDENTIAL-FILES* are searched."
  (let ((id (credential '("TELEGRAM_API_ID" "TDLIB_API_ID") :file file))
        (hash (credential '("TELEGRAM_API_HASH" "TDLIB_API_HASH") :file file)))
    (unless id
      (error 'missing-credentials :variable "TELEGRAM_API_ID"))
    (apply #'make-application :api-id (parse-integer id)
                              :api-hash (or hash "")
                              (alexandria-remove-key initargs :file))))

(defun alexandria-remove-key (plist key)
  (loop for (name value) on plist by #'cddr
        unless (eq name key) append (list name value)))

;;;; Stored sessions

(defparameter *session-file* "~/.telegram-session"
  "Where a session is kept when no other path is given.  It holds an
authorization key, which is the whole credential: anyone with the file is
logged in as you.")

(defun save-session (connection &optional (pathname *session-file*) &rest extra)
  "Write CONNECTION's authorization key and session identity where
LOAD-SESSION will find them.  EXTRA is merged into the stored plist, which is
how a login in progress remembers its phone number and code hash across a
process boundary."
  (let ((session (net:connection-session connection))
        (path (merge-pathnames pathname)))
    (with-open-file (stream path :direction :output :if-exists :supersede
                                 :if-does-not-exist :create
                                 :external-format :utf-8)
      (let ((*package* (find-package :keyword)))
        (prin1 (append
                extra
                (list :dc-id (net:connection-dc-id connection)
                      :test (net:connection-test-p connection)
                      :auth-key (octets:octets-hex
                                 (mt:auth-key-data (mt:session-key session)))
                      :server-salt (mt:session-server-salt session)
                      :time-offset (mt:session-time-offset session)
                      :session-id (mt:session-id session)))
               stream)
        (terpri stream)))
    ;; NAMESTRING would hand chmod a literal "~/", which it cannot resolve;
    ;; the native namestring is the one the operating system understands.
    (sb-posix:chmod (sb-ext:native-namestring path) #o600)
    path))

(defun load-session (&optional (pathname *session-file*))
  "The stored session at PATHNAME, as a plist, or NIL."
  (let ((path (probe-file (merge-pathnames pathname))))
    (when path
      (with-open-file (stream path :external-format :utf-8)
        (let ((*read-eval* nil))
          (read stream nil nil))))))

(defun stored-material (stored)
  "The AUTH-KEY-MATERIAL a stored session describes."
  (make-instance 'mt:auth-key-material
                 :key (mt:make-auth-key
                       (octets:hex-octets (getf stored :auth-key)))
                 :server-salt (getf stored :server-salt)
                 :time-offset (getf stored :time-offset)
                 :dc-id (getf stored :dc-id)))

;;;; Migration
;;;;
;;;; Telegram answers a request meant for another data centre with an error
;;;; naming it.  That is not a failure so much as a redirection, and the only
;;;; sensible response is to open a connection there and try again.

(defun migration-data-center (message)
  "The data centre a MIGRATE error points at, or NIL."
  (loop for prefix in '("PHONE_MIGRATE_" "USER_MIGRATE_" "NETWORK_MIGRATE_"
                        "FILE_MIGRATE_" "STATS_MIGRATE_")
        when (and (>= (length message) (length prefix))
                  (string= prefix message :end2 (length prefix)))
          return (parse-integer message :start (length prefix)
                                        :junk-allowed t)))

;;;; Reading from the person at the keyboard

(defun prompt (control &rest arguments)
  (format *query-io* "~&~?" control arguments)
  (finish-output *query-io*)
  (string-trim '(#\Space #\Tab #\Return) (read-line *query-io*)))

(defun prompt-quietly (control &rest arguments)
  "Like PROMPT, but with terminal echo off if we can manage it, since the
answer is a password."
  (let ((quiet (ignore-errors
                (zerop (sb-ext:process-exit-code
                        (sb-ext:run-program "/bin/sh" '("-c" "stty -echo </dev/tty")
                                            :search nil :wait t))))))
    (unwind-protect (apply #'prompt control arguments)
      (when quiet
        (ignore-errors (sb-ext:run-program "/bin/sh" '("-c" "stty echo </dev/tty")
                                           :search nil :wait t))
        (format *query-io* "~%")))))

(defun default-code-reader (sent-code)
  (prompt "Telegram sent a ~(~A~) code. Enter it: "
          (tl:tl-name (tl:tl-value sent-code :type))))

(defun default-password-reader (password)
  (prompt-quietly "~@[Password hint: ~A~%~]Password: "
                  (tl:tl-value password :hint)))

;;;; The flow

(defun authorized-user (connection)
  "The user this session is logged in as, or NIL if it is not logged in.

Asked by making the cheapest authorized call there is: an unauthorized
session gets a 401 back, which is an answer rather than a failure."
  (handler-case (logged-in-user connection)
    (mt:remote-rpc-error (error)
      (unless (= 401 (mt:remote-rpc-error-code error))
        (error error))
      nil)))

(defun authorized-p (connection)
  "Is this session logged in?"
  (and (authorized-user connection) t))

(defun logged-in-user (connection)
  "The user this session is logged in as."
  (let ((users (invoke connection :users.get-users
                       :id (vector (tl:make-tl :input-user-self)))))
    (when (plusp (length users))
      (elt users 0))))

(defun user-label (user)
  "A short human rendering of a user, for the transcript."
  (format nil "~@[~A~]~@[ ~A~]~@[ (@~A)~]~@[ id ~D~]"
          (tl:tl-value user :first-name) (tl:tl-value user :last-name)
          (tl:tl-value user :username) (tl:tl-value user :id)))

(defun send-login-code (connection phone-number)
  "Ask Telegram to send a login code to PHONE-NUMBER.  Returns the
auth.sentCode, whose phone_code_hash SIGN-IN needs."
  (invoke connection :auth.send-code
          :phone-number phone-number
          :api-id (application-api-id *application*)
          :api-hash (application-api-hash *application*)
          :settings (tl:make-tl :code-settings)))

(defun sign-in (connection phone-number code-hash code)
  "Complete a login with the code Telegram sent."
  (invoke connection :auth.sign-in
          :phone-number phone-number
          :phone-code-hash code-hash
          :phone-code code))

(defun check-password (connection password &key entropy)
  "Prove knowledge of the account's two-factor password, by SRP."
  (let* ((entropy (or entropy octets:*entropy*))
         (state (invoke connection :account.get-password)))
    (unless (tl:tl-value state :has-password)
      (error 'login-failed :detail "the account has no password to check"))
    (let ((algorithm (tl:tl-value state :current-algo)))
      (unless (eq :password-kdf-algo-sha256-sha256-pbkdf2-hmacsha512iter100000-sha256-mod-pow
                  (tl:tl-name algorithm))
        (error 'login-failed
               :detail (format nil "unsupported password algorithm ~(~A~)"
                               (tl:tl-name algorithm))))
      (multiple-value-bind (public proof)
          (crypto:srp-check-password
           password
           :prime (tl:tl-value algorithm :p)
           :generator (tl:tl-value algorithm :g)
           :salt1 (tl:tl-value algorithm :salt1)
           :salt2 (tl:tl-value algorithm :salt2)
           :server-public (tl:tl-value state :srp-b)
           :secret-octets (octets:random-octets entropy 256))
        (invoke connection :auth.check-password
                :password (tl:make-tl :input-check-password-srp
                                      :srp-id (tl:tl-value state :srp-id)
                                      :a public
                                      :m1 proof))))))

(defun log-in (&key phone-number password (dc-id 2) test
                    (application (or *application* (application-from-environment)))
                    (session-file *session-file*)
                    (read-code #'default-code-reader)
                    (read-password #'default-password-reader)
                    (stream *standard-output*))
  "Log in, and return the connection and the user.

Reuses the session at SESSION-FILE when it is still authorized, so that a
second run needs neither a handshake nor a code.  Otherwise it asks Telegram
for a code, calls READ-CODE to get it, follows any PHONE_MIGRATE to the data
centre that owns the number, and answers a password challenge with SRP.

  (telegram.client:log-in :phone-number \"+15551234567\")

PASSWORD, when given, is used instead of asking."
  (let* ((*application* application)
         (stored (load-session session-file))
         (connection nil)
         (user nil))
    (flet ((open-connection (dc-id test material)
             (when connection (net:close-mtproto-connection connection))
             (setf connection (net:open-mtproto-connection :dc-id dc-id
                                                           :test test))
             (net:establish-session
              connection (or material (net:create-auth-key connection)))
             (format stream "~&connected to ~A~%" connection)
             connection))
      (unwind-protect
           (progn
             (if (and stored (eql dc-id (getf stored :dc-id))
                      (eq (not test) (not (getf stored :test))))
                 (open-connection dc-id test (stored-material stored))
                 (open-connection dc-id test nil))
             (when (and stored (setf user (ignore-errors
                                           (authorized-user connection))))
               (format stream "~&already logged in as ~A~%" (user-label user))
               (return-from log-in (values connection user)))
             (unless phone-number
               (setf phone-number (prompt "Phone number: ")))
             (let ((sent nil))
               ;; The number may belong to another data centre; the first
               ;; request is where Telegram says so.
               (loop
                 (handler-case
                     (progn (setf sent (send-login-code connection phone-number))
                            (return))
                   (mt:remote-rpc-error (error)
                     (let ((elsewhere (migration-data-center
                                       (mt:remote-rpc-error-message error))))
                       (unless elsewhere (error error))
                       (format stream "~&~A: moving to dc~D~%"
                               (mt:remote-rpc-error-message error) elsewhere)
                       (setf dc-id elsewhere)
                       (open-connection dc-id test nil)))))
               (format stream "~&code sent, ~(~A~)~%"
                       (tl:tl-name (tl:tl-value sent :type)))
               (let ((authorization
                       (handler-case
                           (sign-in connection phone-number
                                    (tl:tl-value sent :phone-code-hash)
                                    (funcall read-code sent))
                         (mt:remote-rpc-error (error)
                           (unless (string= "SESSION_PASSWORD_NEEDED"
                                            (mt:remote-rpc-error-message error))
                             (error error))
                           (format stream "~&this account has a password~%")
                           (check-password
                            connection
                            (or password
                                (funcall read-password
                                         (invoke connection
                                                 :account.get-password))))))))
                 (when (eq :auth.authorization-sign-up-required
                           (tl:tl-name authorization))
                   (error 'login-failed
                          :detail "this number has no account; sign-up is not
implemented"))
                 (setf user (tl:tl-value authorization :user))))
             (save-session connection session-file)
             (format stream "~&logged in as ~A~%session saved to ~A~%"
                     (user-label user) (merge-pathnames session-file))
             (values connection user))
        (when (and connection (null user))
          (net:close-mtproto-connection connection))))))

(defun log-out (connection &optional (session-file *session-file*))
  "End the session on Telegram's side and forget the stored key."
  (prog1 (invoke connection :auth.log-out)
    (let ((path (probe-file (merge-pathnames session-file))))
      (when path (delete-file path)))))

;;;; Logging in without a terminal
;;;;
;;;; LOG-IN reads the code from whoever is at the keyboard, which is no use
;;;; to a script, a web form, or anything else where the code arrives minutes
;;;; later and in another process.  Splitting it in two makes the wait
;;;; somebody else's problem: BEGIN-LOGIN sends the code and writes the
;;;; authorization key and the pending phone_code_hash to the session file,
;;;; and COMPLETE-LOGIN picks both up again.

(defun connect-stored (stored &key dc-id test)
  "Open a connection, reusing a stored authorization key when it belongs to
the data centre we are opening.

The session id is deliberately *not* reused.  It is only half the session
state: the server also remembers how far the sequence numbers have got, and
resuming an id with the counters back at zero earns a bad_msg_notification
rather than an answer.  The authorization key is the part worth keeping; a
fresh session over it costs one extra round trip and nothing else."
  (let* ((dc-id (or dc-id (getf stored :dc-id) 2))
         (test (if stored (getf stored :test) test))
         (connection (net:open-mtproto-connection :dc-id dc-id :test test))
         (material (when (and stored (eql dc-id (getf stored :dc-id)))
                     (stored-material stored))))
    (net:establish-session connection
                           (or material (net:create-auth-key connection)))
    connection))

(defun begin-login (phone-number &key (dc-id 2) test
                                      (application (or *application*
                                                       (application-from-environment)))
                                      (session-file *session-file*)
                                      (stream *standard-output*))
  "Ask Telegram to send a login code to PHONE-NUMBER, and save everything
COMPLETE-LOGIN will need.  Follows a PHONE_MIGRATE to the data centre that
owns the number.  Returns the auth.sentCode."
  (let* ((*application* application)
         (connection nil))
    (unwind-protect
         (loop
           (setf connection (connect-stored nil :dc-id dc-id :test test))
           (format stream "~&connected to ~A~%" connection)
           (handler-case
               (let ((sent (send-login-code connection phone-number)))
                 (save-session connection session-file
                               :pending-phone phone-number
                               :pending-code-hash (tl:tl-value
                                                   sent :phone-code-hash))
                 (format stream "~&code sent by ~(~A~) to ~A~%~
                                   session saved to ~A~%"
                         (tl:tl-name (tl:tl-value sent :type)) phone-number
                         (merge-pathnames session-file))
                 (return sent))
             (mt:remote-rpc-error (error)
               (let ((elsewhere (migration-data-center
                                 (mt:remote-rpc-error-message error))))
                 (unless elsewhere (error error))
                 (format stream "~&~A: moving to dc~D~%"
                         (mt:remote-rpc-error-message error) elsewhere)
                 (setf dc-id elsewhere)
                 (net:close-mtproto-connection connection)
                 (setf connection nil)))))
      (when connection (net:close-mtproto-connection connection)))))

(defun complete-password (password &key (application (or *application*
                                                        (application-from-environment)))
                                        (session-file *session-file*)
                                        (stream *standard-output*))
  "Answer a two-factor challenge and finish a login that stopped at
SESSION_PASSWORD_NEEDED.

The code has already been accepted at that point, so this needs no new one --
only the stored authorization key, which is why it can run in its own
process."
  (let* ((*application* application)
         (stored (or (load-session session-file)
                     (error 'login-failed :detail "no login is in progress")))
         (connection (connect-stored stored))
         (user nil))
    (unwind-protect
         (let ((authorization (check-password connection password)))
           (setf user (tl:tl-value authorization :user))
           (save-session connection session-file)
           (format stream "~&logged in as ~A~%" (user-label user))
           (values connection user))
      (unless user (net:close-mtproto-connection connection)))))

(defun complete-login (code &key password
                                 (application (or *application*
                                                  (application-from-environment)))
                                 (session-file *session-file*)
                                 (stream *standard-output*))
  "Finish the login BEGIN-LOGIN started, using CODE.  Returns the connection
and the user."
  (let* ((*application* application)
         (stored (or (load-session session-file)
                     (error 'login-failed :detail "no login is in progress")))
         (phone-number (or (getf stored :pending-phone)
                           (error 'login-failed
                                  :detail "the stored session has no pending login")))
         (connection (connect-stored stored))
         (user nil))
    (unwind-protect
         (let ((authorization
                 (handler-case
                     (sign-in connection phone-number
                              (getf stored :pending-code-hash) code)
                   (mt:remote-rpc-error (error)
                     (unless (string= "SESSION_PASSWORD_NEEDED"
                                      (mt:remote-rpc-error-message error))
                       (error error))
                     (format stream "~&this account has a password~%")
                     (check-password
                      connection
                      (or password
                          (funcall #'default-password-reader
                                   (invoke connection :account.get-password))))))))
           (when (eq :auth.authorization-sign-up-required
                     (tl:tl-name authorization))
             (error 'login-failed :detail "this number has no account"))
           (setf user (tl:tl-value authorization :user))
           (save-session connection session-file)
           (format stream "~&logged in as ~A~%" (user-label user))
           (values connection user))
      (unless user (net:close-mtproto-connection connection)))))
