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

(define-condition password-required (login-failed)
  ((hint :initarg :hint :initform nil :reader password-required-hint))
  (:default-initargs :detail "this account has a password")
  (:documentation
   "The code was accepted but the account has two-factor auth, and the caller
asked not to be prompted for it.  COMPLETE-PASSWORD finishes the login."))

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

(defun save-session (connection &optional (pathname *session-file*))
  "Write CONNECTION's authorization key and session identity where
LOAD-SESSION will find them.

Only a finished login is written.  A login in progress lives in a
CODE-LOGIN or QR-LOGIN object for as long as the process that started it,
and no longer: a code is good for a minute or two, so a half-finished login
that outlived a restart is not a login to resume but a dead end to forget."
  (let ((session (net:connection-session connection))
        (path (merge-pathnames pathname)))
    (with-open-file (stream path :direction :output :if-exists :supersede
                                 :if-does-not-exist :create
                                 :external-format :utf-8)
      (let ((*package* (find-package :keyword)))
        (prin1 (list :dc-id (net:connection-dc-id connection)
                     :test (net:connection-test-p connection)
                     :auth-key (octets:octets-hex
                                (mt:auth-key-data (mt:session-key session)))
                     :server-salt (mt:session-server-salt session)
                     :time-offset (mt:session-time-offset session)
                     :session-id (mt:session-id session))
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

(defun authorized-user (&optional connection)
  "The user this session is logged in as, or NIL if it is not logged in.

Asked by making the cheapest authorized call there is: an unauthorized
session gets a 401 back, which is an answer rather than a failure."
  (handler-case (logged-in-user connection)
    (mt:remote-rpc-error (error)
      (unless (= 401 (mt:remote-rpc-error-code error))
        (error error))
      nil)))

(defun authorized-p (&optional connection)
  "Is this session logged in?"
  (and (authorized-user connection) t))

(defun logged-in-user (&optional connection)
  "The user this session is logged in as.  Also refreshes *USER*."
  (let ((users (invoke (current-connection connection) :users.get-users
                       :id (vector (tl:make-tl :input-user-self)))))
    (when (plusp (length users))
      (setf *user* (elt users 0)))))

(defun user-label (user)
  "A short human rendering of a user, for the transcript."
  (format nil "~@[~A~]~@[ ~A~]~@[ (@~A)~]~@[ id ~D~]"
          (tl:tl-value user :first-name) (tl:tl-value user :last-name)
          (tl:tl-value user :username) (tl:tl-value user :id)))

(defun send-login-code (phone-number &optional connection)
  "Ask Telegram to send a login code to PHONE-NUMBER.  Returns the
auth.sentCode, whose phone_code_hash SIGN-IN needs."
  (invoke (current-connection connection) :auth.send-code
          :phone-number phone-number
          :api-id (application-api-id *application*)
          :api-hash (application-api-hash *application*)
          :settings (tl:make-tl :code-settings)))

(defun sign-in (phone-number code-hash code &optional connection)
  "Complete a login with the code Telegram sent."
  (invoke (current-connection connection) :auth.sign-in
          :phone-number phone-number
          :phone-code-hash code-hash
          :phone-code code))

(defun check-password (password &key connection entropy)
  "Prove knowledge of the account's two-factor password, by SRP."
  (let* ((connection (current-connection connection))
         (entropy (or entropy octets:*entropy*))
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
               (return-from log-in (values (make-current connection user) user)))
             (unless phone-number
               (setf phone-number (prompt "Phone number: ")))
             (let ((sent nil))
               ;; The number may belong to another data centre; the first
               ;; request is where Telegram says so.
               (loop
                 (handler-case
                     (progn (setf sent (send-login-code phone-number connection))
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
                           (sign-in phone-number
                                    (tl:tl-value sent :phone-code-hash)
                                    (funcall read-code sent)
                                    connection)
                         (mt:remote-rpc-error (error)
                           (unless (string= "SESSION_PASSWORD_NEEDED"
                                            (mt:remote-rpc-error-message error))
                             (error error))
                           (format stream "~&this account has a password~%")
                           (check-password
                            (or password
                                (funcall read-password
                                         (invoke connection
                                                 :account.get-password)))
                            :connection connection)))))
                 (when (eq :auth.authorization-sign-up-required
                           (tl:tl-name authorization))
                   (error 'login-failed
                          :detail "this number has no account; sign-up is not
implemented"))
                 (setf user (tl:tl-value authorization :user))))
             (save-session connection session-file)
             (format stream "~&logged in as ~A~%session saved to ~A~%"
                     (user-label user) (merge-pathnames session-file))
             (values (make-current connection user) user))
        (when (and connection (null user))
          (net:close-mtproto-connection connection))))))

(defun log-out (&key connection (session-file *session-file*))
  "End the session on Telegram's side and forget the stored key."
  (prog1 (invoke (current-connection connection) :auth.log-out)
    (let ((path (probe-file (merge-pathnames session-file))))
      (when path (delete-file path)))))

;;;; QR login
;;;;
;;;; A login token is deliberately an object rather than a string: it owns a
;;;; live unauthorised connection, can be refreshed, and sometimes has to move
;;;; itself to another data centre.  The three constructors Telegram returns
;;;; are its closed, externally-owned vocabulary, so TOKEN-RESULT quite
;;;; properly CASEs on their TL names.

(defclass qr-login ()
  ((connection :initarg :connection :accessor qr-login-connection)
   (application :initarg :application :reader qr-login-application)
   (session-file :initarg :session-file :reader qr-login-session-file)
   (test :initarg :test :reader qr-login-test-p)
   (token :initform nil :accessor qr-login-token)
   (expires :initform nil :accessor qr-login-expires)
   (password-hint :initform nil :accessor qr-login-password-hint
                  :documentation
                  "NIL until the accepted token stops at the account's
two-factor password; then the account's hint string, possibly empty.  The
token phase is over at that point: ask the person for their password and
finish with COMPLETE-PASSWORD over this same connection.")
   (user :initform nil :accessor qr-login-user))
  (:documentation
   "One pending Telegram QR login, including the connection which receives
UPDATE-LOGIN-TOKEN and eventually becomes the authenticated session."))

(defmethod print-object ((login qr-login) stream)
  (print-unreadable-object (login stream :type t)
    (format stream "~A~@[ expires ~D~]~@[ as ~A~]"
            (qr-login-connection login) (qr-login-expires login)
            (and (qr-login-user login) (user-label (qr-login-user login))))))

(defparameter +base64url-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
  "The unpadded URL-safe base64 alphabet Telegram puts in tg:// login links.")

(defun base64url-octets (octets)
  "Encode OCTETS as unpadded URL-safe base64."
  (let* ((length (length octets))
         (result (make-string (ceiling (* 8 length) 6)))
         (out 0))
    (loop for start from 0 below length by 3
          for first = (aref octets start)
          for second = (and (< (1+ start) length) (aref octets (1+ start)))
          for third = (and (< (+ start 2) length) (aref octets (+ start 2)))
          for word = (logior (ash first 16) (ash (or second 0) 8) (or third 0))
          do (flet ((emit (shift)
                     (setf (char result out)
                           (char +base64url-alphabet+
                                 (ldb (byte 6 shift) word)))
                     (incf out)))
               (emit 18)
               (emit 12)
               (when second (emit 6))
               (when third (emit 0))))
    result))

(defun qr-login-uri (login)
  "The tg:// URL to encode as LOGIN's QR code, or NIL after it completes."
  (when (qr-login-token login)
    (format nil "tg://login?token=~A" (base64url-octets (qr-login-token login)))))

(defparameter *qr-encoder* "qrencode"
  "The program that turns a URI into a module grid.  Its ASCII output is two
characters per module with a four-module quiet zone, which is a grid to parse
rather than a picture to decode.")

(defun qr-code-modules (text)
  "TEXT as a square bit array: 1 where the QR code is dark, 0 where it is
light, quiet zone included.  NIL if the encoder is not there.

This shells out.  A QR encoder is Reed-Solomon over GF(256) and a mask
search, which is a fine thing to write one day and no part of logging in;
qrencode is in the development shell and prints a grid on request."
  (let* ((output (with-output-to-string (out)
                   (handler-case
                       (let ((process (sb-ext:run-program
                                       *qr-encoder* (list "-t" "ASCII" "-o" "-" text)
                                       :search t :wait t :output out
                                       :error nil)))
                         (unless (zerop (sb-ext:process-exit-code process))
                           (return-from qr-code-modules nil)))
                     (error () (return-from qr-code-modules nil)))))
         (rows (with-input-from-string (in output)
                 (loop for line = (read-line in nil nil)
                       while line
                       when (plusp (length line)) collect line))))
    (when rows
      (let* ((size (floor (length (first rows)) 2))
             (modules (make-array (list (length rows) size)
                                  :element-type 'bit :initial-element 0)))
        (loop for line in rows
              for row from 0
              do (dotimes (column size)
                   (when (and (< (* 2 column) (length line))
                              (char= #\# (char line (* 2 column))))
                     (setf (aref modules row column) 1))))
        modules))))

(defun qr-login-modules (login)
  "LOGIN's current code as a bit array, or NIL when it has none to show."
  (let ((uri (qr-login-uri login)))
    (and uri (qr-code-modules uri))))

(defgeneric present-qr-login (login &optional stream)
  (:documentation
   "Present LOGIN's current QR code.  Applications can add a method for
their own surface; the standard method uses qrencode when it is available and
always prints the tg:// URL as a useful fallback."))

(defmethod present-qr-login ((login qr-login) &optional (stream *standard-output*))
  (let ((uri (qr-login-uri login)))
    (when uri
      (format stream "~&Scan this Telegram login code (expires at Unix time ~D):~%"
              (qr-login-expires login))
      (handler-case
          (let ((process (sb-ext:run-program
                          "qrencode" (list "-t" "UTF8" "-o" "-" uri)
                          :search t :wait t :output stream :error stream)))
            (unless (zerop (sb-ext:process-exit-code process))
              (format stream "~A~%" uri)))
        (error ()
          (format stream "~A~%" uri)))
      (finish-output stream)))
  login)

(defun qr-login-token-result (login result)
  "Install RESULT from auth.exportLoginToken or auth.importLoginToken.
Returns LOGIN.  A migrate result opens an unauthorised session at its target
data centre and imports the single-use token there."
  (case (tl:tl-name result)
    (:auth.login-token
     (setf (qr-login-token login) (tl:tl-value result :token)
           (qr-login-expires login) (tl:tl-value result :expires))
     login)
    (:auth.login-token-success
     (setf (qr-login-token login) nil
           (qr-login-expires login) nil
           (qr-login-user login) (tl:tl-value (tl:tl-value result :authorization) :user))
     (save-session (qr-login-connection login) (qr-login-session-file login))
     (setf *application* (qr-login-application login))
     (make-current (qr-login-connection login) (qr-login-user login))
     login)
    (:auth.login-token-migrate-to
     (let* ((old (qr-login-connection login))
            (connection (connect-stored nil :dc-id (tl:tl-value result :dc-id)
                                        :test (qr-login-test-p login))))
       (setf (qr-login-connection login) connection)
       (net:close-mtproto-connection old)
       (qr-login-token-step
        login
        (lambda ()
          (let ((*application* (qr-login-application login)))
            (invoke connection :auth.import-login-token
                    :token (tl:tl-value result :token)))))))
    (otherwise
     (error 'login-failed
            :detail (format nil "unexpected QR login result ~(~A~)"
                            (tl:tl-name result))))))

(defun note-qr-password-gate (login)
  "The phone accepted LOGIN's token, and the account has a password.

The token phase is over -- there is nothing left to present or refresh --
and the connection is one SRP exchange away from an authorization.  Remember
the account's hint so a caller can ask for the password, and finish through
COMPLETE-PASSWORD."
  (setf (qr-login-token login) nil
        (qr-login-expires login) nil
        (qr-login-password-hint login)
        (or (ignore-errors
              (tl:tl-value (invoke (qr-login-connection login)
                                   :account.get-password)
                           :hint))
            ""))
  login)

(defun qr-login-token-step (login thunk)
  "Install THUNK's token-phase result on LOGIN, stopping at the password.

SESSION_PASSWORD_NEEDED is not a failure of the login but its next stage:
the phone has already said yes."
  (handler-case (qr-login-token-result login (funcall thunk))
    (mt:remote-rpc-error (error)
      (unless (string= "SESSION_PASSWORD_NEEDED"
                       (mt:remote-rpc-error-message error))
        (error error))
      (note-qr-password-gate login))))

(defun refresh-qr-login (login)
  "Ask Telegram for LOGIN's current token, following a data-centre migration.
The returned QR-LOGIN is ready to present, has completed, or is waiting on
the account's password."
  (let ((*application* (qr-login-application login)))
    (qr-login-token-step
     login
     (lambda ()
       (invoke (qr-login-connection login) :auth.export-login-token
               :api-id (application-api-id *application*)
               :api-hash (application-api-hash *application*)
               :except-ids #())))))

(defun begin-qr-login (&key (dc-id 2) test
                             (application (or *application*
                                              (application-from-environment)))
                             (session-file *session-file*))
  "Create and return a pending QR login.  Call QR-LOGIN-URI to draw its code,
then WAIT-FOR-QR-LOGIN to receive the authorization after a mobile Telegram
client scans and accepts it."
  (let ((*application* application)
        (connection nil))
    (unwind-protect
         (let ((login (make-instance 'qr-login :application application
                                               :session-file session-file :test test
                                               :connection
                                               (setf connection
                                                     (connect-stored nil :dc-id dc-id
                                                                          :test test)))))
           (refresh-qr-login login)
           (setf connection nil)
           login)
      (when connection (net:close-mtproto-connection connection)))))

(defun new-login-token-event-p (event)
  (and (eq :update (first event)) (eq :update-login-token (second event))))

(defun poll-qr-login (login &key (timeout 1))
  "Wait up to TIMEOUT seconds for the phone to accept LOGIN's code.

Returns LOGIN, whose PASSWORD-HINT or USER says what happened.  A quiet
socket is the ordinary case -- nothing has been scanned yet -- and an
expired token is refreshed whether or not anything arrived, so a code on
screen is always one a phone can still take.

Only the wait itself runs at TIMEOUT: the connection's own read timeout is
put back before the export that follows an acceptance, because that call
does round trips -- possibly including a whole handshake at another data
centre -- that a polling deadline would cut off mid-login."
  (when (or (qr-login-user login) (qr-login-password-hint login))
    (return-from poll-qr-login login))
  (let* ((connection (qr-login-connection login))
         (session (net:connection-session connection))
         (before (mt:session-events session))
         (previous (net:connection-read-timeout connection))
         (accepted nil))
    (setf (net:connection-read-timeout connection) timeout)
    (unwind-protect
         (handler-case
             (progn (net:pump-connection connection)
                    (setf accepted
                          (find-if #'new-login-token-event-p
                                   (ldiff (mt:session-events session) before))))
           (net:connection-timeout () nil))
      (setf (net:connection-read-timeout connection) previous))
    (when (or accepted (qr-login-stale-p login))
      (refresh-qr-login login))
    login))

(defun qr-login-stale-p (login)
  "Has LOGIN's token passed the expiry Telegram gave it?"
  (let ((expires (qr-login-expires login)))
    (and expires (>= (octets:clock-unix-time octets:*clock*) expires))))

(defun wait-for-qr-login (login &key (stream *standard-output*) (present t))
  "Pump LOGIN until a mobile client accepts its code, then return connection
and user.  A quiet 30-second socket is the normal token-expiry path: obtain
and present a fresh code instead of treating that silence as a failure."
  (when present (present-qr-login login stream))
  (loop until (qr-login-user login)
        when (qr-login-password-hint login)
          do (let ((hint (qr-login-password-hint login)))
               (format stream "~&this account has a password~@[ (hint: ~A)~]~%"
                       (and (plusp (length hint)) hint))
               (complete-password
                login (prompt-quietly "Password: ") :stream stream))
        else
          do (let* ((session (net:connection-session (qr-login-connection login)))
                    (old-events (mt:session-events session)))
               (handler-case
                   (net:pump-connection (qr-login-connection login))
                 (net:connection-timeout ()
                   (refresh-qr-login login)
                   (when present (present-qr-login login stream))))
               (when (find-if #'new-login-token-event-p
                              (ldiff (mt:session-events session) old-events))
                 (refresh-qr-login login)
                 (when present (present-qr-login login stream)))))
  (values (qr-login-connection login) (qr-login-user login)))

(defun log-in-with-qr (&key (dc-id 2) test
                            (application (or *application*
                                             (application-from-environment)))
                            (session-file *session-file*)
                            (stream *standard-output*))
  "Display a Telegram QR login and wait for it to be accepted.  Returns the
connection and user, and saves the resulting authorization key."
  (let ((login (begin-qr-login :dc-id dc-id :test test :application application
                               :session-file session-file)))
    (unwind-protect
         (wait-for-qr-login login :stream stream)
      (unless (qr-login-user login)
        (net:close-mtproto-connection (qr-login-connection login))))))

;;;; Logging in without a terminal
;;;;
;;;; LOG-IN reads the code from whoever is at the keyboard, which is no use
;;;; to a UI, a web form, or anything else that has to go and do something
;;;; else while the code arrives.  Splitting it in two makes the wait
;;;; somebody else's problem: BEGIN-LOGIN sends the code and returns a
;;;; CODE-LOGIN, and COMPLETE-LOGIN and COMPLETE-PASSWORD are handed that
;;;; same object.
;;;;
;;;; Like QR-LOGIN, it owns a live connection and nothing on disk.  A login
;;;; in progress is a conversation, not a record: the code expires in about a
;;;; minute, so one that outlives its process is not something to resume.
;;;; Persisting it only ever produced a session file that asked forever for a
;;;; code Telegram had long since forgotten.  Let the object be lost with the
;;;; process, and the next run starts cleanly.

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

(defclass code-login ()
  ((connection :initarg :connection :accessor code-login-connection)
   (application :initarg :application :reader code-login-application)
   (session-file :initarg :session-file :reader code-login-session-file)
   (phone-number :initarg :phone-number :reader code-login-phone-number)
   (code-hash :initarg :code-hash :reader code-login-code-hash)
   (sent :initarg :sent :reader code-login-sent)
   (user :initform nil :accessor code-login-user))
  (:documentation
   "One pending Telegram code login: the connection that was sent the code,
the phone_code_hash that answers it, and the auth.sentCode saying how it was
delivered.  Live only for as long as the process that made it."))

(defmethod print-object ((login code-login) stream)
  (print-unreadable-object (login stream :type t)
    (format stream "~A ~A~@[ as ~A~]"
            (code-login-connection login) (code-login-phone-number login)
            (and (code-login-user login) (user-label (code-login-user login))))))

(defun code-login-delivery (login)
  "How Telegram said it would deliver the code, as a keyword like :SMS."
  (let ((name (string (tl:tl-name (tl:tl-value (code-login-sent login) :type)))))
    (intern (subseq name (length "AUTH.SENT-CODE-TYPE-")) :keyword)))

(defgeneric abandon-login (login)
  (:documentation
   "Give up on a login in progress and close whatever it was holding open.
Safe to call on a login that already finished.")
  (:method ((login code-login))
    (let ((connection (code-login-connection login)))
      (when (and connection (null (code-login-user login)))
        (net:close-mtproto-connection connection)
        (setf (code-login-connection login) nil)))
    nil)
  (:method ((login qr-login))
    (let ((connection (qr-login-connection login)))
      (when (and connection (null (qr-login-user login)))
        (net:close-mtproto-connection connection)
        (setf (qr-login-connection login) nil)))
    nil)
  (:method ((login null))
    nil))

(defun begin-login (phone-number &key (dc-id 2) test
                                      (application (or *application*
                                                       (application-from-environment)))
                                      (session-file *session-file*)
                                      (stream *standard-output*))
  "Ask Telegram to send a login code to PHONE-NUMBER, and return the
CODE-LOGIN that COMPLETE-LOGIN finishes.  Follows a PHONE_MIGRATE to the data
centre that owns the number.

The returned login holds an open connection: finish it, or ABANDON-LOGIN it."
  (let* ((*application* application)
         (connection nil)
         (login nil))
    (unwind-protect
         (loop
           (setf connection (connect-stored nil :dc-id dc-id :test test))
           (format stream "~&connected to ~A~%" connection)
           (handler-case
               (let ((sent (send-login-code phone-number connection)))
                 (setf login
                       (make-instance 'code-login
                                      :connection connection
                                      :application application
                                      :session-file session-file
                                      :phone-number phone-number
                                      :code-hash (tl:tl-value sent :phone-code-hash)
                                      :sent sent))
                 (setf connection nil)
                 (format stream "~&code sent by ~(~A~) to ~A~%"
                         (tl:tl-name (tl:tl-value sent :type)) phone-number)
                 (return login))
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

(defun finish-code-login (login authorization stream)
  "Adopt the authorization a completed code login earned."
  (when (eq :auth.authorization-sign-up-required (tl:tl-name authorization))
    (error 'login-failed :detail "this number has no account; sign-up is not
implemented"))
  (let ((user (tl:tl-value authorization :user))
        (connection (code-login-connection login)))
    (setf (code-login-user login) user)
    (save-session connection (code-login-session-file login))
    (setf *application* (code-login-application login))
    (when stream (format stream "~&logged in as ~A~%" (user-label user)))
    (values (make-current connection user) user)))

(defun complete-login (login code &key password
                                       (password-reader #'default-password-reader)
                                       (stream *standard-output*))
  "Finish the login BEGIN-LOGIN started, using CODE.  Returns the connection
and the user.

If the account has a password and none is given, PASSWORD-READER is asked
for it with the account.password object; a NIL reader signals
PASSWORD-REQUIRED instead, for a caller with no terminal that will come back
through COMPLETE-PASSWORD with the same LOGIN."
  (let ((*application* (code-login-application login))
        (connection (or (code-login-connection login)
                        (error 'login-failed :detail "this login was abandoned"))))
    (finish-code-login
     login
     (handler-case
         (sign-in (code-login-phone-number login) (code-login-code-hash login)
                  code connection)
       (mt:remote-rpc-error (error)
         (unless (string= "SESSION_PASSWORD_NEEDED"
                          (mt:remote-rpc-error-message error))
           (error error))
         (when stream (format stream "~&this account has a password~%"))
         (let ((state (invoke connection :account.get-password)))
           (unless (or password password-reader)
             (error 'password-required :hint (tl:tl-value state :hint)))
           (check-password (or password (funcall password-reader state))
                           :connection connection))))
     stream)))

(defgeneric complete-password (login password &key stream)
  (:documentation
   "Answer the two-factor challenge LOGIN stopped at, and finish it.

The code or token has already been accepted at that point, so this needs no
new one -- only the connection the login is still holding."))

(defmethod complete-password
    ((login code-login) password &key (stream *standard-output*))
  (let ((*application* (code-login-application login))
        (connection (or (code-login-connection login)
                        (error 'login-failed :detail "this login was abandoned"))))
    (finish-code-login login (check-password password :connection connection)
                       stream)))

(defmethod complete-password
    ((login qr-login) password &key (stream *standard-output*))
  (let* ((*application* (qr-login-application login))
         (connection (or (qr-login-connection login)
                         (error 'login-failed :detail "this login was abandoned")))
         (authorization (check-password password :connection connection))
         (user (tl:tl-value authorization :user)))
    (setf (qr-login-password-hint login) nil
          (qr-login-user login) user)
    (save-session connection (qr-login-session-file login))
    (setf *application* (qr-login-application login))
    (when stream (format stream "~&logged in as ~A~%" (user-label user)))
    (make-current connection user)
    login))

(defun resume (&key (session-file *session-file*)
                    (application (or *application*
                                     (application-from-environment)))
                    (stream nil))
  "Reconnect with the stored authorization key and make it current.

This is the ordinary way in once a login has happened: no handshake, no code,
one round trip to find out who we are.

  (resume)
  (invoke :help.get-nearest-dc)"
  (let* ((*application* application)
         (stored (or (load-session session-file)
                     (error 'login-failed
                            :detail (format nil "no session stored at ~A"
                                            (merge-pathnames session-file)))))
         (connection (connect-stored stored)))
    (setf *application* application)
    (make-current connection)
    (let ((user (authorized-user connection)))
      (unless user
        (disconnect connection)
        (error 'login-failed :detail "the stored session is no longer authorized"))
      (when stream (format stream "~&resumed as ~A~%" (user-label user)))
      (values connection user))))
