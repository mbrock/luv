;;;; Calling Telegram.
;;;;
;;;; Between an MTProto session and an API method there are two envelopes.
;;;; Every session must announce, once, what client it is and what schema
;;;; layer it speaks -- initConnection inside invokeWithLayer -- and the
;;;; server answers in the dialect it was told.  Get either wrong and the
;;;; first request comes back as CONNECTION_LAYER_INVALID or API_ID_INVALID
;;;; rather than as an answer.
;;;;
;;;; So INVOKE is where a request stops being bytes and becomes a call: it
;;;; wraps the first one, sends, waits, sees through any compression, decodes
;;;; the answer with the generated schema, and signals the server's rpc_error
;;;; as a Lisp condition.

(defpackage #:telegram.client
  (:use #:cl)
  (:local-nicknames (#:octets #:telegram.octets)
                    (#:tl #:telegram.tl)
                    (#:crypto #:telegram.crypto)
                    (#:mt #:telegram)
                    (#:net #:telegram.net))
  (:documentation
   "The Telegram API over an MTProto connection: application identity, layer
negotiation, and one INVOKE that turns a schema object into an answer.")
  (:export #:application
           #:make-application
           #:application-api-id
           #:application-api-hash
           #:application-device-model
           #:application-system-version
           #:application-app-version
           #:application-system-language
           #:application-language-pack
           #:application-language
           #:application-layer
           #:*application*
           #:missing-credentials
           ;; credentials and stored sessions
           #:application-from-environment
           #:*credential-files*
           #:read-dotenv
           #:credential
           #:*session-file*
           #:save-session
           #:load-session
           #:stored-material
           ;; logging in
           #:login-failed
           #:log-in
           #:begin-login
           #:complete-login
           #:complete-password
           #:connect-stored
           #:log-out
           #:authorized-p
           #:authorized-user
           #:logged-in-user
           #:user-label
           #:default-code-reader
           #:default-password-reader
           #:send-login-code
           #:sign-in
           #:check-password
           #:migration-data-center
           #:initial-query
           #:invoke
           #:invoke-raw
           #:decode-api-result
           #:query-definition
           #:connect
           #:with-telegram))

(in-package #:telegram.client)

(define-condition missing-credentials (mt:mtproto-error)
  ((variable :initarg :variable :reader missing-credentials-variable))
  (:report (lambda (condition stream)
             (format stream "No Telegram api_id: set ~A, or pass :API-ID.~@
Credentials come from https://my.telegram.org/apps."
                     (missing-credentials-variable condition))))
  (:documentation
   "An application identity is needed and none was configured.  Telegram
issues these per developer; there is no anonymous access to the API, though
the authorization key itself needs none."))

(defclass application ()
  ((api-id :initarg :api-id :reader application-api-id)
   (api-hash :initarg :api-hash :initform "" :reader application-api-hash
             :documentation
             "Needed only for the login methods; ordinary calls want the id.")
   (device-model :initarg :device-model :initform "luv"
                 :reader application-device-model)
   (system-version :initarg :system-version :initform "Common Lisp"
                   :reader application-system-version)
   (app-version :initarg :app-version :initform "telegram 0.0.1"
                :reader application-app-version)
   (system-language :initarg :system-language :initform "en"
                    :reader application-system-language)
   (language-pack :initarg :language-pack :initform ""
                  :reader application-language-pack)
   (language :initarg :language :initform "en" :reader application-language)
   (layer :initarg :layer :initform mt::+api-layer+ :reader application-layer))
  (:documentation
   "Who this client says it is.  Telegram wants all of it on the first
request of every session, and shows some of it to the user in their active
sessions list."))

(defmethod print-object ((application application) stream)
  (print-unreadable-object (application stream :type t)
    (format stream "~A layer ~D" (application-app-version application)
            (application-layer application))))

(defun make-application (&rest initargs &key api-id &allow-other-keys)
  "An application identity.  API-ID is the one part Telegram will not invent
for you."
  (unless api-id
    (error 'missing-credentials :variable "TELEGRAM_API_ID"))
  (apply #'make-instance 'application initargs))

(defvar *application* nil
  "The application identity INVOKE uses when none is given.")

;;;; The two envelopes

(defun initial-query (query application)
  "QUERY wrapped in initConnection and invokeWithLayer, which is what the
first request of a session has to be."
  (tl:make-tl :invoke-with-layer
              :layer (application-layer application)
              :query (tl:make-tl
                      :init-connection
                      :api-id (application-api-id application)
                      :device-model (application-device-model application)
                      :system-version (application-system-version application)
                      :app-version (application-app-version application)
                      :system-lang-code (application-system-language application)
                      :lang-pack (application-language-pack application)
                      :lang-code (application-language application)
                      :query query)))

(defun decode-api-result (octets &key specification)
  "Decode a result, seeing through any compression the server applied.

SPECIFICATION is the result type the schema promised.  It matters for the
methods that return a bare Vector<T>: TL boxes the vector but not what it
holds, so the constructor id alone does not say how to read the elements.
For everything else the id on the wire decides, and this is belt and braces."
  (let ((bytes (mt:unwrap-gzip octets)))
    (if specification
        (let ((reader (tl:make-tl-reader bytes)))
          (prog1 (tl:read-tl specification reader)
            (tl:expect-tl-end reader)))
        (tl:decode-tl-octets bytes))))

(defun query-definition (query)
  "The schema definition behind QUERY, when it has one."
  (typecase query
    (tl:tl-record (tl:tl-record-definition query))
    (keyword (tl:find-tl-definition query :errorp nil))))

(defun invoke-raw (connection query &key name)
  "Send QUERY -- a schema record, an MTProto object, or already-encoded bytes
-- and return the raw answer.  Wraps the session's first request; later ones
go bare."
  (let* ((session (net:connection-session connection))
         (application (or *application*
                          (error 'missing-credentials
                                 :variable "TELEGRAM_API_ID")))
         (wrapped (if (mt:session-initialized-p session)
                      query
                      (initial-query query application)))
         (definition (query-definition query))
         (name (or name (and definition (tl:tl-definition-name definition)))))
    (prog1 (net:connection-invoke connection (tl:encode-tl-octets wrapped)
                                  :name name)
      ;; Only mark the session initialized once a request has come back: a
      ;; wrapper the server never saw would leave later requests unannounced.
      (setf (mt:session-initialized-p session) t))))

(defun invoke (connection query &rest fields)
  "Call QUERY on CONNECTION and return the decoded answer.

QUERY is a record, or a keyword naming one, in which case FIELDS build it:

  (invoke connection :help.get-config)
  (invoke connection :messages.get-history :peer peer :limit 20)

An rpc_error from the server is signalled as REMOTE-RPC-ERROR."
  (let* ((query (if (keywordp query) (apply #'tl:make-tl query fields) query))
         (definition (query-definition query)))
    (decode-api-result
     (invoke-raw connection query)
     :specification (when definition
                      (tl:tl-definition-result-specification definition)))))

;;;; Getting there

(defun connect (&key (dc-id 2) test host port
                     (transport (make-instance 'mt:abridged-transport))
                     material session-id (application *application*))
  "Open a connection, create an authorization key unless MATERIAL supplies
one, and put a session over it.  Returns the connection and the material,
which is worth keeping: it is the credential that makes the next connection
skip the handshake."
  (let* ((*application* (or application *application*))
         (connection (net:open-mtproto-connection :dc-id dc-id :test test
                                                  :host host :port port
                                                  :transport transport))
         (material (or material (net:create-auth-key connection))))
    (net:establish-session connection material :session-id session-id)
    (values connection material)))

(defmacro with-telegram ((connection &rest options) &body body)
  "Connect, run BODY, and close however BODY leaves."
  `(let ((,connection (connect ,@options)))
     (unwind-protect (progn ,@body)
       (net:close-mtproto-connection ,connection))))
