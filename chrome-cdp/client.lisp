(in-package #:chrome-cdp)

;;; Conditions

(define-condition cdp-error (error)
  ((detail :initarg :detail :reader cdp-error-detail))
  (:report (lambda (condition stream)
             (format stream "Chrome CDP: ~A" (cdp-error-detail condition)))))

(define-condition chrome-not-debuggable (cdp-error)
  ((active-port-file :initarg :active-port-file :reader chrome-active-port-file))
  (:report (lambda (condition stream)
             (format stream "Chrome has no readable DevToolsActivePort at ~A."
                     (chrome-active-port-file condition)))))

(define-condition browser-closed (cdp-error)
  ((browser :initarg :browser :reader browser-closed-browser))
  (:report (lambda (condition stream)
             (format stream "The Chrome debugging connection is closed~@[ (~A)~]."
                     (cdp-error-detail condition)))))

(define-condition cdp-timeout (cdp-error)
  ((method :initarg :method :reader cdp-timeout-method)
   (seconds :initarg :seconds :reader cdp-timeout-seconds))
  (:report (lambda (condition stream)
             (format stream "Chrome did not answer ~A within ~A seconds."
                     (cdp-timeout-method condition)
                     (cdp-timeout-seconds condition)))))

(define-condition cdp-remote-error (cdp-error)
  ((method :initarg :method :reader cdp-error-method)
   (code :initarg :code :reader cdp-error-code)
   (data :initarg :data :initform nil :reader cdp-error-data))
  (:report (lambda (condition stream)
             (format stream "Chrome rejected ~A (~A): ~A"
                     (cdp-error-method condition)
                     (cdp-error-code condition)
                     (cdp-error-detail condition)))))

(define-condition javascript-error (cdp-error)
  ((expression :initarg :expression :reader javascript-error-expression)
   (details :initarg :details :reader javascript-error-details))
  (:report (lambda (condition stream)
             (format stream "JavaScript evaluation failed: ~A"
                     (cdp-error-detail condition)))))

(define-condition target-not-found (cdp-error)
  ((query :initarg :query :reader target-query))
  (:report (lambda (condition stream)
             (format stream "No Chrome target matches ~S." (target-query condition)))))

;;; JSON vocabulary

(defun json-key-name (key)
  (remove-if-not #'alphanumericp (string key)))

(defun json-value (object key)
  "Value named KEY in a CL-JSON alist, ignoring punctuation and case."
  (cdr (find key object :key #'car
             :test (lambda (wanted actual)
                     (string-equal (json-key-name wanted)
                                   (json-key-name actual))))))

(defun json-object (&rest pairs)
  (loop for (key value) on pairs by #'cddr
        collect (cons key value)))

(defclass json-false () ())

(defmethod cl-json:encode-json
    ((object json-false) &optional (stream cl-json:*json-output*))
  (declare (ignore object))
  (write-string "false" stream)
  nil)

(defparameter +json-false+ (make-instance 'json-false))

(defun json-boolean (value)
  (if value t +json-false+))

;;; Inspectable protocol objects

(defclass browser ()
  ((url :initarg :url :reader browser-url)
   (socket :initarg :socket :initform nil :accessor browser-socket)
   (default-timeout :initarg :default-timeout :initform 30
                    :accessor browser-default-timeout)
   (next-id :initform 0 :accessor browser-next-id)
   (responses :initform (make-hash-table :test #'equal) :reader browser-responses)
   (events :initform '() :accessor browser-event-queue)
   (state-lock :initform (sb-thread:make-mutex :name "Chrome CDP state")
               :reader browser-state-lock)
   (send-lock :initform (sb-thread:make-mutex :name "Chrome CDP send")
              :reader browser-send-lock)
   (ready :initform (sb-thread:make-waitqueue :name "Chrome CDP ready")
          :reader browser-ready)
   (closed-p :initform nil :accessor browser-closed-p)
   (failure :initform nil :accessor browser-failure))
  (:documentation
   "One persistent WebSocket connection to Chrome's browser CDP endpoint."))

(defmethod print-object ((browser browser) stream)
  (print-unreadable-object (browser stream :type t :identity t)
    (format stream "~:[closed~;open~] ~A"
            (browser-open-p browser) (browser-url browser))))

(defclass target ()
  ((browser :initarg :browser :reader target-browser)
   (id :initarg :id :reader target-id)
   (type :initarg :type :reader target-type)
   (title :initarg :title :reader target-title)
   (url :initarg :url :reader target-url))
  (:documentation "A snapshot of one target advertised by Chrome."))

(defmethod print-object ((target target) stream)
  (print-unreadable-object (target stream :type t :identity nil)
    (format stream "~A ~S ~A"
            (target-type target) (target-title target) (target-url target))))

(defclass session ()
  ((browser :initarg :browser :reader session-browser)
   (target :initarg :target :reader session-target)
   (id :initarg :id :reader session-id)
   (closed-p :initform nil :accessor session-closed-p))
  (:documentation
   "An attached, non-flattened CDP session for one target."))

(defmethod print-object ((session session) stream)
  (print-unreadable-object (session stream :type t :identity t)
    (format stream "~:[closed~;open~] ~S"
            (not (session-closed-p session))
            (target-title (session-target session)))))

(defclass event ()
  ((method :initarg :method :reader event-method)
   (params :initarg :params :reader event-params)
   (session-id :initarg :session-id :initform nil :reader event-session-id))
  (:documentation "One unsolicited browser- or target-session CDP event."))

(defmethod print-object ((event event) stream)
  (print-unreadable-object (event stream :type t :identity nil)
    (format stream "~A~@[ session ~A~]"
            (event-method event) (event-session-id event))))

(defun browser-open-p (browser)
  (not (browser-closed-p browser)))

;;; Chrome discovery

(defun default-chrome-active-port-file ()
  "Chrome's recommended remote-debugging rendezvous file."
  (or (uiop:getenv "CHROME_DEVTOOLS_ACTIVE_PORT_FILE")
      (namestring
       (if (uiop:os-macosx-p)
           (merge-pathnames
            #P"Library/Application Support/Google/Chrome/DevToolsActivePort"
            (user-homedir-pathname))
           (merge-pathnames #P".config/google-chrome/DevToolsActivePort"
                            (user-homedir-pathname))))))

(defun chrome-debugging-websocket-url
    (&key (active-port-file (default-chrome-active-port-file)))
  "Read Chrome's port and browser path and return its WebSocket URL."
  (handler-case
      (with-open-file (stream active-port-file)
        (let ((port (read-line stream nil nil))
              (path (read-line stream nil nil)))
          (unless (and port path (plusp (length port)) (plusp (length path)))
            (error 'chrome-not-debuggable
                   :active-port-file active-port-file :detail "incomplete rendezvous file"))
          (format nil "ws://127.0.0.1:~A~A" port path)))
    (file-error ()
      (error 'chrome-not-debuggable
             :active-port-file active-port-file :detail "file is unavailable"))))

;;; Transport routing

(defun notify-waiters (browser)
  (sb-thread:condition-broadcast (browser-ready browser)))

(defun fail-browser (browser detail)
  (sb-thread:with-mutex ((browser-state-lock browser))
    (setf (browser-failure browser) detail
          (browser-closed-p browser) t)
    (notify-waiters browser)))

(defun deliver-response (browser key response)
  (sb-thread:with-mutex ((browser-state-lock browser))
    (setf (gethash key (browser-responses browser)) response)
    (notify-waiters browser)))

(defun enqueue-event (browser method params &optional session-id)
  (sb-thread:with-mutex ((browser-state-lock browser))
    (setf (browser-event-queue browser)
          (nconc (browser-event-queue browser)
                 (list (make-instance 'event :method method :params params
                                             :session-id session-id))))
    (notify-waiters browser)))

(defun receive-session-message (browser params)
  (let* ((session-id (json-value params "sessionId"))
         (message (cl-json:decode-json-from-string
                   (json-value params "message")))
         (id (json-value message "id")))
    (if id
        (deliver-response browser (list session-id id) message)
        (enqueue-event browser (json-value message "method")
                       (json-value message "params") session-id))))

(defun receive-message (browser text)
  (handler-case
      (let* ((message (cl-json:decode-json-from-string text))
             (id (json-value message "id"))
             (method (json-value message "method"))
             (params (json-value message "params")))
        (cond
          (id (deliver-response browser id message))
          ((string= method "Target.receivedMessageFromTarget")
           (receive-session-message browser params))
          (t (enqueue-event browser method params))))
    (error (condition)
      (fail-browser browser condition))))

(defun connect-chrome (&key url
                            (active-port-file (default-chrome-active-port-file))
                            (default-timeout 30))
  "Open one persistent connection to Chrome's browser CDP endpoint.

URL overrides DevToolsActivePort discovery.  Chrome may ask the user to allow
the debugging connection once; keep the returned BROWSER alive to avoid
repeating that prompt."
  (let* ((url (or url (chrome-debugging-websocket-url
                       :active-port-file active-port-file)))
         (browser (make-instance 'browser :url url
                                          :default-timeout default-timeout))
         (socket (websocket-driver:make-client url)))
    (setf (browser-socket browser) socket)
    (websocket-driver:on :message socket
                         (lambda (message) (receive-message browser message)))
    (websocket-driver:on :error socket
                         (lambda (detail) (fail-browser browser detail)))
    (websocket-driver:on :close socket
                         (lambda (&key code reason)
                           (fail-browser browser
                                         (format nil "close ~A~@[ ~A~]" code reason))))
    (websocket-driver:start-connection socket)
    browser))

(defun close-browser (browser)
  "Begin a normal WebSocket close handshake without destroying its reader."
  (when (browser-open-p browser)
    (websocket-driver:send (browser-socket browser) ""
                           :type :close :code 1000))
  browser)

(defun next-command-id (browser)
  (sb-thread:with-mutex ((browser-send-lock browser))
    (incf (browser-next-id browser))))

(defun send-command (browser object)
  (unless (browser-open-p browser)
    (error 'browser-closed :browser browser
                           :detail (browser-failure browser)))
  (sb-thread:with-mutex ((browser-send-lock browser))
    (websocket-driver:send (browser-socket browser)
                           (cl-json:encode-json-to-string object))))

(defun monotonic-seconds ()
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun wait-for-response (browser key method timeout)
  (let ((deadline (and timeout (+ (monotonic-seconds) timeout))))
    (sb-thread:with-mutex ((browser-state-lock browser))
      (loop
        (multiple-value-bind (response present-p)
            (gethash key (browser-responses browser))
          (when present-p
            (remhash key (browser-responses browser))
            (return response)))
        (when (browser-closed-p browser)
          (error 'browser-closed :browser browser
                                 :detail (browser-failure browser)))
        (let ((remaining (and deadline (- deadline (monotonic-seconds)))))
          (when (and remaining (<= remaining 0))
            (error 'cdp-timeout :method method :seconds timeout
                                :detail "request timed out"))
          (sb-thread:condition-wait (browser-ready browser)
                                    (browser-state-lock browser)
                                    :timeout remaining))))))

(defun response-result (response method)
  (let ((remote-error (json-value response "error")))
    (when remote-error
      (error 'cdp-remote-error
             :method method
             :code (json-value remote-error "code")
             :data (json-value remote-error "data")
             :detail (json-value remote-error "message")))
    (json-value response "result")))

(defun browser-call (browser method &optional params
                                      &key (timeout (browser-default-timeout browser)))
  "Call a browser-level CDP METHOD and return its result alist."
  (let ((id (next-command-id browser)))
    (send-command browser
                  (json-object "id" id "method" method "params" params))
    (response-result (wait-for-response browser id method timeout) method)))

(defun session-call (session method &optional params
                                      &key (timeout (browser-default-timeout
                                                     (session-browser session))))
  "Call METHOD inside an attached target SESSION and return its result alist."
  (when (session-closed-p session)
    (error 'browser-closed :browser (session-browser session)
                           :detail "target session is detached"))
  (let* ((browser (session-browser session))
         (inner-id (next-command-id browser))
         (message (cl-json:encode-json-to-string
                   (json-object "id" inner-id "method" method "params" params))))
    (browser-call browser "Target.sendMessageToTarget"
                  (json-object "sessionId" (session-id session)
                               "message" message)
                  :timeout timeout)
    (response-result
     (wait-for-response browser (list (session-id session) inner-id)
                        method timeout)
     method)))

;;; Targets and pages

(defun target-from-info (browser info)
  (make-instance 'target
                 :browser browser
                 :id (json-value info "targetId")
                 :type (json-value info "type")
                 :title (json-value info "title")
                 :url (json-value info "url")))

(defun browser-targets (browser)
  "A fresh list of the targets currently advertised by BROWSER."
  (mapcar (lambda (info) (target-from-info browser info))
          (json-value (browser-call browser "Target.getTargets") "targetInfos")))

(defun target-matches-p (target type title url predicate)
  (and (or (null type) (string= type (target-type target)))
       (or (null title) (search title (or (target-title target) "")
                                :test #'char-equal))
       (or (null url) (search url (or (target-url target) "")
                              :test #'char-equal))
       (or (null predicate) (funcall predicate target))))

(defun find-target (browser &key type title url predicate)
  "Find a target by optional TYPE and TITLE/URL substrings or signal an error."
  (or (find-if (lambda (target)
                 (target-matches-p target type title url predicate))
               (browser-targets browser))
      (error 'target-not-found
             :query (list :type type :title title :url url :predicate predicate)
             :detail "no matching target")))

(defun find-page (browser &key title url predicate)
  (find-target browser :type "page" :title title :url url :predicate predicate))

(defun create-page (browser url)
  "Create a page at URL and return its TARGET."
  (let* ((result (browser-call browser "Target.createTarget"
                               (json-object "url" url)))
         (id (json-value result "targetId")))
    (make-instance 'target :browser browser :id id :type "page"
                           :title "" :url url)))

(defun close-page (target)
  (browser-call (target-browser target) "Target.closeTarget"
                (json-object "targetId" (target-id target))))

(defun attach-target (target)
  "Attach to TARGET and return a durable SESSION."
  (let ((result (browser-call
                 (target-browser target) "Target.attachToTarget"
                 (json-object "targetId" (target-id target)))))
    (make-instance 'session :browser (target-browser target) :target target
                            :id (json-value result "sessionId"))))

(defun detach-session (session)
  (unless (session-closed-p session)
    (browser-call (session-browser session) "Target.detachFromTarget"
                  (json-object "sessionId" (session-id session)))
    (setf (session-closed-p session) t))
  session)

;;; JavaScript and navigation

(defun javascript-exception-detail (details)
  (let ((exception (json-value details "exception")))
    (or (and exception (json-value exception "description"))
        (json-value details "text")
        "unknown JavaScript exception")))

(defun evaluate-javascript
    (session expression &key (await-promise t) (return-by-value t)
                             user-gesture
                             (timeout (browser-default-timeout
                                       (session-browser session))))
  "Evaluate EXPRESSION in SESSION.

The first value is the returned JavaScript value when RETURN-BY-VALUE is true.
The second value is Chrome's complete Runtime.RemoteObject alist."
  (let* ((reply
           (session-call
            session "Runtime.evaluate"
            (json-object "expression" expression
                         "awaitPromise" (json-boolean await-promise)
                         "returnByValue" (json-boolean return-by-value)
                         "userGesture" (json-boolean user-gesture))
            :timeout timeout))
         (details (json-value reply "exceptionDetails"))
         (result (json-value reply "result")))
    (when details
      (error 'javascript-error
             :expression expression :details details
             :detail (javascript-exception-detail details)))
    (values (json-value result "value") result)))

(defun navigate (session url &key
                               (timeout (browser-default-timeout
                                         (session-browser session))))
  "Navigate SESSION's page to URL and return Page.navigate's result."
  (session-call session "Page.navigate" (json-object "url" url)
                :timeout timeout))

;;; Event mailbox

(defun take-event-if (browser predicate timeout method)
  (let ((deadline (and timeout (+ (monotonic-seconds) timeout))))
    (sb-thread:with-mutex ((browser-state-lock browser))
      (loop
        (let ((event (find-if predicate (browser-event-queue browser))))
          (when event
            (setf (browser-event-queue browser)
                  (delete event (browser-event-queue browser)
                          :test #'eq :count 1))
            (return event)))
        (when (browser-closed-p browser)
          (error 'browser-closed :browser browser
                                 :detail (browser-failure browser)))
        (let ((remaining (and deadline (- deadline (monotonic-seconds)))))
          (when (and remaining (<= remaining 0))
            (error 'cdp-timeout :method method :seconds timeout
                                :detail "event wait timed out"))
          (sb-thread:condition-wait (browser-ready browser)
                                    (browser-state-lock browser)
                                    :timeout remaining))))))

(defun next-event (browser &key method predicate
                                (timeout (browser-default-timeout browser)))
  "Remove and return the next matching unsolicited event."
  (take-event-if browser
                 (lambda (event)
                   (and (or (null method) (string= method (event-method event)))
                        (or (null predicate) (funcall predicate event))))
                 timeout (or method "event")))

(defun next-session-event (session &key method predicate
                                        (timeout (browser-default-timeout
                                                  (session-browser session))))
  "Remove and return the next matching event belonging to SESSION."
  (next-event
   (session-browser session)
   :method method :timeout timeout
   :predicate (lambda (event)
                (and (equal (session-id session) (event-session-id event))
                     (or (null predicate) (funcall predicate event))))))

(defun drain-events (browser)
  "Remove and return all currently queued unsolicited events."
  (sb-thread:with-mutex ((browser-state-lock browser))
    (prog1 (browser-event-queue browser)
      (setf (browser-event-queue browser) '()))))
