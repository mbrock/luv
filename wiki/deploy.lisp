;;;; The dynamic wiki's deliberately small blue/green deployment console.

(in-package #:luv.wiki)

(defvar *dynamic-server-p* nil
  "True only while the live Clack application renders a response.")

(defparameter *deployment-directory* #p"/run/luv/deployments/")
(defparameter *deployment-request-pathname* #p"/run/luv/deploy.request")

(defun deployment-id-p (string)
  (and (plusp (length string))
       (every (lambda (character)
                (or (alphanumericp character) (char= character #\-)))
              string)))

(defun new-deployment-id ()
  (format nil "~36R-~36,6,'0R" (get-universal-time) (random (expt 36 6))))

(defun deployment-pathname (id type)
  (unless (deployment-id-p id) (error "Invalid deployment ID."))
  (merge-pathnames (format nil "~A.~A" id type) *deployment-directory*))

(defun request-deployment ()
  "Ask the host's systemd path unit to deploy, returning this run's ID."
  (let ((id (new-deployment-id)))
    (ensure-directories-exist *deployment-request-pathname*)
    (with-open-file (stream *deployment-request-pathname*
                            :direction :output :if-exists :supersede
                            :if-does-not-exist :create)
      (write-line id stream))
    id))

(defun octets-hex-string (octets end)
  (with-output-to-string (stream)
    (loop for index below end
          do (format stream "~2,'0X" (aref octets index)))))

(defun deployment-event-stream (id)
  "A Clack streaming response which follows deployment ID's terminal log."
  (let ((log (deployment-pathname id "log"))
        (done (deployment-pathname id "done")))
    (lambda (respond)
      (let ((write (funcall respond
                            '(200 (:content-type "text/event-stream"
                                   :cache-control "no-store"))))
            (event-loop woo.ev:*evloop*))
        (sb-thread:make-thread
         (lambda ()
           (let ((woo.ev:*evloop* event-loop))
             (handler-case
                 (let ((position 0)
                       (buffer (make-array 4096 :element-type '(unsigned-byte 8))))
                   (loop
                     (when (probe-file log)
                       (with-open-file (input log :element-type '(unsigned-byte 8))
                         (file-position input position)
                         (loop for count = (read-sequence buffer input)
                               while (plusp count)
                               do (incf position count)
                                  (funcall write
                                           (format nil "event: terminal~%data: ~A~%~%"
                                                   (octets-hex-string buffer count))))))
                     (when (and (probe-file done)
                                (or (not (probe-file log))
                                    (= position
                                       (with-open-file (input log
                                                             :element-type '(unsigned-byte 8))
                                         (file-length input)))))
                       (let ((status (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                  (uiop:read-file-string done))))
                         (funcall write (format nil "event: done~%data: ~A~%~%" status)
                                  :close t))
                       (return))
                     (sleep 0.1)))
               (condition (condition)
                 (ignore-errors
                   (funcall write
                            (format nil "event: failed~%data: ~A~%~%" condition)
                            :close t))))))
         :name (format nil "luv deployment ~A" id))))))

(in-package #:luv.wiki.deploy)

(defparameter +crlf+ (coerce (list #\Return #\Newline) 'string))

(defun javascript ()
  "The deployment modal's browser program, entirely in ParenScript."
  (ps:ps*
   `(progn
      (defun deployment-element (id)
        ((@ document get-element-by-id) id))
      (defun deployment-octets (hex)
        (let ((bytes (new (|Uint8Array| (/ (@ hex length) 2)))))
          (loop for index from 0 below (@ bytes length) do
            (setf (aref bytes index)
                  (parse-int ((@ hex slice) (* index 2) (+ (* index 2) 2)) 16)))
          bytes))
      (browser:async-defun begin-deployment ()
        (let* ((dialog (deployment-element "deployment-dialog"))
               (host (deployment-element "deployment-terminal"))
               (button (deployment-element "deploy-luv"))
               (close (deployment-element "close-deployment")))
          (setf (@ button disabled) true
                (@ close disabled) true
                (@ host text-content) "")
          ((@ dialog show-modal))
          (let ((terminal
                  (new (|Terminal|
                        (create :convert-eol false :cursor-blink false
                                :disable-stdin true :font-size 14
                                :font-family
                                "ui-monospace, SFMono-Regular, Menlo, monospace"
                                :scrollback 5000)))))
            ((@ terminal open) host)
            ((@ terminal write)
             ,(concatenate 'string "Requesting a blue/green deployment…" +crlf+))
            (try
              (let* ((response
                       (browser:await
                        (fetch "/admin/deployments"
                               (create :method "POST"
                                       :headers (create :X-Luv-Deploy "1")))))
                     (description (browser:await ((@ response json)))))
                (unless (@ response ok)
                  (throw (new (|Error| (or (@ description error)
                                           "Deployment request failed")))))
                (let ((events (new (|EventSource| (@ description events)))))
                  ((@ events add-event-listener) "terminal"
                   (lambda (event)
                     ((@ terminal write) (deployment-octets (@ event data)))))
                  ((@ events add-event-listener) "done"
                   (lambda (event)
                     ((@ events close))
                     ((@ terminal write)
                      (if (= (@ event data) "0")
                          ,(concatenate 'string +crlf+
                                        "Deployment complete. Reloading…" +crlf+)
                          (+ ,(concatenate 'string +crlf+
                                           "Deployment failed with status ")
                             (@ event data) "." ,+crlf+)))
                     (setf (@ close disabled) false (@ button disabled) false)
                     (when (= (@ event data) "0")
                       (set-timeout (lambda () ((@ window location reload))) 900))))
                  ((@ events add-event-listener) "failed"
                   (lambda (event)
                     ((@ events close))
                     ((@ terminal write)
                      (+ ,+crlf+ (@ event data) ,+crlf+))
                     (setf (@ close disabled) false (@ button disabled) false)))))
              (:catch (error)
                ((@ terminal write)
                 (+ ,+crlf+ (@ error message) ,+crlf+))
                (setf (@ close disabled) false (@ button disabled) false))))))
      ((@ document add-event-listener) "DOMContentLoaded"
       (lambda ()
         (let ((button (deployment-element "deploy-luv"))
               (dialog (deployment-element "deployment-dialog"))
               (close (deployment-element "close-deployment")))
           (when button
             ((@ button add-event-listener) "click" begin-deployment)
             ((@ close add-event-listener) "click"
              (lambda () ((@ dialog close)))))))))))

(in-package #:luv.wiki)

(defun deployment-javascript ()
  (luv.wiki.deploy:javascript))

(defun render-deployment-button ()
  "Emit the small live-site deployment button."
  (spinneret:with-html
    (:button#deploy-luv.deploy-button :type "button" "Deploy")))

(defun render-deployment-dialog ()
  "Emit the live site's deployment terminal dialog."
  (spinneret:with-html
    (:dialog#deployment-dialog.deployment-dialog
      (:header
       (:div
        (:strong "Deploy luv.swa.sh")
        (:span "build the inactive slot, check it, then switch traffic"))
       (:button#close-deployment :type "button" :disabled t "Close"))
      (:div#deployment-terminal.deployment-terminal
       :aria-label "Deployment terminal output"))))

(defun deployment-response (method path)
  "A dynamic-only response for PATH, or NIL when it is not a deploy route."
  (cond
    ((and (eq method :get) (string= path "/admin/deploy.js"))
     `(200 (:content-type "text/javascript; charset=utf-8" :cache-control "no-store")
           (,(deployment-javascript))))
    ((string= path "/admin/deployments")
     (if (eq method :post)
         (let ((id (request-deployment)))
           `(202 (:content-type "application/json; charset=utf-8"
                  :cache-control "no-store")
                 (,(format nil
                           "{\"id\":\"~A\",\"events\":\"/admin/deployments/~A/events\"}"
                           id id))))
         `(405 (:content-type "text/plain; charset=utf-8" :allow "POST")
               ("method not allowed\n"))))
    ((and (eq method :get)
          (let ((prefix "/admin/deployments/") (suffix "/events"))
            (and (> (length path) (+ (length prefix) (length suffix)))
                 (uiop:string-prefix-p prefix path)
                 (uiop:string-suffix-p path suffix))))
     (let* ((prefix "/admin/deployments/")
            (id (subseq path (length prefix) (- (length path) (length "/events")))))
       (if (deployment-id-p id)
           (deployment-event-stream id)
           `(400 (:content-type "text/plain; charset=utf-8")
                 ("bad deployment id\n")))))
    ((uiop:string-prefix-p "/admin/deploy" path)
     `(404 (:content-type "text/plain; charset=utf-8") ("not found\n")))
    (t nil)))
