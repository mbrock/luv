;;;; The deliberately small HTTP and page protocol for the luvcraft web site.
;;;;
;;;; A WEB-PAGE owns one URL subtree.  Adding a page means adding an object and
;;;; a RESPOND-TO-WEB-REQUEST method, rather than growing the socket server's
;;;; knowledge of gallery assets, shader URLs, or future instruments.

(in-package #:luvcraft.web)

(defclass web-response ()
  ((status :initarg :status :reader web-response-status)
   (content-type :initarg :content-type :reader web-response-content-type)
   (body :initarg :body :reader web-response-body)))

(defun make-web-response (status content-type body)
  (make-instance 'web-response
                 :status status :content-type content-type :body body))

(defun ok-response (content-type body)
  (make-web-response "200 OK" content-type body))

(defun not-found-response ()
  (make-web-response "404 Not Found" "text/plain; charset=utf-8"
                     (format nil "not found~%")))

(defclass web-page ()
  ((path :initarg :path :reader web-page-path)
   (label :initarg :label :reader web-page-label)
   (description :initarg :description :reader web-page-description))
  (:documentation "A semantic page mounted at one URL subtree."))

(defclass web-application ()
  ((pages :initarg :pages :reader web-application-pages)))

(defun make-web-application (&rest pages)
  (make-instance 'web-application :pages pages))

(defgeneric respond-to-web-request (receiver path)
  (:documentation
   "Return a WEB-RESPONSE for PATH from a web application or mounted page."))

(defmethod respond-to-web-request ((page web-page) path)
  (declare (ignore path))
  (not-found-response))

(defun html-escaped (string)
  (with-output-to-string (output)
    (loop for character across string
          do (case character
               (#\& (write-string "&amp;" output))
               (#\< (write-string "&lt;" output))
               (#\> (write-string "&gt;" output))
               (#\" (write-string "&quot;" output))
               (otherwise (write-char character output))))))

(defun application-index (application)
  (with-output-to-string (output)
    (format output "<!doctype html>~%")
    (format output "<html lang=\"en\"><head><meta charset=\"utf-8\">~%")
    (format output "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">~%")
    (format output "<title>Luvcraft</title><style>~%")
    (format output ":root { color-scheme: dark; font-family: ui-monospace, SFMono-Regular, monospace; background: #10130f; color: #ecf0df }~%")
    (format output "body { margin: 0; min-height: 100vh; display: grid; place-items: center; background: radial-gradient(circle at 70% 20%, #29352b, #10130f 55%) }~%")
    (format output "main { width: min(42rem, calc(100% - 3rem)); padding: 4rem 0 } p { color: #aeb9a8; line-height: 1.6 }~%")
    (format output "ul { padding: 0; display: grid; gap: 1rem } li { list-style: none } a { display: block; padding: 1.25rem; border: 1px solid #49584a; color: inherit; text-decoration: none; background: #171d17cc }~%")
    (format output "a:hover { border-color: #d49b68; transform: translateY(-1px) } strong { display: block; color: #f2c28f; margin-bottom: .4rem }~%")
    (format output "</style></head><body><main><p>LUVCRAFT / WEB</p><h1>Little windows into the world.</h1><ul>~%")
    (dolist (page (web-application-pages application))
      (format output "<li><a href=\"~A/\"><strong>~A</strong>~A</a></li>~%"
              (html-escaped (web-page-path page))
              (html-escaped (web-page-label page))
              (html-escaped (web-page-description page))))
    (format output "</ul></main></body></html>~%")))

(defun page-relative-path (page path)
  (let ((prefix (web-page-path page)))
    (cond ((string= path prefix) "/")
          ((and (> (length path) (length prefix))
                (uiop:string-prefix-p prefix path)
                (char= #\/ (char path (length prefix))))
           (subseq path (length prefix))))))

(defmethod respond-to-web-request ((application web-application) path)
  (cond ((or (string= path "/") (string= path "/index.html"))
         (ok-response "text/html; charset=utf-8"
                      (application-index application)))
        ((string= path "/healthz")
         (ok-response "text/plain; charset=utf-8" (format nil "ok~%")))
        (t
         (loop for page in (web-application-pages application)
               for relative-path = (page-relative-path page path)
               when relative-path
                 return (respond-to-web-request page relative-path)
               finally (return (not-found-response))))))

(defun response-octet-length (string)
  (length (sb-ext:string-to-octets string :external-format :utf-8)))

(defun write-http-response (stream response)
  (format stream "HTTP/1.1 ~A~C~C"
          (web-response-status response) #\Return #\Linefeed)
  (format stream "Content-Type: ~A~C~C"
          (web-response-content-type response) #\Return #\Linefeed)
  (format stream "Content-Length: ~D~C~C"
          (response-octet-length (web-response-body response))
          #\Return #\Linefeed)
  (format stream "Cache-Control: no-store~C~C" #\Return #\Linefeed)
  (format stream "Connection: close~C~C~C~C" #\Return #\Linefeed
          #\Return #\Linefeed)
  (write-string (web-response-body response) stream)
  (finish-output stream))

(defun bounded-read-line (stream limit)
  (let ((line (read-line stream nil nil)))
    (when (and line (> (length line) limit))
      (error "HTTP line exceeds ~D characters." limit))
    line))

(defun discard-http-headers (stream)
  (loop repeat 64
        for line = (bounded-read-line stream 8192)
        until (or (null line) (string= line "") (string= line (string #\Return)))
        finally (unless (or (null line) (string= line "")
                            (string= line (string #\Return)))
                  (error "Too many HTTP headers."))))

(defun request-path (request-line)
  (let ((first-space (and request-line (position #\Space request-line)))
        (second-space nil))
    (when first-space
      (setf second-space (position #\Space request-line :start (1+ first-space))))
    (unless (and first-space second-space
                 (string= "GET" request-line :end2 first-space))
      (error "Only a well-formed GET request is supported."))
    (let* ((target (subseq request-line (1+ first-space) second-space))
           (query (position #\? target)))
      (subseq target 0 query))))

(defun serve-web-request (client application)
  (let ((stream (sb-bsd-sockets:socket-make-stream
                 client :input t :output t :element-type 'character
                 :buffering :full :external-format :utf-8)))
    (unwind-protect
         (handler-case
             (let* ((line (bounded-read-line stream 8192))
                    (path (request-path line)))
               (discard-http-headers stream)
               (format t "GET ~A~%" path)
               (write-http-response
                stream (respond-to-web-request application path)))
           (error (condition)
             (format *error-output* "luvcraft web request failed: ~A~%" condition)
             (ignore-errors
              (write-http-response
               stream
               (make-web-response "400 Bad Request"
                                  "text/plain; charset=utf-8"
                                  (format nil "bad request~%"))))))
      (ignore-errors (close stream))
      (ignore-errors (sb-bsd-sockets:socket-close client)))))

(defun ipv4-address (host)
  (sb-bsd-sockets:host-ent-address
   (sb-bsd-sockets:get-host-by-name host)))

(defun serve-web-application (application &key (host "127.0.0.1") (port 8765))
  "Serve APPLICATION on a small blocking HTTP/1.1 loop until interrupted."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream :protocol :tcp)))
    (unwind-protect
         (progn
           (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
           (sb-bsd-sockets:socket-bind socket (ipv4-address host) port)
           (sb-bsd-sockets:socket-listen socket 16)
           (format t "luvcraft web: http://~A:~D/~%" host port)
           (finish-output)
           (handler-case
               (loop
                 for client = (sb-bsd-sockets:socket-accept socket)
                 do (serve-web-request client application))
             (sb-sys:interactive-interrupt ()
               (format t "Stopping luvcraft web.~%"))))
      (ignore-errors (sb-bsd-sockets:socket-close socket)))))
