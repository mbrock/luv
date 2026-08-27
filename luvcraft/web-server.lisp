;;;; The deliberately small HTTP and page protocol for the luvcraft web site.
;;;;
;;;; A WEB-PAGE owns one URL subtree.  Adding a page means adding an object and
;;;; a RESPOND-TO-WEB-REQUEST method, rather than growing the Clack adapter's
;;;; knowledge of gallery assets, shader URLs, or future instruments.

(in-package #:luvcraft.web)

(defun ok-response (content-type body)
  `(200 (:content-type ,content-type :cache-control "no-store") (,body)))

(defun not-found-response ()
  `(404 (:content-type "text/plain; charset=utf-8" :cache-control "no-store")
        (,(format nil "not found~%"))))

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
   "Return a Lack response for PATH from a web application or mounted page."))

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

(defun clack-application (application)
  (lambda (environment)
    (let ((method (getf environment :request-method))
          (path (getf environment :path-info)))
      (format t "~A ~A~%" method path)
      (if (eq method :get)
          (respond-to-web-request application path)
          `(405 (:content-type "text/plain; charset=utf-8"
                 :allow "GET"
                 :cache-control "no-store")
                (,(format nil "method not allowed~%")))))))

(defun serve-web-application (application &key (host "127.0.0.1") (port 8765))
  "Serve APPLICATION with Clack and Woo until interrupted."
  (clack:clackup (clack-application application)
                 :server :woo
                 :address host
                 :port port
                 :debug nil
                 :use-thread nil
                 :use-default-middlewares nil))
