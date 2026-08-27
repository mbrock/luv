;;;; One website model, presented either through Clack or as static files.

(in-package #:luv.wiki)

(defclass resource ()
  ((path :initarg :path :reader resource-path)
   (output-path :initarg :output-path :reader resource-output-path)
   (content-type :initarg :content-type :reader resource-content-type)
   (label :initarg :label :initform nil :reader resource-label)
   (description :initarg :description :initform nil :reader resource-description)
   (kind :initarg :kind :initform nil :reader resource-kind)))

(defclass generated-resource (resource)
  ((generator :initarg :generator :reader resource-generator)))

(defclass file-resource (resource)
  ((pathname :initarg :pathname :reader resource-pathname)))

(defun make-generated-resource (path output-path content-type generator
                                &key label description kind)
  (make-instance 'generated-resource :path path :output-path output-path
                 :content-type content-type :generator generator
                 :label label :description description :kind kind))

(defun make-file-resource (path output-path content-type pathname)
  (make-instance 'file-resource :path path :output-path output-path
                 :content-type content-type :pathname pathname))

(defvar *resource-providers* '()
  "Named functions contributing resources to the complete website.")

(defun register-resource-provider (name function)
  "Register FUNCTION to return additional RESOURCE objects for a SITE.
Redefining NAME replaces its provider without disturbing provider order."
  (let ((entry (assoc name *resource-providers*)))
    (if entry
        (setf (cdr entry) function)
        (setf *resource-providers*
              (append *resource-providers* (list (cons name function))))))
  name)

(defun html-resource-string (thunk)
  (with-output-to-string (stream)
    (call-with-html-output stream thunk)))

(defun pathname-content-type (pathname)
  (let ((type (string-downcase (or (pathname-type pathname) ""))))
    (cond ((string= type "png") "image/png")
          ((member type '("jpg" "jpeg") :test #'string=) "image/jpeg")
          ((string= type "webp") "image/webp")
          ((string= type "svg") "image/svg+xml")
          ((string= type "js") "text/javascript; charset=utf-8")
          (t "application/octet-stream"))))

(defun core-resources (site)
  (labels ((html (path output renderer)
             (make-generated-resource
              path output "text/html; charset=utf-8"
              (lambda ()
                (let ((*site* site))
                  (html-resource-string renderer)))))
           (source-root ()
             (and (site-source-directory site)
                  (merge-pathnames "wiki/" (site-source-directory site)))))
    (append
     (loop for document in (site-documents site)
           collect (let ((document document))
                     (html (concatenate 'string "/" (site-page-name document))
                           (site-page-name document)
                           (lambda () (render-page document)))))
     (list
      (html "/pages.html" "pages.html" (lambda () (render-pages-page site)))
      (html "/work.html" "work.html" (lambda () (render-work-page site)))
      (make-generated-resource
       "/style.css" "style.css" "text/css; charset=utf-8"
       #'luv.css:stylesheet-text))
     (when (site-source-files site)
       (cons
        (html "/source.html" "source.html" (lambda () (render-source-index site)))
        (loop for file in (site-source-files site)
              for output = (source-page-name file)
              collect (let ((file file))
                        (html (concatenate 'string "/" output) output
                              (lambda () (render-source-page file)))))))
     (let ((root (source-root)))
       (when root
         (append
          (let ((script (merge-pathnames "site.js" root)))
            (when (probe-file script)
              (list (make-file-resource "/site.js" "site.js"
                                        "text/javascript; charset=utf-8" script))))
          (loop for pathname in (uiop:directory-files (merge-pathnames "images/" root))
                for name = (file-namestring pathname)
                collect (make-file-resource
                         (concatenate 'string "/images/" name)
                         (concatenate 'string "images/" name)
                         (pathname-content-type pathname)
                         pathname))))))))

(defun website-resources (site)
  "Every resource in SITE's web presentation, in routing/publication order."
  (unless (site-resources-realized-p site)
    (setf (site-resources site)
          (append (core-resources site)
                  (site-resources site)
                  (loop for entry in *resource-providers*
                        append (funcall (cdr entry) site)))
          (site-resources-realized-p site) t))
  (site-resources site))

(defun website-navigation (site)
  "The resources that advertise themselves in the shared library band."
  (remove-if-not #'resource-label (website-resources site)))

(defun canonical-request-path (path)
  (cond ((string= path "/") "/index.html")
        ((and (> (length path) 1) (char= #\/ (char path (1- (length path)))))
         path)
        (t path)))

(defun find-resource (path site)
  (let ((resources (website-resources site)))
    (or (find (canonical-request-path path) resources
              :key #'resource-path :test #'string=)
        (and (> (length path) 1)
             (not (char= #\/ (char path (1- (length path)))))
             (find (concatenate 'string path "/") resources
                   :key #'resource-path :test #'string=)))))

(defgeneric resource-body (resource)
  (:method ((resource generated-resource))
    (funcall (resource-generator resource)))
  (:method ((resource file-resource))
    (resource-pathname resource)))

(defun resource-response (resource)
  (if resource
      (let ((body (resource-body resource)))
        `(200 (:content-type ,(resource-content-type resource)
               :cache-control "no-store")
              ,(if (pathnamep body) body (list body))))
      `(404 (:content-type "text/plain; charset=utf-8") ("not found\n"))))

(defun website-response (path site)
  "Answer PATH, including the process-level readiness endpoint."
  (if (string= path "/healthz")
      `(200 (:content-type "text/plain; charset=utf-8"
             :cache-control "no-store")
            ("ok"))
      (resource-response (find-resource path site))))

(defun wiki-clack-application (site)
  (lambda (environment)
    (let ((method (getf environment :request-method))
          (path (getf environment :path-info)))
      (format t "~A ~A~%" method path)
      (if (member method '(:get :head))
          (let ((response (website-response path site)))
            (if (eq method :head)
                (list (first response) (second response) '())
                response))
          `(405 (:content-type "text/plain; charset=utf-8" :allow "GET, HEAD")
                ("method not allowed\n"))))))

(defgeneric publish-resource (resource directory)
  (:method ((resource generated-resource) directory)
    (let ((pathname (merge-pathnames (resource-output-path resource) directory)))
      (ensure-directories-exist pathname)
      (with-open-file (stream pathname :direction :output :if-exists :supersede
                                      :external-format :utf-8)
        (write-string (resource-body resource) stream))
      pathname))
  (:method ((resource file-resource) directory)
    (let ((pathname (merge-pathnames (resource-output-path resource) directory)))
      (ensure-directories-exist pathname)
      (uiop:copy-file (resource-pathname resource) pathname)
      pathname)))

(defun publish-site (site directory)
  "Write the same resources served dynamically into DIRECTORY."
  (let ((directory (uiop:ensure-directory-pathname directory)))
    (dolist (resource (website-resources site))
      (publish-resource resource directory))
    directory))

(defun write-site (site directory &key (stylesheet t))
  "Publish SITE to DIRECTORY through the same resources used by the server.
STYLESHEET is retained for compatibility with the original static renderer."
  (if stylesheet
      (publish-site site directory)
      (let ((directory (uiop:ensure-directory-pathname directory)))
        (dolist (resource (website-resources site))
          (unless (string= (resource-path resource) "/style.css")
            (publish-resource resource directory)))
        directory)))

(defun serve-site (site &key (host "127.0.0.1") (port 8765))
  "Serve SITE dynamically with Clack and Woo until interrupted."
  (clack:clackup (wiki-clack-application site)
                 :server :woo :address host :port port :debug nil
                 :use-thread nil :use-default-middlewares nil))
