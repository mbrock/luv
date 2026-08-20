;;; Named, reproducible media recipes for wiki figures.
;;;
;;; A recipe is source; its PNG or MP4 is a disposable build product.  The
;;; protocol deliberately knows nothing about SSH hosts or artifact stores:
;;; ASDF performs it on whichever GPU machine invoked the operation. #IVRWI8

(in-package #:luv)

(defclass capture-specification ()
  ((name :initarg :name :reader capture-specification-name)
   (figure-id :initarg :figure-id :reader capture-specification-figure-id)
   (kind :initarg :kind :reader capture-specification-kind)
   (description :initarg :description
                :reader capture-specification-description)
   (extension :initarg :extension :reader capture-specification-extension)
   (renderer :initarg :renderer :reader capture-specification-renderer))
  (:documentation
   "A named recipe for one generated wiki image or video.

The semantic metadata is inspectable and retained in source; RENDERER is the
ordinary named function installed by DEFINE-CAPTURE.  Rendered bytes live
under the capture output directory and do not belong in Git. #IVRWI8"))

(defvar *capture-specifications* (make-hash-table :test #'equal))
(defvar *capture-specification-order* '())

(defun normalize-capture-name (name)
  (string-downcase
   (etypecase name
     (symbol (symbol-name name))
     (string name))))

(defun normalize-capture-figure-id (id)
  (let ((id (string-upcase
             (etypecase id
               (symbol (symbol-name id))
               (string id)))))
    (unless (and (= 6 (length id))
                 (every #'alphanumericp id))
      (error "Capture figure ID ~S is not six alphanumeric characters." id))
    id))

(defun normalize-capture-extension (extension)
  (let ((extension (string-downcase (string extension))))
    (unless (and (plusp (length extension))
                 (every #'alphanumericp extension))
      (error "Capture extension ~S is not a simple file extension." extension))
    extension))

(defun register-capture-specification (specification)
  "Install SPECIFICATION by name, replacing a live redefinition in place."
  (check-type specification capture-specification)
  (let ((name (capture-specification-name specification)))
    (unless (gethash name *capture-specifications*)
      (setf *capture-specification-order*
            (append *capture-specification-order* (list name))))
    (setf (gethash name *capture-specifications*) specification))
  specification)

(defun capture-specifications ()
  "Return every registered capture specification in definition order."
  (loop for name in *capture-specification-order*
        for specification = (gethash name *capture-specifications*)
        when specification collect specification))

(defun find-capture-specification (name &key (errorp t))
  "Find the capture recipe NAME, accepting either a symbol or a string."
  (let ((specification
          (gethash (normalize-capture-name name) *capture-specifications*)))
    (cond (specification specification)
          (errorp (error "No capture specification named ~S." name))
          (t nil))))

(defmacro define-capture
    (name (&key figure kind extension (description "")) (pathname) &body body)
  "Define one inspectable wiki capture recipe.

NAME is its command-line identity.  FIGURE is the stable six-character wiki
figure ID; KIND is :IMAGE or :VIDEO; EXTENSION is the generated file suffix.
BODY is an ordinary named renderer function body with PATHNAME bound to its
requested output.  Re-evaluating the definition replaces the recipe without
leaving stale closures in the registry."
  (let ((renderer
          (intern (format nil "RENDER-~A-CAPTURE" (symbol-name name))
                  (symbol-package name))))
    `(progn
       (defun ,renderer (,pathname)
         ,description
         ,@body)
       (register-capture-specification
        (make-instance
         'capture-specification
         :name ,(normalize-capture-name name)
         :figure-id ,(normalize-capture-figure-id figure)
         :kind ,kind
         :description ,description
         :extension ,(normalize-capture-extension extension)
         :renderer #',renderer)))))

(defun capture-output-pathname (specification directory)
  "The deterministic media pathname for SPECIFICATION under DIRECTORY."
  (merge-pathnames
   (format nil "~A-~A.~A"
           (capture-specification-figure-id specification)
           (capture-specification-name specification)
           (capture-specification-extension specification))
   (uiop:ensure-directory-pathname directory)))

(defgeneric render-capture (specification pathname)
  (:documentation "Render SPECIFICATION to PATHNAME and return PATHNAME."))

(defmethod render-capture ((specification capture-specification) pathname)
  (funcall (capture-specification-renderer specification) pathname)
  pathname)

(defun capture-source-revision ()
  (ignore-errors
    (let ((root (asdf:system-source-directory :luv)))
      (string-trim
       '(#\Space #\Tab #\Newline #\Return)
       (uiop:run-program
        (list "git" "-C" (uiop:native-namestring root) "rev-parse" "HEAD")
        :output :string)))))

(defun write-capture-manifest (directory specifications)
  (let ((pathname (merge-pathnames "manifest.sexp" directory)))
    (with-open-file (stream pathname :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
      (with-standard-io-syntax
        (let ((*print-readably* nil))
          (pprint
           `(:version 1
             :source-revision ,(capture-source-revision)
             :captures
             ,(mapcar
               (lambda (specification)
                 (list :name (capture-specification-name specification)
                       :figure (capture-specification-figure-id specification)
                       :kind (capture-specification-kind specification)
                       :file (file-namestring
                              (capture-output-pathname specification directory))))
               specifications))
           stream))))
    pathname))

(defun render-capture-set (directory &key names)
  "Render NAMES, or every registered recipe, under DIRECTORY.

Each recipe announces its start and completion so a GPU build never becomes
an unexplained silent process.  A manifest records stable names, wiki figure
IDs, media kinds, filenames, and the source revision that produced the set."
  (let* ((directory (uiop:ensure-directory-pathname directory))
         (specifications
           (if names
               (mapcar #'find-capture-specification names)
               (capture-specifications))))
    (ensure-directories-exist directory)
    (dolist (specification specifications)
      (let ((pathname (capture-output-pathname specification directory)))
        (format t "~&capture ~A: rendering ~A for #~A...~%"
                (capture-specification-name specification)
                (capture-specification-kind specification)
                (capture-specification-figure-id specification))
        (finish-output)
        (render-capture specification pathname)
        (unless (probe-file pathname)
          (error "Capture ~A returned without writing ~A."
                 (capture-specification-name specification) pathname))
        (format t "capture ~A: wrote ~A~%"
                (capture-specification-name specification) pathname)
        (finish-output)))
    (write-capture-manifest directory specifications)
    (mapcar (lambda (specification)
              (capture-output-pathname specification directory))
            specifications)))

;;; ASDF is the local executor.  Host selection (for example, invoking this
;;; operation over SSH) stays outside the repository and the capture graph.

(defclass capture-op (asdf:selfward-operation) ())

(defmethod asdf:selfward-operation ((operation capture-op))
  (declare (ignore operation))
  'asdf:load-op)

(defmethod asdf:operation-done-p ((operation capture-op) (system asdf:system))
  (declare (ignore operation system))
  ;; Explicit capture builds mean “make fresh evidence”, even when a previous
  ;; artifact exists.
  nil)

(defun capture-operation-output-directory (system)
  (let ((configured (uiop:getenv "LUV_CAPTURE_OUTPUT_DIRECTORY")))
    (if (and configured (plusp (length configured)))
        (uiop:ensure-directory-pathname configured)
        (asdf:system-relative-pathname system "build/wiki/media/"))))

(defmethod asdf:output-files ((operation capture-op) (system asdf:system))
  (declare (ignore operation))
  (values
   (list (merge-pathnames "manifest.sexp"
                          (capture-operation-output-directory system)))
   t))

(defmethod asdf:perform ((operation capture-op) (system asdf:system))
  (declare (ignore operation))
  (call-with-sdl-main-thread
   (lambda ()
     (render-capture-set (capture-operation-output-directory system)))))
