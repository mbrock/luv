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
   (layout :initarg :layout :initform :landscape
           :reader capture-specification-layout)
   (renderer :initarg :renderer :reader capture-specification-renderer))
  (:documentation
   "A named recipe for one generated wiki image or video.

The semantic metadata is inspectable and retained in source; RENDERER is the
ordinary named function installed by DEFINE-CAPTURE.  Rendered bytes live
under the capture output directory and do not belong in Git. #IVRWI8"))

(defvar *capture-specifications* (make-hash-table :test #'equal))
(defvar *capture-specification-order* '())

(defconstant +capture-web-image-width+ 768
  "The default intrinsic width of a showcase card image derivative.")

(defconstant +capture-web-poster-width+ 480
  "The maximum intrinsic width of a showcase video poster.")

(defparameter *capture-web-image-widths* '(480 768)
  "The deterministic responsive widths generated for each larger still.")

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

(defun normalize-capture-layout (layout)
  (unless (member layout '(:landscape :portrait))
    (error "Capture layout ~S is not :LANDSCAPE or :PORTRAIT." layout))
  layout)

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
    (name (&key figure kind extension (description "") (layout :landscape))
     (pathname) &body body)
  "Define one inspectable wiki capture recipe.

NAME is its command-line identity.  FIGURE is the stable six-character wiki
figure ID; KIND is :IMAGE or :VIDEO; EXTENSION is the generated file suffix.
LAYOUT is :LANDSCAPE by default or :PORTRAIT for uncropped 9:16 presentation.
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
         :layout ,(normalize-capture-layout layout)
         :renderer #',renderer)))))

(defun capture-output-pathname (specification directory)
  "The deterministic media pathname for SPECIFICATION under DIRECTORY."
  (merge-pathnames
   (format nil "~A-~A.~A"
           (capture-specification-figure-id specification)
           (capture-specification-name specification)
           (capture-specification-extension specification))
   (uiop:ensure-directory-pathname directory)))

(defun capture-derived-media-pathname (pathname suffix extension)
  "Return PATHNAME with SUFFIX appended to its name and a new EXTENSION."
  (make-pathname :name (format nil "~A~A" (pathname-name pathname) suffix)
                 :type extension :defaults pathname))

(defun capture-responsive-image-pathname
    (pathname &optional (width +capture-web-image-width+))
  "The deterministic card-sized WebP beside an original capture PATHNAME."
  (capture-derived-media-pathname
   pathname (format nil "-~Dw" width) "webp"))

(defun capture-video-poster-pathname (pathname)
  "The deterministic card-sized WebP poster beside a captured film."
  (capture-derived-media-pathname
   pathname (format nil "-poster-~Dw" +capture-web-poster-width+) "webp"))

(defun capture-media-dimensions (pathname)
  "Return the first video stream's WIDTH and HEIGHT using pinned FFprobe."
  (let* ((output
           (uiop:run-program
            (list "ffprobe" "-v" "error" "-select_streams" "v:0"
                  "-show_entries" "stream=width,height"
                  "-of" "csv=s=x:p=0" (uiop:native-namestring pathname))
            :output :string :error-output :interactive))
         (dimensions (string-trim '(#\Space #\Tab #\Newline #\Return)
                                  output))
         (separator (position #\x dimensions)))
    (unless separator
      (error "FFprobe returned no dimensions for ~A: ~S."
             pathname dimensions))
    (values (parse-integer dimensions :end separator)
            (parse-integer dimensions :start (1+ separator)))))

(defun write-capture-web-image (source destination width)
  "Downsample SOURCE to WIDTH as a deterministic photographic WebP."
  (format t "capture web image: writing ~A at ~Dpx wide...~%"
          destination width)
  (finish-output)
  (uiop:run-program
   (list "ffmpeg" "-nostdin" "-hide_banner" "-loglevel" "error" "-y"
         "-i" (uiop:native-namestring source)
         "-frames:v" "1"
         "-vf" (format nil "scale=~D:-2:flags=lanczos" width)
         "-c:v" "libwebp" "-lossless" "0" "-preset" "photo"
         "-quality" "82" "-map_metadata" "-1"
         (uiop:native-namestring destination))
   :output :interactive :error-output :interactive)
  destination)

(defun prepare-capture-web-media (specification pathname)
  "Create the small public-index derivative for captured media at PATHNAME.

Image originals get 480w and 768w WebPs whenever those are true downscales.
Films get a card-sized WebP poster from their first frame.  Originals remain
untouched and retain their stable capture identity. #IVRWI8"
  (multiple-value-bind (width height) (capture-media-dimensions pathname)
    (declare (ignore height))
    (ecase (capture-specification-kind specification)
      (:image
       (dolist (responsive-width *capture-web-image-widths*)
         (when (> width responsive-width)
           (write-capture-web-image
            pathname
            (capture-responsive-image-pathname pathname responsive-width)
            responsive-width))))
      (:video
       (write-capture-web-image
        pathname (capture-video-poster-pathname pathname)
        (min width +capture-web-poster-width+))))))

(defun capture-manifest-entry (specification directory)
  "Describe SPECIFICATION's original and any generated web derivative."
  (let ((pathname (capture-output-pathname specification directory)))
    (multiple-value-bind (width height) (capture-media-dimensions pathname)
      (let ((entry
              (list :name (capture-specification-name specification)
                    :figure (capture-specification-figure-id specification)
                    :kind (capture-specification-kind specification)
                    :file (file-namestring pathname)
                    :layout (capture-specification-layout specification)
                    :width width :height height)))
        (ecase (capture-specification-kind specification)
          (:image
           (let ((variants
                   (loop for expected-width in *capture-web-image-widths*
                         for responsive =
                         (capture-responsive-image-pathname
                          pathname expected-width)
                         when (probe-file responsive)
                           collect
                           (multiple-value-bind
                                 (responsive-width responsive-height)
                               (capture-media-dimensions responsive)
                             (list :file (file-namestring responsive)
                                   :type "image/webp"
                                   :width responsive-width
                                   :height responsive-height)))))
             (when variants
               (setf entry (append entry (list :variants variants))))))
          (:video
           (let ((poster (capture-video-poster-pathname pathname)))
             (when (probe-file poster)
               (multiple-value-bind (poster-width poster-height)
                   (capture-media-dimensions poster)
                 (setf entry
                       (append entry
                               (list :poster
                                     (list :file (file-namestring poster)
                                           :type "image/webp"
                                           :width poster-width
                                           :height poster-height)))))))))
        entry))))

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
             ,(mapcar (lambda (specification)
                        (capture-manifest-entry specification directory))
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
        (prepare-capture-web-media specification pathname)
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
