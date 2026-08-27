;;;; The published, source-reproducible stills and films of luvcraft.
;;;;
;;;; The wiki process owns the semantic page and reads the small capture
;;;; manifest.  Caddy owns the large byte responses: annex symlinks remain
;;;; ordinary static files and never pass through the Lisp server.

(in-package #:luvcraft.web)

(defclass showcase-page ()
  ((directory :initarg :directory :reader showcase-page-directory)
   (media-path :initarg :media-path :reader showcase-page-media-path))
  (:documentation
   "The generated capture catalog mounted beside externally served media.

DIRECTORY contains the capture manifest.  MEDIA-PATH is the public static
URL subtree whose files are served by the deployment edge. #IVRWI8"))

(defun default-showcase-directory ()
  (let ((configured (uiop:getenv "LUV_SHOWCASE_MEDIA_DIRECTORY")))
    (uiop:ensure-directory-pathname
     (if (and configured (plusp (length configured)))
         configured
         (asdf:system-relative-pathname "luvcraft/web" "build/wiki/media/")))))

(defun default-showcase-media-path ()
  (or (uiop:getenv "LUV_SHOWCASE_MEDIA_PATH") "/showcase/media"))

(defun make-showcase-page (&key directory media-path)
  (make-instance 'showcase-page
                 :directory (uiop:ensure-directory-pathname
                             (or directory (default-showcase-directory)))
                 :media-path (or media-path (default-showcase-media-path))))

(defun safe-showcase-filename-p (filename)
  (and (stringp filename)
       (plusp (length filename))
       (every (lambda (character)
                (or (alphanumericp character)
                    (find character "-_.")))
              filename)
       (member (pathname-type filename)
               '("png" "jpg" "jpeg" "webp" "mp4" "webm")
               :test #'string-equal)))

(defun read-showcase-manifest (page)
  (let ((pathname (merge-pathnames "manifest.sexp"
                                   (showcase-page-directory page))))
    (when (probe-file pathname)
      (with-open-file (stream pathname)
        (let ((*read-eval* nil))
          (let ((manifest (read stream nil nil)))
            (when (and (listp manifest)
                       (eql 1 (getf manifest :version))
                       (listp (getf manifest :captures)))
              manifest)))))))

(defun showcase-title (capture)
  (string-capitalize
   (substitute #\Space #\- (or (getf capture :name) "capture"))))

(defun showcase-media-url (page filename)
  (format nil "~A/~A"
          (string-right-trim "/" (showcase-page-media-path page))
          filename))

(defun showcase-layout (capture)
  (let ((layout (getf capture :layout :landscape)))
    (if (member layout '(:landscape :portrait)) layout :landscape)))

(defun positive-showcase-dimension (capture name)
  (let ((value (getf capture name)))
    (and (integerp value) (plusp value) value)))

(defun safe-showcase-variant-p (variant)
  (and (listp variant)
       (safe-showcase-filename-p (getf variant :file))
       (positive-showcase-dimension variant :width)
       (positive-showcase-dimension variant :height)))

(defun showcase-card-sizes (capture)
  (if (eq :portrait (showcase-layout capture))
      "(max-width: 29rem) calc(100vw - 2rem), 27rem"
      "(max-width: 59.5rem) calc(100vw - 2rem), calc((min(100vw - 2rem, 76rem) - 1.5rem) / 2)"))

(defun showcase-image (page capture filename)
  (let* ((original-url (showcase-media-url page filename))
         (width (positive-showcase-dimension capture :width))
         (height (positive-showcase-dimension capture :height))
         (variants
           (sort (remove-if-not #'safe-showcase-variant-p
                                (copy-list (getf capture :variants)))
                 #'< :key (lambda (variant) (getf variant :width))))
         (largest-variant (car (last variants)))
         (largest-variant-url
           (and largest-variant
                (showcase-media-url page (getf largest-variant :file)))))
    (spinneret:with-html
      (:a.original :href original-url :title "Open full-resolution image"
        (:img :attrs (list :decoding "async") :loading "lazy"
              :src (or largest-variant-url original-url) :alt ""
              :width width :height height
              :srcset (and variants
                           (format nil "~{~A~^, ~}"
                                   (mapcar
                                    (lambda (variant)
                                      (format nil "~A ~Dw"
                                              (showcase-media-url
                                               page (getf variant :file))
                                              (getf variant :width)))
                                    variants)))
              :sizes (and variants (showcase-card-sizes capture)))))))

(defun showcase-video (page capture filename)
  (let* ((url (showcase-media-url page filename))
         (width (positive-showcase-dimension capture :width))
         (height (positive-showcase-dimension capture :height))
         (poster (getf capture :poster))
         (poster-file
           (and (safe-showcase-variant-p poster) (getf poster :file))))
    (spinneret:with-html
      (:video :attrs (list :playsinline t) :controls t :loop t :muted t
              :preload (if poster-file "none" "metadata")
              :poster (and poster-file (showcase-media-url page poster-file))
              :width width :height height
        (:source :src url
                 :type (format nil "video/~A"
                               (string-downcase (pathname-type filename))))
        "This browser cannot play the captured video."))))

(defun showcase-card (page capture)
  (let* ((filename (getf capture :file))
         (kind (getf capture :kind))
         (figure (getf capture :figure))
         (layout (showcase-layout capture))
         (layout-name (string-downcase (symbol-name layout))))
    (when (safe-showcase-filename-p filename)
      (spinneret:with-html
        (:article :class layout-name
          (:div :class (format nil "showcase-media ~A" layout-name)
            (if (eq kind :image)
                (showcase-image page capture filename)
                (if (eq kind :video)
                    (showcase-video page capture filename)
                    (error "Unknown showcase capture kind ~S." kind))))
          (:p.capture-figure (format nil "#~A" (or figure "------")))
          (:h2 (showcase-title capture)))))))

(defun render-showcase-page (page site)
  (let* ((manifest (read-showcase-manifest page))
         (captures (and manifest (getf manifest :captures)))
         (revision (and manifest (getf manifest :source-revision))))
    (let ((luv.wiki::*site* site)
          (luv.wiki::*rendering-document* nil)
          (luv.wiki::*page-prefix* "../")
          (luv.wiki::*page-kind* "showcase"))
      (luv.wiki::render-page-frame
       "Possible worlds, rendered"
       (lambda ()
         (spinneret:with-html
           (:h1 "Possible worlds, rendered")
           (:p.lede
             "These stills and films are generated from named Lisp capture recipes. The source stays small; the media is published separately from its rendering machine.")
           (when revision
             (:p.source-revision "Source "
               (:code (subseq revision 0 (min 12 (length revision))))))
           (if captures
               (:section.showcase-grid
                 (dolist (capture captures)
                   (showcase-card page capture)))
               (:p "No capture manifest has been published here yet."))))
       :body-class "wide showcase-page"
       :kind "showcase"
       :crumbs '(("Showcase"))
       :right "capture catalog"))))

(defun showcase-resources (site)
  (let ((page (make-showcase-page)))
    (list
     (luv.wiki:make-generated-resource
      "/showcase/" "showcase/index.html" "text/html; charset=utf-8"
      (lambda ()
        (with-output-to-string (stream)
          (luv.wiki::call-with-html-output
           stream (lambda () (render-showcase-page page site)))))
      :label "Showcase"
      :description "rendered stills and films"
      :kind "showcase"))))

(luv.wiki:register-resource-provider 'showcase #'showcase-resources)
