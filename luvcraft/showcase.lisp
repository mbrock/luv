;;;; The published, source-reproducible stills and films of luvcraft.
;;;;
;;;; The web process owns the semantic page and reads the small capture
;;;; manifest.  Caddy owns the large byte responses: annex symlinks remain
;;;; ordinary static files and never pass through the character HTTP server.

(in-package #:luvcraft.web)

(defclass showcase-page (web-page)
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
                 :path "/showcase"
                 :label "Showcase"
                 :description "Reproducible glimpses of little worlds and live proposals."
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

(defun write-showcase-image (output page capture filename)
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
    ;; SRCSET contains only card-sized derivatives, placing a hard ceiling on
    ;; index traffic.  The surrounding link keeps the original available for
    ;; deliberate detail viewing and download.
    (format output "<a class=original href=\"~A\" title=\"Open full-resolution image\">"
            (html-escaped original-url))
    (format output "<img loading=lazy decoding=async src=\"~A\" alt=\"\""
            (html-escaped (or largest-variant-url original-url)))
    (when (and width height)
      (format output " width=~D height=~D" width height))
    (when variants
      (format output " srcset=\"~{~A~^, ~}\" sizes=\"~A\""
              (mapcar
               (lambda (variant)
                 (format nil "~A ~Dw"
                         (html-escaped
                          (showcase-media-url page (getf variant :file)))
                         (getf variant :width)))
               variants)
              (showcase-card-sizes capture)))
    (format output "></a>")))

(defun write-showcase-video (output page capture filename)
  (let* ((url (showcase-media-url page filename))
         (width (positive-showcase-dimension capture :width))
         (height (positive-showcase-dimension capture :height))
         (poster (getf capture :poster))
         (poster-file
           (and (safe-showcase-variant-p poster) (getf poster :file))))
    (format output
            "<video controls loop muted playsinline preload=~A"
            (if poster-file "none" "metadata"))
    (when poster-file
      (format output " poster=\"~A\""
              (html-escaped (showcase-media-url page poster-file))))
    (when (and width height)
      (format output " width=~D height=~D" width height))
    (format output "><source src=\"~A\" type=\"video/~A\">This browser cannot play the captured video.</video>"
            (html-escaped url)
            (html-escaped (string-downcase (pathname-type filename))))))

(defun write-showcase-card (output page capture)
  (let* ((filename (getf capture :file))
         (kind (getf capture :kind))
         (figure (getf capture :figure))
         (layout (showcase-layout capture))
         (layout-name (string-downcase (symbol-name layout))))
    (when (safe-showcase-filename-p filename)
      (format output "<article class=~A><div class=\"media ~A\">"
              layout-name layout-name)
      (ecase kind
        (:image
         (write-showcase-image output page capture filename))
        (:video
         (write-showcase-video output page capture filename)))
      (format output "</div><p class=figure>#~A</p><h2>~A</h2></article>~%"
              (html-escaped (or figure "------"))
              (html-escaped (showcase-title capture))))))

(defun showcase-page-html (page)
  (let* ((manifest (read-showcase-manifest page))
         (captures (and manifest (getf manifest :captures)))
         (revision (and manifest (getf manifest :source-revision))))
    (with-output-to-string (output)
      (format output "<!doctype html><html lang=en><head><meta charset=utf-8>~%")
      (format output "<meta name=viewport content=\"width=device-width, initial-scale=1\"><title>Luvcraft showcase</title><style>~%")
      (format output ":root{color-scheme:dark;font-family:ui-monospace,SFMono-Regular,monospace;background:#0d100d;color:#edf0e4}body{margin:0;background:radial-gradient(circle at 80% 0,#29352b,#0d100d 42rem)}main{width:min(76rem,calc(100% - 2rem));margin:auto;padding:4rem 0 8rem}header{max-width:48rem;margin-bottom:3rem}a{color:#f2c28f}p{color:#aeb9a8;line-height:1.55}.grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(min(100%,28rem),1fr));gap:1.5rem}article{overflow:hidden;border:1px solid #455247;border-radius:.8rem;background:#151a15}.media{aspect-ratio:3/2;background:#080a08;display:grid;place-items:center}.media .original,.media img,.media video{display:block;width:100%;height:100%}.media img,.media video{object-fit:cover}.media.portrait{aspect-ratio:9/16;width:min(100%,27rem);margin-inline:auto}.media.portrait img,.media.portrait video{object-fit:contain}article h2,article .figure{margin-left:1.2rem;margin-right:1.2rem}.figure{font-size:.78rem;color:#d49b68;margin-bottom:.3rem}article h2{margin-top:0;margin-bottom:1.2rem;font-size:1.1rem}</style></head><body><main>~%")
      (format output "<header><p>LUVCRAFT / SHOWCASE</p><h1>Possible worlds, rendered.</h1><p>These stills and films are generated from named Lisp capture recipes. The source stays small; the media is published separately from its rendering machine.</p><p><a href=\"/\">← all little windows</a>")
      (when revision
        (format output " · source <code>~A</code>"
                (html-escaped (subseq revision 0 (min 12 (length revision))))))
      (format output "</p></header>~%")
      (if captures
          (progn
            (format output "<section class=grid>~%")
            (dolist (capture captures)
              (write-showcase-card output page capture))
            (format output "</section>~%"))
          (format output "<p>No capture manifest has been published here yet.</p>~%"))
      (format output "</main></body></html>~%"))))

(defmethod respond-to-web-request ((page showcase-page) path)
  (if (or (string= path "/") (string= path "/index.html"))
      (ok-response "text/html; charset=utf-8" (showcase-page-html page))
      (call-next-method)))
