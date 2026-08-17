;;;; Loading MuPDF, and the slice of its C ABI this system speaks.
;;;;
;;;; Two things about MuPDF shape everything here.
;;;;
;;;; The first is that its objects are opaque.  A context, a document, a page,
;;;; a pixmap are all pointers you never look inside; everything you want from
;;;; them comes back through an accessor function.  That is why this binding
;;;; declares almost no C structure layouts: only FZ-MATRIX and FZ-RECT, which
;;;; are passed and returned by value and are six floats and four.
;;;;
;;;; The second is that it reports failure by longjmp.  FZ_THROW unwinds to
;;;; the nearest FZ_TRY, and a C library's setjmp is not something a Lisp can
;;;; stand in the middle of, so this binding does not try.  With no handler on
;;;; the stack MuPDF prints the error and aborts the process -- which means a
;;;; malformed document takes the image with it.  OPEN-DOCUMENT therefore
;;;; refuses anything that does not begin with a PDF header, which is a cheap
;;;; guard against the common case rather than a real answer.  The real answer
;;;; is a C shim holding FZ_TRY, and it is not written yet.

(in-package #:luv.mupdf)

(define-condition mupdf-error (error)
  ((operation :initarg :operation :initform nil :reader mupdf-error-operation)
   (detail :initarg :detail :initform nil :reader mupdf-error-detail))
  (:report
   (lambda (condition stream)
     (format stream "MuPDF operation ~@[~S ~]failed~@[: ~A~]."
             (mupdf-error-operation condition)
             (mupdf-error-detail condition)))))

(define-condition mupdf-unavailable (mupdf-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "libmupdf could not be loaded.  The Nix environment puts ~
it on LD_LIBRARY_PATH and sets LUV_MUPDF_LIBDIR; outside it, pass a directory ~
to LOAD-MUPDF."))))

;;;; Loading
;;;;
;;;; The same pinning libav uses and for the same reason: CFFI explodes
;;;; LD_LIBRARY_PATH itself before consulting the system loader, so an
;;;; unqualified soname is free to find some other MuPDF that happens to be
;;;; installed.

(defvar *mupdf-library* nil)

(defparameter *mupdf-default-directory*
  (uiop:getenv "LUV_MUPDF_LIBDIR")
  "Pinned MuPDF library directory captured when this system was loaded.")

(defparameter *mupdf-sonames*
  #+darwin '("libmupdf.dylib")
  #-darwin '("libmupdf.so" "libmupdf.so.26" "libmupdf.so.25")
  "Names to try, most portable first.  MuPDF does not version its soname the
way the libav libraries do, so there is no major number to build one from.")

(defun load-mupdf (&optional directory)
  "Load libmupdf, from DIRECTORY or LUV_MUPDF_LIBDIR or the platform search."
  (or *mupdf-library*
      (let* ((override (or directory
                           (uiop:getenv "LUV_MUPDF_LIBDIR")
                           *mupdf-default-directory*))
             (cffi:*foreign-library-directories*
               (if override
                   (cons (uiop:ensure-directory-pathname override)
                         cffi:*foreign-library-directories*)
                   cffi:*foreign-library-directories*)))
        (setf *mupdf-library*
              (handler-case
                  (cffi:load-foreign-library (cons :or *mupdf-sonames*))
                (error (condition)
                  (error 'mupdf-unavailable :operation 'load-mupdf
                                            :detail condition)))))))

(defun mupdf-loaded-p ()
  (not (null *mupdf-library*)))

;;;; The two layouts
;;;;
;;;; A transform and a rectangle, passed and returned by value.  MuPDF's
;;;; matrix is the usual six-element affine one, laid out so that a point maps
;;;; to (a*x + c*y + e, b*x + d*y + f).

(cffi:defcstruct (fz-matrix :class fz-matrix-type)
  (a :float) (b :float) (c :float) (d :float) (e :float) (f :float))

(cffi:defcstruct (fz-rect :class fz-rect-type)
  (x0 :float) (y0 :float) (x1 :float) (y1 :float))

(cffi:defcstruct fz-point
  (x :float) (y :float))

(cffi:defcstruct fz-quad
  (upper-left (:struct fz-point))
  (upper-right (:struct fz-point))
  (lower-left (:struct fz-point))
  (lower-right (:struct fz-point)))

;;;; Contexts
;;;;
;;;; FZ_NEW_CONTEXT is a macro that pins the header's version string into the
;;;; call, so the library can refuse a caller compiled against a different
;;;; one.  There is no function to ask the library what version it is -- the
;;;; string only exists as a macro -- so the caller has to say it out loud,
;;;; and the mismatch comes back as a null context rather than as a throw,
;;;; which is the one MuPDF failure this binding can see coming.
;;;;
;;;; The version is read off the pinned library directory, which in the Nix
;;;; environment is a store path with the version in its name.  When that
;;;; fails, *MUPDF-VERSION* is the place to say it by hand.

(defparameter *mupdf-version* nil
  "The FZ_VERSION string to claim, or NIL to read it from the library path.")

(defun mupdf-version-from-directory (directory)
  "The version in a store path like .../by3piv…-mupdf-1.27.2/lib."
  (when directory
    (let* ((name (namestring directory))
           (start (search "mupdf-" name :from-end t)))
      (when start
        (let* ((tail (subseq name (+ start 6)))
               (end (position-if-not
                     (lambda (character)
                       (or (digit-char-p character) (char= character #\.)))
                     tail)))
          (let ((version (string-right-trim "." (subseq tail 0 end))))
            (when (plusp (length version)) version)))))))

(cffi:defcfun ("fz_new_context_imp" %fz-new-context-imp) :pointer
  (alloc :pointer) (locks :pointer) (max-store :unsigned-long)
  (version :string))

(cffi:defcfun ("fz_drop_context" %fz-drop-context) :void (ctx :pointer))

(cffi:defcfun ("fz_register_document_handlers" %fz-register-document-handlers)
    :void
  (ctx :pointer))

;;;; Documents and pages

(cffi:defcfun ("fz_open_document" %fz-open-document) :pointer
  (ctx :pointer) (filename :string))

(cffi:defcfun ("fz_drop_document" %fz-drop-document) :void
  (ctx :pointer) (document :pointer))

(cffi:defcfun ("fz_count_pages" %fz-count-pages) :int
  (ctx :pointer) (document :pointer))

(cffi:defcfun ("fz_load_page" %fz-load-page) :pointer
  (ctx :pointer) (document :pointer) (number :int))

(cffi:defcfun ("fz_drop_page" %fz-drop-page) :void
  (ctx :pointer) (page :pointer))

(cffi:defcfun ("fz_bound_page" %fz-bound-page) (:struct fz-rect)
  (ctx :pointer) (page :pointer))

;;;; Pixels

(cffi:defcfun ("fz_device_rgb" %fz-device-rgb) :pointer (ctx :pointer))

(cffi:defcfun ("fz_new_pixmap_from_page" %fz-new-pixmap-from-page) :pointer
  (ctx :pointer) (page :pointer) (matrix (:struct fz-matrix))
  (colorspace :pointer) (alpha :int))

(cffi:defcfun ("fz_drop_pixmap" %fz-drop-pixmap) :void
  (ctx :pointer) (pixmap :pointer))

(cffi:defcfun ("fz_pixmap_samples" %fz-pixmap-samples) :pointer
  (ctx :pointer) (pixmap :pointer))

(cffi:defcfun ("fz_pixmap_width" %fz-pixmap-width) :int
  (ctx :pointer) (pixmap :pointer))

(cffi:defcfun ("fz_pixmap_height" %fz-pixmap-height) :int
  (ctx :pointer) (pixmap :pointer))

(cffi:defcfun ("fz_pixmap_stride" %fz-pixmap-stride) :int
  (ctx :pointer) (pixmap :pointer))

(cffi:defcfun ("fz_pixmap_components" %fz-pixmap-components) :int
  (ctx :pointer) (pixmap :pointer))

;;;; Structured text
;;;;
;;;; The one part of MuPDF whose layout this system has to know, because it is
;;;; a linked structure walked field by field rather than an opaque handle.
;;;; Those records gain fields between releases, so the offsets are groveled
;;;; from the headers in ABI.LISP rather than written down here.

(cffi:defcfun ("fz_new_stext_page_from_page" %fz-new-stext-page-from-page)
    :pointer
  (ctx :pointer) (page :pointer) (options :pointer))

(cffi:defcfun ("fz_drop_stext_page" %fz-drop-stext-page) :void
  (ctx :pointer) (stext :pointer))

(cffi:defcfun ("fz_font_name" %fz-font-name) :string
  (ctx :pointer) (font :pointer))

;;;; Buffers and output
;;;;
;;;; MuPDF will serialize a structured-text page to XML itself, and that is
;;;; what this binding reads instead of walking the FZ_STEXT_* records field
;;;; by field.  Those records gain fields between releases and would have to
;;;; be groveled; the XML is a documented output format, and asking for it
;;;; keeps every C object in this system opaque.  No headers, no offsets.

(cffi:defcfun ("fz_new_buffer" %fz-new-buffer) :pointer
  (ctx :pointer) (capacity :unsigned-long))

(cffi:defcfun ("fz_drop_buffer" %fz-drop-buffer) :void
  (ctx :pointer) (buffer :pointer))

(cffi:defcfun ("fz_buffer_storage" %fz-buffer-storage) :unsigned-long
  (ctx :pointer) (buffer :pointer) (data :pointer))

(cffi:defcfun ("fz_new_output_with_buffer" %fz-new-output-with-buffer) :pointer
  (ctx :pointer) (buffer :pointer))

(cffi:defcfun ("fz_close_output" %fz-close-output) :void
  (ctx :pointer) (output :pointer))

(cffi:defcfun ("fz_drop_output" %fz-drop-output) :void
  (ctx :pointer) (output :pointer))

(cffi:defcfun ("fz_print_stext_page_as_xml" %fz-print-stext-page-as-xml) :void
  (ctx :pointer) (output :pointer) (stext :pointer) (id :int))
