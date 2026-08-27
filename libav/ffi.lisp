;;;; Loading FFmpeg, and the slice of its C ABI luv speaks directly.
;;;;
;;;; The libraries come in together because they version together: libavutil
;;;; owns AVFrame, libavcodec owns the decoders, libavformat owns the
;;;; demuxers, libswscale owns pixel conversion, and FFmpeg only supports
;;;; combinations from a single build.  The
;;;; groveled LIBAV*_VERSION_MAJOR constants are the compile-time half of that
;;;; agreement; LOAD-LIBAV checks them against what the loaded libraries say at
;;;; run time, because a mismatch here does not fail loudly at the call site --
;;;; it silently reads a field from the wrong offset.

(in-package #:luv.libav)

(defmacro with-libav-native-environment (&body body)
  "Run BODY with the floating-point environment FFmpeg's own code expects.

FFmpeg computes with floats that raise invalid-operation and divide-by-zero as
a matter of course -- probing stream timing alone will do it -- and SBCL traps
those by default, so an unmasked call dies inside avformat_find_stream_info
rather than returning an error luv could handle.  This is the same masking
luv's SDL and Vulkan boundaries use."
  #+sbcl
  `(sb-int:with-float-traps-masked
       (:invalid :divide-by-zero :overflow :underflow :inexact)
     ,@body)
  #+(and darwin (not sbcl))
  `(float-features:with-float-traps-masked t ,@body)
  #-(or sbcl darwin)
  `(progn ,@body))

(define-condition libav-error (error)
  ((operation :initarg :operation :reader libav-error-operation)
   (code :initarg :code :reader libav-error-code))
  (:report
   (lambda (condition stream)
     (format stream "FFmpeg operation ~S failed: ~A."
             (libav-error-operation condition)
             (libav-error-message condition)))))

(define-condition libav-version-mismatch (error)
  ((library :initarg :library :reader libav-version-mismatch-library)
   (compiled :initarg :compiled :reader libav-version-mismatch-compiled)
   (loaded :initarg :loaded :reader libav-version-mismatch-loaded))
  (:report
   (lambda (condition stream)
     (format stream
             "~A was groveled against major version ~D but the loaded ~
              library reports major version ~D.  The FFmpeg headers and ~
              shared libraries in this environment are from different builds."
             (libav-version-mismatch-library condition)
             (libav-version-mismatch-compiled condition)
             (libav-version-mismatch-loaded condition)))))

;;; Loading.
;;;
;;; The sonames are built from the groveled majors rather than written out, so
;;; the version this system was compiled against is stated exactly once.
;;; LUV_FFMPEG_LIBDIR, which the Nix environment sets to the exact store path,
;;; is consulted before the platform search: on both Darwin and Linux CFFI
;;; searches process loader paths before consulting the system loader, so an
;;; unqualified soname would otherwise be free to find some other FFmpeg that
;;; happens to be installed.  Luv launchers scope that loader environment to
;;; their own process trees; entering the checkout does not export it.

(defvar *libav-libraries* nil
  "The loaded libav* libraries, or NIL before anything asked for one.")

(defparameter *libav-default-directory*
  (uiop:getenv "LUV_FFMPEG_LIBDIR")
  "Pinned FFmpeg library directory captured when this system was loaded.

A saved standalone image retains this build-environment fallback so it can be
launched directly.  An explicit LOAD-LIBAV argument or the current process
environment still takes precedence.")

(defun library-sonames (stem major)
  "Return the names to try for libSTEM at major version MAJOR, most exact first."
  #+darwin
  (list (format nil "lib~A.~D.dylib" stem major)
        (format nil "lib~A.dylib" stem))
  #-darwin
  (list (format nil "lib~A.so.~D" stem major)
        (format nil "lib~A.so" stem)))

(defun load-libav (&optional directory)
  "Load libavutil, libavcodec, and libavformat, and check their versions.

DIRECTORY is useful for builds which have not been installed.  When it is NIL,
LUV_FFMPEG_LIBDIR is consulted before the platform soname search.  Signals
LIBAV-VERSION-MISMATCH when a loaded library disagrees with the headers this
system was groveled against."
  (or *libav-libraries*
      (let* ((override (or directory
                           (uiop:getenv "LUV_FFMPEG_LIBDIR")
                           *libav-default-directory*))
             (cffi:*foreign-library-directories*
               (if override
                   (cons (uiop:ensure-directory-pathname override)
                         cffi:*foreign-library-directories*)
                   cffi:*foreign-library-directories*)))
        (setf *libav-libraries*
              (loop for (stem major)
                      in (list (list "avutil" +avutil-version-major+)
                               (list "avcodec" +avcodec-version-major+)
                               (list "avformat" +avformat-version-major+)
                               (list "swscale" +swscale-version-major+))
                    collect (cffi:load-foreign-library
                             (cons :or (library-sonames stem major)))))
        (check-libav-versions)
        *libav-libraries*)))

(defun libav-loaded-p ()
  (not (null *libav-libraries*)))

(cffi:defcfun ("avutil_version" %avutil-version) :unsigned-int)
(cffi:defcfun ("avcodec_version" %avcodec-version) :unsigned-int)
(cffi:defcfun ("avformat_version" %avformat-version) :unsigned-int)
(cffi:defcfun ("swscale_version" %swscale-version) :unsigned-int)
(cffi:defcfun ("av_version_info" %av-version-info) :string)
(cffi:defcfun ("avutil_configuration" %avutil-configuration) :string)

(defun version-triple (version)
  "Split an AV_VERSION_INT into its major, minor, and micro parts."
  (list (ldb (byte 8 16) version)
        (ldb (byte 8 8) version)
        (ldb (byte 8 0) version)))

(defun check-libav-versions ()
  "Signal unless every loaded library matches the headers we groveled."
  ;; AV-FRAME spells its arrays as a literal 8 because :COUNT cannot hold a
  ;; constant; this is what keeps that literal honest.
  (assert (= 8 +data-pointer-count+) ()
          "AV_NUM_DATA_POINTERS is ~D, but AV-FRAME was written for 8."
          +data-pointer-count+)
  (loop for (name compiled reader)
          in (list (list "libavutil" +avutil-version-major+ #'%avutil-version)
                   (list "libavcodec" +avcodec-version-major+ #'%avcodec-version)
                   (list "libavformat" +avformat-version-major+ #'%avformat-version)
                   (list "libswscale" +swscale-version-major+ #'%swscale-version))
        for loaded = (ldb (byte 8 16) (funcall reader))
        unless (= compiled loaded)
          do (error 'libav-version-mismatch
                    :library name :compiled compiled :loaded loaded))
  t)

(defun libav-build ()
  "Return FFmpeg's own version string, such as \"8.1.2\"."
  (load-libav)
  (%av-version-info))

(defun libav-versions ()
  "Return an alist of library name to (major minor micro)."
  (load-libav)
  (list (cons "libavutil" (version-triple (%avutil-version)))
        (cons "libavcodec" (version-triple (%avcodec-version)))
        (cons "libavformat" (version-triple (%avformat-version)))
        (cons "libswscale" (version-triple (%swscale-version)))))

(defun libav-configuration ()
  "Return the ./configure line this FFmpeg was built with."
  (load-libav)
  (%avutil-configuration))

;;; Errors.

(cffi:defcfun ("av_strerror" %av-strerror) :int
  (code :int) (buffer :pointer) (size :size))

(defun libav-error-message (condition)
  "Return FFmpeg's own description of CONDITION's error code."
  (let ((code (libav-error-code condition)))
    (cffi:with-foreign-pointer (buffer 256)
      (if (minusp (%av-strerror code buffer 256))
          (format nil "unknown FFmpeg error ~D" code)
          (cffi:foreign-string-to-lisp buffer)))))

(defun check-code (code operation)
  "Return CODE, or signal LIBAV-ERROR when FFmpeg reported a failure."
  (if (minusp code)
      (error 'libav-error :operation operation :code code)
      code))

;;; Formats and hardware devices.
;;;
;;; The iteration and naming entry points are declared with :INT rather than
;;; the groveled enums on purpose.  A CFFI enum translation errors on a value
;;; it has never heard of, and the set of hardware device types a given FFmpeg
;;; was built with is exactly the thing we are asking about -- so the answer
;;; comes back through FFmpeg's own name strings, and an unlisted type still
;;; arrives as a perfectly good keyword.

(cffi:defcfun ("av_get_pix_fmt_name" %av-get-pix-fmt-name) :string
  (format :int))
(cffi:defcfun ("av_hwdevice_iterate_types" %av-hwdevice-iterate-types) :int
  (previous :int))
(cffi:defcfun ("av_hwdevice_get_type_name" %av-hwdevice-get-type-name) :string
  (type :int))
(cffi:defcfun ("avcodec_find_decoder_by_name" %avcodec-find-decoder-by-name)
    :pointer
  (name :string))

(defun pixel-format-name (format)
  "Return FFmpeg's name for FORMAT, a PIXEL-FORMAT keyword or raw integer."
  (load-libav)
  (%av-get-pix-fmt-name
   (if (keywordp format)
       (cffi:foreign-enum-value 'pixel-format format)
       format)))

(defun name-keyword (name)
  (and name (intern (string-upcase (substitute #\- #\_ name)) :keyword)))

(defun hardware-device-types ()
  "Return the hardware device types this FFmpeg was built with, as keywords.

The list is whatever the library reports, so a type this system does not name
in its HARDWARE-DEVICE-TYPE enumeration still appears, keyed by FFmpeg's own
name for it."
  (load-libav)
  (loop with type = 0
        do (setf type (%av-hwdevice-iterate-types type))
        until (zerop type)
        collect (name-keyword (%av-hwdevice-get-type-name type))))

(defun hardware-device-type-name (type)
  "Return FFmpeg's name for TYPE, a HARDWARE-DEVICE-TYPE keyword."
  (load-libav)
  (%av-hwdevice-get-type-name
   (cffi:foreign-enum-value 'hardware-device-type type)))

(defun decoder-available-p (name)
  "Return true when this FFmpeg has a decoder called NAME, such as \"h264\"."
  (load-libav)
  (not (cffi:null-pointer-p (%avcodec-find-decoder-by-name name))))

;;; Frames.

(cffi:defcfun ("av_frame_alloc" %av-frame-alloc) :pointer)
(cffi:defcfun ("av_frame_free" %av-frame-free) :void (frame :pointer))
(cffi:defcfun ("av_frame_unref" %av-frame-unref) :void (frame :pointer))
(cffi:defcfun ("av_frame_get_buffer" %av-frame-get-buffer) :int
  (frame :pointer) (alignment :int))
