;;; HarfBuzz owns text-to-glyph shaping; Slug owns the resulting outlines.

(in-package #:luv.slug)

(cffi:define-foreign-library harfbuzz
  (:darwin (:or "libharfbuzz.dylib" "libharfbuzz.0.dylib"))
  (:unix (:or "libharfbuzz.so" "libharfbuzz.so.0")))

(cffi:use-foreign-library harfbuzz)

(cffi:defcstruct hb-glyph-info
  (codepoint :uint32)
  (mask :uint32)
  (cluster :uint32)
  (var1 :uint32)
  (var2 :uint32))

(cffi:defcstruct hb-glyph-position
  (x-advance :int32)
  (y-advance :int32)
  (x-offset :int32)
  (y-offset :int32)
  (var :uint32))

(cffi:defcfun ("hb_blob_create" hb-blob-create) :pointer
  (data :pointer) (length :uint32) (mode :int)
  (user-data :pointer) (destroy :pointer))
(cffi:defcfun ("hb_blob_destroy" hb-blob-destroy) :void (blob :pointer))
(cffi:defcfun ("hb_face_create" hb-face-create) :pointer
  (blob :pointer) (index :uint32))
(cffi:defcfun ("hb_face_destroy" hb-face-destroy) :void (face :pointer))
(cffi:defcfun ("hb_face_get_upem" hb-face-get-upem) :uint32 (face :pointer))
(cffi:defcfun ("hb_font_create" hb-font-create) :pointer (face :pointer))
(cffi:defcfun ("hb_font_destroy" hb-font-destroy) :void (font :pointer))
(cffi:defcfun ("hb_font_set_scale" hb-font-set-scale) :void
  (font :pointer) (x-scale :int32) (y-scale :int32))
(cffi:defcfun ("hb_ot_font_set_funcs" hb-ot-font-set-funcs) :void
  (font :pointer))
(cffi:defcfun ("hb_buffer_create" hb-buffer-create) :pointer)
(cffi:defcfun ("hb_buffer_destroy" hb-buffer-destroy) :void (buffer :pointer))
(cffi:defcfun ("hb_buffer_add_utf8" hb-buffer-add-utf8) :void
  (buffer :pointer) (text :pointer) (text-length :int)
  (item-offset :uint32) (item-length :int))
(cffi:defcfun ("hb_buffer_set_direction" hb-buffer-set-direction) :void
  (buffer :pointer) (direction :uint32))
(cffi:defcfun ("hb_buffer_guess_segment_properties"
               hb-buffer-guess-segment-properties) :void
  (buffer :pointer))
(cffi:defcfun ("hb_shape" hb-shape) :void
  (font :pointer) (buffer :pointer) (features :pointer) (feature-count :uint32))
(cffi:defcfun ("hb_buffer_get_glyph_infos" hb-buffer-get-glyph-infos) :pointer
  (buffer :pointer) (length :pointer))
(cffi:defcfun ("hb_buffer_get_glyph_positions"
               hb-buffer-get-glyph-positions) :pointer
  (buffer :pointer) (length :pointer))

(define-condition slug-shaping-error (error)
  ((reason :initarg :reason :reader slug-shaping-error-reason)
   (details :initarg :details :initform nil :reader slug-shaping-error-details))
  (:report
   (lambda (condition stream)
     (format stream "Cannot shape Slug text: ~A~@[ (~S)~]."
             (slug-shaping-error-reason condition)
             (slug-shaping-error-details condition)))))

(defstruct slug-shaped-glyph
  glyph-id cluster x-advance y-advance x-offset y-offset)

(defstruct slug-shaped-text
  glyphs units-per-em x-advance y-advance)

(defun read-slug-font-bytes (pathname)
  (with-open-file (stream pathname :direction :input
                          :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length stream)
                             :element-type '(unsigned-byte 8))))
      (unless (= (read-sequence bytes stream) (length bytes))
        (error 'slug-shaping-error :reason :short-font-read
               :details pathname))
      bytes)))

(defun slug-harfbuzz-direction (direction)
  ;; hb_direction_t uses these stable public values so directions compose as
  ;; bit flags in HarfBuzz's C ABI.
  (ecase direction (:ltr 4) (:rtl 5) (:ttb 6) (:btt 7)))

(defun collect-slug-shaped-glyphs (buffer)
  (cffi:with-foreign-object (count :uint32)
    (let* ((infos (hb-buffer-get-glyph-infos buffer count))
           (positions (hb-buffer-get-glyph-positions buffer count))
           (length (cffi:mem-ref count :uint32))
           (glyphs (make-array length)))
      (dotimes (index length glyphs)
        (let ((info (cffi:mem-aptr infos '(:struct hb-glyph-info) index))
              (position
                (cffi:mem-aptr positions '(:struct hb-glyph-position) index)))
          (setf (aref glyphs index)
                (cffi:with-foreign-slots
                    ((codepoint cluster) info (:struct hb-glyph-info))
                  (cffi:with-foreign-slots
                      ((x-advance y-advance x-offset y-offset)
                       position (:struct hb-glyph-position))
                    (make-slug-shaped-glyph
                     :glyph-id codepoint :cluster cluster
                     :x-advance x-advance :y-advance y-advance
                     :x-offset x-offset :y-offset y-offset)))))))))

(defun shape-slug-text (string font-pathname &key direction)
  "Shape STRING with HarfBuzz, returning glyph IDs and font-unit placements.

DIRECTION may be :LTR, :RTL, :TTB, or :BTT.  With no direction HarfBuzz
guesses the segment properties from the Unicode text.  #4G7064"
  (let ((bytes (read-slug-font-bytes font-pathname))
        (blob nil) (face nil) (font nil) (buffer nil))
    (unwind-protect
         (cffi:with-pointer-to-vector-data (font-data bytes)
           ;; HB_MEMORY_MODE_DUPLICATE copies the bytes before this dynamic
           ;; pointer leaves scope.
           (setf blob (hb-blob-create font-data (length bytes) 0
                                      (cffi:null-pointer) (cffi:null-pointer))
                 face (hb-face-create blob 0)
                 font (hb-font-create face)
                 buffer (hb-buffer-create))
           (when (some #'cffi:null-pointer-p (list blob face font buffer))
             (error 'slug-shaping-error :reason :harfbuzz-allocation-failed))
           (let ((units-per-em (hb-face-get-upem face)))
             (unless (plusp units-per-em)
               (error 'slug-shaping-error :reason :invalid-units-per-em
                      :details units-per-em))
             (hb-ot-font-set-funcs font)
             (hb-font-set-scale font units-per-em units-per-em)
             (cffi:with-foreign-string ((text byte-count) string
                                        :encoding :utf-8)
               (let ((length (1- byte-count)))
                 (hb-buffer-add-utf8 buffer text length 0 length)))
             (when direction
               (hb-buffer-set-direction buffer
                                        (slug-harfbuzz-direction direction)))
             (hb-buffer-guess-segment-properties buffer)
             (hb-shape font buffer (cffi:null-pointer) 0)
             (let* ((glyphs (collect-slug-shaped-glyphs buffer))
                    (x-advance (loop for glyph across glyphs
                                     sum (slug-shaped-glyph-x-advance glyph)))
                    (y-advance (loop for glyph across glyphs
                                     sum (slug-shaped-glyph-y-advance glyph))))
               (make-slug-shaped-text
                :glyphs glyphs :units-per-em units-per-em
                :x-advance x-advance :y-advance y-advance))))
      (when buffer (hb-buffer-destroy buffer))
      (when font (hb-font-destroy font))
      (when face (hb-face-destroy face))
      (when blob (hb-blob-destroy blob)))))
