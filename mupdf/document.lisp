;;;; Documents, pages, and what a page can be turned into.
;;;;
;;;; Everything here owns a C object and is responsible for dropping it, so
;;;; the objects are CLOS instances with explicit closers and WITH- macros
;;;; rather than pointers handed around bare.  MuPDF's own ownership rule is
;;;; that a context outlives everything made from it, which is why the
;;;; document keeps a reference to its context rather than the other way
;;;; round.

(in-package #:luv.mupdf)

(defclass context ()
  ((pointer :initarg :pointer :accessor context-pointer))
  (:documentation
   "A MuPDF context: its allocator, its store, and its error stack."))

(defun make-context (&key (max-store (* 256 1024 1024)))
  "Open a MuPDF context with the document handlers registered."
  (load-mupdf)
  (let* ((version (or *mupdf-version*
                      (mupdf-version-from-directory
                       (or (uiop:getenv "LUV_MUPDF_LIBDIR")
                           *mupdf-default-directory*))))
         (pointer (and version
                       (%fz-new-context-imp (cffi:null-pointer)
                                            (cffi:null-pointer)
                                            max-store version))))
    (when (or (null version)
              (null pointer)
              (cffi:null-pointer-p pointer))
      (error 'mupdf-error :operation 'make-context
                          :detail
                          (format nil "no context for claimed version ~S; ~
set LUV.MUPDF:*MUPDF-VERSION* to the library's FZ_VERSION"
                                  version)))
    (%fz-register-document-handlers pointer)
    (make-instance 'context :pointer pointer)))

(defun close-context (context)
  (alexandria:when-let ((pointer (context-pointer context)))
    (setf (context-pointer context) nil)
    (%fz-drop-context pointer))
  context)

(defmacro with-mupdf-context ((variable &rest options) &body body)
  `(let ((,variable (make-context ,@options)))
     (unwind-protect (progn ,@body)
       (close-context ,variable))))

;;;; Documents

(defclass document ()
  ((context :initarg :context :reader document-context)
   (pointer :initarg :pointer :accessor document-pointer)
   (pathname :initarg :pathname :reader document-pathname))
  (:documentation "An open document and the context it was opened in."))

(defmethod print-object ((document document) stream)
  (print-unreadable-object (document stream :type t)
    (format stream "~A~@[ ~D page~:P~]"
            (file-namestring (document-pathname document))
            (when (document-pointer document) (document-page-count document)))))

(defun pdf-file-p (pathname)
  "True when PATHNAME begins with a PDF header.

This is a guard, not a validation.  MuPDF reports a broken file by longjmp to
a handler this binding cannot install, and with none on the stack it takes
the process with it -- so the cheapest thing that catches the ordinary
mistake of handing it something that is not a PDF at all is worth doing."
  (ignore-errors
   (with-open-file (stream pathname :element-type '(unsigned-byte 8))
     (let ((header (make-array 5 :element-type '(unsigned-byte 8))))
       (and (= 5 (read-sequence header stream))
            (equalp header #(#x25 #x50 #x44 #x46 #x2D)))))))

(defun open-document (pathname &key context)
  "Open PATHNAME.  A context is made for the document unless one is given."
  (let ((truename (or (probe-file pathname)
                      (error 'mupdf-error :operation 'open-document
                                          :detail "no such file"))))
    (unless (pdf-file-p truename)
      (error 'mupdf-error :operation 'open-document
                          :detail "not a PDF: no %PDF- header"))
    (let* ((context (or context (make-context)))
           (pointer (%fz-open-document (context-pointer context)
                                       (namestring truename))))
      (when (cffi:null-pointer-p pointer)
        (error 'mupdf-error :operation 'open-document :detail truename))
      (make-instance 'document :context context :pointer pointer
                               :pathname truename))))

(defun close-document (document)
  (alexandria:when-let ((pointer (document-pointer document)))
    (setf (document-pointer document) nil)
    (%fz-drop-document (context-pointer (document-context document)) pointer))
  document)

(defmacro with-document ((variable pathname &rest options) &body body)
  `(let ((,variable (open-document ,pathname ,@options)))
     (unwind-protect (progn ,@body)
       (close-document ,variable))))

(defun document-page-count (document)
  (%fz-count-pages (context-pointer (document-context document))
                   (document-pointer document)))

(defmacro with-page ((variable document number) &body body)
  "Load page NUMBER of DOCUMENT for the duration of BODY."
  (let ((context (gensym "CONTEXT")))
    `(let* ((,context (context-pointer (document-context ,document)))
            (,variable (%fz-load-page ,context (document-pointer ,document)
                                      ,number)))
       (unwind-protect (progn ,@body)
         (%fz-drop-page ,context ,variable)))))

;;;; Measuring

(defun page-bounds (document number)
  "Page NUMBER's box in PDF points, as left, top, right, bottom."
  (with-page (page document number)
    (cffi:with-foreign-object (rect '(:struct fz-rect))
      (let ((value (%fz-bound-page (context-pointer (document-context document))
                                   page)))
        (declare (ignore rect))
        (values (getf value 'x0) (getf value 'y0)
                (getf value 'x1) (getf value 'y1))))))

(defun page-size (document number)
  "Page NUMBER's width and height in PDF points, which are 1/72 inch."
  (multiple-value-bind (x0 y0 x1 y1) (page-bounds document number)
    (values (- x1 x0) (- y1 y0))))

;;;; Pixels

(defun page-rgba-words (document number &key (scale 1.0))
  "Render page NUMBER at SCALE and return HEIGHT by WIDTH packed RGBA words.

The words are alpha in the high byte and red in the third, which is what a
CLIM pattern wants, so the result can be handed straight to MAKE-PATTERN."
  (let ((context (context-pointer (document-context document))))
    (with-page (page document number)
      (let ((pixmap
              (cffi:with-foreign-object (matrix '(:struct fz-matrix))
                (setf (cffi:foreign-slot-value matrix '(:struct fz-matrix) 'a)
                      (float scale 1.0)
                      (cffi:foreign-slot-value matrix '(:struct fz-matrix) 'b) 0.0
                      (cffi:foreign-slot-value matrix '(:struct fz-matrix) 'c) 0.0
                      (cffi:foreign-slot-value matrix '(:struct fz-matrix) 'd)
                      (float scale 1.0)
                      (cffi:foreign-slot-value matrix '(:struct fz-matrix) 'e) 0.0
                      (cffi:foreign-slot-value matrix '(:struct fz-matrix) 'f) 0.0)
                (%fz-new-pixmap-from-page
                 context page
                 (cffi:mem-ref matrix '(:struct fz-matrix))
                 (%fz-device-rgb context) 0))))
        (when (cffi:null-pointer-p pixmap)
          (error 'mupdf-error :operation 'page-rgba-words))
        (unwind-protect
             (let* ((width (%fz-pixmap-width context pixmap))
                    (height (%fz-pixmap-height context pixmap))
                    (stride (%fz-pixmap-stride context pixmap))
                    (components (%fz-pixmap-components context pixmap))
                    (samples (%fz-pixmap-samples context pixmap))
                    (words (make-array (list height width)
                                       :element-type '(unsigned-byte 32))))
               (dotimes (y height words)
                 (let ((row (+ (cffi:pointer-address samples) (* y stride))))
                   (dotimes (x width)
                     (let ((base (cffi:make-pointer (+ row (* x components)))))
                       (setf (aref words y x)
                             (logior
                              (ash 255 24)
                              (ash (cffi:mem-ref base :unsigned-char 0) 16)
                              (ash (cffi:mem-ref base :unsigned-char 1) 8)
                              (cffi:mem-ref base :unsigned-char 2))))))))
          (%fz-drop-pixmap context pixmap))))))

;;;; Text
;;;;
;;;; MuPDF serializes structured text to XML itself, so the positions come
;;;; out of a documented format rather than out of struct offsets this system
;;;; would otherwise have to grovel.  The XML is regular enough to scan
;;;; without a parser: what is wanted is the font size in force, and each
;;;; character with the box it occupies.

(defstruct (text-run (:constructor make-text-run))
  (string "" :type string)
  (x 0.0 :type single-float)
  (y 0.0 :type single-float)
  (width 0.0 :type single-float)
  (height 0.0 :type single-float)
  (size 0.0 :type single-float)
  (font nil))

(defun parse-decimal (string &key (start 0) (end (length string)))
  "A decimal number out of STRING, without going through READ.

The values in this XML are all plain decimals, and READ on text that came out
of a document is a wider door than this needs to open."
  (let ((sign 1) (whole 0) (fraction 0) (scale 1) (index start) (seen nil))
    (when (and (< index end) (member (char string index) '(#\- #\+)))
      (when (char= (char string index) #\-) (setf sign -1))
      (incf index))
    (loop while (and (< index end) (digit-char-p (char string index)))
          do (setf whole (+ (* whole 10) (digit-char-p (char string index)))
                   seen t)
             (incf index))
    (when (and (< index end) (char= (char string index) #\.))
      (incf index)
      (loop while (and (< index end) (digit-char-p (char string index)))
            do (setf fraction (+ (* fraction 10)
                                 (digit-char-p (char string index)))
                     scale (* scale 10)
                     seen t)
               (incf index)))
    (when seen
      (float (* sign (+ whole (/ fraction scale))) 1.0))))

(defun parse-decimals (string)
  "Every space-separated decimal in STRING, in order."
  (let ((values '()) (start 0) (length (length string)))
    (loop while (< start length)
          do (let ((end (or (position #\Space string :start start) length)))
               (alexandria:when-let
                   ((value (parse-decimal string :start start :end end)))
                 (push value values))
               (setf start (1+ end))))
    (nreverse values)))

(defun xml-attribute (text start name)
  "The value of attribute NAME in the tag beginning at START, or NIL.

MuPDF escapes the characters that would end a tag or a quoted value, so the
end of the tag and the end of the value can both be found by looking for the
literal character."
  (let* ((tag-end (or (position #\> text :start start) (length text)))
         (key (concatenate 'string " " name "=\""))
         (at (search key text :start2 start :end2 tag-end)))
    (when at
      (let* ((from (+ at (length key)))
             (to (position #\" text :start from)))
        (when (and to (<= to tag-end))
          (subseq text from to))))))

(defun unescape-xml (string)
  "The five escapes MuPDF's writer emits, turned back into characters."
  (if (find #\& string)
      (with-output-to-string (out)
        (let ((index 0) (length (length string)))
          (loop while (< index length)
                do (let ((character (char string index)))
                     (if (char= character #\&)
                         (let ((semicolon (position #\; string :start index)))
                           (let ((name (and semicolon
                                            (subseq string (1+ index)
                                                    semicolon))))
                             (cond
                               ((null name) (write-char character out)
                                            (incf index))
                               (t
                                (write-string
                                 (cond ((string= name "amp") "&")
                                       ((string= name "lt") "<")
                                       ((string= name "gt") ">")
                                       ((string= name "quot") "\"")
                                       ((string= name "apos") "'")
                                       (t (format nil "&~A;" name)))
                                 out)
                                (setf index (1+ semicolon))))))
                         (progn (write-char character out) (incf index)))))))
      string))

(defun page-text-runs (document number)
  "Page NUMBER's text as one TEXT-RUN per typeset line.

Positions are in PDF points with the origin at the page's top left, which is
the orientation MuPDF's structured text already uses and the one a drawing
surface wants.

MuPDF writes the font after the line it applies to, so a line is held back
until the font element that follows names its size."
  (let ((text (page-stext-xml document number))
        (runs '())
        (pending nil))
    (let ((index 0))
      (loop
        (let ((line-at (search "<line " text :start2 index))
              (font-at (search "<font " text :start2 index)))
          (cond
            ((and (null line-at) (null font-at)) (return))
            ((or (null font-at) (and line-at (< line-at font-at)))
             (let ((bbox (xml-attribute text line-at "bbox"))
                   (string (xml-attribute text line-at "text")))
               (setf pending
                     (when (and bbox string (plusp (length string)))
                       (destructuring-bind (&optional x0 y0 x1 y1)
                           (parse-decimals bbox)
                         (when y1
                           (make-text-run :string (unescape-xml string)
                                          :x x0 :y y0
                                          :width (- x1 x0)
                                          :height (- y1 y0))))))
               (setf index (1+ line-at))))
            (t
             (when pending
               (alexandria:when-let
                   ((size (xml-attribute text font-at "size")))
                 (setf (text-run-size pending)
                       (or (parse-decimal size) 0.0)))
               (setf (text-run-font pending)
                     (xml-attribute text font-at "name"))
               (push pending runs)
               (setf pending nil))
             (setf index (1+ font-at)))))))
    (nreverse runs)))

(defun page-stext-xml (document number)
  "Page NUMBER's structured text, as MuPDF's own XML."
  (let ((context (context-pointer (document-context document))))
    (with-page (page document number)
      (let ((stext (%fz-new-stext-page-from-page context page
                                                 (cffi:null-pointer))))
        (when (cffi:null-pointer-p stext)
          (error 'mupdf-error :operation 'page-stext-xml))
        (unwind-protect
             (let ((buffer (%fz-new-buffer context 65536)))
               (unwind-protect
                    (let ((output (%fz-new-output-with-buffer context buffer)))
                      (unwind-protect
                           (progn
                             (%fz-print-stext-page-as-xml
                              context output stext number)
                             (%fz-close-output context output)
                             (cffi:with-foreign-object (data :pointer)
                               (let ((length (%fz-buffer-storage
                                              context buffer data)))
                                 (cffi:foreign-string-to-lisp
                                  (cffi:mem-ref data :pointer)
                                  :count length :encoding :utf-8))))
                        (%fz-drop-output context output)))
                 (%fz-drop-buffer context buffer)))
          (%fz-drop-stext-page context stext))))))
