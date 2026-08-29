;;;; Executable, typed figures embedded in trusted Org source.

(in-package #:luv.wiki)

(defclass visual-figure () ()
  (:documentation "A semantic figure which can write a native Typst fragment."))

(defgeneric write-figure-typst (figure stream)
  (:documentation "Write FIGURE as deterministic Typst content to STREAM."))

(defgeneric figure-alt-text (figure)
  (:documentation "Plain-text description of FIGURE for its web image."))

(defmethod figure-alt-text ((figure visual-figure))
  (declare (ignore figure))
  "Generated figure")

(defvar *figure-source-files* '()
  "Typst modules and other non-Lisp inputs used by registered figure classes.")

(defun register-figure-source-file (pathname)
  "Make PATHNAME an input to wiki rendering for correct incremental builds."
  (pushnew pathname *figure-source-files* :test #'equal :key #'namestring)
  pathname)

(defun figure-typst-source (figure)
  (with-output-to-string (stream)
    (write-figure-typst figure stream)))

(defun lisp-figure-block-p (element)
  (and (typep element 'src-block)
       (string= (src-block-language element) "lisp-figure")))

(defun lisp-figure-blocks (document)
  (let ((blocks '()))
    (map-elements (lambda (element)
                    (when (lisp-figure-block-p element)
                      (push element blocks)))
                  document)
    (nreverse blocks)))

(defun document-lisp-package (document)
  (let ((name (cdr (assoc "lisp-package" (document-keywords document)
                          :test #'string=))))
    (or (and name (find-package name))
        (and name
             (error "Unknown #+lisp-package ~S in ~A.org."
                    name (document-name document)))
        (find-package '#:cl-user))))

(defun read-lisp-figure-form (block document)
  (let ((*package* (document-lisp-package document))
        (*read-eval* nil)
        (eof (gensym "EOF")))
    (with-input-from-string (stream (block-text block))
      (let ((form (read stream nil eof)))
        (when (eq form eof)
          (error "Empty lisp-figure block in ~A.org." (document-name document)))
        (unless (eq (read stream nil eof) eof)
          (error "A lisp-figure block in ~A.org must contain exactly one form."
                 (document-name document)))
        form))))

(defun evaluate-lisp-figure (block document)
  (handler-case
      (let* ((*package* (document-lisp-package document))
             (value (eval (read-lisp-figure-form block document))))
        (unless (typep value 'visual-figure)
          (error "The lisp-figure block returned ~S, not a VISUAL-FIGURE."
                 (type-of value)))
        value)
    (error (condition)
      (error "Cannot build lisp-figure ~D in ~A.org: ~A"
             (1+ (or (position block (lisp-figure-blocks document) :test #'eq) 0))
             (document-name document) condition))))
