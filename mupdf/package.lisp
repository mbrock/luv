;;;; MuPDF from Lisp.
;;;;
;;;; MuPDF is a PDF engine rather than a viewer: it will hand you a page as
;;;; pixels, or as the text and the boxes that text sits in, and leave the
;;;; presenting to you.  Both are wanted here.  The pixels prove the binding
;;;; and are the honest fallback for a page full of vector art; the text is
;;;; what lets a page become a thing drawn in the world with the glyph
;;;; renderer luv already has, rather than a photograph of a page.
;;;;
;;;; The binding is deliberately narrow.  Almost everything MuPDF exposes is
;;;; an opaque pointer with accessor functions, so the only C layouts this
;;;; system has to know are FZ_MATRIX and FZ_RECT -- six floats and four --
;;;; which have not moved in the library's lifetime.  Anything whose layout
;;;; does move, such as the structured-text records, is read through groveled
;;;; offsets rather than transcribed ones.

(defpackage #:luv.mupdf
  (:use #:cl)
  (:documentation
   "A narrow binding to MuPDF: open a document, measure a page, and get it
back as pixels or as positioned text.")
  (:export #:mupdf-error
           #:mupdf-unavailable
           #:load-mupdf
           #:mupdf-loaded-p
           #:*mupdf-default-directory*
           ;; contexts and documents
           #:context
           #:context-pointer
           #:with-mupdf-context
           #:document
           #:document-pathname
           #:document-page-count
           #:open-document
           #:close-document
           #:with-document
           ;; pages
           #:page-bounds
           #:page-size
           #:page-rgba-words
           #:page-text-runs
           ;; a run of text
           #:text-run
           #:text-run-p
           #:text-run-string
           #:text-run-x
           #:text-run-y
           #:text-run-size
           #:text-run-width
           #:text-run-height
           #:text-run-font))
