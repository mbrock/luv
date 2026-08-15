;;;; The ./wiki command: the wiki corpus from a shell.
;;;;
;;;; A small executable built with ASDF's program-op, for agents and people
;;;; who want the table of contents, the work marks, a figure's text, or the
;;;; places something is mentioned without opening Emacs or a browser.  It
;;;; reads the same objects the site is rendered from, so what it prints
;;;; agrees with what the site shows.

(defpackage #:luv.wiki.cli
  (:use #:cl)
  (:local-nicknames (#:wiki #:luv.wiki))
  (:export #:main #:run))

(in-package #:luv.wiki.cli)

(defvar *build-source-registry* nil
  "CL_SOURCE_REGISTRY as seen when the executable was built.")
(defvar *build-output-translations* nil
  "ASDF_OUTPUT_TRANSLATIONS as seen when the executable was built.")

(defvar *build-sbcl-home* nil
  "SBCL's home directory at build time, where its contrib modules live.")

(defun capture-asdf-configuration ()
  "Remember the ASDF environment of the build, so that the executable can
find systems and their compiled files without the sbcl wrapper's
environment, and SBCL's home so that (require :sb-concurrency) and other
contribs still work from an executable outside SBCL's own directory.
Called by build-wiki.lisp before the image is dumped."
  (setf *build-source-registry* (uiop:getenv "CL_SOURCE_REGISTRY")
        *build-output-translations* (uiop:getenv "ASDF_OUTPUT_TRANSLATIONS")
        *build-sbcl-home* (sb-int:sbcl-homedir-pathname)))

(defun restore-asdf-configuration ()
  "Re-establish the build's ASDF configuration in the running executable.
The Nix shell sets a smaller CL_SOURCE_REGISTRY of its own (the sbcl wrapper
adds the packaged systems only for sbcl processes), so the captured
configuration is used whenever it exists."
  (when (and *build-sbcl-home* (null (sb-int:sbcl-homedir-pathname)))
    (setf sb-sys::*sbcl-homedir-pathname* *build-sbcl-home*))
  (when *build-output-translations*
    (asdf:initialize-output-translations *build-output-translations*))
  (when *build-source-registry*
    (asdf:initialize-source-registry *build-source-registry*)))

(defvar *root* nil
  "The repository root: the directory holding luv.asd and wiki/.")

(defun root ()
  (or *root*
      (setf *root*
            (uiop:ensure-directory-pathname
             (or (uiop:getenv "LUV_ROOT") (uiop:getcwd))))))

(defun ensure-systems ()
  "Register the luv systems from luv.asd so the source scan knows the files.
luv-wiki itself is already in this image; luv.asd only re-reads its
definition, which is harmless."
  (asdf:load-asd (merge-pathnames "luv.asd" (root))))

(defvar *site* nil)

(defun site (&key (code t))
  "The SITE over the wiki pages, and over the source files unless CODE is NIL."
  (or *site*
      (setf *site*
            (let* ((wiki (merge-pathnames "wiki/" (root)))
                   (documents (mapcar #'wiki:read-org-file
                                      (sort (directory (merge-pathnames "*.org" wiki))
                                            #'string< :key #'pathname-name))))
              ;; index.org first, then the rest as on the site.
              (setf documents (append (remove "index" documents :key #'wiki:document-name :test-not #'string=)
                                      (remove "index" documents :key #'wiki:document-name :test #'string=)))
              (when code (ensure-systems))
              (wiki::load-arglists (merge-pathnames "wiki/arglists.sexp" (root)))
              (let ((sources (and code (wiki:code-sources :root (root)))))
                (wiki:make-site documents
                                :source-files sources
                                :systems (and code (wiki::code-systems sources))
                                :source-directory (root)))))))

;;; Plain text

(defun inline-text (inlines)
  (wiki::inline-text inlines))

(defun fill-text (text &key (indent 0) (width 76))
  "Reflow TEXT into lines of at most WIDTH columns with INDENT spaces."
  (let ((words (uiop:split-string text :separator '(#\Space #\Newline #\Tab)))
        (line '())
        (length indent)
        (lines '()))
    (flet ((flush ()
             (when line
               (push (format nil "~v@T~{~A~^ ~}" indent (nreverse line)) lines)
               (setf line '() length indent))))
      (dolist (word (remove "" words :test #'string=))
        (when (and line (> (+ length 1 (length word)) width))
          (flush))
        (push word line)
        (incf length (+ 1 (length word))))
      (flush))
    (format nil "~{~A~^~%~}" (nreverse lines))))

(defgeneric element-text (element &key indent)
  (:documentation "ELEMENT as plain text for a terminal.")
  (:method ((string string) &key (indent 0))
    (fill-text string :indent indent))
  (:method ((paragraph wiki:paragraph) &key (indent 0))
    (fill-text (inline-text (wiki:element-children paragraph)) :indent indent))
  (:method ((list wiki:plain-list) &key (indent 0))
    (format nil "~{~A~^~%~}"
            (loop for item in (wiki:element-children list)
                  for n from 1
                  collect (let ((bullet (if (wiki:plain-list-ordered-p list) (format nil "~D." n) "-")))
                            (format nil "~v@T~A ~A" indent bullet
                                    (string-left-trim " " (element-text item :indent (+ indent 2))))))))
  (:method ((item wiki:list-item) &key (indent 0))
    (format nil "~{~A~^~%~}"
            (mapcar (lambda (child) (element-text child :indent indent))
                    (wiki:element-children item))))
  (:method ((block wiki:example-block) &key (indent 0))
    (format nil "~{~v@T~A~^~%~}"
            (loop for line in (uiop:split-string (wiki:block-text block) :separator '(#\Newline))
                  collect indent collect line)))
  (:method ((table wiki:table) &key (indent 0))
    (format nil "~{~v@T~A~^~%~}"
            (loop for row in (wiki:table-rows table)
                  collect indent
                  collect (format nil "~{~A~^ | ~}" (mapcar #'inline-text row)))))
  (:method ((heading wiki:heading) &key (indent 0))
    (declare (ignore indent))
    ""))

(defun definition-place (definition)
  "FILE:LINE with the file relative to the repository root."
  (format nil "~A:~D"
          (namestring (uiop:enough-pathname (wiki::definition-pathname definition) (root)))
          (wiki:definition-line definition)))

(defun heading-line (heading)
  (format nil "~@[~A ~]~A" (wiki:heading-keyword heading) (inline-text (wiki:heading-title heading))))

(defun section-text (heading)
  "The prose of HEADING's own section, without subheadings."
  (format nil "~{~A~^~%~%~}"
          (loop for child in (wiki:element-children heading)
                unless (typep child 'wiki:heading)
                  collect (element-text child))))

;;; Commands

(defvar *commands* '()
  "Alist of (name docstring function).")

(defmacro define-command (name (&rest lambda-list) documentation &body body)
  `(progn
     (defun ,name ,lambda-list ,documentation ,@body)
     (setf *commands* (append (remove ',name *commands* :key #'third)
                              (list (list ,(string-downcase (symbol-name name))
                                          ,documentation ',name))))))

(define-command help (&rest arguments)
  "Show this help."
  (declare (ignore arguments))
  (format t "usage: wiki COMMAND [ARGUMENTS]~%~%")
  (loop for (name documentation) in *commands*
        do (format t "  ~12A ~A~%" name (first (uiop:split-string documentation :separator '(#\Newline))))))

(define-command toc (&rest names)
  "Table of contents: every page with its figures, IDs, and work marks.
Give page names to restrict."
  (dolist (document (wiki:site-documents (site :code nil)))
    (when (or (null names) (member (wiki:document-name document) names :test #'string=))
      (format t "~&~A  (~A.org)~%" (wiki:document-title document) (wiki:document-name document))
      (dolist (figure (wiki:document-figures document))
        (format t "~v@T~A  ~A~%"
                (* 2 (wiki:heading-level figure))
                (wiki:heading-id figure)
                (heading-line figure)))
      (terpri))))

(defparameter *status-order* '("NEXT" "TODO" "WAIT" "IDEA" "DONE"))

(define-command marks (&rest statuses)
  "Work marks by status: NEXT, TODO, WAIT, IDEA, DONE.  Give statuses to restrict."
  (let ((statuses (or (mapcar #'string-upcase statuses) *status-order*))
        (marks '()))
    (dolist (document (wiki:site-documents (site :code nil)))
      (dolist (figure (wiki:document-figures document))
        (when (wiki:heading-keyword figure)
          (push figure marks))))
    (setf marks (nreverse marks))
    (dolist (status statuses)
      (let ((these (remove status marks :key #'wiki:heading-keyword :test-not #'string=)))
        (when these
          (format t "~&~A~%" status)
          (dolist (mark these)
            (format t "  ~A  ~A  (~A)~%"
                    (wiki:heading-id mark)
                    (inline-text (wiki:heading-title mark))
                    (wiki:document-name (wiki:heading-document mark))))
          (terpri))))))

(defun print-figure (figure &key (site *site*))
  (let ((id (wiki:heading-id figure)))
    (format t "~&~A  ~A~%~A.org, level ~D~%~%"
            id (heading-line figure)
            (wiki:document-name (wiki:heading-document figure))
            (wiki:heading-level figure))
    (let ((text (section-text figure)))
      (when (plusp (length text)) (format t "~A~%~%" text)))
    (let ((children (remove-if-not (lambda (c) (typep c 'wiki:heading)) (wiki:element-children figure))))
      (when children
        (format t "Subheadings:~%")
        (dolist (child children)
          (format t "  ~@[~A  ~]~A~%" (wiki:heading-id child) (heading-line child)))
        (terpri)))
    (let ((backlinks (gethash id (wiki:site-backlinks site))))
      (when backlinks
        (format t "Mentioned in:~%")
        (dolist (other backlinks)
          (format t "  ~A  ~A  (~A)~%" (wiki:heading-id other) (heading-line other)
                  (wiki:document-name (wiki:heading-document other))))
        (terpri)))
    (let ((references (gethash id (wiki:site-code-references site))))
      (when references
        (format t "Referenced from code:~%")
        (dolist (definition references)
          (format t "  ~A ~A~{ ~A~}  ~A~%"
                  (wiki:definition-kind definition) (wiki:definition-name definition)
                  (wiki:definition-qualifiers definition)
                  (definition-place definition)))
        (terpri)))))

(define-command figure (&rest ids)
  "Print figures by ID: title, page, text, subheadings, backlinks, code references."
  (let ((site (site)))
    (dolist (id ids)
      (let ((figure (wiki:find-figure (string-upcase id) site)))
        (if figure
            (print-figure figure :site site)
            (format t "~&No figure ~A~%" id))))))

(define-command page (&rest names)
  "Print whole pages as text."
  (let ((site (site)))
    (dolist (name names)
      (let ((document (find name (wiki:site-documents site) :key #'wiki:document-name :test #'string=)))
        (if document
            (progn
              (format t "~&~A~%~v@{=~}~%~%" (wiki:document-title document) (length (wiki:document-title document)) nil)
              (dolist (child (wiki:element-children document))
                (if (typep child 'wiki:heading)
                    (wiki::map-elements
                     (lambda (e)
                       (when (typep e 'wiki:heading)
                         (format t "~v@{*~} ~A~@[  #~A~]~%~%" (wiki:heading-level e) nil
                                 (heading-line e) (wiki:heading-id e))
                         (let ((text (section-text e)))
                           (when (plusp (length text)) (format t "~A~%~%" text)))))
                     child)
                    (format t "~A~%~%" (element-text child)))))
            (format t "~&No page ~A~%" name))))))

(define-command mentions (&rest ids)
  "Where figures are mentioned: other figures and code definitions."
  (let ((site (site)))
    (dolist (id ids)
      (let* ((id (string-upcase id))
             (backlinks (gethash id (wiki:site-backlinks site)))
             (references (gethash id (wiki:site-code-references site))))
        (format t "~&~A~%" id)
        (dolist (other backlinks)
          (format t "  ~A  ~A  (~A)~%" (wiki:heading-id other) (heading-line other)
                  (wiki:document-name (wiki:heading-document other))))
        (dolist (definition references)
          (format t "  ~A ~A  ~A~%"
                  (wiki:definition-kind definition) (wiki:definition-name definition)
                  (definition-place definition)))
        (unless (or backlinks references) (format t "  (nowhere)~%"))))))

(define-command dangling (&rest arguments)
  "Mentions in pages and code that no figure resolves."
  (declare (ignore arguments))
  (let ((site (site)))
    (loop for (document . ids) in (wiki:dangling-mentions site)
          do (format t "~&~A.org: ~{~A~^ ~}~%" (wiki:document-name document) ids))
    (loop for (definition . ids) in (wiki:dangling-code-mentions site)
          do (format t "~&~A ~A (~A): ~{~A~^ ~}~%"
                     (wiki:definition-kind definition) (wiki:definition-name definition)
                     (definition-place definition) ids))))

(define-command defs (&rest names)
  "Definitions in the source whose name contains each NAME (case-insensitive)."
  (let ((site (site)))
    (dolist (name names)
      (dolist (definition (wiki:site-definitions site))
        (when (search name (wiki:definition-name definition) :test #'char-equal)
          (format t "~&~A ~A~{ ~A~}  ~A~@[  ~{#~A~^ ~}~]~%"
                  (wiki:definition-kind definition) (wiki:definition-name definition)
                  (wiki:definition-qualifiers definition)
                  (definition-place definition)
                  (wiki:definition-mentions definition)))))))

(defparameter *id-characters* "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")

(define-command ids (&optional (count "12"))
  "Print COUNT fresh figure IDs that no page uses (default 12)."
  (let* ((site (site :code nil))
         (used (wiki:site-figures site))
         (state (make-random-state t)))
    (dotimes (i (parse-integer count))
      (loop for candidate = (coerce (loop repeat 6
                                          collect (char *id-characters*
                                                        (random (length *id-characters*) state)))
                                    'string)
            unless (or (gethash candidate used) (find-symbol candidate :keyword))
              do (setf (gethash candidate used) t)
                 (format t "~A~%" candidate)
                 (return)))))

(defparameter *introspection-systems* '("luv" "luv/luvcraft" "luv-wiki")
  "Systems loaded before gathering operator lambda lists.")

(define-command introspect (&rest systems)
  "Load the luv systems into this process and write the real lambda lists
of every operator the sources use to wiki/arglists.sexp, for the renderer's
derived layouts.  Give system names to load others instead."
  (ensure-systems)
  (dolist (name (or systems *introspection-systems*))
    (format t "~&loading ~A~%" name)
    (asdf:load-system name))
  (let* ((files (wiki:code-sources :root (root)))
         (pathname (merge-pathnames "wiki/arglists.sexp" (root)))
         (count (luv.wiki.introspect:write-arglists files pathname)))
    (format t "~&~D operators written to ~A~%" count pathname)))

(define-command build (&rest arguments)
  "Render the site into build/wiki/ with (asdf:make :luv/wiki)."
  (declare (ignore arguments))
  (ensure-systems)
  (asdf:make :luv/wiki)
  (format t "~&~A~%" (wiki:site-output-directory (asdf:find-system :luv/wiki))))

(defun run (arguments)
  "Run the command named by the first of ARGUMENTS."
  (let* ((name (or (first arguments) "help"))
         (command (assoc name *commands* :test #'string-equal)))
    (if command
        (apply (third command) (rest arguments))
        (progn
          (format *error-output* "wiki: unknown command ~A~%" name)
          (help)
          (uiop:quit 2)))))

(defun main ()
  "Entry point of the built executable."
  (handler-case
      (progn
        (restore-asdf-configuration)
        (run (uiop:command-line-arguments))
        (finish-output)
        (uiop:quit 0))
    (error (condition)
      (format *error-output* "~&wiki: ~A~%" condition)
      (uiop:quit 1))))
