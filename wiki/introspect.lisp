;;;; Gathering real lambda lists for the operators the corpus uses.
;;;;
;;;; The dexp renderer draws a form according to a layout.  For the long tail
;;;; of operators, luv's own macros above all, the honest source of layout is
;;;; the operator's real lambda list: what precedes &BODY is the head, a
;;;; destructuring pattern in a position means a clause there, &KEY marks
;;;; where keyword pairs begin.  This file gathers those facts from a Lisp
;;;; image in which the systems are loaded, the way SLY's slynk-indentation
;;;; derives indentation from macro arglists, and writes them to
;;;; wiki/arglists.sexp so the site can be rendered without loading luv.

(defpackage #:luv.wiki.introspect
  (:use #:cl)
  (:local-nicknames (#:wiki #:luv.wiki))
  (:export #:gather-arglists #:write-arglists #:operator-symbols))

(in-package #:luv.wiki.introspect)

(defun package-designator-name (text)
  "The package name written in an IN-PACKAGE form: #:foo, :foo, \"foo\", foo."
  (let ((name (string-trim "#:\"" text)))
    (string-upcase name)))

(defun operator-symbols (source-files)
  "Every (package-name . symbol-name) that appears as the first element of a
list in SOURCE-FILES, resolved against each file's IN-PACKAGE for
unprefixed symbols; keywords and unknown packages are left out."
  (let ((seen (make-hash-table :test 'equal))
        (result '()))
    (dolist (file source-files)
      (let ((current (let ((p (wiki::source-file-package file)))
                       (and p (package-designator-name p)))))
        (labels ((walk (node)
                   (typecase node
                     (wiki:lisp-list
                      (let ((head (first (wiki:element-children node))))
                        (when (typep head 'wiki:lisp-symbol)
                          (let* ((package (wiki:lisp-symbol-package head))
                                 (package-name (cond ((null package) current)
                                                     ((stringp package) (string-upcase package))
                                                     (t nil))))
                            (when (and package-name (not (equal package-name "KEYWORD")))
                              (let ((key (cons package-name (string-upcase (wiki:lisp-symbol-name head)))))
                                (unless (gethash key seen)
                                  (setf (gethash key seen) t)
                                  (push key result)))))))
                      (mapc #'walk (wiki:element-children node)))
                     (wiki:lisp-prefix (walk (wiki::lisp-prefix-child node)))
                     (wiki:lisp-conditional (when (wiki::lisp-conditional-form node)
                                              (walk (wiki::lisp-conditional-form node)))))))
          (mapc #'walk (wiki:source-file-nodes file)))))
    (nreverse result)))

(defun sanitize (tree)
  "Turn a lambda list into readable data: symbols become their names,
lambda-list keywords keywords, other atoms strings; lists are kept."
  (typecase tree
    (null nil)
    (cons (cons (sanitize (car tree)) (sanitize (cdr tree))))
    (symbol (if (member tree lambda-list-keywords)
                (intern (symbol-name tree) :keyword)
                (symbol-name tree)))
    (t (princ-to-string tree))))

(defun symbol-facts (symbol)
  "A plist describing SYMBOL as an operator, or NIL if it is not one."
  (let ((kind (cond ((special-operator-p symbol) :special-operator)
                    ((macro-function symbol) :macro)
                    ((and (fboundp symbol) (typep (fdefinition symbol) 'generic-function)) :generic-function)
                    ((fboundp symbol) :function)
                    (t nil))))
    (when kind
      (let ((lambda-list (ignore-errors (sb-introspect:function-lambda-list symbol))))
        (list :kind kind :lambda-list (sanitize lambda-list))))))

(defun gather-arglists (source-files)
  "An alist ((home-package . name) . facts) for every operator of
SOURCE-FILES whose symbol exists and is fbound in this image, each symbol
once under its home package."
  (let ((seen (make-hash-table :test 'eq))
        (result '()))
    (loop for (package-name . name) in (operator-symbols source-files)
          for package = (find-package package-name)
          for symbol = (and package (find-symbol name package))
          for facts = (and symbol (not (gethash symbol seen)) (symbol-facts symbol))
          when facts
            do (setf (gethash symbol seen) t)
               (push (cons (cons (package-name (symbol-package symbol)) name) facts) result))
    (nreverse result)))

(defun write-arglists (source-files pathname)
  "Write the gathered facts to PATHNAME as one readable form per operator."
  (let ((entries (sort (gather-arglists source-files) #'string<
                       :key (lambda (entry) (format nil "~A:~A" (caar entry) (cdar entry))))))
    (with-open-file (out pathname :direction :output :if-exists :supersede)
      (with-standard-io-syntax
        (let ((*print-case* :downcase)
              (*print-readably* nil)
              (*package* (find-package :cl-user)))
          (format out ";;; Operator lambda lists gathered by scripts/wiki introspect.~%")
          (format out ";;; ((package . name) :kind KIND :lambda-list LIST), symbols as strings.~%~%")
          (dolist (entry entries)
            (prin1 entry out)
            (terpri out)))))
    (length entries)))
