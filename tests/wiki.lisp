(defpackage #:luv-wiki/tests
  (:use #:cl #:rove)
  (:local-nicknames (#:wiki #:luv.wiki)))

(in-package #:luv-wiki/tests)

(defparameter *page*
  "#+title: A test page
#+startup: overview

Preamble mentions #ABC123 before any heading.

* First figure
:PROPERTIES:
:ID: ABC123
:END:

A paragraph with *bold*, /italic/, =verbatim=, ~code~, a [[https://example.org][link]],
and a plain [[file:other.org][page link]].  A mention of #DEF456 and a dangling
#ZZZ999.  A star * alone and 2 * 3 are not emphasis; nor is foo_bar.

- first item
- second item that
  wraps a line
- third *item*

1. one
2. two

#+begin_src lisp
(defun f (x)
  (* x 2))
#+end_src

#+begin_example
  keep < this & that
#+end_example

| Head A | Head B |
|--------+--------|
| a1     | =b1=   |

** NEXT A work mark
:PROPERTIES:
:ID: DEF456
:END:

Intent: mention [[id:ABC123]] with a link.

* Second figure without an ID

Nothing here refers to anything.
")

(defun page (&optional (text *page*) (name "test"))
  (wiki:read-org-string text :name name))

(deftest document-keywords-and-figures
  (let ((doc (page)))
    (ok (string= (wiki:document-title doc) "A test page"))
    (ok (equal (mapcar #'wiki:heading-id (wiki:document-figures doc))
               '("ABC123" "DEF456")))
    (let ((mark (second (wiki:document-figures doc))))
      (ok (string= (wiki:heading-keyword mark) "NEXT"))
      (ok (= (wiki:heading-level mark) 2))
      (ok (string= (wiki::inline-text (wiki:heading-title mark)) "A work mark")))))

(deftest inline-markup-reads-to-objects
  (let* ((inlines (wiki::read-inlines
                   "x *bold* /it/ =verb= ~code~ [[https://e.org][L]] #ABC123 2 * 3 foo_bar [[file:a.org::(defun f][=f=]]"))
         (objects (remove-if #'stringp inlines)))
    (ok (equal (mapcar #'type-of objects)
               '(wiki:emphasis wiki:emphasis wiki:emphasis wiki:emphasis
                 wiki:link wiki:mention wiki:link)))
    (ok (equal (mapcar #'wiki:emphasis-kind (subseq objects 0 4))
               '(:bold :italic :verbatim :code)))
    (ok (string= (wiki:mention-id (sixth objects)) "ABC123"))
    (let ((link (seventh objects)))
      (ok (string= (wiki:link-protocol link) "file"))
      (ok (string= (wiki:link-path link) "a.org"))
      (ok (string= (wiki:link-search link) "(defun f")))
    (ok (find " 2 * 3 foo_bar " (remove-if-not #'stringp inlines) :test #'string=))))

(deftest blocks-read-in-order
  (let* ((doc (page))
         (figure (first (wiki:document-figures doc)))
         (blocks (remove-if (lambda (x) (typep x 'wiki:heading))
                            (wiki:element-children figure))))
    (ok (equal (mapcar #'type-of blocks)
               '(wiki:paragraph wiki:plain-list wiki:plain-list
                 wiki:src-block wiki:example-block wiki:table)))
    (let ((bullets (second blocks)) (numbers (third blocks)))
      (ok (not (wiki:plain-list-ordered-p bullets)))
      (ok (wiki:plain-list-ordered-p numbers))
      (ok (= 3 (length (wiki:element-children bullets))))
      (ok (search "wraps a line"
                  (wiki::inline-text
                   (wiki:element-children
                    (first (wiki:element-children
                            (second (wiki:element-children bullets)))))))))
    (ok (string= (wiki:src-block-language (fourth blocks)) "lisp"))
    (ok (string= (wiki:block-text (fourth blocks))
                 (format nil "(defun f (x)~%  (* x 2))")))
    (ok (string= (wiki:block-text (fifth blocks)) "keep < this & that"))
    (ok (wiki:table-header-p (sixth blocks)))
    (ok (= 2 (length (wiki:table-rows (sixth blocks)))))))

(deftest site-indexes-figures-mentions-and-backlinks
  (let* ((doc (page))
         (site (wiki:make-site (list doc))))
    (ok (eq (wiki:find-figure "ABC123" site) (first (wiki:document-figures doc))))
    (ok (equal (wiki:document-mentions doc) '("ABC123" "DEF456" "ZZZ999")))
    (ok (equal (wiki:dangling-mentions site) (list (cons doc '("ZZZ999")))))
    ;; The work mark links to ABC123 with an id: link, so it is a backlink.
    (ok (equal (mapcar #'wiki:heading-id (gethash "ABC123" (wiki:site-backlinks site)))
               '("DEF456")))))

(deftest rendering-produces-expected-html
  (let* ((doc (page))
         (site (wiki:make-site (list doc)))
         (html (wiki:render-document-string doc site)))
    (ok (search "<title>A test page</title>" html))
    (ok (search "<section class=figure id=ABC123>" html))
    (ok (search "<section class=\"figure work-mark\" id=DEF456>" html))
    (ok (search "<span class=\"mark mark-next\">NEXT</span>" html))
    (ok (search "<b>bold</b>," html))
    (ok (search "<code class=verbatim>verbatim</code>" html))
    (ok (search "<a href=\"https://example.org\">link</a>" html))
    (ok (search "<a href=other.html>page link</a>" html))
    (ok (search "<a class=mention href=#DEF456 title=\"A work mark\">#DEF456</a>" html))
    (ok (search "<span class=\"mention dangling\"" html))
    (ok (search "keep &lt; this &amp; that" html))
    (ok (search "<div class=lisp><div class=list data-callee=defun>" html))
    (ok (search "<table><tr><th>Head A<th>Head B<tr><td>a1<td>" html))
    (ok (search "<ol><li>one<li>two</ol>" html))
    (ok (search "Mentioned in: <a href=#DEF456>A work mark</a>" html))
    (ok (search "mention <a class=mention href=#ABC123 title=\"First figure\">#ABC123</a> with" html))
    (ok (search "<h3><span class=\"mark mark-next\">" html))))

(deftest the-loaded-corpus-is-consistent
  ;; Depending on luv/wiki loads every page into its ORG-FILE component.
  (let* ((system (asdf:find-system :luv/wiki))
         (files (wiki::system-org-files system))
         (site (wiki::system-site system)))
    (ok (every #'wiki:org-file-document files))
    (ok (find "index" files :key #'asdf:component-name :test #'string=))
    (ok (null (wiki:dangling-mentions site)))
    (ok (> (hash-table-count (wiki:site-figures site)) 100))
    ;; RENDER-OP places pages in the site directory, untranslated.
    (let ((index (find "index" files :key #'asdf:component-name :test #'string=)))
      (multiple-value-bind (outputs untranslated-p)
          (asdf:output-files (asdf:make-operation 'wiki:render-op) index)
        (ok untranslated-p)
        (ok (equal (pathname-name (first outputs)) "index"))
        (ok (equal (pathname-type (first outputs)) "html"))))
    (let ((directory (uiop:ensure-directory-pathname
                      (uiop:merge-pathnames* "luv-wiki-test-site/" (uiop:temporary-directory)))))
      (unwind-protect
           (progn
             (wiki:write-site site directory)
             (ok (probe-file (merge-pathnames "index.html" directory)))
             (ok (probe-file (merge-pathnames "figures.html" directory))))
        (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore)))))

(defparameter *source*
  "(in-package #:luv.example)

;;; A comment before the definition mentions #ABC123.

(defun frob (x &optional y)
  \"Frob X.  See #ABC123 and the missing #ZZZ999.\"
  (let ((sum (+ x (or y 1)))
        (name 'frob))
    (list sum name #'car cl:car foo::bar :key #:g #(1 2) `(,x ,@y)
          #+sbcl :sbcl #-sbcl :other #\\a 1.5 \"s\")))

(defmethod frob-method :around ((x integer) y)
  (call-next-method))

(defclass widget () ((a :initarg :a)))
")

(deftest lisp-source-reads-to-nodes-without-interning
  (let* ((nodes (wiki:read-lisp-string *source*))
         (defun (find-if (lambda (n) (and (typep n 'wiki:lisp-list)
                                          (equal (wiki:lisp-symbol-name (first (wiki:element-children n))) "DEFUN")))
                         nodes)))
    (ok (typep (second nodes) 'wiki:lisp-comment))
    (ok defun)
    (ok (= 5 (length (wiki:element-children defun))))
    (let* ((let-form (fifth (wiki:element-children defun)))
           (list-form (third (wiki:element-children let-form)))
           (children (wiki:element-children list-form)))
      (ok (typep (second (wiki:element-children let-form)) 'wiki:lisp-list))
      (ok (typep (fourth children) 'wiki:lisp-prefix))
      (ok (string= (wiki::lisp-prefix-string (fourth children)) "#'"))
      (let ((cl-car (fifth children)) (foo-bar (sixth children)) (key (seventh children))
            (uninterned (eighth children)) (vector (ninth children)) (quasi (tenth children)))
        (ok (string= (wiki:lisp-symbol-package cl-car) "CL"))
        (ok (wiki::lisp-symbol-external-p cl-car))
        (ok (string= (wiki:lisp-symbol-package foo-bar) "FOO"))
        (ok (not (wiki::lisp-symbol-external-p foo-bar)))
        (ok (string= (wiki:lisp-symbol-package key) "KEYWORD"))
        (ok (eq (wiki:lisp-symbol-package uninterned) :uninterned))
        (ok (typep vector 'wiki:lisp-vector))
        (ok (string= (wiki::lisp-prefix-string quasi) "`")))
      (let ((conditionals (remove-if-not (lambda (n) (typep n 'wiki:lisp-conditional)) children)))
        (ok (= 2 (length conditionals)))
        ;; One branch is taken on SBCL and keeps its form; the other is
        ;; kept as text.  Neither is evaluated.
        (ok (= 1 (count-if #'wiki::lisp-conditional-form conditionals)))
        (ok (every (lambda (c) (search "sbcl" (wiki:node-text c))) conditionals)))
      (ok (typep (car (last children 3)) 'wiki:lisp-character))
      (ok (typep (car (last children 2)) 'wiki:lisp-number))
      (ok (typep (car (last children)) 'wiki:lisp-string)))
    ;; Nothing was interned.
    (ok (null (find-package "LUV.EXAMPLE")))
    (ok (null (find-symbol "FROB" "LUV.WIKI")))))

(deftest definitions-index-forms-and-mentions
  (let* ((definitions (wiki:file-definitions #p"/example.lisp" *source*))
         (frob (wiki:find-definition "frob" definitions))
         (method (wiki:find-definition "frob-method" definitions)))
    (ok (equal (mapcar #'wiki:definition-kind definitions) '("defun" "defmethod" "defclass")))
    (ok (= (wiki:definition-line frob) 5))
    (ok (equal (wiki:definition-mentions frob) '("ABC123" "ZZZ999")))
    (ok (= 1 (length (wiki::definition-comments frob))))
    (ok (equal (wiki::definition-package frob) "#:luv.example"))
    (ok (equal (wiki:definition-qualifiers method) '(":around")))
    (ok (wiki:find-definition "luv.example:frob" definitions))
    (ok (wiki:find-definition "widget" definitions :kind "defclass"))
    (ok (null (wiki:find-definition "widget" definitions :kind "defun")))))

(deftest dexp-renders-boxes-with-roles
  (let* ((html (let ((*print-pretty* nil) (spinneret:*suppress-inserted-spaces* t))
                 (spinneret:with-html-string
                   (wiki:render-lisp-source "(defun f (x) ;; c
  (let ((y 1)) (when x (list y :k \"s\"))))")))))
    (ok (search "<div class=lisp>" html))
    (ok (search "<div class=list data-callee=defun>" html))
    (ok (search "<span class=\"operator symbol\" data-symbol-name=DEFUN><span class=name>defun</span></span>" html))
    ;; The lambda list is head, the let form is body; the let binding
    ;; (y 1) is a clause whose first symbol is not an operator.
    (ok (search "<div class=\"body list\" data-callee=let>" html))
    (ok (search "<div class=\"bindings list\">" html))
    (ok (search "<div class=\"clause list\" data-callee=y><span class=symbol data-symbol-name=Y>" html))
    (ok (search "<span class=comment>;; c</span>" html))
    (ok (search "<span class=\"symbol keyword\" data-symbol-name=K><span class=package>:</span><span class=name>k</span></span>" html))
    (ok (search "<span class=string>&quot;s&quot;</span>" html))
    ;; Eclector recovers from unbalanced input, so boxes still appear;
    ;; input that reads to nothing at all falls back to a <pre>.
    (let ((recovered (let ((*print-pretty* nil))
                       (spinneret:with-html-string (wiki:render-lisp-source "(defun (")))))
      (ok (search "<div class=lisp>" recovered)))
    (let ((fallback (let ((*print-pretty* nil))
                      (spinneret:with-html-string (wiki:render-lisp-source "")))))
      (ok (search "<pre class=src" fallback)))))

(deftest code-references-render-inline
  (let* ((doc (page))
         (definitions (wiki:file-definitions #p"/example.lisp" *source*))
         (site (wiki:make-site (list doc) :definitions definitions :source-directory #p"/"))
         (html (wiki:render-document-string doc site)))
    (ok (equal (mapcar #'wiki:definition-name (gethash "ABC123" (wiki:site-code-references site)))
               '("frob")))
    (ok (search "Referenced from code:" html))
    (ok (search "<details class=definition><summary><span class=kind>defun</span> <span class=name>frob</span> <a class=source href=\"https://github.com/mbrock/luv/blob/main/example.lisp#L5\">example.lisp:5</a></summary>" html))
    ;; The mention inside the docstring links to the figure.
    (ok (search "See <a class=mention href=#ABC123" html))
    (ok (equal (wiki:dangling-code-mentions site) (list (cons (first definitions) '("ZZZ999")))))))
