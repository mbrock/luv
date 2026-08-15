(defpackage #:luv-wiki/tests
  (:use #:cl #:rove)
  (:local-nicknames (#:wiki #:luv.wiki)
                    (#:css #:luv.css)))

(in-package #:luv-wiki/tests)
(named-readtables:in-readtable luv.css:syntax)

(defparameter *page*
  "#+title: A test page
#+startup: overview

Preamble mentions #XYZ123 before any heading.

* First figure
:PROPERTIES:
:ID: XYZ123
:END:

A paragraph with *bold*, /italic/, =verbatim=, ~code~, a [[https://example.org][link]],
and a plain [[file:other.org][page link]].  A mention of #UVW456 and a dangling
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
:ID: UVW456
:END:

Intent: mention [[id:XYZ123]] with a link.

* Second figure without an ID

Nothing here refers to anything.
")

(defun page (&optional (text *page*) (name "test"))
  (wiki:read-org-string text :name name))

(deftest document-keywords-and-figures
  (let ((doc (page)))
    (ok (string= (wiki:document-title doc) "A test page"))
    (ok (equal (mapcar #'wiki:heading-id (wiki:document-figures doc))
               '("XYZ123" "UVW456")))
    (let ((mark (second (wiki:document-figures doc))))
      (ok (string= (wiki:heading-keyword mark) "NEXT"))
      (ok (= (wiki:heading-level mark) 2))
      (ok (string= (wiki::inline-text (wiki:heading-title mark)) "A work mark")))))

(deftest inline-markup-reads-to-objects
  (let* ((inlines (wiki::read-inlines
                   "x *bold* /it/ =verb= ~code~ [[https://e.org][L]] #XYZ123 2 * 3 foo_bar [[file:a.org::(defun f][=f=]]"))
         (objects (remove-if #'stringp inlines)))
    (ok (equal (mapcar #'type-of objects)
               '(wiki:emphasis wiki:emphasis wiki:emphasis wiki:emphasis
                 wiki:link wiki:mention wiki:link)))
    (ok (equal (mapcar #'wiki:emphasis-kind (subseq objects 0 4))
               '(:bold :italic :verbatim :code)))
    (ok (string= (wiki:mention-id (sixth objects)) "XYZ123"))
    (let ((link (seventh objects)))
      (ok (string= (wiki:link-protocol link) "file"))
      (ok (string= (wiki:link-path link) "a.org"))
      (ok (string= (wiki:link-search link) "(defun f")))
    (ok (find " 2 * 3 foo_bar " (remove-if-not #'stringp inlines) :test #'string=))
    ;; Six hex digits are a colour, never a figure ID.
    (ok (every #'stringp (wiki::read-inlines "paper is #111517 and ink #C2CBD0")))))

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
    (ok (eq (wiki:find-figure "XYZ123" site) (first (wiki:document-figures doc))))
    (ok (equal (wiki:document-mentions doc) '("XYZ123" "UVW456" "ZZZ999")))
    (ok (equal (wiki:dangling-mentions site) (list (cons doc '("ZZZ999")))))
    ;; The work mark links to XYZ123 with an id: link, so it is a backlink.
    (ok (equal (mapcar #'wiki:heading-id (gethash "XYZ123" (wiki:site-backlinks site)))
               '("UVW456")))))

(deftest rendering-produces-expected-html
  (let* ((doc (page))
         (site (wiki:make-site (list doc)))
         (html (wiki:render-document-string doc site)))
    (ok (search "<title>A test page</title>" html))
    (ok (search "<section class=figure id=XYZ123>" html))
    (ok (search "<section class=\"figure work-mark\" id=UVW456>" html))
    (ok (search "<span class=\"mark mark-next\">NEXT</span>" html))
    (ok (search "<b>bold</b>," html))
    (ok (search "<code class=verbatim>verbatim</code>" html))
    (ok (search "<a href=\"https://example.org\">link</a>" html))
    (ok (search "<a href=other.html>page link</a>" html))
    (ok (search "<a class=mention href=#UVW456 title=\"A work mark\">#UVW456</a>" html))
    (ok (search "<span class=\"mention dangling\"" html))
    (ok (search "keep &lt; this &amp; that" html))
    (ok (search "<div class=lisp><div class=\"list stacked\" data-callee=defun>" html))
    (ok (search "<table><tr><th>Head A<th>Head B<tr><td>a1<td>" html))
    (ok (search "<ol><li>one<li>two</ol>" html))
    (ok (search "Mentioned in: <a href=#UVW456>A work mark</a>" html))
    (ok (search "mention <a class=mention href=#XYZ123 title=\"First figure\">#XYZ123</a> with" html))
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
             (ok (probe-file (merge-pathnames "pages.html" directory)))
             (ok (probe-file (merge-pathnames "work.html" directory)))
             (ok (probe-file (merge-pathnames "style.css" directory))))
        (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore)))))

(defparameter *source*
  "(in-package #:luv.example)

;;; A comment before the definition mentions #XYZ123.

(defun frob (x &optional y)
  \"Frob X.  See #XYZ123 and the missing #ZZZ999.\"
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
    (ok (equal (wiki:definition-mentions frob) '("XYZ123" "ZZZ999")))
    (ok (= 1 (length (wiki::definition-comments frob))))
    (ok (equal (wiki::definition-package frob) "#:luv.example"))
    (ok (equal (wiki:definition-qualifiers method) '(":around")))
    (ok (wiki:find-definition "luv.example:frob" definitions))
    (ok (wiki:find-definition "widget" definitions :kind "defclass"))
    (ok (null (wiki:find-definition "widget" definitions :kind "defun")))))

(deftest the-stylesheet-compiles-from-definitions
  ;; The CSS syntax reads quantities, slash pairs, and --references as
  ;; objects, and everything else as usual.
  (let ((*readtable* (named-readtables:find-readtable 'css:syntax)))
    (let ((quantity (read-from-string "0.85rem")))
      (ok (typep quantity 'css:dimension))
      (ok (= (css:dimension-number quantity) 0.85))
      (ok (string= (css:dimension-unit quantity) "rem")))
    (ok (typep (read-from-string "-0.02em") 'css:dimension))
    (ok (typep (read-from-string "0.72rem/1") 'css:slash))
    (ok (string= (css:variable-reference-name (read-from-string "--ink")) "ink"))
    (ok (eql (read-from-string "17") 17))
    (ok (eql (read-from-string "1.5") 1.5))
    (ok (eq (read-from-string "-") '-))
    (ok (eq (read-from-string "1+") '1+)))
  ;; Values: symbols are CSS words, quantities and references themselves,
  ;; lists Lisp: CSS functions build calls, and any function may return
  ;; values or declarations for the rule.
  (flet ((gutter () (css:clamp 1rem 4vw 3rem))
         (hairline () (list 1px 'solid --rule)))
    (ok (string= (css:css-text
                  (css:rule ".a" :color --ink :margin 0 auto
                    :padding 0.3rem (gutter)
                    :border (hairline)
                    :grid-template-columns (css:repeat 3 (css:minmax 0 1fr))
                    :font 700 0.72rem/1 --display-font
                    :background (css:color-mix --paper 94% --accent)
                    :font-family (css:font-stack "Public Sans" :helvetica)))
                 ".a {
  color: var(--ink);
  margin: 0 auto;
  padding: 0.3rem clamp(1rem, 4vw, 3rem);
  border: 1px solid var(--rule);
  grid-template-columns: repeat(3, minmax(0, 1fr));
  font: 700 0.72rem/1 var(--display-font);
  background: color-mix(in srgb, var(--paper) 94%, var(--accent));
  font-family: \"Public Sans\", helvetica;
}
")))
  ;; Nesting: descendants, & for the parent, groups crossed with groups,
  ;; media queries hoisted around the rule they wrap, mixins spliced in.
  (flet ((row () (css:declarations :display flex :gap 1rem)))
    (ok (string= (css:css-text
                  (css:rule (".door" ".card") (row) :outline none
                    (("&:hover" "&.selected") :outline none)
                    ("h1" :margin 0)
                    (:media "(max-width: 90ch)" :display block)))
                 ".door, .card {
  display: flex;
  gap: 1rem;
  outline: none;
}

.door:hover, .door.selected, .card:hover, .card.selected {
  outline: none;
}

.door h1, .card h1 {
  margin: 0;
}

@media (max-width: 90ch) {
  .door, .card {
    display: block;
  }
}
")))
  ;; The tree is inspectable: a rule's children are declarations and rules.
  (let ((rule (css:rule ".a" :color --ink ("b" :margin 0))))
    (ok (typep (first (css:container-children rule)) 'css:declaration))
    (ok (typep (second (css:container-children rule)) 'css:rule))
    (ok (string= (css:selector-text (css:rule-selector rule)) ".a")))
  ;; The whole sheet has the palette first and the layout roles the dexp
  ;; renderer emits.
  (let ((text (css:stylesheet-text)))
    (ok (search ":root {" text))
    (ok (< (search ":root {" text) (search ".lisp .list.bindings" text)))))

(deftest dexp-renders-boxes-with-roles
  (let* ((html (let ((*print-pretty* nil) (spinneret:*suppress-inserted-spaces* t))
                 (spinneret:with-html-string
                   (wiki:render-lisp-source "(defun f (x) ;; c
  (let ((y 1)) (when x (list y :k \"s\"))))")))))
    (ok (search "<div class=lisp>" html))
    ;; A list with body forms is stacked: its head in a .head span, then rows.
    (ok (search "<div class=\"list stacked\" data-callee=defun><span class=head><span class=\"operator symbol\" data-symbol-name=DEFUN><span class=name>defun</span></span>" html))
    ;; The lambda list is head, the let form is body; the let binding
    ;; (y 1) is a clause whose first symbol is not an operator.
    (ok (search "<div class=\"body list stacked\" data-callee=let>" html))
    (ok (search "<div class=\"bindings list\">" html))
    (ok (search "<div class=\"clause list\"><span class=symbol data-symbol-name=Y><span class=name>y</span></span><span class=rest><span class=number>1</span></span></div>" html))
    (ok (search "<div class=\"comment prose\"><p>c</div>" html))
    (ok (search "<span class=\"symbol keyword\" data-symbol-name=K><span class=package>:</span><span class=name>k</span></span>" html))
    (ok (search "<span class=string>&quot;s&quot;</span>" html))
    ;; Clause forms are tables: each clause a row with its key in a .head
    ;; cell and its body in a .rest cell; a handler-case clause's key is the
    ;; type and its lambda list.
    (let ((clauses (let ((*print-pretty* nil) (spinneret:*suppress-inserted-spaces* t))
                     (spinneret:with-html-string
                       (wiki:render-lisp-source
                        "(cond ((null x) 1) (t (f) (g))) (handler-case (f) (error (c) c))")))))
      (ok (search "<div class=\"list clauses stacked\" data-callee=cond><span class=head><span class=\"operator symbol\" data-symbol-name=COND>" clauses))
      (ok (search "<div class=\"body stacked-clause list\"><span class=head><div class=list data-callee=null>" clauses))
      (ok (search "</div></span><span class=rest><span class=\"body number\">1</span></span></div>" clauses))
      (ok (search "<span class=rest><div class=\"body list\" data-callee=f>" clauses))
      (ok (search "<div class=\"body stacked-clause handler-clause list\"><span class=head><span class=symbol data-symbol-name=ERROR><span class=name>error</span></span><div class=\"lambda-list list\">" clauses)))
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
    (ok (equal (mapcar #'wiki:definition-name (gethash "XYZ123" (wiki:site-code-references site)))
               '("frob")))
    (ok (search "Referenced from code:" html))
    (ok (search "<details class=definition><summary><span class=kind>defun</span> <span class=name>frob</span> <a class=source href=\"https://github.com/mbrock/luv/blob/main/example.lisp#L5\">example.lisp:5</a></summary>" html))
    ;; The mention inside the docstring links to the figure.
    (ok (search "See <a class=mention href=#XYZ123" html))
    (ok (equal (wiki:dangling-code-mentions site) (list (cons (first definitions) '("ZZZ999")))))))

(deftest code-prose-marks-symbol-references
  (let* ((definitions (wiki:file-definitions #p"/example.lisp" *source*))
         (site (wiki:make-site (list (page)) :definitions definitions :source-directory #p"/"))
         (html (let ((wiki:*site* site) (*print-pretty* nil) (spinneret:*suppress-inserted-spaces* t))
                 (spinneret:with-html-string
                   (wiki:render-lisp-source
                    "(defun g (count &key (limit 3))
  \"Return COUNT items up to LIMIT, calling FROB on each; GPU acronyms
and *features* stay text; *special* names are code.\"
  count)")))))
    ;; Parameters and definitions become symbol references, lowercase.
    (ok (search "Return <code class=symbol>count</code> items up to <code class=symbol>limit</code>" html))
    (ok (search "calling <code class=symbol>frob</code>" html))
    ;; Unknown acronyms stay prose; *name* reads as code, not bold.
    (ok (search "GPU acronyms" html))
    (ok (search "<code>*features*</code>" html))
    (ok (search "<code>*special*</code>" html))
    (ok (not (search "<b>" html)))))

(deftest the-cli-prints-the-corpus
  (let* ((root (asdf:system-source-directory :luv/wiki))
         (luv.wiki.cli::*root* root)
         (luv.wiki.cli::*site* nil)
         (toc (with-output-to-string (*standard-output*) (luv.wiki.cli:run '("toc" "index"))))
         (marks (with-output-to-string (*standard-output*) (luv.wiki.cli:run '("marks" "done"))))
         (figure (with-output-to-string (*standard-output*) (luv.wiki.cli:run '("figure" "F2N8VX")))))
    (ok (search "The luv workshop wiki  (index.org)" toc))
    (ok (search "  F2N8VX  String figures" toc))
    (ok (search "DONE" marks))
    (ok (search "String figures" figure))
    (ok (search "Mentioned in:" figure))))

(deftest platform-gated-systems-remain-in-the-source-corpus
  ;; ASDF's :IF-FEATURE keeps these systems registered for introspection on
  ;; every host while making their build actions invalid away from Darwin.
  ;; The wiki deliberately walks the complete component tree rather than only
  ;; ASDF's active SUB-COMPONENTS, so Linux CI can render the Metal sources.
  (let* ((system (asdf:find-system :luv/gpu/metal))
         (root (asdf:system-source-directory :luv/wiki))
         (metal-source (merge-pathnames "gpu/metal.lisp" root)))
    (ok (equal :darwin (asdf/component:component-if-feature system)))
    (ok (find metal-source (wiki:code-source-files) :test #'equal))))
