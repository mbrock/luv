(defpackage #:luv.wiki.tests
  (:use #:cl)
  (:import-from #:parachute #:define-test #:true #:false #:fail #:group #:skip)
  (:local-nicknames (#:wiki #:luv.wiki)
                    (#:browser #:luv.wiki.browser)
                    (#:css #:luv.css)
                    (#:ps #:parenscript)))

(in-package #:luv.wiki.tests)
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

(define-test document-keywords-and-figures
  (let ((doc (page)))
    (true (string= (wiki:document-title doc) "A test page"))
    (true (equal (mapcar #'wiki:heading-id (wiki:document-figures doc))
                 '("XYZ123" "UVW456")))
    (let ((mark (second (wiki:document-figures doc))))
      (true (string= (wiki:heading-keyword mark) "NEXT"))
      (true (= (wiki:heading-level mark) 2))
      (true (string= (wiki::inline-text (wiki:heading-title mark)) "A work mark")))))

(define-test inline-markup-reads-to-objects
  (let* ((inlines (wiki::read-inlines
                   "x *bold* /it/ =verb= ~code~ [[https://e.org][L]] #XYZ123 2 * 3 foo_bar [[file:a.org::(defun f][=f=]]"))
         (objects (remove-if #'stringp inlines)))
    (true (equal (mapcar #'type-of objects)
                 '(wiki:emphasis wiki:emphasis wiki:emphasis wiki:emphasis
                   wiki:link wiki:mention wiki:link)))
    (true (equal (mapcar #'wiki:emphasis-kind (subseq objects 0 4))
                 '(:bold :italic :verbatim :code)))
    (true (string= (wiki:mention-id (sixth objects)) "XYZ123"))
    (let ((link (seventh objects)))
      (true (string= (wiki:link-protocol link) "file"))
      (true (string= (wiki:link-path link) "a.org"))
      (true (string= (wiki:link-search link) "(defun f")))
    (true (find " 2 * 3 foo_bar " (remove-if-not #'stringp inlines) :test #'string=))
    ;; Six hex digits are a colour, never a figure ID.
    (true (every #'stringp (wiki::read-inlines "paper is #111517 and ink #C2CBD0")))))

(define-test blocks-read-in-order
  (let* ((doc (page))
         (figure (first (wiki:document-figures doc)))
         (blocks (remove-if (lambda (x) (typep x 'wiki:heading))
                            (wiki:element-children figure))))
    (true (equal (mapcar #'type-of blocks)
                 '(wiki:paragraph wiki:plain-list wiki:plain-list
                   wiki:src-block wiki:example-block wiki:table)))
    (let ((bullets (second blocks)) (numbers (third blocks)))
      (true (not (wiki:plain-list-ordered-p bullets)))
      (true (wiki:plain-list-ordered-p numbers))
      (true (= 3 (length (wiki:element-children bullets))))
      (true (search "wraps a line"
                    (wiki::inline-text
                     (wiki:element-children
                      (first (wiki:element-children
                              (second (wiki:element-children bullets)))))))))
    (true (string= (wiki:src-block-language (fourth blocks)) "lisp"))
    (true (string= (wiki:block-text (fourth blocks))
                   (format nil "(defun f (x)~%  (* x 2))")))
    (true (string= (wiki:block-text (fifth blocks)) "keep < this & that"))
    (true (wiki:table-header-p (sixth blocks)))
    (true (= 2 (length (wiki:table-rows (sixth blocks)))))))

(define-test site-indexes-figures-mentions-and-backlinks
  (let* ((doc (page))
         (site (wiki:make-site (list doc))))
    (true (eq (wiki:find-figure "XYZ123" site) (first (wiki:document-figures doc))))
    (true (equal (wiki:document-mentions doc) '("XYZ123" "UVW456" "ZZZ999")))
    (true (equal (wiki:dangling-mentions site) (list (cons doc '("ZZZ999")))))
    ;; The work mark links to XYZ123 with an id: link, so it is a backlink.
    (true (equal (mapcar #'wiki:heading-id (gethash "XYZ123" (wiki:site-backlinks site)))
                 '("UVW456")))))

(define-test rendering-produces-expected-html
  (let* ((doc (page))
         (site (wiki:make-site (list doc)))
         (html (wiki:render-document-string doc site)))
    (true (search "<title>A test page</title>" html))
    (true (search "<section class=figure id=XYZ123>" html))
    (true (search "<section class=\"figure work-mark\" id=UVW456>" html))
    (true (search "<span class=\"mark mark-next\">NEXT</span>" html))
    (true (search "<b>bold</b>," html))
    (true (search "<code class=verbatim>verbatim</code>" html))
    (true (search "<a href=\"https://example.org\">link</a>" html))
    (true (search "<a href=other.html>page link</a>" html))
    (true (search "<a class=mention href=#UVW456 title=\"A work mark\">#UVW456</a>" html))
    (true (search "<span class=\"mention dangling\"" html))
    (true (search "keep &lt; this &amp; that" html))
    (true (search "<div class=lisp><div class=\"list stacked\" data-callee=defun>" html))
    (true (search "<table><tr><th>Head A<th>Head B<tr><td>a1<td>" html))
    (true (search "<ol><li>one<li>two</ol>" html))
    (true (search "Mentioned in: <a href=#UVW456>A work mark</a>" html))
    (true (search "mention <a class=mention href=#XYZ123 title=\"First figure\">#XYZ123</a> with" html))
    (true (search "<h3><span class=\"mark mark-next\">" html))))

(define-test capture-links-transclude-generated-images-and-videos
  (let* ((doc
           (page
            "#+title: Captures

* Media
:PROPERTIES:
:ID: CAP12G
:END:

[[capture:CAP12G-still.png]]

[[capture:CAP12G-orbit.mp4]]
"))
         (site (wiki:make-site (list doc)))
         (html (wiki:render-document-string doc site)))
    (true (search "<figure class=\"capture image\"><img src=\"media/CAP12G-still.png\""
                  html))
    (true (search "<figure class=\"capture video\"><video" html))
    (true (search "<source src=\"media/CAP12G-orbit.mp4\" type=\"video/mp4\">"
                  html))))

(define-test write-site-renders-a-small-site
  ;; Ordinary reader and renderer tests use a complete but deliberately small
  ;; site.  The real corpus belongs to `scripts/wiki dangling` and `make wiki`:
  ;; rendering every wiki and source page is not a useful tax on game changes.
  (let* ((document (page *page* "index"))
         (site (wiki:make-site (list document))))
    (let ((directory (uiop:ensure-directory-pathname
                      (uiop:merge-pathnames* "luv-wiki-test-site/" (uiop:temporary-directory)))))
      (unwind-protect
           (progn
             (wiki:write-site site directory)
             (true (probe-file (merge-pathnames "index.html" directory)))
             (true (probe-file (merge-pathnames "pages.html" directory)))
             (true (probe-file (merge-pathnames "work.html" directory)))
             (true (probe-file (merge-pathnames "style.css" directory)))
             (let ((work (uiop:read-file-string (merge-pathnames "work.html" directory))))
               (true (search "<nav class=work-summary aria-label=\"Work statuses\">" work))
               (true (search "<section class=work-status id=next>" work))
               (true (search "<ol class=work><li class=work-item>" work))))
        (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore)))))

(define-test dynamic-and-static-sites-use-the-same-resources
  (let* ((document (page *page* "index"))
         (extra (wiki:make-generated-resource
                 "/hello.txt" "nested/hello.txt" "text/plain; charset=utf-8"
                 (lambda () "hello from Lisp\n")))
         (site (wiki:make-site (list document) :resources (list extra)))
         (directory
           (uiop:ensure-directory-pathname
            (uiop:merge-pathnames* "luv-wiki-resource-test/"
                                   (uiop:temporary-directory)))))
    (unwind-protect
         (progn
           (wiki:publish-site site directory)
           (dolist (pair '(("/" . "index.html")
                           ("/hello.txt" . "nested/hello.txt")))
             (let* ((response
                      (wiki:resource-response
                       (wiki:find-resource (car pair) site)))
                    (dynamic-body (first (third response)))
                    (static-body
                      (uiop:read-file-string (merge-pathnames (cdr pair) directory))))
               (true (= 200 (first response)))
               (true (string= dynamic-body static-body)))))
      (uiop:delete-directory-tree directory :validate t
                                            :if-does-not-exist :ignore))))

(define-test dynamic-site-has-a-readiness-endpoint
  (let* ((site (wiki:make-site (list (page *page* "index"))))
         (application (wiki::wiki-clack-application site))
         (get-response
           (funcall application
                    '(:request-method :get :path-info "/healthz")))
         (head-response
           (funcall application
                    '(:request-method :head :path-info "/healthz"))))
    (true (= 200 (first get-response)))
    (true (equal '("ok\n") (third get-response)))
    (true (= 200 (first head-response)))
    (true (null (third head-response)))))

(define-test parenscript-async-and-await
  (let ((javascript
          (ps:ps*
           `(progn
              (browser:async-defun load-value ()
                (setf result (browser:await (fetch "/value"))))))))
    (true (search "async function loadValue()" javascript))
    (true (search "await (fetch('/value'))" javascript))))

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

(define-test lisp-source-reads-to-nodes-without-interning
  (let* ((nodes (wiki:read-lisp-string *source*))
         (defun (find-if (lambda (n) (and (typep n 'wiki:lisp-list)
                                          (equal (wiki:lisp-symbol-name (first (wiki:element-children n))) "DEFUN")))
                         nodes)))
    (true (typep (second nodes) 'wiki:lisp-comment))
    (true defun)
    (true (= 5 (length (wiki:element-children defun))))
    (let* ((let-form (fifth (wiki:element-children defun)))
           (list-form (third (wiki:element-children let-form)))
           (children (wiki:element-children list-form)))
      (true (typep (second (wiki:element-children let-form)) 'wiki:lisp-list))
      (true (typep (fourth children) 'wiki:lisp-prefix))
      (true (string= (wiki::lisp-prefix-string (fourth children)) "#'"))
      (let ((cl-car (fifth children)) (foo-bar (sixth children)) (key (seventh children))
            (uninterned (eighth children)) (vector (ninth children)) (quasi (tenth children)))
        (true (string= (wiki:lisp-symbol-package cl-car) "CL"))
        (true (wiki::lisp-symbol-external-p cl-car))
        (true (string= (wiki:lisp-symbol-package foo-bar) "FOO"))
        (true (not (wiki::lisp-symbol-external-p foo-bar)))
        (true (string= (wiki:lisp-symbol-package key) "KEYWORD"))
        (true (eq (wiki:lisp-symbol-package uninterned) :uninterned))
        (true (typep vector 'wiki:lisp-vector))
        (true (string= (wiki::lisp-prefix-string quasi) "`")))
      (let ((conditionals (remove-if-not (lambda (n) (typep n 'wiki:lisp-conditional)) children)))
        (true (= 2 (length conditionals)))
        ;; One branch is taken on SBCL and keeps its form; the other is
        ;; kept as text.  Neither is evaluated.
        (true (= 1 (count-if #'wiki::lisp-conditional-form conditionals)))
        (true (every (lambda (c) (search "sbcl" (wiki:node-text c))) conditionals)))
      (true (typep (car (last children 3)) 'wiki:lisp-character))
      (true (typep (car (last children 2)) 'wiki:lisp-number))
      (true (typep (car (last children)) 'wiki:lisp-string)))
    ;; Nothing was interned.
    (true (null (find-package "LUV.EXAMPLE")))
    (true (null (find-symbol "FROB" "LUV.WIKI")))))

(define-test definitions-index-forms-and-mentions
  (let* ((definitions (wiki:file-definitions #p"/example.lisp" *source*))
         (frob (wiki:find-definition "frob" definitions))
         (method (wiki:find-definition "frob-method" definitions)))
    (true (equal (mapcar #'wiki:definition-kind definitions) '("defun" "defmethod" "defclass")))
    (true (= (wiki:definition-line frob) 5))
    (true (equal (wiki:definition-mentions frob) '("XYZ123" "ZZZ999")))
    (true (= 1 (length (wiki::definition-comments frob))))
    (true (equal (wiki::definition-package frob) "#:luv.example"))
    (true (equal (wiki:definition-qualifiers method) '(":around")))
    (true (wiki:find-definition "luv.example:frob" definitions))
    (true (wiki:find-definition "widget" definitions :kind "defclass"))
    (true (null (wiki:find-definition "widget" definitions :kind "defun")))))

(define-test definitions-index-concise-zoned-definitions
  (let* ((source
           "(in-package #:luv.example)
(zdefun plain (x) x)
(zdefun (semantic :zone :example/semantic :value (length xs)) (xs) xs)
(zdefun ((setf property) :zone :example/property) (value object) value)
(zdefmethod (operate :zone :example/operate) :around ((x integer) y)
  (call-next-method))")
         (definitions (wiki:file-definitions #p"/zoned.lisp" source))
         (method (fourth definitions)))
    (true (equal (mapcar #'wiki:definition-kind definitions)
                 '("zdefun" "zdefun" "zdefun" "zdefmethod")))
    (true (equal (mapcar #'wiki:definition-name definitions)
                 '("plain" "semantic" "(setf property)" "operate")))
    (true (equal (wiki:definition-qualifiers method) '(":around")))
    (true (equal (wiki::definition-specializers method) '("integer" "t")))))

(define-test the-stylesheet-compiles-from-definitions
  ;; The CSS syntax reads quantities, slash pairs, and --references as
  ;; objects, and everything else as usual.
  (let ((*readtable* (named-readtables:find-readtable 'css:syntax)))
    (let ((quantity (read-from-string "0.85rem")))
      (true (typep quantity 'css:dimension))
      (true (= (css:dimension-number quantity) 0.85))
      (true (string= (css:dimension-unit quantity) "rem")))
    (true (typep (read-from-string "-0.02em") 'css:dimension))
    (true (typep (read-from-string "0.72rem/1") 'css:slash))
    (true (string= (css:variable-reference-name (read-from-string "--ink")) "ink"))
    (true (eql (read-from-string "17") 17))
    (true (eql (read-from-string "1.5") 1.5))
    (true (eq (read-from-string "-") '-))
    (true (eq (read-from-string "1+") '1+)))
  ;; Values: symbols are CSS words, quantities and references themselves,
  ;; lists Lisp: CSS functions build calls, and any function may return
  ;; values or declarations for the rule.
  (flet ((gutter () (css:clamp 1rem 4vw 3rem))
         (hairline () (list 1px 'solid --rule)))
    (true (string= (css:css-text
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
    (true (string= (css:css-text
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
    (true (typep (first (css:container-children rule)) 'css:declaration))
    (true (typep (second (css:container-children rule)) 'css:rule))
    (true (string= (css:selector-text (css:rule-selector rule)) ".a")))
  ;; The whole sheet has the palette first and the layout roles the dexp
  ;; renderer emits.
  (let ((text (css:stylesheet-text)))
    (true (search ":root {" text))
    (true (< (search ":root {" text) (search ".lisp .list.bindings" text)))))

(define-test dexp-renders-boxes-with-roles
  (let* ((html (let ((*print-pretty* nil) (spinneret:*suppress-inserted-spaces* t))
                 (spinneret:with-html-string
                   (wiki:render-lisp-source "(defun f (x) ;; c
  (let ((y 1)) (when x (list y :k \"s\"))))")))))
    (true (search "<div class=lisp>" html))
    ;; A list with body forms is stacked: its head in a .head span, then rows.
    (true (search "<div class=\"list stacked\" data-callee=defun><span class=head><span class=\"operator symbol\" data-symbol-name=DEFUN><span class=name>defun</span></span>" html))
    ;; The lambda list is head, the let form is body; the let binding
    ;; (y 1) is a clause whose first symbol is not an operator.
    (true (search "<div class=\"body list stacked\" data-callee=let>" html))
    (true (search "<div class=\"bindings list\">" html))
    (true (search "<div class=\"clause list\"><span class=symbol data-symbol-name=Y><span class=name>y</span></span><span class=rest><span class=number>1</span></span></div>" html))
    (true (search "<div class=\"comment prose\"><p>c</div>" html))
    (true (search "<span class=\"symbol keyword\" data-symbol-name=K><span class=package>:</span><span class=name>k</span></span>" html))
    (true (search "<span class=string>&quot;s&quot;</span>" html))
    ;; Clause forms are tables: each clause a row with its key in a .head
    ;; cell and its body in a .rest cell; a handler-case clause's key is the
    ;; type and its lambda list.
    (let ((clauses (let ((*print-pretty* nil) (spinneret:*suppress-inserted-spaces* t))
                     (spinneret:with-html-string
                       (wiki:render-lisp-source
                        "(cond ((null x) 1) (t (f) (g))) (handler-case (f) (error (c) c))")))))
      (true (search "<div class=\"list clauses stacked\" data-callee=cond><span class=head><span class=\"operator symbol\" data-symbol-name=COND>" clauses))
      (true (search "<div class=\"body stacked-clause list\"><span class=head><div class=list data-callee=null>" clauses))
      (true (search "</div></span><span class=rest><span class=\"body number\">1</span></span></div>" clauses))
      (true (search "<span class=rest><div class=\"body list\" data-callee=f>" clauses))
      (true (search "<div class=\"body stacked-clause handler-clause list\"><span class=head><span class=symbol data-symbol-name=ERROR><span class=name>error</span></span><div class=\"lambda-list list\">" clauses)))
    ;; Eclector recovers from unbalanced input, so boxes still appear;
    ;; input that reads to nothing at all falls back to a <pre>.
    (let ((recovered (let ((*print-pretty* nil))
                       (spinneret:with-html-string (wiki:render-lisp-source "(defun (")))))
      (true (search "<div class=lisp>" recovered)))
    (let ((fallback (let ((*print-pretty* nil))
                      (spinneret:with-html-string (wiki:render-lisp-source "")))))
      (true (search "<pre class=src" fallback)))))

(define-test code-references-render-inline
  (let* ((doc (page))
         (definitions (wiki:file-definitions #p"/example.lisp" *source*))
         (site (wiki:make-site (list doc) :definitions definitions :source-directory #p"/"))
         (html (wiki:render-document-string doc site)))
    (true (equal (mapcar #'wiki:definition-name (gethash "XYZ123" (wiki:site-code-references site)))
                 '("frob")))
    (true (search "Referenced from code:" html))
    (true (search "<details class=definition><summary><span class=kind>defun</span> <span class=name>frob</span> <a class=source href=\"https://github.com/mbrock/luv/blob/main/example.lisp#L5\">example.lisp:5</a></summary>" html))
    ;; The mention inside the docstring links to the figure.
    (true (search "See <a class=mention href=#XYZ123" html))
    (true (equal (wiki:dangling-code-mentions site) (list (cons (first definitions) '("ZZZ999")))))))

(define-test code-prose-marks-symbol-references
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
    (true (search "Return <code class=symbol>count</code> items up to <code class=symbol>limit</code>" html))
    (true (search "calling <code class=symbol>frob</code>" html))
    ;; Unknown acronyms stay prose; *name* reads as code, not bold.
    (true (search "GPU acronyms" html))
    (true (search "<code>*features*</code>" html))
    (true (search "<code>*special*</code>" html))
    (true (not (search "<b>" html)))))

(define-test the-cli-prints-a-small-site
  (let* ((luv.wiki.cli::*site* (wiki:make-site (list (page))))
         (toc (with-output-to-string (*standard-output*)
                (luv.wiki.cli:run '("toc" "test"))))
         (marks (with-output-to-string (*standard-output*)
                  (luv.wiki.cli:run '("marks" "next"))))
         (figure (with-output-to-string (*standard-output*)
                   (luv.wiki.cli:run '("figure" "XYZ123")))))
    (true (search "A test page  (test.org)" toc))
    (true (search "  XYZ123  First figure" toc))
    (true (search "NEXT" marks))
    (true (search "First figure" figure))
    (true (search "Mentioned in:" figure))))

(define-test platform-gated-systems-remain-in-the-source-corpus
  ;; ASDF's :IF-FEATURE keeps these systems registered for introspection on
  ;; every host while making their build actions invalid away from Darwin.
  ;; The wiki deliberately walks the complete component tree rather than only
  ;; ASDF's active SUB-COMPONENTS, so Linux CI can render the Metal sources.
  (let* ((system (asdf:find-system :luv))
         (metal-module (asdf:find-component system '("hal" "metal")))
         (root (asdf:system-source-directory :luv))
         (metal-source (merge-pathnames "hal/metal/gpu.lisp" root)))
    (true (equal :darwin (asdf/component:component-if-feature metal-module)))
    (true (find metal-source (wiki:code-source-files) :test #'equal))))
