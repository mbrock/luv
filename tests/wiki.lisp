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
    (ok (search "<pre class=src data-language=lisp><code>(defun f (x)" html))
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
