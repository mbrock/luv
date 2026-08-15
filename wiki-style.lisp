;;;; The site's stylesheet, as style definitions compiled by wiki-css.lisp.
;;;;
;;;; Groups follow the page from the outside in: palette, frame, type, the
;;;; wiki's own elements, then the dexp code boxes and their layout roles,
;;;; whose selectors are the roles wiki-dexp.lisp assigns.  Under the CSS
;;;; syntax 0.85rem is a quantity and --ink a reference; bare symbols are CSS
;;;; words and lists are Lisp, so the palette, the mark colours, and the
;;;; definition-kind colours below are computed from tables, and the layout
;;;; rows every box shares are functions.

(in-package #:luv.wiki.style)
(named-readtables:in-readtable luv.css:syntax)

;;; Palette and shared values

(defparameter *palette*
  '((paper white "#111517")
    (ink black "#c2cbd0")
    (muted "#667078" "#8b969d")
    (rule "#ded8ce" "#2f383d")
    (status-bg black "#d7d2c4")
    (status-ink ivory "#111719")
    (accent "#9a3248" "#de6e7c")
    (code-bg "#f4f1ea" "#171c1f")
    (mark-next --accent --accent)
    (mark-todo --muted --muted)
    (mark-wait "#8a7a1c" "#d9c65a")
    (mark-done "#3d7a3d" "#7ac47a")
    (mark-idea "#5a4d8a" "#a99ad9"))
  "Each custom colour property with its light and dark values.")

(defun palette (scheme)
  "The palette's declarations for SCHEME, :light or :dark."
  (loop for (name light dark) in *palette*
        collect (custom-property name (ecase scheme (:light light) (:dark dark)))))

(defun gutter ()
  "The horizontal page gutter."
  (clamp 1rem 4vw 3rem))

(defun hairline ()
  "The one-pixel rule between things."
  (list 1px 'solid --rule))

(defun display-font (weight size/line-height)
  "The font shorthand for the display face."
  (list weight size/line-height --display-font))

;;; Layout rows shared by the dexp boxes

(defun flowing-row ()
  "A row whose items flow and wrap, aligned on their baselines."
  (declarations :display flex :flex-wrap wrap :align-items baseline
                :column-gap 0.5em :min-width 0))

(defun stacked-rows ()
  "A grid of rows, each as wide as its content."
  (declarations :display grid :grid-template-columns (minmax 0 :max-content)))

(defun subgrid-row ()
  "A row of a two-column table, its cells aligned with the table's columns."
  (declarations :display grid :grid-template-columns subgrid :grid-column span 2
                :align-items baseline))

;;; The sheet

(define-style palette
  "Colours and faces, light and dark."
  (":root"
   :color-scheme light dark
   (palette :light)
   :--display-font (font-stack "Public Sans" :helvetica :arial :sans-serif)
   :--body-font --display-font
   :--mono-font (font-stack :ui-monospace "SF Mono" :menlo :consolas :monospace))
  (:media "(prefers-color-scheme: dark)"
   (":root" (palette :dark))))

(define-style page
  "The document: box model, root type, links."
  (("*" "*::before" "*::after")
   :box-sizing border-box)
  ("html"
   :color --ink
   :background --paper
   :font-family --body-font
   :font-size 17px
   :line-height 1.45
   :text-rendering "optimizeLegibility"
   :-webkit-text-size-adjust 100%)
  ("body"
   :margin 0
   :min-height 100vh)
  ("a"
   :color --accent
   :text-decoration none
   ("&:hover" :text-decoration underline)))

(define-style library
  "The library band: eyebrow, site name, and the three doors."
  (".library"
   :display grid
   :grid-template-columns (minmax 10rem 0.5fr) (minmax 0 1.5fr)
   :gap 1rem
   :align-items center
   :padding 0.85rem (gutter)
   :border-bottom (hairline)
   :background (color-mix --paper 94% --accent))
  (".eyebrow"
   :margin 0 0 0.1rem
   :color --muted
   :font (display-font 700 0.72rem/1)
   :text-transform uppercase
   :letter-spacing 0.06em)
  (".library h1"
   :margin 0
   :font (display-font 700 1.25rem/1.1)
   :letter-spacing -0.02em
   ("a" :color inherit))
  (".doors"
   :display grid
   :grid-template-columns (repeat 3 (minmax 0 1fr))
   :gap 0.65rem)
  (".door"
   :display grid
   :gap 0.1rem
   :min-width 0
   :padding 0.65rem 0.75rem
   :color inherit
   :text-decoration none
   :border (hairline)
   :border-radius 8px
   :background --paper
   (("&:hover" "&:focus-visible")
    :border-color (color-mix --accent 70% --rule)
    :text-decoration none
    :outline none)
   ("&.selected"
    :border-color --accent
    :box-shadow inset 0 0 0 1px --accent))
  (".door-title"
   :font (display-font 700 0.95rem/1.2))
  (".door-meta"
   :color --muted
   :font (display-font 500 0.82rem/1.2)
   :min-width 0
   :overflow-wrap break-word))

(define-style status-bar
  "The status bar, as a Glk grid window: black, monospace, bold."
  (".status"
   :display flex
   :justify-content space-between
   :gap 1rem
   :padding 0.3rem (gutter)
   :font 700 0.9rem/1.5 --mono-font
   :color --status-ink
   :background --status-bg
   :position sticky
   :top 0
   :z-index 1
   ("a" :color inherit))
  ((".status-left" ".status-right")
   :min-width 0
   :overflow hidden
   :text-overflow ellipsis
   :white-space nowrap))

(define-style main-column
  "The main column; wide pages fill the viewport but their prose keeps a
readable measure."
  ("main"
   :max-width 66ch
   :padding (clamp 1.5rem 4vw 3.25rem) (gutter) 2.75rem
   :margin 0 auto)
  ("body.wide main"
   :max-width none
   :margin 0)
  (("body.wide main > p" "body.wide main > h1")
   :max-width 66ch)
  (".site-footer"
   :max-width 66ch
   :margin 0 auto
   :padding 1rem (gutter) 3rem
   :border-top (hairline)
   :font (display-font 500 0.82rem/1.4)
   :color --muted))

(define-style type
  "Headings, paragraphs, figures and their IDs, mentions, marks."
  (("h1" "h2" "h3" "h4")
   :font-family --display-font
   :line-height 1.16
   :font-weight 800)
  ("h1"
   :font-size 2.2rem
   :letter-spacing -0.06em
   :margin 0.5rem 0 1.5rem)
  ("h2"
   :font-size 1.3rem
   :letter-spacing -0.04em
   :margin 2.6rem 0 0.75rem)
  ("h3"
   :font-size 1.1rem
   :letter-spacing -0.02em
   :margin 2rem 0 0.5rem
   :font-weight 700)
  ("p"
   :margin 0 0 1.1rem
   :hyphens manual
   :text-align justify
   :text-align-last start)
  ("section.figure"
   :scroll-margin-top 1rem)
  (("section:target > h2" "section:target > h3" "section:target > h4")
   :background --code-bg
   :outline 0.4rem solid --code-bg)
  ("a.figure-id"
   :margin-left 0.6rem
   :font-family --mono-font
   :font-size 0.7em
   :color --muted
   :opacity 0
   :transition opacity 0.15s)
  (("h2:hover a.figure-id" "h3:hover a.figure-id" "h4:hover a.figure-id"
    "section:target > h2 a.figure-id" "section:target > h3 a.figure-id")
   :opacity 1)
  (".mention"
   :font-family --mono-font
   :font-weight 700
   :font-size 0.82em
   ("&.dangling"
    :color --muted
    :text-decoration line-through))
  (".unresolved-link"
   :border-bottom 1px dotted --muted)
  (".mark"
   :font-family --mono-font
   :font-size 0.6em
   :font-weight 700
   :letter-spacing 0.08em
   :padding 0.15em 0.45em
   :border-radius 0.2em
   :vertical-align middle
   :color --paper
   :background --mark-todo)
  ;; Each other status colours its own mark.
  (loop for status in '(next wait done idea)
        collect (rule (format nil ".mark-~(~A~)" status)
                  :background (var (format nil "mark-~(~A~)" status))))
  ("p.backlinks"
   :font-size 0.85rem
   :color --muted
   :margin-top 1rem))

(define-style text-blocks
  "Code, tables, lists, images."
  (("code" "pre")
   :font-family --mono-font
   :font-size 0.85em)
  ("code"
   :background none
   :padding 0
   :color (color-mix --ink 80% --accent))
  ("pre"
   :background --code-bg
   :padding 0.8rem 1rem
   :overflow-x auto
   :line-height 1.4
   ("code" :padding 0 :background none))
  ("table"
   :border-collapse collapse
   :margin 1rem 0
   :font-size 0.92em)
  (("th" "td")
   :text-align left
   :vertical-align top
   :padding 0.3rem 0.8rem 0.3rem 0
   :border-bottom (hairline))
  ("th"
   :font-weight normal
   :color --muted)
  (("ul" "ol")
   :padding-left 1.4rem)
  ("li"
   :margin 0.25rem 0)
  ("ul.figure-list"
   :list-style none
   :padding-left 0
   ("li.level-2" :padding-left 1.5rem))
  ("figure.image"
   :margin 1.5rem 0)
  (("figure.image img" "img.inline")
   :max-width 100%
   :height auto
   :border-radius 0.4rem))

(define-style dexp-boxes
  "Lisp source as dexp boxes: every list is a box whose side borders are
its parentheses; a list without body forms flows and wraps, one with body
forms is a grid of rows (see the stacked group)."
  (".lisp"
   :--paren (color-mix --ink 30% :transparent)
   :font-family --display-font
   :font-size 0.9rem
   :line-height 1.55
   :margin 1rem 0
   :padding 0.6rem 0.8rem
   :background --code-bg
   :border-radius 0.4rem
   :overflow-x auto
   ("& > *"
    :display block
    :margin 0.4rem 0)
   ("& > .comment"
    :display block)
   (".list"
    (flowing-row)
    :row-gap 0.05em
    :padding 0 0.4em
    :border 0 solid --paren
    :border-width 0 1.5px
    :border-radius 0.55em
    ("& > .list" :margin 0.05em 0))
   (".body"
    :flex-basis 100%)
   ((".operator" ".list > .list.body > .operator" ".list > .list > .operator")
    :color --accent)
   (".symbol"
    ;; A name broken at a hyphen reads as two names; the box scrolls instead.
    :white-space nowrap)
   (".package"
    :opacity 0.55)
   (".keyword"
    :color --mark-idea)
   (".string"
    :color (color-mix --ink 60% --mark-done)
    :white-space pre-wrap
    :overflow-wrap anywhere)
   ((".number" ".character")
    :color --mark-wait)
   (".comment"
    :flex-basis 100%
    :color --muted
    :font-style italic
    :white-space pre-wrap)
   ((".prefixed" ".conditional")
    :display inline-flex
    :align-items baseline
    :gap 0.15em
    :min-width 0)
   (".prefix"
    :color --muted
    :font-weight bold)
   ((".conditional > .list" ".conditional > .skipped")
    :border-style dashed)
   (".skipped"
    :color --muted
    :white-space pre-wrap)
   ((".body.prefixed" ".body.conditional")
    :display flex)))

(define-style code-references
  "Code references and lisp: definition blocks."
  ("details.definition"
   :margin 0.6rem 0
   ("& > summary"
    :cursor pointer
    :font-size 0.9rem
    :color --muted
    (".kind" :color --accent)
    (".name" :color --ink)
    ("a.source"
     :margin-left 0.5rem
     :font-size 0.8rem))
   ("& > .lisp"
    :margin-top 0.4rem))
  (".code-references > p.backlinks"
   :margin-bottom 0.2rem))

(define-style source-browser
  "Source browser: definition links, whole-file pages, source metadata."
  (".lisp a.definition-link"
   :color inherit
   ("&:hover"
    :color --accent
    :text-decoration none))
  (".lisp.source"
   :background none
   :padding 0
   ("& > .toplevel"
    :padding 0
    :margin 1.1rem 0
    :scroll-margin-top 1rem
    ("&:target"
     :box-shadow -0.6rem 0 0 -0.4rem --accent)
    ("& > .comment.prose"
     :border-left none
     :padding-left 0
     :max-width 44rem)))
  ("h1.source-title"
   :font-size 1.8rem
   :letter-spacing -0.04em)
  ("p.source-meta"
   :color --muted
   :font-size 0.9rem))

(defparameter *definition-kind-colours*
  '((--mark-idea defclass defstruct define-condition)
    (--accent defgeneric)
    (--mark-wait defmacro)
    (--mark-done defvar defparameter defconstant))
  "The colour of a definition kind's badge in the definitions index.")

(defun kind-rules ()
  "A rule per colour of *DEFINITION-KIND-COLOURS* for the kinds it paints."
  (loop for (colour . kinds) in *definition-kind-colours*
        collect (make-rule (loop for kind in kinds
                                 collect (format nil "li.definition-entry .kind-~(~A~)" kind))
                           (list :color colour))))

(define-style definitions-index
  "Definitions index: kind badge · name · signature · line."
  ("nav.definitions"
   :margin 1rem 0 2rem
   :padding 0.6rem 0.9rem
   :background --code-bg
   :border-radius 0.4rem
   ("ul"
    :list-style none
    :padding-left 0
    :margin 0
    :display grid
    :grid-template-columns max-content (minmax 0 1fr)
    :column-gap 0.7em
    :row-gap 0.15rem
    :align-items baseline))
  ("li.definition-entry"
   :display contents
   :font-family --display-font
   :font-size 0.85rem
   :line-height 1.45
   (".kind"
    :font-size 0.78em
    :color --muted
    :white-space nowrap
    :text-align right))
  ("li.method-entry .kind"
   :opacity 0.6)
  ("li.method-entry a.name"
   :padding-left 1.2em)
  (kind-rules)
  ("li.definition-entry a.name"
   :color --ink
   :overflow-wrap anywhere
   ("&:hover" :color --accent))
  ("li.definition-entry .signature"
   :margin-left 0.3em
   :color --muted
   :font-size 0.9em
   :overflow-wrap anywhere)
  ("details.definition > summary .kind"
   :color --accent))

(define-style source-index
  "Source index: files expand to their definitions in place; the systems table."
  ("details.source-file"
   :margin 0.25rem 0
   ("& > summary"
    :cursor pointer
    :font-size 0.9rem
    ((".count" ".system")
     :margin-left 0.6em
     :color --muted
     :font-size 0.8em)))
  ("table.systems"
   :width 100%
   :margin 1rem 0 2rem
   :font-size 0.88rem
   :line-height 1.35
   ("th"
    :font (display-font 700 0.72rem/1.4)
    :text-transform uppercase
    :letter-spacing 0.06em
    :color --muted
    :padding 0.3rem 0.9rem 0.3rem 0
    :border-bottom (hairline))
   ("td"
    :padding 0.35rem 0.9rem 0.35rem 0
    :border-bottom (hairline)
    :vertical-align top)
   ("tr:target td"
    :background (color-mix --paper 94% --accent)))
  ("td.system-name"
   :font-weight 700
   :white-space nowrap)
  ("td.system-description"
   :max-width 28rem
   :color (color-mix --ink 80% --muted))
  ("td.system-depends"
   :max-width 16rem
   :color --muted)
  ("td.system-files"
   :min-width 18rem
   ("details.source-file"
    :margin 0
    ("& > summary"
     :font-size 0.88rem
     :line-height 1.5))
   ("nav.definitions"
    :margin 0.3rem 0 0.6rem 0.9rem
    :padding 0.4rem 0.7rem))
  ("details.source-file > summary .directory"
   :color --muted)
  ("details.source-file > summary .file"
   :color --accent
   :font-weight 600)
  ("details.source-file > nav.definitions"
   :margin 0.4rem 0 0.8rem 1.2rem)
  ("a.github"
   :color --muted
   :text-decoration none))

(define-style code-prose
  "Docstrings and comments as prose inside the boxes."
  (".lisp .prose"
   :flex-basis 100%
   :white-space normal
   :font-family --body-font
   :text-align start
   :font-size 0.95em
   :line-height 1.45
   :max-width 40rem
   ("p" :margin 0.15em 0 0.35em)
   (("p:last-child" "ul:last-child") :margin-bottom 0.1em)
   (("ul" "ol")
    :margin 0.2em 0
    :padding-left 1.2rem)
   ("code"
    :font-family --mono-font
    :font-size 0.85em))
  (".lisp .string.prose"
   :color (color-mix --ink 75% --mark-done)
   :padding-left 0.6em
   :border-left 2px solid (color-mix --mark-done 40% :transparent))
  (".lisp .comment.prose"
   :color --muted
   :font-style italic
   :padding-left 0.6em
   :border-left 2px solid --rule
   ("code" :font-style normal))
  (".lisp .prose code.symbol"
   :font-family --display-font
   :font-size 0.95em
   :font-style normal
   :background none
   :padding 0
   :color --ink)
  (".lisp .prose a.definition-link code.symbol"
   :color --accent))

(define-style stacked
  "A list with body forms is a grid of rows: the .head row, then one row
per body form, so the box hugs its widest row instead of stretching to
its parent (#993QQQ)."
  (".lisp .list.stacked"
   (stacked-rows)
   :width fit-content
   :max-width 100%
   ("& > .head" (flowing-row))
   ("& > .body"
    :flex-basis auto
    :max-width 100%)
   ("& > .list.body"
    :width fit-content)))

(define-style clauses
  "COND, CASE, HANDLER-CASE and other clause forms are tables (#4175NC):
each clause a row with its key or test in the first column and its
body forms stacked in the second, aligned across the clauses."
  (".lisp .list.clauses"
   :grid-template-columns (fit-content 50%) (minmax 0 1fr)
   :column-gap 0.7em
   (("& > .head" "& > .comment")
    :grid-column span 2)
   ("& > .stacked-clause"
    (subgrid-row)
    :width auto
    ("& > .head" (flowing-row))
    ("& > .rest"
     (stacked-rows)
     :row-gap 0.05em
     :min-width 0)
    ("& > .rest > .body"
     :flex-basis auto
     :max-width 100%)
    ("& > .rest > .list.body"
     :width fit-content))))

(define-style layouts
  "Layouts: binding grids, clauses, loop rows.  The selectors are the roles
wiki-dexp.lisp assigns."
  (".lisp .list.bindings"
   :display grid
   :grid-template-columns max-content (minmax 0 1fr)
   :column-gap 0.6em
   :row-gap 0.1em
   :align-items baseline
   ("& > .list.clause"
    (subgrid-row)
    :column-gap 0.6em)
   (("& > .symbol" "& > .comment")
    :grid-column span 2))
  (".lisp .rest" (flowing-row))
  (".lisp .list.clause:not(.bindings > *)"
   :display flex)
  (".lisp .break"
   :flex-basis 100%
   :height 0)
  (".lisp .row-start"
   :color --accent))

(define-style pairs
  "Keyword/value pairs stay together; in body position each pair is a row
with keys loosely aligned.  A SETF with several pairs is a place/value
table: each pair a subgrid row of its two-column box."
  (".lisp .pair"
   :display inline-flex
   :align-items baseline
   :column-gap 0.5em
   :min-width 0
   ("&.body"
    :display flex
    :flex-basis 100%
    ("& > .symbol:first-child" :min-width 8em)))
  (".lisp .list.pairs"
   :grid-template-columns max-content (minmax 0 1fr)
   :column-gap 0.6em
   (("& > .head" "& > .comment")
    :grid-column span 2)
   ("& > .pair.body"
    (subgrid-row)
    ("& > .symbol:first-child" :min-width 0))))

(define-style narrow-screens
  "Narrow screens: one column, smaller title, ragged prose."
  (:media "screen and (max-width: 90ch)"
   ((".library" ".doors") :grid-template-columns (minmax 0 1fr))
   ("h1" :font-size 1.65rem)
   ("p" :text-align start)))

(define-style figure-cards
  "Figure and definition cards shown beside mentions."
  (".figure-popover"
   :position absolute
   :margin 0
   :padding 0
   :border (hairline)
   :border-radius 8px
   :background --paper
   :color --ink
   :box-shadow 0 8px 28px (rgba 0 0 0 0.16)
   :font 400 0.92rem/1.4 --body-font
   :overflow hidden
   :inset auto
   ("&:popover-open" :display block))
  ((".figure-card .card-title" ".figure-popover .card-title")
   :display block
   :padding 0.65rem 0.85rem 0.15rem
   :font (display-font 700 1rem/1.25)
   :letter-spacing -0.02em
   :color --ink)
  (".figure-popover .card-title:hover"
   :color --accent
   :text-decoration none)
  (".figure-popover .card-title .mark"
   :font-size 0.55em
   :vertical-align 0.15em)
  (".figure-popover .card-meta"
   :display block
   :padding 0 0.85rem 0.5rem
   :color --muted
   :font (display-font 500 0.8rem/1.3)
   :border-bottom (hairline))
  (".figure-popover .card-id"
   :font-family --mono-font
   :font-weight 700)
  (".figure-popover .card-excerpt"
   :margin 0
   :padding 0.6rem 0.85rem 0.75rem
   :text-align start
   :hyphens auto
   :color (color-mix --ink 88% --paper)))

(define-style math-and-diagrams
  "Math and diagrams."
  (".math.display"
   :margin 1rem 0
   :overflow-x auto
   :text-align center)
  ("pre.mermaid"
   :background none
   :padding 0.5rem 0
   :text-align center
   :font 0.85rem/1.4 --mono-font
   :color --muted
   ("svg" :max-width 100% :height auto)
   (("p" "span") :text-align center :margin 0)))

(define-style definition-cards
  "Definition cards."
  (".figure-popover .card-kind"
   :color --accent
   :font-weight 500)
  (".figure-popover .card-lambda-list"
   :display block
   :padding 0 0.85rem 0.35rem
   :background none
   :color --muted
   :font-size 0.8rem
   :overflow-wrap anywhere))

(define-style systems-table
  "Systems table: file entries and their popover buttons; the systems graph."
  (".file-entry"
   :display flex
   :align-items baseline
   :gap 0.5em
   :line-height 1.5
   (".directory" :color --muted)
   (".file" :color --accent :font-weight 600))
  ("button.count"
   :font (display-font 600 0.72rem/1.4)
   :color --muted
   :background none
   :border (hairline)
   :border-radius 999px
   :padding 0 0.5em
   :cursor pointer
   ("&:hover" :border-color --accent :color --accent))
  (".figure-popover nav.definitions"
   :margin 0
   :padding 0.5rem 0.85rem 0.7rem
   :background none
   :max-height 60vh
   :overflow-y auto)
  ("pre.mermaid.system-graph"
   :margin 0.5rem 0 1.5rem
   :text-align start
   ("svg" :max-width 100% :height auto))
  ("p.graph-note"
   :color --muted
   :font-size 0.85rem
   :margin-bottom 0.2rem))

(define-style pages-table
  "The Pages table."
  ("table.pages"
   :width 100%
   :margin 1rem 0 2rem
   :font-size 0.9rem
   ("td"
    :padding 0.45rem 1rem 0.45rem 0
    :border-bottom (hairline)
    :vertical-align top))
  ("td.page-title"
   :width 18rem
   :font-weight 700
   ("a" :color --ink))
  ("td.page-headings a.heading"
   :display inline-block
   :margin 0 0.9em 0.15em 0
   :color --ink
   ("&.level-2" :color --muted :font-size 0.92em)
   ("&:hover" :color --accent :text-decoration none)
   (".mark" :font-size 0.65em)))

(define-style work-table
  "The Work page: marks by status."
  ("section.work-status"
   :margin 1.5rem 0 2rem
   ("& > h2"
    :font-size 1rem
    :margin 0 0 0.4rem
    (".mark" :font-size 0.75em)))
  (".status-meaning"
   :font-weight 400
   :color --muted)
  ("table.work"
   :width 100%
   :font-size 0.9rem
   ("td"
    :padding 0.4rem 1rem 0.4rem 0
    :border-bottom (hairline)
    :vertical-align top))
  ("td.work-title"
   :width 22rem
   :font-weight 600
   ("a" :color --ink ("&:hover" :color --accent :text-decoration none)))
  (".work-page"
   :display block
   :font-weight 400
   :font-size 0.8rem
   :color --muted)
  ("td.work-intent"
   :color (color-mix --ink 80% --muted)))
