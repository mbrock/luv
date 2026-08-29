// Typst experiments for the structural Lisp source view in wiki/dexp.lisp.
//
// These are deliberately hand-authored from a role-tagged tree.  The wiki's
// Lisp reader and layout objects should continue to decide which children are
// heads, bodies, bindings, clauses, and pairs; Typst is only the drawing
// backend explored here.

#let paper = rgb("#fbfaf7")
#let ink = rgb("#171b1d")
#let muted = rgb("#687178")
#let rule = rgb("#ddd8cf")
#let code-bg = rgb("#f4f1ea")
#let accent = rgb("#a53b51")
#let keyword-color = rgb("#645696")
#let string-color = rgb("#557a55")
#let number-color = rgb("#8a791d")
#let paren-color = ink.transparentize(72%)

#set page(
  width: 13in,
  height: 7.5in,
  margin: (x: 17mm, y: 13mm),
  fill: paper,
)
#set text(font: "DejaVu Sans", size: 9pt, fill: ink, hyphenate: false)
#set par(justify: false, leading: 0.45em)

#let title(name, note) = [
  #text(size: 17pt, weight: 700, name)
  #h(1fr)
  #text(size: 7.5pt, weight: 600, fill: muted, note)
  #v(3mm)
  #line(length: 100%, stroke: 0.6pt + rule)
  #v(5mm)
]

#let label(name) = block(
  above: 0pt,
  below: 2mm,
  text(size: 7pt, weight: 700, fill: muted, tracking: 0.08em, upper(name)),
)

#let note(body) = block(
  inset: (left: 2.5mm),
  stroke: (left: 1.2pt + rule),
  text(size: 8pt, fill: muted, body),
)

#let token(value, kind: "symbol") = {
  let fill = if kind == "operator" {
    accent
  } else if kind == "keyword" {
    keyword-color
  } else if kind == "string" {
    string-color
  } else if kind == "number" {
    number-color
  } else if kind == "muted" {
    muted
  } else {
    ink
  }
  box(text(fill: fill, value))
}

#let flow(items) = items.join([ ])

#let side-stroke = (left: 0.7pt + paren-color, right: 0.7pt + paren-color)

#let inline-list(items) = box(
  inset: (x: 0.38em, y: 0.02em),
  radius: 0.55em,
  stroke: side-stroke,
  flow(items),
)

// A constrained flow list is the closest direct Typst counterpart to a
// wrapping flex row.  Real spaces between atomic children are its breakpoints.
#let wrapping-list(width, items) = block(
  width: width,
  inset: (x: 0.38em, y: 0.08em),
  radius: 0.55em,
  stroke: side-stroke,
  flow(items),
)

// A first production-shaped decision: measure the intrinsic inline box and
// fall back to the width-constrained paragraph only when it does not fit.
#let adaptive-list(width, items) = context {
  let candidate = inline-list(items)
  if measure(candidate).width <= width {
    candidate
  } else {
    wrapping-list(width, items)
  }
}

#let stacked-list(head, rows, width: auto) = box(
  width: width,
  inset: (x: 0.38em, y: 0.08em),
  radius: 0.55em,
  stroke: side-stroke,
  grid(
    columns: 1,
    row-gutter: 0.08em,
    flow(head),
    ..rows,
  ),
)

#let breakable-stacked-list(width, head, rows) = block(
  width: width,
  breakable: true,
  inset: (x: 0.38em, y: 0.08em),
  radius: 0.55em,
  stroke: side-stroke,
  grid(
    columns: 1,
    row-gutter: 0.08em,
    flow(head),
    ..rows,
  ),
)

#let doc(body) = block(
  inset: (left: 0.55em),
  stroke: (left: 1.1pt + string-color.transparentize(50%)),
  text(size: 8.2pt, fill: string-color, body),
)

#let binding-table(rows) = inline-list((grid(
  columns: (auto, auto),
  column-gutter: 0.65em,
  row-gutter: 0.08em,
  align: top,
  ..rows.flatten(),
),))

#let clause-table(head, rows) = box(
  inset: (x: 0.38em, y: 0.08em),
  radius: 0.55em,
  stroke: side-stroke,
  grid(
    columns: (auto, 1fr),
    column-gutter: 0.8em,
    row-gutter: 0.12em,
    grid.cell(colspan: 2, flow(head)),
    ..rows.flatten(),
  ),
)

// ---------------------------------------------------------------------------
// Experiment 1: the basic geometry and the line-breaking boundary.

#title("DEXP in Typst", "01 · side strokes and wrapping")

#grid(
  columns: (1fr, 1fr),
  column-gutter: 12mm,
  [
    #label("One-line list: an inline atomic box")
    #inline-list((
      token("defun", kind: "operator"),
      token("render-lisp-nodes"),
      inline-list((token("nodes"), token("&key"), token("package"))),
    ))

    #v(7mm)
    #label("Multi-row list: a one-column intrinsic grid")
    #stacked-list(
      (
        token("defun", kind: "operator"),
        token("split-head-items"),
        inline-list((token("items"),)),
      ),
      (
        doc([Items before the first body item become the head; the rest become rows.]),
        inline-list((
          token("let", kind: "operator"),
          inline-list((
            inline-list((token("position"), inline-list((token("position-if"), token("#'item-body-p"), token("items"))))),
          )),
        )),
        inline-list((token("values", kind: "operator"), token("items"), token("'()", kind: "muted"))),
      ),
    )
  ],
  [
    #label("Measured, then capped at 72 mm")
    #adaptive-list(72mm, (
      token("loop", kind: "operator"), token("for"), token("candidate"),
      token("in"), inline-list((token("site-source-files"), token("site"))),
      token("when"), inline-list((token("source-file-package"), token("candidate"))),
      token("collect"), inline-list((token("source-page-name"), token("candidate"))),
    ))

    #v(7mm)
    #label("Measured, then capped at 44 mm")
    #adaptive-list(44mm, (
      token("loop", kind: "operator"), token("for"), token("candidate"),
      token("in"), inline-list((token("site-source-files"), token("site"))),
      token("when"), inline-list((token("source-file-package"), token("candidate"))),
      token("collect"), inline-list((token("source-page-name"), token("candidate"))),
    ))

    #v(7mm)
    #note([The border radius turns left/right-only strokes into parenthesis-like caps. `measure` keeps a fitting list inline; an over-wide one becomes a width-constrained paragraph of atomic children, which supplies flex-like wrapping.])
  ],
)

#pagebreak()

// ---------------------------------------------------------------------------
// Experiment 2: render the renderer, using its semantic head/body split.

#title("The renderer renders itself", "02 · head row plus body rows")

#let split-head = stacked-list(
  (
    token("defun", kind: "operator"),
    token("split-head-items"),
    inline-list((token("items"),)),
  ),
  (
    doc([Items before the first body item become the head, and the rest become rows.]),
    stacked-list(
      (
        token("let", kind: "operator"),
        binding-table(((
          token("position"),
          inline-list((token("position-if", kind: "operator"), token("#'item-body-p"), token("items"))),
        ),)),
      ),
      (
        stacked-list(
          (token("if", kind: "operator"), inline-list((token("null", kind: "operator"), token("position")))),
          (
            inline-list((token("values", kind: "operator"), token("items"), token("'()", kind: "muted"))),
            stacked-list(
              (token("progn", kind: "operator"),),
              (
                wrapping-list(88mm, (
                  token("loop", kind: "operator"), token("while"),
                  inline-list((
                    token("and", kind: "operator"),
                    inline-list((token(">", kind: "operator"), token("position"), token("0", kind: "number"))),
                    inline-list((token("eq", kind: "operator"), token("(item-kind …)"), token(":comment", kind: "keyword"))),
                  )),
                  token("do"), inline-list((token("decf", kind: "operator"), token("position"))),
                )),
                inline-list((
                  token("values", kind: "operator"),
                  inline-list((token("subseq", kind: "operator"), token("items"), token("0", kind: "number"), token("position"))),
                  inline-list((token("nthcdr", kind: "operator"), token("position"), token("items"))),
                )),
              ),
            ),
          ),
        ),
      ),
    ),
  ),
)

#let render-children = stacked-list(
  (
    token("defun", kind: "operator"), token("render-children-with-roles"),
    inline-list((token("layout"), token("list"))),
  ),
  (
    doc([When a child has the body role, the head becomes one flowing row and every body form becomes a row.]),
    stacked-list(
      (
        token("let", kind: "operator"),
        binding-table(((token("items"), inline-list((token("layout-items", kind: "operator"), token("layout"), token("list")))),)),
      ),
      (
        stacked-list(
          (token("if", kind: "operator"), inline-list((token("some", kind: "operator"), token("#'item-body-p"), token("items")))),
          (
            stacked-list(
              (
                token("multiple-value-bind", kind: "operator"),
                inline-list((token("head"), token("rows"))),
                inline-list((token("split-head-items", kind: "operator"), token("items"))),
              ),
              (
                inline-list((token("spinneret:with-html", kind: "operator"), inline-list((token(":span.head", kind: "keyword"), token("(dolist …)"))))),
                inline-list((token("dolist", kind: "operator"), inline-list((token("item"), token("rows"))), token("(render-item …)"))),
              ),
            ),
            inline-list((token("dolist", kind: "operator"), inline-list((token("item"), token("items"))), token("(render-item …)"))),
          ),
        ),
      ),
    ),
  ),
)

#grid(
  columns: 1,
  row-gutter: 4mm,
  split-head,
  render-children,
)

#pagebreak()

// ---------------------------------------------------------------------------
// Experiment 3: semantic tables that replace CSS grid/subgrid.

#title("Roles become native tables", "03 · bindings, clauses, and pairs")

#grid(
  columns: (1fr, 1fr),
  column-gutter: 12mm,
  [
    #label("LET bindings: a two-column grid")
    #stacked-list(
      (
        token("let", kind: "operator"),
        binding-table((
          (token("arguments"), inline-list((token("argument-children", kind: "operator"), token("list")))),
          (token("pairs-start"), inline-list((token("layout-pairs-index", kind: "operator"), token("layout"), token("arguments")))),
          (token("index"), token("-1", kind: "number")),
          (token("previous"), token("nil")),
          (token("items"), token("'()", kind: "muted")),
        )),
      ),
      (
        inline-list((token("loop", kind: "operator"), token("while"), token("remaining"), token("do"), token("…", kind: "muted"))),
        inline-list((token("nreverse", kind: "operator"), token("items"))),
      ),
    )

    #v(7mm)
    #note([The renderer emits the binding cells directly into Typst's grid. There is no nested subgrid object and no alignment by textual indentation.])
  ],
  [
    #label("COND clauses: key and consequent columns")
    #clause-table(
      (token("cond", kind: "operator"),),
      (
        (
          inline-list((token("keyword-p"),)),
          inline-list((token("span", kind: "operator"), token("package"), token("\":\"", kind: "string"))),
        ),
        (
          inline-list((token("current-p"),)),
          inline-list((token("render-name", kind: "operator"), token("symbol"))),
        ),
        (
          token("t"),
          stacked-list(
            (token("if", kind: "operator"), token("definition")),
            (
              inline-list((token("render-link", kind: "operator"), token("definition"))),
              inline-list((token("render-text", kind: "operator"), token("symbol"))),
            ),
          ),
        ),
      ),
    )

    #v(7mm)
    #label("Keyword/value pairs: one unbreakable unit")
    #wrapping-list(72mm, (
      token("make-item", kind: "operator"), token(":pair", kind: "keyword"),
      inline-list((token(":value", kind: "keyword"), token("value"))),
      inline-list((token(":comments", kind: "keyword"), token("(nreverse comments)"))),
      inline-list((token(":index", kind: "keyword"), token("index"))),
      inline-list((token(":previous", kind: "keyword"), token("previous"))),
    ))
  ],
)

#v(8mm)
#note([Conclusion: Typst naturally supplies the rows, tables, strokes, colors, and print surface. A production backend still needs either explicit width budgets plus measurement for nested wrapping, or wrap decisions precomputed by the Lisp layout pass.])

#pagebreak()

// ---------------------------------------------------------------------------
// Experiment 4: a structural form crossing a print page boundary.

#title("A form longer than a page", "04 · breakable rows and continuing strokes")

#note([This is one breakable block containing a grid, not two separately drawn boxes. The following page reveals how Typst fragments the left/right parenthesis strokes.])
#v(5mm)

#let long-body = range(1, 57).map(i => inline-list((
  token("render-body-row", kind: "operator"),
  token(str(i), kind: "number"),
  inline-list((token("child-role", kind: "operator"), token("layout"), token("list"), token(str(i), kind: "number"))),
)))

#breakable-stacked-list(
  112mm,
  (
    token("defun", kind: "operator"),
    token("render-long-definition"),
    inline-list((token("layout"), token("list"))),
  ),
  long-body,
)
