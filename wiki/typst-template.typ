#let dexp-planner = plugin("../build/typst-prty-zig/bin/typst-prty.wasm")

#let ink = rgb("#202124")
#let muted = rgb("#62666d")
#let accent = rgb("#4b6488")
#let panel = rgb("#f2f0ea")
#let default-body-font = ("Libertinus Serif",)
#let dexp-keyword = rgb("#755a7c")
#let dexp-string = rgb("#50705f")
#let dexp-number = rgb("#766a2e")
#let dexp-paren = ink.transparentize(72%)
#let dexp-side-stroke = (left: 0.7pt + dexp-paren, right: 0.7pt + dexp-paren)
#let dexp-row-gap = 3.984pt
#let dexp-column-gap = 1.162pt
#let dexp-frame-inset-x = 3.32pt
#let dexp-frame-inset-y = 1.162pt
#let dexp-prefix-gap = 0.415pt

#let example(body) = block(
  width: 100%,
  fill: panel,
  inset: 10pt,
  radius: 3pt,
  breakable: false,
  raw(body, block: true),
)

#let dexp-token-variants(node, limit) = {
  let fill = if node.style == "operator" {
    accent
  } else if node.style == "keyword" {
    dexp-keyword
  } else if node.style == "string" {
    dexp-string
  } else if node.style == "number" {
    dexp-number
  } else if node.style == "comment" or node.style == "muted" {
    muted
  } else {
    ink
  }
  let inline = box(
    baseline: top,
    box(inset: (y: 0.045em), text(
        fill: fill,
        style: if node.style == "comment" { "italic" } else { "normal" },
        node.text,
      ),
    ),
  )
  if node.style == "comment" or node.style == "string" {
    let available = calc.max(32pt, limit - 10pt)
    let widths = (
      available,
      calc.max(32pt, available * 0.75),
      calc.max(32pt, available * 0.5),
      calc.max(32pt, available * 0.33),
    )
    (inline,) + widths.map(width => box(
      baseline: top,
      block(
          width: width,
          text(
            fill: fill,
            style: if node.style == "comment" { "italic" } else { "normal" },
            node.text,
          ),
        ),
    ))
  } else {
    (inline,)
  }
}

#let dexp-operation(kind, children, ..options) = {
  let request = (kind: kind, children: children.map(child => child.request))
  for (name, value) in options.named() {
    request.insert(name, value / 1pt)
  }
  let leaves = ()
  for child in children {
    leaves += child.leaves
  }
  (request: request, leaves: leaves)
}

#let dexp-row(children, gap: dexp-row-gap) = dexp-operation(
  "row",
  children,
  gap: gap,
)

#let dexp-column(children, gap: dexp-column-gap) = dexp-operation(
  "column",
  children,
  gap: gap,
)

#let dexp-row-column(children) = dexp-operation(
  "row_column",
  children,
  row_gap: dexp-row-gap,
  column_gap: dexp-column-gap,
)

#let dexp-prefer-row(children) = dexp-operation(
  "prefer_row",
  children,
  row_gap: dexp-row-gap,
  column_gap: dexp-column-gap,
)

#let dexp-call(children) = dexp-operation(
  "call",
  children,
  row_gap: dexp-row-gap,
  column_gap: dexp-column-gap,
)

#let dexp-frame(child) = dexp-operation(
  "frame",
  (child,),
  inset_x: dexp-frame-inset-x,
  inset_y: dexp-frame-inset-y,
)

#let dexp-prepare-token(node, limit) = {
  let variants = dexp-token-variants(node, limit)
  let sizes = variants.map(body => {
    let size = measure(body)
    (width: size.width / 1pt, height: size.height / 1pt)
  })
  (
    request: (kind: "leaf", variants: sizes),
    leaves: (variants,),
  )
}

#let dexp-prepare-list(node, limit, prepare) = {
  let documents(nodes) = nodes.map(child => prepare(child, limit))
  let inner = if node.structure == "stacked" {
    let rows = node.rows.map(child => prepare(child, limit))
    let parts = if node.head.len() == 0 {
      rows
    } else {
      let head-items = documents(node.head)
      (dexp-prefer-row(head-items),) + rows
    }
    dexp-column(parts)
  } else {
    let items = documents(node.items)
    if node.structure == "column" {
      dexp-column(items)
    } else if node.callee and items.len() > 1 {
      dexp-call(items)
    } else if node.callee and items.len() == 1 {
      items.first()
    } else {
      dexp-row-column(items)
    }
  }
  dexp-frame(inner)
}

#let dexp-prepare-node(node, limit) = {
  if node.kind == "list" {
    dexp-prepare-list(node, limit, dexp-prepare-node)
  } else if node.kind == "pair" {
    dexp-row(node.items.map(child => dexp-prepare-node(child, limit)))
  } else if node.kind == "prefix" {
    dexp-row(
      (
        dexp-prepare-token((style: "muted", text: node.prefix), limit),
        dexp-prepare-node(node.child, limit),
      ),
      gap: dexp-prefix-gap,
    )
  } else {
    dexp-prepare-token(node, limit)
  }
}

#let dexp-materialize(layout, leaves) = {
  let render(layout) = {
    if layout.kind == "empty" {
      box(baseline: top)
    } else if layout.kind == "leaf" {
      leaves.at(layout.leaf).at(layout.variant)
    } else if layout.kind == "row" {
      box(
        baseline: top,
        layout.children.map(render).join(h(layout.gap * 1pt)),
      )
    } else if layout.kind == "column" {
      box(
        baseline: top,
        grid(
          columns: 1,
          row-gutter: layout.gap * 1pt,
          ..layout.children.map(render),
        ),
      )
    } else if layout.kind == "frame" {
      box(
        baseline: top,
        stroke: dexp-side-stroke,
        radius: 0.58em,
        inset: (x: dexp-frame-inset-x, y: dexp-frame-inset-y),
        render(layout.child),
      )
    } else {
      panic("unknown DEXP layout recipe " + layout.kind)
    }
  }
  render(layout)
}

#let dexp-source(nodes) = block(
  width: 100%,
  fill: panel,
  inset: 10pt,
  radius: 3pt,
  breakable: true,
  [
    #set text(font: "Iosevka Aile", size: 8.3pt, fill: ink, hyphenate: false)
    #set par(justify: false, leading: 0.56em)
    #context {
      layout(size => {
        let documents = nodes.map(node => {
          let prepared = dexp-prepare-node(node, size.width)
          let request = (
            limit: size.width / 1pt,
            max_frontier: 8,
            root: prepared.request,
          )
          let result = json(dexp-planner.layout(bytes(json.encode(request))))
          dexp-materialize(result.layout, prepared.leaves)
        })
        grid(
          columns: 1,
          row-gutter: 0.72em,
          ..documents,
        )
      })
    }
  ],
)

#let workshop(title: none, body-font: default-body-font, body) = {
  set page(
    paper: "a4",
    margin: (inside: 27mm, outside: 22mm, top: 24mm, bottom: 24mm),
    numbering: "1",
    number-align: right + bottom,
    header: context {
      if counter(page).get().first() > 1 {
        set text(font: "Iosevka Aile", size: 7.5pt, fill: muted)
        title
      }
    },
    footer: context {
      set text(font: "Iosevka Aile", size: 8pt, fill: muted)
      h(1fr)
      counter(page).display("1")
    },
  )
  set text(font: body-font, size: 10.5pt, lang: "en", fill: ink)
  set par(justify: true, leading: 0.68em)
  set list(indent: 1.2em, body-indent: 0.55em)
  set enum(indent: 1.2em, body-indent: 0.55em)
  set table(stroke: (x: none, y: 0.35pt + rgb("#c9c7c0")), inset: 5pt)
  show raw: set text(font: "Monaspace Neon", size: 7.7pt,
                     features: ("calt": 0, "liga": 0))

  show heading.where(level: 1): it => block(above: 1.75em, below: 0.55em, sticky: true)[
    #set text(font: body-font, size: 15pt, weight: "bold", fill: ink,
              hyphenate: false)
    #it.body
  ]
  show heading.where(level: 2): it => block(above: 1.7em, below: 0.6em, sticky: true)[
    #set text(font: body-font, size: 13pt, weight: "bold", fill: ink,
              hyphenate: false)
    #it.body
  ]
  show raw.where(block: true): it => block(
    width: 100%,
    fill: panel,
    inset: 10pt,
    radius: 3pt,
    breakable: false,
    it,
  )
  show link: set text(fill: accent)

  align(center + horizon)[
    #block(width: 100%)[
      #v(22mm)
      #text(font: "Iosevka Aile", size: 8pt, weight: "bold", tracking: 0.12em,
            fill: accent)[LUV WORKSHOP / DESIGN NOTES]
      #v(13mm)
      #text(font: body-font, size: 28pt, weight: "bold", fill: ink)[#title]
      #v(8mm)
      #line(length: 28mm, stroke: 1.2pt + accent)
      #v(1fr)
    ]
  ]
  pagebreak()
  counter(page).update(1)
  body
}
