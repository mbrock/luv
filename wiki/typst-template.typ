#import "typst-prty.typ": leaf, row, column, frame, choose, best

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

#let example(body) = block(
  width: 100%,
  fill: panel,
  inset: 10pt,
  radius: 3pt,
  breakable: false,
  raw(body, block: true),
)

#let dexp-token(node, limit) = {
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
  let inline = leaf(
    box(inset: (y: 0.045em), text(
      fill: fill,
      style: if node.style == "comment" { "italic" } else { "normal" },
      node.text,
    )),
    plan: (),
  )
  if node.style == "comment" or node.style == "string" {
    let available = calc.max(32pt, limit - 10pt)
    let widths = (
      available,
      calc.max(32pt, available * 0.75),
      calc.max(32pt, available * 0.5),
      calc.max(32pt, available * 0.33),
    )
    choose((inline,) + widths.map(width => leaf(
      block(
        width: width,
        text(
          fill: fill,
          style: if node.style == "comment" { "italic" } else { "normal" },
          node.text,
        ),
      ),
      plan: (),
    )))
  } else {
    inline
  }
}

#let dexp-list(node, limit, render) = {
  let child-limit = limit
  let documents(nodes) = nodes.map(child => render(child, child-limit))
  let inner = if node.structure == "stacked" {
    let rows = node.rows.map(child => render(child, child-limit))
    let parts = if node.head.len() == 0 {
      rows
    } else {
      let head-items = documents(node.head)
      let head-row = row(head-items, child-limit, plan: ())
      let head = if head-row.filter(candidate => candidate.width <= child-limit).len() > 0 {
        head-row
      } else {
        column(head-items, child-limit, plan: ())
      }
      (head,) + rows
    }
    column(parts, child-limit, plan: ())
  } else {
    let items = documents(node.items)
    if node.structure == "column" {
      column(items, child-limit, plan: ())
    } else if node.callee and items.len() > 1 {
      let arguments = items.slice(1)
      let arrangement = choose((
        row(arguments, child-limit, plan: ()),
        column(arguments, child-limit, plan: ()),
      ))
      let beside = row((items.first(), arrangement), child-limit, plan: ())
      if beside.filter(candidate => candidate.width <= child-limit).len() > 0 {
        beside
      } else {
        column(items, child-limit, plan: ())
      }
    } else if node.callee and items.len() == 1 {
      items.first()
    } else {
      choose((
        row(items, child-limit, plan: ()),
        column(items, child-limit, plan: ()),
      ))
    }
  }
  frame(
    inner,
    limit,
    stroke: dexp-side-stroke,
    radius: 0.58em,
    inset-x: 3.32pt,
    inset-y: 1.162pt,
    plan: (),
  )
}

#let dexp-node(node, limit) = {
  if node.kind == "list" {
    dexp-list(node, limit, dexp-node)
  } else if node.kind == "pair" {
    row(node.items.map(child => dexp-node(child, limit)), limit, plan: ())
  } else if node.kind == "prefix" {
    row((
      dexp-token((style: "muted", text: node.prefix), limit),
      dexp-node(node.child, limit),
    ), limit, gap: 0.415pt, plan: ())
  } else {
    dexp-token(node, limit)
  }
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
        let documents = nodes.map(node => dexp-node(node, size.width))
        grid(
          columns: 1,
          row-gutter: 0.72em,
          ..documents.map(document => best(document, size.width).body),
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
