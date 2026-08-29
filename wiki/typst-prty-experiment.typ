// A proportional, rectangle-based port of Wisp's tiny Bernardy frontier.
// The semantic policy offers argument ROW or COLUMN layouts, never fill.

#import "typst-prty.typ": *

#let paper = rgb("#f7f6f2")
#let ink = rgb("#202124")
#let muted = rgb("#62666d")
#let rule = rgb("#c9c7c0")
#let accent = rgb("#4b6488")
#let keyword-color = rgb("#755a7c")
#let number-color = rgb("#766a2e")
#let paren-color = ink.transparentize(72%)
#let side-stroke = (left: 0.7pt + paren-color, right: 0.7pt + paren-color)

#set page(
  width: 13in,
  height: 7.5in,
  margin: (x: 17mm, y: 13mm),
  fill: paper,
)
#set text(font: "Iosevka Aile", size: 8.3pt, fill: ink, hyphenate: false)
#set par(justify: false, leading: 0.56em)

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
  below: 1.5mm,
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
  } else if kind == "number" {
    number-color
  } else {
    ink
  }
  box(text(fill: fill, value))
}

#let atom(value, kind: "symbol") = leaf(
  token(value, kind: kind),
  plan: (),
)

#let list-frame(document, limit, plan: ()) = frame(
  document,
  limit,
  stroke: side-stroke,
  radius: 0.55em,
  inset-x: 3.32pt,
  inset-y: 1.162pt,
  plan: plan,
)

// Like Wisp's generic symbolic call: the operator remains at the left while
// its complete argument sequence chooses either one row or one column.
#let call(name, arguments, limit) = {
  let argument-layout = choose((
    row(arguments, limit, plan: name + ": row"),
    column(arguments, limit, plan: name + ": column"),
  ))
  list-frame(
    row((atom(name, kind: "operator"), argument-layout), limit, plan: ()),
    limit,
  )
}

#let parameter-list(names, limit) = list-frame(
  row(names.map(name => atom(name)), limit, plan: ()),
  limit,
)

// LAMBDA is semantically a head followed by body rows; this is not offered as
// a horizontal/vertical stylistic choice.
#let lambda-form(limit) = {
  let condition = call("item-body-p", (atom("item"),), limit)
  let consequent = call("render-item", (atom("layout"), atom("list"), atom("item")), limit)
  let conditional = call("if", (condition, consequent, atom("nil")), limit)
  let head = row((
    atom("lambda", kind: "operator"),
    parameter-list(("item",), limit),
  ), limit, plan: ())
  list-frame(
    column((head, conditional), limit, plan: ()),
    limit,
  )
}

#let example(limit) = call(
  "every",
  (lambda-form(limit), atom("items")),
  limit,
)

#let metric(length) = str(calc.round(length / 1mm, digits: 1)) + " mm"

#let summary(candidate) = [
  #text(weight: 600, fill: muted)[#metric(candidate.width) × #metric(candidate.height)]
  #h(1em)
  #text(fill: muted)[cost #candidate.cost]
]

#let check-frontiers() = {
  let narrow = example(62mm)
  let medium = example(95mm)
  let wide = example(145mm)
  let narrow-best = best(narrow, 62mm)
  let medium-best = best(medium, 95mm)
  let wide-best = best(wide, 145mm)

  assert(narrow.len() == 2, message: "expected two narrow candidates")
  assert(medium.len() == 4, message: "expected four medium candidates")
  assert(wide.len() == 7, message: "expected seven wide candidates")
  assert(not any(medium, candidate => candidate.width > 95mm))
  assert(not any(medium, candidate => better(candidate, medium-best)))
  assert(narrow-best.height > medium-best.height)
  assert(medium-best.height > wide-best.height)

  (
    candidates: (narrow.len(), medium.len(), wide.len()),
    medium-plans: medium.map(candidate => candidate.plan),
    winner-width-mm: (
      narrow-best.width / 1mm,
      medium-best.width / 1mm,
      wide-best.width / 1mm,
    ),
    winner-height-mm: (
      narrow-best.height / 1mm,
      medium-best.height / 1mm,
      wide-best.height / 1mm,
    ),
  )
}

#let specimen(limit) = {
  let frontier = example(limit)
  let winner = best(frontier, limit)
  block(
    width: limit,
    inset: 2.5mm,
    stroke: 0.6pt + rule,
    radius: 2mm,
    [
      #label("available width " + metric(limit) + " · frontier " + str(frontier.len()))
      #winner.body
      #v(2mm)
      #summary(winner)
      #v(1mm)
      #text(size: 6.8pt, fill: muted, winner.plan.join(" · "))
    ],
  )
}

#title("A tiny non-greedy DEXP", "Typst · leaf-measured analytic frontier · row or column")

#note([Every proportional syntax token is measured once as a top-baseline leaf. Nested rows, columns, and frames trust their analytic bounding boxes and never call `measure`. Calls offer exactly two argument arrangements: all in one row or all in one column. Pareto pruning retains narrower/taller and wider/shorter possibilities through nesting; the final choice minimizes physical height within the supplied width.])

#v(5mm)

#context {
  [#metadata(check-frontiers()) <prty-stats>]
  grid(
    columns: (1fr, 1fr),
    column-gutter: 7mm,
    row-gutter: 5mm,
    align(left, specimen(62mm)),
    align(right, specimen(95mm)),
    grid.cell(colspan: 2, align(center, specimen(145mm))),
  )
}

#pagebreak()

#title("The retained frontier", "same source · 95 mm budget")

#context {
  let limit = 95mm
  let frontier = example(limit)
  let winner = best(frontier, limit)

  note([The candidates below are not line-break partitions. They arise only from nested row/column choices. A candidate survives when no other candidate is simultaneously narrower, shorter, and cheaper. The selected candidate is outlined in red.])
  v(5mm)

  grid(
    columns: (1fr, 1fr),
    column-gutter: 8mm,
    row-gutter: 6mm,
    ..frontier.map(candidate => block(
      inset: 3mm,
      stroke: if candidate == winner { 1pt + accent } else { 0.6pt + rule },
      radius: 2mm,
      [
        #candidate.body
        #v(2mm)
        #summary(candidate)
        #v(1mm)
        #text(size: 6.5pt, fill: muted, candidate.plan.join(" · "))
      ],
    )),
  )
}
