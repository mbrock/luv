#let ink = rgb("#202124")
#let muted = rgb("#62666d")
#let accent = rgb("#4b6488")
#let panel = rgb("#f2f0ea")

#let example(body) = block(
  width: 100%,
  fill: panel,
  inset: 10pt,
  radius: 3pt,
  breakable: false,
  raw(body, block: true),
)

#let workshop(title: none, body) = {
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
  set text(font: "Libertinus Serif", size: 10.5pt, lang: "en", fill: ink)
  set par(justify: true, leading: 0.68em)
  set list(indent: 1.2em, body-indent: 0.55em)
  set enum(indent: 1.2em, body-indent: 0.55em)
  set table(stroke: (x: none, y: 0.35pt + rgb("#c9c7c0")), inset: 5pt)
  show raw: set text(font: "Monaspace Neon", size: 7.7pt,
                     features: ("calt": 0, "liga": 0))

  show heading.where(level: 1): it => block(above: 2.2em, below: 0.75em, sticky: true)[
    #set text(font: "Iosevka Aile", size: 17pt, weight: "bold", fill: ink,
              hyphenate: false)
    #it.body
  ]
  show heading.where(level: 2): it => block(above: 1.7em, below: 0.6em, sticky: true)[
    #set text(font: "Iosevka Aile", size: 13pt, weight: "bold", fill: ink,
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
      #text(font: "Iosevka Aile", size: 28pt, weight: "bold", fill: ink)[#title]
      #v(8mm)
      #line(length: 28mm, stroke: 1.2pt + accent)
      #v(1fr)
    ]
  ]
  pagebreak()
  counter(page).update(1)
  body
}
