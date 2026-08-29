#import "@preview/cetz:0.5.2": canvas, draw

#let ink = rgb("#27221d")
#let quiet = rgb("#8f877c")
#let occupancy-color = rgb("#a94b62")
#let face-color = rgb("#416f91")
#let band-color = rgb("#c48a34")
#let junction-color = rgb("#a74335")

#let project(point) = {
  let (x, y, z) = point.map(value => value / 8)
  let yaw = 42deg
  let pitch = 27deg
  let across = calc.cos(yaw) * x - calc.sin(yaw) * y
  let away = calc.sin(yaw) * x + calc.cos(yaw) * y
  let up = calc.cos(pitch) * z - calc.sin(pitch) * away
  let depth = calc.sin(pitch) * z + calc.cos(pitch) * away
  (across, up, depth)
}

#let depth(points) = points.map(p => project(p).at(2)).fold(0, (a, b) => a + b) / points.len()

#let item(points, fill, stroke: ink, width: .38pt) = (
  points: points,
  fill: fill,
  stroke: (paint: stroke, thickness: width),
)

#let items(polygons, fill, stroke: ink, width: .38pt) = {
  polygons.map(p => item(p, fill, stroke: stroke, width: width))
}

#let geometry-items(geometry, alpha: 25%, structure: false) = {
  let fill(color) = if structure { none } else { color.transparentize(alpha) }
  let result = items(geometry.faces, fill(face-color), stroke: face-color.darken(28%))
  result += items(geometry.bands, fill(band-color), stroke: band-color.darken(32%))
  result += items(geometry.junctions, fill(junction-color), stroke: junction-color.darken(26%))
  result
}

#let render-items(elements, length: 14mm, diagonals: false) = canvas(
  length: length,
  padding: 2pt,
  {
    import draw: line, circle
    for element in elements.sorted(key: it => depth(it.points)) {
      let points = element.points.map(p => project(p).slice(0, 2))
      line(..points, close: true, fill: element.fill, stroke: element.stroke)
      if diagonals and points.len() == 4 {
        line(
          points.at(0), points.at(2),
          stroke: (
            paint: element.stroke.paint.transparentize(42%),
            thickness: .28pt,
          ),
        )
      }
    }
    circle(project((0, 0, 0)).slice(0, 2), radius: .75pt, fill: ink, stroke: none)
  },
)

#let occupancy-view(star, length: 12mm) = render-items(
  items(
    star.occupancy,
    occupancy-color.transparentize(77%),
    stroke: occupancy-color.darken(20%),
    width: .32pt,
  ),
  length: length,
)

#let atlas-view(star, length: 17mm, structure: false, diagonals: false) = render-items(
  geometry-items(star.whole, alpha: 22%, structure: structure),
  length: length,
  diagonals: diagonals,
)

#let ownership-view(star, length: 17mm) = render-items(
  geometry-items(star.whole, alpha: 91%).map(it => item(
    it.points,
    it.fill,
    stroke: quiet.transparentize(66%),
    width: .26pt,
  )) + geometry-items(star.owned, alpha: 8%),
  length: length,
)

#let mini-title(number, title, note) = [
  #set text(size: 8.2pt)
  #number #h(3pt) #smallcaps[#title] #h(1fr)
  #text(size: 6.8pt, fill: quiet)[#note]
  #v(2pt)
  #line(length: 100%, stroke: .35pt + quiet.transparentize(45%))
]

#let plate(star, width: 160mm) = {
  set text(size: 9pt, fill: ink)
  block(
    width: width,
    grid(
      columns: (1fr, 1.18fr),
      rows: auto,
      column-gutter: 10mm,
      row-gutter: 8mm,
      [
        #mini-title("I", "incident cells", [four of eight])
        #align(center, occupancy-view(star))
        #v(3pt)
        #text(size: 7.2pt, fill: quiet)[The bitmask #raw("00011011") names the occupied cells around one lattice vertex.]
      ],
      [
        #mini-title("II", "the whole patch", [6 faces · 8 bands · 8 junctions])
        #align(center, atlas-view(star))
        #v(3pt)
        #text(size: 7.2pt, fill: quiet)[One local star, before ownership removes overlap with its neighbors.]
      ],
      [
        #mini-title("III", "polygon structure", [quads and triangles])
        #align(center, atlas-view(star, structure: true, diagonals: true))
        #v(3pt)
        #text(size: 7.2pt, fill: quiet)[Faces and bands are quads in the atlas; junctions remain triangles. Faint diagonals show the GPU split.]
      ],
      [
        #mini-title("IV", "one vertex owns", [1 face · 4 bands · 8 junctions])
        #align(center, ownership-view(star))
        #v(3pt)
        #text(size: 7.2pt, fill: quiet)[Opaque pieces are emitted by this star. The complete patch recedes but keeps correspondence visible.]
      ],
    ),
  )
}
