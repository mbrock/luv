#import "@preview/cetz:0.5.2": canvas, draw

#let ink = rgb("#27221d")
#let quiet = rgb("#8f877c")
#let occupancy-color = rgb("#a94b62")

#let owner-color(index) = color.hsl(index * 137deg, 34%, 50%)

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
#let move-point(point, offset) = range(3).map(i => point.at(i) + offset.at(i))
#let site-ticks(site) = site.map(value => value * 8)
#let explode-offset(site, center, amount: .62) = range(3).map(
  i => (site-ticks(site).at(i) - center.at(i)) * amount,
)

#let polygon-item(points, fill, stroke) = (
  points: points,
  fill: fill,
  stroke: (paint: stroke, thickness: .32pt),
)

#let occupancy-items(region) = region.occupancy.map(points => polygon-item(
  points,
  occupancy-color.transparentize(74%),
  occupancy-color.darken(22%),
))

#let owned-items(region, exploded: false, alpha: 20%) = region.sites.fold(
  (),
  (answer, site) => {
    let color = owner-color(site.index)
    let offset = if exploded { explode-offset(site.site, region.center) } else { (0, 0, 0) }
    answer + site.triangles.map(points => polygon-item(
      points.map(point => move-point(point, offset)),
      color.transparentize(alpha),
      color.darken(30%),
    ))
  },
)

#let render(items, length: 12mm, sites: none, center: none) = canvas(
  length: length,
  padding: 2pt,
  {
    import draw: line, circle
    for item in items.sorted(key: it => depth(it.points)) {
      line(
        ..item.points.map(point => project(point).slice(0, 2)),
        close: true,
        fill: item.fill,
        stroke: item.stroke,
      )
    }
    if sites != none {
      for site in sites {
        let offset = explode-offset(site.site, center)
        let point = move-point(site-ticks(site.site), offset)
        circle(
          project(point).slice(0, 2),
          radius: 1.05pt,
          fill: owner-color(site.index),
          stroke: .25pt + ink,
        )
      }
    }
  },
)

#let panel-title(number, title, note) = [
  #set text(size: 7.8pt)
  #number #h(3pt) #smallcaps[#title] #h(1fr)
  #text(size: 6.5pt, fill: quiet)[#note]
  #v(2pt)
  #line(length: 100%, stroke: .35pt + quiet.transparentize(45%))
]

#let region-plate(region, width: 168mm) = {
  set text(size: 8.5pt, fill: ink)
  block(
    width: width,
    breakable: false,
    grid(
      columns: (0.8fr, 1.25fr, 1fr),
      column-gutter: 7mm,
      [
        #panel-title("I", "occupied cells", [#region.cells.len() voxels])
        #align(center, render(occupancy-items(region), length: 14mm))
        #v(3pt)
        #text(size: 7pt, fill: quiet)[The compact volumetric fact. Internal cube faces are absent.]
      ],
      [
        #panel-title("II", "owned patches", [#region.sites.len() lattice sites])
        #align(center, render(
          owned-items(region, exploded: true),
          length: 10mm,
          sites: region.sites,
          center: region.center,
        ))
        #v(3pt)
        #text(size: 7pt, fill: quiet)[Each colored patch moves away from its owner; dots mark the lattice sites.]
      ],
      [
        #panel-title("III", "composed surface", [#region.triangle-count triangles])
        #align(center, render(owned-items(region, alpha: 38%), length: 14mm))
        #v(3pt)
        #text(size: 7pt, fill: quiet)[The same owner colors, with every patch returned to global mesh coordinates.]
      ],
    ),
  )
}
