// A tiny eager Pareto-frontier layout algebra for Typst, after Wisp's
// core/sexp-prty.zig and Bernardy's "A Pretty But Not Greedy Printer".
//
// This version works with measured proportional Typst content.  A document is
// an array of candidate rectangles; every candidate retains its concrete
// content, as the Wisp implementation retains concrete lines.  Call these
// functions from a `context` block so `measure` can see the active text style.
// Every candidate gets a top baseline.  With that invariant, width, height,
// and monotone cost are sufficient for dominance under the combinators below:
// rows sum widths and take maximum height; columns do the converse; frames add
// fixed extents.

#let layout(body, cost: 0, plan: ()) = {
  let size = measure(body)
  (
    body: body,
    width: size.width,
    height: size.height,
    cost: cost,
    plan: plan,
  )
}

#let leaf(body, cost: 0, plan: ()) = (
  layout(box(baseline: top, body), cost: cost, plan: plan),
)

#let dominates(a, b) = {
  (a.width <= b.width) and (a.height <= b.height) and (a.cost <= b.cost)
}

#let any(candidates, predicate) = {
  let found = false
  for candidate in candidates {
    if predicate(candidate) {
      found = true
    }
  }
  found
}

#let max-frontier = 8

#let bound-frontier(candidates) = {
  if candidates.len() <= max-frontier {
    candidates
  } else {
    let ordered = candidates.sorted(key: candidate => candidate.width)
    range(max-frontier).map(index => ordered.at(calc.floor(
      index * (ordered.len() - 1) / (max-frontier - 1),
    )))
  }
}

#let pareto(candidates) = {
  let kept = ()
  for candidate in candidates {
    if not any(kept, other => dominates(other, candidate)) {
      kept = kept.filter(other => not dominates(candidate, other))
      kept.push(candidate)
    }
  }
  bound-frontier(kept)
}

#let within-limit(candidates, limit) = {
  let fitting = candidates.filter(candidate => candidate.width <= limit)
  if fitting.len() == 0 { candidates } else { fitting }
}

#let choose(documents) = {
  let candidates = ()
  for document in documents {
    candidates += document
  }
  pareto(candidates)
}

#let child-plans(candidates) = {
  let plans = ()
  for candidate in candidates {
    let child = candidate.at("plan", default: ())
    if child != none {
      plans += child
    }
  }
  plans
}

#let own-plan(plan) = {
  if type(plan) == str {
    if plan == "" { () } else { (plan,) }
  } else {
    plan
  }
}

#let combined-plan(plan, children) = {
  own-plan(plan) + child-plans(children)
}

#let composition-state(candidate) = (
  body: candidate.body,
  width: candidate.width,
  height: candidate.height,
  cost: candidate.cost,
  plan: candidate.plan,
  parts: (candidate.body,),
)

// ROW and COLUMN are intentionally all-or-nothing.  There is no fill-style
// mixed line breaking in this experiment.  Each composition prunes its partial
// frontier before adding the next child; materializing the full Cartesian
// product makes ordinary binding lists exponentially large.  Partial states
// carry rectangle arithmetic and flat child-body arrays; only the surviving
// completed candidates become Typst boxes and call `measure`.  The retained
// frontier is sampled at eight widths: bounded optimality in exchange for
// predictable compilation rather than exponential memory.
#let row(documents, limit, gap: 0.48em, cost: 0, plan: "row") = {
  let gap-width = measure(box(width: gap)).width
  let initial = if documents.len() == 0 { leaf(box(), plan: ()) } else { documents.first() }
  let candidates = initial.map(composition-state)
  if documents.len() > 1 {
    for document in documents.slice(1) {
      let next = ()
      for left in candidates {
        for right in document {
          let parts = left.parts + (right.body,)
          next.push((
            body: none,
            width: left.width + gap-width + right.width,
            height: calc.max(left.height, right.height),
            cost: left.cost + right.cost,
            plan: left.plan + right.plan,
            parts: parts,
          ))
        }
      }
      candidates = pareto(within-limit(next, limit))
    }
  }
  let result = ()
  for child in candidates {
    result.push(layout(
      box(baseline: top, child.parts.join(h(gap))),
      cost: child.cost + cost,
      plan: own-plan(plan) + child.plan,
    ))
  }
  pareto(within-limit(result, limit))
}

#let column(documents, limit, gap: 0.14em, cost: 0, plan: "column") = {
  let gap-height = measure(box(height: gap)).height
  let initial = if documents.len() == 0 { leaf(box(), plan: ()) } else { documents.first() }
  let candidates = initial.map(composition-state)
  if documents.len() > 1 {
    for document in documents.slice(1) {
      let next = ()
      for above in candidates {
        for below in document {
          let parts = above.parts + (below.body,)
          next.push((
            body: none,
            width: calc.max(above.width, below.width),
            height: above.height + gap-height + below.height,
            cost: above.cost + below.cost,
            plan: above.plan + below.plan,
            parts: parts,
          ))
        }
      }
      candidates = pareto(within-limit(next, limit))
    }
  }
  let result = ()
  for child in candidates {
    result.push(layout(
      box(
        baseline: top,
        grid(
          columns: 1,
          row-gutter: gap,
          ..child.parts,
        ),
      ),
      cost: child.cost + cost,
      plan: own-plan(plan) + child.plan,
    ))
  }
  pareto(within-limit(result, limit))
}

#let frame(
  document,
  limit,
  stroke: none,
  radius: 0pt,
  inset: 0pt,
  cost: 0,
  plan: (),
) = {
  let candidates = ()
  for child in document {
    let body = box(
      baseline: top,
      stroke: stroke,
      radius: radius,
      inset: inset,
      child.body,
    )
    let candidate = layout(
      body,
      cost: child.cost + cost,
      plan: combined-plan(plan, (child,)),
    )
    candidates.push(candidate)
  }
  pareto(within-limit(candidates, limit))
}

#let better(a, b) = {
  if a.height != b.height {
    a.height < b.height
  } else if a.cost != b.cost {
    a.cost < b.cost
  } else {
    a.width < b.width
  }
}

#let best(document, limit) = {
  let candidates = document.filter(candidate => candidate.width <= limit)
  assert(candidates.len() > 0, message: "no layout fits the supplied width")
  let winner = candidates.first()
  for candidate in candidates.slice(1) {
    if better(candidate, winner) {
      winner = candidate
    }
  }
  winner
}
