// A tiny eager Pareto-frontier layout algebra for Typst, after Wisp's
// core/sexp-prty.zig and Bernardy's "A Pretty But Not Greedy Printer".
//
// This version measures only proportional leaf content.  Every composite
// trusts the rectangle algebra: rows sum widths and take maximum height;
// columns do the converse; frames add explicit insets.  A document is an array
// of candidate rectangles retaining concrete content, as the Wisp
// implementation retains concrete lines.  Call `leaf` from a `context` block
// so `measure` can see the active text style.  Every body has a top baseline.

#let leaf(body, cost: 0, plan: ()) = {
  let body = box(baseline: top, body)
  let size = measure(body)
  ((
    body: body,
    width: size.width,
    height: size.height,
    cost: cost,
    plan: plan,
  ),)
}

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

#let empty-document = ((
  body: box(baseline: top),
  width: 0pt,
  height: 0pt,
  cost: 0,
  plan: (),
),)

// ROW and COLUMN are intentionally all-or-nothing.  There is no fill-style
// mixed line breaking in this experiment.  Each composition prunes its partial
// frontier before adding the next child; materializing the full Cartesian
// product makes ordinary binding lists exponentially large.  Partial states
// carry rectangle arithmetic and flat child-body arrays.  Composite bodies
// are never measured.  Gaps and frame insets must therefore be absolute
// lengths already resolved for the active typography.  The retained frontier
// is sampled at eight widths: bounded optimality in exchange for predictable
// compilation rather than exponential memory.
#let row(documents, limit, gap: 3.984pt, cost: 0, plan: "row") = {
  let initial = if documents.len() == 0 { empty-document } else { documents.first() }
  let candidates = initial.map(composition-state)
  if documents.len() > 1 {
    for document in documents.slice(1) {
      let next = ()
      for left in candidates {
        for right in document {
          let parts = left.parts + (right.body,)
          next.push((
            body: none,
            width: left.width + gap + right.width,
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
    result.push((
      body: box(baseline: top, child.parts.join(h(gap))),
      width: child.width,
      height: child.height,
      cost: child.cost + cost,
      plan: own-plan(plan) + child.plan,
    ))
  }
  pareto(within-limit(result, limit))
}

#let column(documents, limit, gap: 1.162pt, cost: 0, plan: "column") = {
  let initial = if documents.len() == 0 { empty-document } else { documents.first() }
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
            height: above.height + gap + below.height,
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
    result.push((
      body: box(
        baseline: top,
        grid(
          columns: 1,
          row-gutter: gap,
          ..child.parts,
        ),
      ),
      width: child.width,
      height: child.height,
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
  inset-x: 0pt,
  inset-y: 0pt,
  cost: 0,
  plan: (),
) = {
  let candidates = ()
  for child in document {
    let body = box(
      baseline: top,
      stroke: stroke,
      radius: radius,
      inset: (x: inset-x, y: inset-y),
      child.body,
    )
    let candidate = (
      body: body,
      width: child.width + 2 * inset-x,
      height: child.height + 2 * inset-y,
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
