// A tiny eager Pareto-frontier layout algebra for Typst, after Wisp's
// core/sexp-prty.zig and Bernardy's "A Pretty But Not Greedy Printer".
//
// This version works with measured proportional Typst content.  A document is
// an array of candidate rectangles; every candidate retains its concrete
// content, as the Wisp implementation retains concrete lines.  Call these
// functions from a `context` block so `measure` can see the active text style.
// Every candidate gets a bottom baseline.  With that invariant, width, height,
// and monotone cost are sufficient for dominance under the combinators below:
// rows sum widths and take maximum height; columns do the converse; frames add
// fixed extents.

#let layout(body, cost: 0, plan: ()) = {
  let body = box(baseline: bottom, body)
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
  layout(body, cost: cost, plan: plan),
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

#let pareto(candidates) = {
  let kept = ()
  for candidate in candidates {
    if not any(kept, other => dominates(other, candidate)) {
      kept = kept.filter(other => not dominates(candidate, other))
      kept.push(candidate)
    }
  }
  kept
}

#let choose(documents) = {
  let candidates = ()
  for document in documents {
    candidates += document
  }
  pareto(candidates)
}

#let combinations(documents) = {
  let states = ((),)
  for document in documents {
    let next = ()
    for state in states {
      for candidate in document {
        next.push(state + (candidate,))
      }
    }
    states = next
  }
  states
}

#let total-cost(candidates) = {
  let cost = 0
  for candidate in candidates {
    cost += candidate.cost
  }
  cost
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

// ROW and COLUMN are intentionally all-or-nothing.  There is no fill-style
// mixed line breaking in this experiment.
#let row(documents, limit, gap: 0.48em, cost: 0, plan: "row") = {
  let candidates = ()
  for children in combinations(documents) {
    let body = box(children.map(child => child.body).join(h(gap)))
    let candidate = layout(
      body,
      cost: total-cost(children) + cost,
      plan: combined-plan(plan, children),
    )
    if candidate.width <= limit {
      candidates.push(candidate)
    }
  }
  pareto(candidates)
}

#let column(documents, limit, gap: 0.08em, cost: 0, plan: "column") = {
  let candidates = ()
  for children in combinations(documents) {
    let body = box(grid(
      columns: 1,
      row-gutter: gap,
      ..children.map(child => child.body),
    ))
    let candidate = layout(
      body,
      cost: total-cost(children) + cost,
      plan: combined-plan(plan, children),
    )
    if candidate.width <= limit {
      candidates.push(candidate)
    }
  }
  pareto(candidates)
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
    if candidate.width <= limit {
      candidates.push(candidate)
    }
  }
  pareto(candidates)
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
