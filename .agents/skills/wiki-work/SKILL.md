---
name: wiki-work
description: "Use when editing luv's Org wiki, updating roadmap or work-tracking notes, adding cross-references, turning implementation evidence into design memory, or choosing the next step from wiki pages. Captures the wiki paradigm: figures with stable IDs, light inline references, and tiny work marks for roadmap state."
---

# Wiki work in luv

Use the wiki as living design memory, not as an external tracker or frozen
specification. Before editing roadmap/design pages, read `wiki/index.org`,
especially "A workshop, not an authority", "String figures", and "Work marks";
then read the relevant subject page and compare it with current code/tests.

## Figures

Keep durable claims addressable as Org headings with a stable six-character
`ID` property. Files gather figures by subject; IDs are the durable handles.

Use plain `#ABC123` mentions when prose only needs a lightweight reference.
Use Org `[[id:ABC123]]` links when a clickable link is more useful. Do not
renumber or replace an ID because a heading moved or was revised.

When adding a figure:

- choose a short all-caps/digit ID that is not already present;
- keep the heading title readable without the ID;
- place the figure near the design it explains; and
- update `wiki/index.org` only when the page becomes a new path into the
  material.

## Work marks

A work mark is a figure that also tracks one coherent iteration. It is an Org
heading whose title starts with one of these status words:

- `NEXT`: current best small bet.
- `TODO`: visible and likely, but not selected yet.
- `WAIT`: blocked by outside evidence or another completed step.
- `DONE`: closed with evidence.
- `IDEA`: tempting but not allowed to steer implementation yet.

Give each work mark:

- `Intent`: why the work exists and what shape of change it wants.
- `Evidence`: code, tests, captures, commits, or observations that justify the
  status.
- `Done when`: concrete acceptance criteria.

One work mark should be small enough for an exploratory commit or two. If it
needs a nested plan, split it into several work marks or move details into the
subject prose. Keep status changes honest: update evidence when moving a mark
to `DONE`, and prefer `IDEA` over pretending a speculative branch is roadmap.

## Editing workflow

1. Refresh orientation: check `git status`, skim `wiki/index.org`, then read
   the relevant wiki page and nearby code/tests.
2. Separate voices: distinguish implemented facts, design interpretations,
   and questions for evidence.
3. Preserve the front-door shape: do not move deep workflow or design detail
   into `README.md` when the wiki or `AGENTS.md` is the better home.
4. Update nearby work marks when implementation changes the roadmap. Avoid
   creating a disconnected status page.
5. Run cheap validation: `rg` for stale phrases, `git diff --check`, and
   tests only when the doc edit claims code behavior that was not just
   verified.

For Lisp architecture work that changes protocols or dispatch, also use the
`clos-design` skill before proposing or editing code.
