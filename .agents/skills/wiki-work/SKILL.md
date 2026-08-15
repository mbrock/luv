---
name: wiki-work
description: "Use when reading or editing luv's Org wiki (wiki/*.org), tracking work with work marks, cross-referencing figures from prose or code, adding pages or images, or checking the rendered site. Covers the wiki paradigm (figures with stable IDs, light #ID mentions, tiny work marks), what makes pages read well on the web at mbrock.github.io/luv, the code-to-wiki links, and the scripts/wiki tool."
---

# Wiki work in luv

The wiki in `wiki/*.org` is the project's living design memory: the place
where understanding, decisions, evidence, and the next small step are kept
in prose. It is also a website (https://mbrock.github.io/luv/, rebuilt from
`main` on every push) that renders the pages, every figure's backlinks, the
work marks, and the entire source of the luv systems as browsable "dexp"
pages. Write for that reader: a human, often on a phone, following a link
from a figure into code and back.

Read `wiki/index.org` first ("A workshop, not an authority", "String
figures", "Work marks"), and `wiki/wiki-site.org` when the tooling itself is
in question.

## Getting oriented: scripts/wiki

`scripts/wiki` is a small Lisp executable (built on first use with ASDF,
run inside the Nix shell) that reads the same objects the site renders:

```sh
scripts/wiki toc                # every page: figures with IDs, work marks
scripts/wiki toc wiki-site      # one page
scripts/wiki marks              # work marks by status: NEXT TODO WAIT IDEA DONE
scripts/wiki marks next todo    # only some statuses
scripts/wiki figure H7QK3M      # a figure's text, subheadings, backlinks, code refs
scripts/wiki page index         # a whole page as text
scripts/wiki mentions LNRY72    # where a figure is mentioned, in pages and code
scripts/wiki defs mesher        # definitions whose name contains a string
scripts/wiki dangling           # mentions no figure resolves (pages and code)
scripts/wiki ids 6              # fresh figure IDs no page uses
scripts/wiki build              # render the site into build/wiki/
```

Start with `marks` and `toc` before choosing what to work on; use `figure`
rather than reading whole files when you only need one figure; use `defs`
and `mentions` to see how code and prose already connect.

## Figures

The unit of the wiki is the *figure*: an Org heading with a stable
six-character `ID` property. Files gather figures by subject; the ID is the
durable handle. IDs never change when a heading moves or is retitled.

- Get IDs from `scripts/wiki ids`. Never invent one by hand.
- One figure, one idea. A figure should be readable on its own from a link
  (`page.html#ID`): the title says what it claims, the first paragraph
  stands without the rest of the page.
- Titles are short noun phrases or claims ("Destroy is logically immediate
  and physically conditional"), not section labels ("Overview").
- Refer to other figures with the light mention `#ABC123` in prose; use
  `[[id:ABC123]]` only when a clickable word is better than the ID. Both
  render as links with the target's title as tooltip and both count as
  backlinks; a bare `[[id:X]]` renders like `#X`.
- Mention figures generously across pages: the "Mentioned in" lines the site
  shows under each figure are the wiki's map, and they are derived, so
  they cost nothing to keep.
- Keep the three voices distinct: what a source says, what an
  implementation must therefore do, and what luv might choose. Say which
  is which.

## Work marks

A work mark is a figure whose title starts with `NEXT`, `TODO`, `WAIT`,
`DONE`, or `IDEA` (`NEXT` is the current best small bet; `WAIT` needs
outside evidence; `IDEA` may not steer implementation; `DONE` records the
evidence that closed it). Give each mark `Intent`, `Evidence`, and `Done
when` paragraphs, sized to an exploratory commit or two. Place it in the
page whose design it moves, next to that design. When work changes the plan,
update the mark and its evidence there; do not add status pages. Moving a
mark to `DONE` means writing what was observed, not what was intended.

## Code and the wiki

The site reads every source file of the `luv` and `luvcraft` systems (with
Eclector, without loading), so prose and code point at each other:

- A `#ID` in a docstring or comment makes that definition appear under the
  figure's "Referenced from code", expanded in place as dexp boxes with the
  mention linking back. Cite figures from code where the code embodies the
  figure's decision (see `interpret-quantity-specification` and `#PLRP3A`).
- `[[lisp:name]]` in a page links to a definition's source page; a paragraph
  that is only such a link embeds the definition itself. `scripts/wiki defs`
  finds names.
- Docstrings and comments are rendered as prose: paragraphs, lists,
  `=verbatim=`, mentions, and uppercase parameter or definition names as
  linked symbols. Write them as prose. In code prose `*name*` reads as a
  special variable, not bold.
- Prefer prose in the wiki and references in the code: the wiki explains,
  the code cites. Do not duplicate design text into docstrings.

## Writing for the site

The reader supports a small, regular Org subset; keep to it and pages
render well everywhere:

- Paragraphs, `-` and `1.` lists (items may wrap and hold blank lines),
  `#+begin_example`, `#+begin_src lisp` (drawn structurally; other
  languages as text), simple tables with a header rule.
- `*bold*`, `/italic/`, `=verbatim=`, `~code~` with Org's spacing rules.
- Links: `[[file:other.org][text]]` between pages, `[[https://…][text]]`,
  `[[file:../path/to.lisp][text]]` into the repository (GitHub),
  `[[lisp:name]]` to definitions, `[[file:images/x.png]]` alone in a
  paragraph as a figure image (put images in `wiki/images/` and list them
  as `(:static-file "images/x.png")` in `luv.asd`).
- Anything else renders as plain text rather than failing, but avoid it.

Adding a page: create `wiki/name.org` with `#+title:`, add `(:file "name")`
to the `luv/wiki` system in `luv.asd`, and add it to the paths in
`wiki/index.org` if it is a new way into the material.

## Editing workflow

1. Orient: `git status`, `scripts/wiki marks`, `scripts/wiki toc PAGE`,
   then read the figure or page and the nearby code and tests.
2. Edit the Org, keeping figures small and mentions honest; update the
   nearby work mark rather than writing a status page.
3. Check: `scripts/wiki dangling` (must print nothing), `git diff --check`,
   and `make wiki` if the change touches the tooling or you want to look at
   the result (`python3 -m http.server -d build/wiki 8765`).
4. Commit and push; the site redeploys from `main` in about a minute.

For Lisp design work that changes protocols or dispatch, use `clos-design`
as well. For the wiki tooling itself (`wiki-*.lisp`, `luv-wiki.asd`), the
tests are `luv-wiki/tests`; run them with
`(asdf:test-system :luv-wiki)` in the Nix shell.
