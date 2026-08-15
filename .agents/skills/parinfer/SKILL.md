---
name: parinfer
description: Use when checking, repairing, or improving Lisp parenthesis/indentation consistency in luv with `./sly parinfer`; when a parinfer check flags files; when balanced Common Lisp source looks visually wrong; or when changing the parinfer algorithm, command wrapper, tests, or dev-flow integration.
---

# Parinfer

Use parinfer as a tree/layout diagnostic, not as a blind formatter. Its job is
to make suspicious Lisp source cheap to notice and cheap to inspect.

## Commands

- Run the same gate as `make test` with `make parinfer-check`.
- Check one file with `./sly parinfer --strict --check path/to/file.lisp`.
- Inspect the proposed indentation tree with `./sly parinfer --diff path/to/file.lisp`.
- Repair only unbalanced source with `./sly parinfer --write path/to/file.lisp`.

Ordinary `--check` fails for unbalanced files with a validated repair. Strict
`--check` also fails when the reader can parse the file but indentation implies
a different tree. Prefer strict mode for repo audits.

## Working A Finding

1. Run `./sly parinfer --diff FILE` before editing.
2. Classify the finding:
   - If `git diff -w` would change, inspect the parens and surrounding scope.
   - If only whitespace changes, align indentation with the existing tree.
   - If parinfer is wrong, preserve the source and improve
     `parinfer/implementation.lisp` or
     document the false-positive shape here.
3. Avoid parallel `./sly parinfer` runs while collecting evidence. Sequential
   output is easier to trust.
4. Recheck the touched file, then run `make parinfer-check`.

## Common Shapes

- Top-level forms after a blank line, starting at column zero, should usually
  mean the paren depth intended by indentation has returned to zero.
- Binding forms such as `let`, `let*`, `flet`, `labels`, and `macrolet` have a
  binding-list area and then a body area; many false-looking findings are body
  forms indented as if they were still bindings.
- For balanced source, `--write` intentionally refuses suspicious rewrites.
  Use `--diff`, then move exactly the paren or indentation that matches the
  surrounding lexical intent.

## Improving The Tool

Keep parinfer useful by encoding repeated real-world shapes in
`parinfer/implementation.lisp`
instead of normalizing pain into habit. Good improvements are small heuristics
with clear evidence from existing files, plus a repo-wide `make parinfer-check`
run afterward. The tool is allowed to be heuristic, but it should be honest:
when it cannot safely rewrite, it should say so and show a diff rather than
pretending certainty.
