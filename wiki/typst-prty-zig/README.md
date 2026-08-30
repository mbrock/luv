# Typst DEXP rectangle planner

Typst shapes and measures proportional token boxes, then sends their physical
dimensions and the semantic Lisp layout tree to this Zig WebAssembly plugin.
The planner computes composite bounds analytically, retains at most eight
Pareto candidates after each composition step, and returns one resolved layout
recipe. Typst materializes only that recipe as rows, columns, and framed lists.

This follows Zoot's useful separation between measures, compact resolved plan
nodes, and final emission, but not its line-printer geometry. DEXPs need a real
two-dimensional `(width, height)` frontier, so Zoot's current-column context and
two-candidate frontier do not transfer.

Build and test from the repository root:

```sh
make typst-prty-test
make typst-prty-plugin
```

The generated plugin is
`build/typst-prty-zig/bin/typst-prty.wasm`. `scripts/wiki pdf` rebuilds it when
the Zig source is newer.
