# libghostty-vt CFFI spike

This experimental `luv/ghostty` ASDF system binds enough of libghostty-vt to
create a terminal, feed its VT parser, format the active screen as plain text,
encode physical key events against its live terminal modes, and release all
native resources explicitly.

The luv Nix environment pins and builds Ghostty's maintained
`libghostty-vt-releasesafe` derivation. Enter it through the usual development
wrapper and run the focused test:

```sh
cd /path/to/luv
./scripts/dev sbcl --non-interactive \
  --eval '(require :asdf)' \
  --eval '(asdf:load-asd (truename "luv.asd"))' \
  --eval '(asdf:test-system :luv/ghostty)'
```

The package is also available as `.#libghostty-vt`. The environment exports
`LUV_GHOSTTY_LIBRARY` as its exact shared-library store path, while ordinary
installed soname lookup remains available outside Nix.

From Lisp:

```lisp
(asdf:load-system :luv/ghostty)
(ghostty:with-terminal (terminal :columns 80 :rows 24)
  (ghostty:write-terminal terminal "Hello from Ghostty!")
  (ghostty:terminal-text terminal))
```

The reusable key encoder stays separate from the terminal's ownership:

```lisp
(ghostty:with-terminal (terminal)
  (ghostty:with-key-encoder (encoder)
    (ghostty:encode-key-event
     encoder terminal :press :a
     :modifiers '(:control) :text "a" :unshifted-codepoint 97)))
```
