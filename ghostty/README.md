# libghostty-vt CFFI spike

This experimental `luv/ghostty` ASDF system binds enough of libghostty-vt to
create a terminal, feed its VT parser, format the active screen as plain text,
and release all native resources explicitly.

Build libghostty-vt, point luv at the shared library, and run the focused test:

```sh
cd ~/src/ghostty
nix develop -c zig build -Demit-lib-vt=true -Doptimize=ReleaseSafe

cd /path/to/luv
LUV_GHOSTTY_LIBRARY="$HOME/src/ghostty/zig-out/lib/libghostty-vt.so" \
  ./scripts/dev sbcl --non-interactive \
  --eval '(require :asdf)' \
  --eval '(asdf:load-asd (truename "luv.asd"))' \
  --eval '(asdf:test-system :luv/ghostty)'
```

From Lisp:

```lisp
(asdf:load-system :luv/ghostty)
(ghostty:with-terminal (terminal :columns 80 :rows 24)
  (ghostty:write-terminal terminal "Hello from Ghostty!")
  (ghostty:terminal-text terminal))
```
