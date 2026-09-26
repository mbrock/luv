# Captures, showcase publication, and wiki tools.

.PHONY: capture showcase-bootstrap showcase-render showcase-deploy showcase-publish \
	showcase-status readme-screenshots mcclim-gallery wiki-cli typst-prty-plugin \
	typst-prty-test wiki

capture:
	./scripts/captures render

showcase-bootstrap showcase-render showcase-deploy showcase-publish showcase-status:
	./scripts/showcase $(patsubst showcase-%,%,$@)

readme-screenshots:
	sbcl --script scripts/readme-screenshots.lisp screenshots

mcclim-gallery:
	sbcl --script scripts/mcclim-gallery.lisp build/mcclim-gallery

wiki-cli:
	sbcl --script wiki/build.lisp

typst-prty-plugin:
	./scripts/build-typst-prty-plugin

typst-prty-test:
	@zig build --build-file wiki/typst-prty-zig/build.zig \
		--cache-dir build/typst-prty-zig/cache \
		--prefix build/typst-prty-zig test

wiki:
	./scripts/wiki build

