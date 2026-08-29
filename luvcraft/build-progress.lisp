;;;; Compatibility bootstrap for existing build entry points.

(require :asdf)
(asdf:load-system "luv/build/cli")
