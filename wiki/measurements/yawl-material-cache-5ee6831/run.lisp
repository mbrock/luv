;;;; See README.org; only load in an isolated process.
(require :asdf)
(assert (uiop:getenv "INTERLEAVED_LIBRARY"))
(load "wiki/measurements/yawl-runtime-f87a44f/interleaved.lisp")
(load "wiki/measurements/yawl-material-cache-5ee6831/material-cache.lisp")
(when (uiop:getenv "CACHE_INSPECT")
  (load "wiki/measurements/yawl-material-cache-5ee6831/inspect.lisp"))
(gpu-fps::main)
(sb-ext:exit :code 0 :abort t)
