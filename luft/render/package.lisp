(defpackage #:luft.render.shaders
  (:use #:cl #:luv.shader)
  (:shadowing-import-from #:luv.shader #:step)
  (:documentation
   "The face-record vertex stage and its fragment stage.

The vertex shader is a transliteration of LUFT:REALIZE-FACE-PATCH: it
realizes one of the sixteen patch points of one exposed face from the
oriented face site, the 32-bit shape word, and the chamfer width alone.
No occupancy is bound; every classification was authored on the CPU into
the shape word.")
  (:export #:+frame-binding+
           #:+faces-binding+
           #:+template-binding+
           #:*frame-uniform-members*
           #:+patch-vertices-per-face+
           #:patch-vertex-shader
           #:patch-fragment-shader))

(defpackage #:luft.render
  (:use #:cl #:luv)
  (:local-nicknames (#:shaders #:luft.render.shaders)
                    (#:vec3 #:luv.arithmetic.lisp.vec3))
  (:documentation
   "A greenfield atelier drawing dense face records.

The CPU end is LUFT:MATERIALIZE-SURFACE: occupancy -> solid 3-chain ->
oriented surface 2-chain -> per-face shape words -> one 16-byte record per
exposed face.  This package uploads those records and draws them.")
  (:export #:make-atelier-world
           #:fill-box
           #:make-fly-camera
           #:camera-position
           #:camera-yaw
           #:camera-pitch
           #:make-renderer
           #:destroy-renderer
           #:renderer-camera
           #:renderer-world
           #:upload-world
           #:render-to-png
           #:capture-atelier-png
           #:start-viewer
           #:stop-viewer
           #:viewer-renderer
           #:*viewer*
           #:main))
