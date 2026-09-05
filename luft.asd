(in-package #:asdf-user)

(defsystem "luft"
  :description "Canonical cubical topology and integer manifold-sheet meshing."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("sb-simd")
  :serial t
  :components ((:file "luft/luft")
               (:file "luft/fibers")
               (:file "luft/voxel-light")
               (:file "luft/mesh")
               (:file "luft/star-geometry")
               (:file "luft/star-atlas")
               (:file "luft/star-table")
               (:file "luft/star-selection")
               #+x86-64 (:file "luft/star-selection-avx512")
               (:file "luft/star-selection-simd")
               (:file "luft/mesh-query"))
  :in-order-to ((test-op (test-op "luft/test"))))

(defsystem "luft/test-support"
  :description "LUFT's topology test claims and shared mesh diagnostics."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luft")
  :components ((:file "luft/tests")))

(defsystem "luft/atlas"
  :description "A static browser atlas of LUFT's 256 width-one occupancy stars."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luft" "luv-wiki" "spinneret" "parenscript")
  :serial t
  :components ((:file "luft/atlas-package")
               (:file "luft/atlas-style")
               (:file "luft/atlas-script")
               (:file "luft/atlas"))
  :in-order-to ((test-op (test-op "luft/atlas/test"))))

(defsystem "luft/atlas/test"
  :description "Executable claims for the static width-one star atlas."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luft/atlas" "luv/test-support")
  :components ((:file "luft/atlas-tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:luv.test-support '#:test-package
                               '#:luft.atlas.tests)))

(defsystem "luft/test"
  :description "Executable claims for LUFT topology and manifold-sheet meshing."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luft/test-support")
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:luft '#:run-luft-tests)))

(defsystem "luft/renderer"
  :description "The indexed integer-mesh GPU renderer for LUFT solids."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luft" "luv" "luv/production")
  :serial t
  :components ((:module "render"
                :pathname "luft/render"
                :serial t
                :components ((:file "package")
                             (:file "quantities")
                             (:file "frame-quantities")
                             (:file "materials")
                             (:file "lighting")
                             (:file "shaders")
                             (:file "gpu-resources")
                             (:file "drawing-program")
                             (:file "scene-drawing")
                             (:file "player")
                             (:file "sky")
                             (:file "exposure")
                             (:file "torch-frame")
                             (:file "torch-body")
                             (:file "torch-reference")
                             (:file "torch-shaders")
                             (:file "shader-manifest")
                             (:file "torch-drawing")
                             (:file "scene")
                             (:file "decoration")
                             (:file "render")
                             (:file "frame")
                             (:file "world")
                             (:file "streaming-state")
                             (:file "streaming")
                             (:file "streaming-mesh")
                             (:file "streaming-publication")
                             (:file "fixtures")))))

(defsystem "luft/simulation"
  :description "Bodies, character controllers, and the LUFT world clock without an atelier."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luft/renderer")
  :components ((:file "luft/render/simulation")))

(defsystem "luft/render"
  :description "The McCLIM LUFT atelier over the packed-site renderer."
  :version "0.0.1"
  :author "Mikael Brockman"
  ;; Share Luv's substrate and compositor, not another game's runtime.
  :depends-on ("luft/simulation" "luv/mcclim" "luv/workbench"
               "luv/lobby/mcclim"
               "luv/tracy-capture" "luv/application-agent")
  :serial t
  :components ((:file "luft/render/studio")
               (:file "luft/render/live-artifact")
               (:file "luft/render/workbench")
               (:file "luft/render/tracy-capture")
               (:file "luft/render/command-menu")
               (:file "luft/render/source-update")
               (:file "luft/render/metabar")
               (:file "luft/render/lobby")
               (:file "luft/render/status-bar")
               (:file "luft/render/application-agent"))
  :in-order-to ((test-op (test-op "luft/render/test"))))

(defsystem "luft/program"
  :description "The standalone LUFT atelier executable."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luft/render")
  :serial t
  :components ((:file "luft/render/main"))
  :build-operation "program-op"
  ;; BUILD/LUFT is the atelier's long-lived image/output directory. Keep the
  ;; application beside it rather than making the two meanings fight over one
  ;; pathname.
  :build-pathname "build/luft-atelier"
  :entry-point "luft.render:main")

(defsystem "luft/render/test"
  :description "Executable claims for the 256-star mesh renderer."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luft/render" "luft/test-support" "luv/test-support")
  :serial t
  :components ((:file "luft/render/star-tests")
               (:file "luft/render/gpu-test-support")
               (:file "luft/render/drawing-program-tests")
               (:file "luft/render/exposure-tests")
               (:file "luft/render/scene-drawing-tests")
               (:file "luft/render/streaming-publication-tests")
               (:file "luft/render/static-scene-tests")
               (:file "luft/render/torch-drawing-tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:luv.test-support '#:test-package
                               '#:luft.render.tests)))
