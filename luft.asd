(in-package #:asdf-user)

(defsystem "luft"
  :description "Canonical cubical topology and integer manifold-sheet meshing."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("sb-simd")
  :serial t
  :components ((:file "luft/luft")
               (:file "luft/voxel-light")
               (:static-file "luft/blender-arc-stars.sexp")
               (:file "luft/mesh")
               (:file "luft/mesh-query")
               (:file "luft/star-geometry")
               (:file "luft/mesh-realization")
               (:file "luft/mesh-variation-policy")
               (:file "luft/mesh-variation-plan")
               (:file "luft/mesh-variation-emit")
               (:file "luft/mesh-variation")
               (:file "luft/mesh-compression"))
  :in-order-to ((test-op (test-op "luft/test"))))

(defsystem "luft/test-support"
  :description "LUFT's topology test claims and shared mesh diagnostics."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luft")
  :components ((:file "luft/tests")))

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
                             (:file "flame-shaders")
                             (:file "render")))))

(defsystem "luft/render"
  :description "The McCLIM LUFT atelier over the packed-site renderer."
  :version "0.0.1"
  :author "Mikael Brockman"
  ;; The atelier shares Luv's McCLIM backend directly.  Luvcraft remains only
  ;; for the small physics vocabulary used by RENDER/GAME; LUFT no longer
  ;; loads another game's widget adapters to obtain the common compositor.
  :depends-on ("luft/renderer" "luv/mcclim" "luv/lobby/mcclim"
               "luv/tracy-capture" "luv/application-agent"
               "luvcraft/core")
  :serial t
  :components ((:file "luft/render/game")
               (:file "luft/render/studio")
               (:file "luft/render/live-artifact")
               (:file "luft/render/instruments")
               (:file "luft/render/tracy-capture")
               (:file "luft/render/command-menu")
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
  :description "Executable claims for the indexed integer-mesh renderer."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luft/render" "luft/test-support" "luv/test-support")
  :serial t
  :components ((:file "luft/render/tests")
               (:file "luft/render/quantity-tests")
               (:file "luft/render/frame-quantity-tests")
               (:file "luft/render/flame-quantity-tests")
               (:file "luft/render/instrument-tests")
               (:file "luft/render/tracy-capture-tests")
               (:file "luft/render/live-artifact-tests")
               (:file "luft/render/metabar-tests")
               (:file "luft/render/lobby-tests")
               (:file "luft/render/status-bar-tests")
               (:file "luft/render/application-agent-tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:luv.test-support '#:test-package
                               '#:luft.render.tests)))

(defsystem "luft/mesh-query-profile"
  :description "Stage-isolated statistical profiles of the LUFT mesh query."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luft/renderer" "sb-sprof")
  :components ((:file "luft/mesh-query-profile")))

(defsystem "luft/z-fiber-benchmark"
  :description "Scalar and native-SIMD experiments for full-height LUFT fibers."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luft" "luv" "sb-simd")
  :components ((:file "luft/z-fiber-benchmark"))
  :in-order-to ((test-op (test-op "luft/z-fiber-benchmark/test"))))

(defsystem "luft/z-fiber-benchmark/test"
  :description "Differential checks for LUFT Z-fiber benchmark kernels."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luft/z-fiber-benchmark" "luv/test-support")
  :components ((:file "luft/z-fiber-benchmark-tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:luv.test-support '#:test-package
                               '#:luft.z-fiber-benchmark.tests)))
