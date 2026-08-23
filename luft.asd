(in-package #:asdf-user)

(defsystem "luft"
  :description "Canonical cubical topology and integer manifold-sheet meshing."
  :version "0.0.1"
  :author "Mikael Brockman"
  :serial t
  :components ((:file "luft/luft")
               (:static-file "luft/blender-arc-stars.sexp")
               (:file "luft/mesh")
               (:file "luft/tests"))
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
                             (:file "materials")
                             (:file "lighting")
                             (:file "shaders")
                             (:file "render")))))

(defsystem "luft/render"
  :description "The McCLIM LUFT atelier over the packed-site renderer."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luft/renderer" "luvcraft/mcclim")
  :components ((:file "luft/render/studio"))
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
  :depends-on ("luft/render" "rove")
  :components ((:file "luft/render/tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call '#:rove '#:run-suite
                                       (uiop:symbol-call '#:rove '#:find-suite
                                                         '#:luft.render.tests))
               (error "LUFT render tests failed"))))

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
  :depends-on ("luft/z-fiber-benchmark" "rove")
  :components ((:file "luft/z-fiber-benchmark-tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call '#:rove '#:run-suite
                                       (uiop:symbol-call '#:rove '#:find-suite
                                                         '#:luft.z-fiber-benchmark.tests))
               (error "LUFT Z-fiber benchmark tests failed"))))
