(in-package #:asdf-user)

(defsystem "mcluv/backend"
  :description "An experimental McCLIM backend presented through luv canvases."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luv" "mcclim-render" "cl-dejavu")
  :serial t
  :components ((:module "mcclim"
                :serial t
                :components ((:file "package")
                             (:file "paint")
                             (:file "port")
                             (:file "mirror")
                             (:file "gpu")
                             (:file "widget-lab")
                             (:file "compositor"))))
  :in-order-to ((test-op (test-op "mcluv/test"))))

(defsystem "mcluv/test"
  :description "Executable claims for the direct McCLIM GPU backend."
  :version "0.0.1"
  :depends-on ("mcluv/backend" "rove")
  :components ((:file "mcclim/tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call '#:rove '#:run-suite
                                       (uiop:symbol-call '#:rove '#:find-suite
                                                         '#:mcluv.tests)
                                       :style :luv)
               (error "mcluv tests failed"))))

(defsystem "mcluv/shader-lab"
  :description "A McCLIM presentation browser for luvcraft's live shaders."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("mcluv/backend" "luvcraft")
  :components ((:file "mcclim/shader-lab")))

(defsystem "mcluv/gallery"
  :description "A screenshot gallery and primitive-fallback audit for McCLIM."
  :version "0.0.1"
  :depends-on ("mcluv/backend" "clim-examples")
  :components ((:file "mcclim/gallery")))

(defsystem "mcluv/roundrect-benchmark"
  :description "A Tracy A/B of native and decomposed McCLIM roundrects."
  :version "0.0.1"
  :depends-on ("mcluv/backend")
  :components ((:file "mcclim/roundrect-benchmark")))

(defsystem "mcluv/paint-benchmark"
  :description "A Tracy comparison of solid, gradient, and image GUI paints."
  :version "0.0.1"
  :depends-on ("mcluv/backend")
  :components ((:file "mcclim/paint-benchmark")))

(defsystem "mcluv/luvcraft"
  :description "McCLIM gadget textures embedded in a live luvcraft session."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("mcluv/backend" "luvcraft")
  :serial t
  :components ((:file "mcclim/surveyor")
               (:file "mcclim/luvcraft")
               (:file "mcclim/terminal-film-browser")
               (:file "mcclim/hotbar")
               (:file "mcclim/inventory")))

(defsystem "mcluv/telegram"
  :description "A Telegram terminal mounted on a luvcraft wall."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("mcluv/luvcraft" "telegram/chat" "sb-concurrency")
  :components ((:file "mcclim/telegram")))

(defsystem "mcluv/luvcraft-test"
  :description "Executable claims for McCLIM instruments embedded in luvcraft."
  :version "0.0.1"
  :depends-on ("mcluv/luvcraft" "rove")
  :components ((:file "mcclim/surveyor-tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call '#:rove '#:run-suite
                                       (uiop:symbol-call '#:rove '#:find-suite
                                                         '#:mcluv.surveyor-tests)
                                       :style :luv)
               (error "mcluv surveyor tests failed"))))

(defsystem "mcluv/listener"
  :description "The McCLIM Listener running on the mcluv backend."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("mcluv/backend" "clim-listener")
  :components ((:file "mcclim/listener")))

(defsystem "mcluv"
  :description "The McCLIM Listener and shader lab as a standalone luv program."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("mcluv/listener" "mcluv/shader-lab")
  :components ((:file "mcclim/main"))
  :build-operation "program-op"
  :build-pathname "build/mcluv"
  :entry-point "mcluv:main")
