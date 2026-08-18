(in-package #:asdf-user)

(defsystem "luvcraft/world"
  :description "Renderer-independent coordinates, chunk domains, and resident voxel data."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luv/arithmetic")
  :serial t
  :components
  ((:module "luvcraft"
    :serial t
    :components ((:file "world-quantities-package")
                 (:file "world-quantities")
                 (:file "world-fields-package")
                 (:file "world-fields")
                 (:file "world-package")
                 (:file "world")))))

(defsystem "luvcraft"
  :description "The interactive block world built on luv."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luv"
               "luv/ghostty"
               "luv/libav"
               "luv/terminal/canvas"
               "luvcraft/world"
               "cl-dejavu"
               "sb-concurrency"
               (:require #:sb-bsd-sockets)
               (:require #:sb-posix)
               "uiop")
  :serial t
  :components
  ((:module "luvcraft"
    :serial t
    :components ((:file "package")
                 (:file "quantities-package")
                 (:file "quantities")
                 (:file "arithmetic-package")
                 (:file "arithmetic")
                 (:file "knobs")
                 (:file "shaders")
                 (:file "fields")
                 (:file "production")
                 (:file "blocks")
                 (:file "inventory")
                 (:file "terrain")
                 (:file "frontier")
                 (:file "light")
                 (:file "frontier-light")
                 (:file "mesher")
                 (:file "particles")
                 (:file "intent")
                 (:file "simulation")
                 (:file "critters")
                 (:file "persistence")
                 (:file "sky")
                 (:file "frame-performance")
                 (:file "live-pipeline")
                 (:file "release")
                 (:file "text")
                 (:file "video-screen")
                 (:file "app")
                 (:file "riding")
                 (:file "body")
                 (:file "streaming")
                 (:file "render")
                 (:file "play")
                 (:file "terminal-wall")
                 (:file "phone")
                 (:file "capture")
                 (:file "mirror" :if-feature :darwin)
                 (:file "portal" :if-feature :darwin)
                 (:file "portal-server" :if-feature :darwin)
                 (:file "benchmark")
                 (:file "gazetteer"))))
  :in-order-to ((test-op (test-op "luvcraft/test"))))

(defsystem "luvcraft/tools"
  :description "One-shot command-line tools for luvcraft development."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luvcraft" "uiop")
  :build-operation "program-op"
  :build-pathname "build/luv"
  :entry-point "luvcraft.tools:main"
  :serial t
  :components
  ((:module "luvcraft/tools"
    :serial t
    :components ((:file "package")
                 (:file "runner")
                 (:file "block-world")
                 (:file "gazetteer")))))

(defsystem "luvcraft/clim"
  :description "Luvcraft's verbs as CLIM commands in an application frame."
  :version "0.0.1"
  :author "Mikael Brockman"
  ;; The Telegram panel is part of the game: it is what the phone shows, and
  ;; the wall's third mode.  It has to be loaded before the command layer,
  ;; which binds a keystroke to every mode the wall offers at load time.
  :depends-on ("luvcraft" "mcluv/luvcraft" "mcluv/telegram" "alexandria")
  :serial t
  :components ((:module "luvcraft/clim"
                :serial t
                :components ((:file "package")
                             (:file "frame")
                             (:file "commands")
                             (:file "legend")
                             (:file "input"))))
  :in-order-to ((test-op (test-op "luvcraft/clim-test"))))

(defsystem "luvcraft/clim-test"
  :description "Executable claims for luvcraft's CLIM command vocabulary."
  :version "0.0.1"
  :depends-on ("luvcraft/clim" "rove")
  :components ((:file "luvcraft/clim/tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call '#:rove '#:run-suite
                                       (uiop:symbol-call '#:rove '#:find-suite
                                                         '#:luvcraft.clim.tests)
                                       :style :luv)
               (error "luvcraft CLIM tests failed"))))

(defsystem "luvcraft/program"
  :description "The standalone luvcraft executable with its live Slynk endpoint."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luvcraft" "luvcraft/clim" "sb-posix" "slynk")
  :components ((:file "luvcraft/main"))
  :build-operation "program-op"
  :build-pathname "build/luvcraft"
  :entry-point "luvcraft:main")

(defsystem "luvcraft/test"
  :description "Executable claims for the world, shaders, and interactive slice."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luvcraft" "cl-dejavu" "rove")
  :serial t
  :components ((:file "hal/shader/tests")
               (:file "luvcraft/world-tests")
               (:file "luvcraft/tests")
               (:file "luvcraft/light-tests")
               (:file "hal/metal/msl/tests")
               (:file "hal/metal/tests" :if-feature :darwin))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call '#:rove '#:run-suite
                                       (uiop:symbol-call '#:rove '#:find-suite
                                                         '#:luvcraft.tests)
                                       :style :luv)
               (error "luvcraft tests failed"))))
