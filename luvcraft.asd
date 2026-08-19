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

(defsystem "luvcraft/core"
  :description "The renderer, simulation, and devices of the luvcraft block world."
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
               (:require #:sb-simd)
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
                 (:file "physics")
                 (:file "physics-simd" :if-feature :sbcl)
                 (:file "persistence")
                 (:file "sky")
                 (:file "frame-performance")
                 (:file "live-pipeline")
                 (:file "release")
                 (:file "text")
                 (:file "sound")
                 (:file "video-screen")
                 (:file "app")
                 (:file "riding")
                 (:file "body")
                 (:file "balls")
                 (:file "streaming")
                 (:file "render")
                 (:file "play")
                 (:file "terminal-wall")
                 (:file "urbit")
                 (:file "phone")
                 (:file "tape")
                 (:file "capture")
                 (:file "mirror" :if-feature :darwin)
                 (:file "portal" :if-feature :darwin)
                 (:file "portal-server" :if-feature :darwin)
                 (:file "benchmark")
                 (:file "gazetteer"))))
  :in-order-to ((test-op (test-op "luvcraft/core/test"))))

(defsystem "luvcraft/tools"
  :description "One-shot command-line tools for luvcraft development."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luvcraft/core" "uiop")
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

(defsystem "luvcraft/mcclim"
  :description "McCLIM gadget textures embedded in a live luvcraft session."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luv/mcclim" "luvcraft/core")
  :serial t
  :components ((:file "mcclim/surveyor")
               (:file "mcclim/luvcraft")
               (:file "mcclim/terminal-film-browser")
               (:file "mcclim/block-icon")
               (:file "mcclim/hotbar")
               (:file "mcclim/inventory")
               (:file "mcclim/metabar"))
  :in-order-to ((test-op (test-op "luvcraft/mcclim/test"))))

(defsystem "luvcraft/mcclim/test"
  :description "Executable claims for McCLIM instruments embedded in luvcraft."
  :version "0.0.1"
  :depends-on ("luvcraft/mcclim" "rove")
  :components ((:file "mcclim/surveyor-tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call '#:rove '#:run-suite
                                       (uiop:symbol-call '#:rove '#:find-suite
                                                         '#:mcluv.surveyor-tests)
                                       :style :luv)
               (error "luvcraft/mcclim tests failed"))))

(defsystem "luvcraft/telegram"
  :description "A Telegram terminal mounted on a luvcraft wall and phone."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luvcraft/mcclim" "telegram/chat" "luv/libav" "sb-concurrency")
  :components ((:file "mcclim/telegram")))

(defsystem "luvcraft/paper"
  :description "A sheet of PDF paper hung on a luvcraft wall."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luvcraft/mcclim" "luv/mupdf" "cl-dejavu" "zpb-ttf")
  :components ((:file "mcclim/paper")))

(defsystem "luvcraft/shader-lab"
  :description "A McCLIM presentation browser for luvcraft's live shaders."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luvcraft/mcclim")
  :components ((:file "mcclim/shader-lab")))

(defsystem "luvcraft"
  :description "The complete interactive luvcraft game."
  :version "0.0.1"
  :author "Mikael Brockman"
  ;; Telegram is part of the game: it is what the phone initially shows and
  ;; the wall's third mode.  Load it before the command layer, whose keymap is
  ;; assembled from the modes available at load time.
  :depends-on ("luvcraft/core"
               "luvcraft/mcclim"
               "luvcraft/telegram"
               "alexandria")
  :serial t
  :components ((:module "luvcraft/clim"
                :serial t
                :components ((:file "package")
                             (:file "frame")
                             (:file "commands")
                             (:file "legend")
                             (:file "tape")
                             (:file "input"))))
  :in-order-to ((test-op (test-op "luvcraft/core/test")
                         (test-op "luv/mcclim/test")
                         (test-op "luvcraft/mcclim/test")
                         (test-op "luvcraft/test"))))

(defsystem "luvcraft/program"
  :description "The standalone luvcraft executable with its live Slynk endpoint."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luvcraft" "sb-posix" "slynk")
  :components ((:file "luvcraft/main"))
  :build-operation "program-op"
  :build-pathname "build/luvcraft"
  :entry-point "luvcraft:main")

(defsystem "luvcraft/core/test"
  :description "Executable claims for the world, shaders, and interactive slice."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luvcraft/core" "cl-dejavu" "rove")
  :serial t
  :components ((:file "hal/shader/tests")
               (:file "luvcraft/world-tests")
               (:file "luvcraft/tests")
               (:file "luvcraft/physics-tests")
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

(defsystem "luvcraft/test"
  :description "Executable claims for the complete game's CLIM command vocabulary."
  :version "0.0.1"
  :depends-on ("luvcraft" "rove")
  :components ((:file "luvcraft/clim/tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call '#:rove '#:run-suite
                                       (uiop:symbol-call '#:rove '#:find-suite
                                                         '#:luvcraft.clim.tests)
                                       :style :luv)
               (error "luvcraft tests failed"))))
