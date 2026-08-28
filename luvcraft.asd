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

(defsystem "luvcraft/frontier"
  :description "Inspectable frontier programs compiled over packed world fields."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luv/arithmetic" "luvcraft/world")
  :serial t
  :components ((:file "luvcraft/frontier-package")
               (:file "luvcraft/frontier")))

(defsystem "luvcraft/core"
  :description "The renderer, simulation, and devices of the luvcraft block world."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luv"
               "luv/tracy-capture"
               "luv/ghostty"
               "luv/libav"
               "luv/terminal/canvas"
               "luv/production"
               "luvcraft/world"
               "luvcraft/frontier"
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
                 (:file "player-body-shaders")
                 (:file "fields")
                 (:file "blocks")
                 (:file "inventory")
                 (:file "terrain")
                 (:file "light")
                 (:file "frontier-light")
                 (:file "mesher")
                 (:file "particles")
                 (:file "intent")
                 (:file "simulation")
                 (:file "locomotion")
                 (:file "critters")
                 (:file "physics")
                 (:file "physics-simd" :if-feature :sbcl)
                 (:file "persistence")
                 (:file "sky")
                 (:file "frame-performance")
                 (:file "text")
                 (:file "sound")
                 (:file "video-interop")
                 (:file "video-interop-vulkan")
                 (:file "video-interop-metal" :if-feature :darwin)
                 (:file "video-screen")
                 (:file "renderer")
                 (:file "app")
                 (:file "riding")
                 (:file "body")
                 (:file "balls")
                 (:file "streaming")
                 (:file "render")
                 (:file "play")
                 (:file "mirror" :if-feature :darwin)
                 (:file "portal" :if-feature :darwin)
                 (:file "portal-server" :if-feature :darwin)
                 (:file "terminal-wall")
                 (:file "urbit")
                 (:file "phone")
                 (:file "tape")
                 (:file "capture")
                 (:file "film")
                 (:file "benchmark")
                 (:file "gazetteer"))))
  :in-order-to ((test-op (test-op "luvcraft/core/test"))))

(defsystem "luvcraft/light-reference"
  :description "Test-only voxel-light oracle and differential diagnostics."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luvcraft/core")
  :components ((:file "luvcraft/light-reference")))

(defsystem "luvcraft/agent-bodies"
  :description "The analytic gnome and cat body shaders and their live knobs."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luvcraft/core")
  :serial t
  :components ((:file "luvcraft/agent/shaders")
               (:file "luvcraft/agent/cat-shaders")))

(defsystem "luvcraft/web"
  :description "Luvcraft's showcase and WebGPU body pages in the workshop wiki."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luvcraft/agent-bodies" "luv/wgsl" "luv-wiki" "cl-json"
               "spinneret" "parenscript" "uiop")
  :serial t
  :components ((:file "luvcraft/web-package")
               (:file "luvcraft/wiki-style")
               (:file "luvcraft/showcase")
               (:file "luvcraft/body-gallery-script")
               (:file "luvcraft/body-gallery")))

(defsystem "luvcraft/tools"
  :description "One-shot command-line tools for luvcraft development."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luvcraft/core" "mqtt/net" "uiop")
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
                 (:file "gazetteer")
                 (:file "lobby")))))

(defsystem "luvcraft/lobby"
  :description "Luvcraft ownership of the shared application lobby radio."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luvcraft/core" "luv/lobby")
  :components ((:file "luvcraft/lobby")))

(defsystem "luvcraft/mcclim"
  :description "McCLIM command surfaces embedded in a live luvcraft session."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luv/mcclim" "luvcraft/core")
  :serial t
  :components ((:file "mcclim/surveyor")
               (:file "mcclim/luvcraft")
               (:file "mcclim/luvcraft-source-update")
               (:file "mcclim/terminal-film-browser")
               (:file "mcclim/block-icon")
               (:file "mcclim/hotbar")
               (:file "mcclim/inventory")
               (:file "mcclim/luvcraft-metabar"))
  :in-order-to ((test-op (test-op "luvcraft/mcclim/test"))))

(defsystem "luvcraft/mcclim/test"
  :description "Executable claims for McCLIM instruments embedded in luvcraft."
  :version "0.0.1"
  :depends-on ("luvcraft/mcclim" "luv/test-support")
  :serial t
  :components ((:file "mcclim/surveyor-tests")
               (:file "mcclim/luvcraft-metabar-tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:luv.test-support '#:test-package
                               '#:mcluv.surveyor-tests)))

(defsystem "luvcraft/lobby/mcclim"
  :description "The shared lobby HUD placed in Luvcraft's final HUD pass."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luvcraft/lobby" "luvcraft/mcclim" "luv/lobby/mcclim")
  :serial t
  :components ((:file "mcclim/lobby")
               (:file "mcclim/status-bar")))

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
               "luvcraft/lobby/mcclim"
               "alexandria")
  :serial t
  :components ((:file "luvcraft/lobby-credentials")
               (:module "luvcraft/clim"
                :serial t
                :components ((:file "package")
                             (:file "frame")
                             (:file "commands")
                             (:file "legend")
                             (:file "command-menu")
                             (:file "source-update")
                             (:file "tape")
                             (:file "input"))))
  :in-order-to ((test-op (test-op "luvcraft/core/test")
                         (test-op "luv/mcclim/test")
                         (test-op "luv/lobby/test")
                         (test-op "luv/lobby/mcclim/test")
                         (test-op "luvcraft/mcclim/test")
                         (test-op "luvcraft/test"))))

(defsystem "luvcraft/agent"
  :description "An agent in the little world: tools as CLIM commands, results as presentations."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luvcraft" "luvcraft/agent-bodies"
               "luv/application-agent"
               "openai" "alexandria" "sb-concurrency")
  :serial t
  :components ((:module "luvcraft/agent"
                :serial t
                :components ((:file "package")
                             (:file "application-adapter")
                             (:file "commands")
                             (:file "approval")
                             (:file "hud")
                             (:file "wall")
                             (:file "gnome")
                             (:file "construction")
                             (:file "cat")
                             (:file "surroundings"))))
  :in-order-to ((test-op (test-op "luvcraft/agent/test"))))

(defsystem "luvcraft/agent/test"
  :description "Tests for embodied agent observations and tools."
  :depends-on ("luvcraft/agent" "luv/test-support")
  :serial t
  :components ((:module "luvcraft/agent/tests"
                :serial t
                :components ((:file "package")
                             (:file "surroundings")
                             (:file "construction")
                             (:file "opening"))))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:luv.test-support '#:test-package
                               '#:luvcraft.agent.tests)))

(defsystem "luvcraft/shader-validation/test"
  :description "External validation of every production SPIR-V shader."
  :depends-on ("luvcraft/agent")
  :components ((:file "scripts/shader-validation"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:luv.shader-validation
                               '#:validate-production-shaders)))

(defsystem "luvcraft/birthday"
  :description "A birthday party in the little world: meadow, gazebo, balloons, gnomes, fireworks."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luvcraft/agent")
  :serial t
  :components ((:module "luvcraft/birthday"
                :serial t
                :components ((:file "package")
                             (:file "world")
                             (:file "gazebo")
                             (:file "balloons")
                             (:file "gnomes")
                             (:file "fireworks")
                             (:file "marquee")
                             (:file "party")))))

(defsystem "luvcraft/program"
  :description "The standalone luvcraft executable with its live Slynk endpoint."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luvcraft/agent" "luvcraft/birthday" "sb-posix" "slynk")
  :components ((:file "luvcraft/main"))
  :build-operation "program-op"
  :build-pathname "build/luvcraft"
  :entry-point "luvcraft:main")

(defsystem "luvcraft/core/test"
  :description "Executable claims for the world, shaders, and interactive slice."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luvcraft/core" "luvcraft/light-reference"
               "luvcraft/web" "cl-dejavu" "luv/test-support")
  :serial t
  :components ((:file "hal/shader/tests")
               (:file "luvcraft/world-tests")
               (:file "luvcraft/tests")
               (:file "luvcraft/renderer-tests")
               (:file "luvcraft/video-interop-tests")
               (:file "luvcraft/physics-tests")
               (:file "luvcraft/light-tests")
               (:file "hal/metal/msl/tests")
               (:file "hal/webgpu/wgsl/tests")
               (:file "hal/metal/tests" :if-feature :darwin))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:luv.test-support '#:test-package
                               '#:luvcraft.tests)))

(defsystem "luvcraft/test"
  :description "Executable claims for the complete game's CLIM command vocabulary."
  :version "0.0.1"
  :depends-on ("luvcraft" "luv/test-support")
  :serial t
  :components ((:file "luvcraft/clim/tests")
               (:file "luvcraft/lobby-tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:luv.test-support '#:test-package
                               '#:luvcraft.clim.tests)))
