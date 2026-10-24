(defpackage #:luvcraft.luft.game.tests
  (:use #:cl)
  (:import-from #:parachute #:define-test #:is #:true)
  (:local-nicknames (#:host #:luvcraft.luft.game)
                    (#:game #:luvcraft.luft)
                    (#:render #:luft.render)))

(in-package #:luvcraft.luft.game.tests)

(define-test player-instruments-share-the-old-quickbar-without-spending-stock
  (let* ((viewer (allocate-instance (find-class 'host:luvcraft-viewer)))
         (inventory (host::restore-inventory nil))
         (block luvcraft::*bricks-block*))
    (setf (slot-value viewer 'host::inventory) inventory
          (slot-value viewer 'host::held-block) nil
          (slot-value viewer 'render::edit-material)
          (game:block-placement luvcraft::*grass-block*))
    (luvcraft:add-block-to-inventory inventory block 7)
    (luvcraft:assign-luvcraft-quickbar-slot viewer block 3)
    (is eq block (luvcraft:player-selected-block viewer))
    (is eq (game:block-placement block) (third (render:viewer-hotbar viewer)))
    (is = 7 (luvcraft:block-inventory-entry-quantity
             (luvcraft:block-inventory-entry-for inventory block)))
    ;; Simple mode is the same six-block kit the old inventory edits, not
    ;; another palette. Selection through the old public operation reaches
    ;; the engine while retaining the very same finite entry.
    (setf (luvcraft::block-inventory-simple-p inventory) t
          (luvcraft::block-inventory-kit inventory) (list block))
    (luvcraft:put-luvcraft-block-on-quickbar viewer luvcraft::*planks-block*)
    (is eq luvcraft::*planks-block* (luvcraft:player-selected-block viewer))
    (is = 2 (length (render:viewer-hotbar viewer)))
    (is = 7 (luvcraft:block-inventory-entry-quantity
             (luvcraft:block-inventory-entry-for inventory block)))))

(define-test game-host-m-x-and-metabar-have-their-executable-vocabulary
  (let* ((viewer (make-instance 'host:luvcraft-viewer
                                :hotbar (list (game:block-placement luvcraft::*stone-block*))
                                :source (luft.world:make-mountain-sanctuary-scene)))
         (labels (mapcar #'mcluv:command-menu-entry-label
                         (mcluv:command-menu-entries-for-tables
                          (mcluv:command-menu-tables-for viewer)))))
    (dolist (label '("Execute Command" "Toggle Metabar" "Inventory" "Creative Mode" "Use Terminal"))
      (true (member label labels :test #'string=)))
    (true (member :player (mcluv:metabar-groups-for viewer)))
    (let ((keys (cdr (assoc "show keys"
                            (cdr (assoc "Any time" (mcluv::keymap-legend-sections viewer)
                                        :test #'string=)) :test #'string=))))
      (true (member "F1" keys :test #'string=))
      (true (member "H" keys :test #'string=)))
    (let ((walk (luv.knobs:find-knob 'render::movement-speed))
          (clock (luv.knobs:find-knob 'render::time-of-day)))
      (true (member walk (mcluv:metabar-controls-for viewer :player)))
      (is eq :scalar (mcluv:metabar-control-kind walk viewer))
      (is eq :scalar (mcluv:metabar-control-kind clock viewer)))
    ;; The vocabulary alone is insufficient: McCLIM needs inherited menus
    ;; enabled for inherited keystrokes, even though the commands are present.
    (setf (render:viewer-pointer-captured-p viewer) nil
          (slot-value viewer 'clim:command-table)
          (clim:find-command-table 'host:luvcraft-viewer)
          (slot-value viewer 'clim-internals::disabled-commands) nil
          (render:viewer-mode viewer) (make-instance 'render:first-person-mode))
    (dolist (event (list (make-instance 'luv:canvas-key-press-event
                                      :timestamp 0 :key-name :x
                                      :unshifted-character #\x :modifiers '(:meta))
                        (make-instance 'luv:canvas-key-press-event
                                       :timestamp 0 :key-name :return)
                        (make-instance 'luv:canvas-key-press-event
                                       :timestamp 0 :key-name :f1)
                        (make-instance 'luv:canvas-key-press-event
                                       :timestamp 0 :key-name :h :unshifted-character #\h)))
      (true (render::viewer-key-command viewer event)))))

(define-test game-terminal-material-and-keyboard-use-the-host-protocol
  (let* ((terminal (game:block-placement luvcraft::*terminal-block*))
         (builder (luft.terrain:make-scene-builder
                   :horizontal-bits 11
                   :material-vocabulary (game:luvcraft-palette-vocabulary game:*luvcraft-palette*)
                   :ground-material terminal :architecture-material terminal)))
    (loop for x from 1024 below 1028 do
      (loop for z from 4 below 7 do (luft.terrain:scene-builder-cell builder x 1024 z)))
    (let* ((scene (luft.terrain:finish-scene-builder builder))
           (viewer (make-instance 'host:luvcraft-viewer :source scene :hotbar (list terminal)))
           (surface (render::find-terminal-surface scene 1025 1024 5 '(0 -1 0)))
           (state (render::viewer-terminals viewer))
           (screen (make-instance 'render::world-terminal :surface surface
                                  :columns 40 :rows 15 :terminal nil :glyph-cache nil)))
      (true surface)
      (is = 4 (render::terminal-surface-width surface))
      (is = 3 (render::terminal-surface-height surface))
      (setf (render::viewer-terminals-terminals state) (list screen)
            (render::viewer-terminals-focus state) screen)
      (render:focus-viewer viewer state)
      ;; F1 and Return must belong to the shell, not open host drawers.
      (dolist (name '(:f1 :return :escape))
        (luv:handle-canvas-event viewer nil
                                (make-instance 'luv:canvas-key-press-event :timestamp 0 :key-name name))
        (is eq state (render:viewer-modal-focus viewer)))
      (luv:handle-canvas-event viewer nil
                              (make-instance 'luv:canvas-key-press-event :timestamp 0 :key-name :tab
                                             :modifiers '(:shift)))
      (true (null (render:viewer-modal-focus viewer)))
      (true (null (render::viewer-terminals-focus state)))
      (render:remove-viewer-overlay viewer state)
      (true (null (render::viewer-terminals-terminals state))))))

(define-test inventory-and-carried-identities-round-trip-through-the-game-save
  (let* ((world (luvcraft:make-empty-little-block-world :seed 291 :chunk-height 16))
         (source (game:luvcraft-world-luft-source world))
         (viewer (allocate-instance (find-class 'host:luvcraft-viewer)))
         (inventory (host::restore-inventory nil))
         (film (luvcraft::ensure-film "inventory-round-trip" :title "A carried film")))
    (luvcraft:add-block-to-inventory inventory luvcraft::*planks-block* 7)
    (luvcraft:add-block-to-inventory inventory film 2)
    (setf (slot-value viewer 'host::luvcraft-world) world
          (slot-value viewer 'host::resume) nil
          (slot-value viewer 'host::inhabitants) nil
          (slot-value viewer 'host::animals-initialized-p) nil
          (slot-value viewer 'host::inventory) inventory
          (slot-value viewer 'host::creative-p) t
          (slot-value viewer 'host::held-block) film
          (slot-value viewer 'host::hotbar) (list (game:block-placement film))
          (slot-value viewer 'render::edit-material) (game:block-placement film)
          (slot-value viewer 'render::player)
          (render::make-walking-character
           :position (luv.arithmetic.lisp.vec3:make-vec3 1024.5 1024.5 8.0))
          (slot-value viewer 'render::camera) (render:make-fly-camera)
          (slot-value viewer 'render::simulation)
          (render:make-world-simulation (game:make-luvcraft-streaming-scene source)))
    ;; Exercise the game's actual save protocol, then Luvcraft's reader.
    ;; A player pose also checks that refreshing it does not drop inventory.
    (uiop:with-temporary-file (:pathname pathname :type "sexp")
      (render:write-viewer-world-description
       viewer (render:viewer-world-description viewer) pathname)
      (multiple-value-bind (restored-world resume) (luvcraft:read-luvcraft-save pathname)
        (true restored-world)
        (let ((restored (host::restore-inventory resume)))
          (is = 7 (luvcraft:block-inventory-entry-quantity
                   (luvcraft:block-inventory-entry-for restored luvcraft::*planks-block*)))
          (is = 2 (luvcraft:block-inventory-entry-quantity
                   (luvcraft:block-inventory-entry-for restored film)))
          (is equal (luvcraft::block-save-description film)
              (getf resume :selected-block))
          (is equal (list (luvcraft::block-save-description film)) (getf resume :carried))
          (is equal '(:film) (getf resume :quickbar))
          (true (getf resume :creative-p)))
        (let* ((next-source (game:luvcraft-world-luft-source restored-world))
               (next (game:luvcraft-save-description
                      next-source restored-world :resume resume
                      :position '(1024d0 1024d0 9d0) :yaw 0d0 :pitch 0d0
                      :selected-block film)))
          (is equal (getf resume :inventory)
              (getf (getf (rest next) :resume) :inventory)))))))

(define-test legacy-carried-blocks-enter-finite-storage
  (let* ((film (luvcraft::ensure-film "legacy-inventory"))
         (inventory (host::restore-inventory
                     (list :carried (list (luvcraft::block-save-description film))))))
    (is = 1 (luvcraft:block-inventory-entry-quantity
             (luvcraft:block-inventory-entry-for inventory film)))
    (is = 0 (luvcraft:block-inventory-entry-quantity
             (luvcraft:block-inventory-entry-for inventory luvcraft::*stone-block*)))))

(define-test companions-cross-the-inhabitant-save-without-losing-other-inhabitants
  (let* ((world (luvcraft:make-empty-little-block-world :seed 7 :chunk-height 16))
         (source (game:luvcraft-world-luft-source world))
         (viewer (allocate-instance (find-class 'host:luvcraft-viewer)))
         (descriptions '((:gnome :cell (3 5 0) :facing-yaw 0.4
                                 :conversation "response-test" :asked "Build" :said "Yes")
                         (:cat :cell (0 5 3)) (:fantus :cell (-3 5 0))
                         (:gnome :cell (99999 5 0))
                         (:luft-critters :animals nil) (:future-inhabitant :value 42))))
    (setf (slot-value viewer 'host::inhabitants) descriptions
          (slot-value viewer 'host::agents) nil
          (slot-value viewer 'host::agents-initialized-p) nil
          (slot-value viewer 'render::simulation)
          (render:make-world-simulation (game:make-luvcraft-streaming-scene source)))
    (host::initialize-viewer-agents viewer)
    (is = 3 (length (host::viewer-agents viewer)))
    (let* ((gnome (find :gnome (host::viewer-agents viewer) :key #'host::inhabitant-kind))
           (position (host::inhabitant-position gnome)))
      ;; Move in Luft's horizontal Y and save in old Luvcraft's horizontal Z.
      (incf (luv.arithmetic.lisp.vec3:vec3-y position) 2)
      (let* ((saved (host::agent-inhabitant-descriptions viewer descriptions))
             (record (assoc :gnome saved)))
        (is = 6 (length saved))
        (true (member (fourth descriptions) saved))
        (true (member (sixth descriptions) saved))
        (is equal '(3 5 2) (getf (rest record) :cell))
        (is equal "response-test" (getf (rest record) :conversation))
        (is equal "Build" (getf (rest record) :asked))
        ;; Old Luvcraft's inhabitant reader accepts the new game's record.
        (let ((old (luvcraft.agent::described-embodied-agent 'luvcraft.agent:gnome nil (rest record))))
          (is equal '(3 5 2) (list (luvcraft.agent::gnome-x old)
                                   (luvcraft.agent::gnome-y old) (luvcraft.agent::gnome-z old))))
        (setf (slot-value viewer 'host::inhabitants) saved
              (slot-value viewer 'host::agents-initialized-p) nil
              (slot-value viewer 'host::agents) nil)
        (host::initialize-viewer-agents viewer)
        (is = 3 (length (host::viewer-agents viewer)))
        (is equal saved (host::agent-inhabitant-descriptions viewer saved))))))

(define-test mining-at-the-crosshair-emits-material-tinted-fragments-only-on-success
  (let* ((block luvcraft::*red-wool-block*)
         (placement (game:block-placement block))
         (world (luvcraft:make-empty-little-block-world :chunk-height 16))
         (source (game:luvcraft-world-luft-source world))
         (builder (luft.terrain:make-scene-builder
                   :horizontal-bits 11
                   :material-vocabulary (game:luvcraft-palette-vocabulary game:*luvcraft-palette*)))
         (producer (luv.production:make-single-worker-production-system)))
    (unwind-protect
         (progn
           (luft.terrain:scene-builder-cell builder 1032 1033 5 :material placement)
           (let* ((scene (luft.terrain:make-streaming-scene
                          (luft.terrain:finish-scene-builder builder :voxel-light-propagation-p nil)))
                  (viewer (make-instance 'host:luvcraft-viewer :source scene :luvcraft-world world
                                         :production-system producer :hotbar (list placement)
                                         :camera (render:make-fly-camera
                                                  :position (luv.arithmetic.lisp.vec3:make-vec3 1032.5 1030.5 5.5)
                                                  :yaw (/ pi 2) :pitch 0.0))))
             ;; Source-backed edits retain resident material records, unlike
             ;; the finite-scene fixture's initial bare fiber store. Supply
             ;; its empty halo too, so the real production snapshot is valid.
             (loop for x from 15 to 17 do
               (loop for y from 15 to 17
                     for key = (luft:chunk-key-at (* 64 x) (* 64 y))
                     for fibers = (or (gethash key (luft.terrain:streaming-scene-store scene))
                                      (luft:make-chunk-fibers (luft.terrain:scene-domain scene) key))
                     do (setf (gethash key (luft.terrain:streaming-scene-store scene))
                              (luft.terrain:%make-resident-cell-chunk
                               key 1 fibers
                               (or (gethash key (luft.terrain::material-store-chunks
                                                 (luft.terrain:scene-material-cells scene)))
                                   (make-hash-table :test #'eql))))))
             (setf (slot-value scene 'luft.terrain::source) source
                   (slot-value viewer 'render::canvas) (luv:make-sdl-canvas :width 800 :height 600)
                   (render:viewer-mode viewer) (make-instance 'render:first-person-mode)
                   (render:viewer-pointer-captured-p viewer) t)
             (let* ((hit (render::update-viewer-inspection viewer))
                    (boundary (host::target-face-boundary
                               scene (render:site-inspection-site hit) (render:site-inspection-cell hit))))
               ;; An isolated block's true front starts a bevel tick in from
               ;; each side, rather than at the uncut voxel corners.
               (is = 4 (length boundary))
               (dolist (p boundary)
                 (true (member (luv.arithmetic.lisp.vec3:vec3-x p) '(1032.125 1032.875) :test #'=))
                 (is = 1033 (luv.arithmetic.lisp.vec3:vec3-y p))
                 (true (member (luv.arithmetic.lisp.vec3:vec3-z p) '(5.125 5.875) :test #'=))))
             (luv:handle-canvas-event
              viewer nil (make-instance 'luv:canvas-pointer-button-press-event
                                        :timestamp 0 :x 400 :y 300 :button :left))
             (is eq :edited (render:viewer-last-edit-status viewer))
             (is = 18 (luvcraft::block-particle-count (host::viewer-particles viewer)))
             (let ((figures (render:viewer-figures viewer)))
               (is = 18 (length figures))
               (is equal (append (nth-value 1 (luft.terrain:material-kind-oriented-tones
                                               (luft.terrain:material-placement-kind placement))) '(0.0))
                   (fifth (first figures))))
             ;; The same click now hits air: there is no phantom second burst.
             (luv:handle-canvas-event
              viewer nil (make-instance 'luv:canvas-pointer-button-press-event
                                        :timestamp 0 :x 400 :y 300 :button :left))
             (is = 18 (luvcraft::block-particle-count (host::viewer-particles viewer)))))
      (luv.production:stop-production-system producer))))

(define-test creature-figures-retain-original-atlas-and-mesh
  (let* ((animal (make-instance 'luvcraft::turtle :seed 7 :yaw 0.65d0
                                :position (luv.arithmetic.lisp.vec3:make-vec3 8d0 4d0 6d0)))
         (figures (host::animal-figures animal))
         (data (render:pack-figure-instances figures))
         (actual (host::creature-figure-vertices data))
         (expected (make-array (length actual) :element-type 'single-float :fill-pointer 0)))
    ;; The original emitter is the oracle, including UV scale, body-part tile,
    ;; face shading, normals and winding. Packing loses only float precision.
    (luvcraft::map-critter-boxes
     animal (lambda (x y z hx hy hz tile)
              (luvcraft::emit-critter-box expected animal x y z hx hy hz tile 1.0 0.0)))
    (is = 5040 (length actual))
    (is = (length expected) (length actual))
    (true (loop for a across actual for b across expected always (< (abs (- a b)) 0.0002)))
    (is = 3 (length (remove-duplicates (mapcar (lambda (figure) (fourth (fifth figure))) figures))))
    ;; A prior pose belongs to this presentation, not a shared model or the
    ;; current pose. Temporal reconstruction must see the animal's motion.
    (incf (aref data 4) 1.0)
    (let ((previous (host::creature-figure-vertices data t)))
      (true (< (abs (- (- (aref previous 0) (aref actual 0)) 1.0)) 0.0002)))))

(define-test quiet-game-status-follows-the-world-clock
  (let ((viewer (allocate-instance (find-class 'host:luvcraft-viewer)))
        (host::*status-diagnostics-p* nil))
    (setf (slot-value viewer 'host::sky-clock) (make-instance 'luvcraft::sky-clock)
          (render::host-sky-hour viewer) 11.5)
    (is equal '(:daytime) (mcluv:status-bar-channels-for viewer))
    (is eq :chip (mcluv:status-bar-presentation viewer))
    (is string= "Daytime  11:30" (mcluv:status-bar-channel-value :daytime viewer nil))
    (setf (render::host-sky-hour viewer) 24.0)
    (is string= "Night  00:00" (mcluv:status-bar-channel-value :daytime viewer nil))))
