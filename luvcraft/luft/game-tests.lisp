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
  (let* ((viewer (allocate-instance (find-class 'host:luvcraft-viewer)))
         (labels (mapcar #'mcluv:command-menu-entry-label
                         (mcluv:command-menu-entries-for-tables
                          (mcluv:command-menu-tables-for viewer)))))
    (dolist (label '("Execute Command" "Toggle Metabar" "Inventory" "Creative Mode"))
      (true (member label labels :test #'string=)))
    (true (member :navigation (mcluv:metabar-groups-for viewer)))
    (true (member :movement-speed (mcluv:metabar-controls-for viewer :navigation)))
    (is eq :scalar (mcluv:metabar-control-kind :movement-speed viewer))
    (is eq :scalar (mcluv:metabar-control-kind :time-of-day viewer))
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
                                       :timestamp 0 :key-name :return)))
      (true (render::viewer-key-command viewer event)))))

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
