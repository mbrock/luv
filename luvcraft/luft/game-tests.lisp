(defpackage #:luvcraft.luft.game.tests
  (:use #:cl)
  (:import-from #:parachute #:define-test #:is #:true)
  (:local-nicknames (#:host #:luvcraft.luft.game)
                    (#:game #:luvcraft.luft)
                    (#:render #:luft.render)))

(in-package #:luvcraft.luft.game.tests)

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
