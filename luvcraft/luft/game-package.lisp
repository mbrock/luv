(defpackage #:luvcraft.luft.game
  (:use #:cl)
  (:local-nicknames (#:render #:luft.render)
                    (#:terrain #:luft.terrain)
                    (#:game #:luvcraft.luft)
                    (#:vec3 #:luv.arithmetic.lisp.vec3))
  (:documentation "The Luvcraft on Luft game: a world, its players, and their views (#32DUBY).")
  (:export #:play
           #:*starting-hotbar*
           ;; The world, which runs without a window.
           #:luvcraft-world
           #:make-luvcraft-world
           #:world-block-world
           #:world-scene
           #:world-source
           #:world-simulation
           #:world-players
           #:find-world-player
           #:add-world-player
           #:remove-world-player
           #:world-owner
           #:world-editor
           #:advance-luvcraft-world
           #:advance-world-sky
           #:advance-world-populations
           #:player-edit-world-cell
           #:luvcraft-world-save-description
           #:save-luvcraft-world
           ;; A player in it.
           #:luvcraft-player
           #:make-luvcraft-player
           #:player-id
           #:player-name
           #:player-character
           #:player-hotbar
           #:player-placement
           #:player-held-block
           #:player-rider-intent
           #:player-ball-in-hand-p
           ;; One view of it.
           #:luvcraft-viewer
           #:viewer-world
           #:viewer-local-player))
