(defpackage #:luvcraft.luft.game
  (:use #:cl)
  (:local-nicknames (#:render #:luft.render)
                    (#:terrain #:luft.terrain)
                    (#:game #:luvcraft.luft)
                    (#:vec3 #:luv.arithmetic.lisp.vec3)
                    (#:wire #:luvcraft.wire)
                    (#:net #:luvcraft.wire.net))
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
           #:world-edit-actor
           #:*world-edit-actor*
           #:luvcraft-world-save-description
           #:save-luvcraft-world
           #:shutdown-luvcraft-world
           ;; Sessions joining worlds over the wire (#OLWSWF).
           #:world-session
           #:host-session
           #:guest-session
           #:host-luvcraft-world
           #:join-luvcraft-world
           #:advance-world-session
           #:close-world-session
           #:session-world
           #:session-local-player
           #:session-endpoint
           #:session-status
           #:session-undo
           #:session-replies
           #:session-applied
           #:session-mismatched-chunks
           #:session-matched-chunks
           #:report-chunk-hashes
           #:world-tick
           #:remote-player-figures
           ;; A remote player's track (#VM5TN6).
           #:player-track
           #:remote-track
           #:make-remote-track
           #:push-pose-snapshot
           #:advance-remote-track
           #:track-shown
           #:track-delay
           #:make-pose-snapshot
           #:pose-snapshot-position
           #:pose-snapshot-sent
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
           #:steer-luvcraft-player
           #:player-ball-in-hand-p
           ;; One view of it.
           #:luvcraft-viewer
           #:viewer-world
           #:viewer-local-player))
