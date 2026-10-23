(defpackage #:luvcraft.luft
  (:use #:cl)
  (:local-nicknames (#:terrain #:luft.terrain)
                    (#:domains #:luv.domains))
  (:documentation
   "Luvcraft as a game on the Luft terrain: its block kinds as a palette, its
little world as a world source, and its saves replayed onto that source.

Luvcraft's Y is up and Luft's Z, so a Luvcraft cell X,Y,Z is the Luft cell
X+OFFSET, Z+OFFSET, Y.  The generator is Luvcraft's own code, run on the
terrain's worker, so the old program stays the oracle until it is deleted.")
  (:export #:*luvcraft-palette*
           #:luvcraft-palette
           #:luvcraft-palette-vocabulary
           #:block-placement
           #:luvcraft-world-source
           #:luvcraft-world-source-seed
           #:luvcraft-world-source-relief
           #:luvcraft-world-source-height
           #:luvcraft-world-source-offset
           #:luvcraft-cell-site
           #:make-luvcraft-world-source
           #:luvcraft-save-world-source
           #:make-luvcraft-streaming-scene))
