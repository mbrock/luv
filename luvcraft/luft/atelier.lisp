;;;; The Luft atelier as a stand-in host for Luvcraft on Luft: it walks,
;;;; streams, and edits any world source, so a Luvcraft world opens in it
;;;; before Luvcraft has a host of its own.

(in-package #:luvcraft.luft)

(export 'play-in-atelier)

(defun play-in-atelier (&rest viewer-arguments
                        &key (pathname (luvcraft::default-luvcraft-world-pathname))
                          seed (relief :alpine)
                          (height luvcraft::*little-world-height*)
                        &allow-other-keys)
  "Open a Luvcraft world in the Luft atelier and return its viewer.

The world is the Luvcraft save at PATHNAME when it exists, else a fresh
little world of SEED, RELIEF, and HEIGHT.  Edits made in the atelier are not
yet saved back.  Other keys go to LUFT.RENDER:START-VIEWER."
  (let ((source (if (and pathname (null seed) (probe-file pathname))
                    (luvcraft-save-world-source pathname)
                    (make-luvcraft-world-source
                     :seed (or seed 1) :relief relief :height height))))
    (apply #'luft.render:start-viewer
           :solid (make-luvcraft-streaming-scene
                   source :mesh-preparer luft.render::+render-population-preparer+)
           :world-pathname nil
           :title "Luvcraft on LUFT — click to play · WASD move · Space jump, twice to fly · L/R edit · Esc release"
           (alexandria:remove-from-plist viewer-arguments
                                         :pathname :seed :relief :height))))
