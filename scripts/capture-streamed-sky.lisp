;;;; Headless software-Vulkan evidence in both streamed games.
;;;; XDG_RUNTIME_DIR=/tmp/luv-runtime ./env --slim sbcl --script scripts/capture-streamed-sky.lisp
(require :asdf)
(asdf:load-system "luvcraft/luft/game")

(defpackage #:capture-streamed-sky
  (:use #:cl)
  (:local-nicknames (#:terrain #:luft.terrain) (#:render #:luft.render)
                    (#:v #:luv.arithmetic.lisp.vec3)))
(in-package #:capture-streamed-sky)

(setf render::*sky-minutes-per-day* nil render:*sky-hour* 12.0
      render::*projection* :perspective)

(defun overhang-pose (scene spawn-x spawn-y)
  "Find a generated roof above walkable air, seen from an open-sky approach."
  (let* ((domain (terrain:scene-domain scene))
         (materials (terrain:scene-material-cells scene))
         (best nil) (best-score most-positive-fixnum))
    (labels ((occupied (x y z)
               (nth-value 1 (terrain:material-cell-at
                             materials (luft:make-site domain x y z luft:+cell-extent+ 1))))
             (open-sky (x y z)
               (let* ((key (luft:chunk-key-at x y))
                      (table (terrain::streaming-scene-chunk-material-table scene key)))
                 (when table
                   (let ((column (aref (terrain::chunk-sky-columns
                                        table (terrain:scene-authored-light-opacity-table scene))
                                       (+ (mod x 64) (* 64 (mod y 64))))))
                     (or (null column) (>= z (length column))))))))
      (terrain::map-material-cells
       (lambda (cell material)
         (declare (ignore material))
         (let* ((x (luft:site-x cell)) (y (luft:site-y cell))
                (z (1+ (luft:site-z cell)))
                (score (+ (abs (- x spawn-x)) (abs (- y spawn-y)))))
           (when (and (< score best-score) (< score 100) (< z 230)
                      (> x 8) (> y 8)
                      (< x (- (luft:world-domain-x-limit domain) 8))
                      (< y (- (luft:world-domain-y-limit domain) 8))
                      (loop for dz below 3 never (occupied x y (+ z dz)))
                      (loop for dz from 3 to 8 thereis (occupied x y (+ z dz))))
             (dolist (direction '((1 0) (-1 0) (0 1) (0 -1)))
               (destructuring-bind (dx dy) direction
                 (when (and (open-sky (+ x (* 6 dx)) (+ y (* 6 dy)) (+ z 1))
                            (loop for step from 1 to 6
                                  always (and (not (occupied (+ x (* step dx)) (+ y (* step dy)) z))
                                              (not (occupied (+ x (* step dx)) (+ y (* step dy)) (1+ z))))))
                   (setf best (list x y z dx dy) best-score score)))))))
       materials))
    (or best (error "No generated overhang found near spawn."))))

(defun capture-world (name scene &key focus)
  (multiple-value-bind (x y z) (terrain:world-source-spawn (terrain:streaming-scene-source scene))
    (when focus (setf x (first focus) y (second focus) z (third focus)))
    (let* ((camera (render:make-fly-camera
                    :position (v:make-vec3 (+ x 0.5) (- y 6.0) (+ z 3.0))
                    :yaw (/ pi 2) :pitch -0.12))
           (viewer (render:start-viewer
                    :solid scene :camera camera :player-p nil :world-pathname nil
                    :width 960 :height 640 :fixed-exposure 0.65 :frames-per-second 15)))
      (unwind-protect
           (progn
             ;; Chunk generation runs in real time, not in a tight screenshot loop.
             (sleep 30)
             (format t "~&SKY-CAPTURE ~A spawn=~S resident=~D published=~D~%"
                     name (list x y z)
                     (hash-table-count (terrain:streaming-scene-store scene))
                     (hash-table-count (terrain:streaming-scene-loaded scene)))
             (destructuring-bind (cx cy cz dx dy) (overhang-pose scene x y)
               (format t "SKY-CAPTURE ~A overhang=~S~%" name (list cx cy cz dx dy))
               (luv::call-on-sdl-canvas-thread
                (render::viewer-canvas viewer)
                (lambda ()
                  (setf (render:camera-position camera)
                        (v:make-vec3 (+ cx 0.5 (* 6 dx)) (+ cy 0.5 (* 6 dy)) (+ cz 2.0))
                        (render:camera-yaw camera) (atan (- dy) (- dx))
                        (render:camera-pitch camera) -0.12))))
             (sleep 3)
             (dolist (hour '(12.0 18.5))
               (setf render:*sky-hour* hour)
               (sleep 1)
               (render:capture-viewer-frame
                (format nil ".amp/in/artifacts/~A-~A.png" name
                        (if (= hour 12.0) "day" "dusk")) viewer)))
        (render:stop-viewer viewer)))))

(ensure-directories-exist #P".amp/in/artifacts/sky.png")
(capture-world "luvcraft-sky"
               (terrain:make-world-source-streaming-scene
                (luvcraft.luft:make-luvcraft-world-source :seed 7)
                :mesh-preparer render:+render-population-preparer+))
(capture-world "atelier-sky"
               (luft.world:make-authored-world-streaming-scene
                :mesh-preparer render:+render-population-preparer+)
               :focus '(1462 650 24))
