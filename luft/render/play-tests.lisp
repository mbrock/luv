(in-package #:luft.render.tests)

(define-test authored-world-spawns-in-view-of-a-summit-away-from-the-corner
  (let* ((scene (render:make-authored-world-streaming-scene))
         (source (render::streaming-scene-source scene)))
    (multiple-value-bind (x y yaw) (render::large-world-scenic-spawn source)
      (true (>= x 192) "the spawn is well inside the corner")
      (true (= y (round (render::large-world-road-centre-y x)))
            "the spawn stands on the road")
      (true (typep yaw 'single-float))
      (true (render::large-world-spawn-view source x y)
            "a snow-line summit is in plain sight from there"))))

(define-test flying-characters-rise-hover-and-fall-when-they-land
  (let* ((scene (render:make-authored-world-streaming-scene))
         (source (render::streaming-scene-source scene))
         (player (render::make-scene-walking-character scene))
         (simulation (render::make-world-simulation scene))
         (body (render::character-body player))
         (position (render::body-position body))
         (key (luft:chunk-key-at (floor (vec3-x position)) (floor (vec3-y position)))))
    (setf (gethash key (render::streaming-scene-store scene))
          (render::materialize-authored-world-chunk source key 1))
    (render::rebuild-authored-world-resident-values scene (list key))
    (render::add-simulation-character simulation player)
    (let ((ground (vec3-z position)))
      (render::set-character-flying player t)
      (render::set-character-vertical-urge player 1.0)
      (dotimes (i 10) (render::advance-world-simulation simulation 0.1))
      (true (> (vec3-z position) (+ ground 3.0)) "flight rises under urge")
      (render::set-character-vertical-urge player 0.0)
      (dotimes (i 5) (render::advance-world-simulation simulation 0.1))
      (let ((hover (vec3-z position)))
        (dotimes (i 5) (render::advance-world-simulation simulation 0.1))
        (true (< (abs (- hover (vec3-z position))) 0.01) "flight holds height"))
      (render::set-character-flying player nil)
      (dotimes (i 30) (render::advance-world-simulation simulation 0.1))
      (true (< (abs (- ground (vec3-z position))) 0.01)
            "walking again falls back to the ground"))))

(define-test luft-world-saves-replay-their-edits-into-a-fresh-source
  (let* ((pathname (uiop:with-temporary-file (:pathname path :keep t) path))
         (description
           (list :luft-world 1 :seed 121 :x-bits 11 :y-bits 11
                 :edits '((300 140 20 :terminal) (301 140 20 nil))
                 :yaw 1.0 :pitch 0.0 :sky-hour 12.0
                 :player '(300.5 141.5 21.0) :flying-p t)))
    (unwind-protect
         (let ((render::*sky-hour* render::*sky-hour*))
           (render::write-luft-world-description description pathname)
           (true (equalp description
                         (render::read-luft-world-description pathname)))
           (let* ((scene (render:make-authored-world-streaming-scene))
                  (source (render::streaming-scene-source scene))
                  (domain (render::authored-world-source-domain source)))
             (true (render::load-luft-world-description scene pathname))
             (multiple-value-bind (placement present-p)
                 (render::authored-world-edit-at
                  source (luft:make-site domain 300 140 20 luft:+cell-extent+ 1))
               (true present-p)
               (true (eq render::*terminal-material-placement* placement)))
             (multiple-value-bind (placement present-p)
                 (render::authored-world-edit-at
                  source (luft:make-site domain 301 140 20 luft:+cell-extent+ 1))
               (true present-p "a removal is an edit too")
               (true (null placement))))
           ;; Another seed's save is not applied to this landscape.
           (render::write-luft-world-description
            (list* :luft-world 1 :seed 7 (cddddr description)) pathname)
           (let ((scene (render:make-authored-world-streaming-scene)))
             (true (null (handler-bind ((warning #'muffle-warning))
                           (render::load-luft-world-description
                            scene pathname))))))
      (uiop:delete-file-if-exists pathname))))
