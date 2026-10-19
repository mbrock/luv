(in-package #:luvcraft.agent.tests)

(define-test level-framing-puts-the-band-on-its-rows
  ;; Project the band's ends through the solved pinhole camera and land on
  ;; the asked rows, to rounding: the conversation view frames feet to cloud
  ;; exactly, whatever the viewport.
  (loop for (bottom top rows row-top row-bottom) in '((9d0 11.2d0 1188 48d0 846d0)
                                                      (0d0 1d0 600 20d0 300d0)
                                                      (-3d0 4d0 2000 100d0 1900d0))
        for fov = 0.87266463d0
        do (multiple-value-bind (distance height)
               (agent::solve-level-framing bottom top rows row-top row-bottom fov)
             (flet ((row (y)
                      (- (/ rows 2d0)
                         (* (- y height)
                            (/ (/ rows 2d0) (* distance (tan (/ fov 2d0))))))))
               (true (plusp distance))
               (true (< (abs (- (row bottom) row-bottom)) 1d-6))
               (true (< (abs (- (row top) row-top)) 1d-6))))))

(defparameter *two-summaries*
  (format nil "**Identifying block coordinates**~%~%I need to identify the block.~%~%~
               **Calculating movement toward player**~%~%I am analyzing the player.")
  "Reasoning summaries as the provider writes them: a bold title, then prose.")

(define-test thought-heading-is-the-latest-summary-title
  (true (string= "Calculating movement toward player"
                 (agent::thought-heading *two-summaries*)))
  (true (string= "Then walk over."
                 (agent::thought-heading "Look first. Then walk over.")))
  (false (agent::thought-heading ""))
  (false (agent::thought-heading nil)))

(define-test cloud-text-hides-model-handles
  (true (string= "facing north-east"
                 (agent::strip-handles "facing north-east #EABA")))
  (true (string= "grass at x=1 #note"
                 (agent::strip-handles "grass at x=1 #note #FABA"))))

(define-test conversation-camera-swings-round-a-wall
  ;; A gnome on open floor is seen from the player's side; build a wall
  ;; there and the camera finds an approach from which all of it shows.
  (let ((world (world:make-block-world :chunk-width 16 :chunk-height 8
                                       :chunk-depth 16))
        (gnome (make-instance 'agent:gnome
                              :session nil :x 8 :y 1 :z 8
                              :position (luvcraft::make-vec3 8.5d0 1d0 8.5d0))))
    (world:ensure-world-chunk world 0 0 0)
    (dotimes (x 16)
      (dotimes (z 16)
        (setf (world:world-block-at world x 0 z) (luvcraft:block-kind-named :stone))))
    ;; Yaw 0 looks toward +z, so its camera stands south of the gnome.
    (true (= 0 (agent::conversation-yaw gnome world 0d0 3d0 1.5d0)))
    (loop for x from 5 to 12
          do (loop for y from 1 to 4
                   do (setf (world:world-block-at world x y 6)
                            (luvcraft:block-kind-named :stone))))
    (let ((yaw (agent::conversation-yaw gnome world 0d0 3d0 1.5d0)))
      (false (= 0 yaw))
      (true (agent::sight-clear-p
             world
             (luvcraft::make-vec3 (- 8.5d0 (* 3d0 (sin yaw))) 1.5d0
                                  (- 8.5d0 (* 3d0 (cos yaw))))
             (agent::embodied-agent-face-position gnome))))))
