;;; The party: how the aspects compose into one scene.
;;;
;;; CELEBRATE-BIRTHDAY is the only place that knows the whole arrangement:
;;; where the gazebo stands on the meadow, how the balloons tie to its posts,
;;; where the gnomes dance, where the funny balls scatter, and when the sky
;;; settles into the pinned dusk the fireworks want.  Each aspect stays
;;; ignorant of the others; changing the composition means editing only this
;;; file.
;;;
;;; The world lives under its own save file beside the ordinary one, so the
;;; everyday game is untouched and the party can be reopened any time.

(in-package #:luvcraft.birthday)

(defun birthday-world-pathname ()
  "The party world's save, beside the ordinary world under its own name."
  (merge-pathnames #P"alex-birthday.sexp"
                   (luvcraft::default-luvcraft-world-pathname)))

(defparameter *party-sky-hour* 18.0
  "Pinned time of day: past sunset, before deep night, so a warm band
still hangs on the horizon while the stars and fireworks read against
the indigo above it.")

(defun birthday-surface-height (world x z)
  "The y of the highest solid block in the column, probing downward."
  (loop for y from 30 downto 0
        for block = (luvcraft:world-block-at world x y z)
        when block return y
        finally (return 6)))

(defun balloon-plan (anchors)
  "Balloon plists from the gazebo's anchors: one balloon floating above
each post, and a bouquet crowding the roof finial."
  (let ((balloons '())
        (hue 0.0))
    (loop for (x y z) in (getf anchors :post-tops)
          do (push (list :x (+ x 0.0) :y (+ y 3.4) :z (+ z 0.0)
                         :radius 0.5 :hue hue :phase (* hue 7.0))
                   balloons)
             (incf hue 0.13))
    (destructuring-bind (x y z) (getf anchors :roof-apex)
      (loop for (dx dy dz) in '((0.0 1.8 0.0) (-0.9 2.3 0.4) (0.8 2.6 -0.3)
                                (0.3 3.0 0.8) (-0.5 2.0 -0.8))
            do (push (list :x (+ x dx) :y (+ y dy) :z (+ z dz)
                           :radius 0.55 :hue hue :phase (* hue 9.0))
                     balloons)
               (incf hue 0.17)))
    balloons))

(defun gnome-ring (world center-x center-z &key (count 6) (radius 10.5))
  "Gnome plists standing in a ring on the lawn around the gazebo."
  (loop for index below count
        for angle = (/ (* 2 pi index) count)
        for x = (+ center-x (* radius (cos angle)))
        for z = (+ center-z (* radius (sin angle)))
        for ground = (birthday-surface-height world (round x) (round z))
        collect (list :x (float x) :y (float (1+ ground)) :z (float z)
                      :scale (+ 0.85 (* 0.3 (/ index count)))
                      :phase (float (/ index count))
                      :hue (float (/ index count)))))

(defun celebrate-birthday (&key (name "ALEX") fullscreen-p
                                (provider luv:*gpu-provider*))
  "Throw the party: open the birthday meadow and decorate it.

Loads or creates the party world under its own save file, raises the gazebo,
ties on the balloons, calls in the gnomes, scatters the funny balls, hangs
the greeting in the air, pins the sky at dusk, and starts the fireworks.
Returns the session; LUVCRAFT:STOP-PLAYING ends the party and saves it."
  (when luvcraft:*session*
    (luvcraft:stop-playing))
  (let ((pathname (birthday-world-pathname)))
    (multiple-value-bind (world resume-description)
        (if (probe-file pathname)
            (luvcraft:read-luvcraft-save pathname)
            (values (make-birthday-world) nil))
      ;; The lawn must be resident before the gazebo's edits can land.
      ;; Rebuilding over an existing gazebo rewrites the same cells with the
      ;; same blocks, so the build is idempotent across reopenings.
      (luvcraft:center-little-world-residency
       (luvcraft::block-world-source world) world 0 0 :radius 3)
      ;; The meadow promises a flat lawn at *CLEARING-HEIGHT*, so the gazebo
      ;; is told its ground rather than probing for it: the probe scans the
      ;; column from above the top chunk layer, which a restored world's
      ;; space rejects rather than reading as absent.
      (let ((anchors (build-birthday-gazebo world :center-x 0 :center-z 0
                                                  :ground-y
                                                  (round *clearing-height*))))
        (luvcraft:relight-block-world world)
        (multiple-value-bind (camera player selected-block carried)
            (luvcraft::restore-luvcraft-resume-save-description
             resume-description)
          (declare (ignore selected-block carried))
          ;; A fresh party spawns at the vantage that frames gazebo and
          ;; greeting together; a resumed one keeps its player where Alex
          ;; last stood.
          (unless resume-description
            (setf camera (make-instance 'luvcraft:fly-camera
                                        :position (luvcraft::make-vec3
                                                   0d0 10.5d0 -21d0)
                                        :yaw 0.0d0 :pitch 0.08d0)
                  player (luvcraft:make-player-for-camera camera)))
          (let ((writer (luvcraft::make-world-checkpoint-writer pathname))
                (session nil))
            (unwind-protect
                 (setf session
                       (luvcraft:start-luvcraft
                        :provider provider
                        :title (format nil "luvcraft — ~:(~a~)'s birthday!"
                                       name)
                        :world world :camera camera :player player
                        :fullscreen-p fullscreen-p
                        :world-text-string
                        (format nil "HAPPY BIRTHDAY ~:@(~a~)!" name)
                        ;; The banner must hang nearer than the gazebo's
                        ;; front eave or the roof occludes it: the camera
                        ;; stands 21 cells out, so 11 keeps the text plane
                        ;; well in front with the greeting clear of the
                        ;; apex sightline.
                        :world-text-distance 11.0
                        :world-text-lift 6.0
                        :world-text-units-per-em 0.32
                        :checkpoint-writer writer))
              (unless session
                (luvcraft::stop-world-checkpoint-writer writer)))
            (let ((clock (luvcraft:luvcraft-session-sky-clock session)))
              (setf (luvcraft::sky-clock-hour clock) *party-sky-hour*
                    (luvcraft:sky-clock-paused-p clock) t))
            ;; The lobby's corner instrument has no place at a party.
            (dolist (overlay (luvcraft::luvcraft-session-overlays session))
              (when (typep overlay 'mcluv::luvcraft-lobby-hud-overlay)
                (luvcraft:remove-luvcraft-overlay session overlay)))
            (add-birthday-balloons session (balloon-plan anchors))
            (add-dancing-gnomes session (gnome-ring world 0 0))
            (luvcraft::scatter-party-balls session :center '(3 8 -13)
                                                   :count 24)
            ;; Shells rise from high behind the roof: from the lawn the
            ;; launch point hides behind the gazebo, rockets crest the
            ;; ridge, and every burst opens in the clear sky above the
            ;; finial's balloon bouquet rather than behind the cap.
            (destructuring-bind (x y z) (getf anchors :roof-apex)
              (declare (ignore y))
              (add-birthday-fireworks session
                                      :origin (list (+ x 3.0) 24.0
                                                    (+ z 20.0))))
            (setf luvcraft::*checkpoint-writer* writer
                  luvcraft:*session* session)
            session))))))
