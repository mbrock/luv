(in-package #:luv.showcase)

;;; Focused inhabitants and player instruments, each photographed through its
;;; real interaction owner.  Fixture speech and menu state live here in source;
;;; no WORLD-AGENT or network-backed conversation is created. #W7T2IT #DYJZBK

(defconstant +character-focus-film-seconds+ 4)
(defconstant +character-focus-film-frame-rate+ 20)

(defun character-instrument-meadow-pose ()
  "A quiet midtone meadow view behind dialogue and instrument surfaces."
  (gallery-look-pose
   8.0d0 4.1d0 2.0d0
   8.0d0 2.8d0 8.0d0
   luvcraft::+luvcraft-camera-vertical-field-of-view+))

(defun make-character-instrument-meadow-world ()
  "The gazetteer meadow with a sparse, deterministic flower garden."
  (let ((world (luvcraft::make-gazetteer-meadow-world)))
    (dolist (coordinate '((6 1 7) (7 1 9) (9 1 6) (10 1 9)))
      (destructuring-bind (x y z) coordinate
        (setf (luvcraft:world-block-at world x y z)
              luvcraft::*flowers-block*)))
    (luvcraft:relight-block-world world)
    world))

(defun call-with-character-instrument-session
    (function &key title width height (day-fraction 0.42))
  "Call FUNCTION in the deterministic meadow with only subject-owned HUD."
  (call-with-gallery-session
   function
   :title title :width width :height height
   :world (make-character-instrument-meadow-world)
   :camera (make-gallery-camera (character-instrument-meadow-pose))
   :sky-clock (luvcraft::make-pinned-sky-clock day-fraction)
   :sky-profile (luvcraft:make-default-sky-profile)
   :critters (make-instance 'luvcraft::critter-population)
   :residency-radius 0 :clean-p t
   :exposure 0.52 :bloom-gain 0.12 :shaft-gain 0.18))

(defun call-with-focused-character
    (function spawn-function speech &key title width height day-fraction)
  "Focus one source-owned character and call FUNCTION with its exact pose."
  (call-with-character-instrument-session
   (lambda (session)
     (let ((character (funcall spawn-function session)))
       ;; SPAWN-EMBODIED-AGENT retains the player's travel heading.  This
       ;; authored conversation turns the subject toward the actual camera.
       (let ((eye (luvcraft:camera-position
                   (luvcraft:luvcraft-session-camera session))))
         (setf (agent::embodied-agent-facing-yaw character)
               (atan (- (luvcraft::vec3-x eye)
                        (luvcraft::body-x character))
                     (- (luvcraft::vec3-z eye)
                        (luvcraft::body-z character)))))
       ;; FOCUS-LUVCRAFT-SESSION creates the character's real dialogue overlay.
       ;; GNOME-SAY only publishes fixture text; unlike GNOME-ASK it creates no
       ;; WORLD-AGENT and performs no model or network work.
       (luvcraft:focus-luvcraft-session session character)
       (agent:gnome-say character speech)
       (funcall function session character
                (luvcraft:luvcraft-focus-camera-pose character session))))
   :title title :width width :height height :day-fraction day-fraction))

(defun spawn-capture-gnome (session)
  (agent:spawn-agent :session session :x 8 :y 2 :z 8))

(defun spawn-capture-cat (session)
  (agent:spawn-cat :session session :x 8 :y 2 :z 8))

(defun film-character-focus-pull (session pathname target-pose label)
  "Film the real focused character while the camera eases to TARGET-POSE."
  (let* ((camera (luvcraft:luvcraft-session-camera session))
         (frame-rate +character-focus-film-frame-rate+)
         (seconds +character-focus-film-seconds+)
         (step (/ 1d0 frame-rate)))
    (luvcraft:film-luvcraft-session
     session pathname :seconds seconds :frame-rate frame-rate
     :include-hud-p t :include-viewmodel-p nil
     :before-frame
     (lambda (frame)
       (luvcraft::advance-camera-focus camera target-pose step)
       (when (zerop (mod frame frame-rate))
         (format t "capture ~A: focus second ~D/~D~%"
                 label (1+ (/ frame frame-rate)) seconds)
         (finish-output))))))

;;; The focused FoV and dialogue are the gnome's own interaction, not a crop or
;;; a separately painted caption. #W7T2IT

(luv:define-capture gnome-garden-conversation
    (:figure W7T2IT :kind :image :extension "png"
     :description
     "A gnome at its real conversational FoV with source-owned dialogue.")
    (pathname)
  (call-with-focused-character
   (lambda (session gnome pose)
     (declare (ignore gnome))
     (luvcraft:capture-luvcraft-screenshot
      session pathname :camera-pose pose
      :include-hud-p t :include-viewmodel-p nil))
   #'spawn-capture-gnome
   "The ridge is warm now; the shaded path stays cool."
   :title "gnome garden conversation"
   :width +gallery-landscape-width+ :height +gallery-landscape-height+
   :day-fraction 0.42))

(luv:define-capture gnome-garden-focus-pull
    (:figure W7T2IT :kind :video :extension "mp4"
     :description
     "A short deterministic pull into the gnome's focused conversation view.")
    (pathname)
  (call-with-focused-character
   (lambda (session gnome pose)
     (declare (ignore gnome))
     (film-character-focus-pull
      session pathname pose "gnome-garden-focus-pull"))
   #'spawn-capture-gnome
   "I kept a little room between the flowers and the path."
   :title "gnome garden focus pull"
   :width +gallery-landscape-width+ :height +gallery-landscape-height+
   :day-fraction 0.42))

;;; CAT inherits the embodied-agent conversation protocol but retains its own
;;; SDF body, face position, audience distance, and focused camera pose.
;;; These portrait recipes enter that actual interaction. #W7T2IT

(luv:define-capture cat-in-the-sun-conversation
    (:figure W7T2IT :kind :image :extension "png" :layout :portrait
     :description
     "The seated cat in warm light, using its real focused dialogue surface.")
    (pathname)
  (call-with-focused-character
   (lambda (session cat pose)
     (declare (ignore cat))
     (luvcraft:capture-luvcraft-screenshot
      session pathname :camera-pose pose
      :include-hud-p t :include-viewmodel-p nil))
   #'spawn-capture-cat
   "The sunny stone is occupied.  The path may proceed around me."
   :title "cat in the sun conversation"
   :width +gallery-portrait-width+ :height +gallery-portrait-height+
   :day-fraction 0.38))

(luv:define-capture cat-in-the-sun-focus-pull
    (:figure W7T2IT :kind :video :extension "mp4" :layout :portrait
     :description
     "A portrait pull into the cat's own audience distance and focused FoV.")
    (pathname)
  (call-with-focused-character
   (lambda (session cat pose)
     (declare (ignore cat))
     (film-character-focus-pull
      session pathname pose "cat-in-the-sun-focus-pull"))
   #'spawn-capture-cat
   "I have inspected the meadow.  It is acceptable."
   :title "cat in the sun focus pull"
   :width +gallery-portrait-width+ :height +gallery-portrait-height+
   :day-fraction 0.38))

;;; Player instruments remain live McCLIM frames composited into the one game
;;; canvas.  The fixtures below configure those owners directly. #DYJZBK

(defun configure-capture-metabar (session)
  "Open the real metabar on grading and select its light-shafts knob."
  (let* ((overlay (mcluv::open-luvcraft-metabar session))
         (frame (mcluv:widget-overlay-frame overlay))
         (knob (or (luvcraft:find-knob 'luvcraft::shaft-gain)
                   (error "The grading metabar has no SHAFT-GAIN knob."))))
    (mcluv::metabar-open-group-row overlay :grading)
    (let ((index (position knob (mcluv::metabar-rows frame)
                           :key #'second :test #'eq)))
      (unless index
        (error "The SHAFT-GAIN knob is absent from the open grading group."))
      (mcluv::metabar-select overlay index))
    ;; A still wants the fully open endpoint, not wall-clock easing captured at
    ;; an arbitrary phase.
    (setf (mcluv::metabar-slide overlay) 1d0
          (mcluv::metabar-last-time overlay) 0d0)
    (mcluv::repaint-metabar frame)
    overlay))

(luv:define-capture metabar-grading
    (:figure DYJZBK :kind :image :extension "png"
     :description
     "The real metabar's grading group, with light shafts selected in context.")
    (pathname)
  (call-with-character-instrument-session
   (lambda (session)
     (configure-capture-metabar session)
     (luvcraft:capture-luvcraft-screenshot
      session pathname :include-hud-p t :include-viewmodel-p nil))
   :title "metabar grading"
   :width +gallery-landscape-width+ :height +gallery-landscape-height+
   :day-fraction 0.42))

(defun configure-capture-command-menu (session)
  "Open the real M-x menu with a useful multi-result partial query."
  (let* ((overlay (luvcraft.clim:open-luvcraft-command-menu session))
         (frame (mcluv:widget-overlay-frame overlay)))
    (setf (luvcraft.clim::command-menu-query frame) "toggle"
          (luvcraft.clim::command-menu-selected frame) 0)
    (let ((matches (luvcraft.clim::command-menu-results frame)))
      (unless (>= (length matches) 3)
        (error "M-x fixture query TOGGLE produced only ~D match~:P."
               (length matches))))
    (luvcraft.clim::repaint-command-menu frame)
    overlay))

(luv:define-capture command-menu-filter
    (:figure DYJZBK :kind :image :extension "png"
     :description
     "The real M-x command table filtered by a useful partial query.")
    (pathname)
  (call-with-character-instrument-session
   (lambda (session)
     (configure-capture-command-menu session)
     (luvcraft:capture-luvcraft-screenshot
      session pathname :include-hud-p t :include-viewmodel-p nil))
   :title "command menu filter"
   :width +gallery-landscape-width+ :height +gallery-landscape-height+
   :day-fraction 0.42))
