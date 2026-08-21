(in-package #:luv.showcase)

;;; Two authored play sequences through the ordinary centre-ray edit verb.
;;; The wall and scaffold are merely deterministic terrain; every filmed
;;; removal and placement still crosses LUVCRAFT's player mutation, particle,
;;; lighting, mesh-publication, and checkpoint-notification path. #B4K7WR #P3L8YX

(defparameter *gameplay-smash-targets*
  '((14 3 11) (15 3 11) (16 3 11)
    (16 2 11) (15 2 11) (14 2 11)
    (14 1 11) (15 1 11) (16 1 11))
  "The brick panel swept from top left to bottom right by the mining film.")

(defparameter *gameplay-arch-targets*
  '((13 1 11) (17 1 11)
    (13 2 11) (17 2 11)
    (13 3 11) (17 3 11)
    (13 4 11) (14 4 11) (15 4 11) (16 4 11) (17 4 11))
  "The two posts and lintel placed by the building film, in play order.")

(defun make-gameplay-action-yard ()
  "Make the fully resident grass yard shared by the two action films."
  (let ((world
          (luvcraft::make-block-world
           :source (make-instance 'luvcraft::gazetteer-open-sky-source))))
    (dotimes (chunk-x 2)
      (luvcraft::ensure-world-chunk world chunk-x 0 0))
    (loop for x below 32 do
      (loop for z below 16 do
        (setf (luvcraft:world-block-at world x 0 z)
              luvcraft::*grass-block*)))
    world))

(defun make-gameplay-smash-wall-world ()
  "Make a stone wall with one contrasting three-by-three brick panel."
  (let ((world (make-gameplay-action-yard)))
    (loop for x from 12 to 18 do
      (loop for y from 1 to 5 do
        (setf (luvcraft:world-block-at world x y 11)
              luvcraft::*stone-block*)))
    (dolist (coordinate *gameplay-smash-targets*)
      (destructuring-bind (x y z) coordinate
        (setf (luvcraft:world-block-at world x y z)
              luvcraft::*bricks-block*)))
    ;; The opening reveals a luminous landmark instead of undifferentiated
    ;; empty ground after the brick panel is gone.
    (setf (luvcraft:world-block-at world 15 1 14)
          luvcraft:*crystal-block*)
    (luvcraft:relight-block-world world)
    world))

(defun make-gameplay-arch-world ()
  "Make an open yard with an arch-shaped wooden placement scaffold."
  (let ((world (make-gameplay-action-yard)))
    ;; Each wood cell is exactly one cell behind its eventual brick.  A ray
    ;; from the authored play camera therefore hits its front face and asks
    ;; EDIT-LUVCRAFT-BLOCK to place the brick at Z=11.  The doorway's centre
    ;; has no scaffold, so it remains genuinely open in the finished shot.
    (dolist (coordinate *gameplay-arch-targets*)
      (destructuring-bind (x y z) coordinate
        (setf (luvcraft:world-block-at world x y (1+ z))
              luvcraft::*wood-block*)))
    (setf (luvcraft:world-block-at world 15 1 14)
          luvcraft:*crystal-block*)
    (luvcraft:relight-block-world world)
    world))

(defun gameplay-action-camera-pose
    (targets frame first-action-frame frames-between-actions)
  "Pan the first-person camera smoothly between centre-ray TARGETS."
  (let* ((last-index (1- (length targets)))
         (position
           (min (coerce last-index 'double-float)
                (max 0d0
                     (/ (- frame first-action-frame)
                        (coerce frames-between-actions 'double-float)))))
         (left-index (floor position))
         (right-index (min last-index (1+ left-index)))
         (amount (- position left-index))
         (left (elt targets left-index))
         (right (elt targets right-index)))
    (flet ((blend (index)
             (+ (nth index left)
                (* amount (- (nth index right) (nth index left)))
                0.5d0)))
      (gallery-look-pose
       15.5d0 2.62d0 5.5d0
       (blend 0) (blend 1) (blend 2)
       (* 0.92d0 luvcraft::+luvcraft-camera-vertical-field-of-view+)))))

(defun wait-for-gameplay-action-publication (session title &key (timeout 10d0))
  "Drive owner-side publication, reporting once a second until settled."
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout internal-time-units-per-second))))
        (next-report (+ (get-internal-real-time)
                        internal-time-units-per-second)))
    (loop
      ;; FILM-LUVCRAFT-SESSION calls this helper between encoded frames.  An
      ;; edit has dirtied lighting at that point, but no ordinary frame can
      ;; reconcile it until BEFORE-FRAME returns.  Give the canvas owner the
      ;; same refresh turn explicitly, then inspect the resulting boundary.
      do (luv:request-canvas-frame
          (luvcraft:luvcraft-session-canvas session)
          (lambda (timestamp)
            (declare (ignore timestamp))
            (luvcraft:refresh-luvcraft-mesh session)))
      for state = (luvcraft::luvcraft-streaming-trace-state session)
      do (when (plusp (getf state :errors))
           (error "Capture ~A production failed while publishing: ~S"
                  title state))
         (when (luvcraft::luvcraft-streaming-trace-state-quiescent-p state)
           (return state))
         (when (>= (get-internal-real-time) next-report)
           (format t
                   "capture ~A: publishing (~D outstanding, ~D staged)...~%"
                   title (getf state :outstanding) (getf state :staged))
           (finish-output)
           (incf next-report internal-time-units-per-second))
         (when (>= (get-internal-real-time) deadline)
           (error "Capture ~A did not publish within ~,2F seconds: ~S"
                  title timeout state))
         (sleep 0.01))))

(defun perform-gameplay-centre-edit (session action expected-coordinate title)
  "Perform and verify one ordinary player ACTION at EXPECTED-COORDINATE."
  (multiple-value-bind (coordinate status)
      (luvcraft:edit-luvcraft-block session action)
    (unless (and (eq status :edited)
                 coordinate
                 (= (luvcraft::world-coordinate-x coordinate)
                    (first expected-coordinate))
                 (= (luvcraft::world-coordinate-y coordinate)
                    (second expected-coordinate))
                 (= (luvcraft::world-coordinate-z coordinate)
                    (third expected-coordinate)))
      (error "Capture ~A expected ~S at ~S, got ~S at ~S."
             title action expected-coordinate status
             (and coordinate
                  (list (luvcraft::world-coordinate-x coordinate)
                        (luvcraft::world-coordinate-y coordinate)
                        (luvcraft::world-coordinate-z coordinate)))))
    (format t "capture ~A: ~(~A~) at (~{~D~^, ~}); publishing...~%"
            title action expected-coordinate)
    (finish-output)
    ;; Do not let worker timing choose which filmed frame first contains the
    ;; edit.  The owner publishes the complete current mesh cohort before this
    ;; action's next frame is encoded.
    (wait-for-gameplay-action-publication session title)
    (format t "capture ~A: edit published~%" title)
    (finish-output)
    coordinate))

(defun call-with-gameplay-action-session
    (function title world initial-pose &key selected-block)
  "Call FUNCTION in one fully resident gameplay-shaped hidden session."
  (call-with-gallery-session
   (lambda (session)
     (when selected-block
       (setf (luvcraft:luvcraft-session-selected-block session)
             selected-block))
     (wait-for-gameplay-action-publication session title)
     (funcall function session))
   :title title
   :world world
   :camera (make-gallery-camera initial-pose)
   :sky-clock (luvcraft::make-pinned-sky-clock 0.41)
   :sky-profile (luvcraft:make-default-sky-profile)
   :width +gallery-landscape-width+
   :height +gallery-landscape-height+
   :residency-radius 0
   :clean-p nil
   :exposure 0.50
   :bloom-gain 0.20
   :shaft-gain 0.22))

(luv:define-capture gameplay-smash-wall
    (:figure P3L8YX :kind :video :extension "mp4"
     :description
     "First-person mining opens a brick panel with ordinary fragment bursts.")
    (pathname)
  (let ((frame-rate +gallery-film-frame-rate+)
        (seconds 7)
        (first-action-frame 20)
        (frames-between-actions 10))
    (call-with-gameplay-action-session
     (lambda (session)
       (luvcraft:film-luvcraft-session
        session pathname :seconds seconds :frame-rate frame-rate
        :include-hud-p t :include-viewmodel-p t
        :before-frame
        (lambda (frame)
          (luvcraft::set-camera-pose
           (luvcraft:luvcraft-session-camera session)
           (gameplay-action-camera-pose
            *gameplay-smash-targets* frame
            first-action-frame frames-between-actions))
          (luvcraft::advance-luvcraft-session-to
           session (/ frame (coerce frame-rate 'double-float)))
          (when (and (>= frame first-action-frame)
                     (zerop (mod (- frame first-action-frame)
                                 frames-between-actions)))
            (let ((action-index
                    (/ (- frame first-action-frame)
                       frames-between-actions)))
              (when (< action-index (length *gameplay-smash-targets*))
                (perform-gameplay-centre-edit
                 session :remove
                 (elt *gameplay-smash-targets* action-index)
                 "gameplay-smash-wall"))))
          (when (zerop (mod frame frame-rate))
            (format t "capture gameplay-smash-wall: second ~D/~D~%"
                    (1+ (/ frame frame-rate)) seconds)
            (finish-output)))))
     "gameplay-smash-wall"
     (make-gameplay-smash-wall-world)
     (gameplay-action-camera-pose
      *gameplay-smash-targets* 0 first-action-frame frames-between-actions))))

(luv:define-capture gameplay-build-brick-arch
    (:figure P3L8YX :kind :video :extension "mp4"
     :description
     "First-person placement raises two brick posts and closes their lintel.")
    (pathname)
  (let ((frame-rate +gallery-film-frame-rate+)
        (seconds 8)
        (first-action-frame 20)
        (frames-between-actions 10)
        (scaffold-targets
          (mapcar (lambda (coordinate)
                    (list (first coordinate)
                          (second coordinate)
                          (1+ (third coordinate))))
                  *gameplay-arch-targets*)))
    (call-with-gameplay-action-session
     (lambda (session)
       (luvcraft:film-luvcraft-session
        session pathname :seconds seconds :frame-rate frame-rate
        :include-hud-p t :include-viewmodel-p t
        :before-frame
        (lambda (frame)
          (luvcraft::set-camera-pose
           (luvcraft:luvcraft-session-camera session)
           (gameplay-action-camera-pose
            scaffold-targets frame
            first-action-frame frames-between-actions))
          (luvcraft::advance-luvcraft-session-to
           session (/ frame (coerce frame-rate 'double-float)))
          (when (and (>= frame first-action-frame)
                     (zerop (mod (- frame first-action-frame)
                                 frames-between-actions)))
            (let ((action-index
                    (/ (- frame first-action-frame)
                       frames-between-actions)))
              (when (< action-index (length *gameplay-arch-targets*))
                (perform-gameplay-centre-edit
                 session :place
                 (elt *gameplay-arch-targets* action-index)
                 "gameplay-build-brick-arch"))))
          (when (zerop (mod frame frame-rate))
            (format t "capture gameplay-build-brick-arch: second ~D/~D~%"
                    (1+ (/ frame frame-rate)) seconds)
            (finish-output)))))
     "gameplay-build-brick-arch"
     (make-gameplay-arch-world)
     (gameplay-action-camera-pose
      scaffold-targets 0 first-action-frame frames-between-actions)
     :selected-block luvcraft::*bricks-block*)))
