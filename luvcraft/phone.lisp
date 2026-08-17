;;; The phone: a wall terminal you can carry.
;;;
;;; #S27JKR is the design; this is what it turned into.
;;;
;;; A phone is the first thing the player's hand can hold (BODY.LISP), and
;;; what it does is what the wall terminals do: it shows a live shell.  The
;;; whole terminal display -- Ghostty terminal, PTY, glyph and cell runs,
;;; screen panel and faceplate glass -- is the one TERMINAL-WALL.LISP already
;;; builds; only its surface is new.  A wall's surface is a rectangle of
;;; blocks in the world.  The phone's is a rectangle in the *grip frame*, the
;;; hand's own coordinate space, where the screen never moves at all.  What
;;; moves is the hand, and so, seen from the phone, the camera: each frame
;;; the display draws against a frame uniform whose camera is the real one
;;; re-expressed in grip coordinates.  Nothing about the runs is rebuilt for
;;; motion; the same glyph instances that were right last frame are right
;;; this frame, because in their own space nothing happened.
;;;
;;; The phone is a focus like a wall is: TAB with the phone out enters it and
;;; keys go to its shell, shift-TAB leaves.  Putting the phone away pockets
;;; it with its shell still running; taking it out again brings the same
;;; shell back.

(in-package #:luvcraft)

;;; The slab, in the grip frame: 0.40 wide, 0.58 tall, 0.024 thin, held in
;;; front of the palm by its lower third with the thumb up one edge and the
;;; fingers round the other.  Comically large, which is the point, and also
;;; what makes a real terminal on it readable: the screen is proportioned
;;; for seventy-odd condensed columns.

(defparameter *phone-half-width* 0.220d0)
(defparameter *phone-half-height* 0.290d0)
(defparameter *phone-half-depth* 0.012d0)
(defparameter *phone-corner-radius* 0.030d0
  "How far the slab's corners are rounded, in cells; the screen's too.")
(defparameter *phone-center-height* 0.160d0
  "The slab's centre above the palm, which holds it by its lower third.")
(defparameter *phone-center-depth* -0.170d0
  "The slab's centre in front of the palm.")
(defparameter *phone-screen-half-width* 0.200d0)
(defparameter *phone-screen-half-height* 0.262d0)
(defparameter *phone-screen-depth* (+ *phone-center-depth* (- *phone-half-depth*))
  "The screen plane's z in the grip frame: the slab's face toward the eye.")

(defparameter *phone-terminal-rows* 46
  "How many rows of shell the phone shows; the columns follow its width.")
(defparameter *phone-terminal-margin* 0.008
  "The bezel between the screen's edge and its font grid, in cells.")

(defun phone-font-pathname (weight)
  "The phone's font: Input Mono Condensed from the user's fonts when it is
installed (its licence keeps it out of the repository), else the wall's.
WEIGHT is \"Regular\" or \"Bold\"."
  (let ((installed
          (merge-pathnames
           (format nil "Library/Fonts/InputMonoCondensed-~A.ttf" weight)
           (user-homedir-pathname))))
    (if (probe-file installed)
        installed
        (if (string= weight "Bold")
            *terminal-display-bold-font-pathname*
            *terminal-display-font-pathname*))))

;;; ---------------------------------------------------------------------
;;; The phone's screen as a terminal surface.

(defclass phone-screen-surface ()
  ((phone :initarg :phone :reader phone-screen-surface-phone))
  (:documentation
   "The phone's screen rectangle in the grip frame: right is grip x, up is
grip y, and outward is toward the eye, grip -z."))

(defmethod terminal-surface-axes ((surface phone-screen-surface))
  (values (make-vec3 1.0 0.0 0.0)
          (make-vec3 0.0 1.0 0.0)
          (make-vec3 0.0 0.0 -1.0)))

(defmethod terminal-surface-lower-left-point
    ((surface phone-screen-surface) &optional (offset 0.006))
  (make-vec3 (- *phone-screen-half-width*)
             (- *phone-center-height* *phone-screen-half-height*)
             (- *phone-screen-depth* offset)))

(defmethod terminal-surface-physical-width ((surface phone-screen-surface))
  (* 2 *phone-screen-half-width*))

(defmethod terminal-surface-physical-height ((surface phone-screen-surface))
  (* 2 *phone-screen-half-height*))

(defmethod terminal-surface-current-p
    ((surface phone-screen-surface) &optional session)
  (declare (ignore session))
  t)

(defmethod terminal-surface-focus-score
    ((surface phone-screen-surface) (session luvcraft-session))
  ;; The phone in hand is nearer than anything the crosshair could be on.
  (when (phone-in-hand-p (phone-screen-surface-phone surface) session)
    0.0))

(defmethod terminal-surface-focus-camera-pose
    ((surface phone-screen-surface) (session luvcraft-session))
  ;; The phone is wherever the hand is; the camera stays put and only the
  ;; narrowed field of view says the player is looking at it.
  nil)

;;; ---------------------------------------------------------------------
;;; The display: a terminal display whose camera lives in the hand.

(defclass phone-terminal-display (terminal-display)
  ((uniform-buffers :initform (make-hash-table :test #'eq)
                    :reader phone-terminal-display-uniform-buffers))
  (:documentation
   "A wall terminal display drawn on a phone screen: the same runs, drawn
against a per-frame uniform whose camera is expressed in the grip frame."))

(defmethod terminal-display-frame-uniform-buffer
    ((display phone-terminal-display) frame)
  (let ((buffers (phone-terminal-display-uniform-buffers display)))
    (or (gethash frame buffers)
        (setf (gethash frame buffers)
              (create (luvcraft-session-device
                       (terminal-display-session display))
                      (make-buffer-descriptor
                       :label "phone terminal frame uniform"
                       :size (block-world-camera-uniform-size
                              (terminal-display-session display))
                       :usage '(:uniform)))))))

(defun phone-camera-lanes (session width height)
  "The camera's five uniform lanes, expressed in the right hand's grip frame."
  (multiple-value-bind (palm grip-right grip-up grip-forward)
      (player-body-grip-frame session)
    (let* ((camera (luvcraft-session-camera session))
           (eye (camera-position camera)))
      (multiple-value-bind (right up forward) (camera-basis camera)
        (flet ((local (vector)
                 (make-vec3 (vec3-dot vector grip-right)
                            (vec3-dot vector grip-up)
                            (vec3-dot vector grip-forward))))
          (camera-lanes-uniform-data
           (local (make-vec3 (- (vec3-x eye) (vec3-x palm))
                             (- (vec3-y eye) (vec3-y palm))
                             (- (vec3-z eye) (vec3-z palm))))
           (local right) (local up) (local forward)
           (camera-field-of-view camera) width height))))))

(defmethod encode-luvcraft-overlay :around
    ((display phone-terminal-display) session pass surface-texture)
  (let ((phone (phone-screen-surface-phone (terminal-display-surface display))))
    (when (and (phone-in-hand-p phone session)
               (> (player-body-equip-amount (luvcraft-session-body session))
                  0.02d0))
      ;; The camera moved relative to the hand since last frame; say where
      ;; it is now, in the frame the screen was built in.
      (destructuring-bind (width height)
          (canvas-extent (luvcraft-session-context session))
        (write-buffer
         (terminal-display-frame-uniform-buffer
          display (luvcraft-frame-state session surface-texture))
         (frame-uniform-data
          session width height
          :camera-lanes (phone-camera-lanes session width height))))
      (call-next-method))))

(defmethod release-luvcraft-overlay :after ((display phone-terminal-display))
  (let ((buffers (phone-terminal-display-uniform-buffers display)))
    (maphash (lambda (frame buffer)
               (declare (ignore frame))
               (destroy buffer))
             buffers)
    (clrhash buffers)))

;;; ---------------------------------------------------------------------
;;; The phone itself.

(defclass phone ()
  ((display :initform nil :accessor phone-display)
   ;; 0 when the phone is merely held, 1 when its shell has the focus and
   ;; the hand has brought it square to the centre of the view; eased.
   (attention :initform 0d0 :type double-float :accessor phone-attention))
  (:documentation
   "A comically large phone: a graphite slab with a live shell on its face
toward the player.  Held in the right hand; TAB focuses its shell."))

(defmethod hand-item-name ((item phone)) "phone")
(defmethod hand-item-box-count ((item phone)) 10)

(defmethod hand-item-carry-pose ((item phone) body)
  (declare (ignore body))
  ;; Up in front of the face, screen toward the eye and a little to the
  ;; right of centre so the crosshair still shows past its edge; when its
  ;; shell has the focus, brought square to the centre and held a little
  ;; further off, where the narrowed field of view frames the whole screen.
  (lerp-pose '(0.26d0 -0.28d0 0.66d0 -0.16d0 -0.16d0 0.04d0)
             '(0.02d0 -0.19d0 0.96d0 -0.02d0 0.0d0 0.0d0)
             (phone-attention item)))

(defmethod advance-hand-item ((item phone) body seconds)
  (declare (ignore body))
  (let* ((display (phone-display item))
         (session (and display (terminal-display-session display)))
         (target (if (and session
                          (eq display (luvcraft-session-modal-focus session)))
                     1d0 0d0))
         (current (phone-attention item))
         (step (* 6d0 seconds)))
    (setf (phone-attention item)
          (cond ((> target current) (min target (+ current step)))
                ((< target current) (max target (- current step)))
                (t current)))))

(defmethod map-hand-item-boxes ((item phone) body function)
  (declare (ignore body))
  (let* ((hw *phone-half-width*)
         (hh *phone-half-height*)
         (hd *phone-half-depth*)
         (r *phone-corner-radius*)
         (cy *phone-center-height*)
         (cz *phone-center-depth*)
         (metal +phone-body-tile+))
    ;; The slab with its corners rounded: a cross of two boxes leaves a
    ;; notch at each corner, and a box turned forty-five degrees fills each
    ;; notch as a chamfer, which at arm's length reads as a radius.
    (funcall function 0d0 cy cz (- hw r) hh hd metal :stretch-p t)
    (funcall function 0d0 cy cz hw (- hh r) hd metal :stretch-p t)
    (let ((side (/ r (sqrt 2d0))))
      (dolist (corner '((-1 -1) (1 -1) (-1 1) (1 1)))
        (funcall function
                 (* (first corner) (- hw r)) (+ cy (* (second corner) (- hh r)))
                 cz side side hd metal :stretch-p t
                 :tilt (list 0d0 0d0 (/ pi 4)))))
    ;; A camera bump on the back, high on the left as seen from behind.
    (funcall function (- hw 0.06d0) (+ cy hh -0.07d0) (+ cz hd 0.006d0)
             0.030d0 0.045d0 0.006d0 metal :stretch-p t)
    ;; The grip takes the sides, never the screen: a thumb up the right
    ;; edge, fingers round the left edge with their tips just onto the
    ;; bezel.
    (funcall function (+ hw 0.018d0) 0.05d0 cz
             0.020d0 0.085d0 0.022d0 +player-skin-tile+
             :tilt '(0d0 0d0 -0.12d0))
    (funcall function (- (+ hw 0.018d0)) 0.01d0 cz
             0.020d0 0.075d0 0.030d0 +player-skin-tile+)
    (funcall function (+ (- hw) 0.008d0) 0.01d0 (- cz hd 0.005d0)
             0.012d0 0.070d0 0.008d0 +player-skin-tile+)))

(defun phone-in-hand-p (phone session)
  (eq phone (player-body-hand-item (luvcraft-session-body session))))

(defun ensure-phone-display (phone session)
  "The phone's terminal display, made -- with a shell in it -- on first use."
  (or (phone-display phone)
      (let ((surface (make-instance 'phone-screen-surface :phone phone))
            (display nil)
            (completed-p nil))
        (unwind-protect
             (multiple-value-bind (columns rows)
                 (terminal-grid-columns-for-rows
                  surface (phone-font-pathname "Regular")
                  *phone-terminal-margin* *phone-terminal-rows*)
               (setf display
                     (make-terminal-display
                      session surface columns rows
                      :class 'phone-terminal-display
                      :fixture ""
                      :margin *phone-terminal-margin*
                      :font-pathname (phone-font-pathname "Regular")
                      :bold-font-pathname (phone-font-pathname "Bold")
                      :screen-role :phone-screen
                      :faceplate-role :phone-glass))
               (attach-terminal-display-shell display)
               (setf (phone-display phone) display
                     completed-p t)
               display)
          (unless (or completed-p (null display))
            (remove-luvcraft-overlay session display))))))

(defmethod hand-item-taken-out ((item phone) body session)
  (declare (ignore body))
  (ensure-phone-display item session))

(defmethod hand-item-put-away ((item phone) body session)
  (declare (ignore body))
  ;; The shell keeps running in the pocket; only the focus lets go.
  (let ((display (phone-display item)))
    (when (and display (eq display (luvcraft-session-modal-focus session)))
      (unfocus-luvcraft-session session))))

(defun toggle-luvcraft-phone (session)
  "Take the session's phone out, or put it away."
  (toggle-hand-item (luvcraft-session-body session) session 'phone))
