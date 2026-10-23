;;;; Native old/new software-Vulkan framing and focus-input proof.
;;;; LUV_GHOSTTY_LIBRARY=/path/to/libghostty-vt.so ./env --slim sbcl --script scripts/capture-terminal-camera.lisp
(require :asdf)
(asdf:load-system "luvcraft/luft/game")
(asdf:load-system "luvcraft")
(luv.ghostty:load-libghostty-vt)
(setf luv::*canvas-watchdog-fatal-seconds* nil)

(defpackage #:terminal-camera-proof
  (:use #:cl)
  (:local-nicknames (#:old #:luvcraft) (#:host #:luvcraft.luft.game)
                    (#:game #:luvcraft.luft) (#:r #:luft.render)
                    (#:v #:luv.arithmetic.lisp.vec3) (#:terrain #:luft.terrain)))
(in-package #:terminal-camera-proof)

(defvar *old-poses* (make-hash-table :test #'equal))

(defun world ()
  (let* ((world (old:make-empty-little-block-world :seed 7 :chunk-height 16 :relief :meadow))
         (edits (old:little-world-source-edits (old:block-world-source world))))
    (loop for x below 16 do
      (loop for z below 16 do
        (loop for y below 16 do
          (old::record-block-edit edits
                                  (cond ((= y 3) old::*planks-block*)
                                        ((and (= z 10) (<= 5 x 11) (<= 4 y 7))
                                         old::*terminal-block*))
                                  x y z))))
    world))

(defun key (owner canvas name &key modifiers character)
  (dolist (class '(luv:canvas-key-press-event luv:canvas-key-release-event))
    (luv:handle-canvas-event
     owner canvas (make-instance class :timestamp 0 :key-name name :modifiers modifiers
                                      :character character :unshifted-character character))))

(defun pose-values (camera new)
  (let ((p (if new (r:camera-position camera) (old:camera-position camera))))
    (append (if new (list (- (v:vec3-x p) 1024) (v:vec3-z p) (- (v:vec3-y p) 1024))
                (v:vec3-list p))
            (if new (list (- (/ pi 2) (r:camera-yaw camera)) (r:camera-pitch camera)
                          (r:camera-field-of-view camera))
                (list (old:camera-yaw camera) (old:camera-pitch camera)
                      (old:camera-field-of-view camera))))))

(defun proof (new)
  (let* ((world (world))
         (source (when new (game:luvcraft-world-luft-source world)))
         (scene (when new (game:make-luvcraft-streaming-scene
                          source :residency-radius 0 :mesh-preparer r:+render-population-preparer+)))
         (camera (if new (r:make-fly-camera :position (v:make-vec3 1032.5 1028.5 5.62)
                                           :yaw (/ pi 2) :pitch 0.0)
                     (make-instance 'old:fly-camera :position (v:make-vec3 8.5 5.62 4.5)
                                                    :yaw 0.0 :pitch 0.0)))
         (owner (if new
                    (r:start-game-host
                     :solid scene :camera camera :player-p nil :width 800 :height 600
                     :frames-per-second 1 :fixed-exposure 1.0
                     :figure-factory 'host::make-ball-figure-drawing
                     :frame-class 'host:luvcraft-viewer
                     :frame-initargs (list :luvcraft-world world
                                          :hotbar (list (game:block-placement old::*terminal-block*))))
                    (old:start-luvcraft :world world :camera camera :width 800 :height 600
                                        :visible-p nil :high-pixel-density-p nil
                                        :frames-per-second nil :residency-radius 0)))
         (canvas (if new (r:viewer-canvas owner) (old:luvcraft-session-canvas owner))))
    (labels ((on (fn) (let ((luv::*canvas-dispatch-timeout* 120.0))
                       (luv::call-on-sdl-canvas-thread canvas fn)))
             (advance (count)
               (on (lambda ()
                     (dotimes (i count)
                       (if new (r::advance-viewer-camera owner (/ 1d0 60) nil)
                           (old::advance-luvcraft-focus-camera owner (/ 1d0 60)))))))
             (capture (state)
               (let* ((path (format nil ".amp/in/artifacts/terminal-~A-~A.png"
                                    (if new "new" "old") state))
                      (pose (on (lambda () (pose-values camera new)))))
                 (if new
                     (let ((old (gethash state *old-poses*)))
                       (assert (every (lambda (a b) (< (abs (- a b)) 2e-3)) old pose)))
                     (setf (gethash state *old-poses*) pose))
                 (format t "~&~A ~A: ~S~%" (if new "NEW" "OLD") state pose)
                 (if new (progn
                           (on (lambda () (r::advance-viewer-world-text owner)))
                           (r:capture-viewer-frame path owner))
                     (old:capture-luvcraft-screenshot owner path)))))
      (unwind-protect
           (progn
             (if new
                 (loop repeat 600 do
                   (on (lambda () (r::advance-viewer-streaming owner)))
                   (when (and (plusp (hash-table-count (terrain:streaming-scene-loaded scene)))
                              (null (terrain::streaming-scene-replacement scene))
                              (zerop (hash-table-count (terrain::streaming-scene-load-outstanding scene))))
                     (return))
                   (sleep 0.1)
                   finally (error "Terrain never settled"))
                 (old::wait-for-luvcraft-products owner))
             (on (lambda ()
                   (setf (luv:canvas-clock canvas) (luv:make-demand-clock))
                   (if new
                       (setf (r:camera-position camera) (v:make-vec3 1032.5 1028.5 5.62)
                             (r:camera-field-of-view camera) old::+luvcraft-camera-vertical-field-of-view+
                             (r:viewer-pointer-captured-p owner) t
                             (host::viewer-animals-initialized-p owner) t
                             (host::viewer-agents-initialized-p owner) t)
                       (setf (old:camera-position camera) (v:make-vec3 8.5 5.62 4.5)
                             (old:camera-field-of-view camera) old::+luvcraft-camera-vertical-field-of-view+
                             (old::luvcraft-session-pointer-captured-p owner) t))))
             (capture "before")
             (on (lambda () (key owner canvas :tab)))
             (assert (if new (r:viewer-modal-focus owner) (old:luvcraft-session-modal-focus owner)))
             (on (lambda ()
                   (loop for c across "printf 'CAMERA FOCUS\\n'" do
                     (key owner canvas :x :character c))
                   (key owner canvas :return)))
             (sleep 0.5)
             (on (lambda ()
                   (let* ((focus (if new (r::viewer-terminals-focus (r::viewer-terminals owner))
                                     (old:luvcraft-session-modal-focus owner)))
                          (device (if new (r::world-terminal-device focus)
                                      (old::terminal-display-device focus)))
                          (text (luv.terminal:call-with-pty-device-terminal device #'luv.ghostty:terminal-text)))
                     (assert (search (format nil "~%CAMERA FOCUS") text)))))
             (unless new
               ;; Capture was only an offscreen input-routing flag above.
               ;; Do not pretend SDL can restore native relative mode: old
               ;; Luvcraft snaps the return when recapturing a real pointer.
               ;; Exercise its original uncaptured cinematic return instead.
               (on (lambda () (setf (old::luvcraft-session-pointer-capture-suspended-p owner) nil))))
             (advance 6)
             (capture "entering")
             (advance 114)
             (capture "focused")
             ;; The old return follows the player's eyes, so put that player
             ;; at the initial view rather than the generator's default spawn.
             (unless new
               (on (lambda ()
                     (let ((p (old:luvcraft-session-player owner)))
                       (setf (old::player-x p) 8.5 (old::player-y p) 4.0 (old::player-z p) 4.5)))))
             (on (lambda () (key owner canvas :tab :modifiers '(:shift))))
             (assert (null (if new (r:viewer-modal-focus owner) (old:luvcraft-session-modal-focus owner))))
             (advance 6)
             (capture "leaving")
             (advance 114)
             (capture "returned")
             (assert (null (if new (r::viewer-focus-camera-origin owner)
                              (old::luvcraft-session-focus-camera-origin owner))))
             (assert (null (luv:canvas-failures canvas))))
        (if new (r:stop-viewer owner) (old:stop-luvcraft owner))))))

(ensure-directories-exist #p".amp/in/artifacts/terminal-old-before.png")
(proof nil)
(proof t)
(format t "~&Terminal Tab/Shift-Tab, matched poses, entry/return easing: PASS.~%")
