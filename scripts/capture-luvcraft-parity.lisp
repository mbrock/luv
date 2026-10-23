;;;; ./sly eval '(load "scripts/capture-luvcraft-parity.lisp")'
;;;; Then (luvcraft-parity:capture-parity). Uses only build/ saves and
;;;; .amp/in/artifacts/ captures. Load libghostty-vt first for the PTY check.
(require :asdf)
(asdf:load-system "luvcraft/luft/game")

(defpackage #:luvcraft-parity
  (:use #:cl)
  (:local-nicknames (#:old #:luvcraft) (#:host #:luvcraft.luft.game)
                    (#:game #:luvcraft.luft) (#:render #:luft.render)
                    (#:terrain #:luft.terrain) (#:v #:luv.arithmetic.lisp.vec3))
  (:export #:capture-parity))
(in-package #:luvcraft-parity)

(defun proof-world ()
  (let* ((world (old:make-empty-little-block-world :seed 7 :relief :meadow :chunk-height 64))
         (edits (old:little-world-source-edits (old:block-world-source world))))
    (loop for x from -10 to 10 do
      (loop for z from -12 to 8 do
        (old::record-block-edit edits old::*planks-block* x 50 z)
        (loop for y from 51 below 64 do (old::record-block-edit edits nil x y z))))
    (old::record-block-edit edits old::*fountain-block* -2 51 0)
    (old::record-block-edit edits old::*lava-spring-block* 2 51 0)
    (loop for x from -3 to 3 do
      (loop for y from 51 to 54 do (old::record-block-edit edits old::*terminal-block* x y 4)))
    world))

(defun on-canvas (viewer function)
  (let ((luv::*canvas-dispatch-timeout* 60.0))
    (luv::call-on-sdl-canvas-thread (render:viewer-canvas viewer) function)))

(defun key (viewer name &key character modifiers)
  (on-canvas viewer
             (lambda ()
               (luv:handle-canvas-event
                viewer (render:viewer-canvas viewer)
                (make-instance 'luv:canvas-key-press-event :timestamp 0 :key-name name
                               :character character :unshifted-character character :modifiers modifiers)))))

(defun capture-old-parity ()
  (when render:*viewer* (render:stop-viewer render:*viewer*))
  (setf luv::*canvas-watchdog-fatal-seconds* nil)
  (ensure-directories-exist #p".amp/in/artifacts/parity-old-springs.png")
  (let* ((world (proof-world))
         (camera (make-instance 'old:fly-camera :position (v:make-vec3 0.5 52.62 -8.5)
                                :yaw 0.0 :pitch 0.08))
         (session (old:start-luvcraft :world world :camera camera :width 1100 :height 760
                                     :visible-p nil :high-pixel-density-p nil
                                     :frames-per-second nil :residency-radius 2)))
    (unwind-protect
         (progn
           ;; Capture waits for the old streaming meshes before the fixed steps.
           (old:capture-luvcraft-screenshot session #p"build/parity-old-warmup.png")
           (luv::call-on-sdl-canvas-thread
            (old:luvcraft-session-canvas session)
            (lambda ()
              (dotimes (i 90) (old::advance-luvcraft-physics session old::*physics-step-seconds*))))
           (old:capture-luvcraft-screenshot session #p".amp/in/artifacts/parity-old-springs.png")
           (format t "~&Old springs: ~D bodies.~%"
                   (old::physics-world-body-count (old::luvcraft-session-physics session)))
           (when (luv.ghostty:libghostty-vt-loaded-p)
             (let ((display
                     (luv::call-on-sdl-canvas-thread
                      (old:luvcraft-session-canvas session)
                      (lambda ()
                        (old::attach-terminal-display-shell
                         (old::open-terminal-display session 0 52 4 :back :fixture ""))))))
               (luv.terminal:send-pty-device-text
                (old::terminal-display-device display) (format nil "printf 'LUVCRAFT ON LUFT\\n'~%"))
               (sleep 1)
               (luv::call-on-sdl-canvas-thread
                (old:luvcraft-session-canvas session)
                (lambda () (setf (old:camera-position camera) (v:make-vec3 0.5 52.62 0.5))))
               (old:capture-luvcraft-screenshot session #p".amp/in/artifacts/parity-old-terminal.png"))))
      (old:stop-luvcraft session))))

(defun capture-new-parity ()
  (let* ((world (proof-world))
         (source (game:luvcraft-world-luft-source world))
         (scene (game:make-luvcraft-streaming-scene
                 source :mesh-preparer render:+render-population-preparer+))
         (viewer (render:start-game-host
                  :solid scene :title "Luvcraft parity" :width 1100 :height 760
                  :world-pathname #p"build/parity-springs.sexp"
                  :fixed-exposure 1.0 :frames-per-second 10
                  :figure-factory 'host::make-ball-figure-drawing
                  :saved-world (list :player '(1024.5 1015.5 51.0)
                                     :yaw (/ pi 2) :pitch 0.08)
                  :frame-class 'host:luvcraft-viewer
                  :frame-initargs (list :luvcraft-world world
                                       :hotbar (list (game:block-placement old::*fountain-block*)
                                                     (game:block-placement old::*lava-spring-block*)
                                                     (game:block-placement old::*terminal-block*))))))
    (unwind-protect
         (progn
           (loop repeat 1200
                 when (on-canvas viewer
                                 (lambda ()
                                   (and (gethash (luft:chunk-key-at 1024 1024)
                                                 (terrain:streaming-scene-loaded scene))
                                        (null (terrain:streaming-scene-replacement scene)))))
                   return t
                 do (sleep 0.1)
                 finally (error "Parity terrain never published"))
           (on-canvas viewer
                      (lambda ()
                        (setf (luv:canvas-clock (render:viewer-canvas viewer)) (luv:make-demand-clock)
                              (render:camera-field-of-view (render:viewer-camera viewer))
                              old::+luvcraft-camera-vertical-field-of-view+
                              (host::viewer-physics viewer) nil
                              (host::viewer-physics-clock viewer) 0d0
                              (host::viewer-springs viewer) nil
                              (host::viewer-springs-initialized-p viewer) nil)
                        (render::restore-luft-world-player
                         viewer (list :player '(1024.5 1015.5 51.0) :yaw (/ pi 2) :pitch 0.08))
                        (render::place-viewer-at-player-eyes viewer)
                        ;; SDL offscreen cannot capture a relative pointer;
                        ;; supply only that ownership bit for world key/ray tests.
                        (setf (render:viewer-pointer-captured-p viewer) t)
                        (dotimes (i 90) (render:advance-viewer-game viewer old::*physics-step-seconds*))
                        (assert (= 2 (length (host::viewer-springs viewer))))))
           (render:capture-viewer-frame #p".amp/in/artifacts/parity-new-springs.png" viewer)
           (format t "~&Luft springs: ~D bodies; two live sources.~%"
                   (old::physics-world-body-count (host::viewer-physics viewer)))
           ;; A real terminal is looked at through terrain, opened by Tab,
           ;; and fed a command through the same canvas keys as the player.
           (when (luv.ghostty:libghostty-vt-loaded-p)
             (key viewer :tab)
             (assert (render::viewer-terminals-focus (render::viewer-terminals viewer)))
             (loop for c across "printf 'LUVCRAFT ON LUFT\\n'" do (key viewer :x :character c))
             (key viewer :return)
             (sleep 1)
             (let ((text
                     (on-canvas viewer
                                (lambda ()
                                  (let* ((terminal (render::viewer-terminals-focus (render::viewer-terminals viewer)))
                                         (text (luv.terminal:call-with-pty-device-terminal
                                                (render::world-terminal-device terminal) #'luv.ghostty:terminal-text)))
                                    (assert (search (format nil "~%LUVCRAFT ON LUFT") text))
                                    (setf (render:camera-position (render:viewer-camera viewer))
                                          (v:make-vec3 1024.5 1024.5 52.62)
                                          (render::renderer-history-valid-p (render:viewer-renderer viewer)) nil)
                                    (render::advance-viewer-world-text viewer)
                                    text)))))
               (format t "~&Terminal shell: ~A~%" text))
             (format t "~&Text knob renderer revisions: ~S~%"
                     (on-canvas
                      viewer
                      (lambda ()
                        (let* ((knob (luv.knobs:find-knob 'old::slug-optical-weight))
                               (saved (luv.knobs:knob-value knob viewer))
                               (artifact (render::viewer-live-artifact viewer))
                               (before (luv:live-artifact-installed-revision artifact)))
                          (unwind-protect
                               (progn
                                 (luv.knobs:set-knob-value knob 0.75 viewer)
                                 (luv:refresh-application-live-artifacts viewer)
                                 (let ((after (luv:live-artifact-installed-revision artifact)))
                                   (assert (> after before))
                                   (list before after)))
                            (luv.knobs:set-knob-value knob saved viewer)
                            (luv:refresh-application-live-artifacts viewer))))))
             ;; The demand clock is parked: perform the normal frame's surface
             ;; publication after the renderer-cohort replacement above.
             (on-canvas viewer (lambda () (render::advance-viewer-world-text viewer)))
             (render:capture-viewer-frame #p".amp/in/artifacts/parity-new-terminal.png" viewer)
             (key viewer :tab :modifiers '(:shift))
             (assert (null (render:viewer-modal-focus viewer))))
           (key viewer :f1)
           (render:capture-viewer-frame #p".amp/in/artifacts/parity-new-help.png" viewer)
           (key viewer :h :character #\h)
           (key viewer :h :character #\h)
           (render:capture-viewer-frame #p".amp/in/artifacts/parity-new-help-h.png" viewer)
           (format t "~&F1 and H exercised; screenshots are native 1100 x 760.~%")
           (on-canvas
            viewer
            (lambda ()
              (setf (host::viewer-creative-p viewer) t)
              (assert (eq :edited
                          (nth-value 1 (render:apply-viewer-world-edit
                                        viewer (game:luvcraft-cell-site source 6 51 0)
                                        (game:block-placement old::*fountain-block*)))))
              (render:advance-viewer-game viewer 0d0)
              (assert (= 3 (length (host::viewer-springs viewer))))))
           (format t "~&Live source placement: two -> three springs through the host edit boundary.~%"))
      (render:stop-viewer viewer))))

(defun capture-parity ()
  (capture-old-parity)
  (capture-new-parity))
