;;;; Headless play-through: ./sly play wizard, then
;;;; ./sly eval '(load "scripts/wizard-journey.lisp")'
;;;; Uses real screen rays, routes, simulation, streaming, edits and Vulkan
;;;; captures. Starts with a fresh wizard; never teleports the character.
;;;; Leaves its clock frozen for inspection; stop-playing/play starts anew.
(in-package #:luft.wizard)

(defun check-wizard-journey (&optional (wizard *wizard*))
  (assert (zerop (wizard-embers wizard)))
  (let ((canvas (wizard-canvas wizard)))
    (labels ((on-canvas (function)
               (request-canvas-frame canvas
                                     (lambda (timestamp)
                                       (declare (ignore timestamp))
                                       (funcall function))))
             (settle ()
               (loop repeat 1800
                     do (on-canvas (lambda () (advance-wizard-streaming wizard)))
                     when (and (wizard-resident-p wizard)
                               (null (terrain:streaming-scene-replacement (wizard-scene wizard))))
                       return t
                     do (sleep 0.1)
                     finally (error "Wizard terrain did not settle")))
             (capture (name)
               (capture-wizard-frame
                (format nil ".amp/in/artifacts/ember-~A.png" name) wizard)))
      (on-canvas (lambda () (setf (canvas-clock canvas) (make-demand-clock))))
      (settle)
      (on-canvas (lambda ()
                   (setf (wizard-pointer wizard) '(700 450))
                   (handle-canvas-event wizard canvas
                                        (make-instance 'canvas-key-press-event :key-name :space))
                   (assert (zerop (wizard-embers wizard)))))
      (capture "start")
      (loop for beacon in *wizard-beacons* for index from 1 do
        (let* ((x (- (second beacon) 2)) (y (third beacon))
               (z (wizard-ground-height x y)) (route nil))
          (on-canvas
           (lambda ()
             ;; Zoom out to click a distant destination, just as with the wheel.
             (setf (wizard-isometric-height wizard) 64.0)
             (destructuring-bind (width height) (wizard-logical-extent wizard)
               (destructuring-bind (px py)
                   (wizard-project wizard (+ x 0.5) (+ y 0.5) z width height)
                 (assert (and (< 0 px width) (< 121 py (- height 58))))
                 (setf route (wizard-click wizard px py))
                 (assert route)
                 (assert (eq :running (terrain:walking-route-status route)))))))
          (when (= index 1) (capture "route"))
          (on-canvas
           (lambda ()
             (loop repeat 18000 while (eq :running (terrain:walking-route-status route))
                   do (advance-wizard wizard
                                      (+ (or (wizard-last-timestamp wizard) 0) (/ 1.0 60))))
             (assert (eq :arrived (terrain:walking-route-status route)))
             (setf (wizard-isometric-height wizard) 24.0)
             (follow-wizard wizard)))
          (settle)
          (on-canvas (lambda ()
                       (handle-canvas-event wizard canvas
                                            (make-instance 'canvas-key-press-event :key-name :space))
                       (assert (= index (wizard-embers wizard)))))
          (settle)
          (on-canvas (lambda ()
                       (advance-wizard wizard (+ (wizard-last-timestamp wizard) 0.35))))
          (capture (string-downcase (first beacon)))
          (format t "~&Reached and kindled ~A: ~D/3, ~D A* visits.~%"
                  (first beacon) (wizard-embers wizard) (terrain:walking-route-visits route))))
      (assert (null (wizard-next-beacon wizard)))
      (on-canvas (lambda ()
                   (handle-canvas-event wizard canvas
                                        (make-instance 'canvas-key-press-event :key-name :r))))
      (assert (= 3 (wizard-embers wizard)))
      (format t "~&Journey passed; return to spawn preserves all three lights.~%"))))

(check-wizard-journey)
