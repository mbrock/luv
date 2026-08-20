(in-package #:luvcraft.agent.tests)

(deftest dense-neighborhood-census-distinguishes-air-and-unavailable
  (let ((world (world:make-block-world :chunk-width 2 :chunk-height 2
                                       :chunk-depth 2)))
    (world:ensure-world-chunk world 0 0 0)
    (setf (world:world-block-at world 0 0 0)
          (luvcraft:block-kind-named :stone)
          (world:world-block-at world 1 0 0)
          (luvcraft:block-kind-named :grass))
    (let ((census (agent::census-block-neighborhood
                   world '(-1 0 0) '(1 1 1))))
      (ok (= 12 (agent::census-total census)))
      (ok (= 8 (agent::census-resident census)))
      (ok (= 4 (agent::census-unavailable census)))
      (ok (= 6 (agent::census-air census)))
      (ok (equal '((:grass . 1) (:stone . 1))
                 (agent::census-kind-counts census))))))

(deftest third-person-pose-follows-retained-body-facing
  (let* ((gnome (make-instance 'agent:gnome
                               :session nil :x 2 :y 3 :z 4
                               :position (luvcraft::make-vec3 2.5d0 3d0 4.5d0)
                               :facing-yaw (/ pi 2)))
         (pose (agent::surroundings-camera-pose-for gnome))
         (position (luvcraft::camera-pose-position pose)))
    (ok (< (luvcraft::vec3-x position) 2.5d0))
    (ok (< (abs (- (luvcraft::vec3-z position) 4.5d0)) 1d-9))
    (ok (< (abs (- (luvcraft::camera-pose-yaw pose) (/ pi 2))) 1d-9))
    (ok (minusp (luvcraft::camera-pose-pitch pose)))))

(deftest view-surroundings-is-an-agent-tool-with-its-own-canvas-request
  (ok (member 'agent:com-view-surroundings agent::*gnome-tools*))
  (ng (agent:command-tool-runs-on-canvas-p
       'agent:com-view-surroundings)))
