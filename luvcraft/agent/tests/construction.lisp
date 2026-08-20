(in-package #:luvcraft.agent.tests)

(deftest additive-box-change-sets-are-frozen-and-stale-checked
  (let* ((world (world:make-block-world :chunk-width 4 :chunk-height 4
                                        :chunk-depth 4))
         (stone (luvcraft:block-kind-named :stone)))
    (world:ensure-world-chunk world 0 0 0)
    (let ((change-set
            (agent::make-additive-box-change-set world stone 0 0 0 1 0 1)))
      (ok (= 4 (agent::block-change-set-count change-set)))
      (ok (equalp #(0 0 0 1 0 0 0 0 1 1 0 1)
                  (agent::block-change-set-coordinates change-set)))
      (ok (agent::validate-block-change-set change-set world))
      (setf (world:world-block-at world 1 0 1) stone)
      (signals (agent::validate-block-change-set change-set world)))))

(deftest applying-a-change-set-publishes-one-world-revision
  (let* ((world (world:make-block-world :chunk-width 4 :chunk-height 4
                                        :chunk-depth 4))
         (stone (luvcraft:block-kind-named :stone)))
    (world:ensure-world-chunk world 0 0 0)
    (let* ((change-set
             (agent::make-additive-box-change-set world stone 0 0 0 1 0 1))
           (revision (world:block-world-revision world)))
      (agent::apply-block-change-set change-set world)
      (ok (= (1+ revision) (world:block-world-revision world)))
      (dotimes (index (agent::block-change-set-count change-set))
        (multiple-value-bind (x y z)
            (agent::block-change-set-coordinate change-set index)
          (ok (eq stone (world:world-block-at world x y z))))))))

(deftest construction-preview-is-real-block-geometry-with-a-glow-lane
  (let* ((world (world:make-block-world :chunk-width 4 :chunk-height 4
                                        :chunk-depth 4))
         (stone (luvcraft:block-kind-named :stone)))
    (world:ensure-world-chunk world 0 0 0)
    (let* ((change-set
             (agent::make-additive-box-change-set world stone 1 1 1 1 1 1))
           (mesh (agent::make-construction-preview-mesh change-set world))
           (vertices (luvcraft:block-mesh-vertices mesh)))
      (ok (= 6 (luvcraft:block-mesh-face-count mesh)))
      (ok (= 36 (luvcraft:block-mesh-vertex-count mesh)))
      (ok (loop for offset from 11 below (length vertices)
                by luvcraft::+block-mesh-floats-per-vertex+
                always (>= (aref vertices offset)
                           agent::*construction-preview-emission*))))))

(deftest a-finished-approval-does-not-wait
  (let ((approval (make-instance 'agent:tool-approval
                                 :agent nil :presence nil :session nil)))
    (agent:deny-tool-approval approval "nope")
    (ok (eq (agent::settle-command-result 'anything approval) approval))
    (ok (eq :denied (agent:tool-approval-state approval)))
    (ok (string= "nope" (agent:tool-approval-note approval)))))

(deftest propose-box-is-in-the-embodied-agent-toolbox
  (ok (member 'agent:com-propose-block-box agent::*gnome-tools*)))
