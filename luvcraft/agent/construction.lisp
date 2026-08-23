(in-package #:luvcraft.agent)

;;; Construction approval: an actual possible world, not a drafting diagram.
;;; This first primitive is deliberately additive.  Its preview can therefore
;;; draw the exact candidate blocks over live terrain without pretending it
;;; can hide removals that the renderer has already submitted.

(defparameter *maximum-construction-proposal-cells* 512)
(defparameter *construction-preview-emission* 2.8)

(defclass block-change-set ()
  ((coordinates :initarg :coordinates :reader block-change-set-coordinates)
   (expected :initarg :expected :reader block-change-set-expected)
   (proposed :initarg :proposed :reader block-change-set-proposed)
   (lookup :initarg :lookup :reader block-change-set-lookup)
   (bounds :initarg :bounds :reader block-change-set-bounds)
   (world-revision :initarg :world-revision
                   :reader block-change-set-world-revision))
  (:documentation
   "A frozen sparse voxel edit: dense coordinate/value columns plus lookup."))

(defun block-change-set-count (change-set)
  (length (block-change-set-proposed change-set)))

(defun block-change-set-coordinate (change-set index)
  (let ((coordinates (block-change-set-coordinates change-set))
        (offset (* 3 index)))
    (values (aref coordinates offset)
            (aref coordinates (+ offset 1))
            (aref coordinates (+ offset 2)))))

(defun make-additive-box-change-set (world kind x1 y1 z1 x2 y2 z2)
  "Freeze the air cells in inclusive box corners into one additive change set."
  (unless (luvcraft::block-kind-placeable-p kind)
    (error "~A is not a placeable block kind." (luvcraft:block-kind-name kind)))
  (let* ((minimum-x (min x1 x2)) (maximum-x (max x1 x2))
         (minimum-y (min y1 y2)) (maximum-y (max y1 y2))
         (minimum-z (min z1 z2)) (maximum-z (max z1 z2))
         (volume (* (1+ (- maximum-x minimum-x))
                    (1+ (- maximum-y minimum-y))
                    (1+ (- maximum-z minimum-z)))))
    (when (> volume *maximum-construction-proposal-cells*)
      (error "The ~D-cell box exceeds the basic proposal limit of ~D cells."
             volume *maximum-construction-proposal-cells*))
    (let ((coordinate-list nil)
          (expected-list nil)
          (proposed-list nil))
      (loop for z from minimum-z to maximum-z do
        (loop for y from minimum-y to maximum-y do
          (loop for x from minimum-x to maximum-x do
            (multiple-value-bind (old residency)
                (luvcraft:world-block-at world x y z)
              (unless (eq residency :resident)
                (error "Cell ~D ~D ~D is not loaded (~(~A~))."
                       x y z residency))
              (cond ((null old)
                     (push x coordinate-list)
                     (push y coordinate-list)
                     (push z coordinate-list)
                     (push nil expected-list)
                     (push kind proposed-list))
                    ((not (eq old kind))
                     (error "The additive preview would replace ~A at ~D ~D ~D."
                            (luvcraft:block-kind-name old) x y z)))))))
      (when (null proposed-list)
        (error "Every cell in that box is already ~A."
               (luvcraft:block-kind-name kind)))
      (let* ((coordinates
               (make-array (length coordinate-list) :element-type 'fixnum
                            :initial-contents (nreverse coordinate-list)))
             (expected (coerce (nreverse expected-list) 'simple-vector))
             (proposed (coerce (nreverse proposed-list) 'simple-vector))
             (lookup (make-hash-table :test #'equal)))
        (dotimes (index (length proposed))
          (multiple-value-bind (x y z)
              (let ((offset (* 3 index)))
                (values (aref coordinates offset)
                        (aref coordinates (+ offset 1))
                        (aref coordinates (+ offset 2))))
            (setf (gethash (list x y z) lookup) index)))
        (make-instance
         'block-change-set
         :coordinates coordinates :expected expected :proposed proposed
         :lookup lookup
         :bounds (list minimum-x minimum-y minimum-z
                       maximum-x maximum-y maximum-z)
         :world-revision (world:block-world-revision world))))))

(defun validate-block-change-set (change-set world)
  "Require every site to remain resident and equal to its frozen expectation."
  (dotimes (index (block-change-set-count change-set) t)
    (multiple-value-bind (x y z) (block-change-set-coordinate change-set index)
      (multiple-value-bind (current residency)
          (luvcraft:world-block-at world x y z)
        (unless (eq residency :resident)
          (error "Proposal cell ~D ~D ~D is no longer resident." x y z))
        (unless (eq current (aref (block-change-set-expected change-set) index))
          (error "Proposal is stale at ~D ~D ~D." x y z))))))

(defun block-change-set-after-value-at (change-set world x y z)
  (multiple-value-bind (index proposed-p)
      (gethash (list x y z) (block-change-set-lookup change-set))
    (if proposed-p
        (values (aref (block-change-set-proposed change-set) index) :resident)
        (luvcraft:world-block-at world x y z))))

(defun make-construction-preview-mesh (change-set world)
  "Mesh only CHANGE-SET's additions against its complete counterfactual occupancy."
  (let ((vertices
          (make-array
           (* (block-change-set-count change-set) 6
              luvcraft::+block-mesh-floats-per-face+)
           :element-type 'single-float :adjustable t :fill-pointer 0))
        (face-count 0))
    (dotimes (index (block-change-set-count change-set))
      (multiple-value-bind (x y z) (block-change-set-coordinate change-set index)
        (let ((block (aref (block-change-set-proposed change-set) index)))
          (multiple-value-bind (sky-level block-level light-status)
              (luvcraft:world-light-at world x y z)
            (declare (ignore light-status))
            (loop for face in luvcraft::*block-faces* do
              (let* ((direction (luvcraft::block-face-neighbor face))
                     (nx (world:voxel-direction-dx direction))
                     (ny (world:voxel-direction-dy direction))
                     (nz (world:voxel-direction-dz direction)))
                (multiple-value-bind (neighbor residency)
                    (block-change-set-after-value-at
                     change-set world (+ x nx) (+ y ny) (+ z nz))
                  (when (and (eq residency :resident)
                             (not (luvcraft:block-solid-p neighbor)))
                    (incf face-count)
                    (let ((corners (luvcraft::block-face-corners face))
                          (tile (luvcraft::block-atlas-tile-offset
                                 (luvcraft:block-face-tile block face))))
                      (dolist (corner-index '(0 1 2 0 2 3))
                        (let ((corner (nth corner-index corners)))
                          (multiple-value-bind (u v)
                              (luvcraft::block-face-local-uv face corner)
                            (luvcraft::push-block-vertex-components
                             vertices
                             (+ x (first corner)) (+ y (second corner))
                             (+ z (third corner)) u v
                             (luvcraft::block-color-variation x y z)
                             nx ny nz
                             (/ (or sky-level 0) 15.0)
                             (/ (or block-level 0) 15.0)
                             (max (luvcraft:block-surface-emission block)
                                  *construction-preview-emission*)
                             tile 0.0 0.0 0.0 0.0)))))))))))))
    (let ((packed (make-array (length vertices) :element-type 'single-float
                              :initial-contents vertices)))
      (make-instance 'luvcraft:block-mesh
                     :vertices packed
                     :vertex-count (* face-count
                                      luvcraft::+block-mesh-vertices-per-face+)
                     :face-count face-count))))

(defclass construction-preview-overlay ()
  ((approval :initarg :approval :reader construction-preview-approval)
   (mesh :initarg :mesh :reader construction-preview-mesh)
   (vertex-buffer :initarg :vertex-buffer
                  :accessor construction-preview-vertex-buffer))
  (:documentation "GPU-owned rendering of one inert construction proposal."))

(defmethod luvcraft:luvcraft-overlay-stage ((overlay construction-preview-overlay))
  (declare (ignore overlay))
  :scene)

(defmethod luvcraft:encode-luvcraft-overlay
    ((overlay construction-preview-overlay) session pass surface-texture)
  (let ((frame (luvcraft::luvcraft-frame-state session surface-texture))
        (mesh (construction-preview-mesh overlay)))
    (luv:set-pipeline pass (luvcraft::luvcraft-session-pipeline session))
    (luv:set-bind-group pass 0 (luvcraft::luvcraft-frame-scene-bind-group frame))
    (luv:set-vertex-buffer pass 0 (construction-preview-vertex-buffer overlay))
    (luv:draw pass (luvcraft:block-mesh-vertex-count mesh)))
  overlay)

(defmethod luvcraft:release-luvcraft-overlay
    ((overlay construction-preview-overlay))
  (alexandria:when-let ((buffer (construction-preview-vertex-buffer overlay)))
    (luv:destroy buffer)
    (setf (construction-preview-vertex-buffer overlay) nil)))

(defun make-construction-preview-overlay (approval)
  (let* ((session (tool-approval-session approval))
         (mesh (make-construction-preview-mesh
                (construction-approval-change-set approval)
                (luvcraft:luvcraft-session-world session)))
         (vertices (luvcraft:block-mesh-vertices mesh))
         (buffer (luv:create
                  (luvcraft:luvcraft-session-device session)
                  (luv:make-buffer-descriptor
                   :label "construction possible world"
                   :size (max 4 (* 4 (length vertices)))
                   :usage '(:vertex :copy-dst)))))
    (luv:write-buffer buffer vertices)
    (make-instance 'construction-preview-overlay
                   :approval approval :mesh mesh :vertex-buffer buffer)))

(defclass construction-approval (tool-approval)
  ((change-set :initarg :change-set :reader construction-approval-change-set)
   (preview :initform nil :accessor construction-approval-preview))
  (:documentation "Approval for a frozen additive block change set."))

(defmethod validate-tool-approval ((approval construction-approval))
  (validate-block-change-set
   (construction-approval-change-set approval)
   (luvcraft:luvcraft-session-world (tool-approval-session approval))))

(defun apply-block-change-set (change-set world &key after-edit)
  "Publish CHANGE-SET as one world revision, optionally calling AFTER-EDIT."
  (world:with-world-change-transaction (world)
    (dotimes (index (block-change-set-count change-set))
      (multiple-value-bind (x y z) (block-change-set-coordinate change-set index)
        (let ((block (aref (block-change-set-proposed change-set) index)))
          (luvcraft:edit-block-at block world x y z)
          (when after-edit
            (funcall after-edit block x y z))))))
  change-set)

(defmethod commit-tool-approval ((approval construction-approval))
  (let* ((session (tool-approval-session approval))
         (world (luvcraft:luvcraft-session-world session))
         (change-set (construction-approval-change-set approval)))
    (apply-block-change-set
     change-set world
     :after-edit (lambda (block x y z)
                   (luvcraft:luvcraft-block-placed block session x y z)))
    (destructuring-bind (min-x min-y min-z max-x max-y max-z)
        (block-change-set-bounds change-set)
      (let ((wake-radius
              (+ 2.0
                 (sqrt (+ (expt (- max-x min-x) 2)
                          (expt (- max-z min-z) 2))))))
        (alexandria:when-let
            ((physics (luvcraft::luvcraft-session-physics session)))
          (luvcraft:wake-physics-bodies-near
           physics (/ (+ min-x max-x 1) 2.0)
           (/ (+ min-y max-y 1) 2.0)
           (/ (+ min-z max-z 1) 2.0)
           wake-radius))))
    (luvcraft:request-luvcraft-session-checkpoint session)
    change-set))

(defmethod detach-tool-approval ((approval construction-approval))
  (let ((presence (tool-approval-presence approval))
        (session (tool-approval-session approval)))
    (when (eq approval (embodied-agent-pending-approval presence))
      (setf (embodied-agent-pending-approval presence) nil))
    (alexandria:when-let ((preview (construction-approval-preview approval)))
      (luvcraft:remove-luvcraft-overlay session preview)
      (setf (construction-approval-preview approval) nil))
    (alexandria:when-let ((dialogue (gnome-dialogue presence)))
      ;; Session teardown may already have disowned this frame before it
      ;; reaches the semantic gnome overlay.  A normal decision repaints; a
      ;; dead mirror has nothing left to update.
      (unless (eq :disowned
                  (frame-state
                   (mcluv:widget-overlay-frame dialogue)))
        (repaint-gnome-dialogue presence))))
  approval)

(defmethod tool-approval-focus-camera-pose
    ((approval construction-approval) presence session)
  (declare (ignore presence session))
  (destructuring-bind (min-x min-y min-z max-x max-y max-z)
      (block-change-set-bounds
       (construction-approval-change-set approval))
    (let* ((center-x (/ (+ min-x max-x 1) 2.0d0))
           (center-y (/ (+ min-y max-y 1) 2.0d0))
           (center-z (/ (+ min-z max-z 1) 2.0d0))
           (radius (* 0.5d0
                      (sqrt (+ (expt (1+ (- max-x min-x)) 2)
                               (expt (1+ (- max-y min-y)) 2)
                               (expt (1+ (- max-z min-z)) 2)))))
           (distance (max 5.0d0 (+ 2.5d0 (* radius 1.45d0))))
           (seconds (/ (- (get-internal-real-time)
                          (tool-approval-created-at approval))
                       (float internal-time-units-per-second 1.0d0)))
           (angle (+ 0.65d0 (* seconds 0.16d0)))
           (position (luvcraft::make-vec3
                      (+ center-x (* distance (sin angle)))
                      (+ max-y 2.4d0 (* radius 0.35d0))
                      (+ center-z (* distance (cos angle)))))
           (dx (- center-x (luvcraft::vec3-x position)))
           (dy (- center-y (luvcraft::vec3-y position)))
           (dz (- center-z (luvcraft::vec3-z position)))
           (flat (max 0.001d0 (sqrt (+ (* dx dx) (* dz dz))))))
      (luvcraft::make-camera-pose
       position (atan dx dz) (atan dy flat)
       luvcraft::+luvcraft-camera-focused-vertical-field-of-view+))))

(define-presentation-method present
    (approval (type construction-approval) stream (view textual-view) &key)
  (let ((change-set (construction-approval-change-set approval)))
    (destructuring-bind (min-x min-y min-z max-x max-y max-z)
        (block-change-set-bounds change-set)
      (format stream
              "Construction proposal ~(~A~): ~D block~:P in [~D,~D,~D]..[~D,~D,~D]"
              (tool-approval-state approval)
              (block-change-set-count change-set)
              min-x min-y min-z max-x max-y max-z)
      (when (tool-approval-note approval)
        (format stream " — ~A" (tool-approval-note approval))))))

(defun install-construction-approval (approval)
  (let ((presence (tool-approval-presence approval))
        (session (tool-approval-session approval)))
    (when (embodied-agent-pending-approval presence)
      (error "~A already has a proposal waiting for the player."
             (embodied-agent-name presence)))
    (let ((preview (make-construction-preview-overlay approval)))
      (luvcraft:add-luvcraft-overlay session preview)
      ;; A rejected ADD consumes PREVIEW.  Publish approval state only after
      ;; the application owns the corresponding visible attachment.
      (setf (construction-approval-preview approval) preview
            (embodied-agent-pending-approval presence) approval)
      (ensure-gnome-dialogue presence)
      (repaint-gnome-dialogue presence)))
  approval)

(define-command (com-propose-block-box :command-table luvcraft-agent
                                       :name "Propose Block Box")
    ((kind 'luvcraft:block-kind :prompt "kind")
     (x1 'integer :prompt "first x") (y1 'integer :prompt "first y")
     (z1 'integer :prompt "first z") (x2 'integer :prompt "second x")
     (y2 'integer :prompt "second y") (z2 'integer :prompt "second z"))
  "Propose filling the inclusive axis-aligned box between two corners with KIND. The call waits while the player sees the possible world and approves, denies, or types revision guidance. This basic preview adds blocks only and never replaces occupied cells."
  (let* ((agent *current-agent*)
         (presence (and agent (world-agent-presence agent)))
         (session (and agent (world-agent-session agent))))
    (unless (typep presence 'embodied-agent)
      (error "A construction proposal needs an embodied agent beside the player."))
    (install-construction-approval
     (make-instance
      'construction-approval :agent agent :presence presence :session session
      :change-set
      (make-additive-box-change-set
       (luvcraft:luvcraft-session-world session) kind x1 y1 z1 x2 y2 z2)))))
