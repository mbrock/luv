(in-package #:luvcraft.luft.game)

;;; The frozen proposal is itself the focus. Camera, preview, card and
;;; decision all read the same payload, even when another companion is waiting.

(defun proposal-corners (proposal)
  (etypecase proposal
    (companion-box-approval
     ;; The tool enumerates its normalized inclusive box in Z, Y, X order.
     (let ((changes (box-approval-changes proposal)))
       (values (caar changes) (caar (last changes)))))
    (companion-edit-approval
     (values (approval-cell proposal) (approval-cell proposal)))))

(defun proposal-title (proposal)
  (flet ((label (name) (substitute #\Space #\- (string-downcase name))))
    (etypecase proposal
      (companion-box-approval
       (let ((changes (box-approval-changes proposal)))
         (format nil "Add ~D block~:P of ~A" (length changes)
                 (label (luvcraft:block-kind-name (third (first changes)))))))
      (companion-edit-approval
       (if (approval-placement proposal)
           (format nil "~A ~A" (if (approval-original proposal) "Replace with" "Place")
                   (label (terrain:material-placement-name (approval-placement proposal))))
           "Remove one block")))))

(defun proposal-preview-figure (cell replacing)
  (let* ((center (vec3:make-vec3 (+ 0.5 (luft:site-x cell)) (+ 0.5 (luft:site-y cell))
                                 (+ 0.5 (luft:site-z cell))))
         (figure (render:box-figure center center 0.0 1.0
                                    (if replacing '(0.505 0.505 0.505) '(0.46 0.46 0.46))
                                    '(0.25 0.95 1.5))))
    (setf (fourth (fourth figure)) 3.0)
    figure))

(defmethod render:viewer-figures :around ((viewer luvcraft-viewer))
  (let ((focus (render:viewer-modal-focus viewer)))
    (append (call-next-method)
            (loop for inhabitant in (world-agents (viewer-world viewer))
                  for proposal = (inhabitant-approval inhabitant)
                  when (and proposal (or (not (typep focus 'companion-proposal)) (eq proposal focus)))
                    append (etypecase proposal
                             (companion-box-approval
                              (loop for (cell) in (box-approval-changes proposal)
                                    collect (proposal-preview-figure cell nil)))
                             (companion-edit-approval
                              (list (proposal-preview-figure (approval-cell proposal)
                                                             (approval-original proposal)))))))))

(defun proposal-orbit-pose (proposal viewer angle aspect)
  (multiple-value-bind (low high) (proposal-corners proposal)
    (let* ((width (1+ (- (luft:site-x high) (luft:site-x low))))
           (depth (1+ (- (luft:site-y high) (luft:site-y low))))
           (height (1+ (- (luft:site-z high) (luft:site-z low))))
           (cx (+ (luft:site-x low) (* 0.5 width)))
           (cy (+ (luft:site-y low) (* 0.5 depth)))
           (cz (+ (luft:site-z low) (* 0.5 height)))
           (center (vec3:make-vec3 cx cy cz))
           (radius (* 0.5 (sqrt (+ (* width width) (* depth depth) (* height height)))))
           (fov 0.88)
           (distance (max 4.2 (/ (* 1.5 radius) (sin (* 0.5 fov)) (min 1.0 aspect))))
           (best-arm 0)
           (best-pose nil))
      ;; Lift the orbit over walls before shortening the arm. In a confined
      ;; room use the clearest of the available sight lines.
      (dolist (elevation '(0.53 0.82 1.08 1.32) best-pose)
        (let* ((dx (* (cos angle) (cos elevation)))
               (dy (* (sin angle) (cos elevation)))
               (dz (sin elevation))
               (hit (nth-value 5 (terrain:raycast-face
                                  (render:viewer-source viewer) center (vec3:make-vec3 dx dy dz)
                                  :max-distance distance)))
               (arm (if hit (max 0.12 (- hit 0.24)) distance)))
          (when (> arm best-arm)
            (setf best-arm arm
                  best-pose (render:make-camera-pose
                             :position (vec3:make-vec3 (+ cx (* arm dx)) (+ cy (* arm dy))
                                                       (+ cz (* arm dz)))
                             :yaw (+ angle pi) :pitch (- (+ elevation 0.09)) :field-of-view fov)))
          (when (>= arm (* 0.9 distance)) (return best-pose)))))))

(defmethod render:viewer-focus-camera-pose ((proposal companion-proposal) (viewer luvcraft-viewer))
  (multiple-value-bind (width height) (luv:canvas-logical-size (render:viewer-canvas viewer))
    (proposal-orbit-pose proposal viewer
                         (+ 0.72 (* 0.16 (luvcraft.agent::seconds-since
                                          (luvcraft.agent::tool-approval-created-at proposal))))
                         (/ (max 1 width) (max 1 height)))))

(defconstant +review-width+ 510)
(defconstant +review-height+ 126)
(defparameter *review-buttons*
  '((:y "Build" 16 122) (:t "Ask for changes" 131 323) (:n "Decline" 332 494)))

(defclass proposal-pane (mcluv:transparent-gpu-application-pane) ())

(clim:define-application-frame proposal-frame ()
  ((proposal :initarg :proposal :reader frame-proposal))
  (:menu-bar nil)
  (:panes (card (clim:make-pane 'proposal-pane :background luvcraft.agent::+clear-ink+
                                :width +review-width+ :height +review-height+)))
  (:layouts (default card)))

(defmethod clim:handle-repaint ((pane proposal-pane) region)
  (declare (ignore region))
  (let* ((proposal (frame-proposal (clim:pane-frame pane)))
         (kind (inhabitant-kind (luvcraft.agent::tool-approval-presence proposal)))
         (cream (luvcraft.agent::display-color 0.98 0.96 0.88))
         (green (luvcraft.agent::display-color 0.71 0.89 0.75))
         (dark (luvcraft.agent::display-color 0.10 0.16 0.15))
         (secondary (luvcraft.agent::display-color 0.21 0.29 0.27)))
    (mcluv:draw-analytic-rounded-rectangle* pane 0 0 +review-width+ +review-height+
                                            :radius 18 :ink dark)
    (clim:draw-text* pane (format nil "~:(~A~)'s proposal" kind) 19 25
                     :text-family :sans-serif :text-face :bold :text-size 16 :ink green)
    (clim:draw-text* pane "Esc   Later" 490 25 :align-x :right :text-size 14 :ink cream)
    (let* ((title (proposal-title proposal))
           (style (clim:make-text-style :serif :roman 30))
           (width (max 1 (clim:text-size pane title :text-style style))))
      (clim:draw-text* pane title 18 60 :text-family :serif
                       :text-size (min 30 (floor (* 30 (- +review-width+ 36)) width)) :ink cream))
    (loop for (key label left right) in *review-buttons*
          for primary = (eq key :y)
          for text = (if (and primary (typep proposal 'companion-edit-approval))
                         "Apply" label)
          do (mcluv:draw-analytic-rounded-rectangle* pane left 76 right 112 :radius 9
                                                     :ink (if primary green secondary))
             (clim:draw-text* pane (format nil "~A   ~A" key text) (/ (+ left right) 2) 94
                              :align-x :center :align-y :center :text-family :sans-serif
                              :text-size 16 :text-face :bold :ink (if primary dark cream)))))

(defclass proposal-card (mcluv:luvcraft-hud-widget-overlay) ())

(defmethod luvcraft:luvcraft-overlay-stage ((card proposal-card)) :hud)

(defmethod luvcraft:encode-luvcraft-overlay ((card proposal-card) viewer pass surface)
  (declare (ignore pass))
  (mcluv:prepare-direct-widget-overlay card viewer surface (mcluv::hotbar-screen-state card)))

(defmethod render:viewer-focus-entered ((proposal companion-proposal) (viewer luvcraft-viewer))
  (setf (viewer-conversation-agent viewer) (luvcraft.agent::tool-approval-presence proposal)
        (proposal-reviewed-p proposal) t)
  (let* ((frame (luvcraft.agent::make-embedded-frame viewer 'proposal-frame :proposal proposal))
         (mirror (clim:sheet-direct-mirror (clim:frame-top-level-sheet frame)))
         (card (make-instance 'proposal-card :session viewer :frame frame :mirror mirror)))
    (setf (mcluv:mirror-compositor mirror) card
          (proposal-card proposal) card)
    (render:add-viewer-overlay viewer card)
    (mcluv:repaint-gpu-mirror mirror)))

(defmethod render:viewer-focus-left ((proposal companion-proposal) (viewer luvcraft-viewer))
  (when (proposal-card proposal)
    (render:remove-viewer-overlay viewer (proposal-card proposal))
    (setf (proposal-card proposal) nil)))

(defmethod render:encode-viewer-overlay :around
    ((overlay mcluv:luvcraft-hud-widget-overlay) (viewer luvcraft-viewer) pass surface)
  (when (or (not (typep (render:viewer-modal-focus viewer) 'companion-proposal))
            (typep overlay 'proposal-card))
    (call-next-method)))

(defmethod render:encode-viewer-overlay :around
    ((bubble luvcraft.agent::gnome-bubble-overlay) (viewer luvcraft-viewer) pass surface)
  (unless (typep (render:viewer-modal-focus viewer) 'companion-proposal)
    (call-next-method)))

(defun present-waiting-proposal (viewer)
  (let ((focus (render:viewer-modal-focus viewer)))
    (dolist (inhabitant (world-agents (viewer-world viewer)))
      (let ((proposal (inhabitant-approval inhabitant)))
        (when (and proposal (not (proposal-reviewed-p proposal))
                   (eq viewer (luvcraft.agent::tool-approval-session proposal))
                   (or (null focus)
                       (and (eq focus (inhabitant-presentation inhabitant))
                            (string= "" (luvcraft.agent::gnome-draft focus))))
                   (let ((bench (luv.workbench:application-workbench viewer)))
                     (and bench (null (luv.workbench:workbench-active-layer bench)))))
          (return (render:focus-viewer viewer proposal)))))))

(defun review-action (proposal viewer key)
  (case key
    (:y (luvcraft.agent:approve-tool-approval proposal))
    (:n (luvcraft.agent:deny-tool-approval proposal "Player declined."))
    (:escape (render:unfocus-viewer viewer))
    (:t (luv.workbench:open-workbench-prompt
         (luv.workbench:application-workbench viewer) :label "What should change?"
         :function (lambda (text)
                     (unless (string= "" (string-trim " " text))
                       (luvcraft.agent:steer-tool-approval proposal text)))))))

(defmethod render:handle-viewer-focus-event
    ((proposal companion-proposal) (viewer luvcraft-viewer) canvas (event luv:canvas-key-press-event))
  (declare (ignore canvas))
  (when (and (null (luv:canvas-key-event-modifiers event))
             (not (luv:canvas-key-event-repeat-p event)))
    (review-action proposal viewer (luv:canvas-key-event-key-name event))))

(defmethod render:handle-viewer-focus-event
    ((proposal companion-proposal) (viewer luvcraft-viewer) canvas
     (event luv:canvas-pointer-button-press-event))
  (declare (ignore canvas))
  (when (eq :left (luv:canvas-pointer-event-button event))
    (let ((uv (mcluv:luvcraft-widget-texture-coordinate
               (proposal-card proposal) (luv:canvas-pointer-event-x event)
               (luv:canvas-pointer-event-y event))))
      (when uv
        (let ((x (* (first uv) +review-width+)) (y (* (second uv) +review-height+)))
          (cond ((and (> x 408) (< y 38)) (review-action proposal viewer :escape))
                ((<= 76 y 112)
                 (loop for (key label left right) in *review-buttons*
                       when (<= left x right) return (review-action proposal viewer key)))))))))
