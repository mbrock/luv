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

(defun proposal-block (proposal)
  (etypecase proposal
    (companion-box-approval (third (first (box-approval-changes proposal))))
    (companion-edit-approval
     (let ((placement (or (approval-placement proposal) (approval-original proposal))))
       (when placement (luvcraft:block-kind-named (terrain:material-placement-name placement)))))))

(defun proposal-title (proposal)
  (let* ((block (proposal-block proposal))
         (label (and block (substitute #\Space #\- (string-downcase (luvcraft:block-kind-name block))))))
    (etypecase proposal
      (companion-box-approval
       (let ((count (length (box-approval-changes proposal)))
             (noun (subseq label (1+ (or (position #\Space label :from-end t) -1)))))
         (if (member noun '("bricks" "planks" "tiles") :test #'string=)
             (format nil "Add ~D ~A?" count (if (= count 1) (string-right-trim "s" label) label))
             (format nil "Add ~D ~A block~P?" count label count))))
      (companion-edit-approval
       (cond ((null (approval-placement proposal)) "Remove this block?")
             ((approval-original proposal) (format nil "Replace with ~A?" label))
             (t (format nil "Add ~A?" label)))))))

(defun proposal-preview-figure (proposal)
  "A single depth-writing box. Its face tiles come from the same atlas as the inventory."
  (let ((block (proposal-block proposal)))
    (unless block (return-from proposal-preview-figure nil))
    (multiple-value-bind (low high) (proposal-corners proposal)
      (let* ((half (mapcar (lambda (axis) (/ (1+ (- (funcall axis high) (funcall axis low))) 2.0))
                           (list #'luft:site-x #'luft:site-y #'luft:site-z)))
             (center (vec3:make-vec3 (+ (luft:site-x low) (first half))
                                     (+ (luft:site-y low) (second half))
                                     (+ (luft:site-z low) (third half))))
             ;; Keep the preview just inside air cells or just outside an occupied cell.
             (offset (if (and (typep proposal 'companion-edit-approval) (approval-original proposal))
                         0.006 -0.003)))
        (flet ((tile (name)
                 (luvcraft:block-atlas-tile-offset
                  (luvcraft:block-face-tile block (find name luvcraft::*block-faces*
                                                        :key #'luvcraft:block-face-name)))))
          (let ((figure (render:box-figure
                         center center 0.0 1.0 (mapcar (lambda (h) (+ h offset)) half)
                         ;; One pair per Luft axis, negative face in the low 8 bits.
                         (list (+ (tile :left) (* 256 (tile :right)))
                               (+ (tile :back) (* 256 (tile :front)))
                               (+ (tile :bottom) (* 256 (tile :top)))))))
            (setf (fourth (fourth figure)) 5.0)
            figure))))))

(defmethod render:viewer-figures :around ((viewer luvcraft-viewer))
  (let ((focus (render:viewer-modal-focus viewer)))
    (append (call-next-method)
            (loop for inhabitant in (world-agents (viewer-world viewer))
                  for proposal = (inhabitant-approval inhabitant)
                  for figure = (when (and proposal (or (not (typep focus 'companion-proposal)) (eq proposal focus)))
                                 (proposal-preview-figure proposal))
                  when figure collect figure))))

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

(defconstant +review-sheet-width+ 868)
(defconstant +review-sheet-height+ 132)
(defparameter *review-buttons*
  '((:y "Yes" "Y" :affirm 92) (:t "Talk" "T" :discuss 98)
    (:n "No" "N" :decline 88) (:escape "Later" "Esc" :inset 96)))

;; Embed the two CLIM image arrays in the compiled game, including standalone builds.
(defparameter *proposal-portraits*
  (list :fantus (clim:make-pattern
                 #.(clim:read-bitmap-file (asdf:system-relative-pathname
                                           :luvcraft "luvcraft/companions/portraits/fantus.png")
                                          :format :png) nil)
        :gnome (clim:make-pattern
                #.(clim:read-bitmap-file (asdf:system-relative-pathname
                                          :luvcraft "luvcraft/companions/portraits/gnome.png")
                                         :format :png) nil)))

(defclass proposal-pane (mcluv:transparent-gpu-application-pane) ())
(mcluv:define-ui-themed-pane proposal-pane)

(clim:define-application-frame proposal-frame ()
  ((proposal :initarg :proposal :reader frame-proposal)
   (compact :initform nil :accessor review-compact-p)
   (hovered :initform nil :accessor review-hovered))
  (:menu-bar nil)
  (:panes (card (clim:make-pane 'proposal-pane :background luvcraft.agent::+clear-ink+
                                :width +review-sheet-width+ :height +review-sheet-height+)))
  (:layouts (default card)))

(defun review-bar-size (compact)
  (if compact (values 420 128) (values +review-sheet-width+ 76)))

(defun map-review-buttons (compact function)
  (let ((x (if compact 12 460)) (top (if compact 72 16)))
    (dolist (button *review-buttons*)
      (destructuring-bind (key label shortcut role width) button
        (funcall function key label shortcut role x top (+ x width) (+ top 42))
        (incf x (+ width 7))))))

(defmethod clim:handle-repaint ((pane proposal-pane) region)
  (declare (ignore region))
  (let* ((frame (clim:pane-frame pane))
         (proposal (frame-proposal frame))
         (kind (inhabitant-kind (luvcraft.agent::tool-approval-presence proposal)))
         (compact (review-compact-p frame))
         (title (proposal-title proposal))
         (style (clim:make-text-style :sans-serif :bold (mcluv:ui-text-size :heading))))
    (multiple-value-bind (width height) (review-bar-size compact)
      (mcluv:draw-ui-panel pane 0 0 width height))
    (clim:draw-pattern* pane (getf *proposal-portraits* kind) 11 8)
    (clim:draw-text* pane (string-capitalize kind) 78 23 :align-y :center
                     :text-size (mcluv:ui-text-size :caption) :text-face :bold :ink (mcluv:ui-ink :muted))
    (clim:draw-text* pane (luvcraft.agent::fit-to-width pane title style (if compact 328 365))
                     78 47 :align-y :center :text-style style :ink (mcluv:ui-ink :text))
    (map-review-buttons compact
                        (lambda (key label shortcut role left top right bottom)
                          (mcluv:draw-ui-action pane left top right bottom label shortcut role
                                                :hovered (eq key (review-hovered frame)))))))

(defclass proposal-card (mcluv:luvcraft-hud-widget-overlay) ())

(defmethod luvcraft:luvcraft-overlay-stage ((card proposal-card)) :hud)

(defmethod luvcraft:refresh-luvcraft-overlay ((card proposal-card) viewer)
  (let* ((frame (mcluv:widget-overlay-frame card))
         (compact (< (luv:canvas-logical-size (render:viewer-canvas viewer)) 720)))
    (when (or (mcluv:ui-theme-changed-p frame) (not (eq compact (review-compact-p frame))))
      (setf (review-compact-p frame) compact)
      (mcluv:repaint-gpu-mirror (mcluv::widget-overlay-mirror card)))))

(defmethod luvcraft:encode-luvcraft-overlay ((card proposal-card) viewer pass surface)
  (declare (ignore pass))
  (multiple-value-bind (width height) (review-bar-size (review-compact-p (mcluv:widget-overlay-frame card)))
    (mcluv:prepare-direct-widget-overlay card viewer surface
                                         (mcluv:bottom-hud-screen-state card width height))))

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

(clim:define-command (com-answer-proposal :command-table luvcraft-world)
    ((proposal 'companion-proposal) (key 'symbol))
  (let ((viewer (render:viewer-command-viewer)))
    (when (eq proposal (render:viewer-modal-focus viewer))
      (review-action proposal viewer key))))

(defun answer-proposal (viewer proposal key)
  (when (assoc key *review-buttons*)
    (clim:execute-frame-command viewer (list 'com-answer-proposal proposal key))))

(defmethod render:handle-viewer-focus-event
    ((proposal companion-proposal) (viewer luvcraft-viewer) canvas (event luv:canvas-key-press-event))
  (declare (ignore canvas))
  (when (and (null (luv:canvas-key-event-modifiers event))
             (not (luv:canvas-key-event-repeat-p event)))
    (answer-proposal viewer proposal (luv:canvas-key-event-key-name event))))

(defun review-button-at (card event)
  (let ((uv (mcluv:luvcraft-widget-texture-coordinate
             card (luv:canvas-pointer-event-x event) (luv:canvas-pointer-event-y event))))
    (when uv
      (let ((x (* (first uv) +review-sheet-width+)) (y (* (second uv) +review-sheet-height+)))
        (map-review-buttons
         (review-compact-p (mcluv:widget-overlay-frame card))
         (lambda (key label shortcut role left top right bottom)
           (declare (ignore label shortcut role))
           (when (and (<= left x right) (<= top y bottom))
             (return-from review-button-at key)))))))
  nil)

(defmethod render:handle-viewer-focus-event
    ((proposal companion-proposal) (viewer luvcraft-viewer) canvas (event luv:canvas-pointer-motion-event))
  (declare (ignore canvas))
  (let* ((card (proposal-card proposal)) (frame (mcluv:widget-overlay-frame card))
         (key (review-button-at card event)))
    (unless (eq key (review-hovered frame))
      (setf (review-hovered frame) key)
      (mcluv:repaint-gpu-mirror (mcluv::widget-overlay-mirror card)))))

(defmethod render:handle-viewer-focus-event
    ((proposal companion-proposal) (viewer luvcraft-viewer) canvas
     (event luv:canvas-pointer-button-press-event))
  (declare (ignore canvas))
  (when (eq :left (luv:canvas-pointer-event-button event))
    (answer-proposal viewer proposal (review-button-at (proposal-card proposal) event))))
