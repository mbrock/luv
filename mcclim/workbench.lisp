(in-package #:luv.workbench)

;;; The workbench is one fixed set of screen-space layers, not an extension
;;; registry. The shared status line is the fixed passive child; later known
;;; tools acquire authored children in their corresponding semantic layers.

(defclass workbench-application () ()
  (:documentation "Optional protocol base for an application hosted by Luv."))

(defgeneric workbench-application-name (application))
(defgeneric workbench-application-command-frame (application))
(defgeneric workbench-application-canvas (application))
(defgeneric workbench-application-context (application))
(defgeneric workbench-application-stop-controller (application))
(defgeneric suspend-workbench-application-input (application)
  (:documentation
   "Clear held input, release relative capture, and return an opaque token."))
(defgeneric resume-workbench-application-input (application token)
  (:documentation "End the suspension denoted by TOKEN."))

(defclass workbench-layer-pane
    (clime:never-repaint-background-mixin
     clim-internals::multiple-child-composite-pane)
  ((kind :initarg :kind :reader workbench-layer-pane-kind)))

(defmethod initialize-instance :after
    ((pane workbench-layer-pane) &key contents)
  (dolist (child contents)
    (clim:sheet-adopt-child pane child)))

(defmethod clim:compose-space
    ((pane workbench-layer-pane) &key (width 640) (height 480))
  (declare (ignore pane))
  (clim:make-space-requirement
   :width width :height height :min-width 1 :min-height 1
   :max-width clim:+fill+ :max-height clim:+fill+))

(defmethod clim:allocate-space ((pane workbench-layer-pane) width height)
  ;; Status is fixed shell chrome. Its McCLIM parent, not an application
  ;; placement affine, owns top-edge layout and the child's retained clip.
  (dolist (child (clim:sheet-children pane))
    (cond
      ((typep child 'mcluv::status-bar-pane)
       (clim:move-and-resize-sheet
        child 0 0 width mcluv::+status-bar-height+))
      ((typep child 'mcluv::mx-command-menu-pane)
       (clim:move-and-resize-sheet
        child
        (max 0 (floor (- width mcluv::+command-menu-width+) 2))
        (max 0 (floor (- height mcluv::+command-menu-height+) 2))
        mcluv::+command-menu-width+ mcluv::+command-menu-height+))
      ((typep child 'mcluv::source-update-pane)
       (clim:move-and-resize-sheet
        child
        (max 0 (floor (- width mcluv::+source-update-width+) 2))
        (max 0 (floor (- height mcluv::+source-update-height+) 2))
        mcluv::+source-update-width+ mcluv::+source-update-height+)))))

(defclass workbench-layout-pane
    (clime:never-repaint-background-mixin
     clim-internals::multiple-child-composite-pane)
  ())

(defmethod initialize-instance :after
    ((pane workbench-layout-pane) &key contents)
  (dolist (child contents)
    (clim:sheet-adopt-child pane child)))

(defmethod clim:compose-space
    ((pane workbench-layout-pane) &key (width 640) (height 480))
  (declare (ignore pane))
  (clim:make-space-requirement
   :width width :height height :min-width 1 :min-height 1
   :max-width clim:+fill+ :max-height clim:+fill+))

(defmethod clim:allocate-space ((pane workbench-layout-pane) width height)
  ;; Children are full-viewport layer roots in painter order. Their authored
  ;; descendants, rather than application placement adapters, will own actual
  ;; panel geometry and clipping.
  (dolist (child (clim:sheet-children pane))
    (clim:move-and-resize-sheet child 0 0 width height)))

(clim:define-application-frame workbench-frame
    (mcluv:status-bar mcluv::command-menu-state mcluv::source-update-state)
  ((application :initarg :application :reader frame-workbench-application))
  (:menu-bar nil)
  (:panes
   (passive (clim:make-pane 'workbench-layer-pane :kind :passive
                            :contents
                            (list
                             (clim:make-pane
                              'mcluv::status-bar-pane
                              :background clim:+transparent-ink+
                              :width mcluv::*status-bar-construction-width*
                              :height mcluv::+status-bar-height+
                              :min-width 1
                              :min-height mcluv::+status-bar-height+
                              :max-width clim:+fill+
                              :max-height mcluv::+status-bar-height+))))
   (modeless (clim:make-pane 'workbench-layer-pane :kind :modeless
                             :contents nil))
   (transient (clim:make-pane 'workbench-layer-pane :kind :transient
                              :contents nil))
   (modal
    (clim:make-pane
     'workbench-layer-pane :kind :modal
     :contents
     (list
      (clim:make-pane
       'mcluv::mx-command-menu-pane
       :background clim:+transparent-ink+
       :width mcluv::+command-menu-width+
       :height mcluv::+command-menu-height+)
      (clim:make-pane
       'mcluv::source-update-pane
       :background clim:+transparent-ink+
       :width mcluv::+source-update-width+
       :height mcluv::+source-update-height+)))))
  (:layouts
   (default
    (clim:make-pane 'workbench-layout-pane
                    :contents (list passive modeless transient modal)))))

(defclass workbench-layer ()
  ((kind :initarg :kind :reader workbench-layer-kind)
   (pane :initarg :pane :reader workbench-layer-pane)
   (focus :initform nil :accessor workbench-layer-focus)
   (visible-p :initarg :visible-p :initform nil
              :accessor workbench-layer-visible-p)))

(defclass workbench (luv:canvas-event-handler)
  ((application :initarg :application :reader workbench-application)
   (frame :initarg :frame :reader workbench-frame)
   (mirror :initarg :mirror :reader workbench-mirror)
   (compositor :initarg :compositor :reader workbench-compositor)
   (layers :initarg :layers :reader workbench-layers)
   (downstream-handler :initarg :downstream-handler
                       :reader workbench-downstream-handler)
   (input-token :initform nil :accessor workbench-input-token)
   (held-input :initform (make-hash-table :test #'equal)
               :reader workbench-held-input)
   (shell-held-input :initform (make-hash-table :test #'equal)
                     :reader workbench-shell-held-input)
   (swallowed-releases :initform (make-hash-table :test #'equal)
                       :reader workbench-swallowed-releases)
   (active-tool :initform nil :accessor workbench-active-tool)
   (stopped-p :initform nil :accessor workbench-stopped-p)))

(defvar *application-workbenches*
  (make-hash-table :test #'eq :weakness :key))

(defvar *application-workbenches-lock*
  (sb-thread:make-mutex :name "Luv application workbenches"))

(defun application-workbench (application)
  (sb-thread:with-mutex (*application-workbenches-lock*)
    (gethash application *application-workbenches*)))

(defun workbench-layer (workbench kind)
  (or (find kind (workbench-layers workbench) :key #'workbench-layer-kind)
      (error "Unknown workbench layer ~S." kind)))

(defun workbench-tool-pane (workbench tool)
  (find-if
   (lambda (pane)
     (ecase tool
       (:command-menu (typep pane 'mcluv::mx-command-menu-pane))
       (:source-update (typep pane 'mcluv::source-update-pane))))
   (clim:sheet-children
    (workbench-layer-pane (workbench-layer workbench :modal)))))

(defun set-workbench-tool-pane (workbench tool)
  (dolist (candidate '(:command-menu :source-update))
    (setf (clim:sheet-enabled-p (workbench-tool-pane workbench candidate))
          (eq candidate tool)))
  (setf (workbench-active-tool workbench) tool)
  (let ((layer (workbench-layer workbench :modal)))
    (setf (workbench-layer-focus layer)
          (and tool (workbench-tool-pane workbench tool)))
    (when (workbench-layer-visible-p layer)
      (focus-workbench-layer workbench layer)))
  workbench)

(defun call-with-workbench-frame-mutation (workbench function)
  "Run FUNCTION at WORKBENCH's native frame boundary."
  (luv:request-canvas-frame
   (workbench-application-canvas (workbench-application workbench))
   (lambda (timestamp)
     (declare (ignore timestamp))
     (funcall function)))
  workbench)

(defun %open-workbench-command-menu (workbench)
  (let ((frame (workbench-frame workbench)))
    (mcluv:refresh-command-menu-entries frame)
    (set-workbench-tool-pane workbench :command-menu)
    (show-workbench-layer workbench :modal)
    (mcluv:repaint-command-menu frame))
  workbench)

(defun open-workbench-command-menu (workbench)
  "Open the fixed M-x pane at WORKBENCH's frame boundary."
  (call-with-workbench-frame-mutation
   workbench (lambda () (%open-workbench-command-menu workbench))))

(defun %close-workbench-command-menu (workbench)
  (when (eq :command-menu (workbench-active-tool workbench))
    (hide-workbench-layer workbench :modal)
    (set-workbench-tool-pane workbench nil)
    (mcluv:repaint-gpu-mirror (workbench-mirror workbench)))
  nil)

(defun close-workbench-command-menu (workbench)
  (call-with-workbench-frame-mutation
   workbench (lambda () (%close-workbench-command-menu workbench)))
  nil)

(defun toggle-workbench-command-menu (workbench)
  (call-with-workbench-frame-mutation
   workbench
   (lambda ()
     (if (eq :command-menu (workbench-active-tool workbench))
         (%close-workbench-command-menu workbench)
         (%open-workbench-command-menu workbench))))
  t)

(defun %open-workbench-source-update (workbench)
  (set-workbench-tool-pane workbench :source-update)
  (show-workbench-layer workbench :modal)
  (mcluv::repaint-source-update (workbench-frame workbench))
  (when (eq :idle
            (mcluv:source-update-snapshot-state
             (mcluv:current-source-update-snapshot
              (mcluv:source-update-frame-session
               (workbench-frame workbench)))))
    (mcluv:start-source-update (workbench-frame workbench)))
  workbench)

(defun open-workbench-source-update (workbench)
  "Open and start the fixed source-update pane at a frame boundary."
  (call-with-workbench-frame-mutation
   workbench (lambda () (%open-workbench-source-update workbench))))

(defun %close-workbench-source-update (workbench)
  (when (and (eq :source-update (workbench-active-tool workbench))
             (not
              (mcluv:source-update-busy-p
               (mcluv:source-update-frame-session
                (workbench-frame workbench)))))
    (hide-workbench-layer workbench :modal)
    (set-workbench-tool-pane workbench nil)
    (mcluv:repaint-gpu-mirror (workbench-mirror workbench)))
  nil)

(defun close-workbench-source-update (workbench)
  (call-with-workbench-frame-mutation
   workbench (lambda () (%close-workbench-source-update workbench)))
  nil)

(defun workbench-active-layer (workbench)
  (find-if #'workbench-layer-visible-p
           (mapcar (lambda (kind) (workbench-layer workbench kind))
                   '(:modal :transient :modeless))))

(defun workbench-input-suspended-p (workbench)
  (not (null (workbench-input-token workbench))))

(defun interactive-workbench-visible-p (workbench)
  (not (null (workbench-active-layer workbench))))

(defun sheet-descendant-p (sheet ancestor)
  (loop for current = sheet then (clim:sheet-parent current)
        while current
        thereis (eq current ancestor)))

(defun workbench-authored-sheet-p (layer sheet)
  (let ((root (workbench-layer-pane layer)))
    (and sheet
         (not (eq sheet root))
         (sheet-descendant-p sheet root))))

(defgeneric workbench-event-hits-layer-p (workbench layer event target)
  (:documentation
   "Whether McCLIM routed EVENT to an authored descendant of LAYER."))

(defmethod workbench-event-hits-layer-p
    ((workbench workbench) (layer workbench-layer) event target)
  (declare (ignore workbench event))
  (workbench-authored-sheet-p layer target))

(defun workbench-top-level-sheet (workbench)
  (alexandria:when-let ((frame (workbench-frame workbench)))
    (clim:frame-top-level-sheet frame)))

(defun workbench-port (workbench)
  (alexandria:when-let ((sheet (workbench-top-level-sheet workbench)))
    (clim:port sheet)))

(defun workbench-neutral-focus-sheet (workbench)
  (or (alexandria:when-let ((layer (first (workbench-layers workbench))))
        (clim:sheet-parent (workbench-layer-pane layer)))
      (workbench-top-level-sheet workbench)))

(defun workbench-focus (workbench)
  (alexandria:when-let ((port (workbench-port workbench)))
    (clim:port-keyboard-input-focus port)))

(defun workbench-pointer-target (workbench)
  (alexandria:when-let ((port (workbench-port workbench)))
    (clim:pointer-sheet (clim:port-pointer port))))

(defun input-event-key (event)
  (typecase event
    (luv:canvas-key-event
     (list :key (luv:canvas-key-event-key-name event)))
    (luv:canvas-pointer-button-event
     (list :button (luv:canvas-pointer-event-button event)))))

(defun note-application-input (workbench event)
  (alexandria:when-let ((key (input-event-key event)))
    (etypecase event
      ((or luv:canvas-key-press-event luv:canvas-pointer-button-press-event)
       (setf (gethash key (workbench-held-input workbench)) t))
      ((or luv:canvas-key-release-event luv:canvas-pointer-button-release-event)
       (remhash key (workbench-held-input workbench)))))
  nil)

(defun note-shell-input (workbench event layer)
  (alexandria:when-let ((key (input-event-key event)))
    (etypecase event
      ((or luv:canvas-key-press-event luv:canvas-pointer-button-press-event)
       (setf (gethash key (workbench-shell-held-input workbench)) layer))
      ((or luv:canvas-key-release-event luv:canvas-pointer-button-release-event)
       (remhash key (workbench-shell-held-input workbench)))))
  nil)

(defun begin-workbench-input-suspension (workbench)
  (unless (workbench-input-token workbench)
    (maphash (lambda (key value)
               (declare (ignore value))
               (setf (gethash key (workbench-swallowed-releases workbench)) t))
             (workbench-held-input workbench))
    (clrhash (workbench-held-input workbench))
    (setf (workbench-input-token workbench)
          (suspend-workbench-application-input
           (workbench-application workbench))))
  workbench)

(defun end-workbench-input-suspension (workbench)
  (alexandria:when-let ((token (workbench-input-token workbench)))
    (setf (workbench-input-token workbench) nil)
    (resume-workbench-application-input
     (workbench-application workbench) token))
  workbench)

(defun remember-workbench-focus (workbench)
  (when (workbench-frame workbench)
    (let ((focus (workbench-focus workbench)))
      (dolist (layer (workbench-layers workbench))
        (when (workbench-authored-sheet-p layer focus)
          (setf (workbench-layer-focus layer) focus)))))
  workbench)

(defun focus-workbench-layer (workbench layer)
  (when (workbench-frame workbench)
    (let ((focus (and layer (workbench-layer-focus layer))))
      ;; A layer root is only transparent layout. Never make it an input hit.
      (setf (clim:port-keyboard-input-focus (workbench-port workbench))
            (or (and layer focus
                     (workbench-authored-sheet-p layer focus)
                     focus)
                (workbench-neutral-focus-sheet workbench)))))
  layer)

(defun show-workbench-layer (workbench kind)
  "Show fixed layer KIND and apply its explicit focus/input policy."
  (let ((layer (workbench-layer workbench kind)))
    (unless (workbench-layer-visible-p layer)
      (remember-workbench-focus workbench)
      (setf (workbench-layer-visible-p layer) t
            (clim:sheet-enabled-p (workbench-layer-pane layer)) t)
      (when (member kind '(:modeless :transient :modal))
        (begin-workbench-input-suspension workbench)
        (focus-workbench-layer workbench layer)))
    layer))

(defun hide-workbench-layer (workbench kind)
  (let ((layer (workbench-layer workbench kind)))
    (when (workbench-layer-visible-p layer)
      (remember-workbench-focus workbench)
      (setf (workbench-layer-visible-p layer) nil
            (clim:sheet-enabled-p (workbench-layer-pane layer)) nil)
      (when (member kind '(:modeless :transient :modal))
        (let ((next (workbench-active-layer workbench)))
          (focus-workbench-layer workbench next)
          (unless next
            (end-workbench-input-suspension workbench)))))
    layer))

(defun release-event-swallowed-p (workbench event)
  (alexandria:when-let ((key (input-event-key event)))
    (when (and (typep event '(or luv:canvas-key-release-event
                              luv:canvas-pointer-button-release-event))
               (gethash key (workbench-swallowed-releases workbench)))
      (remhash key (workbench-swallowed-releases workbench))
      t)))

(defun shell-input-event-p (event)
  (typep event '(or luv:canvas-key-event luv:canvas-pointer-event)))

(defun workbench-pane-local-position (workbench pane event)
  (clim:untransform-position
   (clim:sheet-delta-transformation
    pane (workbench-top-level-sheet workbench))
   (luv:canvas-pointer-event-x event)
   (luv:canvas-pointer-event-y event)))

(defun handle-workbench-tool-event (workbench event)
  "Apply fixed tool semantics after McCLIM has routed EVENT through its pane."
  (let ((frame (workbench-frame workbench)))
    (case (workbench-active-tool workbench)
      (:command-menu
       (cond
         ((typep event 'luv:canvas-key-press-event)
          (multiple-value-bind (action command)
              (mcluv:handle-command-menu-key-event frame event)
            (case action
              (:dismiss (close-workbench-command-menu workbench))
              (:execute
               (mcluv:execute-command-menu-command
                frame command
                :before-execute
                (lambda ()
                  (%close-workbench-command-menu workbench)))))))
         ((typep event 'luv:canvas-pointer-button-press-event)
          (let ((pane (workbench-tool-pane workbench :command-menu)))
            (multiple-value-bind (x y)
                (workbench-pane-local-position workbench pane event)
              (multiple-value-bind (action command)
                  (mcluv:handle-command-menu-pointer-press
                   frame x y (luv:canvas-pointer-event-button event))
                (when (eq action :execute)
                  (mcluv:execute-command-menu-command
                   frame command
                   :before-execute
                   (lambda ()
                     (%close-workbench-command-menu workbench))))))))))
      (:source-update
       (cond
         ((typep event 'luv:canvas-key-press-event)
          (when (eq :dismiss
                    (mcluv:handle-source-update-key-event frame event))
            (%close-workbench-source-update workbench)))
         ((typep event 'luv:canvas-pointer-button-press-event)
          (let ((pane (workbench-tool-pane workbench :source-update)))
            (multiple-value-bind (x y)
                (workbench-pane-local-position workbench pane event)
              (mcluv:handle-source-update-pointer-press
               frame x y (luv:canvas-pointer-event-button event))))))))))

(defun dispatch-workbench-event (workbench event)
  "Offer EVENT shell-first and return true exactly when application input stops.

Passive UI never consumes. Modeless UI receives events while visible but only
claims key/button input routed by McCLIM to an authored descendant. Transient
UI closes on a press routed to transparent root background; modal UI captures
every key and pointer event."
  (when (release-event-swallowed-p workbench event)
    (return-from dispatch-workbench-event t))
  (let* ((key (input-event-key event))
         (shell-press-layer
           (and key (gethash key (workbench-shell-held-input workbench)))))
    ;; If the layer which consumed the press has closed, the release belongs
    ;; to neither a new shell layer nor the application.
    (when (and shell-press-layer
               (typep event '(or luv:canvas-key-release-event
                                  luv:canvas-pointer-button-release-event))
               (not (workbench-layer-visible-p shell-press-layer)))
      (remhash key (workbench-shell-held-input workbench))
      (return-from dispatch-workbench-event t)))
  (when (typep event 'luv:canvas-window-resized-event)
    (mcluv:dispatch-embedded-mirror-event (workbench-mirror workbench) event)
    (return-from dispatch-workbench-event nil))
  (let ((active (workbench-active-layer workbench))
        (keyboard-target
          (and (typep event 'luv:canvas-key-event)
               (workbench-focus workbench))))
    (cond
      ((null active) nil)
      ((not (shell-input-event-p event))
       (mcluv:dispatch-embedded-mirror-event (workbench-mirror workbench) event)
       nil)
      (t
       (mcluv:dispatch-embedded-mirror-event (workbench-mirror workbench) event)
       (handle-workbench-tool-event workbench event)
       (let* ((target
                (if (typep event 'luv:canvas-pointer-event)
                    (workbench-pointer-target workbench)
                    keyboard-target))
              (authored-hit-p
                (workbench-event-hits-layer-p
                 workbench active event target))
              (shell-release-p
                (and (input-event-key event)
                     (typep event '(or luv:canvas-key-release-event
                                          luv:canvas-pointer-button-release-event))
                     (gethash (input-event-key event)
                              (workbench-shell-held-input workbench))))
              (consumed-p
                (case (workbench-layer-kind active)
                  (:modal t)
                  (:transient
                   (when (and
                          (typep event 'luv:canvas-pointer-button-press-event)
                          (not authored-hit-p))
                     (hide-workbench-layer workbench :transient))
                   t)
                  (:modeless
                   (and (typep event '(or luv:canvas-key-event
                                          luv:canvas-pointer-button-event))
                        authored-hit-p))
                  (otherwise nil))))
         ;; McCLIM correctly focuses the innermost pointer target. A bare
         ;; layer root is transparent shell layout, so do not retain it as
         ;; keyboard focus after its background press.
         (when (and (typep event 'luv:canvas-pointer-button-press-event)
                    (not authored-hit-p)
                    (workbench-layer-visible-p active))
           (setf (workbench-layer-focus active) nil)
           (alexandria:when-let ((port (workbench-port workbench)))
             (setf (clim:port-keyboard-input-focus port)
                   (workbench-neutral-focus-sheet workbench))))
         (values (or consumed-p shell-release-p) active))))))

(defmethod luv:handle-canvas-event
    ((workbench workbench) canvas event)
  (multiple-value-bind (consumed-p layer)
      (dispatch-workbench-event workbench event)
    (if consumed-p
        (note-shell-input workbench event layer)
        (progn
          (note-application-input workbench event)
          (luv:handle-canvas-event
           (workbench-downstream-handler workbench) canvas event)))))

(defun workbench-screen-state ()
  (make-array 12 :element-type 'single-float
              :initial-contents
              '(0.0 0.0 0.0 1.0
                1.0 0.0 0.0 0.0
                0.0 1.0 0.0 0.0)))

(defun refresh-workbench (workbench)
  (multiple-value-bind (width height)
      (luv:canvas-logical-size
       (workbench-application-canvas (workbench-application workbench)))
    (declare (ignore height))
    ;; Semantic sampling and sparse repaint remain shared status behavior;
    ;; layout and prepared-stream ownership now belong to this one shell frame.
    (mcluv:refresh-status-bar
     (workbench-frame workbench) width :resize-frame-p nil)
    (mcluv:prepare-status-bar (workbench-frame workbench))
    (case (workbench-active-tool workbench)
      (:command-menu
       (mcluv:prepare-command-menu (workbench-frame workbench)))
      (:source-update
       (mcluv:refresh-source-update (workbench-frame workbench))
       (mcluv:prepare-source-update (workbench-frame workbench)))))
  workbench)

(defun encode-workbench (workbench pass surface-texture)
  "Replay WORKBENCH into APPLICATION's open final presentation PASS.

The portable HAL cannot reopen a completed Vulkan presentation attachment with
LOAD. This deliberately small fallback keeps pipeline preparation outside the
pass and gives the shell neither renderer ownership nor queue submission."
  (mcluv:encode-direct-gpu-mirror
   (workbench-compositor workbench) pass surface-texture
   (workbench-screen-state))
  workbench)

(defun make-workbench-frame (application width height)
  (let* ((canvas (workbench-application-canvas application))
         (context (workbench-application-context application))
         (device (luv:context-device context))
         (port (clim:find-port :server-path '(:luv-gpu)))
         (manager (or (first (clim-internals::frame-managers port))
                      (make-instance 'mcluv:luv-frame-manager :port port)))
         (mcluv:*embedded-mirror-target* canvas)
         (mcluv:*embedded-mirror-context* context)
         (mcluv:*embedded-mirror-device* device)
         (mcluv::*status-bar-construction-width* width)
         (source-session
           (mcluv:make-source-update-session
            (mcluv:source-update-root-for application)
            (mcluv:source-update-systems-for application)
            :start-p nil))
         (frame (clim:make-application-frame
                 'workbench-frame :application application
                 :frame-manager manager :enable t
                 :owner-frame
                 (workbench-application-command-frame application)
                 :command-tables (mcluv:command-menu-tables-for application)
                 :session source-session
                 :owner application :logical-width width
                 :worktree
                 (mcluv::cached-status-bar-worktree
                  (mcluv:status-bar-source-root application))))
         (mirror (clim:sheet-direct-mirror
                  (clim:frame-top-level-sheet frame))))
    (mcluv:make-gpu-frame-background-transparent frame)
    (dolist (tool '(:command-menu :source-update))
      (setf (clim:sheet-enabled-p
             (find-if
              (lambda (pane)
                (ecase tool
                  (:command-menu
                   (typep pane 'mcluv::mx-command-menu-pane))
                  (:source-update
                   (typep pane 'mcluv::source-update-pane))))
              (clim:sheet-children (clim:find-pane-named frame 'modal))))
            nil))
    (mcluv:dispatch-embedded-mirror-event
     mirror
     (make-instance 'luv:canvas-window-resized-event
                    :timestamp 0 :width width :height height))
    (mcluv:repaint-gpu-mirror mirror)
    (values frame mirror)))

(defgeneric quiesce-workbench (workbench)
  (:documentation "Finish or cancel shell asynchronous work before its fence."))

(defmethod quiesce-workbench ((workbench workbench))
  (when (workbench-frame workbench)
    (mcluv:quiesce-source-update-session
     (mcluv:source-update-frame-session (workbench-frame workbench))))
  workbench)

(defgeneric release-workbench-resources (workbench)
  (:documentation "Release retained shell resources while the GPU remains live."))

(defmethod release-workbench-resources ((workbench workbench))
  (unless (eq :disowned (clim:frame-state (workbench-frame workbench)))
    (clim:destroy-frame (workbench-frame workbench)))
  workbench)

(defun %release-workbench (workbench)
  (unless (workbench-stopped-p workbench)
    (setf (workbench-stopped-p workbench) t)
    (end-workbench-input-suspension workbench)
    (release-workbench-resources workbench))
  nil)

(defun start-workbench (application)
  "Construct and atomically admit APPLICATION's one workbench.

Construction, publication, and event ownership occur at the native frame
boundary. A stop race consumes and releases the unpublished shell before
signalling APPLICATION-ATTACHMENT-CLOSED."
  (alexandria:when-let ((present (application-workbench application)))
    (return-from start-workbench present))
  (let* ((canvas (workbench-application-canvas application))
         (controller (workbench-application-stop-controller application)))
    (luv:request-canvas-frame
     canvas
     (lambda (timestamp)
       (declare (ignore timestamp))
       (multiple-value-bind (width height) (luv:canvas-logical-size canvas)
         (multiple-value-bind (frame mirror)
             (make-workbench-frame application width height)
           (let* ((compositor
                    (make-instance 'mcluv:direct-gpu-mirror-compositor
                                   :mirror mirror))
                  (layers
                    (mapcar
                     (lambda (kind)
                       (let ((pane
                               (clim:find-pane-named
                                frame
                                (ecase kind
                                  (:passive 'passive)
                                  (:modeless 'modeless)
                                  (:transient 'transient)
                                  (:modal 'modal)))))
                         (setf (clim:sheet-enabled-p pane) (eq kind :passive))
                         (make-instance 'workbench-layer
                                        :kind kind :pane pane
                                        :visible-p (eq kind :passive))))
                     '(:passive :modeless :transient :modal)))
                  (workbench
                    (make-instance
                     'workbench :application application :frame frame
                     :mirror mirror :compositor compositor :layers layers
                     :downstream-handler (luv:canvas-event-handler canvas)))
                  (published-p nil))
             (setf (mcluv:mirror-compositor mirror) compositor)
             (unwind-protect
                  (prog1
                      (luv:call-with-running-stop-controller
                       controller
                       (lambda ()
                         (sb-thread:with-mutex
                             (*application-workbenches-lock*)
                           (unless
                               (gethash application *application-workbenches*)
                             (setf
                              (gethash application *application-workbenches*)
                              workbench
                              (luv:canvas-event-handler canvas) workbench
                              published-p t))))
                       :attachment workbench)
                    (mcluv:repaint-gpu-mirror mirror))
               (unless published-p
                 (%release-workbench workbench)))))))))
    ;; SDL owner requests acknowledge completion with true rather than
    ;; forwarding callback values. Publication is the authoritative result.
    (application-workbench application))

(defun stop-workbench (workbench)
  "Quiesce, detach, fence, and release WORKBENCH while its device remains live."
  (unless (workbench-stopped-p workbench)
    (quiesce-workbench workbench)
    (let* ((application (workbench-application workbench))
           (canvas (workbench-application-canvas application))
           (context (workbench-application-context application)))
      (declare (ignore context))
      ;; This synchronous owner request is the terminal frame fence: no encoder
      ;; can retain the shell after detachment and before resource release.
      (luv:request-canvas-frame
       canvas
       (lambda (timestamp)
         (declare (ignore timestamp))
         (sb-thread:with-mutex (*application-workbenches-lock*)
           (when (eq workbench
                     (gethash application *application-workbenches*))
             (remhash application *application-workbenches*)))
         (when (eq workbench (luv:canvas-event-handler canvas))
           (setf (luv:canvas-event-handler canvas)
                 (workbench-downstream-handler workbench)))
         (%release-workbench workbench)))))
  nil)
