;;; A compact terrain instrument whose state is sampled from a live luvcraft
;;; session and whose presentation is ordinary McCLIM drawing and gadgets.

(in-package #:mcluv)

(defclass surveyor-map-snapshot ()
  ((center-x :initarg :center-x :reader surveyor-snapshot-center-x)
   (center-z :initarg :center-z :reader surveyor-snapshot-center-z)
   (width :initarg :width :reader surveyor-snapshot-width)
   (depth :initarg :depth :reader surveyor-snapshot-depth)
   (heights :initarg :heights :reader surveyor-snapshot-heights)
   (materials :initarg :materials :reader surveyor-snapshot-materials)
   (lights :initarg :lights :reader surveyor-snapshot-lights)
   (minimum-height :initarg :minimum-height
                   :reader surveyor-snapshot-minimum-height)
   (maximum-height :initarg :maximum-height
                   :reader surveyor-snapshot-maximum-height)))

(defun surveyor-snapshot-offset (snapshot column row)
  (+ column (* row (surveyor-snapshot-width snapshot))))

(defun surveyor-generated-surface (world source x z)
  (if (typep source 'luvcraft:little-world-source)
      (let* ((height (luvcraft:little-world-surface-height source x z 16))
             (generated
               (luvcraft:little-world-surface-material source x z height 16)))
        (multiple-value-bind (resident status)
            (luvcraft:world-block-at world x height z)
          (values height
                  (if (and (eq status :resident) resident)
                      resident generated))))
      (loop for y from 31 downto 0
            do (multiple-value-bind (block status)
                   (luvcraft:world-block-at world x y z)
                 (when (and (eq status :resident) block)
                   (return (values y block))))
            finally (return (values 0 nil)))))

(defun capture-surveyor-map-snapshot
    (session &key (width 40) (depth 28))
  "Capture one dense inspector-scale terrain product around SESSION's player."
  (let* ((world (luvcraft:luvcraft-session-world session))
         (source (luvcraft:block-world-source world))
         (player (luvcraft:luvcraft-session-player session))
         (center-x (floor (luvcraft:player-x player)))
         (center-z (floor (luvcraft:player-z player)))
         (count (* width depth))
         (heights (make-array count :element-type '(unsigned-byte 8)))
         (materials (make-array count :initial-element nil))
         (lights (make-array count :element-type '(unsigned-byte 8)))
         (minimum-height 255)
         (maximum-height 0))
    (dotimes (row depth)
      (dotimes (column width)
        (let* ((offset (+ column (* row width)))
               (x (+ center-x column (- (floor width 2))))
               (z (+ center-z row (- (floor depth 2)))))
          (multiple-value-bind (height material)
              (surveyor-generated-surface world source x z)
            (multiple-value-bind (sky block ignored-state)
                (luvcraft:world-light-levels-at world x height z)
              (declare (ignore ignored-state))
              (setf (aref heights offset) height
                    (aref materials offset) material
                    (aref lights offset) (max sky block)
                    minimum-height (min minimum-height height)
                    maximum-height (max maximum-height height)))))))
    (make-instance
     'surveyor-map-snapshot
     :center-x center-x :center-z center-z :width width :depth depth
     :heights heights :materials materials :lights lights
     :minimum-height minimum-height :maximum-height maximum-height)))

(defclass surveyor-mode-button-pane (relief-button-pane)
  ((mode :initarg :mode :reader surveyor-button-mode)))

(defmethod handle-repaint ((pane surveyor-mode-button-pane) region)
  (declare (ignore region))
  (with-slots (climi::armed climi::pressedp) pane
    (let* ((frame (gadget-client pane))
           (active-p (eq (surveyor-button-mode pane)
                         (surveyor-map-mode frame))))
      (repaint-relief-button
       pane (and climi::armed climi::pressedp)
       (if active-p
           (make-rgb-color 0.05 0.68 0.65)
           (make-rgb-color 0.23 0.27 0.27))))))

(define-application-frame surveyor-map ()
  ((session :initarg :session :reader surveyor-map-session)
   (snapshot :initarg :snapshot :accessor surveyor-map-snapshot)
   (mode :initarg :mode :initform :terrain :accessor surveyor-map-mode))
  (:menu-bar nil)
  (:panes
   (terrain-mode
    (make-pane 'surveyor-mode-button-pane :label "TERRAIN" :mode :terrain
               :activate-callback 'activate-surveyor-mode))
   (material-mode
    (make-pane 'surveyor-mode-button-pane :label "MATERIAL" :mode :material
               :activate-callback 'activate-surveyor-mode))
   (light-mode
    (make-pane 'surveyor-mode-button-pane :label "LIGHT" :mode :light
               :activate-callback 'activate-surveyor-mode))
   (height-mode
    (make-pane 'surveyor-mode-button-pane :label "HEIGHT" :mode :height
               :activate-callback 'activate-surveyor-mode))
   (map :application :display-function 'display-surveyor-map
        :scroll-bars nil)
   (details :application :display-function 'display-surveyor-details
            :scroll-bars nil)
   (refresh
    (make-pane 'relief-button-pane :label "SURVEY"
               :activate-callback 'refresh-surveyor-map)))
  (:layouts
   (default
    (vertically (:width 760 :height 520 :spacing 8)
      (1/8 (horizontally (:spacing 8)
             terrain-mode material-mode light-mode height-mode))
      (7/8 (horizontally (:spacing 10)
             (3/4 map)
             (1/4 (vertically (:spacing 10)
                    (4/5 details)
                    (1/5 refresh)))))))))

(defun activate-surveyor-mode (gadget)
  (let ((frame (gadget-client gadget)))
    (setf (surveyor-map-mode frame) (surveyor-button-mode gadget))
    (redisplay-frame-panes frame :force-p t)))

(defun refresh-surveyor-map (gadget)
  (let ((frame (gadget-client gadget)))
    (setf (surveyor-map-snapshot frame)
          (capture-surveyor-map-snapshot (surveyor-map-session frame)))
    (redisplay-frame-panes frame :force-p t)))

(defun surveyor-material-color (material)
  (if material
      (luvcraft:block-kind-display-color material)
      '(0.08 0.12 0.14)))

(defun surveyor-cell-color (snapshot offset mode)
  (let* ((height (aref (surveyor-snapshot-heights snapshot) offset))
         (minimum (surveyor-snapshot-minimum-height snapshot))
         (span (max 1 (- (surveyor-snapshot-maximum-height snapshot) minimum)))
         (height-reading (/ (- height minimum) span))
         (material (aref (surveyor-snapshot-materials snapshot) offset))
         (light (/ (aref (surveyor-snapshot-lights snapshot) offset) 15.0)))
    (destructuring-bind (red green blue)
        (ecase mode
          (:material (surveyor-material-color material))
          (:terrain
           (let ((base (surveyor-material-color material))
                 (shade (+ 0.68 (* 0.42 height-reading))))
             (mapcar (lambda (component) (min 1.0 (* component shade))) base)))
          (:height
           (list (+ 0.10 (* 0.72 height-reading))
                 (+ 0.18 (* 0.68 height-reading))
                 (+ 0.24 (* 0.52 height-reading))))
          (:light
           (list (+ 0.03 (* 0.22 light))
                 (+ 0.08 (* 0.72 light))
                 (+ 0.10 (* 0.82 light)))))
      (make-rgb-color red green blue))))

(defun display-surveyor-map (frame stream)
  (let ((snapshot (surveyor-map-snapshot frame)))
    (with-bounding-rectangle* (left top right bottom) stream
      (draw-rectangle* stream left top right bottom
                       :ink (make-rgb-color 0.045 0.065 0.07))
      (let* ((margin 12)
             (map-left (+ left margin))
             (map-top (+ top margin))
             (map-right (- right margin))
             (map-bottom (- bottom margin))
             (cell-width (/ (- map-right map-left)
                            (surveyor-snapshot-width snapshot)))
             (cell-height (/ (- map-bottom map-top)
                             (surveyor-snapshot-depth snapshot))))
        (dotimes (row (surveyor-snapshot-depth snapshot))
          (dotimes (column (surveyor-snapshot-width snapshot))
            (let ((x1 (+ map-left (* column cell-width)))
                  (y1 (+ map-top (* row cell-height))))
              (draw-rectangle*
               stream x1 y1 (+ x1 cell-width 0.5) (+ y1 cell-height 0.5)
               :ink (surveyor-cell-color
                     snapshot (surveyor-snapshot-offset snapshot column row)
                     (surveyor-map-mode frame))))))
        (loop for column from 0 to (surveyor-snapshot-width snapshot) by 5
              for x = (+ map-left (* column cell-width))
              do (draw-line* stream x map-top x map-bottom
                             :ink (make-rgb-color 0.10 0.58 0.60)))
        (loop for row from 0 to (surveyor-snapshot-depth snapshot) by 5
              for y = (+ map-top (* row cell-height))
              do (draw-line* stream map-left y map-right y
                             :ink (make-rgb-color 0.10 0.58 0.60)))
        (let ((center-x (/ (+ map-left map-right) 2))
              (center-y (/ (+ map-top map-bottom) 2)))
          (draw-circle* stream center-x center-y 10 :filled nil
                        :line-thickness 3
                        :ink (make-rgb-color 0.90 0.98 0.92))
          (draw-line* stream (- center-x 17) center-y (+ center-x 17) center-y
                      :line-thickness 2 :ink (make-rgb-color 0.90 0.98 0.92))
          (draw-line* stream center-x (- center-y 17) center-x (+ center-y 17)
                      :line-thickness 2 :ink (make-rgb-color 0.90 0.98 0.92)))))))

(defun display-surveyor-details (frame stream)
  (let* ((snapshot (surveyor-map-snapshot frame))
         (session (surveyor-map-session frame))
         (player (luvcraft:luvcraft-session-player session))
         (x (floor (luvcraft:player-x player)))
         (y (floor (luvcraft:player-y player)))
         (z (floor (luvcraft:player-z player))))
    (with-bounding-rectangle* (left top right bottom) stream
      (draw-rectangle* stream left top right bottom
                       :ink (make-rgb-color 0.065 0.08 0.085))
      (draw-text* stream "SURVEYOR" (+ left 18) (+ top 34)
                  :text-size 20 :ink (make-rgb-color 0.20 0.90 0.87))
      (loop for text in (list (format nil "X  ~D" x)
                              (format nil "Y  ~D" y)
                              (format nil "Z  ~D" z)
                              (format nil "MODE  ~A" (surveyor-map-mode frame))
                              (format nil "SPAN  ~D x ~D"
                                      (surveyor-snapshot-width snapshot)
                                      (surveyor-snapshot-depth snapshot))
                              (format nil "HEIGHT  ~D..~D"
                                      (surveyor-snapshot-minimum-height snapshot)
                                      (surveyor-snapshot-maximum-height snapshot)))
            for line from 0
            do (draw-text* stream text (+ left 18) (+ top 76 (* line 29))
                           :text-size 15
                           :ink (make-rgb-color 0.86 0.90 0.88)))
      (draw-text* stream "live terrain product" (+ left 18) (- bottom 24)
                  :text-size 12 :ink (make-rgb-color 0.48 0.58 0.57)))))

(defun open-surveyor-map
    (session &key (server-path '(:luv-gpu)) (title "surveyor map")
                  target context device)
  "Create and enable a surveyor instrument sampled from SESSION."
  (when (and target (not (and context device)))
    (error ":TARGET requires the shared :CONTEXT and :DEVICE."))
  (let* ((port (find-port :server-path server-path))
         (manager (or (first (climi::frame-managers port))
                      (make-instance 'luv-frame-manager :port port)))
         (frame
           (let ((*embedded-mirror-target* target)
                 (*embedded-mirror-context* context)
                 (*embedded-mirror-device* device))
             (make-application-frame
              'surveyor-map :frame-manager manager :enable t
              :session session
              :snapshot (capture-surveyor-map-snapshot session)))))
    (setf (frame-pretty-name frame) title)
    frame))

(defun close-surveyor-map (frame)
  (check-type frame surveyor-map)
  (unless (eq :disowned (frame-state frame))
    (destroy-frame frame))
  nil)
