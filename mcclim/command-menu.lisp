(in-package #:mcluv)

;;; M-x is an application instrument, not a game feature.  It reads an
;;; application's existing command tables, retains one searchable snapshot,
;;; and executes the selected command on the frame which owns that vocabulary.
;;; Nothing in this file knows about a luvcraft session or a LUFT viewer.

(defconstant +command-menu-width+ 620)
(defconstant +command-menu-height+ 420)
(defconstant +command-menu-margin+ 26)
(defconstant +command-menu-row-height+ 34)
(defconstant +command-menu-result-limit+ 8)
(defconstant +command-menu-field-top+ 54)
(defconstant +command-menu-field-bottom+ 98)
(defconstant +command-menu-results-top+ 116)
(defconstant +command-menu-query-limit+ 80)
(defconstant +command-menu-viewport-margin+ 24)

(defun command-menu-alpha-ink (red green blue alpha)
  (compose-in (make-rgb-color red green blue) (make-opacity alpha)))

;; Uniform translucent inks remain uniform composita in McCLIM.  The direct
;; compositor carries their alpha with the semantic primitive and
;; premultiplies RGB in its vertex shader before blending over the game.
(defparameter *command-menu-shadow-ink*
  (command-menu-alpha-ink 0.0 0.0 0.0 0.42))
(defparameter *command-menu-edge-ink*
  (command-menu-alpha-ink 0.38 0.44 0.40 0.88))
(defparameter *command-menu-panel-ink*
  (command-menu-alpha-ink 0.055 0.060 0.056 0.92))
(defparameter *command-menu-field-edge-ink*
  (command-menu-alpha-ink 0.48 0.67 0.50 0.86))
(defparameter *command-menu-field-ink*
  (command-menu-alpha-ink 0.018 0.021 0.019 0.78))
(defparameter *command-menu-row-ink*
  (command-menu-alpha-ink 0.11 0.12 0.112 0.64))
(defparameter *command-menu-selected-row-ink*
  (command-menu-alpha-ink 0.55 0.75 0.53 0.94))
(defparameter *command-menu-text-ink* (make-rgb-color 0.93 0.92 0.87))
(defparameter *command-menu-muted-ink* (make-rgb-color 0.64 0.66 0.61))
(defparameter *command-menu-selected-text-ink*
  (make-rgb-color 0.055 0.065 0.055))

(define-condition command-menu-requires-direct-gpu (error)
  ((object :initarg :object :reader command-menu-non-gpu-object))
  (:report
   (lambda (condition stream)
     (format stream
             "M-x requires retained direct GPU McCLIM media, not ~S."
             (command-menu-non-gpu-object condition)))))

(define-condition command-menu-direct-presentation-violation (error)
  ((reason :initarg :reason :reader command-menu-presentation-violation-reason))
  (:report
   (lambda (condition stream)
     (format stream "M-x violated its direct presentation contract: ~A."
             (command-menu-presentation-violation-reason condition)))))

(defclass command-menu-entry ()
  ((label :initarg :label :reader command-menu-entry-label)
   (command-name :initarg :command-name :reader command-menu-entry-command-name)
   (table :initarg :table :reader command-menu-entry-table))
  (:documentation
   "One cached, executable command-table entry displayed by M-x."))

(defmethod print-object ((entry command-menu-entry) stream)
  (print-unreadable-object (entry stream :type t)
    (format stream "~A -> ~S"
            (command-menu-entry-label entry)
            (command-menu-entry-command-name entry))))

(defgeneric command-menu-tables-for (owner-frame)
  (:documentation
   "Return the command tables whose executable vocabulary M-x shows.

The default is OWNER-FRAME's complete table.  Applications may either pass an
explicit list to MAKE-EMBEDDED-COMMAND-MENU or specialize this protocol when
their semantic layers should remain separately inspectable."))

(defmethod command-menu-tables-for ((owner-frame standard-application-frame))
  (list (frame-command-table owner-frame)))

(defmethod command-menu-tables-for ((owner-frame t))
  (declare (ignore owner-frame))
  nil)

(defun command-menu-command-without-required-arguments-p (name)
  "Whether NAME is a command M-x can execute without prompting yet."
  (alexandria:when-let
      ((parsers (gethash name climi::*command-parser-table*)))
    (null (climi::required-args parsers))))

(defun command-menu-entries-for-tables
    (tables &key owner-frame (inherited nil))
  "Return cached M-x entries read from TABLES.

Each command appears once even when command-table inheritance exposes it in
several layers.  OWNER-FRAME, when supplied, excludes commands which that
application currently disables.  Commands with required arguments remain out
until M-x has an argument-prompting interaction instead of becoming dead rows."
  (let ((entries nil)
        (seen (make-hash-table :test #'eq)))
    (dolist (table tables)
      (map-over-command-table-commands
       (lambda (name)
         (unless (gethash name seen)
           (setf (gethash name seen) t)
           (alexandria:when-let
               ((label
                  (command-line-name-for-command name table :errorp nil)))
             (when (and (command-menu-command-without-required-arguments-p
                         name)
                        (or (null owner-frame)
                            (command-enabled name owner-frame)))
               (push (make-instance 'command-menu-entry
                                    :label label
                                    :command-name name
                                    :table table)
                     entries)))))
       table :inherited inherited))
    (sort entries #'string-lessp :key #'command-menu-entry-label)))

(defun matching-command-menu-entries
    (entries query &key (key #'command-menu-entry-label))
  "Return ENTRIES whose labels contain every whitespace-separated query word."
  (let ((words (remove-if #'alexandria:emptyp
                          (uiop:split-string query
                                             :separator '(#\Space #\Tab)))))
    (if (null words)
        entries
        (remove-if-not
         (lambda (entry)
           (let ((label (funcall key entry)))
             (every (lambda (word)
                      (search word label :test #'char-equal))
                    words)))
         entries))))

(defclass mx-command-menu-pane (transparent-gpu-application-pane) ())

(defclass command-menu-state ()
  ((owner-frame :initarg :owner-frame :initform nil
                :reader command-menu-owner-frame)
   (command-tables :initarg :command-tables :initform nil
                   :accessor command-menu-command-tables)
   (inherited-p :initarg :inherited :initform nil
                :accessor command-menu-inherited-p)
   ;; Command discovery happens explicitly at construction or refresh time.
   ;; Repaint reads only these cached rows and never invokes application code.
   (entries :initform nil :accessor command-menu-entries)
   (results :initform nil :accessor command-menu-results)
   (query :initform "" :accessor command-menu-query)
   (selected :initform 0 :accessor command-menu-selected)
   (dirty-p :initform t :accessor command-menu-dirty-p)))

(define-application-frame command-menu
    (standard-application-frame command-menu-state)
  ()
  (:menu-bar nil)
  (:panes
   (sheet (make-pane 'mx-command-menu-pane
                     :background +transparent-ink+
                     :width +command-menu-width+
                     :height +command-menu-height+
                     :min-width +command-menu-width+
                     :min-height +command-menu-height+
                     :max-width +command-menu-width+
                     :max-height +command-menu-height+)))
  ;; A one-pane overlay has no layout job.  Making SHEET the layout also keeps
  ;; an HRACK-PANE (and its opaque McCLIM background) out of the retained GPU
  ;; stream, including the rounded panel's transparent corner samples.
  (:layouts (default sheet)))

(defun update-command-menu-results (frame &key reset-selection-p)
  "Synchronously filter FRAME's cached entries without repainting it."
  (setf (command-menu-results frame)
        (matching-command-menu-entries
         (command-menu-entries frame) (command-menu-query frame)))
  (when reset-selection-p
    (setf (command-menu-selected frame) 0))
  frame)

(defun refresh-command-menu-entries (frame)
  "Re-read FRAME's application vocabulary outside its repaint method."
  (setf (command-menu-entries frame)
        (command-menu-entries-for-tables
         (command-menu-command-tables frame)
         :owner-frame (command-menu-owner-frame frame)
         :inherited (command-menu-inherited-p frame))
        (command-menu-dirty-p frame) t)
  (update-command-menu-results frame :reset-selection-p t))

(defmethod initialize-instance :after ((frame command-menu-state) &key)
  (unless (command-menu-command-tables frame)
    (setf (command-menu-command-tables frame)
          (command-menu-tables-for (command-menu-owner-frame frame))
          (command-menu-inherited-p frame) t))
  (refresh-command-menu-entries frame))

(defun command-menu-visible-results (frame)
  "Return FRAME's visible result window and its zero-based start index."
  (let* ((results (command-menu-results frame))
         (count (length results))
         (selected (if (plusp count)
                       (mod (command-menu-selected frame) count)
                       0))
         (start
           (min (max 0 (- selected (1- +command-menu-result-limit+)))
                (max 0 (- count +command-menu-result-limit+)))))
    (values (subseq results start
                    (min count (+ start +command-menu-result-limit+)))
            start)))

(defun command-menu-selected-command (frame)
  "Return FRAME's selected argument-free command form, or NIL."
  (let ((results (command-menu-results frame)))
    (when results
      (list
       (command-menu-entry-command-name
        (nth (mod (command-menu-selected frame) (length results)) results))))))

(defun draw-command-menu-row (stream entry top selected-p)
  (let ((left +command-menu-margin+)
        (right (- +command-menu-width+ +command-menu-margin+)))
    (draw-analytic-rounded-rectangle*
     stream left (+ top 2) right (- (+ top +command-menu-row-height+) 2)
     :radius 7
     :ink (if selected-p
              *command-menu-selected-row-ink*
              *command-menu-row-ink*))
    (draw-text* stream (command-menu-entry-label entry) (+ left 12)
                (+ top (/ +command-menu-row-height+ 2))
                :align-y :center :text-size 17
                :ink (if selected-p
                         *command-menu-selected-text-ink*
                         *command-menu-text-ink*))))

(defun ensure-command-menu-gpu-medium (medium)
  (unless (typep medium 'luv-gpu-medium)
    (error 'command-menu-requires-direct-gpu :object medium))
  medium)

(defmethod handle-repaint ((pane mx-command-menu-pane) region)
  (declare (ignore region))
  (let* ((frame (pane-frame pane))
         (query (command-menu-query frame))
         (results (command-menu-results frame))
         (selected (command-menu-selected frame))
         (margin +command-menu-margin+)
         (right (- +command-menu-width+ margin)))
    (with-bounding-rectangle* (left top right-edge bottom) pane
      (with-sheet-medium (medium pane)
        (ensure-command-menu-gpu-medium medium)
        ;; Every surface is an analytic primitive.  Layered filled roundrects
        ;; provide borders without invoking McCLIM's decomposed outline path.
        (draw-analytic-rounded-rectangle*
         medium (+ left 7) (+ top 9) (- right-edge 1) (- bottom 1)
         :radius 18 :ink *command-menu-shadow-ink*)
        (draw-analytic-rounded-rectangle*
         medium left top right-edge bottom
         :radius 18 :ink *command-menu-edge-ink*)
        (draw-analytic-rounded-rectangle*
         medium (+ left 2) (+ top 2) (- right-edge 2) (- bottom 2)
         :radius 16 :ink *command-menu-panel-ink*)
        (draw-text* pane "M-x" margin 30
                    :align-y :center :text-size 22 :text-face :bold
                    :ink *command-menu-text-ink*)
        (draw-text* pane "Esc cancels" right 30
                    :align-x :right :align-y :center :text-size 14
                    :ink *command-menu-muted-ink*)
        (draw-analytic-rounded-rectangle*
         medium margin +command-menu-field-top+
         right +command-menu-field-bottom+
         :radius 9 :ink *command-menu-field-edge-ink*)
        (draw-analytic-rounded-rectangle*
         medium (+ margin 1) (+ +command-menu-field-top+ 1)
         (- right 1) (- +command-menu-field-bottom+ 1)
         :radius 8 :ink *command-menu-field-ink*)
        (let* ((prompt "M-x ")
               (text (concatenate 'string prompt query))
               (text-x (+ margin 12))
               (text-y (/ (+ +command-menu-field-top+
                              +command-menu-field-bottom+)
                           2))
               (width (text-size pane text
                                 :text-style
                                 (make-text-style nil nil 18))))
          (draw-text* pane text text-x text-y
                      :align-y :center :text-size 18
                      :ink *command-menu-text-ink*)
          (draw-rectangle* pane (+ text-x width 2) (- text-y 11)
                           (+ text-x width 4) (+ text-y 11)
                           :ink *command-menu-field-edge-ink*))
        (multiple-value-bind (visible start)
            (command-menu-visible-results frame)
          (loop for entry in visible
                for index from start
                for y from +command-menu-results-top+
                      by +command-menu-row-height+
                do (draw-command-menu-row pane entry y (= index selected))))
        (when (null results)
          (draw-text* pane "No matching command" margin
                      (+ +command-menu-results-top+ 20)
                      :align-y :center :text-size 16
                      :ink *command-menu-muted-ink*))
        (draw-text* pane
                    (format nil "~D command~:P  ·  ↑↓ choose  ·  RET runs"
                            (length results))
                    margin (- +command-menu-height+ 24)
                    :align-y :center :text-size 14
                    :ink *command-menu-muted-ink*)))))

(defun command-menu-mirror (frame &key (errorp t))
  "Return FRAME's embedded direct-GPU mirror."
  (let* ((sheet (frame-top-level-sheet frame))
         (mirror (and sheet (sheet-direct-mirror sheet))))
    (cond ((typep mirror 'luv-gpu-mirror) mirror)
          (errorp
           (error 'command-menu-requires-direct-gpu :object mirror))
          (t nil))))

(defun validate-command-menu-direct-presentation (frame)
  "Assert FRAME contains analytic/text GPU media and no rasterized image path."
  (let* ((mirror (command-menu-mirror frame))
         (sheet (mirror-sheet mirror)))
    (when (mirror-texture mirror)
      (error 'command-menu-direct-presentation-violation
             :reason "the embedded mirror acquired a backing texture"))
    (dolist (painted-sheet (gpu-sheet-paint-order sheet))
      (let ((medium (gpu-sheet-presentation-medium painted-sheet)))
        (when (and (typep medium 'luv-gpu-medium)
                   (gpu-medium-fallback-report medium))
          (error 'command-menu-direct-presentation-violation
                 :reason
                 (format nil "~S used decomposed primitive fallbacks"
                         painted-sheet)))))
    (when (find-if (lambda (command)
                     (typep command 'gpu-prepared-image-command))
                   (gpu-mirror-prepared-commands mirror))
      (error 'command-menu-direct-presentation-violation
             :reason "a rasterized image command reached the prepared stream"))
    frame))

(defun repaint-command-menu (frame)
  "Synchronously rebuild and publish FRAME's retained semantic GPU stream.

An embedded GPU mirror performs no drawable acquisition, command submission,
readback, or GPU wait here.  It snapshots and uploads the complete semantic
revision for replay by the owning application's next presentation pass."
  (alexandria:when-let ((mirror (command-menu-mirror frame :errorp nil)))
    (unless (mirror-embedded-p mirror)
      (error 'command-menu-requires-direct-gpu :object mirror))
    (repaint-gpu-mirror mirror)
    (validate-command-menu-direct-presentation frame)
    (setf (command-menu-dirty-p frame) nil))
  frame)

(defun invalidate-command-menu (frame)
  "Mark FRAME changed and synchronously repaint it when it is realized."
  (setf (command-menu-dirty-p frame) t)
  (repaint-command-menu frame))

(defun prepare-command-menu (frame)
  "Ensure FRAME has a complete retained revision for direct composition."
  (let ((mirror (command-menu-mirror frame)))
    (if (or (command-menu-dirty-p frame)
            (null (gpu-mirror-prepared-commands mirror)))
        (repaint-command-menu frame)
        ;; Static semantic media must still observe live shader revisions.
        (prepare-gpu-mirror-compositor mirror)))
  frame)

(defun command-menu-panel-scale (viewport-extent)
  (destructuring-bind (viewport-width viewport-height) viewport-extent
    (min 1.0
         (/ (max 1.0 (- viewport-width
                        (* 2 +command-menu-viewport-margin+)))
            +command-menu-width+)
         (/ (max 1.0 (- viewport-height
                        (* 2 +command-menu-viewport-margin+)))
            +command-menu-height+))))

(defun command-menu-screen-state (frame viewport-logical-extent)
  "Return a centered affine state measured in destination logical pixels.

At scale one, one command-menu coordinate occupies one logical pixel in the
game viewport.  The final direct render therefore receives the destination's
native pixel density automatically: a 2x drawable evaluates every analytic
edge and Slug glyph at twice the samples without a raster upscale."
  (declare (ignore frame))
  (destructuring-bind (viewport-width viewport-height)
      viewport-logical-extent
    (let* ((scale (command-menu-panel-scale viewport-logical-extent))
           (half-width (/ (* +command-menu-width+ scale) viewport-width))
           (half-height (/ (* +command-menu-height+ scale) viewport-height)))
      (make-array
       12 :element-type 'single-float
       :initial-contents
       (mapcar (lambda (value) (coerce value 'single-float))
               (list 0.0 0.0 0.0 1.0
                     half-width 0.0 0.0 0.0
                     0.0 half-height 0.0 0.0))))))

(defun command-menu-local-coordinate
    (frame pointer-x pointer-y viewport-logical-extent)
  "Map a destination-logical pointer to FRAME coordinates, or return NIL."
  (declare (ignore frame))
  (destructuring-bind (viewport-width viewport-height)
      viewport-logical-extent
    (let* ((scale (command-menu-panel-scale viewport-logical-extent))
           (display-width (* +command-menu-width+ scale))
           (display-height (* +command-menu-height+ scale))
           (left (* 0.5 (- viewport-width display-width)))
           (top (* 0.5 (- viewport-height display-height))))
      (when (and (<= left pointer-x (+ left display-width))
                 (<= top pointer-y (+ top display-height)))
        (values (/ (- pointer-x left) scale)
                (/ (- pointer-y top) scale))))))

(defun move-command-menu-selection (frame delta)
  (let ((count (length (command-menu-results frame))))
    (when (plusp count)
      (setf (command-menu-selected frame)
            (mod (+ (command-menu-selected frame) delta) count))
      (invalidate-command-menu frame)))
  frame)

(defun edit-command-menu-query (frame query)
  (setf (command-menu-query frame) query)
  (update-command-menu-results frame :reset-selection-p t)
  (invalidate-command-menu frame))

(defun handle-command-menu-key-event (frame event)
  "Handle one portable key press and return ACTION and optional COMMAND.

ACTION is :CONTINUE, :DISMISS, or :EXECUTE.  This function never calls the
application command while it is updating or repainting M-x; the host dismisses
its modal surface first and passes COMMAND to EXECUTE-COMMAND-MENU-COMMAND."
  (check-type event luv:canvas-key-press-event)
  (when (luv:canvas-key-event-repeat-p event)
    (return-from handle-command-menu-key-event (values :continue nil)))
  (let ((key (luv:canvas-key-event-key-name event))
        (character (luv:canvas-key-event-character event)))
    (case key
      (:escape (values :dismiss nil))
      ((:return :keypad-enter)
       (values :execute (command-menu-selected-command frame)))
      (:up
       (move-command-menu-selection frame -1)
       (values :continue nil))
      (:down
       (move-command-menu-selection frame 1)
       (values :continue nil))
      (:backspace
       (let ((query (command-menu-query frame)))
         (when (plusp (length query))
           (edit-command-menu-query
            frame (subseq query 0 (1- (length query))))))
       (values :continue nil))
      (t
       (when (and character (graphic-char-p character)
                  (null (intersection
                         '(:control :meta :super)
                         (luv:canvas-key-event-modifiers event)))
                  (< (length (command-menu-query frame))
                     +command-menu-query-limit+))
         (edit-command-menu-query
          frame
          (concatenate 'string (command-menu-query frame)
                       (string character))))
       (values :continue nil)))))

(defun handle-command-menu-pointer-press (frame x y button)
  "Select the result row at local X,Y and return ACTION and COMMAND."
  (when (and (eq button :left)
             (<= 0 x +command-menu-width+)
             (<= +command-menu-results-top+ y +command-menu-height+))
    (let ((row (floor (- y +command-menu-results-top+)
                      +command-menu-row-height+)))
      (multiple-value-bind (visible start)
          (command-menu-visible-results frame)
        (when (< row (length visible))
          (setf (command-menu-selected frame) (+ start row))
          (invalidate-command-menu frame)
          (return-from handle-command-menu-pointer-press
            (values :execute (command-menu-selected-command frame)))))))
  (values :continue nil))

(defun execute-command-menu-command
    (frame command &key before-execute)
  "Execute COMMAND on FRAME's owner, after optional BEFORE-EXECUTE.

The owner is captured first so BEFORE-EXECUTE may destroy FRAME while removing
the modal surface.  Application commands therefore never run underneath M-x."
  (when command
    (let ((owner (command-menu-owner-frame frame)))
      (when before-execute (funcall before-execute))
      (execute-frame-command owner command))))

(defun make-embedded-command-menu
    (owner-frame canvas context device
     &key command-tables (inherited (null command-tables)) (title "M-x"))
  "Create OWNER-FRAME's retained direct-GPU M-x surface on CANVAS.

The returned frame owns no native window and no raster backing texture.  Its
prepared semantic commands are replayed by the host application's existing
GPU pass."
  (let* ((port (find-port :server-path '(:luv-gpu)))
         (manager (or (first (climi::frame-managers port))
                      (make-instance 'luv-frame-manager :port port)))
         (frame
           (let ((*embedded-mirror-target* canvas)
                 (*embedded-mirror-context* context)
                 (*embedded-mirror-device* device))
             (make-application-frame
              'command-menu :frame-manager manager :enable t
              :owner-frame owner-frame
              :command-tables command-tables
              :inherited inherited))))
    (setf (frame-pretty-name frame) title)
    (make-gpu-frame-background-transparent frame)
    (handler-case
        (let ((mirror (command-menu-mirror frame)))
          (unless (and (mirror-embedded-p mirror)
                       (null (mirror-texture mirror)))
            (error 'command-menu-requires-direct-gpu :object mirror))
          ;; Publish one complete initial revision before the host attaches its
          ;; compositor.  This is an embedded upload only, never a drawable or
          ;; queue wait.
          (repaint-command-menu frame)
          frame)
      (error (condition)
        (unless (eq :disowned (frame-state frame))
          (destroy-frame frame))
        (error condition)))))

(defun destroy-command-menu (frame)
  "Release FRAME's mirror, retained buffers, and McCLIM frame ownership."
  (check-type frame command-menu)
  (unless (eq :disowned (frame-state frame))
    (destroy-frame frame))
  nil)
