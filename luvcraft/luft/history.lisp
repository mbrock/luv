;;;; Fine history in the world (#OVEP9A): who authored a cell and when, read
;;;; from the journal's provenance, and the history lens that shows it.
;;;;
;;;; The lens tints every authored block near the camera by its age, from a
;;;; hot yellow for what was placed a moment ago to a deep blue for last
;;;; month's foundations, and floats a plate over the block at the crosshair:
;;;; "placed by Bea, Tuesday".  Both are world text (luft/render/world-text),
;;;; so walls hide them like anything else in the scene.
(in-package #:luvcraft.luft.game)

(defun world-cell-provenance (world cell)
  "Who authored CELL's current value in WORLD, and when: a
TERRAIN:CELL-PROVENANCE, or NIL when CELL is not authored or its author is
unknown, as an edit made before provenance was kept."
  (terrain:world-cell-provenance (terrain:world-source-journal (world-source world)) cell))

(defun actor-name (world actor)
  "What to call journal ACTOR in WORLD: a player's name, a companion's kind."
  (cond ((stringp actor)
         (let ((player (find-world-player world actor))
               (record (absent-player-record world actor)))
           (or (and player (player-name player))
               (getf record :name)
               actor)))
        ((and (consp actor) (eq (first actor) :inhabitant))
         (let ((inhabitant (find-world-inhabitant world (second actor))))
           (if inhabitant
               (format nil "the ~(~A~)" (inhabitant-kind inhabitant))
               "a companion")))
        ((eq actor :import) "an import")
        ((eq actor :local) "you")
        (t (princ-to-string actor))))

(defparameter *weekday-names*
  #("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))

(defparameter *month-names*
  #("January" "February" "March" "April" "May" "June" "July" "August" "September"
    "October" "November" "December"))

(defun when-phrase (time &optional (now (get-universal-time)))
  "When universal TIME was, as a person says it from NOW: \"today at 14:05\",
\"yesterday\", \"Tuesday\" within the week, else \"3 November\"."
  (multiple-value-bind (s minute hour day month year weekday) (decode-universal-time time)
    (declare (ignore s))
    (multiple-value-bind (s m h today-day today-month today-year) (decode-universal-time now)
      (declare (ignore s m h))
      (let ((days (- (floor (encode-universal-time 0 0 12 today-day today-month today-year) 86400)
                     (floor (encode-universal-time 0 0 12 day month year) 86400))))
        (cond ((<= days 0) (format nil "today at ~2,'0D:~2,'0D" hour minute))
              ((= days 1) "yesterday")
              ((< days 7) (aref *weekday-names* weekday))
              ((= year today-year) (format nil "~D ~A" day (aref *month-names* (1- month))))
              (t (format nil "~D ~A ~D" day (aref *month-names* (1- month)) year)))))))

(defun cell-history-phrase (world cell &key (now (get-universal-time)))
  "How the inspector says who authored CELL and when, such as \"placed by
Bea, Tuesday\", or NIL when nobody is known to have."
  (let ((provenance (world-cell-provenance world cell)))
    (when provenance
      (let ((time (terrain:cell-provenance-time provenance))
            (tick (terrain:cell-provenance-tick provenance)))
        (format nil "~:[removed~;placed~] by ~A~@[, ~A~]"
                (world-authored-block world cell)
                (actor-name world (terrain:cell-provenance-actor provenance))
                (cond (time (when-phrase time now))
                      (tick (format nil "at tick ~D" tick))))))))

;;; The age lens's colours: the logarithm of age, so a minute, an hour, a
;;; day, and a month are each a visible step.

(defparameter *age-lens-stops*
  '((0.0 1.0 0.92 0.35) (0.35 1.0 0.55 0.12) (0.6 0.95 0.2 0.55) (0.8 0.45 0.25 1.0)
    (1.0 0.15 0.45 1.0))
  "(POSITION R G B) along log age from now to *AGE-LENS-OLDEST*.")

(defparameter *age-lens-oldest* (* 30 86400) "Seconds after which every block is oldest blue.")
(defparameter *age-lens-alpha* 0.4)
(defparameter *age-lens-unknown* '(0.5 0.5 0.5) "An authored block whose time is unknown.")

(defun age-lens-tint (age)
  "The premultiplied colour of a block AGE seconds old, or of one of unknown
age when AGE is NIL."
  (let ((rgb (if (null age)
                 *age-lens-unknown*
                 (let ((x (min 1.0 (/ (log (+ 1.0 (max 0 age)))
                                      (log (+ 1.0 *age-lens-oldest*))))))
                   (loop for (a b) on *age-lens-stops*
                         when (or (null b) (<= x (first b)))
                           return (if (null b)
                                      (rest a)
                                      (let ((u (/ (- x (first a)) (max 1e-6 (- (first b) (first a))))))
                                        (mapcar (lambda (p q) (+ p (* u (- q p)))) (rest a) (rest b)))))))))
    (append (mapcar (lambda (c) (* c *age-lens-alpha*)) rgb) (list *age-lens-alpha*))))

;;; The lens: world text over the view's authored neighbourhood.

(defparameter *age-lens-radius* 40 "Cells around the camera whose authored blocks are tinted.")
(defparameter *age-lens-limit* 6000 "The most blocks tinted at once, nearest first.")
(defparameter *age-lens-lift* 0.004 "Cells between a block's face and its tint.")
(defparameter *history-plate-ink* '(1.6 1.55 1.4))

(defclass history-lens ()
  ((glyph-cache :initform nil :accessor lens-glyph-cache)
   (batch :initform nil :accessor lens-batch)
   (key :initform nil :accessor lens-key :documentation "What the batch was built for."))
  (:documentation "One view's age lens and provenance plate (#OVEP9A)."))

(defmethod render:release-viewer-overlay ((lens history-lens))
  (when (lens-batch lens)
    (render::release-world-text-batch (lens-batch lens))
    (setf (lens-batch lens) nil))
  (when (lens-glyph-cache lens)
    (luv.slug:release-slug-glyph-cache (lens-glyph-cache lens))
    (setf (lens-glyph-cache lens) nil)))

(defun viewer-history-lens (viewer)
  (find-if (lambda (overlay) (typep overlay 'history-lens)) (render:viewer-overlays viewer)))

(defun toggle-history-lens (viewer)
  "Show or hide VIEWER's history lens; return whether it is shown now."
  (let ((lens (viewer-history-lens viewer)))
    (if lens
        (progn (render:remove-viewer-overlay viewer lens) nil)
        (progn (render:add-viewer-overlay viewer (make-instance 'history-lens)) t))))

(clim:define-command (com-history-lens :command-table luvcraft-world
                                       :name "History Lens" :keystroke (:l))
    ()
  (let ((viewer (render:viewer-command-viewer)))
    (setf (render:viewer-last-edit-status viewer)
          (if (toggle-history-lens viewer) :history-lens :history-lens-off))))

(defun lensed-cells (world eye)
  "Authored, present cells within *AGE-LENS-RADIUS* of EYE, nearest first,
each (CELL . AGE-OR-NIL)."
  (let* ((journal (terrain:world-source-journal (world-source world)))
         (edits (terrain:world-journal-edits journal))
         (now (get-universal-time))
         (r *age-lens-radius*)
         (ex (vec3:vec3-x eye)) (ey (vec3:vec3-y eye)) (ez (vec3:vec3-z eye))
         (found nil))
    (maphash (lambda (site value)
               (when value
                 (let ((dx (- (+ 0.5 (luft:site-x site)) ex))
                       (dy (- (+ 0.5 (luft:site-y site)) ey))
                       (dz (- (+ 0.5 (luft:site-z site)) ez)))
                   (when (and (< (abs dx) r) (< (abs dy) r) (< (abs dz) r))
                     (let* ((provenance (terrain:world-cell-provenance journal site))
                            (time (and provenance (terrain:cell-provenance-time provenance))))
                       (push (list (+ (* dx dx) (* dy dy) (* dz dz)) site (and time (- now time)))
                             found))))))
             edits)
    (let ((sorted (sort found #'< :key #'first)))
      (loop for (nil site age) in sorted
            repeat *age-lens-limit*
            collect (cons site age)))))

(defparameter *cell-faces*
  ;; Neighbour step, then the face's corner offset, right edge, and up edge.
  '(((0 0 1) (0 0 1) (1 0 0) (0 1 0)) ((0 0 -1) (0 0 0) (1 0 0) (0 1 0))
    ((1 0 0) (1 0 0) (0 1 0) (0 0 1)) ((-1 0 0) (0 0 0) (0 1 0) (0 0 1))
    ((0 1 0) (0 1 0) (1 0 0) (0 0 1)) ((0 -1 0) (0 0 0) (1 0 0) (0 0 1))))

(defun age-lens-panels (world cells)
  "Panel records tinting each open face of CELLS, each (CELL . AGE)."
  (let ((scene (world-scene world))
        (panels (make-array 0 :element-type 'single-float :adjustable t :fill-pointer 0)))
    (loop for (site . age) in cells
          for tint = (age-lens-tint age)
          for x = (luft:site-x site) for y = (luft:site-y site) for z = (luft:site-z site)
          do (loop for ((sx sy sz) (ox oy oz) right up) in *cell-faces*
                   for lift = *age-lens-lift*
                   for neighbour = (ignore-errors
                                    (luft:make-site (terrain:scene-domain scene) (+ x sx) (+ y sy)
                                                    (+ z sz) luft:+cell-extent+ 1))
                   when (and neighbour
                             (null (terrain:scene-material-placement-at scene neighbour)))
                     do (render::push-panel-record
                         panels
                         (vec3:make-vec3 (+ x ox (* sx lift)) (+ y oy (* sy lift))
                                         (+ z oz (* sz lift)))
                         (apply #'vec3:make-vec3 right) (apply #'vec3:make-vec3 up) tint)))
    panels))

(defun history-plate-placement (viewer)
  "(TEXT CENTER RIGHT UP EM) of the plate over the block at VIEWER's
crosshair, or NIL when nobody is known to have authored it."
  (let* ((inspection (render::viewer-inspection viewer))
         (cell (and inspection (render:site-inspection-cell inspection)))
         (text (and cell (cell-history-phrase (viewer-world viewer) cell))))
    (when text
      ;; In the open cell before the face looked at, so the block itself and
      ;; its neighbours in the same wall never hide it.
      (let* ((open (render::site-inspection-adjacent-cell inspection))
             (eye (render:camera-position (render:viewer-camera viewer)))
             (cx (+ 0.5 (luft:site-x open))) (cy (+ 0.5 (luft:site-y open)))
             (cz (+ 0.5 (luft:site-z open)))
             (dx (- (vec3:vec3-x eye) cx)) (dy (- (vec3:vec3-y eye) cy))
             (flat (max 1d-6 (sqrt (+ (* dx dx) (* dy dy)))))
             (distance (sqrt (+ (* dx dx) (* dy dy) (expt (- (vec3:vec3-z eye) cz) 2))))
             (em (max 0.12 (* 0.03 distance))))
        (list text
              (vec3:make-vec3 cx cy (+ cz 0.25))
              (vec3:make-vec3 (- (/ dy flat)) (/ dx flat) 0)
              (vec3:make-vec3 0 0 1)
              em)))))

(defun history-lens-key (viewer world)
  (let ((eye (render:camera-position (render:viewer-camera viewer)))
        (plate (history-plate-placement viewer)))
    (list (terrain:world-journal-head (terrain:world-source-journal (world-source world)))
          (floor (vec3:vec3-x eye) 4) (floor (vec3:vec3-y eye) 4) (floor (vec3:vec3-z eye) 4)
          ;; Ages move on; a minute is the finest step a reader sees.
          (floor (get-universal-time) 60)
          (and plate (first plate))
          (and plate (nameplate-key (list (cons "" (rest plate))))))))

(defun history-lens-batch (lens viewer)
  "LENS's world text for VIEWER's next frame, rebuilt when what it shows changed."
  (let* ((world (viewer-world viewer))
         (key (history-lens-key viewer world)))
    (unless (equal key (lens-key lens))
      (let ((old (lens-batch lens))
            (device (render:viewer-device viewer))
            (panels (age-lens-panels
                     world (lensed-cells world (render:camera-position (render:viewer-camera viewer)))))
            (glyphs (make-array 0 :element-type 'single-float :adjustable t :fill-pointer 0))
            (plate (history-plate-placement viewer))
            (atlas nil))
        (setf (lens-key lens) key
              (lens-batch lens) nil)
        (when plate
          (let* ((cache (or (lens-glyph-cache lens)
                            (setf (lens-glyph-cache lens) (luv.slug:make-slug-glyph-cache device))))
                 (all (luv.slug:make-slug-glyph-placements
                       (luv.slug:cached-slug-shaped-text cache *nameplate-font-pathname* (first plate))
                       (luv.slug:slug-font-loader *nameplate-font-pathname*)
                       cache *nameplate-font-pathname*)))
            (setf atlas (and all (luv.slug:slug-glyph-atlas-for cache all)))
            (when atlas
              (let ((*nameplate-ink* *history-plate-ink*))
                (multiple-value-bind (p g) (apply #'nameplate-records cache atlas plate)
                  (when p
                    (loop for x across p do (vector-push-extend x panels))
                    (loop for x across g do (vector-push-extend x glyphs))))))))
        (when (plusp (length panels))
          (setf (lens-batch lens)
                (render::make-world-text-batch
                 device (coerce panels '(simple-array single-float (*)))
                 (coerce glyphs '(simple-array single-float (*))) atlas)))
        (when old (render::release-world-text-batch old)))))
  (lens-batch lens))

(defmethod render::viewer-world-text-batches :around ((viewer luvcraft-viewer))
  (let* ((lens (viewer-history-lens viewer))
         (batch (and lens (history-lens-batch lens viewer))))
    (if batch
        (append (call-next-method) (list batch))
        (call-next-method))))
