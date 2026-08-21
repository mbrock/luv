;;; Luvcraft's new authored world, directly in LUFT.
;;;
;;; This is deliberately not a materialization of BLOCK-WORLD.  Its cells are
;;; LUFT fixnum sites, its solid is a chain, its drawable boundary is a LUFT
;;; scene, and LUFT stocks are its material authority.  The old world keeps
;;; running beside it only while player interaction and the remaining systems
;;; cross over in playable slices.

(in-package #:luvcraft)

(defparameter +native-luvcraft-initial-materials+
  #(:turf :granite :sand :limestone :oak :leaf :conifer
    :snow :crystal :terminal :terracotta :bronze :slate :brick)
  "The first native world's physical scene palette.

Fourteen stocks exercise a useful material vocabulary while fitting LUFT's
current four-bit per-face stock slot.  This is a scene encoding cohort, not a
limit on the materials Luvcraft may define or use across worlds.")

(defclass native-luvcraft-world ()
  ((world
    :initarg :world
    :reader native-luvcraft-world-world
    :documentation "The authoritative LUFT solid, slots, and stock palette.")
   (scene
    :initarg :scene
    :reader native-luvcraft-world-scene
    :documentation "The incrementally maintained drawable boundary."))
  (:documentation
   "A Luvcraft world authored directly as LUFT sites, chains, and stocks."))

(defun native-luvcraft-world-domain (native-world)
  "The packed-site domain of NATIVE-WORLD."
  (luft.render:world-domain
   (native-luvcraft-world-world native-world)))

(defun native-luvcraft-world-materials (native-world)
  "The physical stock palette of NATIVE-WORLD's current scene."
  (luft.render:scene-stocks
   (native-luvcraft-world-scene native-world)))

(defun native-luvcraft-cell-site
    (native-world x y z &optional (polarity 1))
  "Return the canonical LUFT fixnum cell site X,Y,Z in NATIVE-WORLD.

These are LUFT coordinates directly: X east, Y north, Z up.  There is no
Luvcraft coordinate object and no Y-up translation at this boundary."
  (luft:make-site (native-luvcraft-world-domain native-world)
                  x y z luft:+cell-extent+ polarity))

(defun check-native-luvcraft-cell-site (native-world site)
  "Return SITE's positive geometry after validating it for NATIVE-WORLD."
  (let ((domain (native-luvcraft-world-domain native-world)))
    (unless (and (luft:site-valid-p domain site)
                 (= luft:+cell-extent+ (luft:site-extent site)))
      (error "~S is not a canonical cell site in this native world." site))
    (luft:site-geometry site)))

(defun native-luvcraft-cell-p (native-world site)
  "Whether the LUFT cell SITE is solid in NATIVE-WORLD."
  (let ((cell (check-native-luvcraft-cell-site native-world site)))
    (luft.render:scene-cell-p
     (native-luvcraft-world-scene native-world)
     (luft:site-x cell) (luft:site-y cell) (luft:site-z cell))))

(defun set-native-luvcraft-cell
    (native-world site solid-p &key (stock :turf))
  "Fill or clear LUFT cell SITE and publish its exact boundary delta.

Filling also paints STOCK.  The current scene palette is a bounded physical
cohort; a stock not preregistered in it fails loudly rather than reviving a
block-kind or texture-atlas fallback."
  (let* ((cell (check-native-luvcraft-cell-site native-world site))
         (scene (native-luvcraft-world-scene native-world))
         (edit (luft:make-chain (native-luvcraft-world-domain native-world)))
         (present-p (native-luvcraft-cell-p native-world cell))
         (solid-p (not (null solid-p))))
    (unless (eql present-p solid-p)
      (luft:add-chain-site
       edit (luft:site-with-polarity cell (if solid-p 1 -1))))
    (if solid-p
        (let ((slot (position stock (luft.render:scene-stocks scene))))
          (unless slot
            (error "Stock ~S is not in this native world's physical palette."
                   stock))
          (luft.render:apply-scene-edit
           scene edit
           :stock-cells
           (make-array 1 :element-type '(unsigned-byte 64)
                         :initial-element cell)
           :stock-slots
           (make-array 1 :element-type '(unsigned-byte 8)
                         :initial-element slot)))
        (luft.render:apply-scene-edit scene edit))
    solid-p))

(defun native-luvcraft-ground-height (x y)
  "A compact first landscape whose foreground begins at the default camera."
  (+ 4.0
     (* 1.6 (sin (/ x 8.0)) (cos (/ y 10.0)))
     (* 0.9 (sin (/ (+ x (* 0.7 y)) 5.5)))
     (if (> y 36) (* 0.20 (- y 36)) 0.0)))

(defun make-native-luvcraft-spawn-camera ()
  "Make the temporary Luvcraft camera at the native LUFT world's first view.

The returned camera still has Luvcraft's Y-up representation because the
player controller has not crossed over yet.  Its position maps exactly to
LUFT (8,4,11), looking north toward the shrine.  Keeping this compatibility
shim here, beside the world it views, prevents the legacy save from choosing
a meaningless pose in the native world; the shim disappears with the old
player controller."
  (make-instance 'fly-camera
                 :position (make-vec3 8.0 11.0 4.0)
                 :yaw 0.0
                 :pitch -0.18))

(defun paint-native-luvcraft-top (world x y stock)
  "Paint the exposed top cell at X,Y in WORLD when the column is nonempty."
  (let ((top (luft.render:column-top world x y)))
    (when top
      (luft.render:paint-cell world x y top stock))))

(defun furnish-native-luvcraft-world (world)
  "Author the first visible native landscape into WORLD and return it."
  (luft.render:lay-ground world :height #'native-luvcraft-ground-height
                                :stock :granite)
  (luft.render:grass-the-flats world :stock :turf :flat 1 :depth 1)
  ;; A sandy trail begins immediately in front of the default view and winds
  ;; toward a small terminal shrine.
  (loop for y from 0 to 25
        for centre = (+ 8 (round (* 2.0 (sin (/ y 5.0)))))
        do (loop for x from (- centre 2) to (+ centre 2)
                 do (paint-native-luvcraft-top world x y :sand)))
  ;; Snow catches on the distant rising ground, beyond the first little wood.
  (loop for x from 0 below 64
        do (loop for y from 42 below 57
                 do (paint-native-luvcraft-top world x y :snow)))
  (luft.render:plant-tree world 2 15 :kind :round :height 8
                                       :trunk :oak :crown :leaf)
  (luft.render:plant-tree world 17 20 :kind :fir :height 10
                                         :trunk :oak :crown :conifer)
  (luft.render:plant-tree world 25 29 :kind :round :height 8
                                         :trunk :oak :crown :leaf)
  ;; A dark terminal chassis and bronze cap make one readable destination.
  (let ((base (or (luft.render:column-top world 8 18) 4)))
    (luft.render:with-stock (:terminal)
      (luft.render:fill-box world 6 10 17 20 (1+ base) (+ base 5)))
    (luft.render:with-stock (:bronze)
      (luft.render:fill-box world 5 11 16 21 (+ base 6) (+ base 6)))
    (luft.render:with-stock (:slate)
      (loop for step from 0 below 4
            do (luft.render:fill-box world (- 7 step) (+ 9 step)
                                          (- 16 step) (- 16 step)
                                          0 (+ base (- 3 step))))))
  ;; Crystal columns and a low terracotta wall show emissive and matte stocks
  ;; in the same first frame without turning the world into a material chart.
  (dolist (column '((14 12 3) (16 14 5) (18 11 2)))
    (destructuring-bind (x y height) column
      (let ((base (or (luft.render:column-top world x y) 4)))
        (luft.render:with-stock (:crystal)
          (luft.render:fill-box world x x y y
                                      (1+ base) (+ base height))))))
  (let ((base (or (luft.render:column-top world 23 14) 4)))
    (luft.render:with-stock (:terracotta)
      (luft.render:fill-box world 21 31 14 14 (1+ base) (+ base 3)))
    (luft.render:with-stock (:brick)
      (loop for x from 21 to 31 by 2
            do (luft.render:fill-box world x x 14 14
                                          (+ base 4) (+ base 4)))))
  world)

(defun make-native-luvcraft-world (&key (horizontal-bits 8))
  "Make Luvcraft's first directly authored LUFT world and drawable scene."
  (let ((world (luft.render:make-world :horizontal-bits horizontal-bits)))
    ;; Register the complete physical cohort before WORLD-SCENE copies it.
    (loop for stock across +native-luvcraft-initial-materials+
          do (luft.render:world-stock-slot world stock))
    (furnish-native-luvcraft-world world)
    (make-instance 'native-luvcraft-world
                   :world world
                   :scene (luft.render:world-scene world))))
