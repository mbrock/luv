;;; The studio: small worlds built to show every kind of corner, cameras
;;; that look at them closely, and contact sheets that put the views of
;;; several styles side by side.
;;;
;;; A shaping rule is judged at its sites.  The demonstration world has
;;; thousands of them and shows none well, so this file builds a world whose
;;; every object is one star configuration -- a lone cube, a bar, an L, a
;;; slab, a block standing on the floor, a pit, stairs, a pillar, a ledge,
;;; two cubes meeting along an edge, two meeting at a vertex -- and renders
;;; each style from the same cameras at several times the output size, box
;;; filtered down, into one PNG.  Rows are cameras, columns are styles.

(in-package #:luft.render)

;;; ------------------------------------------------------------------------
;;; A world of stars

(defun make-studio-scene (&key (horizontal-bits 5) (floor-height 2))
  "A floor carrying one exhibit of each crease and corner kind.

The exhibits stand far enough apart that no two share a star."
  (let* ((domain (luft:make-world-domain :horizontal-bits horizontal-bits))
         (period (luft:world-domain-x-period domain))
         (solid (luft:make-solid-chain domain))
         (z floor-height))
    (dotimes (x period)
      (dotimes (y period)
        (dotimes (k floor-height)
          (setf (luft:solid-cell-p solid x y k) t))))
    ;; Row one: convex things resting on the floor.
    (fill-box solid 3 3 3 3 z z)                  ; a cube on the floor
    (fill-box solid 7 8 3 3 z z)                  ; a bar
    (fill-box solid 12 13 3 3 z z)                ; an L
    (fill-box solid 12 12 4 4 z z)
    (fill-box solid 17 18 3 4 z z)                ; a 2x2 slab
    (fill-box solid 22 23 3 4 z (1+ z))           ; a 2x2x2 block
    (fill-box solid 27 27 3 3 z (+ z 3))          ; a pillar
    ;; Row two: things in the air, whose every corner is pure.
    (fill-box solid 3 3 9 9 (+ z 2) (+ z 2))      ; a floating cube
    (fill-box solid 7 9 9 9 (+ z 2) (+ z 2))      ; a floating bar
    (fill-box solid 12 13 9 10 (+ z 2) (+ z 2))   ; a floating slab
    (fill-box solid 17 17 9 9 (+ z 2) (+ z 2))    ; two cubes meeting at an edge
    (fill-box solid 18 18 10 10 (+ z 2) (+ z 2))
    (fill-box solid 22 22 9 9 (+ z 2) (+ z 2))    ; two cubes meeting at a vertex
    (fill-box solid 23 23 10 10 (+ z 3) (+ z 3))
    (fill-box solid 27 27 9 9 (+ z 2) (+ z 2))    ; a cube under a cube, offset
    (fill-box solid 28 28 9 9 (+ z 3) (+ z 3))
    ;; Row three: concave things cut into or built on the floor.
    (fill-box solid 3 3 15 15 (1- z) (1- z) nil)  ; a one-cell pit
    (fill-box solid 7 8 15 16 (1- z) (1- z) nil)  ; a 2x2 pit
    (fill-box solid 12 12 15 16 (1- z) (1- z) nil) ; a trench
    (fill-box solid 12 12 17 17 (1- z) (1- z) nil)
    (loop for step from 0 below 3                 ; stairs
          do (fill-box solid (+ 17 step) (+ 17 step) 15 17 z (+ z step)))
    (fill-box solid 22 24 15 15 z (+ z 2))        ; a wall with a ledge
    (fill-box solid 22 24 16 16 (+ z 2) (+ z 2))
    (fill-box solid 27 29 15 17 z (+ z 1))        ; a block with an inner corner
    (fill-box solid 27 28 15 16 z (+ z 1) nil)
    ;; Row four: a doorway through a wall, and a roofed nook.
    (fill-box solid 3 8 22 22 z (+ z 2))
    (fill-box solid 5 6 22 22 z (+ z 1) nil)
    (fill-box solid 12 15 22 25 z (+ z 2))
    (fill-box solid 13 14 23 24 z (+ z 1) nil)
    (fill-box solid 13 14 22 22 z (+ z 1) nil)
    (make-scene domain :solid solid)))

;;; ------------------------------------------------------------------------
;;; Cameras

(defun studio-camera (x y z &key (look-x 0.0) (look-y 0.0) (look-z 0.0)
                                 (field-of-view 0.9))
  "A camera at X,Y,Z looking at LOOK-X,LOOK-Y,LOOK-Z."
  (let* ((dx (- look-x x)) (dy (- look-y y)) (dz (- look-z z))
         (flat (sqrt (+ (* dx dx) (* dy dy)))))
    (make-fly-camera :position (vec3:make-vec3 x y z)
                     :yaw (atan dy dx)
                     :pitch (atan dz flat)
                     :field-of-view field-of-view)))

(defun studio-cameras ()
  "Named views of the studio: an overview, then close-ups of the exhibits."
  (list
   (cons :overview (studio-camera 30.5 1.0 14.0 :look-x 14.0 :look-y 14.0
                                  :look-z 2.0 :field-of-view 0.95))
   (cons :cube (studio-camera 6.0 0.5 5.0 :look-x 3.5 :look-y 3.5 :look-z 2.5
                              :field-of-view 0.7))
   (cons :l (studio-camera 15.5 0.5 5.5 :look-x 13.0 :look-y 4.0 :look-z 2.5
                           :field-of-view 0.7))
   (cons :block (studio-camera 25.5 0.0 6.5 :look-x 23.0 :look-y 4.0
                               :look-z 3.0 :field-of-view 0.75))
   (cons :floating (studio-camera 9.0 5.0 7.5 :look-x 12.0 :look-y 10.0
                                  :look-z 4.5 :field-of-view 0.8))
   (cons :pits (studio-camera 5.0 11.0 6.0 :look-x 7.0 :look-y 15.5
                              :look-z 1.5 :field-of-view 0.8))
   (cons :stairs (studio-camera 21.5 12.0 5.5 :look-x 18.5 :look-y 16.0
                                :look-z 3.0 :field-of-view 0.8))
   (cons :nook (studio-camera 9.5 27.0 5.5 :look-x 13.5 :look-y 23.5
                              :look-z 3.0 :field-of-view 0.8))))

;;; ------------------------------------------------------------------------
;;; Contact sheets

(defun blit-pixels (sheet sheet-width tile tile-width tile-height x0 y0)
  "Copy the RGBA TILE into SHEET at X0,Y0."
  (dotimes (y tile-height sheet)
    (replace sheet tile
             :start1 (* 4 (+ x0 (* (+ y0 y) sheet-width)))
             :start2 (* 4 (* y tile-width))
             :end2 (* 4 (* (1+ y) tile-width)))))

(defun render-contact-sheet
    (pathname &key (scene (make-studio-scene))
                   (cameras (studio-cameras))
                   (styles '(:flat :bevel :chamfer :paper))
                   (columns (mapcar #'list styles))
                   (width 320) (height 240) (supersample 3)
                   (technique *default-technique*)
                   (effects '(:sky))
                   (renderer nil))
  "Render every CAMERA in every column and tile them into one PNG at PATHNAME.

Each tile is WIDTH by HEIGHT, rendered SUPERSAMPLE times larger and box
filtered down in linear light.  Rows are cameras; columns are STYLES, or
COLUMNS given explicitly as lists of a style followed by special variables
and values to bind while that column renders, so one sheet can compare one
style under several knobs:

  :columns ((:field) (:field *bevel-radius* 0.12 *field-vertical-radius* 0.45))

With RENDERER, that renderer (whose extent must match) draws instead of a
fresh one; its scene and camera are restored afterwards."
  (let* ((styles (remove-duplicates (mapcar #'first columns)))
         (rows (length cameras))
         (sheet-width (* (length columns) width))
         (sheet-height (* rows height))
         (sheet (make-array (* 4 sheet-width sheet-height)
                            :element-type '(unsigned-byte 8)
                            :initial-element 0))
         (owned-p (null renderer))
         (renderer (or renderer
                       (make-renderer :scene scene
                                      :camera (cdr (first cameras))
                                      :width (* supersample width)
                                      :height (* supersample height)
                                      :technique technique
                                      :style (first styles)
                                      :pipeline-styles styles
                                      :effects effects)))
         (old-scene (renderer-scene renderer))
         (old-camera (renderer-camera renderer))
         (old-style (renderer-style renderer))
         (format nil))
    (unwind-protect
         (progn
           (setf (renderer-scene renderer) scene)
           (loop for (nil . camera) in cameras
                 for row from 0
                 do (setf (renderer-camera renderer) camera)
                    (loop for (style . specials) in columns
                          for column from 0
                          do (setf (renderer-style renderer) style)
                             (multiple-value-bind (pixels big-width big-height
                                                   pixel-format)
                                 (progv (loop for (name nil) on specials by #'cddr
                                              collect name)
                                        (loop for (nil value) on specials by #'cddr
                                              collect value)
                                   (render-pixels renderer))
                               (setf format pixel-format)
                               (multiple-value-bind (tile tile-width tile-height)
                                   (if (> supersample 1)
                                       (downsample-pixels
                                        pixels big-width big-height supersample
                                        :srgb-p (eq pixel-format
                                                    :rgba8-unorm-srgb))
                                       (values pixels big-width big-height))
                                 (blit-pixels sheet sheet-width tile
                                              tile-width tile-height
                                              (* column width) (* row height))))))
           (ensure-directories-exist pathname)
           (write-rgba-png pathname sheet sheet-width sheet-height format)
           pathname)
      (if owned-p
          (destroy-renderer renderer)
          (setf (renderer-scene renderer) old-scene
                (renderer-camera renderer) old-camera
                (renderer-style renderer) old-style)))))
