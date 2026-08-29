(in-package #:luft.atlas)

;;; Browser presentation for the static star atlas.  ParenScript owns all
;;; executable browser code; the generated page contains no handwritten
;;; JavaScript.

(defun parenscript-array-form (items)
  `(array ,@(map 'list
                 (lambda (item)
                   (if (or (listp item) (vectorp item))
                       (parenscript-array-form item)
                       item))
                 items)))

(defun matrix-product (left right)
  (loop for row in left
        collect
        (loop for column below 3
              collect
              (loop for index below 3
                    sum (* (nth index row)
                           (nth column (nth index right)))))))

(defun quarter-turns ()
  '(((1 0 0) (0 0 -1) (0 1 0))
    ((1 0 0) (0 0 1) (0 -1 0))
    ((0 0 1) (0 1 0) (-1 0 0))
    ((0 0 -1) (0 1 0) (1 0 0))
    ((0 -1 0) (1 0 0) (0 0 1))
    ((0 1 0) (-1 0 0) (0 0 1))))

(defun star-orientation-phase (representative start-transformation target-stars)
  (labels
      ((entry-after-turn (entry turn)
         (let ((transformation (matrix-product turn (rest entry))))
           (cons (luft:transform-star transformation representative)
                 transformation)))
       (candidates (entry seen)
         (remove-if
          (lambda (candidate)
            (or (member (first candidate) seen)
                (not (member (first candidate) target-stars))))
          (mapcar (lambda (turn) (entry-after-turn entry turn))
                  (quarter-turns))))
       (onward-count (entry seen)
         (length
          (remove-duplicates (candidates entry seen)
                             :key #'first)))
       (visit (path seen)
         (if (= (length seen) (length target-stars))
             (reverse path)
             (let ((next (remove-duplicates
                          (candidates (first path) seen)
                          :key #'first)))
               (setf next
                     (stable-sort next #'<
                                  :key (lambda (candidate)
                                         (onward-count candidate seen))))
               (loop for candidate in next
                     for result =
                       (visit (cons candidate path)
                              (cons (first candidate) seen))
                     when result return result)))))
    (let ((start
            (cons (luft:transform-star start-transformation representative)
                  start-transformation)))
      (or (visit (list start) (list (first start)))
          (error "No quarter-turn walk through star family #x~2,'0X"
                 representative)))))

(defparameter *star-orientation-walks* (make-hash-table))

(defun star-orientation-walk (representative)
  "Return (STAR . TRANSFORMATION) entries joined by simple cube-group moves."
  (or (gethash representative *star-orientation-walks*)
      (setf (gethash representative *star-orientation-walks*)
            (let* ((identity '((1 0 0) (0 1 0) (0 0 1)))
                   (mirror '((-1 0 0) (0 1 0) (0 0 1)))
                   (proper-stars (luft:star-orbit representative))
                   (full-stars
                     (luft:star-orbit representative :reflections t))
                   (proper
                     (star-orientation-phase
                      representative identity proper-stars)))
              (if (= (length proper-stars) (length full-stars))
                  proper
                  (let* ((last-transformation (rest (car (last proper))))
                         (reflected-start
                           (matrix-product mirror last-transformation))
                         (reflected-stars
                           (set-difference full-stars proper-stars)))
                    (append proper
                            (star-orientation-phase
                             representative reflected-start
                             reflected-stars))))))))

(defun gray-star-orbit (representative)
  (mapcar #'first (star-orientation-walk representative)))

(defun star-atlas-data-form ()
  "A ParenScript array literal containing display-normalized geometry of 256 stars."
  `(array
    ,@(loop for mask below 256
            collect
            (multiple-value-bind (representative display-transformation)
                (star-display-frame mask)
              `(create
                :mask ,mask
                :representative ,representative
                :orientation
                ,(parenscript-array-form display-transformation)
                :owned
                ,(star-geometry-data-form
                  (transform-star-geometry
                   display-transformation (luft:star-triangles mask)))
                :surface
                ,(star-geometry-data-form
                  (luft:star-local-surface-triangles representative)))))))

(defun star-display-frame (mask)
  "Return MASK's representative and the transform carrying MASK onto it."
  (let* ((representative
           (luft:star-canonical-form mask :reflections t))
         (transformation
           (rest (find mask (star-orientation-walk representative)
                       :key #'first))))
    (values representative (apply #'mapcar #'list transformation))))

(defun transform-star-geometry (transformation geometry)
  (loop for kind in '(:faces :bands :junctions)
        append
        (list kind
              (let ((triangles
                      (luft:transform-star-triangles
                       transformation (getf geometry kind))))
                (if (minusp (transformation-determinant transformation))
                    (mapcar (lambda (triangle)
                              (list (first triangle)
                                    (third triangle)
                                    (second triangle)))
                            triangles)
                    triangles)))))

(defun transformation-determinant (matrix)
  (destructuring-bind ((a b c) (d e f) (g h i)) matrix
    (+ (* a (- (* e i) (* f h)))
       (- (* b (- (* d i) (* f g))))
       (* c (- (* d h) (* e g))))))

(defun triangle-pairs-as-quads (triangles)
  (loop for (first second) on triangles by #'cddr
        when (null second)
          do (error "Unpaired atlas quad triangle")
        collect (triangle-pair-quad first second)))

(defun triangle-pair-quad (first second)
  (let ((boundary '()))
    (dolist (triangle (list first second))
      (loop for start in triangle
            for end in (append (rest triangle) (list (first triangle)))
            for reverse =
              (find-if (lambda (edge)
                         (and (equal (first edge) end)
                              (equal (second edge) start)))
                       boundary)
            do (if reverse
                   (setf boundary (remove reverse boundary :test #'eq))
                   (push (list start end) boundary))))
    (unless (= 4 (length boundary))
      (error "Atlas triangle pair does not form a quad"))
    (let* ((edge (pop boundary))
           (points (list (first edge) (second edge))))
      (loop while (< (length points) 4)
            for next =
              (find (car (last points)) boundary :key #'first :test #'equal)
            do (unless next
                 (error "Atlas quad boundary is not connected"))
               (setf boundary (remove next boundary :test #'eq)
                     points (append points (list (second next)))))
      points)))

(defun star-geometry-data-form (geometry)
  `(create :faces
           ,(parenscript-array-form
             (triangle-pairs-as-quads (getf geometry :faces)))
           :bands
           ,(parenscript-array-form
             (triangle-pairs-as-quads (getf geometry :bands)))
           :junctions ,(parenscript-array-form (getf geometry :junctions))))

(defun star-owned-geometry (star)
  "STAR's owned triangles grouped like STAR-TRIANGLES for illustration."
  (let ((owned (luft:star-atlas-owned-triangles star)))
    (loop for kind in '(:faces :bands :junctions)
          append (list kind
                       (remove-if-not
                        (lambda (triangle) (member triangle owned :test #'equal))
                        (getf (luft:star-triangles star) kind))))))

(defun occupancy-cube-corners (sample)
  (flet ((bounds (axis)
           (let ((low (if (logbitp axis sample) 0 -8)))
             (list low (+ low 8)))))
    (destructuring-bind ((x0 x1) (y0 y1) (z0 z1))
        (loop for axis below 3 collect (bounds axis))
      (list (list x0 y0 z0) (list x1 y0 z0)
            (list x1 y1 z0) (list x0 y1 z0)
            (list x0 y0 z1) (list x1 y0 z1)
            (list x1 y1 z1) (list x0 y1 z1)))))

(defun occupancy-boundary-quads (star)
  "Visible cubical boundary quads of STAR's occupied incident cells."
  (loop with face-axes = '(2 2 1 1 0 0)
        for sample below 8
        when (logbitp sample star)
          append
          (let* ((corners (occupancy-cube-corners sample))
                 (faces (list (mapcar (lambda (i) (nth i corners)) '(0 3 2 1))
                              (mapcar (lambda (i) (nth i corners)) '(4 5 6 7))
                              (mapcar (lambda (i) (nth i corners)) '(0 1 5 4))
                              (mapcar (lambda (i) (nth i corners)) '(3 7 6 2))
                              (mapcar (lambda (i) (nth i corners)) '(0 4 7 3))
                              (mapcar (lambda (i) (nth i corners)) '(1 2 6 5)))))
            (loop for face in faces
                  for face-number from 0
                  for axis in face-axes
                  for negative-face-p = (evenp face-number)
                  for neighbor = (logxor sample (ash 1 axis))
                  unless (and (eql negative-face-p (logbitp axis sample))
                              (logbitp neighbor star))
                    collect face))))

(defun write-typst-polygon-sequence (polygons stream &optional (indent 6))
  (format stream "(~%")
  (dolist (polygon polygons)
    (format stream "~V@T(" indent)
    (loop for point in polygon
          for first = t then nil
          do (unless first (write-string ", " stream))
             (format stream "(~D, ~D, ~D)" (first point) (second point)
                     (third point)))
    (format stream "),~%"))
  (format stream "~V@T)" (- indent 2)))

(defun write-typst-star-geometry (geometry stream)
  (format stream "(~%    faces: ")
  (write-typst-polygon-sequence
   (triangle-pairs-as-quads (getf geometry :faces)) stream 8)
  (format stream ",~%    bands: ")
  (write-typst-polygon-sequence
   (triangle-pairs-as-quads (getf geometry :bands)) stream 8)
  (format stream ",~%    junctions: ")
  (write-typst-polygon-sequence (getf geometry :junctions) stream 8)
  (format stream ",~%  )"))

(defun typst-star-data-source (star)
  "Typst data for one exact production STAR illustration."
  (with-output-to-string (stream)
    (format stream "// Generated from LUFT's production star atlas; do not edit.~%")
    (format stream "#let star-x~A = (~%  mask: ~D,~%  bits: ~S,~%  occupied: ("
            (string-downcase (format nil "~2,'0X" star))
            star (format nil "~8,'0B" star))
    (loop for sample below 8
          when (logbitp sample star)
            do (format stream "~D, " sample))
    (format stream "),~%  occupancy: ")
    (write-typst-polygon-sequence (occupancy-boundary-quads star) stream 6)
    (format stream ",~%  whole: ")
    (write-typst-star-geometry (luft:star-triangles star) stream)
    (format stream ",~%  owned: ")
    (write-typst-star-geometry (star-owned-geometry star) stream)
    (format stream ",~%)~%")))

(defun star-atlas-javascript ()
  (ps:ps*
   `(progn
    (defvar stars ,(star-atlas-data-form))
    (defvar families ,(parenscript-array-form (star-symmetry-classes)))
    (defvar selected-mask 8)
    (defvar yaw -0.72)
    (defvar pitch 0.48)
    (defvar zoom 1.0)
    (defvar dragging false)
    (defvar previous-x 0)
    (defvar previous-y 0)
    (defvar show-faces true)
    (defvar show-bands true)
    (defvar show-junctions true)
    (defvar view-mode "surface")

    (defun element (id)
      ((@ document get-element-by-id) id))

    (defun clamp (value low high)
      (max low (min high value)))

    (defun occupied-p (mask sample)
      (/= 0 (logand mask (ash 1 sample))))

    (defun average-depth (points)
      (let ((total 0))
        (dolist (point points)
          (incf total (aref point 2)))
        (/ total (@ points length))))

    (defun polygon-normal (points)
      (let* ((a (aref points 0))
             (b (aref points 1))
             (c (aref points 2))
             (ab (array (- (aref b 0) (aref a 0))
                        (- (aref b 1) (aref a 1))
                        (- (aref b 2) (aref a 2))))
             (ac (array (- (aref c 0) (aref a 0))
                        (- (aref c 1) (aref a 1))
                        (- (aref c 2) (aref a 2))))
             (normal
               (array (- (* (aref ab 1) (aref ac 2))
                         (* (aref ab 2) (aref ac 1)))
                      (- (* (aref ab 2) (aref ac 0))
                         (* (aref ab 0) (aref ac 2)))
                      (- (* (aref ab 0) (aref ac 1))
                         (* (aref ab 1) (aref ac 0)))))
             (length
               (sqrt (+ (* (aref normal 0) (aref normal 0))
                        (* (aref normal 1) (aref normal 1))
                        (* (aref normal 2) (aref normal 2))))))
        (array (/ (aref normal 0) length)
               (/ (aref normal 1) length)
               (/ (aref normal 2) length))))

    (defun signed-area (points)
      (let ((area 0))
        (dotimes (index (@ points length))
          (let ((point (aref points index))
                (next (aref points (mod (1+ index) (@ points length)))))
            (incf area (- (* (aref point 0) (aref next 1))
                          (* (aref next 0) (aref point 1))))))
        (/ area 2)))

    (defun project-point (point width height scale)
      (let* ((x (- (aref point 0) 0.5))
             (y (- (aref point 1) 0.5))
             (z (- (aref point 2) 0.5))
             (cos-yaw (cos yaw))
             (sin-yaw (sin yaw))
             (cos-pitch (cos pitch))
             (sin-pitch (sin pitch))
             (across (- (* cos-yaw x) (* sin-yaw y)))
             (away (+ (* sin-yaw x) (* cos-yaw y)))
             (up (- (* cos-pitch z) (* sin-pitch away)))
             (depth (+ (* sin-pitch z) (* cos-pitch away))))
        (array (+ (/ width 2) (* scale across))
               (+ (/ height 2) (* -1 scale up))
               depth)))

    (defun cube-corners (sample)
      (let* ((x0 (if (occupied-p sample 0) 0 -8))
             (x1 (+ x0 8))
             (y0 (if (occupied-p sample 1) 0 -8))
             (y1 (+ y0 8))
             (z0 (if (occupied-p sample 2) 0 -8))
             (z1 (+ z0 8)))
        (array (array x0 y0 z0) (array x1 y0 z0)
               (array x1 y1 z0) (array x0 y1 z0)
               (array x0 y0 z1) (array x1 y0 z1)
               (array x1 y1 z1) (array x0 y1 z1))))

    (defun cube-faces (sample)
      (let ((corners (cube-corners sample)))
        (array
         (array (aref corners 0) (aref corners 3)
                (aref corners 2) (aref corners 1))
         (array (aref corners 4) (aref corners 5)
                (aref corners 6) (aref corners 7))
         (array (aref corners 0) (aref corners 1)
                (aref corners 5) (aref corners 4))
         (array (aref corners 3) (aref corners 7)
                (aref corners 6) (aref corners 2))
         (array (aref corners 0) (aref corners 4)
                (aref corners 7) (aref corners 3))
         (array (aref corners 1) (aref corners 2)
                (aref corners 6) (aref corners 5)))))

    (defun projected-polygon (points width height scale color alpha stroke)
      (let ((projected (array))
            (normal (polygon-normal points)))
        (dolist (point points)
          ((@ projected push) (project-point point width height scale)))
        (create :points projected
                :depth (average-depth projected)
                :color color
                :alpha alpha
                :stroke stroke
                :backface (< (signed-area projected) 0)
                :light
                (+ 0.72
                   (* 0.28
                      (max 0 (+ (* -0.35 (aref normal 0))
                                (* -0.45 (aref normal 1))
                                (* 0.82 (aref normal 2)))))))))

    (defun internal-occupancy-face-p (mask sample face)
      (let ((axis (aref (array 2 2 1 1 0 0) face))
            (negative-face-p (= 0 (mod face 2))))
        (and (= negative-face-p (occupied-p sample axis))
             (occupied-p mask (logxor sample (ash 1 axis))))))

    (defun occupancy-polygons (star width height scale)
      (let ((polygons (array)))
        (dotimes (sample 8)
          (when (occupied-p (@ star representative) sample)
            (let ((faces (cube-faces sample)))
              (dotimes (face 6)
                (unless (internal-occupancy-face-p
                         (@ star representative) sample face)
                  ((@ polygons push)
                   (projected-polygon (aref faces face) width height scale
                                      "#b85d70" 0.34 "#8f4052")))))))
        polygons))

    (defun geometry-polygons (geometry width height scale color alpha stroke)
      (let ((polygons (array)))
        (dolist (polygon geometry)
          ((@ polygons push)
           (projected-polygon polygon width height scale
                              color alpha stroke)))
        polygons))

    (defun add-geometry-polygons (mesh geometry width height scale alpha stroke)
      (when show-faces
        (dolist (polygon
                  (geometry-polygons (@ geometry faces) width height scale
                                     "#438fb7" alpha stroke))
          ((@ mesh push) polygon)))
      (when show-bands
        (dolist (polygon
                  (geometry-polygons (@ geometry bands) width height scale
                                     "#d9a43b" alpha stroke))
          ((@ mesh push) polygon)))
      (when show-junctions
        (dolist (polygon
                  (geometry-polygons (@ geometry junctions) width height scale
                                     "#cf6558" alpha stroke))
          ((@ mesh push) polygon))))

    (defun shade-color (color factor)
      (let ((red (parse-int ((@ color slice) 1 3) 16))
            (green (parse-int ((@ color slice) 3 5) 16))
            (blue (parse-int ((@ color slice) 5 7) 16)))
        (+ "rgb(" (round (* red factor)) ","
           (round (* green factor)) ","
           (round (* blue factor)) ")")))

    (defun draw-polygon (context polygon)
      (unless (@ polygon backface)
        (let ((points (@ polygon points))
              (shade (@ polygon light)))
          ((@ context begin-path))
          ((@ context move-to) (aref (aref points 0) 0)
                               (aref (aref points 0) 1))
          (loop for index from 1 below (@ points length) do
            ((@ context line-to) (aref (aref points index) 0)
                                 (aref (aref points index) 1)))
          ((@ context close-path))
          (setf (@ context global-alpha) (@ polygon alpha)
                (@ context fill-style)
                (shade-color (@ polygon color) shade))
          ((@ context fill))
          (setf (@ context global-alpha)
                (if (= (@ polygon alpha) 0)
                    0.3
                    (min 1 (+ (@ polygon alpha) 0.38)))
                (@ context stroke-style)
                (shade-color (@ polygon stroke) shade)
                (@ context line-width) 0.85)
          ((@ context stroke)))))

    (defun triangle-depth-at (point a b c)
      (let* ((px (aref point 0))
             (py (aref point 1))
             (denominator
               (+ (* (- (aref b 1) (aref c 1))
                     (- (aref a 0) (aref c 0)))
                  (* (- (aref c 0) (aref b 0))
                     (- (aref a 1) (aref c 1))))))
        (if (< (abs denominator) 0.000001)
            null
            (let* ((px-from-c (- px (aref c 0)))
                   (py-from-c (- py (aref c 1)))
                   (wa
                     (/
                      (+
                       (* (- (aref b 1) (aref c 1)) px-from-c)
                       (* (- (aref c 0) (aref b 0)) py-from-c))
                      denominator))
                   (wb
                     (/
                      (+
                       (* (- (aref c 1) (aref a 1)) px-from-c)
                       (* (- (aref a 0) (aref c 0)) py-from-c))
                      denominator))
                   (wc (- 1 wa wb)))
              (if (and (>= wa -0.0001) (>= wb -0.0001) (>= wc -0.0001))
                  (+ (* wa (aref a 2))
                     (* wb (aref b 2))
                     (* wc (aref c 2)))
                  null)))))

    (defun polygon-depth-at (point polygon)
      (let ((points (@ polygon points))
            (depth null))
        (loop for index from 1 below (1- (@ points length)) do
          (let ((candidate
                  (triangle-depth-at point
                                     (aref points 0)
                                     (aref points index)
                                     (aref points (1+ index)))))
            (when (/= candidate null)
              (setf depth candidate))))
        depth))

    (defun point-occluded-p (point polygons)
      (let ((occluded false))
        (dolist (polygon polygons)
          (when (and (not (@ polygon backface))
                     (> (@ polygon alpha) 0))
            (let ((depth (polygon-depth-at point polygon)))
              (when (and (/= depth null)
                         (> depth (+ (aref point 2) 0.015)))
                (setf occluded true)))))
        occluded))

    (defun transform-direction (transformation direction)
      (let ((result (array)))
        (dolist (row transformation)
          ((@ result push)
           (+ (* (aref row 0) (aref direction 0))
              (* (aref row 1) (aref direction 1))
              (* (aref row 2) (aref direction 2)))))
        result))

    (defun draw-orientation-axes (context width height transformation)
      (let ((axes
              (array
               (array (array 1 0 0) "X" "#c24f4a")
               (array (array 0 1 0) "Y" "#4c8b57")
               (array (array 0 0 1) "Z" "#477fba")))
            (anchor-x 38)
            (anchor-y (- height 38))
            (projected-origin (project-point (array 0 0 0) width height 22)))
        (dolist (axis axes)
          (let* ((direction
                   (transform-direction transformation (aref axis 0)))
                 (projected
                   (project-point direction width height 22))
                 (end-x (+ anchor-x (- (aref projected 0)
                                       (aref projected-origin 0))))
                 (end-y (+ anchor-y (- (aref projected 1)
                                       (aref projected-origin 1))))
                 (color (aref axis 2)))
            ((@ context begin-path))
            ((@ context move-to) anchor-x anchor-y)
            ((@ context line-to) end-x end-y)
            (setf (@ context global-alpha) 0.9
                  (@ context stroke-style) color
                  (@ context line-width) 2)
            ((@ context stroke))
            (setf (@ context fill-style) color
                  (@ context font) "700 11px ui-monospace, monospace")
            ((@ context fill-text) (aref axis 1) (+ end-x 3) (+ end-y 3))))
        ((@ context begin-path))
        ((@ context arc) anchor-x anchor-y 2.5 0 (* 2 pi))
        (setf (@ context global-alpha) 1
              (@ context fill-style) "#17201e")
        ((@ context fill))))

    (defun canvas-context (canvas)
      (let* ((ratio (min 2 (or (@ window device-pixel-ratio) 1)))
             (width (@ canvas client-width))
             (height (@ canvas client-height)))
        (setf (@ canvas width) (round (* width ratio))
              (@ canvas height) (round (* height ratio)))
        (let ((context ((@ canvas get-context) "2d")))
          ((@ context set-transform) ratio 0 0 ratio 0 0)
          (array context width height))))

    (defun draw-star (canvas star geometry detailed-p)
      (let* ((prepared (canvas-context canvas))
             (context (aref prepared 0))
             (width (aref prepared 1))
             (height (aref prepared 2))
             (scale (* (min (/ width 25) (/ height 25))
                       (if detailed-p zoom 1)))
             (occupancy (occupancy-polygons star width height scale))
             (mesh (array)))
        ((@ context clear-rect) 0 0 width height)
        ((@ occupancy sort)
         (lambda (left right) (- (@ left depth) (@ right depth))))
        (dolist (polygon occupancy)
          (draw-polygon context polygon))
        (when (and detailed-p (= view-mode "owned"))
          (add-geometry-polygons mesh (@ star surface) width height scale
                                 0 "#6f7976"))
        (add-geometry-polygons mesh geometry width height scale
                               1 "#35413f")
        ((@ mesh sort)
         (lambda (left right) (- (@ left depth) (@ right depth))))
        (dolist (polygon mesh)
          (draw-polygon context polygon))
        (let ((origin (project-point (array 0 0 0) width height scale)))
          (unless (or (point-occluded-p origin occupancy)
                      (point-occluded-p origin mesh))
            ((@ context begin-path))
            ((@ context arc) (aref origin 0) (aref origin 1)
                             (if detailed-p 3.2 1.8) 0 (* 2 pi))
            (setf (@ context global-alpha) 1
                  (@ context fill-style) "#17201e")
            ((@ context fill))))
        (when detailed-p
          (draw-orientation-axes context width height (@ star orientation)))))

    (defun star-at (mask)
      (aref stars mask))

    (defun render-thumbnails ()
      (let ((cards ((@ document query-selector-all) ".star-card")))
        ((@ cards for-each)
         (lambda (card)
           (let ((mask (parse-int (@ card dataset mask) 10)))
             (draw-star ((@ card query-selector) "canvas")
                        (star-at mask) (@ (star-at mask) surface) false))))))

    (defun set-text (id value)
      (setf (@ (element id) text-content) value))

    (defun bit-label (mask)
      (let ((text ((@ mask to-string) 2)))
        ((@ text pad-start) 8 "0")))

    (defun hexadecimal-label (mask)
      (+ "#x" (chain mask (to-string 16) (to-upper-case)
                          (pad-start 2 "0"))))

    (defun render-occupancy (mask)
      (let ((cells ((@ document query-selector-all) ".occupancy-cell")))
        ((@ cells for-each)
         (lambda (cell)
           (let ((sample (parse-int (@ cell dataset sample) 10)))
             ((@ cell class-list toggle) "occupied"
                                          (occupied-p mask sample)))))))

    (defun render-selected ()
      (let* ((star (star-at selected-mask))
             (display-mask (@ star representative))
             (geometry (if (= view-mode "owned")
                           (@ star owned)
                           (@ star surface))))
        (set-text "selected-mask" (hexadecimal-label display-mask))
        (set-text "selected-bits" (bit-label display-mask))
        (set-text "selected-view"
                  (if (= view-mode "owned")
                      "Owned orientation"
                      "Whole local patch"))
        (set-text "view-explanation"
                  (if (= view-mode "owned")
                      "Opaque quads belong to this orientation in the fixed family frame; XYZ shows how its axes map into that frame. Rose volumes are solid; unshaded cells are air."
                      "Every face and band touching the center, with ownership forgotten. Rose volumes are solid; unshaded cells are air."))
        (set-text "face-count" (@ (@ geometry faces) length))
        (set-text "band-count" (@ (@ geometry bands) length))
        (set-text "junction-count" (@ (@ geometry junctions) length))
        (render-occupancy display-mask)
        (render-selected-family)
        (draw-star (element "selected-canvas") star geometry true)))

    (defun render-selected-family ()
      (let ((choices ((@ document query-selector-all) ".star-choice"))
            (selected-class -1))
        ((@ choices for-each)
         (lambda (choice)
           (let ((choice-mask (parse-int (@ choice dataset mask) 10))
                 (choice-view (@ choice dataset view)))
             ((@ choice class-list toggle)
              "selected" (and (= choice-mask selected-mask)
                              (= choice-view view-mode)))
             (when (= choice-mask selected-mask)
               (setf selected-class
                     (parse-int (@ choice dataset class) 10))))))
        (let ((families ((@ document query-selector-all) ".star-family")))
          ((@ families for-each)
           (lambda (family)
             (let ((selected-p
                     (= (parse-int (@ family dataset representative) 10)
                        selected-class)))
               ((@ family class-list toggle) "selected-family" selected-p)
               (setf (@ ((@ family query-selector) ".family-orbit") open)
                     selected-p)))))))

    (defun select-star (mask mode)
      (setf selected-mask (mod (+ mask 256) 256)
            view-mode mode
            (@ window location hash)
            (+ (hexadecimal-label selected-mask)
               (if (= view-mode "owned") "-owned" "")))
      (setf (@ (element (if (= view-mode "owned")
                            "view-owned"
                            "view-surface")) checked) true)
      (render-selected))

    (defun selected-family ()
      (let ((selected nil))
        (dolist (family families)
          (when (/= -1 ((@ family index-of) selected-mask))
            (setf selected family)))
        selected))

    (defun step-orientation (offset)
      (let* ((family (selected-family))
             (index ((@ family index-of) selected-mask))
             (count (@ family length)))
        (select-star (aref family (mod (+ index offset count) count))
                     view-mode)))

    (defun mask-from-hash ()
      (let* ((text ((@ (@ window location hash) replace) "#" ""))
             (plain ((@ text replace) "x" ""))
             (mask (parse-int plain 16)))
        (if (and (not (is-na-n mask)) (<= 0 mask) (< mask 256))
            mask
            8)))

    (defun mode-from-hash ()
      (if (/= -1 ((@ (@ window location hash) index-of) "owned"))
          "owned"
          "surface"))

    (defun install-card-events ()
      (let ((choices ((@ document query-selector-all) ".star-choice")))
        ((@ choices for-each)
         (lambda (choice)
           ((@ choice add-event-listener)
            "click"
            (lambda ()
              (select-star (parse-int (@ choice dataset mask) 10)
                           (@ choice dataset view))))))))

    (defun install-view-mode-events ()
      (let ((controls ((@ document query-selector-all) "input[name=mesh-view]")))
        ((@ controls for-each)
         (lambda (control)
           ((@ control add-event-listener)
            "change"
            (lambda ()
              (when (@ control checked)
                (setf view-mode (@ control value)
                      (@ window location hash)
                      (+ (hexadecimal-label selected-mask)
                         (if (= view-mode "owned") "-owned" "")))
                (render-selected))))))))

    (defun install-layer-toggle (id variable-name)
      (let ((control (element id)))
        ((@ control add-event-listener)
         "change"
         (lambda ()
           (cond ((= variable-name "faces")
                  (setf show-faces (@ control checked)))
                 ((= variable-name "bands")
                  (setf show-bands (@ control checked)))
                 (t
                  (setf show-junctions (@ control checked))))
           (render-thumbnails)
           (render-selected)))))

    (defun install-detail-events ()
      (let ((canvas (element "selected-canvas")))
        ((@ canvas add-event-listener)
         "pointerdown"
         (lambda (event)
           (setf dragging true
                 previous-x (@ event client-x)
                 previous-y (@ event client-y))
           ((@ canvas set-pointer-capture) (@ event pointer-id))))
        ((@ canvas add-event-listener)
         "pointermove"
         (lambda (event)
           (when dragging
             (incf yaw (* 0.012 (- (@ event client-x) previous-x)))
             (setf pitch
                   (clamp (+ pitch
                             (* 0.012 (- (@ event client-y) previous-y)))
                          -1.15 1.15)
                   previous-x (@ event client-x)
                   previous-y (@ event client-y))
             (render-selected))))
        ((@ canvas add-event-listener)
         "pointerup" (lambda () (setf dragging false)))
        ((@ canvas add-event-listener)
         "pointercancel" (lambda () (setf dragging false)))
        ((@ canvas add-event-listener)
         "wheel"
         (lambda (event)
           ((@ event prevent-default))
           (setf zoom (clamp (* zoom (if (> (@ event delta-y) 0) 0.92 1.08))
                             0.65 1.8))
           (render-selected))
         (create :passive false))))

    (setf selected-mask (mask-from-hash)
          view-mode (mode-from-hash)
          (@ (element (if (= view-mode "owned")
                          "view-owned"
                          "view-surface")) checked) true)
    (install-card-events)
    (install-layer-toggle "show-faces" "faces")
    (install-layer-toggle "show-bands" "bands")
    (install-layer-toggle "show-junctions" "junctions")
    (install-view-mode-events)
    ((@ (element "previous-star") add-event-listener)
     "click" (lambda () (step-orientation -1)))
    ((@ (element "next-star") add-event-listener)
     "click" (lambda () (step-orientation 1)))
    (install-detail-events)
    ((@ window add-event-listener)
     "resize"
     (lambda ()
       (render-thumbnails)
       (render-selected)))
    (render-thumbnails)
    (render-selected))))
