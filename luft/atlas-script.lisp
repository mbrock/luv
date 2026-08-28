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
  (multiple-value-bind (representative transformation)
      (luft:star-canonical-form mask :reflections t)
    (values representative
            (if (= mask representative)
                '((1 0 0) (0 1 0) (0 0 1))
                (apply #'mapcar #'list transformation)))))

(defun transform-star-geometry (transformation geometry)
  (loop for kind in '(:faces :bands :junctions)
        append
        (list kind
              (luft:transform-star-triangles
               transformation (getf geometry kind)))))

(defun star-geometry-data-form (geometry)
  `(create :faces ,(parenscript-array-form (getf geometry :faces))
           :bands ,(parenscript-array-form (getf geometry :bands))
           :junctions ,(parenscript-array-form (getf geometry :junctions))))

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
      (let ((projected (array)))
        (dolist (point points)
          ((@ projected push) (project-point point width height scale)))
        (create :points projected
                :depth (average-depth projected)
                :color color
                :alpha alpha
                :stroke stroke)))

    (defun occupancy-polygons (star width height scale)
      (let ((polygons (array)))
        (dotimes (sample 8)
          (when (occupied-p (@ star representative) sample)
            (dolist (face (cube-faces sample))
              ((@ polygons push)
               (projected-polygon face width height scale
                                  "#79908a" 0.075 "#647873")))))
        polygons))

    (defun triangle-polygons (triangles width height scale color alpha stroke)
      (let ((polygons (array)))
        (dolist (triangle triangles)
          ((@ polygons push)
           (projected-polygon triangle width height scale
                              color alpha stroke)))
        polygons))

    (defun add-geometry-polygons (mesh geometry width height scale alpha stroke)
      (when show-faces
        (dolist (polygon
                  (triangle-polygons (@ geometry faces) width height scale
                                     "#2d8fbd" alpha stroke))
          ((@ mesh push) polygon)))
      (when show-bands
        (dolist (polygon
                  (triangle-polygons (@ geometry bands) width height scale
                                     "#e6aa35" alpha stroke))
          ((@ mesh push) polygon)))
      (when show-junctions
        (dolist (polygon
                  (triangle-polygons (@ geometry junctions) width height scale
                                     "#db6555" alpha stroke))
          ((@ mesh push) polygon))))

    (defun draw-polygon (context polygon)
      (let ((points (@ polygon points)))
        ((@ context begin-path))
        ((@ context move-to) (aref (aref points 0) 0)
                             (aref (aref points 0) 1))
        (loop for index from 1 below (@ points length) do
          ((@ context line-to) (aref (aref points index) 0)
                               (aref (aref points index) 1)))
        ((@ context close-path))
        (setf (@ context global-alpha) (@ polygon alpha)
              (@ context fill-style) (@ polygon color))
        ((@ context fill))
        (setf (@ context global-alpha) (min 1 (+ (@ polygon alpha) 0.28))
              (@ context stroke-style) (@ polygon stroke)
              (@ context line-width) 0.85)
        ((@ context stroke))))

    (defun draw-line (context a b width height scale color dash)
      (let ((pa (project-point a width height scale))
            (pb (project-point b width height scale)))
        ((@ context begin-path))
        ((@ context set-line-dash) dash)
        ((@ context move-to) (aref pa 0) (aref pa 1))
        ((@ context line-to) (aref pb 0) (aref pb 1))
        (setf (@ context global-alpha) 0.55
              (@ context stroke-style) color
              (@ context line-width) 1)
        ((@ context stroke))
        ((@ context set-line-dash) (array))))

    (defun draw-star-frame (context width height scale)
      (let ((corners
              (array (array -8 -8 -8) (array 8 -8 -8)
                     (array 8 8 -8) (array -8 8 -8)
                     (array -8 -8 8) (array 8 -8 8)
                     (array 8 8 8) (array -8 8 8)))
            (edges
              (array (array 0 1) (array 1 2) (array 2 3) (array 3 0)
                     (array 4 5) (array 5 6) (array 6 7) (array 7 4)
                     (array 0 4) (array 1 5) (array 2 6) (array 3 7))))
        (dolist (edge edges)
          (draw-line context
                     (aref corners (aref edge 0))
                     (aref corners (aref edge 1))
                     width height scale "#8d9894" (array 4 4)))))

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
        (when detailed-p
          (draw-star-frame context width height scale))
        ((@ occupancy sort)
         (lambda (left right) (- (@ left depth) (@ right depth))))
        (dolist (polygon occupancy)
          (draw-polygon context polygon))
        (when (and detailed-p (= view-mode "owned"))
          (add-geometry-polygons mesh (@ star surface) width height scale
                                 0.14 "#53615e"))
        (add-geometry-polygons mesh geometry width height scale
                               0.88 "#35413f")
        ((@ mesh sort)
         (lambda (left right) (- (@ left depth) (@ right depth))))
        (dolist (polygon mesh)
          (draw-polygon context polygon))
        (let ((origin (project-point (array 0 0 0) width height scale)))
          ((@ context begin-path))
          ((@ context arc) (aref origin 0) (aref origin 1)
                           (if detailed-p 3.2 1.8) 0 (* 2 pi))
          (setf (@ context global-alpha) 1
                (@ context fill-style) "#17201e")
          ((@ context fill)))
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
                      "Bright triangles belong to this orientation in the fixed family frame; XYZ shows how its axes map into that frame."
                      "Every face and band touching the center, with ownership forgotten."))
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
