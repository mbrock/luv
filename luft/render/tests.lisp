(defpackage #:luft.render.tests
  (:use #:cl #:rove #:luft.render)
  (:local-nicknames (#:vec3 #:luv.arithmetic.lisp.vec3)))

(in-package #:luft.render.tests)

(defun sky-pixel-p (pixels offset)
  ;; The clear colour is a pale blue: blue clearly above red.
  (> (aref pixels (+ offset 2)) (+ 30 (aref pixels offset))))

(defun count-pixels (pixels width height predicate &key (from-row 0)
                                                        (to-row height))
  (loop for y from from-row below to-row
        sum (loop for x below width
                  count (funcall predicate pixels (* 4 (+ x (* y width)))))))

(deftest renderer-creation-steps-leave-traces-and-breadcrumbs
  (let ((trace (luv:make-cpu-trace :label "luft creation test"))
        (stream (make-string-output-stream)))
    (let ((luv:*log-stream* stream)
          (luv:*log-categories* '(:luft)))
      (ok (eq :created
              (luv:with-cpu-trace (trace)
                (luft.render::with-renderer-creation-step
                    (:luft/test-creation "test creation")
                  :created))))
      (let ((zones (luv:cpu-trace-zones trace)))
        (ok (= 1 (length zones)))
        (ok (eq :luft/test-creation
                (luv:cpu-trace-zone-name (first zones)))))
      (let ((log (get-output-stream-string stream)))
        (ok (search "begin test creation" log))
        (ok (search "complete test creation" log))
        (ok (not (search "interrupted test creation" log))))
      (handler-case
          (luft.render::with-renderer-creation-step
              (:luft/test-interruption "test interruption")
            (error "deliberate test interruption"))
        (error () nil))
      (let ((log (get-output-stream-string stream)))
        (ok (search "begin test interruption" log))
        (ok (search "interrupted test interruption" log))
        (ok (not (search "complete test interruption" log)))))))

(defun packed-site (site)
  "The LUFT site inside a packed one: its low sixty bits, without the stock."
  (ldb (byte luft.render.shaders:+site-stock-shift+ 0) site))

(deftest demo-scene-sites-are-its-surface-in-whole-bricks
  (let* ((scene (make-demo-scene))
         (surface (scene-surface scene))
         (sites (scene-sites scene))
         (present (map 'list #'packed-site (remove 0 sites))))
    (ok (zerop (mod (length sites) luft.render.shaders:+brick-size+)))
    (ok (= (scene-brick-count scene)
           (/ (length sites) luft.render.shaders:+brick-size+)))
    (ok (= (length present) (luft:chain-count surface)))
    (ok (every (lambda (site)
                 (luft:chain-site-p surface site))
               present))
    (ok (every (lambda (site)
                 (= 2 (luft:site-dimension site)))
               present))
    ;; The surface is closed: its boundary vanishes.
    (ok (zerop (luft:chain-count (luft:boundary-chain surface))))))

(deftest packed-sites-carry-the-stock-of-the-solid-behind-them
  ;; A face is stamped with the stock of the cell it bounds, not of the air
  ;; on the other side, and the stamp lives above the sixty bits a LUFT
  ;; site occupies, so the site itself is untouched.
  (let* ((world (make-world :horizontal-bits 4))
         (scene (progn
                  (with-stock (:limestone)
                    (fill-box world 2 5 2 5 0 0))
                  (with-stock (:oak)
                    (fill-box world 3 4 3 4 1 1))
                  (world-scene world)))
         (stocks (scene-stocks scene))
         (limestone (position :limestone stocks))
         (oak (position :oak stocks))
         (slot-of (make-hash-table)))
    (ok (and limestone oak (/= limestone oak)))
    (loop for site across (scene-sites scene)
          unless (zerop site)
            do (setf (gethash (ldb (byte luft.render.shaders:+site-stock-bits+
                                         luft.render.shaders:+site-stock-shift+)
                                   site)
                              slot-of)
                     t))
    ;; Both stocks reach the packed sites, and nothing else does.
    (ok (gethash limestone slot-of))
    (ok (gethash oak slot-of))
    (ok (= 2 (hash-table-count slot-of)))
    ;; The oak block's own top face carries oak: the cell below a face with
    ;; an upward normal is the one that owns it.
    (let ((top (find-if (lambda (site)
                          (and (not (zerop site))
                               (= luft:+xy-face-extent+
                                  (luft:site-extent (packed-site site)))
                               (= 2 (luft:site-z (packed-site site)))
                               (= 3 (luft:site-x (packed-site site)))
                               (= 3 (luft:site-y (packed-site site)))))
                        (scene-sites scene))))
      (ok top)
      (ok (= oak (ldb (byte luft.render.shaders:+site-stock-bits+
                            luft.render.shaders:+site-stock-shift+)
                      top))))))

(deftest a-world-holds-sixteen-stocks-at-most
  ;; Sixteen is what the four free bits above a packed site can name.
  (let ((world (make-world :horizontal-bits 4))
        (names (material-names)))
    (ok (= 16 luft.render.shaders:+stock-slots+))
    ;; Slot zero is turf before anything asks for a slot at all.
    (ok (zerop (world-stock-slot world :turf)))
    ;; Each new stock takes the next slot, and asking twice is idempotent.
    (let ((slots (mapcar (lambda (name) (world-stock-slot world name)) names)))
      (ok (equal slots (mapcar (lambda (name) (world-stock-slot world name))
                               names)))
      (ok (= (length slots) (length (remove-duplicates slots)))))
    ;; Past the sixteenth the world says so rather than truncating.  The
    ;; throwaway stocks live in a table of their own, or every later
    ;; picture in this image would be drawn from a palette of thirty.
    (let ((luft.render::*material-table*
            (make-hash-table :test 'eq)))
      (loop for k from 0 below 20
            for name = (intern (format nil "TEST-STOCK-~D" k) :keyword)
            do (eval `(define-material ,name)))
      (ok (signals (dotimes (k 20)
                     (world-stock-slot
                      world (intern (format nil "TEST-STOCK-~D" k) :keyword)))
                   'error)))
    ;; And an undefined stock is an error where it is asked for, not later.
    (ok (signals (world-stock-slot world :no-such-stock) 'error))))

(deftest rounded-mesh-output-fits-vulkan-guaranteed-limits
  ;; Two bevel rings make a 6x6 point grid and 5x5x2 triangles per face.
  ;; VK_EXT_mesh_shader guarantees at least 256 of each output kind.
  (let* ((side (luft.render.shaders::bevel-grid-side))
         (vertices (* side side luft.render.shaders:+brick-size+))
         (primitives (* 2 (1- side) (1- side)
                        luft.render.shaders:+brick-size+)))
    (ok (= 180 vertices))
    (ok (= 250 primitives))
    (ok (<= vertices 256))
    (ok (<= primitives 256))))

(deftest standalone-render-modes-select-only-their-own-pipelines
  (multiple-value-bind (mode style pipelines effects technique)
      (luft.render::standalone-render-options "clear" :vertex)
    (ok (equal '(:clear :flat nil nil :vertex)
               (list mode style pipelines effects technique))))
  (multiple-value-bind (mode style pipelines effects technique)
      (luft.render::standalone-render-options "bevel" :mesh)
    (ok (equal '(:bevel :bevel (:bevel) nil :mesh)
               (list mode style pipelines effects technique))))
  (multiple-value-bind (mode style pipelines effects technique)
      (luft.render::standalone-render-options "bevel" :vertex)
    (ok (equal '(:bevel :bevel (:bevel) nil :vertex)
               (list mode style pipelines effects technique))))
  (multiple-value-bind (mode style pipelines effects technique)
      (luft.render::standalone-render-options "full" :mesh)
    (ok (eq :full mode))
    (ok (eq :stock style))
    (ok (equal '(:flat :bevel :chamfer :paper :stock) pipelines))
    (ok (equal '(:sky :lens) effects))
    (ok (eq :mesh technique)))
  (multiple-value-bind (mode style pipelines effects technique)
      (luft.render::standalone-render-options "full" :vertex)
    (ok (eq :full mode))
    (ok (eq :stock style))
    (ok (equal '(:flat :bevel :chamfer :paper :stock :field :soft :ink)
                   pipelines))
    (ok (equal '(:sky :lens) effects))
    (ok (eq :vertex technique)))
  ;; A mode of its own selects only its own pipeline, the stock included.
  (multiple-value-bind (mode style pipelines effects technique)
      (luft.render::standalone-render-options "stock" :vertex)
    (ok (equal '(:stock :stock (:stock) nil :vertex)
               (list mode style pipelines effects technique))))
  ;; And with nothing named at all, the atelier opens on the whole world.
  (multiple-value-bind (mode style)
      (luft.render::standalone-render-options nil :vertex)
    (ok (eq :full mode))
    (ok (eq :stock style))))

(deftest vertex-pulling-draws-whole-grids-per-face
  ;; Six vertices draw a flat quad; the chamfer grid of one ring has four
  ;; points a side, nine quads, and so fifty-four vertices; the rounding's
  ;; two rings make six a side, twenty-five quads, a hundred and fifty.
  (ok (= 6 (luft.render.shaders:surface-vertices-per-face :flat)))
  (ok (= 54 (luft.render.shaders:surface-vertices-per-face :chamfer)))
  (ok (= 54 (luft.render.shaders:surface-vertices-per-face :paper)))
  (ok (= 150 (luft.render.shaders:surface-vertices-per-face :bevel)))
  (ok (= (luft.render.shaders:surface-vertices-per-face :bevel)
         (let ((side (let ((luft.render.shaders::*bevel-rings* 2))
                       (luft.render.shaders::bevel-grid-side))))
           (* 6 (1- side) (1- side)))))
  ;; A style outside the pipelines asked for is refused before any GPU work.
  (ok (signals (luft.render:make-renderer :style :bevel
                                          :pipeline-styles '(:flat)
                                          :scene nil :camera nil))))

(deftest brick-spheres-enclose-their-faces
  (let* ((scene (make-demo-scene))
         (sites (scene-sites scene))
         (spheres (scene-bricks scene))
         (size luft.render.shaders:+brick-size+))
    (ok (= (length spheres) (* 4 (scene-brick-count scene))))
    (ok (loop for brick below (scene-brick-count scene)
              for center-x = (aref spheres (* 4 brick))
              for center-y = (aref spheres (+ 1 (* 4 brick)))
              for center-z = (aref spheres (+ 2 (* 4 brick)))
              for radius = (aref spheres (+ 3 (* 4 brick)))
              always
              (loop for index from (* brick size) below (* (1+ brick) size)
                    for site = (packed-site (aref sites index))
                    always
                    (or (zerop site)
                        (flet ((reach (axis anchor center)
                                 (max (abs (- anchor center))
                                      (abs (- (if (luft:site-extends-p site axis)
                                                  (1+ anchor)
                                                  anchor)
                                              center)))))
                          (let* ((dx (reach :x (luft:site-x site) center-x))
                                 (dy (reach :y (luft:site-y site) center-y))
                                 (dz (reach :z (luft:site-z site) center-z)))
                            (<= (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))
                                (+ radius 1.0e-3))))))))))

(deftest the-demo-scene-renders-ground-under-sky
  ;; The background is the flat clear colour: the sky pass would put the
  ;; sun's white disc into the straight-up view below.
  (let* ((width 160)
         (height 100)
         (renderer (make-renderer :scene (make-demo-scene)
                                  :camera (make-fly-camera)
                                  :width width :height height
                                  :style :flat :effects nil)))
    (unwind-protect
         (progn
           (let* ((pixels (render-pixels renderer))
                  (sky-above (count-pixels pixels width height #'sky-pixel-p
                                           :to-row 10))
                  (ground-below (count-pixels
                                 pixels width height
                                 (lambda (pixels offset)
                                   (not (sky-pixel-p pixels offset)))
                                 :from-row 80)))
             (ok (= (* 4 width height) (length pixels)))
             (ok (> sky-above (* 0.9 10 width)))
             (ok (> ground-below (* 0.9 20 width))))
           ;; Turned straight up, nothing of the world is in view -- by the
           ;; mesh technique's frustum test or by ordinary clipping -- and
           ;; only sky remains.
           (setf (camera-pitch (renderer-camera renderer)) 1.5)
           (let ((pixels (render-pixels renderer)))
             (ok (= (* width height)
                    (count-pixels pixels width height #'sky-pixel-p)))))
      (destroy-renderer renderer))))


(defun probe-scene ()
  "A floor with a block and an L-shaped stack: pure, mixed, and concave stars."
  (let* ((domain (luft:make-world-domain :horizontal-bits 4))
         (solid (luft:make-solid-chain domain)))
    (loop for x from 1 to 8
          do (loop for y from 1 to 8
                   do (setf (luft:solid-cell-p solid x y 0) t)))
    (setf (luft:solid-cell-p solid 4 4 1) t
          (luft:solid-cell-p solid 6 4 1) t
          (luft:solid-cell-p solid 6 5 1) t
          (luft:solid-cell-p solid 6 5 2) t)
    (make-scene domain :solid solid)))

(defun mixed-stock-scene ()
  "A floor of one stock carrying shapes of several others.

Every kind of star is in it, and no two adjacent things are cut from the
same stock, so a rule that lets two faces disagree about a shared crease
has somewhere to show it."
  (let ((world (make-world :horizontal-bits 4)))
    (with-stock (:limestone)
      (dotimes (x 16) (dotimes (y 16) (setf (world-cell-p world x y 0) t))))
    (with-stock (:oak)
      (setf (world-cell-p world 4 4 1) t
            (world-cell-p world 6 4 1) t
            (world-cell-p world 6 5 1) t
            (world-cell-p world 6 5 2) t))
    (with-stock (:granite)
      ;; A two-by-two with one cell missing: mixed corners, where a
      ;; classification taken from a face rather than from the star gives
      ;; two incident faces two different answers.
      (setf (world-cell-p world 9 9 1) t
            (world-cell-p world 10 9 1) t
            (world-cell-p world 10 10 1) t
            (world-cell-p world 9 10 2) t))
    (with-stock (:brick)
      (setf (world-cell-p world 12 4 1) t
            (world-cell-p world 12 5 1) t
            (world-cell-p world 13 5 1) t
            (world-cell-p world 13 5 2) t
            (world-cell-p world 4 11 1) t
            (world-cell-p world 4 12 1) t))
    (world-scene world)))

(deftest a-site-width-and-a-bent-lattice-keep-the-surface-closed
  ;; The two experiments of #V0YCZA, put to the same question the shaping
  ;; rules were put to: straight down onto a floor, any sky inside it is a
  ;; crack.  A deformation cannot open one, because it is a function of
  ;; position alone and the faces incident to a site all hand it the same
  ;; position.  A per-site chamfer can, and did: a width taken from the
  ;; sign of the minority's dot with the /face's/ normal is coherent at an
  ;; edge and not at a mixed corner, and an inset that varies from site to
  ;; site tears a seam even where the displacements agree.
  (let* ((width 220)
         (height 220)
         (*chamfer-width* 0.3)
         (renderer (make-renderer
                    :scene (mixed-stock-scene)
                    :camera (make-fly-camera
                             :position (vec3:make-vec3 8.0 8.0 9.0)
                             :yaw 0.0 :pitch -1.5
                             :field-of-view 0.75)
                    :width width :height height
                    :style :stock :pipeline-styles '(:stock)
                    :effects nil)))
    (unwind-protect
         (dolist (rule '(:uniform :relief :stock))
           (let ((*chamfer-rule* rule))
             (ok (zerop (count-pixels (render-pixels renderer) width height
                                      #'sky-pixel-p
                                      :from-row 25 :to-row 195))
                 (format nil "~A chamfers are watertight" rule))
             (loop for (kind strength scale)
                     in '((:lean 0.2 6.0) (:taper 0.02 6.0) (:bend 0.02 6.0)
                          (:twist 0.05 6.0) (:swirl 1.2 6.0)
                          (:billow 2.0 6.0) (:globe 0.02 6.0))
                   do (let ((*deformation* kind)
                            (*deform-strength* strength)
                            (*deform-scale* scale))
                        (ok (zerop (count-pixels (render-pixels renderer)
                                                 width height #'sky-pixel-p
                                                 :from-row 25 :to-row 195))
                            (format nil "~A chamfers survive a ~A lattice"
                                    rule kind))))))
      (destroy-renderer renderer))))

(deftest every-deformation-has-a-lane-and-none-moves-its-own-centre
  (ok (equal '(:none :lean :taper :bend :twist :swirl :billow :globe)
             luft.render.shaders:*deformations*))
  (ok (zerop (luft.render.shaders:deformation-index :none)))
  (ok (signals (luft.render.shaders:deformation-index :nonesuch) 'error))
  ;; The centre lane is the middle of the world's floor unless told
  ;; otherwise, because a deformation about a corner throws the world off
  ;; the screen.
  (let ((domain (luft:make-world-domain :horizontal-bits 6)))
    (ok (equal '(32.0 32.0 0.0 0.0)
               (luft.render::deform-centre-lane domain)))
    (let ((luft.render::*deform-centre* '(1 2 3)))
      (ok (equal '(1.0 2.0 3.0 0.0)
                 (luft.render::deform-centre-lane domain))))))

(deftest shaped-surfaces-are-watertight-from-above
  ;; Straight down onto the floor, every pixel inside the floor is ground:
  ;; a crack between shaped faces would let the sky through.  Every style
  ;; the default technique draws is tried.
  (let* ((width 200)
         (height 200)
         (*bevel-radius* 0.3)
         (*chamfer-width* 0.3)
         (styles (technique-styles *default-technique*))
         (renderer (make-renderer
                    :scene (probe-scene)
                    :camera (make-fly-camera
                             :position (vec3:make-vec3 5.0 5.0 9.0)
                             :yaw 0.0 :pitch -1.5
                             :field-of-view 0.75)
                    :width width :height height
                    :style (first styles) :pipeline-styles styles
                    :effects nil)))
    (unwind-protect
         (dolist (style styles)
           (setf (renderer-style renderer) style)
           (ok (zerop (count-pixels (render-pixels renderer) width height
                                    #'sky-pixel-p
                                    :from-row 20 :to-row 180))
               (format nil "~A is watertight" style)))
      (destroy-renderer renderer))))
