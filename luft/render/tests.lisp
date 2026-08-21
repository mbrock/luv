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

(deftest demo-scene-sites-are-exactly-its-surface
  (let* ((scene (make-demo-scene))
         (surface (scene-surface scene))
         (sites (scene-sites scene))
         (present (map 'list #'packed-site (remove 0 sites))))
    (ok (= (length sites) (luft:chain-count surface)))
    (ok (= (length present) (luft:chain-count surface)))
    (ok (every (lambda (site)
                 (luft:chain-site-p surface site))
               present))
    (ok (every (lambda (site)
                 (= 2 (luft:site-dimension site)))
               present))
    ;; The surface is closed: its boundary vanishes.
    (ok (zerop (luft:chain-count (luft:boundary-chain surface))))))

(deftest foundation-records-are-one-four-u32-record-per-face
  (let* ((domain (luft:make-world-domain :horizontal-bits 3))
         (scene (make-scene domain)))
    (setf (scene-cell-p scene 2 2 2) t)
    (let ((sites (scene-sites scene))
          (records (scene-face-records scene)))
      (ok (= 6 (length sites)))
      (ok (= (* luft:+face-record-words+ (length sites))
             (length records)))
      (loop for site across sites
            for offset from 0 by luft:+face-record-words+
            do (multiple-value-bind (decorated shape reserved)
                   (luft:unpack-face-record records offset)
                 (ok (= site (ldb (byte 60 0) decorated)))
                 (ok (typep shape '(unsigned-byte 32)))
                 (ok (zerop reserved)))))))

(deftest refreshing-a-scene-publishes-a-new-revision
  (let* ((scene (make-demo-scene))
         (revision (scene-revision scene)))
    (ok (plusp revision))
    (ok (eq scene (refresh-scene scene)))
    (ok (= (1+ revision) (scene-revision scene)))))

(defun scene-agrees-with-boundary-reference-p (scene)
  "Whether every incremental product of SCENE equals a fresh reconstruction."
  (let* ((reference (luft:surface-chain (scene-solid scene)))
         (reference-sites (luft:chain-sites reference))
         (incremental-sites
           (sort (map 'vector #'packed-site (scene-sites scene)) #'<)))
    (and (equalp reference-sites
                 (luft:chain-sites (scene-surface scene)))
         (equalp reference-sites incremental-sites)
         (equalp (luft:chain-cell-bits (scene-solid scene))
                 (scene-cell-bits scene))
         (zerop (luft:chain-count
                 (luft:boundary-chain (scene-surface scene)))))))

(deftest signed-cell-edits-are-exactly-linear-boundary-updates
  ;; Cross ordinary and chunk boundaries, wrap around the horizontal torus,
  ;; and repeatedly remove cells.  At every publication the incremental
  ;; surface and dense occupancy must equal the deliberately slow reference.
  (let* ((domain (luft:make-world-domain :horizontal-bits 4))
         (scene (make-scene domain)))
    (labels ((apply-cells (&rest cells)
               (let ((edit (luft:make-chain domain)))
                 (dolist (cell cells)
                   (destructuring-bind (x y z state) cell
                     (luft:add-chain-site
                      edit (luft:make-site domain x y z luft:+cell-extent+
                                           (if state 1 -1)))))
                 (apply-scene-edit scene edit)
                 (ok (scene-agrees-with-boundary-reference-p scene)))))
      (apply-cells '(7 7 0 t) '(8 7 0 t) '(7 8 0 t) '(8 8 0 t))
      (apply-cells '(15 3 1 t) '(0 3 1 t) '(15 4 1 t))
      (apply-cells '(7 7 0 nil) '(8 8 0 nil) '(0 3 1 nil))
      ;; A deterministic toggle walk supplies additions, removals, vertical
      ;; crossings, and many successive revision histories.
      (dotimes (step 80)
        (let ((x (mod (* 7 step) 16))
              (y (mod (+ 3 (* 11 step)) 16))
              (z (mod (+ step (floor step 5)) 18)))
          (setf (scene-cell-p scene x y z)
                (not (scene-cell-p scene x y z))))
        (when (zerop (mod step 8))
          (ok (scene-agrees-with-boundary-reference-p scene))))
      (ok (scene-agrees-with-boundary-reference-p scene)))))

(deftest one-cell-publication-names-only-its-chunk-and-occupancy-word
  (let* ((scene (make-scene (luft:make-world-domain :horizontal-bits 4)))
         (revision (scene-revision scene)))
    (setf (scene-cell-p scene 3 3 3) t)
    (multiple-value-bind (chunks cell-words slot-words available-p)
        (luft.render::scene-changes-since scene revision)
      (ok available-p)
      (ok (= 1 (length chunks)))
      (ok (= 1 (length cell-words)))
      (ok (zerop (length slot-words))))
    ;; The edit protocol rejects an invalid delta before mutating any product.
    (let ((bad (luft:make-chain (scene-domain scene)))
          (revision (scene-revision scene))
          (sites (scene-sites scene)))
      (luft:add-chain-site
       bad (luft:make-site (scene-domain scene) 3 3 3 luft:+cell-extent+))
      (ok (signals (apply-scene-edit scene bad) 'error))
      (ok (= revision (scene-revision scene)))
      (ok (equalp sites (scene-sites scene))))))

(deftest foundation-records-follow-edits-across-chunk-boundaries
  ;; These two cells meet across the Y=8 surface-chunk boundary.  Removing
  ;; the first changes the corner and edge stars of faces on the second even
  ;; though those faces remain members of the surface.
  (let* ((domain (luft:make-world-domain :horizontal-bits 4))
         (solid (luft:make-solid-chain domain)))
    (setf (luft:solid-cell-p solid 7 7 2) t
          (luft:solid-cell-p solid 7 8 2) t)
    (let ((scene (make-scene domain :solid solid)))
      (setf (scene-cell-p scene 7 7 2) nil)
      (let ((incremental (scene-face-records scene)))
        (refresh-scene scene)
        (ok (equalp incremental (scene-face-records scene)))))))

(deftest a-grid-ray-names-the-hit-cell-and-the-placement-cell
  (let ((scene (make-scene (luft:make-world-domain :horizontal-bits 4))))
    (setf (scene-cell-p scene 4 4 4) t)
    (multiple-value-bind (hit before distance)
        (raycast-scene scene
                       (vec3:make-vec3 4.5 4.5 8.0)
                       (vec3:make-vec3 0.0 0.0 -1.0))
      (ok (equal '(4 4 4) hit))
      (ok (equal '(4 4 5) before))
      (ok (= 3.0 distance)))
    ;; An exact diagonal crosses grid corners simultaneously and therefore
    ;; cannot report either of the merely touched off-diagonal cells.
    (setf (scene-cell-p scene 2 1 1) t)
    (multiple-value-bind (hit)
        (raycast-scene scene
                       (vec3:make-vec3 0.5 0.5 1.5)
                       (vec3:make-vec3 1.0 1.0 0.0)
                       :max-distance 3.0)
      (ok (null hit)))))

(deftest every-construction-mode-finishes-at-the-authored-world
  (let ((target (atelier-scene :joinery)))
    (dolist (mode '(:rise :spiral :carve))
      (multiple-value-bind (scene cells ceiling)
          (luft.render::target-construction target mode)
        (declare (ignore ceiling))
        (let ((edit (luft:make-chain (scene-domain scene))))
          (loop for cell across cells
                do (luft:add-chain-site edit cell))
          (apply-scene-edit scene edit))
        (ok (luft.render::construction-finished-p scene target)
            (format nil "~S reaches the exact target solid" mode))
        (ok (scene-agrees-with-boundary-reference-p scene)
            (format nil "~S reaches the exact target surface" mode))))))

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

(deftest standalone-render-modes-select-only-their-own-pipelines
  (multiple-value-bind (mode style pipelines effects)
      (luft.render::standalone-render-options "clear")
    (ok (equal '(:clear :flat nil nil)
               (list mode style pipelines effects))))
  (multiple-value-bind (mode style pipelines effects)
      (luft.render::standalone-render-options "bevel")
    (ok (equal '(:bevel :bevel (:bevel) nil)
               (list mode style pipelines effects))))
  (multiple-value-bind (mode style pipelines effects)
      (luft.render::standalone-render-options "foundation")
    (ok (equal '(:foundation :foundation (:foundation) nil)
               (list mode style pipelines effects))))
  (multiple-value-bind (mode style pipelines effects)
      (luft.render::standalone-render-options "clay")
    (ok (equal '(:clay :clay (:clay) nil)
               (list mode style pipelines effects))))
  (multiple-value-bind (mode style pipelines effects)
      (luft.render::standalone-render-options "full")
    (ok (eq :full mode))
    (ok (eq :stock style))
    (ok (equal '(:foundation :flat :bevel :chamfer :paper :stock :field :soft
                 :ink :clay)
               pipelines))
    (ok (equal '(:sky :lens :taa) effects)))
  ;; A mode of its own selects only its own pipeline, the stock included.
  (multiple-value-bind (mode style pipelines effects)
      (luft.render::standalone-render-options "stock")
    (ok (equal '(:stock :stock (:stock) nil)
               (list mode style pipelines effects))))
  ;; And with nothing named at all, the standalone begins at the supplied
  ;; foundation ABI rather than the older multi-style showcase.
  (multiple-value-bind (mode style)
      (luft.render::standalone-render-options nil)
    (ok (eq :foundation mode))
    (ok (eq :foundation style))))

(deftest vertex-pulling-draws-whole-grids-per-face
  ;; Six vertices draw a flat quad; the chamfer grid of one ring has four
  ;; points a side, nine quads, and so fifty-four vertices; the rounding's
  ;; two rings make six a side, twenty-five quads, a hundred and fifty.
  (ok (= 6 (luft.render.shaders:surface-vertices-per-face :flat)))
  (ok (= 54 (luft.render.shaders:surface-vertices-per-face :foundation)))
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
           ;; ordinary clipping leaves only sky.
           (setf (camera-pitch (renderer-camera renderer)) 1.5)
           (let ((pixels (render-pixels renderer)))
             (ok (= (* width height)
                    (count-pixels pixels width height #'sky-pixel-p)))))
      (destroy-renderer renderer))))

(deftest a-rendered-cell-edit-uploads-one-small-face-page
  (let* ((scene (probe-scene))
         (renderer (make-renderer
                    :scene scene
                    :camera (make-fly-camera
                             :position (vec3:make-vec3 5.0 1.0 5.0)
                             :yaw 1.6 :pitch -0.6)
                    :width 96 :height 64
                    :style :flat :pipeline-styles '(:flat) :effects nil))
         (full-bytes (renderer-last-scene-upload-bytes renderer)))
    (unwind-protect
         (progn
           ;; This cell and all six of its boundary sites lie in one 8^3
           ;; chunk.  The next frame rewrites that page and one occupancy word.
           (setf (scene-cell-p scene 4 4 2) t)
           (render-pixels renderer)
           (ok (eq :incremental
                   (renderer-last-scene-upload-kind renderer)))
           ;; One face page is two coherent GPU writes: packed sites for the
           ;; established styles and foundation records for the new path.
           ;; The changed occupancy word is the third write.
           (ok (= 3 (renderer-last-scene-upload-writes renderer)))
           (ok (< (renderer-last-scene-upload-bytes renderer) full-bytes))
           (ok (= (scene-revision scene)
                  (luft.render::renderer-uploaded-scene-revision renderer))))
      (destroy-renderer renderer))))

(deftest temporal-jitter-and-frame-views-are-frame-sized-and-frozen
  (let* ((width 320)
         (height 200)
         (samples (loop for index below 8
                        collect (luft.render::temporal-jitter
                                 index width height))))
    (ok (= 8 (length (remove-duplicates samples :test #'equalp))))
    (ok (every (lambda (jitter)
                 (and (< (abs (* 0.5 width (aref jitter 0))) 0.5)
                      (< (abs (* 0.5 height (aref jitter 1))) 0.5)))
               samples))
    (let* ((camera (make-fly-camera))
           (view (luft.render::capture-frame-view
                  camera width height (first samples)))
           (old-x (vec3:vec3-x (luft.render::frame-view-position view))))
      (setf (camera-position camera) (vec3:make-vec3 1.0 2.0 3.0))
      (ok (= old-x
             (vec3:vec3-x (luft.render::frame-view-position view))))
      (let ((data (frame-uniform-data view width height nil 0.2 0.01
                                      view t 0.875)))
        (ok (= 104 (length data)))
        (ok (= (aref (first samples) 0) (aref data 96)))
        (ok (= (aref (first samples) 1) (aref data 97)))
        (ok (= 1.0 (aref data 102)))
        (ok (= 0.875 (aref data 103)))))))

(deftest only-temporal-surface-shaders-write-motion
  (dolist (pair (list
                 (list (luft.render.shaders:surface-fragment-shader)
                       (luft.render.shaders:temporal-surface-fragment-shader))
                 (list (luft.render.shaders:chamfer-fragment-shader)
                       (luft.render.shaders:temporal-chamfer-fragment-shader))
                 (list (luft.render.shaders:paper-fragment-shader)
                       (luft.render.shaders:temporal-paper-fragment-shader))
                 (list (luft.render.shaders:sky-fragment-shader)
                       (luft.render.shaders:temporal-sky-fragment-shader))
                 (list (luft.render.shaders:field-fragment-shader)
                       (luft.render.shaders:temporal-field-fragment-shader))
                 (list (luft.render.shaders:ink-fragment-shader)
                       (luft.render.shaders:temporal-ink-fragment-shader))
                 (list (luft.render.shaders:stock-fragment-shader)
                       (luft.render.shaders:temporal-stock-fragment-shader))))
    (destructuring-bind (ordinary temporal) pair
      (ok (= 1 (length (luv.shader:shader-specification-outputs ordinary))))
      (let ((outputs (luv.shader:shader-specification-outputs temporal)))
        (ok (= 2 (length outputs)))
        (ok (= 1 (luv.shader:shader-interface-location (second outputs))))))))


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

(deftest temporal-history-resolves-and-invalidates
  (let* ((scene (probe-scene))
         (camera (make-fly-camera
                  :position (vec3:make-vec3 5.0 1.0 5.0)
                  :yaw 1.6 :pitch -0.6))
         (renderer (make-renderer :scene scene :camera camera
                                  :width 96 :height 64
                                  :style :flat :pipeline-styles '(:flat)
                                  :effects '(:taa))))
    (unwind-protect
         (progn
           (ok (eq :rgba16-float
                   (luv:gpu-texture-format
                    (luft.render::renderer-scene-texture renderer))))
           (ok (eq :rg16-float
                   (luv:gpu-texture-format
                    (luft.render::renderer-motion-texture renderer))))
           #+darwin
           (ok (typep
                (luft.render::frame-surfaces-temporal-scaler
                 (luft.render::renderer-surfaces renderer))
                'luv:gpu-temporal-scaler))
           #-darwin
           (ok (null
                (luft.render::frame-surfaces-temporal-scaler
                 (luft.render::renderer-surfaces renderer))))
           (ok (= (* 4 96 64) (length (render-pixels renderer))))
           (ok (not (luft.render::renderer-history-used-p renderer)))
           (render-pixels renderer)
           (ok (luft.render::renderer-history-used-p renderer))
           (ok (= 2 (luft.render::renderer-frame-index renderer)))
           (ok (zerop (luft.render::renderer-history-index renderer)))
           ;; The key is made from resolved uniform/material values, not just
           ;; their preset names: hand-tuning an atelier knob is a cut too.
           (let ((*exposure* (+ *exposure* 0.1)))
             (render-pixels renderer)
             (ok (not (luft.render::renderer-history-used-p renderer))))
           ;; Refreshing the same object is a publication, not an identity
           ;; change; its revision must still force a fresh history sample.
           (refresh-scene scene)
           (render-pixels renderer)
           (ok (not (luft.render::renderer-history-used-p renderer)))
           (ok (= (scene-revision scene)
                  (luft.render::renderer-uploaded-scene-revision renderer)))
           ;; Likewise a teleport is a cut, while an ordinary fly-camera
           ;; step on the next frame resumes reprojection.
           (setf (camera-position camera) (vec3:make-vec3 40.0 40.0 30.0))
           (render-pixels renderer)
           (ok (not (luft.render::renderer-history-used-p renderer)))
           ;; Extent-sized temporal ownership is replaced as one cohort.  The
           ;; next frame must use the new scaler/history and begin cold.
           (let ((old-scaler
                   (luft.render::frame-surfaces-temporal-scaler
                    (luft.render::renderer-surfaces renderer))))
             (declare (ignorable old-scaler))
             (luft.render::ensure-renderer-extent renderer '(80 48))
             #+darwin
             (ok (not (eq old-scaler
                          (luft.render::frame-surfaces-temporal-scaler
                           (luft.render::renderer-surfaces renderer)))))
             (ok (= (* 4 80 48) (length (render-pixels renderer))))
             (ok (not (luft.render::renderer-history-used-p renderer)))))
      (destroy-renderer renderer))))

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
  ;; The two experiments of #REZ0PU, put to the same question the shaping
  ;; rules were put to: straight down onto a floor, any sky inside it is a
  ;; crack.  A deformation cannot open one, because it is a function of
  ;; position alone and the faces incident to a site all hand it the same
  ;; position.  A per-site chamfer can, and did: a width taken from the
  ;; sign of the minority's dot with the /face's/ normal is coherent at an
  ;; edge and not at a mixed corner, and an inset that varies from site to
  ;; site tears a seam even where the displacements agree.  #HJ6YTC
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
                                    rule kind))))
             ;; And the noisy lattice, whose amplitude differs from cell to
             ;; cell: a field, therefore the same from every face that asks
             ;; about a point, therefore watertight.  The scene it is asked
             ;; of has four stocks of four different grits in it.
             (loop for (strength grain) in '((0.4 2.0) (1.0 3.0) (1.8 6.0))
                   do (let ((*erode-strength* strength)
                            (*erode-grain* grain))
                        (ok (zerop (count-pixels (render-pixels renderer)
                                                 width height #'sky-pixel-p
                                                 :from-row 25 :to-row 195))
                            (format nil "~A chamfers survive a lattice eroded ~
by ~,1F cells" rule strength))))
             ;; Eroded and bent at once, since the two compose.
             (let ((*erode-strength* 0.8)
                   (*erode-grain* 3.0)
                   (*deformation* :twist)
                   (*deform-strength* 0.05))
               (ok (zerop (count-pixels (render-pixels renderer)
                                        width height #'sky-pixel-p
                                        :from-row 25 :to-row 195))
                   (format nil "~A chamfers survive erosion and a twist"
                           rule)))))
      (destroy-renderer renderer))))

(deftest every-deformation-has-a-lane-and-none-moves-its-own-centre
  (ok (equal '(:none :lean :taper :bend :twist :swirl :billow :globe)
             luft.render.shaders:*deformations*))
  (ok (zerop (luft.render.shaders:deformation-index :none)))
  (ok (signals (luft.render.shaders:deformation-index :nonesuch) 'error))
  ;; The centre lane is the middle of the world's floor unless told
  ;; otherwise, because a deformation about a corner throws the world off
  ;; the screen; its fourth component is the erosion's wavelength.
  (let ((domain (luft:make-world-domain :horizontal-bits 6))
        (luft.render::*erode-grain* 4.0))
    (ok (equal '(32.0 32.0 0.0 4.0)
               (luft.render::deform-centre-lane domain)))
    (let ((luft.render::*deform-centre* '(1 2 3)))
      (ok (equal '(1.0 2.0 3.0 4.0)
                 (luft.render::deform-centre-lane domain))))))

(deftest shaped-surfaces-are-watertight-from-above
  ;; Straight down onto the floor, every pixel inside the floor is ground:
  ;; a crack between shaped faces would let the sky through.  Every style
  ;; Luft draws is tried.
  (let* ((width 200)
         (height 200)
         (*bevel-radius* 0.3)
         (*chamfer-width* 0.3)
         (styles luft.render::*surface-styles*)
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
