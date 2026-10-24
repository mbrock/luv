(in-package #:luft.render)

;;; The CPU form of a terrain population: active star sites and their parallel
;;; packed appearance.  A streaming scene given +RENDER-POPULATION-PREPARER+
;;; builds it on the worker beside each mesh, so the owner thread only stages
;;; buffers; GPU residency is renderer-residency.lisp.

(defun ensure-terrain-quantity-contract ()
  "Check the actual decoded shader products once, outside packing loops."
  (let* ((vertex (shaders:terrain-vertex-specification))
         (fragment (shaders:star-fragment-specification))
         (outputs (luv.shader:shader-specification-outputs vertex))
         (inputs (luv.shader:shader-specification-inputs fragment)))
    (dolist (entry '((6 :surface) (7 :pattern) (8 :mortar) (9 :accent) (10 :light)))
      (destructuring-bind (location product) entry
        (let* ((output (find location outputs :key #'luv.shader:shader-interface-location))
               (input (find location inputs :key #'luv.shader:shader-interface-location))
               (expected
                 (if (eq product :light)
                     (luft::light-point-product-layout)
                     (luv.shader::parse-declaration-quantity-layout
                      (luft.terrain::terrain-varying-components product)
                      (luv.shader:find-shader-type :vec4) entry))))
          (unless (and output input
                       (math:quantity-layout=
                        expected (math:declaration-quantity-layout output))
                       (math:quantity-layout=
                        expected (math:declaration-quantity-layout input)))
            (error "Terrain ~S product disagrees with shader location ~D."
                   product location)))))
    ;; The descriptor consumes two uvec4 rows; the sidecar one, per site.
    (dolist (entry '((8 luft::light-sidecar 128)
                     (6 compile-terrain-material-descriptors 256)))
      (destructuring-bind (binding name bits) entry
        (let* ((resource (find binding (luv.shader:shader-specification-resources vertex)
                               :key #'luv.shader:shader-resource-binding))
               (layout (math:declaration-quantity-layout (math:value-declaration-for name))))
          (unless (and resource
                       (luv.shader:shader-type=
                        (luv.shader:shader-storage-buffer-element-type resource) :uvec4)
                       (= bits (math:repeated-quantity-layout-stride layout)))
            (error "Terrain packed product ~S disagrees with shader binding ~D."
                   name binding)))))
    t))

(defun pack-terrain-appearance-codes (codes)
  "Pack eight u8 sample codes per star into the GPU's parallel uvec2 lane."
  (declare (type (simple-array (unsigned-byte 8) (*)) codes)
           (optimize (speed 3) (safety 1)))
  (unless (zerop (mod (length codes) 8))
    (error "Terrain appearance has ~D bytes, not eight per active star."
           (length codes)))
  (let ((words (make-array (/ (length codes) 4)
                           :element-type '(unsigned-byte 32))))
    (loop for offset fixnum from 0 below (length codes) by 4
          for word fixnum from 0
          do (setf (aref words word)
                   (logior (aref codes offset)
                           (ash (aref codes (+ offset 1)) 8)
                           (ash (aref codes (+ offset 2)) 16)
                           (ash (aref codes (+ offset 3)) 24))))
    words))

(defstruct (render-population
             (:constructor %make-render-population
                 (instance-words appearance-words descriptor-words
                  site-count light-words &optional body-data
                  (shadow-rectangles (merge-terrain-shadow-faces instance-words))))
             (:copier nil))
  "Geometry sites plus a one-for-one, independently replaceable appearance
and voxel light."
  (instance-words #() :type (simple-array (unsigned-byte 32) (*)) :read-only t)
  (appearance-words #() :type (simple-array (unsigned-byte 32) (*))
                    :read-only t)
  (descriptor-words #() :type (simple-array (unsigned-byte 32) (*))
                    :read-only t)
  (descriptor-declaration
   (math:value-declaration-for 'compile-terrain-material-descriptors)
   :read-only t)
  (light-declaration (math:value-declaration-for 'luft::light-sidecar)
                     :read-only t)
  (site-count 0 :type (integer 0 *) :read-only t)
  ;; Four words per site: STAR-LATTICE-LIGHT-WORDS.
  (light-words #() :type (simple-array (unsigned-byte 32) (*)) :read-only t)
  ;; Geometry-only shadow compression is prepared on the worker and reused
  ;; across appearance/light changes, independently of the scene's star runs.
  (shadow-rectangles #.(make-array 0 :element-type 'single-float)
                     :type (simple-array single-float (*)) :read-only t)
  ;; Fixture bodies (LUFT.TERRAIN::PACK-FIXTURE-BODIES), drawn beside the stars.
  (body-data #.(make-array 0 :element-type 'single-float)
             :type (simple-array single-float (*)) :read-only t))

(defun ensure-population-quantity-declarations (population)
  "Check retained CPU writer contracts before GPU upload, never per lane."
  (dolist (entry
            (list (list (render-population-light-declaration population)
                        'luft::light-sidecar
                        (render-population-light-words population)
                        (* 4 (render-population-site-count population)))
                  (list (render-population-descriptor-declaration population)
                        'compile-terrain-material-descriptors
                        (render-population-descriptor-words population) nil)))
    (destructuring-bind (retained name words length) entry
      (let* ((current (math:value-declaration-for name))
             (stride (math:repeated-quantity-layout-stride
                      (math:declaration-quantity-layout current))))
        (math:ensure-declarations-compatible retained current)
        (unless (and (typep words (math:declaration-representation-type retained))
                     (zerop (mod (* 32 (length words)) stride))
                     (or (null length) (= length (length words))))
          (error "Terrain ~S storage violates its packed product declaration." name)))))
  population)

;;; Voxel light reaches the star shader at lattice points (#BQMYNU).  Every
;;; star-table vertex lies within a tick of one of the seven lattice points
;;; of the cube on the star's +X+Y+Z side, the eighth corner never being
;;; owned, so a site carries the light of those seven points.  Each is
;;; VOXEL-LIGHT-AT-LATTICE-POINT, the one value every star meeting there
;;; reads, so light is continuous across stars, owners, and seams.

(defun %light-page-near-p (field domain x y z)
  "Whether FIELD stores any page over the cells around lattice point X/Y/Z's cube."
  (let ((x-limit (1- (luft:world-domain-x-limit domain)))
        (y-limit (1- (luft:world-domain-y-limit domain))))
    (dolist (cx (list (- x 1) (+ x 1)) nil)
      (dolist (cy (list (- y 1) (+ y 1)))
        (dolist (cz (list (- z 1) (+ z 1)))
          (when (luft:voxel-light-page-present-p
                 field (max 0 (min x-limit cx)) (max 0 (min y-limit cy))
                 (max 0 (min 254 cz)))
            (return-from %light-page-near-p t)))))))

(zdefun (star-lattice-light-words :zone :luft/star-lattice-light
                                  :value (length (luft:surface-mesh-star-site-words mesh)))
    (mesh)
  "Pack MESH's lattice light: four words per site, sixteen bits per point.

Point DX + 2 DY + 4 DZ of a site at X/Y/Z is lattice point (X+DX, Y+DY, Z+DZ),
for the seven points other than (1, 1, 1); its RGB4 is the low twelve bits."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((sites (luft:surface-mesh-star-site-words mesh))
         (field (luft:surface-mesh-voxel-light mesh))
         (sky (and field (luft:voxel-light-field-skylight field)))
         (words (make-array (length sites) :element-type '(unsigned-byte 32)
                                           :initial-element #xf000f000)))
    (when field
      (let* ((domain (luft:voxel-light-field-domain field))
             (xmin 0) (ymin 0) (zmin 0)
             (xmax 0) (ymax 0) (zmax 0))
        (declare (type fixnum xmin ymin zmin xmax ymax zmax))
        (when (plusp (length sites))
          (setf xmin (aref sites 0) ymin (aref sites 1) zmin (aref sites 2))
          (loop for offset from 0 below (length sites) by 4 do
            (setf xmin (min xmin (aref sites offset))
                  ymin (min ymin (aref sites (+ offset 1)))
                  zmin (min zmin (aref sites (+ offset 2)))
                  xmax (max xmax (aref sites offset))
                  ymax (max ymax (aref sites (+ offset 1)))
                  zmax (max zmax (aref sites (+ offset 2))))))
        (let* ((width (+ 2 (- xmax xmin)))
               (height (+ 2 (- ymax ymin)))
               (depth (+ 2 (- zmax zmin)))
               (volume (* width height depth))
               ;; Regional meshes have compact bounds. Avoid allocating the
               ;; entire enclosing world for a sparse, non-regional mesh.
               (dense (when (<= volume 1048576)
                        (make-array volume :element-type '(unsigned-byte 32)
                                           :initial-element #x10000)))
               (reader (when dense
                         (%star-light-lattice-reader field sky xmin ymin zmin
                                                     width height depth dense)))
               (points (unless dense (make-hash-table :test #'eql))))
          (declare (type fixnum width height depth volume)
                   (type (or null (simple-array (unsigned-byte 32) (*))) dense))
          (flet ((point-light (x y z)
                   (declare (type fixnum x y z))
                   (let* ((key (if dense
                                   (%star-light-cache-index x y z xmin ymin zmin height depth)
                                   (logior z (ash y 9) (ash x 27))))
                          (cached (if dense (aref dense key) (gethash key points #x10000))))
                     (declare (type fixnum key cached))
                     (if (< cached #x10000)
                         cached
                         (let ((light
                                 (if reader
                                     (funcall reader x y z)
                                     (logior
                                      (luft:voxel-light-at-lattice-point field x y z)
                                      (ash (if sky
                                               (luft:voxel-sky-at-lattice-point sky x y z)
                                               15) 12)))))
                           (if dense (setf (aref dense key) light)
                               (setf (gethash key points) light))
                           light)))))
            (loop for offset fixnum from 0 below (length sites) by 4
                  for x fixnum = (aref sites offset)
                  for y fixnum = (aref sites (+ offset 1))
                  for z fixnum = (aref sites (+ offset 2))
                  when (or sky (%light-page-near-p field domain x y z))
                    do (setf (aref words offset) 0
                             (aref words (+ offset 1)) 0
                             (aref words (+ offset 2)) 0
                             (aref words (+ offset 3)) 0)
                       (dotimes (point 7)
                         (let ((light (point-light (+ x (ldb (byte 1 0) point))
                                                   (+ y (ldb (byte 1 1) point))
                                                   (+ z (ldb (byte 1 2) point)))))
                           (declare (type (unsigned-byte 16) light))
                           (setf (aref words (+ offset (floor point 2)))
                                 (logior (aref words (+ offset (floor point 2)))
                                         (ash light (* 16 (mod point 2))))))))))))
    words))

(defstruct (prepared-render-mesh
             (:constructor %make-prepared-render-mesh (mesh population))
             (:copier nil))
  "Worker-transferable CPU realization of one semantic surface mesh."
  (mesh nil :type luft:surface-mesh :read-only t)
  (population nil :type render-population :read-only t))

(zdefun (prepare-render-mesh :zone :luft/prepare-population) (mesh)
  "Canonicalize one MESH before it crosses to the renderer owner."
  (check-type mesh luft:surface-mesh)
  (%make-prepared-render-mesh mesh (make-render-population (list mesh))))

;;; The mesher marks every occupied sample with code 1 before any palette
;;; paints it.  A population whose meshes name no palette draws that code in
;;; one neutral clay, so the renderer needs no game's materials.
(defparameter +unpainted-terrain-descriptors+
  (let ((words (make-array 16 :element-type '(unsigned-byte 32)
                             :initial-element 0))
        (clay (pack-terrain-tone '(0.50 0.47 0.42))))
    ;; Two rows per code (COMPILE-TERRAIN-MATERIAL-DESCRIPTORS); code 1 is
    ;; words 8-15, with no relief and no pattern.
    (setf (aref words 8) clay (aref words 9) clay (aref words 10) clay
          (aref words 11) (pack-terrain-surface
                           (make-instance 'material-kind :base-tone nil)))
    words)
  "AIR and the mesher's one unpainted occupied code.")

(defun pack-site-edge-shaping (mesh)
  "Keep canonical stars intact; use the upload lane's spare 24 bits for edges."
  (let ((words (copy-seq (luft:surface-mesh-star-site-words mesh)))
        (edges (luft::surface-mesh-edge-shaping mesh)))
    (unless (or (zerop (length edges)) (= (* 4 (length edges)) (length words)))
      (error "Edge shaping must have one word per star."))
    (loop for edge across edges for offset from 3 by 4 do
      (setf (aref words offset) (logior (aref words offset) (ash edge 8))))
    words))

(defun %make-star-render-population (meshes)
  "Flatten geometry and its parallel active-star appearance independently."
  (load-time-value (ensure-terrain-quantity-contract))
  (let* ((site-words
           (apply #'concatenate '(simple-array (unsigned-byte 32) (*))
                  (mapcar #'pack-site-edge-shaping meshes)))
         (appearance-codes
           (apply #'concatenate '(simple-array (unsigned-byte 8) (*))
                  (mapcar #'luft:surface-mesh-appearance-codes meshes)))
         (descriptor-words
           (or (loop for mesh in meshes
                     for words =
                       (luft:surface-mesh-appearance-descriptor-words mesh)
                     when (plusp (length words)) return words)
               +unpainted-terrain-descriptors+)))
    (unless (zerop (mod (length descriptor-words) 8))
      (error "Terrain descriptor product needs eight u32 words per material."))
    (unless (= (* 2 (length site-words)) (length appearance-codes))
      (error "~D site words do not have a one-for-one eight-byte appearance (~D bytes)."
             (length site-words) (length appearance-codes)))
    (dolist (mesh meshes)
      (let ((words (luft:surface-mesh-appearance-descriptor-words mesh)))
        (unless (or (zerop (length words)) (equalp words descriptor-words))
          (error "One terrain population contains incompatible material palettes."))))
    (%make-render-population
     site-words (pack-terrain-appearance-codes appearance-codes)
     descriptor-words (/ (length site-words) 4)
     (apply #'concatenate '(simple-array (unsigned-byte 32) (*))
            (mapcar #'star-lattice-light-words meshes))
     (apply #'concatenate '(simple-array single-float (*))
            (mapcar #'luft:surface-mesh-bodies meshes)))))


(zdefun (make-render-population :zone :luft/build-render-population
                                :value (length meshes))
    (meshes)
  "Prepare the sole terrain ABI: active sites indexing the fixed star atlas."
  (%make-star-render-population meshes))

(defclass render-population-preparer () ()
  (:documentation "The renderer's realization of streamed meshes."))

(defvar +render-population-preparer+ (make-instance 'render-population-preparer)
  "Pass as a streaming scene's :MESH-PREPARER to build populations on the worker.")

(defun %mesh-light-unchanged-p (mesh previous)
  "Prove equality over the complete sampling envelope, including the cell halo.
Page identity is sufficient, page equality is exact, and sky columns witness
the implicit baseline. Unknown baseline functions never justify reuse."
  (let ((new (luft:surface-mesh-voxel-light mesh))
        (old (luft:surface-mesh-voxel-light previous))
        (sites (luft:surface-mesh-star-site-words mesh)))
    (when (eq old new) (return-from %mesh-light-unchanged-p t))
    (unless (and old new (equalp (luft:voxel-light-field-domain old)
                                (luft:voxel-light-field-domain new)))
      (return-from %mesh-light-unchanged-p nil))
    (when (zerop (length sites)) (return-from %mesh-light-unchanged-p t))
    (let* ((domain (luft:voxel-light-field-domain new))
           (x0 (aref sites 0)) (y0 (aref sites 1)) (z0 (aref sites 2))
           (x1 x0) (y1 y0) (z1 z0))
      (loop for i from 0 below (length sites) by 4 do
        (setf x0 (min x0 (aref sites i)) x1 (max x1 (aref sites i))
              y0 (min y0 (aref sites (+ i 1))) y1 (max y1 (aref sites (+ i 1)))
              z0 (min z0 (aref sites (+ i 2))) z1 (max z1 (aref sites (+ i 2)))))
      (setf x0 (max 0 (1- x0)) x1 (min (1- (luft:world-domain-x-limit domain)) (1+ x1))
            y0 (max 0 (1- y0)) y1 (min (1- (luft:world-domain-y-limit domain)) (1+ y1))
            z0 (max 0 (1- z0)) z1 (min (1- luft:+top-z+) (1+ z1)))
      (labels ((same-field (a b)
                 (when (eq a b) (return-from same-field t))
                 (unless (and a b) (return-from same-field nil))
                 (unless (eq (luft::voxel-light-field-baseline a) (luft::voxel-light-field-baseline b))
                   (let ((ac (luft::voxel-light-field-baseline-column a))
                         (bc (luft::voxel-light-field-baseline-column b)))
                     (unless (and ac bc) (return-from same-field nil))
                     (loop for y from y0 to y1 do
                       (loop for x from x0 to x1 do
                         (unless (equalp (funcall ac x y) (funcall bc x y))
                           (return-from same-field nil))))))
                 (loop for x from (* 8 (ash x0 -3)) to x1 by 8 do
                   (loop for y from (* 8 (ash y0 -3)) to y1 by 8 do
                     (loop for z from (* 8 (ash z0 -3)) to z1 by 8 do
                       (flet ((page (f)
                                (luft::%voxel-light-page-at
                                 (luft::voxel-light-field-pages f) (luft::voxel-light-field-direct-p f)
                                 (luft::voxel-light-field-x-pages f) (luft::voxel-light-field-y-pages f) x y z)))
                         (unless (equalp (page a) (page b)) (return-from same-field nil))))))
                 t))
        (and (same-field old new)
             (same-field (luft:voxel-light-field-skylight old)
                         (luft:voxel-light-field-skylight new)))))))

(defmethod prepare-streamed-mesh
    ((preparer render-population-preparer) mesh previous)
  "Reuse PREVIOUS's sites and appearance, and its light when MESH's is the same.

Each cohort joins a fresh regional field. Prove its pages and direct columns
unchanged over this mesh before sampling; otherwise repack and compare words."
  (if previous
      (let* ((population (prepared-render-mesh-population previous))
             (lights
               (if (%mesh-light-unchanged-p mesh (prepared-render-mesh-mesh previous))
                   (render-population-light-words population)
                   (star-lattice-light-words mesh)))
             (bodies (luft:surface-mesh-bodies mesh)))
        (%make-prepared-render-mesh
         mesh
         (if (and (equalp lights (render-population-light-words population))
                  (equalp bodies (render-population-body-data population)))
             population
             (%make-render-population
              (render-population-instance-words population)
              (render-population-appearance-words population)
              (render-population-descriptor-words population)
              (render-population-site-count population)
              lights
              bodies
              (render-population-shadow-rectangles population)))))
      (prepare-render-mesh mesh)))

(defmethod streamed-mesh-preparation-signature
    ((preparer render-population-preparer))
  (list (symbol-function 'make-render-population)
        (symbol-function 'pack-terrain-appearance-codes)
        (symbol-function 'star-lattice-light-words)
        (symbol-function 'merge-terrain-shadow-faces)))
