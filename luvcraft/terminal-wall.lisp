;;; A Ghostty terminal projected across a rectangle of authored world blocks.

(in-package #:luvcraft)

(defclass terminal-grid-domain ()
  ((columns :initarg :columns :reader terminal-grid-domain-columns)
   (rows :initarg :rows :reader terminal-grid-domain-rows)))

(defmethod domains:domain-cardinality ((domain terminal-grid-domain))
  (* (terminal-grid-domain-columns domain)
     (terminal-grid-domain-rows domain)))

(defun terminal-grid-offset (domain column row)
  "Return the row-major offset for COLUMN,ROW in DOMAIN."
  (let ((columns (terminal-grid-domain-columns domain))
        (rows (terminal-grid-domain-rows domain)))
    (unless (and (<= 0 column) (< column columns)
                 (<= 0 row) (< row rows))
      (error "Terminal cell (~D,~D) is outside ~Dx~D."
             column row columns rows))
    (+ column (* row columns))))

(defun terminal-grid-coordinate (domain offset)
  "Return COLUMN and ROW for row-major OFFSET in DOMAIN."
  (unless (< -1 offset (domains:domain-cardinality domain))
    (error "Terminal offset ~D is outside a ~D-cell grid."
           offset (domains:domain-cardinality domain)))
  (multiple-value-bind (row column)
      (floor offset (terminal-grid-domain-columns domain))
    (values column row)))

(defstruct (terminal-grid-presentation
             (:constructor %make-terminal-grid-presentation
                 (&key domain characters)))
  domain
  characters)

(defstruct terminal-glyph-occurrence
  glyph
  column
  row)

;;; The frame records screen-right, screen-up, and outward in voxel axes.  EQL
;;; methods keep the six closed orientations inspectable without making block
;;; materials or terminal surfaces branch on their names.

(defstruct (terminal-face-frame
             (:constructor make-terminal-face-frame (right up outward)))
  right
  up
  outward)

(defgeneric terminal-face-frame (face))

(defmethod terminal-face-frame ((face (eql :front)))
  (make-terminal-face-frame
   +voxel-negative-x+ +voxel-positive-y+ +voxel-positive-z+))

(defmethod terminal-face-frame ((face (eql :back)))
  (make-terminal-face-frame
   +voxel-positive-x+ +voxel-positive-y+ +voxel-negative-z+))

(defmethod terminal-face-frame ((face (eql :right)))
  (make-terminal-face-frame
   +voxel-positive-z+ +voxel-positive-y+ +voxel-positive-x+))

(defmethod terminal-face-frame ((face (eql :left)))
  (make-terminal-face-frame
   +voxel-negative-z+ +voxel-positive-y+ +voxel-negative-x+))

(defmethod terminal-face-frame ((face (eql :top)))
  (make-terminal-face-frame
   +voxel-positive-x+ +voxel-positive-z+ +voxel-positive-y+))

(defmethod terminal-face-frame ((face (eql :bottom)))
  (make-terminal-face-frame
   +voxel-positive-x+ +voxel-negative-z+ +voxel-negative-y+))

(defclass terminal-surface ()
  ((world :initarg :world :reader terminal-surface-world)
   (material :initarg :material :reader terminal-surface-material)
   (absent-neighbor-policy
    :initarg :absent-neighbor-policy :initform :air
    :reader terminal-surface-absent-neighbor-policy)
   (face :initarg :face :reader terminal-surface-face)
   ;; The block at screen-space column zero, row zero (lower left).
   (origin :initarg :origin :reader terminal-surface-origin)
   (block-width :initarg :block-width :reader terminal-surface-width)
   (block-height :initarg :block-height :reader terminal-surface-height)
   ;; These keys are the exact authored chunk materializations whose block
   ;; content can change membership or exposure.  Owner keys are the subset
   ;; whose visible native meshes carry the display face itself.
   (dependency-keys :initform nil :accessor terminal-surface-dependency-keys)
   (owner-chunk-keys :initform nil
                     :accessor terminal-surface-owner-chunk-keys)
   (dependency-stamp :initform nil
                     :accessor terminal-surface-retained-dependency-stamp)
   (observed-world-revision
    :initform -1 :accessor terminal-surface-observed-world-revision)
   (state :initform :current :accessor terminal-surface-state)
   (reconciliations :initform 0
                    :accessor terminal-surface-reconciliations)))

(defclass terminal-display ()
  ((session :initarg :session :reader terminal-display-session)
   (surface :initarg :surface :reader terminal-display-surface)
   (terminal :initarg :terminal :reader terminal-display-terminal)
   (device :initarg :device :initform nil :accessor terminal-display-device)
   (presentation :initarg :presentation :reader terminal-display-presentation)
   (glyph-cache :initarg :glyph-cache :reader terminal-display-glyph-cache)
   (glyph-run :initarg :glyph-run :reader terminal-display-glyph-run)
   (frame-bind-groups :initform (make-hash-table :test #'eq)
                      :reader terminal-display-frame-bind-groups)))

(defun split-terminal-lines (text)
  (loop with start = 0
        for end = (position #\Newline text :start start)
        collect (string-right-trim '(#\Return) (subseq text start end))
        while end
        do (setf start (1+ end))))

(defun make-terminal-grid-presentation (domain text)
  "Copy formatted terminal TEXT into one exact dense viewport DOMAIN."
  (let* ((columns (terminal-grid-domain-columns domain))
         (rows (terminal-grid-domain-rows domain))
         (characters
           (make-array (domains:domain-cardinality domain)
                       :element-type 'character :initial-element #\Space)))
    (loop for line in (split-terminal-lines text)
          for row below rows
          do (loop for character across line
                   for column below columns
                   do (setf (aref characters
                                  (terminal-grid-offset domain column row))
                            character)))
    (%make-terminal-grid-presentation
     :domain domain :characters characters)))

(defun terminal-grid-character (presentation column row)
  (aref (terminal-grid-presentation-characters presentation)
        (terminal-grid-offset
         (terminal-grid-presentation-domain presentation) column row)))

(defun terminal-fixture-framed-line (content)
  (format nil "│ ~76A │" content))

(defun terminal-display-fixture ()
  "Return deterministic fake PTY output for the world display proof."
  (let ((escape (code-char 27))
        (return (code-char 13)))
    (with-output-to-string (stream)
      (format stream "~C[2J~C[H" escape escape)
      (dolist (line
               (list
                "┌──────────────────────────────────────────────────────────────────────────────┐"
                (terminal-fixture-framed-line
                 "luvcraft :: ghostty terminal block material")
                "├──────────────────────────────────────────────────────────────────────────────┤"
                (terminal-fixture-framed-line
                 "one 80 x 24 terminal projected across one linked 8 x 5 block surface")
                (terminal-fixture-framed-line "")
                (terminal-fixture-framed-line "$ uname -a")
                (terminal-fixture-framed-line
                 "Darwin little-world arm64 26.0.0")
                (terminal-fixture-framed-line "$ ./scripts/wiki marks next")
                (terminal-fixture-framed-line
                 "NTQ5KY  Bridge Ghostty render-state rows into the world display")
                (terminal-fixture-framed-line "")
                (terminal-fixture-framed-line
                 "Ghostty owns the grid.  Luv owns the voxels.  Slug owns the curves.")
                (terminal-fixture-framed-line "")
                (terminal-fixture-framed-line "terminal surface")
                (terminal-fixture-framed-line "  ├─ material   :terminal")
                (terminal-fixture-framed-line "  ├─ face       :back")
                (terminal-fixture-framed-line "  ├─ blocks     8 x 5")
                (terminal-fixture-framed-line "  └─ cells      80 x 24")
                (terminal-fixture-framed-line "")
                (terminal-fixture-framed-line
                 "the font grid belongs to the whole face rectangle, not to each block")
                (terminal-fixture-framed-line "")
                (terminal-fixture-framed-line "$ echo 'hello from the magic voxel tv'")
                (terminal-fixture-framed-line "hello from the magic voxel tv")
                (terminal-fixture-framed-line "$ █")
                "└──────────────────────────────────────────────────────────────────────────────┘"))
        (write-string line stream)
        (write-char return stream)
        (terpri stream)))))

(defun terminal-offset-point (origin &rest vector-scales)
  (let ((x (vec3-x origin))
        (y (vec3-y origin))
        (z (vec3-z origin)))
    (loop for (vector scale) on vector-scales by #'cddr
          do (incf x (* (vec3-x vector) scale))
             (incf y (* (vec3-y vector) scale))
             (incf z (* (vec3-z vector) scale)))
    (make-vec3 x y z)))

(defun voxel-direction-vec3 (direction)
  (make-vec3 (voxel-direction-dx direction)
             (voxel-direction-dy direction)
             (voxel-direction-dz direction)))

(defun terminal-coordinate-step (coordinate direction &optional (amount 1))
  (make-world-coordinate
   (+ (world-coordinate-x coordinate)
      (* amount (voxel-direction-dx direction)))
   (+ (world-coordinate-y coordinate)
      (* amount (voxel-direction-dy direction)))
   (+ (world-coordinate-z coordinate)
      (* amount (voxel-direction-dz direction)))))

(defun terminal-absent-neighbor-solid-p (policy coordinate)
  (ecase policy
    (:air nil)
    (:solid t)
    (:error
     (error "Terminal surface reached absent terrain at ~S." coordinate))))

(defun terminal-face-exposed-p
    (world material coordinate outward absent-neighbor-policy)
  (multiple-value-bind (block availability)
      (sample-block-at world
                       (world-coordinate-x coordinate)
                       (world-coordinate-y coordinate)
                       (world-coordinate-z coordinate))
    (and (eq availability :resident)
         (eq block material)
         (let ((neighbor (terminal-coordinate-step coordinate outward)))
           (multiple-value-bind (outside outside-availability)
               (sample-block-at world
                                (world-coordinate-x neighbor)
                                (world-coordinate-y neighbor)
                                (world-coordinate-z neighbor))
             (ecase outside-availability
               (:resident (not (block-solid-p outside)))
               (:absent
                (not (terminal-absent-neighbor-solid-p
                      absent-neighbor-policy neighbor)))))))))

;;; Discovery is a discover-once frontier program (#K3PCP3): starting from
;;; one exposed terminal face, admit coplanar neighbours which are the same
;;; material and exposed, mark each once, and retain the admitted component.
;;; The compiled realization walks chunk offsets through the world's chunk
;;; window; no coordinate objects, hash keys, or conses appear per site.

(frontiers:define-frontier-program terminal-surface-discovery
  :family :discover-once
  :frontier-layout :lifo-stack
  :neighborhood (:voxel-relations coplanar-directions)
  :materialization :block-chunks
  :fields ((visited :memo t) terminal exposed)
  :constants (material outward absent-neighbor-policy coplanar-directions memo)
  :admission (and (terminal target) (exposed target))
  :retain-admissions t)

(defun terminal-discovery-visited-lane (memo chunk)
  "The per-chunk visited bits owned by one discovery's MEMO table."
  (or (gethash chunk memo)
      (setf (gethash chunk memo)
            (make-array (chunk-domain-cardinality (block-chunk-domain chunk))
                        :element-type 'bit :initial-element 0))))

(declaim (inline terminal-site-exposed-p))
(defun terminal-site-exposed-p (window chunk offset outward policy)
  "Whether the OUTWARD neighbour of CHUNK/OFFSET leaves that face visible."
  (let ((domain (block-chunk-domain chunk)))
    (multiple-value-bind (local-x local-y local-z)
        (chunk-domain-local-components domain offset)
      (multiple-value-bind (world-x world-y world-z)
          (chunk-domain-world-components domain local-x local-y local-z)
        (multiple-value-bind (neighbor neighbor-offset availability)
            (locate-chunk-window-site
             window
             (+ world-x (voxel-direction-dx outward))
             (+ world-y (voxel-direction-dy outward))
             (+ world-z (voxel-direction-dz outward)))
          (ecase availability
            (:available
             (not (block-solid-p
                   (block-content-at-offset
                    (block-chunk-content neighbor) neighbor-offset))))
            (:unavailable
             (ecase policy
               (:air t)
               (:solid nil)
               (:error
                (error "Terminal surface reached absent terrain at ~S."
                       (make-world-coordinate
                        (+ world-x (voxel-direction-dx outward))
                        (+ world-y (voxel-direction-dy outward))
                        (+ world-z (voxel-direction-dz outward)))))))))))))

(defun terminal-discovery-bindings ()
  (list
   (frontiers:make-frontier-field-binding
    'visited
    :lanes '((bits (terminal-discovery-visited-lane memo materialization)
                   :type simple-bit-vector))
    :read '(= 1 (sbit bits offset))
    :write '(setf (sbit bits offset) 1))
   (frontiers:make-frontier-field-binding
    'terminal
    :lanes '((content (block-chunk-content materialization)))
    :read '(eq (block-content-at-offset content offset) material))
   ;; Exposure probes the outward neighbour through the window; it is only
   ;; worth asking once the site is terminal material, so it reads lazily.
   (frontiers:make-frontier-field-binding
    'exposed
    :read '(terminal-site-exposed-p
            window materialization offset outward absent-neighbor-policy)
    :lazy t)))

(defvar *terminal-discovery-realization* nil)

(defun terminal-discovery-realization ()
  (if (and *terminal-discovery-realization*
           (frontiers:frontier-realization-current-p
            *terminal-discovery-realization*))
      *terminal-discovery-realization*
      (setf *terminal-discovery-realization*
            (frontiers:compile-frontier-program
             'terminal-surface-discovery
             :bindings (terminal-discovery-bindings)
             :site-domain '(block-chunk-domain materialization)))))

(defmethod frontiers:note-frontier-program-redefinition
    ((name (eql 'terminal-surface-discovery)))
  (declare (ignore name))
  (setf *terminal-discovery-realization* nil))

(defun discover-terminal-component (world x y z face material absent-neighbor-policy)
  "Discover the coplanar exposed component of MATERIAL containing X,Y,Z.

Return the retained execution, or NIL and a status when the seed itself is
not an exposed terminal face."
  (let* ((frame (terminal-face-frame face))
         (right (terminal-face-frame-right frame))
         (up (terminal-face-frame-up frame))
         (outward (terminal-face-frame-outward frame))
         (directions (list right (opposite-voxel-direction right)
                           up (opposite-voxel-direction up)))
         (realization (terminal-discovery-realization))
         (frontier (frontiers:make-realization-frontier realization
                                                        :initial-capacity 64))
         (execution (frontiers:make-realization-execution
                     realization world frontier))
         (memo (make-hash-table :test #'eq)))
    (multiple-value-bind (chunk offset availability)
        (locate-chunk-window-site world x y z)
      (unless (and (eq availability :available)
                   (eq (block-content-at-offset (block-chunk-content chunk) offset)
                       material))
        (return-from discover-terminal-component (values nil :not-terminal)))
      (unless (terminal-site-exposed-p world chunk offset outward
                                       absent-neighbor-policy)
        (return-from discover-terminal-component (values nil :covered)))
      (flet ((constants ()
               (list :material material :outward outward
                     :absent-neighbor-policy absent-neighbor-policy
                     :coplanar-directions directions :memo memo)))
        (apply #'frontiers:admit-frontier-realization-site
               realization world frontier execution chunk offset nil
               (constants))
        (apply #'frontiers:drain-frontier-realization
               realization world frontier execution (constants))))
    (values execution :component)))

(defun map-execution-admitted-sites (function execution)
  "Call FUNCTION with the world X, Y, Z of every retained admitted site."
  (let* ((sites (frontiers:frontier-execution-admitted-sites execution))
         (chunks (frontiers:frontier-site-buffer-materialization-lane sites))
         (offsets (frontiers:frontier-site-buffer-offset-lane sites)))
    (dotimes (index (frontiers:frontier-site-buffer-length sites))
      (let* ((chunk (aref chunks index))
             (domain (block-chunk-domain chunk)))
        (multiple-value-bind (local-x local-y local-z)
            (chunk-domain-local-components domain (aref offsets index))
          (multiple-value-bind (x y z)
              (chunk-domain-world-components domain local-x local-y local-z)
            (funcall function x y z)))))))

(defun find-terminal-surface (world x y z face
                              &key (material *terminal-block*)
                                   (absent-neighbor-policy :air))
  "Discover the maximal exposed rectangular terminal surface at X,Y,Z,FACE.

Return the surface and :RECTANGLE.  A seed which is not terminal, is covered,
or belongs to a non-rectangular coplanar component instead returns NIL and a
descriptive status.  Discovery runs the compiled TERMINAL-SURFACE-DISCOVERY
program; this function only folds the retained component into a rectangle."
  (let* ((frame (terminal-face-frame face))
         (right (terminal-face-frame-right frame))
         (up (terminal-face-frame-up frame)))
    (multiple-value-bind (execution status)
        (discover-terminal-component
         world x y z face material absent-neighbor-policy)
      (unless execution
        (return-from find-terminal-surface (values nil status)))
      (let ((minimum-u most-positive-fixnum) (maximum-u most-negative-fixnum)
            (minimum-v most-positive-fixnum) (maximum-v most-negative-fixnum)
            (count 0)
            (origin-x 0) (origin-y 0) (origin-z 0))
        (flet ((projection (x y z direction)
                 (+ (* x (voxel-direction-dx direction))
                    (* y (voxel-direction-dy direction))
                    (* z (voxel-direction-dz direction)))))
          (map-execution-admitted-sites
           (lambda (x y z)
             (let ((u (projection x y z right))
                   (v (projection x y z up)))
               (incf count)
               (setf minimum-u (min minimum-u u) maximum-u (max maximum-u u)
                     minimum-v (min minimum-v v) maximum-v (max maximum-v v))))
           execution)
          (map-execution-admitted-sites
           (lambda (x y z)
             (when (and (= minimum-u (projection x y z right))
                        (= minimum-v (projection x y z up)))
               (setf origin-x x origin-y y origin-z z)))
           execution))
        (let ((width (1+ (- maximum-u minimum-u)))
              (height (1+ (- maximum-v minimum-v))))
          (unless (= count (* width height))
            (return-from find-terminal-surface
              (values nil :non-rectangular)))
          (let ((surface
                  (make-instance
                   'terminal-surface
                   :world world :material material
                   :absent-neighbor-policy absent-neighbor-policy
                   :face face
                   :origin (make-world-coordinate origin-x origin-y origin-z)
                   :block-width width :block-height height)))
            (initialize-terminal-surface-dependencies surface)
            (values surface :rectangle)))))))

(defun terminal-surface-coordinate (surface column row)
  (let* ((frame (terminal-face-frame (terminal-surface-face surface)))
         (across
           (terminal-coordinate-step
            (terminal-surface-origin surface)
            (terminal-face-frame-right frame) column)))
    (terminal-coordinate-step
     across (terminal-face-frame-up frame) row)))

(defun terminal-surface-shape-current-p (surface)
  "Check SURFACE membership and maximality against authored world content."
  (let* ((world (terminal-surface-world surface))
         (material (terminal-surface-material surface))
         (absent-neighbor-policy
           (terminal-surface-absent-neighbor-policy surface))
         (frame (terminal-face-frame (terminal-surface-face surface)))
         (right (terminal-face-frame-right frame))
         (up (terminal-face-frame-up frame))
         (outward (terminal-face-frame-outward frame))
         (width (terminal-surface-width surface))
         (height (terminal-surface-height surface)))
    (and
     (loop for row below height always
       (loop for column below width
             always (terminal-face-exposed-p
                     world material
                     (terminal-surface-coordinate surface column row)
                     outward absent-neighbor-policy)))
     ;; A newly adjacent exposed terminal block means this retained rectangle
     ;; is no longer maximal; its renderer must be rediscovered/rebuilt.
     (loop for row below height always
       (and (not (terminal-face-exposed-p
                  world material
                  (terminal-coordinate-step
                   (terminal-surface-coordinate surface 0 row) right -1)
                  outward absent-neighbor-policy))
            (not (terminal-face-exposed-p
                  world material
                  (terminal-coordinate-step
                   (terminal-surface-coordinate surface (1- width) row) right)
                  outward absent-neighbor-policy))))
     (loop for column below width always
       (and (not (terminal-face-exposed-p
                  world material
                  (terminal-coordinate-step
                   (terminal-surface-coordinate surface column 0) up -1)
                  outward absent-neighbor-policy))
            (not (terminal-face-exposed-p
                  world material
                  (terminal-coordinate-step
                   (terminal-surface-coordinate surface column (1- height)) up)
                  outward absent-neighbor-policy)))))))

(defun terminal-surface-dependency-coordinates (surface)
  "Return the cold set of cells whose content determines SURFACE validity."
  (let* ((frame (terminal-face-frame (terminal-surface-face surface)))
         (right (terminal-face-frame-right frame))
         (up (terminal-face-frame-up frame))
         (outward (terminal-face-frame-outward frame))
         (width (terminal-surface-width surface))
         (height (terminal-surface-height surface))
         (coordinates nil))
    (labels ((observe (coordinate)
               (push coordinate coordinates)
               (push (terminal-coordinate-step coordinate outward)
                     coordinates)))
      (dotimes (row height)
        (dotimes (column width)
          (observe (terminal-surface-coordinate surface column row)))
        (observe
         (terminal-coordinate-step
          (terminal-surface-coordinate surface 0 row) right -1))
        (observe
         (terminal-coordinate-step
          (terminal-surface-coordinate surface (1- width) row) right)))
      (dotimes (column width)
        (observe
         (terminal-coordinate-step
          (terminal-surface-coordinate surface column 0) up -1))
        (observe
         (terminal-coordinate-step
          (terminal-surface-coordinate surface column (1- height)) up))))
    coordinates))

(defun terminal-coordinate-chunk-key (space coordinate)
  (multiple-value-bind (chunk-x chunk-y chunk-z)
      (voxel-space-decompose-components
       space
       (world-coordinate-x coordinate)
       (world-coordinate-y coordinate)
       (world-coordinate-z coordinate))
    (chunk-key chunk-x chunk-y chunk-z)))

(defun terminal-sort-chunk-keys (keys)
  (sort (remove-duplicates keys :test #'equal)
        (lambda (left right)
          (loop for a in left
                for b in right
                when (/= a b) return (< a b)
                finally (return nil)))))

(defun terminal-surface-dependency-stamp (surface)
  "Name the authored chunk materializations which justify SURFACE."
  (let ((world (terminal-surface-world surface)))
    (loop for key in (terminal-surface-dependency-keys surface)
          collect
          (multiple-value-bind (chunk present-p)
              (apply #'world-chunk-at world key)
            (if present-p
                (list (copy-list key)
                      (block-chunk-incarnation chunk)
                      (block-chunk-revision chunk))
                (list (copy-list key) nil))))))

(defun initialize-terminal-surface-dependencies (surface)
  (let* ((world (terminal-surface-world surface))
         (space (block-world-space world))
         (owner-keys
           (loop for row below (terminal-surface-height surface)
                 append
                 (loop for column below (terminal-surface-width surface)
                       collect
                       (terminal-coordinate-chunk-key
                        space
                        (terminal-surface-coordinate
                         surface column row)))))
         (dependency-keys
           (mapcar (lambda (coordinate)
                     (terminal-coordinate-chunk-key space coordinate))
                   (terminal-surface-dependency-coordinates surface))))
    (setf (terminal-surface-owner-chunk-keys surface)
          (terminal-sort-chunk-keys owner-keys)
          (terminal-surface-dependency-keys surface)
          (terminal-sort-chunk-keys dependency-keys)
          (terminal-surface-retained-dependency-stamp surface)
          (terminal-surface-dependency-stamp surface)
          (terminal-surface-observed-world-revision surface)
          (block-world-revision world)
          (terminal-surface-state surface) :current)
    surface))

(defun terminal-surface-visible-mesh-current-p (surface session)
  "Whether each native mesh carrying SURFACE is a current visible product."
  (let ((products (luvcraft-session-chunk-products session)))
    (loop for key in (terminal-surface-owner-chunk-keys surface)
          for product = (gethash key products)
          always (and product
                      (current-luvcraft-chunk-product-p
                       session key product)))))

(defun reconcile-terminal-surface (surface &optional session)
  "Reconcile derived SURFACE state after its native mesh publication boundary.

An authored edit does not immediately alter the retained display.  When the
surface's visible chunk products are still stale, the old geometry and glyph
projection remain one last-known-good cohort."
  (let* ((world (terminal-surface-world surface))
         (world-revision (block-world-revision world)))
    (unless (= world-revision
               (terminal-surface-observed-world-revision surface))
      (let ((stamp (terminal-surface-dependency-stamp surface)))
        (cond
          ((equal stamp
                  (terminal-surface-retained-dependency-stamp surface))
           ;; The edit was outside the chunks this surface observes.
           (setf (terminal-surface-observed-world-revision surface)
                 world-revision))
          ((and session
                (not (terminal-surface-visible-mesh-current-p
                      surface session)))
           ;; Keep both halves of the old visible cohort until native mesh
           ;; publication catches up.  Revisit on the next frame.
           nil)
          (t
           (setf (terminal-surface-retained-dependency-stamp surface) stamp
                 (terminal-surface-observed-world-revision surface)
                 world-revision
                 (terminal-surface-state surface)
                 (if (terminal-surface-shape-current-p surface)
                     :current
                     :invalid))
           (incf (terminal-surface-reconciliations surface))))))
    (terminal-surface-state surface)))

(defun terminal-surface-current-p (surface &optional session)
  "True when SURFACE's last coherently published derived state is current."
  (eq :current (reconcile-terminal-surface surface session)))

(defun terminal-surface-axis-extent (surface direction)
  (let ((extent
          (voxel-space-cell-extent
           (block-world-space (terminal-surface-world surface)))))
    (+ (* (abs (voxel-direction-dx direction)) (vec3-x extent))
       (* (abs (voxel-direction-dy direction)) (vec3-y extent))
       (* (abs (voxel-direction-dz direction)) (vec3-z extent)))))

(defun terminal-surface-physical-width (surface)
  (let ((frame (terminal-face-frame (terminal-surface-face surface))))
    (* (terminal-surface-width surface)
       (terminal-surface-axis-extent
        surface (terminal-face-frame-right frame)))))

(defun terminal-surface-physical-height (surface)
  (let ((frame (terminal-face-frame (terminal-surface-face surface))))
    (* (terminal-surface-height surface)
       (terminal-surface-axis-extent
        surface (terminal-face-frame-up frame)))))

(defun terminal-surface-lower-left-point (surface &optional (offset 0.006))
  "Return the metric world point OFFSET cell-depths outside the lower left."
  (let* ((origin (terminal-surface-origin surface))
         (space (block-world-space (terminal-surface-world surface)))
         (cell-origin (world-coordinate-cell-origin space origin))
         (extent (voxel-space-cell-extent space))
         (frame (terminal-face-frame (terminal-surface-face surface)))
         (right (terminal-face-frame-right frame))
         (up (terminal-face-frame-up frame))
         (outward (terminal-face-frame-outward frame)))
    (labels ((component (point-reader direction-reader)
               (let ((base (funcall point-reader cell-origin))
                     (cell-size (funcall point-reader extent))
                     (normal (funcall direction-reader outward))
                     (horizontal (funcall direction-reader right))
                     (vertical (funcall direction-reader up)))
                 (+ base
                    (* cell-size
                       (+ (cond ((plusp normal) 1.0)
                                ((minusp normal) 0.0)
                                ((minusp horizontal) 1.0)
                                ((minusp vertical) 1.0)
                                (t 0.0))
                          (* normal offset)))))))
      (make-vec3
       (component #'vec3-x #'voxel-direction-dx)
       (component #'vec3-y #'voxel-direction-dy)
       (component #'vec3-z #'voxel-direction-dz)))))

(defun place-terminal-block-rectangle
    (world origin-x origin-y origin-z face width height
     &key (material *terminal-block*))
  "Place a WIDTH by HEIGHT terminal-material rectangle in resident WORLD.

ORIGIN is the lower-left block as seen from FACE's outward side."
  (check-type width (integer 1))
  (check-type height (integer 1))
  (let* ((origin (make-world-coordinate origin-x origin-y origin-z))
         (frame (terminal-face-frame face)))
    (with-world-change-transaction (world)
      (dotimes (row height)
        (dotimes (column width)
          (let ((coordinate
                  (terminal-coordinate-step
                   (terminal-coordinate-step
                    origin (terminal-face-frame-right frame) column)
                   (terminal-face-frame-up frame) row)))
            (edit-block-at material world
                           (world-coordinate-x coordinate)
                           (world-coordinate-y coordinate)
                           (world-coordinate-z coordinate)))))))
  world)

(defun fit-terminal-grid-in-surface
    (domain surface font-advance font-height margin font-scale)
  "Fit DOMAIN uniformly inside SURFACE and return scale, left, bottom, width,
and height in world units.  FONT-SCALE is an explicit multiplier on contain."
  (check-type margin (real 0))
  (check-type font-scale (real (0)))
  (let* ((surface-width (terminal-surface-physical-width surface))
         (surface-height (terminal-surface-physical-height surface))
         (available-width (- surface-width (* 2 margin)))
         (available-height (- surface-height (* 2 margin)))
         (columns (terminal-grid-domain-columns domain))
         (rows (terminal-grid-domain-rows domain)))
    (unless (and (plusp available-width) (plusp available-height))
      (error "Margin ~S leaves no room on a ~,3Fx~,3F terminal surface."
             margin surface-width surface-height))
    (let* ((scale
             (* font-scale
                (min (/ available-width (* columns font-advance))
                     (/ available-height (* rows font-height)))))
           (grid-width (* columns font-advance scale))
           (grid-height (* rows font-height scale)))
      (values scale
              (/ (- surface-width grid-width) 2.0)
              (/ (- surface-height grid-height) 2.0)
              grid-width grid-height))))

(defun terminal-display-glyph-occurrences
    (presentation glyph-cache font-pathname font-loader)
  (let ((glyphs-by-character (make-hash-table :test #'eql))
        (occurrences nil)
        (domain (terminal-grid-presentation-domain presentation)))
    (labels ((glyphs-for (character)
               (multiple-value-bind (cached-glyphs present-p)
                   (gethash character glyphs-by-character)
                 (if present-p
                     cached-glyphs
                     (setf (gethash character glyphs-by-character)
                           (luv.slug:make-slug-glyph-placements
                            (luv.slug:cached-slug-shaped-text
                             glyph-cache font-pathname (string character))
                            font-loader glyph-cache font-pathname))))))
      (dotimes (row (terminal-grid-domain-rows domain))
        (dotimes (column (terminal-grid-domain-columns domain))
          (let ((character
                  (terminal-grid-character presentation column row)))
            (unless (char= character #\Space)
              (dolist (glyph (glyphs-for character))
                (push (make-terminal-glyph-occurrence
                       :glyph glyph :column column :row row)
                      occurrences))))))
      (nreverse occurrences))))

(defun make-terminal-display-glyph-instances
    (occurrences atlas domain surface font-loader margin font-scale)
  "Place glyphs in one font grid fitted across the entire block SURFACE."
  (let* ((frame (terminal-face-frame (terminal-surface-face surface)))
         (right (voxel-direction-vec3
                 (terminal-face-frame-right frame)))
         (up (voxel-direction-vec3 (terminal-face-frame-up frame)))
         (origin (terminal-surface-lower-left-point surface))
         (ascender (zpb-ttf:ascender font-loader))
         (descender (zpb-ttf:descender font-loader))
         (units-per-em (zpb-ttf:units/em font-loader))
         (font-height (/ (- ascender descender) units-per-em))
         (font-advance
           (/ (zpb-ttf:advance-width
               (zpb-ttf:find-glyph #\M font-loader))
              units-per-em))
         (padding 0.035)
         (data (make-array (* 24 (length occurrences))
                           :element-type 'single-float)))
    (multiple-value-bind (scale left bottom grid-width grid-height)
        (fit-terminal-grid-in-surface
         domain surface font-advance font-height margin font-scale)
      (declare (ignore grid-width))
      (labels ((difference (end start)
                 (make-vec3 (- (vec3-x end) (vec3-x start))
                            (- (vec3-y end) (vec3-y start))
                            (- (vec3-z end) (vec3-z start))))
               (write-values (offset values)
                 (loop for value in values
                       for index from offset
                       do (setf (aref data index)
                                (coerce value 'single-float)))))
        (loop for occurrence in occurrences
              for base from 0 by 24
              for glyph = (terminal-glyph-occurrence-glyph occurrence)
              for column = (terminal-glyph-occurrence-column occurrence)
              for row = (terminal-glyph-occurrence-row occurrence)
              for baseline =
                (+ bottom grid-height
                   (- (* (1+ row) font-height scale))
                   (* (- (/ descender units-per-em)) scale))
              for cell-x = (+ left (* column font-advance scale))
              for outline-left =
                (- (luv.slug:slug-glyph-placement-outline-min-x glyph) padding)
              for outline-bottom =
                (- (luv.slug:slug-glyph-placement-outline-min-y glyph) padding)
              for outline-right =
                (+ (luv.slug:slug-glyph-placement-outline-max-x glyph) padding)
              for outline-top =
                (+ (luv.slug:slug-glyph-placement-outline-max-y glyph) padding)
              for glyph-origin =
                (terminal-offset-point
                 origin
                 right
                 (+ cell-x
                    (* (luv.slug:slug-glyph-placement-origin-x glyph) scale))
                 up
                 (+ baseline
                    (* (luv.slug:slug-glyph-placement-origin-y glyph) scale)))
              for quad-origin =
                (terminal-offset-point
                 glyph-origin right (* outline-left scale)
                 up (* outline-bottom scale))
              for right-edge =
                (terminal-offset-point
                 glyph-origin right (* outline-right scale)
                 up (* outline-bottom scale))
              for top-edge =
                (terminal-offset-point
                 glyph-origin right (* outline-left scale)
                 up (* outline-top scale))
              for resource = (luv.slug:slug-glyph-placement-resource glyph)
              for serialized =
                (luv.slug:slug-device-glyph-serialized resource)
              for atlas-location =
                (gethash resource (luv.slug:slug-glyph-atlas-locations atlas))
              do (write-values
                  base
                  (list
                   (vec3-x quad-origin) (vec3-y quad-origin)
                   (vec3-z quad-origin)
                   (vec3-x (difference right-edge quad-origin))
                   (vec3-y (difference right-edge quad-origin))
                   (vec3-z (difference right-edge quad-origin))
                   (vec3-x (difference top-edge quad-origin))
                   (vec3-y (difference top-edge quad-origin))
                   (vec3-z (difference top-edge quad-origin))
                   outline-left outline-bottom
                   (luv.slug:slug-serialized-outline-horizontal-band-count
                    serialized)
                   outline-right outline-top
                   (luv.slug:slug-serialized-outline-vertical-band-count
                    serialized)
                   (first atlas-location) (second atlas-location) 0.0
                   (luv.slug:slug-glyph-placement-outline-min-x glyph)
                   (luv.slug:slug-glyph-placement-outline-min-y glyph) 0.0
                   (luv.slug:slug-glyph-placement-outline-max-x glyph)
                   (luv.slug:slug-glyph-placement-outline-max-y glyph) 0.0)))
        data))))

(defun make-terminal-display-glyph-run
    (session presentation surface glyph-cache font-pathname margin font-scale)
  (zpb-ttf:with-font-loader (font-loader font-pathname)
    (let* ((occurrences
             (terminal-display-glyph-occurrences
              presentation glyph-cache font-pathname font-loader))
           (glyphs (mapcar #'terminal-glyph-occurrence-glyph occurrences))
           (atlas (luv.slug:slug-glyph-atlas-for glyph-cache glyphs))
           (instances
             (make-terminal-display-glyph-instances
              occurrences atlas
              (terminal-grid-presentation-domain presentation)
              surface font-loader margin font-scale))
           (lower-left (terminal-surface-lower-left-point surface))
           (frame (terminal-face-frame (terminal-surface-face surface)))
           (center
             (terminal-offset-point
              lower-left
              (voxel-direction-vec3 (terminal-face-frame-right frame))
              (/ (terminal-surface-physical-width surface) 2.0)
              (voxel-direction-vec3 (terminal-face-frame-up frame))
              (/ (terminal-surface-physical-height surface) 2.0))))
      (make-world-text-run-from-instances
       (luvcraft-session-device session)
       (canvas-format (luvcraft-session-context session))
       "terminal display" font-pathname nil glyphs atlas center 1.0 instances
       :label "terminal surface Slug glyphs"))))

(defun terminal-display-frame-bind-group (display frame)
  (or (gethash frame (terminal-display-frame-bind-groups display))
      (let ((group
              (aref
               (make-world-text-frame-bind-groups
                (terminal-display-glyph-run display)
                (luvcraft-session-device (terminal-display-session display))
                (luvcraft-frame-uniform-buffer frame))
               0)))
        (setf (gethash frame
                       (terminal-display-frame-bind-groups display))
              group)
        group)))

(defmethod encode-luvcraft-overlay
    ((display terminal-display) session pass surface-texture)
  (when (terminal-surface-current-p
         (terminal-display-surface display) session)
    (let ((frame (luvcraft-frame-state session surface-texture))
          (glyph-run (terminal-display-glyph-run display)))
      (set-pipeline pass (world-text-run-native-pipeline glyph-run))
      (set-vertex-buffer pass 0 (world-text-run-vertex-buffer glyph-run))
      (set-vertex-buffer pass 1 (world-text-run-instance-buffer glyph-run))
      (set-bind-group pass 0
                      (terminal-display-frame-bind-group display frame))
      (draw pass 6 (length (world-text-run-glyphs glyph-run)))))
  display)

(defmethod release-luvcraft-overlay ((display terminal-display))
  (when (terminal-display-device display)
    (termdev:close-pty-device (terminal-display-device display))
    (setf (terminal-display-device display) nil))
  (maphash (lambda (frame group)
             (declare (ignore frame))
             (destroy group))
           (terminal-display-frame-bind-groups display))
  (clrhash (terminal-display-frame-bind-groups display))
  (release-world-text-run (terminal-display-glyph-run display))
  (luv.slug:release-slug-glyph-cache
   (terminal-display-glyph-cache display))
  (ghostty:close-terminal (terminal-display-terminal display))
  display)

(defmethod handle-luvcraft-focus-event
    ((display terminal-display) session canvas (event canvas-key-event))
  (declare (ignore session canvas))
  (let ((device (terminal-display-device display)))
    (when device
      (termdev:send-pty-device-canvas-key-event device event)))
  t)

(defmethod handle-luvcraft-focus-event
    ((display terminal-display) session canvas (event canvas-event))
  (declare (ignore display session canvas event))
  nil)

(defmethod luvcraft-focus-score ((display terminal-display) session)
  (multiple-value-bind (hit status) (luvcraft-session-target session)
    (declare (ignore status))
    (when hit
      (let ((surface (terminal-display-surface display))
            (coordinate (block-ray-hit-coordinate hit)))
        (when (loop for row below (terminal-surface-height surface)
                    thereis
                    (loop for column below (terminal-surface-width surface)
                          thereis
                          (equalp
                           coordinate
                           (terminal-surface-coordinate surface column row))))
          (block-ray-hit-distance hit))))))

(defun attach-terminal-display-pty (display &rest open-arguments)
  "Attach one owned PTY device to DISPLAY's existing Ghostty terminal."
  (check-type display terminal-display)
  (when (terminal-display-device display)
    (error "Terminal display ~S already has an input device." display))
  (setf (terminal-display-device display)
        (apply #'termdev:open-pty-device
               (terminal-display-terminal display) open-arguments))
  display)

(defun open-terminal-display
    (session x y z face
     &key (columns 80) (rows 24)
          (fixture (terminal-display-fixture))
          (material *terminal-block*)
          (margin 0.12)
          (font-scale 1.0)
          (font-pathname
            (cl-dejavu:font-pathname "DejaVuSansMono.ttf")))
  "Attach a deterministic Ghostty terminal to an authored block rectangle.

X,Y,Z names any block in the desired exposed FACE.  The maximal coplanar
terminal-material component must be rectangular.  Its native block meshes
remain the display body; this overlay contributes only one fitted Slug grid."
  (multiple-value-bind (surface status)
      (find-terminal-surface
       (luvcraft-session-world session) x y z face :material material)
    (unless surface
      (error "Cannot open terminal display at (~D ~D ~D) ~S: ~S."
             x y z face status))
    (let ((terminal nil)
          (glyph-cache nil)
          (glyph-run nil)
          (display nil)
          (completed-p nil))
      (unwind-protect
           (progn
             (setf terminal
                   (ghostty:make-terminal :columns columns :rows rows))
             (ghostty:write-terminal terminal fixture)
             (let* ((domain
                      (make-instance 'terminal-grid-domain
                                     :columns columns :rows rows))
                    (presentation
                      (make-terminal-grid-presentation
                       domain (ghostty:terminal-text terminal))))
               (setf glyph-cache
                     (luv.slug:make-slug-glyph-cache
                      (luvcraft-session-device session))
                     glyph-run
                     (make-terminal-display-glyph-run
                      session presentation surface glyph-cache font-pathname
                      margin font-scale)
                     display
                     (make-instance
                      'terminal-display
                      :session session :surface surface :terminal terminal
                      :presentation presentation :glyph-cache glyph-cache
                      :glyph-run glyph-run)))
             (add-luvcraft-overlay session display)
             (setf completed-p t)
             display)
        (unless completed-p
          (when glyph-run
            (ignore-errors (release-world-text-run glyph-run)))
          (when glyph-cache
            (ignore-errors (luv.slug:release-slug-glyph-cache glyph-cache)))
          (when terminal
            (ignore-errors (ghostty:close-terminal terminal))))))))
