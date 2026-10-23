(in-package #:luft.render)

;;; Immutable geometry, publication evidence, and the GPU residency transaction.
;;; A replacement changes one publication pointer. Frame bindings join it to
;;; current targets; publishing geometry never rebuilds those targets.

(defclass renderer-publication (gpu-resource-owner)
  ((mesh-slots :initarg :mesh-slots :reader renderer-publication-mesh-slots)
   (slot-order :initarg :slot-order :reader renderer-publication-slot-order)
   (torch-frame-data :initarg :torch-frame-data :reader renderer-publication-torch-frame-data)
   (flame-instance-count :initarg :flame-instance-count :reader renderer-publication-flame-instance-count)
   (flame-instance-buffer :initarg :flame-instance-buffer :reader renderer-publication-flame-instance-buffer)
   (scene-generation :initarg :scene-generation :reader renderer-publication-scene-generation))
  (:documentation "One immutable published residency identity. Own its global buffer;
mesh slots may be shared with the next publication and have separate custody."))

(defun %make-renderer-publication (mesh-slots order frames count buffer generation)
  (let ((publication (make-instance 'renderer-publication
                                    :mesh-slots mesh-slots :slot-order order
                                    :torch-frame-data frames :flame-instance-count count
                                    :flame-instance-buffer buffer :scene-generation generation)))
    (own-gpu-object publication buffer)
    publication))

(defun %make-empty-renderer-publication (&key flame-instance-buffer)
  (%make-renderer-publication (make-hash-table :test #'eql) nil
                              (make-array 0 :element-type 'single-float) 0 flame-instance-buffer nil))

(defun renderer-set-mesh (renderer key mesh &key scene-generation)
  "Make MESH resident under KEY, replacing any previous resident mesh."
  (cdar (renderer-set-meshes
         renderer (list (cons key mesh))
         :scene-generation scene-generation)))

(defun renderer-set-meshes (renderer meshes &key scene-generation)
  "Transactionally replace the keyed MESHES as one visible residency cohort.

MESHES is an alist of key to surface mesh. Every GPU slot is created before
the renderer's table changes; a failed upload therefore leaves the installed
cohort untouched. No frame can interleave with the owner-thread publication."
  (renderer-update-meshes
   renderer meshes nil :scene-generation scene-generation))

(zdefun (renderer-update-meshes :zone :luft/publish-residency)
    (renderer meshes removed-keys &key scene-generation)
  "Transactionally replace geometry and its realized torch-frame cohort."
  (validate-renderer-mesh-update meshes removed-keys)
  (when scene-generation
    (check-type scene-generation scene-mesh-generation))
  (let ((prepared-meshes nil)
        (semantic-entries nil)
        (publication-scene-generation nil)
        (candidates nil)
        (requested-candidates nil)
        (old-publication (renderer-publication renderer))
        (staged-publication nil)
        (publication-changed-p
          (or meshes
              (some (lambda (key)
                      (nth-value
                       1 (gethash key (renderer-mesh-slots renderer))))
                    removed-keys)))
        (torch-frame-data nil)
        (flame-count 0)
        (flame-buffer nil)
        (installed-p nil))
    (unless publication-changed-p
      (return-from renderer-update-meshes nil))
    (unwind-protect
         (progn
           ;; CPU canonicalization finishes before candidate GPU allocation.
           (setf prepared-meshes
                 (prepare-renderer-mesh-candidates meshes))
           (when scene-generation
             (setf semantic-entries
                   (validate-renderer-scene-generation-cohort
                    renderer prepared-meshes removed-keys scene-generation)
                   publication-scene-generation
                   (make-renderer-publication-scene-generation
                    scene-generation semantic-entries)))
           (dolist (entry prepared-meshes)
             (push (cons (car entry)
                         (%make-renderer-mesh-slot
                          renderer (cdr entry)
                          (and scene-generation
                               (third
                                (find (car entry) semantic-entries
                                      :key #'first :test #'eql)))))
                   candidates))
           (setf candidates (nreverse candidates))
           (setf requested-candidates
                 (mapcar
                  (lambda (entry)
                    (cons (car entry)
                          (cdr (assoc (car entry) candidates :test #'eql))))
                  prepared-meshes))
           (let* ((entries
                    (prospective-renderer-slot-entries
                     renderer candidates removed-keys))
                  (table (renderer-slot-table entries))
                  (order (mapcar #'car entries)))
             ;; Every fallible GPU and CPU staging operation finishes before
             ;; any part of this generation becomes visible.
             (multiple-value-setq
                 (torch-frame-data flame-count flame-buffer)
               (%make-renderer-flame-resources
                renderer (mesh-slots-torch-frame-data entries)))
             (setf staged-publication
                   (%make-renderer-publication
                    table order torch-frame-data flame-count flame-buffer publication-scene-generation))

             (when *renderer-publication-precommit-hook*
               (funcall *renderer-publication-precommit-hook*
                        renderer staged-publication))
             ;; The frame binds the current publication and target together.
             ;; Residency has one publication pointer; it never replaces targets.
             (setf (renderer-publication renderer) staged-publication installed-p t)
             (with-release-warnings
               (releasing :superseded-frame-bindings
                 (clear-renderer-frame-bind-groups renderer)))
             ;; Residency changes invalidate the previous color/depth/motion
             ;; correspondence.  The next temporal encode must reset rather
             ;; than blend arrivals with history in which they did not exist,
             ;; or retain departed silhouettes as ghosts.
             (setf (renderer-history-valid-p renderer) nil)
             (dolist (slot
                       (renderer-publication-retired-slots
                        old-publication table))
               (with-release-warnings
                 (releasing :retired-mesh-slot (retire-gpu-object renderer slot))))

             (with-release-warnings
               (releasing :retired-publication (retire-gpu-object renderer old-publication))))
           requested-candidates)
      (unless installed-p

        (when staged-publication
          (with-release-warnings
            (releasing :unpublished-residency (retire-gpu-object renderer staged-publication))))
        ;; A condition may occur after GPU staging but before the publication
        ;; record itself is allocated.  Those locals still own the resources.
        (unless staged-publication

          (when flame-buffer (ignore-errors (destroy flame-buffer))))
        (dolist (entry candidates)
          (with-release-warnings
            (releasing :unpublished-mesh-slot (retire-gpu-object renderer (cdr entry)))))))))

(defun renderer-remove-mesh (renderer key)
  (renderer-update-meshes renderer nil (list key))
  (values))

(defun renderer-clear-meshes (renderer)
  (renderer-update-meshes renderer nil (copy-list (renderer-slot-order renderer)))
  (values))

;;; Staging populations (render-population.lisp) into GPU buffers.

(defclass resident-population (gpu-resource-owner)
  ((population :initarg :population :reader resident-population-population)
   (instance-buffer :accessor resident-population-instance-buffer)
   (appearance-buffer :accessor resident-population-appearance-buffer)
   (light-buffer :accessor resident-population-light-buffer)
   (descriptor-buffer :accessor resident-population-descriptor-buffer)
   ;; (STAR FIRST COUNT) for each star present, over the star-sorted sites.
   (star-runs :initform nil :accessor resident-population-star-runs)
   ;; World-cell box (MIN-X MIN-Y MIN-Z MAX-X MAX-Y MAX-Z) around the drawn
   ;; geometry, or NIL when the population draws nothing.
   (bounds :initform nil :accessor resident-population-bounds)
   ;; Fixture bodies, sorted by code, and their (CODE FIRST COUNT) runs.
   (body-buffer :initform nil :accessor resident-population-body-buffer)
   (body-runs :initform nil :accessor resident-population-body-runs)))

(defstruct (renderer-slot-provenance
             (:constructor %make-renderer-slot-provenance
                 (key scene request-stamp result-stamp light-generation tree))
             (:copier nil))
  "Narrow immutable evidence for one exact installed semantic mesh tree."
  (key nil :read-only t)
  (scene nil :type scene :read-only t)
  (request-stamp nil :read-only t)
  (result-stamp nil :read-only t)
  (light-generation nil :type realized-light-generation :read-only t)
  (tree nil :type surface-mesh-tree-manifest :read-only t))

(defclass mesh-slot (gpu-resource-owner)
  ((mesh :initarg :mesh :reader mesh-slot-mesh)
   (provenance :initarg :provenance :reader mesh-slot-provenance)
   (resident :accessor mesh-slot-resident)
   (lattice-point-buffer :initform nil :accessor mesh-slot-lattice-point-buffer)
   (lattice-point-count :initform 0 :accessor mesh-slot-lattice-point-count)))

(defun %copy-torch-frame-data (source)
  "Validate and copy packed three-Vec4 frames across an ownership boundary."
  (check-type source vector)
  (unless (zerop (mod (length source) +torch-flame-instance-scalar-count+))
    (error "A ~D-scalar flame stream is not an integral frame population."
           (length source)))
  (let ((copy (map '(simple-array single-float (*))
                   (lambda (scalar)
                     (check-type scalar single-float)
                     scalar)
                   source)))
    (loop for offset from 0 below (length copy)
            by +torch-flame-instance-scalar-count+
          do (validate-torch-flame-frame copy offset))
    copy))

(defun surface-mesh-torch-frame-data (mesh)
  "Flatten MESH's sparse attachment frames, including companion owners."
  (check-type mesh luft:surface-mesh)
  (let ((frames nil))
    (labels ((visit (surface)
               (dolist (frame (luft:surface-mesh-attachments surface))
                 (push frame frames))
               (dolist (companion (luft:surface-mesh-companions surface))
                 (visit companion))))
      (visit mesh))
    (let* ((frames (nreverse frames))
           (data
             (make-array (* +torch-flame-instance-scalar-count+ (length frames))
                         :element-type 'single-float)))
      (loop for frame in frames
            for offset from 0 by +torch-flame-instance-scalar-count+
            do (validate-torch-flame-frame frame)
               (replace data frame :start1 offset))
      data)))

(defun mesh-slots-torch-frame-data (entries)
  "Flatten attachment frames from sorted key-to-MESH-SLOT ENTRIES."
  (let* ((runs
           (mapcar (lambda (entry)
                     (surface-mesh-torch-frame-data
                      (mesh-slot-mesh (cdr entry))))
                   entries))
         (length (reduce #'+ runs :key #'length :initial-value 0))
         (data (make-array length :element-type 'single-float)))
    (loop with offset = 0
          for run in runs
          do (replace data run :start1 offset)
             (incf offset (length run)))
    data))

(defun renderer-mesh-slots (renderer)
  (renderer-publication-mesh-slots (renderer-publication renderer)))

(defun renderer-slot-order (renderer)
  (renderer-publication-slot-order (renderer-publication renderer)))

(defun renderer-torch-frame-data (renderer)
  (renderer-publication-torch-frame-data (renderer-publication renderer)))

(defun renderer-flame-instance-count (renderer)
  (renderer-publication-flame-instance-count
   (renderer-publication renderer)))

(defun renderer-flame-instance-buffer (renderer)
  (renderer-publication-flame-instance-buffer
   (renderer-publication renderer)))

(defun %make-renderer-flame-resources (renderer source)
  "Build, but do not publish, one validated and immutable torch-frame buffer."
  (let* ((data (%copy-torch-frame-data source))
         (buffer (create (renderer-device renderer)
                         (make-buffer-descriptor :label "luft global torch flame instances"
                                                 :size (max 16 (* 4 (length data)))
                                                 :usage '(:storage :copy-dst))))
         (completed-p nil))
    (unwind-protect
         (progn
           (when (plusp (length data)) (write-buffer buffer data))
           (setf completed-p t)
           (values data (/ (length data) +torch-flame-instance-scalar-count+) buffer))
      (unless completed-p
        (with-release-warnings (releasing :torch-upload (destroy buffer)))))))

(defun mesh-slot-prepared-mesh (slot)
  "Borrow SLOT's immutable CPU realization for renderer reconstruction."
  (%make-prepared-render-mesh
   (mesh-slot-mesh slot)
   (resident-population-population (mesh-slot-resident slot))))

(defun %make-renderer-mesh-slot (renderer mesh-or-prepared provenance)
  "Upload a slot privately; construction markers remain lazy."
  (let* ((prepared (if (typep mesh-or-prepared 'prepared-render-mesh)
                       mesh-or-prepared (prepare-render-mesh mesh-or-prepared)))
         (slot (make-instance 'mesh-slot :mesh (prepared-render-mesh-mesh prepared)
                                        :provenance provenance)))
    (with-gpu-construction (slot)
      (setf (mesh-slot-resident slot)
            (own-gpu-object slot (%upload-render-population
                                  renderer (prepared-render-mesh-population prepared)))))))

(defun site-bounds (instance-words)
  "The world-cell box holding every star drawn at INSTANCE-WORDS' sites.

A site sits at a lattice vertex and its star's geometry stays within one
cell of it on every axis, so the box is the sites' extent grown by one."
  (declare (type (simple-array (unsigned-byte 32) (*)) instance-words))
  (when (plusp (length instance-words))
    (let ((low (make-list 3 :initial-element most-positive-fixnum))
          (high (make-list 3 :initial-element most-negative-fixnum)))
      (loop for site from 0 below (length instance-words) by 4
            do (dotimes (axis 3)
                 (let ((value (aref instance-words (+ site axis))))
                   (setf (nth axis low) (min (nth axis low) value)
                         (nth axis high) (max (nth axis high) value)))))
      (map '(simple-array single-float (6))
           (lambda (value) (coerce value 'single-float))
           (append (mapcar #'1- low) (mapcar #'1+ high))))))

(defun sort-sites-by-star
    (instance-words appearance-words
     &optional (light-words (make-array (length instance-words)
                                        :element-type '(unsigned-byte 32)
                                        :initial-element 0)))
  "Return INSTANCE-WORDS and APPEARANCE-WORDS reordered by star, the runs,
and LIGHT-WORDS reordered with them.

Each site is four instance words ending in its star, two appearance words,
and four light words; the arrays keep their one-for-one order.  The runs are
(STAR FIRST COUNT) for every star present, the instances one draw covers."
  (declare (type (simple-array (unsigned-byte 32) (*))
                 instance-words appearance-words light-words)
           (optimize (speed 3) (safety 1)))
  (let* ((sites (floor (length instance-words) 4))
         (counts (make-array 256 :element-type 'fixnum :initial-element 0))
         (starts (make-array 256 :element-type 'fixnum :initial-element 0))
         (sorted-instances (make-array (length instance-words)
                                       :element-type '(unsigned-byte 32)))
         (sorted-appearances (make-array (length appearance-words)
                                         :element-type '(unsigned-byte 32)))
         (sorted-lights (make-array (length light-words)
                                    :element-type '(unsigned-byte 32))))
    (unless (= (length light-words) (length instance-words))
      (error "~D light words do not have four per site (~D instance words)."
             (length light-words) (length instance-words)))
    (flet ((star (site) (logand #xff (aref instance-words (+ 3 (* 4 site))))))
      (dotimes (site sites) (incf (aref counts (star site))))
      (loop with next fixnum = 0
            for star below 256
            do (setf (aref starts star) next)
               (incf next (aref counts star)))
      (let ((runs (loop for star below 256
                        when (plusp (aref counts star))
                          collect (list star (aref starts star) (aref counts star)))))
        (dotimes (site sites)
          (let ((slot (aref starts (star site))))
            (incf (aref starts (star site)))
            (replace sorted-instances instance-words
                     :start1 (* 4 slot) :start2 (* 4 site) :end2 (* 4 (1+ site)))
            (replace sorted-appearances appearance-words
                     :start1 (* 2 slot) :start2 (* 2 site) :end2 (* 2 (1+ site)))
            (replace sorted-lights light-words
                     :start1 (* 4 slot) :start2 (* 4 site) :end2 (* 4 (1+ site)))))
        (values sorted-instances sorted-appearances runs sorted-lights)))))

(zdefun (%upload-render-population :zone :luft/upload-slot) (renderer population)
  "Own each buffer before uploading, so even a failed write rolls back."
  (let ((resident (make-instance 'resident-population :population population)))
    (with-gpu-construction (resident)
      (flet ((upload (label words)
               (let ((buffer (own-gpu-resource
                              resident (renderer-device renderer)
                              (make-buffer-descriptor :label label :size (max 16 (* 4 (length words)))
                                                      :usage '(:storage :copy-dst)))))
                 (when (plusp (length words)) (write-buffer buffer words))
                 buffer)))
        ;; One indexed draw per star covers a contiguous run of sites.
        (multiple-value-bind (instances appearances runs lights)
            (sort-sites-by-star (render-population-instance-words population)
                                (render-population-appearance-words population)
                                (render-population-light-words population))
          (setf (resident-population-instance-buffer resident)
                (upload "luft resident site instances" instances)
                (resident-population-appearance-buffer resident)
                (upload "luft active-star appearance sidecars" appearances)
                (resident-population-light-buffer resident)
                (upload "luft active-star lattice light" lights)
                (resident-population-descriptor-buffer resident)
                (upload "luft terrain material descriptors"
                        (render-population-descriptor-words population))
                (resident-population-star-runs resident) runs
                (resident-population-bounds resident) (site-bounds instances)))
        (let ((bodies (render-population-body-data population)))
          (when (plusp (length bodies))
            (setf (resident-population-body-buffer resident)
                  (upload "luft resident fixture bodies" bodies)
                  (resident-population-body-runs resident) (fixture-body-runs bodies)
                  (resident-population-bounds resident)
                  (fixture-body-bounds bodies (resident-population-bounds resident)))))))))

(defun prospective-renderer-slot-entries (renderer candidates removed-keys)
  "Return the sorted slot population after applying one candidate cohort."
  (let ((entries nil))
    (loop for key being the hash-keys of (renderer-mesh-slots renderer)
          for slot = (gethash key (renderer-mesh-slots renderer))
          unless (or (assoc key candidates :test #'eql)
                     (member key removed-keys :test #'eql))
            do (push (cons key slot) entries))
    (dolist (entry candidates)
      (push entry entries))
    (sort entries #'< :key #'car)))

(defun renderer-slot-table (entries)
  "Materialize sorted key-to-slot ENTRIES into a fresh publication table."
  (let ((table (make-hash-table :test #'eql :size (length entries))))
    (dolist (entry entries table)
      (setf (gethash (car entry) table) (cdr entry)))))

(defun validate-renderer-mesh-update (meshes removed-keys)
  "Validate one keyed residency transaction before any GPU allocation."
  (check-type meshes list)
  (check-type removed-keys list)
  (let ((candidate-keys (make-hash-table :test #'eql))
        (removal-keys (make-hash-table :test #'eql)))
    (dolist (entry meshes)
      (unless (consp entry)
        (error "A renderer mesh cohort entry must be (KEY . MESH), not ~S."
               entry))
      (let ((key (car entry))
            (mesh (cdr entry)))
        (check-type key luft:chunk-key)
        (unless (or (typep mesh 'luft:surface-mesh)
                    (typep mesh 'prepared-render-mesh))
          (error "Renderer owner ~D has invalid mesh value ~S." key mesh))
        (when (nth-value 1 (gethash key candidate-keys))
          (error "Renderer mesh cohort repeats owner ~D." key))
        (setf (gethash key candidate-keys) t)))
    (dolist (key removed-keys)
      (check-type key luft:chunk-key)
      (when (nth-value 1 (gethash key removal-keys))
        (error "Renderer removal cohort repeats owner ~D." key))
      (when (nth-value 1 (gethash key candidate-keys))
        (error "Renderer owner ~D cannot be replaced and removed together."
               key))
      (setf (gethash key removal-keys) t)))
  (values))

(defun prepare-renderer-mesh-candidates (meshes)
  "Prepare MESHES into immutable CPU populations without allocating on the GPU."
  (loop for (key . mesh) in meshes
        for prepared = (if (typep mesh 'prepared-render-mesh)
                           mesh
                           (prepare-render-mesh mesh))
        collect (cons key prepared)))

(defun surface-mesh-tree-uses-light-field-p (mesh field)
  "Whether MESH and every companion retain exact immutable FIELD."
  (and (eq field (luft:surface-mesh-voxel-light mesh))
       (every (lambda (companion)
                (surface-mesh-tree-uses-light-field-p companion field))
              (luft:surface-mesh-companions mesh))))

(defun make-renderer-slot-provenance (key generation manifest-index)
  "Project GENERATION's matched output into one nonrecursive slot witness."
  (let* ((manifest
           (scene-mesh-generation-mesh-manifest generation))
         (entry (aref manifest manifest-index))
         (merged
           (scene-mesh-generation-slot-provenances generation)))
    (if (plusp (length merged))
        (let ((provenance (aref merged manifest-index)))
          (unless (and (eql key (renderer-slot-provenance-key provenance))
                       (eq (scene-mesh-generation-scene generation)
                           (renderer-slot-provenance-scene provenance)))
            (error "Merged scene-generation provenance does not name renderer owner ~S."
                   key))
          provenance)
        (%make-renderer-slot-provenance
         key (scene-mesh-generation-scene generation)
         (copy-scene-generation-stamp-value
          (scene-mesh-generation-request-stamp generation))
         (copy-scene-generation-stamp-value
          (scene-mesh-generation-result-stamp generation))
         (scene-mesh-generation-light-generation generation)
         (scene-mesh-output-manifest-entry-tree entry)))))

(defun consume-renderer-generation-manifest
    (prepared-meshes generation)
  "Match every candidate key/tree one-to-one against GENERATION's manifest."
  (let* ((manifest (scene-mesh-generation-mesh-manifest generation))
         (used (make-array (length manifest) :element-type 'bit
                                             :initial-element 0)))
    (unless (= (length prepared-meshes) (length manifest))
      (error "Renderer candidate count ~D does not match generation output count ~D."
             (length prepared-meshes) (length manifest)))
    (loop for (key . prepared) in prepared-meshes
          for mesh = (prepared-render-mesh-mesh prepared)
          for index =
            (loop for index below (length manifest)
                  for entry = (aref manifest index)
                  when (and (zerop (aref used index))
                            (or (not
                                 (scene-mesh-output-manifest-entry-keyed-p
                                  entry))
                                (eql key
                                     (scene-mesh-output-manifest-entry-key
                                      entry)))
                            (surface-mesh-tree-manifest-matches-p
                             (scene-mesh-output-manifest-entry-tree entry)
                             mesh))
                    return index)
          unless index
            do (error "Renderer owner ~S is not the exact mesh tree named by its scene generation."
                      key)
          do (setf (aref used index) 1)
          collect (cons key
                        (make-renderer-slot-provenance
                         key generation index)))))

(defun prospective-renderer-semantic-entries
    (renderer prepared-meshes removed-keys candidate-provenances)
  "Return sorted (KEY MESH PROVENANCE) tuples before any GPU allocation."
  (let ((entries nil))
    (loop for key being the hash-keys of (renderer-mesh-slots renderer)
          using (hash-value slot)
          unless (or (assoc key prepared-meshes :test #'eql)
                     (member key removed-keys :test #'eql))
            do (push (list key (mesh-slot-mesh slot)
                           (and
                            (renderer-publication-scene-generation
                             (renderer-publication renderer))
                            (mesh-slot-provenance slot)))
                     entries))
    (dolist (entry prepared-meshes)
      (push
       (list (car entry)
             (prepared-render-mesh-mesh (cdr entry))
             (cdr (assoc (car entry) candidate-provenances :test #'eql)))
       entries))
    (sort entries #'< :key #'car)))

(defun scene-generation-exactly-manifests-semantic-entries-p
    (generation entries)
  "Whether a previously merged GENERATION already names all keyed ENTRIES."
  (let* ((manifest (scene-mesh-generation-mesh-manifest generation))
         (provenances (scene-mesh-generation-slot-provenances generation))
         (used (make-array (length manifest) :element-type 'bit
                                             :initial-element 0)))
    (and (= (length entries) (length manifest) (length provenances))
         (every
          (lambda (semantic)
            (let ((key (first semantic))
                  (mesh (second semantic))
                  (provenance (third semantic)))
              (loop for index below (length manifest)
                    for entry = (aref manifest index)
                    when (and (zerop (aref used index))
                              (scene-mesh-output-manifest-entry-keyed-p entry)
                              (eql key
                                   (scene-mesh-output-manifest-entry-key entry))
                              (eq provenance (aref provenances index))
                              (surface-mesh-tree-manifest-matches-p
                               (scene-mesh-output-manifest-entry-tree entry)
                               mesh))
                      do (setf (aref used index) 1)
                         (return t)
                    finally (return nil))))
          entries))))

(defun make-renderer-publication-scene-generation (generation entries)
  "Merge the current transaction stamp with exact prospective slot lineage."
  (when generation
    (if (scene-generation-exactly-manifests-semantic-entries-p
         generation entries)
        generation
        (make-scene-mesh-generation-value
         (scene-mesh-generation-scene generation)
         (scene-mesh-generation-request-stamp generation)
         (scene-mesh-generation-light-generation generation)
         :mesh-entries
         (mapcar (lambda (entry) (cons (first entry) (second entry))) entries)
         :slot-provenances (map 'vector #'third entries)))))

(defun validate-renderer-scene-generation-cohort
    (renderer prepared-meshes removed-keys generation)
  "Validate and return prospective semantic entries before GPU allocation."
  (when generation
    (let* ((light-generation
             (scene-mesh-generation-light-generation generation))
           (field (realized-light-generation-field light-generation))
           (candidate-provenances
             (consume-renderer-generation-manifest
              prepared-meshes generation))
           (entries
             (prospective-renderer-semantic-entries
              renderer prepared-meshes removed-keys candidate-provenances)))
      (dolist (entry entries)
        (destructuring-bind (key mesh provenance) entry
          (unless provenance
            (error "Retained renderer owner ~S has no exact scene-generation provenance."
                   key))
          (unless (and
                   (eq (scene-mesh-generation-scene generation)
                       (renderer-slot-provenance-scene provenance))
                   ;; Regional light (#BQMYNU) relights owners by tile, so a
                   ;; retained owner keeps the generation it was lit with and
                   ;; must still carry that generation's exact field.
                   (if (scene-regional-light-p
                        (scene-mesh-generation-scene generation))
                       (surface-mesh-tree-uses-light-field-p
                        mesh
                        (realized-light-generation-field
                         (renderer-slot-provenance-light-generation provenance)))
                       (and (eq light-generation
                                (renderer-slot-provenance-light-generation
                                 provenance))
                            (surface-mesh-tree-uses-light-field-p mesh field)))
                   (surface-mesh-tree-manifest-matches-p
                    (renderer-slot-provenance-tree provenance) mesh))
            (error "Renderer owner ~S does not retain its exact claimed mesh/light generation."
                   key))))
      entries)))

(defvar *renderer-publication-precommit-hook* nil
  "Optional test instrumentation run after complete staging, before commit.")

(defun renderer-publication-retired-slots (old-publication new-table)
  (let ((retired nil))
    (loop for key being the hash-keys of
          (renderer-publication-mesh-slots old-publication)
          using (hash-value old-slot)
          unless (eq old-slot (gethash key new-table))
            do (push old-slot retired))
    retired))


;;; Publishing a ready streaming replacement is the renderer's half of the
;;; owner/worker handoff in streaming-publication.lisp.

(zdefun (publish-ready-streaming-scene :zone :luft/publish-ready-region)
    (scene renderer)
  "Install a whole replacement, then make its light and CPU cache reusable."
  (multiple-value-bind (meshes generation ready-p)
      (ready-streaming-scene-meshes scene)
    (when ready-p
      (let ((replacement (streaming-scene-replacement scene)))
        (renderer-update-meshes
         renderer meshes (streaming-replacement-removals replacement)
         :scene-generation generation)
        ;; Only a successful GPU publication advances these owner-thread values.
        (setf (streaming-scene-light-generation scene)
              (scene-mesh-generation-light-generation generation))
        (when (streaming-replacement-request replacement)
          (setf (streaming-scene-mesh-cache scene)
                (streaming-mesh-result-mesh-cache
                 (streaming-replacement-result replacement))))
        (install-streaming-light-tiles
         scene (streaming-mesh-result-light-tiles
                (streaming-replacement-result replacement)))
        (setf (streaming-scene-replacement scene) nil)
        (length meshes)))))

(zdefun (drain-streaming-scene-production :zone :luft/drain-production)
    (scene renderer production-system &key (limit 2))
  "Drain bounded worker results, then try the pending renderer publication."
  (loop repeat limit
        do (multiple-value-bind (result present-p)
               (production:receive-production-result-no-hang production-system)
             (unless present-p (return))
             (etypecase (production:production-result-request result)
               (authored-chunk-load-request
                (receive-streaming-source-result scene result))
               (streaming-mesh-request
                (receive-streaming-mesh-result scene result)))))
  (publish-ready-streaming-scene scene renderer))
