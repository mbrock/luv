;;;; ./env --slim sbcl --script scripts/profile-light.lisp > build/light-profile.txt
;;;; Materialization is excluded. CPU and allocation profiles are separate runs.
;;;; LIGHT_NO_PROFILE=1 retains timings/counts/exact checks but skips sampling.
;;;; For the original control, extract these files with git show 1b9c3e0:<path>:
;;;;   luft/render/render-population.lisp -> build/light-reference.lisp
;;;;   luft/terrain/streamed-light.lisp -> build/light-reference-terrain.lisp
;;;; LIGHT_REFERENCE=build/light-reference.lisp checks the new output exactly.
;;;; Also set LIGHT_USE_REFERENCE=1 and
;;;; LIGHT_REFERENCE_TERRAIN=build/light-reference-terrain.lisp to time the old code.
(require :asdf)
(require :sb-sprof)
(asdf:load-system "luvcraft/luft")
(asdf:load-system "luft/renderer")
(defpackage #:profile-light
  (:use #:cl)
  (:local-nicknames (#:terrain #:luft.terrain) (#:render #:luft.render)
                    (#:domains #:luv.domains)))
(in-package #:profile-light)

(defvar *reference* nil)
(when (uiop:getenv "LIGHT_REFERENCE")
  (load (uiop:getenv "LIGHT_REFERENCE"))
  (setf *reference* (symbol-function 'render::star-lattice-light-words))
  (if (uiop:getenv "LIGHT_USE_REFERENCE")
      (when (uiop:getenv "LIGHT_REFERENCE_TERRAIN")
        (load (uiop:getenv "LIGHT_REFERENCE_TERRAIN")))
      (load (compile-file "luft/render/render-population.lisp"
                          :output-file "build/profile-render-population.fasl"))))

(defun check-words (meshes)
  (when *reference*
    (dolist (entry meshes)
      (assert (equalp (funcall *reference* (cdr entry))
                      (render::star-lattice-light-words (cdr entry)))))
    (format t "~&EXACT ~D owner sidecars match reference~%" (length meshes))))

(defun field-counts (meshes)
  (let* ((field (luft:surface-mesh-voxel-light (cdar meshes)))
         (sky (luft:voxel-light-field-skylight field)))
    (dolist (entry (list (cons :rgb field) (cons :sky sky)))
      (let ((f (cdr entry)))
        (format t "~&COUNTS ~A visits=~D pushes=~D stale=~D pages=~D~%"
                (car entry) (luft:voxel-light-field-visits f)
                (luft:voxel-light-field-pushes f) (luft::voxel-light-field-stale-pops f)
                (luft:voxel-light-field-page-count f))))))

(defun cohort (scene owners)
  (multiple-value-bind (meshes census diagnostics generation products tiles)
      (terrain::mesh-streaming-snapshot
       (terrain::make-streaming-region-snapshot scene owners 1) :prepare-products-p t)
    (declare (ignore census diagnostics generation))
    (setf (terrain::streaming-scene-mesh-cache scene) products)
    (terrain:install-streaming-light-tiles scene tiles)
    meshes))

(defun check-products (scene)
  (when *reference*
    (dolist (product (terrain::streaming-scene-mesh-cache scene))
      (assert
       (equalp (funcall *reference* (terrain::streaming-star-product-mesh product))
               (render::render-population-light-words
                (render::prepared-render-mesh-population
                 (terrain::streaming-star-product-prepared product))))))
    (format t "~&EXACT prepared cached products match reference~%")))

(defun solver-counts (name thunk)
  "Instrumentation runs separately, never inside the timed or sampled runs."
  (let ((solve (symbol-function 'luft:solve-voxel-light))
        (opacity (symbol-function 'luft::%material-opacity))
        (fields nil) (reads 0))
    (unwind-protect
         (progn
           (setf (symbol-function 'luft:solve-voxel-light)
                 (lambda (&rest args)
                   (let ((field (apply solve args))) (push field fields) field))
                 (symbol-function 'luft::%material-opacity)
                 (lambda (&rest args) (incf reads) (apply opacity args)))
           (funcall thunk))
      (setf (symbol-function 'luft:solve-voxel-light) solve
            (symbol-function 'luft::%material-opacity) opacity))
    (format t "~&SOLVER ~A solves=~D opacity-reads=~D visits=~D pushes=~D stale=~D allocated-pages=~D~%"
            name (length fields) reads
            (reduce #'+ fields :key #'luft:voxel-light-field-visits)
            (reduce #'+ fields :key #'luft:voxel-light-field-pushes)
            (reduce #'+ fields :key #'luft::voxel-light-field-stale-pops)
            (reduce #'+ fields :key #'luft:voxel-light-field-page-count))))

(defun measure (name thunk &optional (runs 10))
  (sb-ext:gc :full t)
  (let ((start (get-internal-real-time))
        (bytes (sb-ext:get-bytes-consed)) (gc sb-ext:*gc-run-time*))
    (dotimes (i runs) (funcall thunk))
    (format t "~&MEASURE ~A runs=~D ms=~,3F bytes=~,1F gc-ms=~,3F~%"
            name runs
            (/ (* 1000.0 (- (get-internal-real-time) start))
               internal-time-units-per-second runs)
            (/ (- (sb-ext:get-bytes-consed) bytes) runs)
            (/ (* 1000.0 (- sb-ext:*gc-run-time* gc))
               internal-time-units-per-second runs))))

(defun packing-batch-counts (name thunk)
  "Count batch fills outside the timed and sampled runs."
  (let ((original (symbol-function 'render::%fill-light-corner-tile))
        (fills 0))
    (unwind-protect
         (progn
           (setf (symbol-function 'render::%fill-light-corner-tile)
                 (lambda (&rest args) (incf fills) (apply original args)))
           (funcall thunk))
      (setf (symbol-function 'render::%fill-light-corner-tile) original))
    (format t "~&BATCH ~A tiles=~D cell-loads=~D corners-computed=~D page-slots=~D sky-columns=~D~%"
            name fills (* 729 fills) (* 512 fills) (* 16 fills) (* 81 fills))))

(defun profile-case (name thunk)
  (measure name thunk)
  (packing-batch-counts name thunk)
  (when (uiop:getenv "LIGHT_NO_PROFILE") (return-from profile-case))
  (dolist (mode '(:cpu :alloc))
    (format t "~&PROFILE ~A ~A~%" name mode)
    (sb-ext:gc :full t)
    (sb-sprof:with-profiling (:mode mode :max-samples 20000 :sample-interval 0.001 :report nil)
      (dotimes (i 48) (funcall thunk)))
    (sb-sprof:report :type :flat :max 50)
    (sb-sprof:report :type :graph :max 70)))

(let* ((source (luvcraft.luft:make-luvcraft-world-source :seed 7))
       (domain (terrain:world-source-domain source))
       ;; The same supported cave lantern as capture-frontier-light.lisp.
       (landing
         (let ((edits (terrain::world-source-edits source))
               (planks (luvcraft.luft::block-placement luvcraft::*planks-block*)))
           (loop for x from 1000 to 1004 do
             (loop for y from 1082 to 1088 do
               (setf (gethash (luft:make-site domain x y 7 luft:+cell-extent+ 1) edits) planks)
               (loop for z from 8 to 10 do
                 (setf (gethash (luft:make-site domain x y z luft:+cell-extent+ 1) edits) nil))))
           (setf (gethash (luft:make-site domain 1002 1086 8 luft:+cell-extent+ 1) edits)
                 (luvcraft.luft::block-placement luvcraft::*lantern-block*))))
       (scene (terrain:make-world-source-streaming-scene
               source :mesh-preparer render:+render-population-preparer+))
       (spawn (multiple-value-list (terrain:world-source-spawn source)))
       (key (luft:chunk-key-at (first spawn) (second spawn)))
       (sources (terrain::streaming-scene-dependency-guard-keys scene (list key)))
       (owners (terrain::streaming-scene-canonical-owner-closure scene sources))
       (tiles (terrain::streaming-scene-dependency-guard-keys scene owners))
       (captured (terrain::streaming-scene-dependency-guard-keys scene tiles)))
  (declare (ignore landing))
  (dolist (key captured)
    (setf (gethash key (terrain:streaming-scene-store scene))
          (terrain:materialize-world-source-chunk
           source key 1 :edits (terrain::capture-world-source-chunk-edits source key))))
  (terrain::rebuild-authored-world-resident-values scene captured)
  (dolist (key sources) (setf (gethash key (terrain:streaming-scene-loaded scene)) 1))
  (let* ((meshes (cohort scene owners))
         (stars (loop for entry in meshes sum
                  (/ (length (luft:surface-mesh-star-site-words (cdr entry))) 4)))
         (pack (lambda () (dolist (entry meshes)
                           (render::star-lattice-light-words (cdr entry))))))
    (format t "~&FIXTURE spawn=~S owners=~D tiles=~D stars=~D requests=~D~%"
            spawn (length owners) (length tiles) stars (* 7 stars))
    (format t "~&LANTERN attachments=~D light-front=~D~%"
            (length (terrain:scene-attachments scene))
            (luft:voxel-light-at (luft:surface-mesh-voxel-light (cdar meshes)) 1002 1083 8))
    (assert (plusp (length (terrain:scene-attachments scene))))
    (check-words meshes)
    (field-counts meshes)
    ;; Count misses through the reference's public lattice readers. This is
    ;; outside timing and sampling, and includes mesh-local cache duplication.
    (when *reference*
      (let ((original (symbol-function 'luft:voxel-light-at-lattice-point))
            (misses 0))
        (unwind-protect
             (progn
               (setf (symbol-function 'luft:voxel-light-at-lattice-point)
                     (lambda (&rest args) (incf misses) (apply original args)))
               (dolist (entry meshes) (funcall *reference* (cdr entry))))
          (setf (symbol-function 'luft:voxel-light-at-lattice-point) original))
        (format t "~&CACHE requests=~D misses=~D hits=~D rate=~,3F~%"
                (* 7 stars) misses (- (* 7 stars) misses)
                (/ (- (* 7 stars) misses) (* 7.0 stars)))))
    (profile-case :packing pack)
    (profile-case :warm-cohort (lambda () (cohort scene owners)))
    (check-products scene)
    (profile-case :cold-cohort
                  (lambda ()
                    (clrhash (terrain:streaming-scene-light-tiles scene))
                    (setf (terrain::streaming-scene-mesh-cache scene) nil)
                    (cohort scene owners)))
    (profile-case :cold-tiles
                  (lambda () (terrain:realize-streaming-light-tiles
                              scene tiles (make-hash-table :test #'eql))))
    (solver-counts :cold-tiles
                   (lambda () (terrain:realize-streaming-light-tiles
                               scene tiles (make-hash-table :test #'eql))))
    (let* ((domain (terrain:scene-domain scene))
           (cell (luft:make-site domain 1002 1084 8 luft:+cell-extent+ 1))
           (stone (domains:identity-vocabulary-member
                   (terrain:scene-material-vocabulary scene) 0))
           (placed nil))
      (profile-case :edit
                    (lambda ()
                      (setf placed (not placed))
                      (terrain:edit-streaming-scene-cell scene cell (when placed stone))
                      (cohort scene owners)))
      (let ((repair
              (lambda ()
                (setf placed (not placed))
                (terrain:edit-streaming-scene-cell scene cell (when placed stone))
                (terrain:install-streaming-light-tiles
                 scene (terrain:realize-streaming-light-tiles
                        scene tiles (terrain:streaming-scene-light-tiles scene))))))
        (profile-case :edit-repair repair)
        (solver-counts :edit-repair repair))
      (check-words (cohort scene owners))
      (check-products scene))
    (dolist (name '(render::star-lattice-light-words
                    luft:voxel-light-at-lattice-point
                    luft:voxel-sky-at-lattice-point luft:solve-voxel-light
                    render::%star-light-lattice-reader))
      (disassemble name))
    (disassemble 'render::%fill-light-corner-tile)
    (let* ((field (luft:surface-mesh-voxel-light (cdar meshes)))
           (reader (render::%star-light-lattice-reader
                    field (luft:voxel-light-field-skylight field)
                    1023 1087 0 10 10 10
                    (make-array 1000 :element-type '(unsigned-byte 32)))))
      (when reader (disassemble reader)))))
