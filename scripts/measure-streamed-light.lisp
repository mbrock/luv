;;;; Run with ./env --slim sbcl --script scripts/measure-streamed-light.lisp
;;;; A deterministic seed-7 spawn cohort, with materialization outside timing.
(require :asdf)
(asdf:load-system "luvcraft/luft")
(asdf:load-system "luft/renderer")

(defpackage #:measure-streamed-light
  (:use #:cl)
  (:local-nicknames (#:terrain #:luft.terrain) (#:render #:luft.render)
                    (#:domains #:luv.domains)))
(in-package #:measure-streamed-light)

(defun milliseconds (start)
  (* 1000.0 (/ (- (get-internal-real-time) start)
               internal-time-units-per-second)))

(defun prepare-cohort (scene owners)
  "CPU capture through prepared sidecars, excluding GPU/owner scheduling."
  (multiple-value-bind (meshes census diagnostics generation products tiles)
      (terrain::mesh-streaming-snapshot
       (terrain::make-streaming-region-snapshot scene owners 1) :prepare-products-p t)
    (declare (ignore census diagnostics generation))
    (setf (terrain::streaming-scene-mesh-cache scene) products)
    (terrain:install-streaming-light-tiles scene tiles)
    meshes))

(let* ((source (luvcraft.luft:make-luvcraft-world-source :seed 7))
       (scene (terrain:make-world-source-streaming-scene
               source :mesh-preparer render:+render-population-preparer+))
       (spawn (multiple-value-list (terrain:world-source-spawn source)))
       (key (luft:chunk-key-at (first spawn) (second spawn)))
       (sources (terrain::streaming-scene-dependency-guard-keys scene (list key)))
       (owners (terrain::streaming-scene-canonical-owner-closure scene sources))
       (tiles (terrain::streaming-scene-dependency-guard-keys scene owners))
       (captured (terrain::streaming-scene-dependency-guard-keys scene tiles))
       (start (get-internal-real-time)))
  (dolist (key captured)
    (setf (gethash key (terrain:streaming-scene-store scene))
          (terrain:materialize-world-source-chunk source key 1)))
  (terrain::rebuild-authored-world-resident-values scene captured)
  (dolist (key sources)
    (setf (gethash key (terrain:streaming-scene-loaded scene)) 1))
  (format t "~&LIGHT-BENCH spawn=~S owners=~D tiles=~D materialize-ms=~,1F~%"
          spawn (length owners) (length tiles) (milliseconds start))
  (dotimes (run 3)
    (let* ((start (get-internal-real-time))
           (realized (terrain:realize-streaming-light-tiles
                      scene tiles (terrain:streaming-scene-light-tiles scene)))
           (solve-ms (milliseconds start))
           (generation (terrain::light-tiles-generation scene realized 0))
           (start (get-internal-real-time))
           (snapshot (terrain::make-streaming-region-snapshot scene owners 1)))
      (multiple-value-bind (meshes) (terrain::mesh-streaming-snapshot snapshot)
        (let ((mesh-ms (milliseconds start))
              (start (get-internal-real-time))
              (bytes 0))
          (dolist (entry meshes)
            (incf bytes (* 4 (length (render::star-lattice-light-words (cdr entry))))))
          (format t "LIGHT-BENCH run=~D tiles-ms=~,1F cohort-ms=~,1F pack-ms=~,1F bytes=~D visits=~D pushes=~D~%"
                  run solve-ms mesh-ms (milliseconds start) bytes
                  (luft:voxel-light-field-visits
                   (terrain:realized-light-generation-field generation))
                  (luft:voxel-light-field-pushes
                   (terrain:realized-light-generation-field generation))))
        (terrain:install-streaming-light-tiles scene realized))))
  ;; Repeated roof placement/removal uses the real geometry cache and renderer
  ;; preparer. These are serial worker CPU latencies, not end-to-end frame times.
  (let* ((previous (prepare-cohort scene owners))
         (domain (terrain:scene-domain scene))
         (roof (luft:make-site domain (+ 4 (first spawn)) (+ 4 (second spawn))
                               200 luft:+cell-extent+ 1))
         (stone (domains:identity-vocabulary-member
                 (terrain:scene-material-vocabulary scene) 0)))
    (dotimes (edit 4)
      (let ((start (get-internal-real-time)))
        (multiple-value-bind (change status)
            (terrain:edit-streaming-scene-cell scene roof (when (evenp edit) stone))
          (declare (ignore change))
          (unless (eq status :edited) (error "Roof edit refused: ~S" status)))
        (let* ((edit-ms (milliseconds start))
               (dirty (terrain:streaming-light-dirty-owners scene owners))
               (tile-start (get-internal-real-time))
               (allocated (sb-ext:get-bytes-consed))
               (old (terrain:streaming-scene-light-tiles scene))
               (realized (terrain:realize-streaming-light-tiles scene tiles old))
               (tile-ms (milliseconds tile-start))
               (tile-bytes (- (sb-ext:get-bytes-consed) allocated))
               (start (get-internal-real-time))
               (meshes (prepare-cohort scene owners)))
          ;; Independent trial, like the cold tile/cohort measurements above.
          ;; Do not install REALIZED before PREPARE-COHORT: that would hide the
          ;; reconciliation work from the prepared cohort's timing.
          (format t "LIGHT-EDIT edit=~D tiles-ms=~,1F allocated-MiB=~,2F replaced-rgb=~D replaced-sky=~D/~D~%"
                  edit tile-ms (/ tile-bytes 1048576.0)
                  (count-if (lambda (tile)
                              (not (eq (terrain::light-tile-field tile)
                                       (terrain::light-tile-field
                                        (gethash (terrain::light-tile-key tile) old))))) realized)
                  (count-if (lambda (tile)
                              (not (eq (terrain::light-tile-sky tile)
                                       (terrain::light-tile-sky
                                        (gethash (terrain::light-tile-key tile) old))))) realized)
                  (length realized))
          (format t "LIGHT-BURST edit=~D edit-ms=~,1F prepared-ms=~,1F dirty=~D reused-geometry=~D/~D~%"
                  edit edit-ms (milliseconds start) (length dirty)
                  (count-if (lambda (entry)
                              (eq (luft:surface-mesh-star-site-words (cdr entry))
                                  (luft:surface-mesh-star-site-words
                                   (cdr (assoc (car entry) previous))))) meshes)
                  (length meshes))
          (setf previous meshes))))))
