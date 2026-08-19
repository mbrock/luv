;;; Production voxel light stated and lowered as frontier programs.

(in-package #:luvcraft)

(frontiers:define-frontier-program voxel-light-addition
  :family :monotone-max-fixpoint
  :frontier-layout :brightest-first-buckets
  :neighborhood :voxel-face-relations
  ;; The local law of voxel light.  LEVEL is the relaxed best-known field,
  ;; bound to the sky or block lane at realization; OPACITY is the entered
  ;; cell's attenuation.  A relation transfers the source level minus the
  ;; propagation loss, admits a strict improvement, and schedules it at its
  ;; own brightness.  Those three are the monotone family's meaning; only the
  ;; transfer needs stating.
  :fields ((level :relaxed t) opacity)
  :constants (direct-direction)
  :predicates ((direct (direction= direct-direction)))
  :transfer (- (level source)
               (as-field-quantity level
                 (luvcraft.arithmetic:light-propagation-loss
                  (opacity target) direct))))

;;; Field bindings close the program over the light region's storage: each
;;; role names the lanes it borrows from a materialization (a light region
;;; entry) and how one offset is read or written through them.

(defun light-level-field-binding (field-name)
  (let ((lane-reader
          (ecase field-name
            (:sky-light 'light-region-entry-sky)
            (:block-light 'light-region-entry-block))))
    (frontiers:make-frontier-field-binding
     'level
     :declaration (fields:field-definition-for field-name)
     :lanes `((levels (,lane-reader materialization)
                      :type (simple-array (unsigned-byte 8) (*))))
     :read '(aref levels offset)
     :write '(setf (aref levels offset) value))))

(defun light-opacity-field-binding ()
  (frontiers:make-frontier-field-binding
   'opacity
   :declaration (represented-block-slot-declaration 'light-opacity)
   :lanes '((losses (light-region-entry-opacity-lut materialization)
                    :type (simple-array (unsigned-byte 8) (*)))
            (indices (light-region-entry-indices materialization)
                     :type (simple-array (unsigned-byte 16) (*))))
   :read '(aref losses (aref indices offset))))

(defvar *compiled-light-realizations* (make-hash-table :test #'eq)
  "Realizations of VOXEL-LIGHT-ADDITION per relaxed field name.")

(defun compiled-light-realization (field-name)
  "Return the current realization of the light program over FIELD-NAME."
  (let ((realization (gethash field-name *compiled-light-realizations*)))
    (if (and realization
             (frontiers:frontier-realization-current-p realization))
        realization
        (setf (gethash field-name *compiled-light-realizations*)
              (frontiers:compile-frontier-program
               'voxel-light-addition
               :bindings (list (light-level-field-binding field-name)
                               (light-opacity-field-binding))
               :site-domain '(light-region-entry-domain materialization))))))

(defun clear-compiled-light-realizations ()
  (clrhash *compiled-light-realizations*))

(defmethod frontiers:note-frontier-program-redefinition
    ((name (eql 'voxel-light-addition)))
  (declare (ignore name))
  (clear-compiled-light-realizations))

;;; The compiled solver.  Seeds and drains both go through the realization:
;;; a sky boundary is a virtual source at full brightness whose loss follows
;;; the same law, and an emitter is a value joined into the block field.

(defun seed-compiled-sky-boundaries
    (realization region frontier execution &key key entry)
  "Relate open sky into every open face of REGION's entries, or of ENTRY."
  (flet ((seed (key entry)
           (dolist (direction *voxel-face-directions*)
             (when (and (not (light-region-neighbor-resident-p
                              region key direction))
                        (eq (light-region-absent-boundary-semantics
                             region key direction)
                            :open-sky))
               ;; Open sky is a virtual source at full brightness beyond the
               ;; face; the relation into the entry runs the other way.  The
               ;; program's own transfer law decides the level and admission.
               ;; #581ZQP
               (let ((inward (opposite-voxel-direction direction)))
                 (map-entry-face-sites
                  entry direction
                  (lambda (offset local)
                    (values local)
                    (frontiers:relate-frontier-realization-site
                     realization region frontier execution entry offset inward
                     :direct-direction +voxel-negative-y+
                     :level +maximum-light-level+))))))))
    (if entry
        (seed key entry)
        (maphash #'seed (light-region-entries region)))))

(defun seed-compiled-emitters (realization region frontier execution)
  (maphash
   (lambda (key entry)
     (declare (ignore key))
     (let ((indices (light-region-entry-indices entry))
           (emission (light-region-entry-emission-lut entry))
           (domain (light-region-entry-domain entry)))
       (do-chunk-domain-sites (offset local domain)
         (values local)
         (let ((level (aref emission (aref indices offset))))
           (when (plusp level)
             (frontiers:admit-frontier-realization-site
              realization region frontier execution entry offset level
              :direct-direction nil))))))
   (light-region-entries region)))

(defun solve-compiled-light-region (region)
  "Solve REGION from scratch with the compiled voxel-light realization. #PJY6E1"
  (let* ((sky (compiled-light-realization :sky-light))
         (block (compiled-light-realization :block-light))
         (sky-frontier (frontiers:make-realization-frontier sky))
         (block-frontier (frontiers:make-realization-frontier block))
         (sky-execution
           (frontiers:make-realization-execution sky region sky-frontier))
         (block-execution
           (frontiers:make-realization-execution block region block-frontier)))
    (with-cpu-trace-zone (:lighting/compiled/seed-sky)
      (seed-compiled-sky-boundaries sky region sky-frontier sky-execution))
    (with-cpu-trace-zone (:lighting/compiled/propagate-sky)
      (with-cpu-trace-zone
          (:lighting/compiled/drain-sites
           :tracy-value (frontiers:frontier-execution-visits sky-execution))
        (frontiers:drain-frontier-realization
         sky region sky-frontier sky-execution
         :direct-direction +voxel-negative-y+)))
    (with-cpu-trace-zone (:lighting/compiled/seed-block)
      (seed-compiled-emitters block region block-frontier block-execution))
    (with-cpu-trace-zone (:lighting/compiled/propagate-block)
      (with-cpu-trace-zone
          (:lighting/compiled/drain-sites
           :tracy-value (frontiers:frontier-execution-visits block-execution))
        (frontiers:drain-frontier-realization
         block region block-frontier block-execution
         :direct-direction nil)))
    (values region
            (+ (frontiers:frontier-execution-visits sky-execution)
               (frontiers:frontier-execution-visits block-execution))
            (list sky-execution block-execution))))

(defmethod solve-light-region-using
    ((solver (eql :compiled)) (region light-region) &key &allow-other-keys)
  (declare (ignore solver))
  (with-cpu-trace-zone (:lighting/compiled)
    (solve-compiled-light-region region)))

;;; Invalidation.  Removing a source, an occluder's disappearance being an
;;; addition, or a departed neighbour can leave levels that no surviving
;;; source justifies.  The removal program clears every level the removed
;;; site fed--strictly dimmer, or equal for direct sky continuing down--and
;;; re-admits the cleared site at the level it had; a lit neighbour it did
;;; not feed is handed to the addition frontier as a surviving source.  The
;;; cleared set is the execution's retained admissions. #K3WRD3 #DURBKN

(frontiers:define-frontier-program voxel-light-removal
  :family :invalidation
  :frontier-layout :brightest-first-buckets
  :neighborhood :voxel-face-relations
  :fields ((level :invalidated t))
  :constants (direct-direction)
  :predicates ((direct (direction= direct-direction)))
  :admission (or (< (level target) (level source))
                 (and direct (= (level target) (level source))))
  :retain-admissions t)

(defvar *compiled-light-removal-realizations* (make-hash-table :test #'eq))

(defun compiled-light-removal-realization (field-name)
  (let ((realization
          (gethash field-name *compiled-light-removal-realizations*)))
    (if (and realization
             (frontiers:frontier-realization-current-p realization))
        realization
        (setf (gethash field-name *compiled-light-removal-realizations*)
              (frontiers:compile-frontier-program
               'voxel-light-removal
               :bindings (list (light-level-field-binding field-name))
               :site-domain '(light-region-entry-domain materialization))))))

(defmethod frontiers:note-frontier-program-redefinition
    ((name (eql 'voxel-light-removal)))
  (declare (ignore name))
  (clrhash *compiled-light-removal-realizations*))

(defstruct (compiled-light-reconciliation
            (:constructor %make-compiled-light-reconciliation))
  "The frontiers and executions of one compiled incremental relight."
  (region nil)
  (removal nil :type list)
  (addition nil :type list)
  (removal-executions nil :type list)
  (addition-executions nil :type list))

(defun make-compiled-light-reconciliation (region)
  (let* ((sky-removal (compiled-light-removal-realization :sky-light))
         (block-removal (compiled-light-removal-realization :block-light))
         (sky-addition (compiled-light-realization :sky-light))
         (block-addition (compiled-light-realization :block-light))
         (removal-frontiers
           (list (frontiers:make-realization-frontier sky-removal)
                 (frontiers:make-realization-frontier block-removal)))
         (addition-frontiers
           (list (frontiers:make-realization-frontier sky-addition)
                 (frontiers:make-realization-frontier block-addition))))
    (%make-compiled-light-reconciliation
     :region region
     :removal (list sky-removal block-removal)
     :addition (list sky-addition block-addition)
     :removal-executions
     (mapcar (lambda (realization frontier)
               (frontiers:make-realization-execution realization region frontier))
             (list sky-removal block-removal) removal-frontiers)
     :addition-executions
     (mapcar (lambda (realization frontier)
               (frontiers:make-realization-execution realization region frontier))
             (list sky-addition block-addition) addition-frontiers))))

(defun reconciliation-direct-direction (index)
  (if (zerop index) +voxel-negative-y+ nil))

(defun invalidate-compiled-light-site (reconciliation entry offset)
  "Clear both fields at ENTRY/OFFSET and admit them for removal."
  (loop for realization in (compiled-light-reconciliation-removal reconciliation)
        for execution in (compiled-light-reconciliation-removal-executions
                          reconciliation)
        for lane in (list (light-region-entry-sky entry)
                          (light-region-entry-block entry))
        for index from 0
        do (frontiers:admit-frontier-realization-site
            realization (compiled-light-reconciliation-region reconciliation)
            (frontiers:frontier-execution-frontier execution) execution
            entry offset (aref lane offset)
            :direct-direction (reconciliation-direct-direction index))))

(defun invalidate-compiled-light-face (reconciliation entry direction)
  "Clear and admit for removal every lit site on ENTRY's DIRECTION face."
  (loop for realization in (compiled-light-reconciliation-removal reconciliation)
        for execution in (compiled-light-reconciliation-removal-executions
                          reconciliation)
        for lane in (list (light-region-entry-sky entry)
                          (light-region-entry-block entry))
        for index from 0
        do (map-entry-face-sites
            entry direction
            (lambda (offset local)
              (values local)
              (when (plusp (aref lane offset))
                (frontiers:admit-frontier-realization-site
                 realization
                 (compiled-light-reconciliation-region reconciliation)
                 (frontiers:frontier-execution-frontier execution) execution
                 entry offset (aref lane offset)
                 :direct-direction (reconciliation-direct-direction index)))))))

(defun drain-compiled-light-removals (reconciliation)
  "Run both removal programs; survivors land in the addition frontiers."
  (loop for realization in (compiled-light-reconciliation-removal reconciliation)
        for execution in (compiled-light-reconciliation-removal-executions
                          reconciliation)
        for addition in (compiled-light-reconciliation-addition-executions
                         reconciliation)
        for index from 0
        do (frontiers:drain-frontier-realization
            realization (compiled-light-reconciliation-region reconciliation)
            (frontiers:frontier-execution-frontier execution) execution
            (frontiers:frontier-execution-frontier addition)
            :direct-direction (reconciliation-direct-direction index))))

(defun map-cleared-light-sites (function execution)
  "Call FUNCTION with ENTRY and OFFSET for every site EXECUTION cleared."
  (let* ((sites (frontiers:frontier-execution-admitted-sites execution))
         (entries (frontiers:frontier-site-buffer-materialization-lane sites))
         (offsets (frontiers:frontier-site-buffer-offset-lane sites)))
    (dotimes (index (frontiers:frontier-site-buffer-length sites))
      (funcall function (aref entries index) (aref offsets index)))))

(defun reseed-compiled-emitter (reconciliation entry offset)
  "Join ENTRY/OFFSET's own emission back into the block field."
  (let ((level (aref (light-region-entry-emission-lut entry)
                     (aref (light-region-entry-indices entry) offset))))
    (when (plusp level)
      (let ((execution (second (compiled-light-reconciliation-addition-executions
                                reconciliation))))
        (frontiers:admit-frontier-realization-site
         (second (compiled-light-reconciliation-addition reconciliation))
         (compiled-light-reconciliation-region reconciliation)
         (frontiers:frontier-execution-frontier execution) execution
         entry offset level :direct-direction nil)))))

(defun chunk-domain-contains-components-p (domain x y z)
  (let ((shape (voxel-space-chunk-shape (chunk-domain-space domain))))
    (and (<= 0 x) (< x (chunk-shape-width shape))
         (<= 0 y) (< y (chunk-shape-height shape))
         (<= 0 z) (< z (chunk-shape-depth shape)))))

(defun reseed-compiled-open-sky (reconciliation entry offset)
  "Relate open sky back into ENTRY/OFFSET if it sits on an open face."
  (let* ((region (compiled-light-reconciliation-region reconciliation))
         (realization (first (compiled-light-reconciliation-addition
                              reconciliation)))
         (execution (first (compiled-light-reconciliation-addition-executions
                            reconciliation)))
         (key (light-region-entry-key entry))
         (domain (light-region-entry-domain entry)))
    (dolist (direction *voxel-face-directions*)
      (when (and (not (light-region-neighbor-resident-p region key direction))
                 (eq (light-region-absent-boundary-semantics
                      region key direction)
                     :open-sky))
        (multiple-value-bind (x y z) (chunk-domain-local-components domain offset)
          (multiple-value-bind (neighbor-x neighbor-y neighbor-z)
              (values (+ x (voxel-direction-dx direction))
                      (+ y (voxel-direction-dy direction))
                      (+ z (voxel-direction-dz direction)))
            (unless (chunk-domain-contains-components-p
                     domain neighbor-x neighbor-y neighbor-z)
              (frontiers:relate-frontier-realization-site
               realization region
               (frontiers:frontier-execution-frontier execution) execution
               entry offset (opposite-voxel-direction direction)
               :direct-direction +voxel-negative-y+
               :level +maximum-light-level+))))))))

(defun seed-compiled-arrived-chunk (reconciliation key)
  "Seed a newly resident chunk and reschedule its neighbours' facing sites."
  (let* ((region (compiled-light-reconciliation-region reconciliation))
         (world (light-region-world region))
         (entry (multiple-value-bind (chunk present-p)
                    (world-chunk-at-coordinate world key)
                  (and present-p
                       (or (gethash key (light-region-entries region))
                           (add-light-region-entry
                            region chunk :from-field-p t))))))
    (when entry
      (let ((sky (first (compiled-light-reconciliation-addition reconciliation)))
            (sky-execution
              (first (compiled-light-reconciliation-addition-executions
                      reconciliation))))
        (seed-compiled-sky-boundaries
         sky region (frontiers:frontier-execution-frontier sky-execution)
         sky-execution :key key :entry entry))
      (let ((indices (light-region-entry-indices entry))
            (emission (light-region-entry-emission-lut entry))
            (domain (light-region-entry-domain entry)))
        (do-chunk-domain-sites (offset local domain)
          (values local)
          (when (plusp (aref emission (aref indices offset)))
            (reseed-compiled-emitter reconciliation entry offset))))
      (dolist (direction *voxel-face-directions*)
        (let* ((neighbor-key (chunk-coordinate-neighbor key direction))
               (neighbor
                 (multiple-value-bind (neighbor-chunk present-p)
                     (world-chunk-at-coordinate world neighbor-key)
                   (and present-p
                        (or (gethash neighbor-key
                                     (light-region-entries region))
                            (add-light-region-entry
                             region neighbor-chunk :from-field-p t))))))
          (declare (dynamic-extent neighbor-key))
          (when neighbor
            (loop for realization in (compiled-light-reconciliation-addition
                                      reconciliation)
                  for execution in (compiled-light-reconciliation-addition-executions
                                    reconciliation)
                  for lane in (list (light-region-entry-sky neighbor)
                                    (light-region-entry-block neighbor))
                  do (map-entry-face-sites
                      neighbor (opposite-voxel-direction direction)
                      (lambda (offset local)
                        (values local)
                        (frontiers:schedule-frontier-realization-site
                         realization
                         (frontiers:frontier-execution-frontier execution)
                         execution neighbor offset (aref lane offset)))))))))))

(defun drain-compiled-light-additions (reconciliation)
  (loop for realization in (compiled-light-reconciliation-addition reconciliation)
        for execution in (compiled-light-reconciliation-addition-executions
                          reconciliation)
        for index from 0
        do (frontiers:drain-frontier-realization
            realization (compiled-light-reconciliation-region reconciliation)
            (frontiers:frontier-execution-frontier execution) execution
            :direct-direction (reconciliation-direct-direction index))))

(defmethod reconcile-light-region-using
    ((solver (eql :compiled)) state region)
  (declare (ignore solver))
  (with-cpu-trace-zone (:lighting/compiled/reconcile)
    (nth-value 1 (reconcile-compiled-lighting state region))))

(defun reconcile-compiled-lighting (state region)
  "Settle STATE's dirty cells, departures, and arrivals over candidate REGION
with the compiled removal and addition programs.  Return the executions and
the total visits. #K3WRD3"
  (let ((reconciliation (make-compiled-light-reconciliation region))
        (world (lighting-state-world state)))
    (with-cpu-trace-zone (:lighting/compiled/invalidate)
      (maphash
       (lambda (cell present)
         (declare (ignore present))
         (multiple-value-bind (entry offset) (light-region-locate region cell)
           (when entry
             (invalidate-compiled-light-site reconciliation entry offset))))
       (lighting-state-dirty-cells state))
      (maphash
       (lambda (key present)
         (declare (ignore present))
         (dolist (direction *voxel-face-directions*)
           (let* ((neighbor-key (chunk-coordinate-neighbor key direction))
                  (entry
                    (multiple-value-bind (chunk present-p)
                        (world-chunk-at-coordinate world neighbor-key)
                      (and present-p
                           (or (gethash neighbor-key
                                        (light-region-entries region))
                               (add-light-region-entry
                                region chunk :from-field-p t))))))
             (declare (dynamic-extent neighbor-key))
             (when entry
               (invalidate-compiled-light-face
                reconciliation entry (opposite-voxel-direction direction))))))
       (lighting-state-departures state)))
    (with-cpu-trace-zone (:lighting/compiled/remove)
      (drain-compiled-light-removals reconciliation))
    (with-cpu-trace-zone (:lighting/compiled/reseed)
      (map-cleared-light-sites
       (lambda (entry offset)
         (reseed-compiled-emitter reconciliation entry offset))
       (second (compiled-light-reconciliation-removal-executions
                reconciliation)))
      (map-cleared-light-sites
       (lambda (entry offset)
         (reseed-compiled-open-sky reconciliation entry offset))
       (first (compiled-light-reconciliation-removal-executions
               reconciliation)))
      (maphash (lambda (key present)
                 (declare (ignore present))
                 (seed-compiled-arrived-chunk reconciliation key))
               (lighting-state-arrivals state)))
    (with-cpu-trace-zone (:lighting/compiled/propagate)
      (drain-compiled-light-additions reconciliation))
    (let ((executions
            (append (compiled-light-reconciliation-removal-executions
                     reconciliation)
                    (compiled-light-reconciliation-addition-executions
                     reconciliation))))
      (values executions
              (reduce #'+ executions
                      :key #'frontiers:frontier-execution-visits)))))
