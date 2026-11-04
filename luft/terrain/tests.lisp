;;;; A second game on the terrain alone: its own palette, its own world
;;;; source, and its own mesh preparer, with no renderer and no Luft world.

(defpackage #:luft.terrain.tests
  (:use #:cl)
  (:import-from #:parachute #:define-test #:true #:is)
  (:local-nicknames (#:terrain #:luft.terrain)
                    (#:domains #:luv.domains)
                    (#:production #:luv.production)))

(in-package #:luft.terrain.tests)

(define-test voxel-light-fields-retain-their-meaning
  (let* ((domain (luft:make-world-domain :horizontal-bits 4))
         (cells (make-hash-table :test #'eql))
         (opacity (make-array 0 :element-type '(unsigned-byte 8)))
         (rgb (luft:solve-voxel-light domain cells opacity #()))
         (sky (luft:solve-voxel-light domain cells opacity #() :field-name :luft-sky)))
    (dolist (entry (list (cons rgb :luft-rgb4) (cons sky :luft-sky)))
      (true (eq (luv.fields:field-definition-for (cdr entry))
                (luv.fields:materialized-field-definition (car entry) (cdr entry))))
      (let ((copy (luft:join-voxel-light-fields domain (list (car entry)))))
        (true (luv.fields:materialized-field-current-p copy (cdr entry)))))
    (true (handler-case
              (progn (luft:join-voxel-light-fields domain (list rgb sky)) nil)
            (luv.arithmetic:declaration-compatibility-error () t)))
    (true (handler-case
              (progn (luft:replace-voxel-light-region domain rgb sky '(0 0 8 8)) nil)
            (luv.arithmetic:declaration-compatibility-error () t))))
  ;; The scalar frontier realization is exact for every RGB4 and every loss.
  (true (loop for light below 4096 always
          (loop for opacity from 0 to 15 always
            (= (luft:voxel-light-transfer light opacity)
               (luft:pack-voxel-light
                (max 0 (- (logand light 15) (1+ opacity)))
                (max 0 (- (ldb (byte 4 4) light) (1+ opacity)))
                (max 0 (- (ldb (byte 4 8) light) (1+ opacity)))))))))

(define-test voxel-light-frontier-rejects-a-duration-as-loss
  ;; The physical realization must fail at expansion, not tag runtime lanes.
  (luv.frontier:define-frontier-program invalid-light-loss
    :family :monotone-max-fixpoint
    :frontier-layout (:buckets 15)
    :neighborhood :voxel-face-relations
    :fields ((level :relaxed t) loss)
    :transfer (- (level source) (loss target)))
  (let* ((level (luv.fields:field-definition-for :luft-block-lane))
         (loss (luv.arithmetic:make-represented-value-declaration
                :representation-type 'real
                :quantity-specification
                (luv.arithmetic:make-declared-quantity-specification
                 '(:quantity :duration :unit :second)))))
    (true
     (handler-case
         (progn
           (macroexpand-1
            `(luv.frontier:with-frontier-kernel
                 (invalid-light-loss
                  :fields (list (luv.frontier:make-frontier-field-binding
                                 'level :declaration ',level)
                                (luv.frontier:make-frontier-field-binding
                                 'loss :declaration ',loss))
                  (transfer :transfer (((level source) level) ((loss target) loss))))
               (transfer 15 1)))
           nil)
       (luv.arithmetic.language:arithmetic-language-error () t)))))

(defparameter *stone*
  (make-instance 'terrain:material-placement
                 :name :stone
                 :kind (make-instance 'terrain:material-kind
                                      :name :stone :base-tone '(0.4 0.4 0.4))))

(defparameter *glow*
  (make-instance 'terrain:material-placement
                 :name :glow
                 :kind (make-instance 'terrain:material-kind
                                      :name :glow :base-tone '(0.9 0.8 0.2)
                                      :light-opacity 1
                                      :light-emission '(15 12 4))))

(defun make-palette ()
  (domains:make-identity-vocabulary-domain :members (list *stone* *glow*)
                                           :limit #xff))

(defun make-builder ()
  (terrain:make-scene-builder :material-vocabulary (make-palette)
                              :ground-material *stone*
                              :architecture-material *glow*))

(define-test a-game-palette-paints-a-built-scene
  (let ((builder (make-builder)))
    (terrain:scene-builder-cell builder 3 3 0)
    (terrain:scene-builder-cell builder 4 3 0 :architecture-p t)
    (terrain:scene-builder-cell builder 5 3 0 :material *glow*)
    (let* ((scene (terrain:finish-scene-builder builder))
           (domain (terrain:scene-domain scene)))
      (flet ((at (x)
               (terrain::scene-material-placement-at
                scene (luft:make-site domain x 3 0 luft:+cell-extent+ 1))))
        (is eq *stone* (at 3))
        (is eq *glow* (at 4))
        (is eq *glow* (at 5))
        (is eq nil (at 6)))
      ;; Both glowing cells, and only they, seed authored light.
      (is = 2 (length (terrain:scene-authored-light-sources scene))))))

(define-test a-material-surface-packs-the-layout-the-star-shader-reads
  (let* ((kind (make-instance 'terrain:material-kind
                              :name :worn :base-tone '(0.5 0.5 0.5)
                              :relief :stone :relief-strength 1.0
                              :roughness 0.0 :edge-wear 1.0))
         (palette (domains:make-identity-vocabulary-domain
                   :members (list *stone*
                                  (make-instance 'terrain:material-placement
                                                 :name :worn :kind kind))
                   :limit #xff))
         (words (terrain:compile-terrain-material-descriptors palette)))
    ;; Relief code 2 (stone), strength 15, roughness 0, wear 255.
    (is = #x00FF00F2 (aref words (+ 3 (* 8 2))))
    ;; Bond, 4 rows, elements 4 half-rows long, mortar 31/255, variance 38/255.
    (is = #x0261F441
        (terrain:pack-terrain-pattern
         (make-instance 'terrain:material-kind :name :brick :base-tone '(0.5 0.2 0.1)
                                               :pattern '(:bond))))
    ;; A window's accent word: red pane tone, then surface emission 1.0 / 4.
    (is = #x400000FF
        (nth-value 2 (terrain:pack-terrain-pattern
                      (make-instance 'terrain:material-kind
                                     :name :pane :base-tone '(0.5 0.5 0.5)
                                     :surface-emission 1.0
                                     :pattern '(:window :accent-tone (1 0 0))))))
    ;; A default kind has no relief, mid strength, and rough, half-worn edges.
    (is = (logior 0 (ash 8 4) (ash 217 8) (ash 128 16))
        (aref words (+ 3 (* 8 1))))))

(define-test an-unpatterned-surface-keeps-its-own-emission
  (let ((kind (make-instance 'terrain:material-kind :name :glow
                             :base-tone '(0.2 0.4 0.8) :surface-emission 1.2)))
    (multiple-value-bind (pattern mortar accent) (terrain:pack-terrain-pattern kind)
      (is = 0 pattern)
      (is = 0 mortar)
      (is = (round (* 255 (/ 1.2 4))) (ldb (byte 8 24) accent)))))

(define-test an-attachment-keeps-its-support-and-clearance
  (let ((builder (make-builder)))
    (terrain:scene-builder-cell builder 3 3 0)
    (terrain:scene-builder-cell builder 4 3 0)
    ;; A glowing lamp on top of the first cell.
    (terrain:scene-builder-attach builder 3 3 0 :z :high
                                  (terrain:material-placement-kind *glow*))
    (let* ((scene (terrain:make-streaming-scene
                   (terrain:finish-scene-builder builder)))
           (domain (terrain:scene-domain scene))
           (attachment (aref (terrain:scene-attachments scene) 0)))
      (flet ((cell (x z) (luft:make-site domain x 3 z luft:+cell-extent+ 1)))
        (is = (luft:pack-voxel-light 15 12 4)
            (terrain:surface-attachment-light-emission attachment))
        (is eq :attachment
            (nth-value 1 (terrain:edit-streaming-scene-cell scene (cell 3 0) nil)))
        (is eq :attachment
            (nth-value 1 (terrain:edit-streaming-scene-cell scene (cell 3 1) *stone*)))
        ;; The neighbouring cell is the game's to change.
        (is eq :edited
            (nth-value 1 (terrain:edit-streaming-scene-cell scene (cell 4 0) nil)))))))

(define-test a-body-walks-around-a-wall-it-cannot-climb
  (let ((builder (make-builder)))
    ;; An 8 x 8 floor, and a wall two cells high across x = 3 for y < 6.
    (dotimes (x 8)
      (dotimes (y 8)
        (terrain:scene-builder-cell builder x y 0)
        (when (and (= x 3) (< y 6))
          (terrain:scene-builder-cell builder x y 1)
          (terrain:scene-builder-cell builder x y 2))))
    (let* ((scene (terrain:finish-scene-builder builder))
           (domain (terrain:scene-domain scene))
           (body (make-instance 'terrain:physical-body)))
      (flet ((cell (x y z) (luft:make-site domain x y z luft:+cell-extent+ 1)))
        (multiple-value-bind (cells status)
            (terrain:search-walking-path body scene (cell 1 1 1) (cell 5 1 1))
          (is eq :running status)
          (is = (cell 5 1 1) (car (last cells)))
          ;; It goes round the end of the wall, one neighbouring cell at a time.
          (true (notany (lambda (c) (and (= 3 (luft:site-x c)) (< (luft:site-y c) 6)))
                        cells))
          (true (every (lambda (a b)
                         (= 1 (max (abs (- (luft:site-x a) (luft:site-x b)))
                                   (abs (- (luft:site-y a) (luft:site-y b))))))
                       (cons (cell 1 1 1) cells) cells)))
        ;; The wall's top is standable but two cells up, out of a step's reach.
        (true (terrain:body-standable-cell-p body scene 3 2 3))
        (is eq :failed
            (nth-value 1 (terrain:search-walking-path
                          body scene (cell 1 1 1) (cell 3 2 3))))
        (is = 1.0 (terrain:body-support-height body scene 2.5 1.5 1.0))))))

(defclass flat-source (terrain:world-source) ()
  (:documentation "Two layers of stone under the whole domain."))

(defmethod terrain:generate-world-source-chunk ((source flat-source) key place)
  (loop for x from (luft:chunk-origin-x key)
          below (+ (luft:chunk-origin-x key) luft:+chunk-size+)
        do (loop for y from (luft:chunk-origin-y key)
                   below (+ (luft:chunk-origin-y key) luft:+chunk-size+)
                 do (funcall place x y 0 *stone*)
                    (funcall place x y 1 *stone*))))

(define-test a-world-source-replays-its-edits-over-generated-cells
  (let* ((domain (luft:make-world-domain :x-bits 7 :y-bits 7))
         (source (make-instance 'flat-source :domain domain
                                             :material-vocabulary (make-palette)))
         (key (luft:chunk-key-at 64 0))
         (removed (luft:make-site domain 70 5 1 luft:+cell-extent+ 1))
         (added (luft:make-site domain 70 5 2 luft:+cell-extent+ 1))
         (elsewhere (luft:make-site domain 5 5 2 luft:+cell-extent+ 1)))
    (terrain:record-world-events
     (terrain:world-source-journal source)
     (list (list removed *stone* nil) (list added nil *glow*)
           (list elsewhere nil *glow*)))
    (let* ((edits (terrain:capture-world-source-chunk-edits source key))
           (chunk (terrain:materialize-world-source-chunk
                   source key 1 :edits edits))
           (fibers (terrain::resident-cell-chunk-fibers chunk))
           (materials (terrain::resident-cell-chunk-material-cells chunk)))
      ;; The edit in another chunk is not captured for this one.
      (is = 2 (length edits))
      (is = 1 (luft:fibers-cell-bit fibers 70 6 1))
      (is = 0 (luft:fibers-cell-bit fibers 70 5 1))
      (is = 1 (luft:fibers-cell-bit fibers 70 5 2))
      (is = 0 (luft:fibers-cell-bit fibers 70 6 2))
      (is = 1 (gethash added materials))
      (is eq nil (nth-value 1 (gethash removed materials)))
      ;; 64 x 64 columns of two cells, one removed and one added.
      (is = (* 64 64 2) (hash-table-count materials)))))

(defclass counting-preparer ()
  ((calls :initform nil :accessor preparer-calls))
  (:documentation "Records each realization and returns a tagged product."))

(defmethod terrain:prepare-streamed-mesh
    ((preparer counting-preparer) mesh previous)
  (let ((product (list :prepared mesh)))
    (push (list product previous) (preparer-calls preparer))
    product))

(defmethod terrain:streamed-mesh-preparation-signature
    ((preparer counting-preparer))
  (list :counting-preparer))

(defun stream-once (scene system)
  "Mesh chunk 0 on SYSTEM's worker and install the result as a renderer would."
  (terrain:schedule-streaming-scene-cohort scene system '(0) 1 0)
  (loop for (result present-p)
          = (multiple-value-list
             (production:receive-production-result-no-hang system))
        until present-p do (sleep 0.005)
        finally (terrain::receive-streaming-mesh-result scene result))
  (multiple-value-bind (meshes generation ready-p)
      (terrain::ready-streaming-scene-meshes scene)
    (declare (ignore generation))
    (setf (terrain:streaming-scene-mesh-cache scene)
          (terrain:streaming-mesh-result-mesh-cache
           (terrain::streaming-replacement-result
            (terrain:streaming-scene-replacement scene)))
          (terrain:streaming-scene-replacement scene) nil)
    (values meshes ready-p)))

(defun call-with-streamed-scene (function &optional mesh-preparer)
  (let* ((builder (make-builder)))
    (terrain:scene-builder-cell builder 10 10 2)
    (terrain:scene-builder-cell builder 11 10 2)
    (let ((scene (terrain:make-streaming-scene
                  (terrain:finish-scene-builder builder)
                  :mesh-preparer mesh-preparer))
          (system (production:make-single-worker-production-system
                   :name "terrain test")))
      (setf (gethash 0 (terrain:streaming-scene-loaded scene)) 1)
      (unwind-protect (funcall function scene system)
        (production:stop-production-system system)))))

(define-test placed-lanterns-light-streamed-bodies-without-a-residency-rebuild
  (let* ((lamp (make-instance 'terrain:material-placement
                              :name :lantern
                              :kind (make-instance 'terrain:material-kind
                                                   :name :lantern
                                                   :base-tone '(0.5 0.4 0.2)
                                                   :attachment-form :lantern
                                                   :light-emission '(15 12 4))))
         (crate (make-instance 'terrain:material-placement
                               :name :crate
                               :kind (make-instance 'terrain:material-kind
                                                    :name :crate :body :crate
                                                    :base-tone '(0.5 0.3 0.1))))
         (palette (domains:make-identity-vocabulary-domain
                   :members (list *stone* lamp crate) :limit #xff))
         (builder (terrain:make-scene-builder
                   :horizontal-bits 6 :material-vocabulary palette
                   :ground-material *stone* :architecture-material *stone*)))
    (dotimes (x 64) (dotimes (y 64) (terrain:scene-builder-cell builder x y 0)))
    ;; A full-height return blocks light; the only way around is out of reach.
    (dotimes (y 64)
      (loop for z from 1 to 20 do (terrain:scene-builder-cell builder 12 y z)))
    (terrain:scene-builder-cell builder 10 11 1 :material crate)
    (terrain:scene-builder-cell builder 30 30 1 :material crate)
    (let* ((scene (terrain:make-streaming-scene (terrain:finish-scene-builder builder)))
           (domain (terrain:scene-domain scene))
           (cell (luft:make-site domain 10 10 1 luft:+cell-extent+ 1))
           (key (luft:site-chunk-key cell))
           (system (production:make-single-worker-production-system
                    :name "placed lantern test")))
      (setf (gethash key (terrain:streaming-scene-loaded scene)) 1)
      (unwind-protect
           (progn
             (is eq :edited (nth-value 1 (terrain:edit-streaming-scene-cell scene cell lamp)))
             (is = 1 (length (terrain:scene-attachments scene)))
             (is = 0 (terrain:scene-cell-bit scene 10 10 1))
             (true (not (nth-value 1 (terrain:material-cell-at
                                     (terrain:scene-material-cells scene) cell))))
             (let* ((mesh (cdr (assoc key (stream-cohort scene system (list key)))))
                    (field (luft:surface-mesh-voxel-light mesh))
                    (bodies (luft:surface-mesh-bodies mesh))
                    (frame (first (luft:surface-mesh-attachments mesh))))
               (is = (luft:pack-voxel-light 15 12 4) (luft:voxel-light-at-site field cell))
               (is = (luft:voxel-light-at-site field cell)
                   (ldb (byte 12 0) (round (aref frame 7))))
               (is = 0 (luft:voxel-light-at field 13 10 1))
               (is = 32 (length bodies))
               (true (plusp (aref bodies 12)))
               (is = 0.0 (aref bodies 28))
               ;; Relighting must not alter the cached instance data in place.
               (let ((old (copy-seq bodies)))
                 (stream-cohort scene system (list key))
                 (is equalp old bodies))))
        (production:stop-production-system system)))))

(define-test streaming-without-a-preparer-yields-meshes
  (call-with-streamed-scene
   (lambda (scene system)
     (multiple-value-bind (meshes ready-p) (stream-once scene system)
       (true ready-p)
       (is equal '(0) (mapcar #'car meshes))
       (true (typep (cdr (first meshes)) 'luft:surface-mesh))
       ;; Two cells: a closed surface with active star sites.
       (true (plusp (length (luft:surface-mesh-star-site-words
                             (cdr (first meshes))))))))))

(define-test a-preparer-reuses-its-previous-realization
  (let ((preparer (make-instance 'counting-preparer)))
    (call-with-streamed-scene
     (lambda (scene system)
       (let* ((first (cdr (first (stream-once scene system))))
              (second (cdr (first (stream-once scene system))))
              (calls (reverse (preparer-calls preparer))))
         (is = 2 (length calls))
         ;; The worker publishes exactly what the preparer returned.
         (is eq (first (first calls)) first)
         (is eq (first (second calls)) second)
         ;; Cold, there is nothing to reuse; warm, the cached realization
         ;; of the unchanged chunk is offered back with its star sites.
         (is eq nil (second (first calls)))
         (is eq first (second (second calls)))
         (is eq (luft:surface-mesh-star-site-words (second first))
             (luft:surface-mesh-star-site-words (second second)))))
     preparer)))

(defun stream-cohort (scene system keys)
  "Mesh KEYS as one cohort on SYSTEM's worker and publish it as a renderer would."
  (terrain:schedule-streaming-scene-cohort scene system keys 1 0)
  (loop for (result present-p)
          = (multiple-value-list
             (production:receive-production-result-no-hang system))
        until present-p do (sleep 0.005)
        finally (terrain::receive-streaming-mesh-result scene result))
  (let ((result (terrain::streaming-replacement-result
                 (terrain:streaming-scene-replacement scene))))
    (terrain:install-streaming-light-tiles
     scene (terrain:streaming-mesh-result-light-tiles result))
    (setf (terrain:streaming-scene-replacement scene) nil)
    (terrain:streaming-mesh-result-meshes result)))

(define-test streamed-light-crosses-a-chunk-seam-and-follows-edits
  ;; A floor over four chunks, a wall with a gap across the x = 64 seam, and
  ;; one emitter just west of it: its light must cross the seam, and owners
  ;; on both sides must sample one value where they meet (#BQMYNU).
  (let ((builder (terrain:make-scene-builder
                  :horizontal-bits 7 :material-vocabulary (make-palette)
                  :ground-material *stone* :architecture-material *glow*)))
    (dotimes (x 128) (dotimes (y 128) (terrain:scene-builder-cell builder x y 0)))
    (loop for y below 30 unless (= y 12)
          do (terrain:scene-builder-cell builder 66 y 1))
    (terrain:scene-builder-cell builder 62 10 1 :material *glow*)
    (let* ((authored (terrain:finish-scene-builder builder))
           (oracle (terrain:scene-authored-voxel-light authored))
           (scene (terrain:make-streaming-scene authored))
           (domain (terrain:scene-domain scene))
           (west (luft:chunk-key-at 0 0))
           (east (luft:chunk-key-at 64 0))
           (keys (list west east (luft:chunk-key-at 0 64) (luft:chunk-key-at 64 64)))
           (system (production:make-single-worker-production-system
                    :name "streamed light test")))
      (dolist (key keys) (setf (gethash key (terrain:streaming-scene-loaded scene)) 1))
      (unwind-protect
           (flet ((field (meshes key)
                    (luft:surface-mesh-voxel-light (cdr (assoc key meshes)))))
             (let* ((meshes (stream-cohort scene system keys))
                    (west-field (field meshes west))
                    (east-field (field meshes east)))
               (true (terrain:scene-regional-light-p scene))
               ;; Two steps across the seam, behind the wall's gap, exactly
               ;; as the whole-domain solver has it.
               (is = (luft:pack-voxel-light 12 9 1) (luft:voxel-light-at east-field 65 10 1))
               (is = (luft:voxel-light-at oracle 67 12 1)
                   (luft:voxel-light-at east-field 67 12 1))
               (true (plusp (luft:voxel-light-at oracle 67 12 1)))
               (dotimes (y 30)
                 (is = (luft:voxel-light-at-lattice-point west-field 64 y 1)
                     (luft:voxel-light-at-lattice-point east-field 64 y 1)))
               ;; Removing the emitter dirties the two lit tiles; every owner
               ;; whose neighbourhood reads them is relit, and the light is
               ;; gone on both sides of the seam.
               (terrain:edit-streaming-scene-cell
                scene (luft:make-site domain 62 10 1 luft:+cell-extent+ 1) nil)
               (let* ((dirty (terrain:streaming-light-dirty-owners scene keys)))
                 (is equal keys (sort (copy-list dirty) #'<))
                 (let ((meshes (stream-cohort scene system dirty)))
                   (is = 0 (luft:voxel-light-at (field meshes east) 65 10 1))
                   (is = 0 (luft:voxel-light-at (field meshes west) 61 10 1))))))
        (production:stop-production-system system)))))

(define-test incremental-rgb-tiles-match-whole-domain-after-edit-bursts
  ;; Distinct color lanes, removals, occlusion, and two distant edit regions.
  ;; Cached publications must stay immutable and carry the new input identity.
  (let* ((blue (make-instance 'terrain:material-placement
                              :name :blue
                              :kind (make-instance 'terrain:material-kind
                                                   :name :blue :light-opacity 0
                                                   :light-emission '(1 3 15))))
         (palette (domains:make-identity-vocabulary-domain
                   :members (list *stone* *glow* blue) :limit #xff))
         (builder (terrain:make-scene-builder :horizontal-bits 7
                                               :material-vocabulary palette
                                               :ground-material *stone*)))
    (dotimes (x 128) (dotimes (y 128) (terrain:scene-builder-cell builder x y 0)))
    (terrain:scene-builder-cell builder 61 63 1 :material *glow*)
    (terrain:scene-builder-cell builder 67 63 1 :material blue)
    (terrain:scene-builder-cell builder 110 110 1 :material blue)
    (let* ((scene (terrain:make-streaming-scene (terrain:finish-scene-builder builder)))
           (domain (terrain:scene-domain scene))
           (keys (loop for x in '(0 64) nconc
                   (loop for y in '(0 64) collect (luft:chunk-key-at x y))))
           (cache (make-hash-table :test #'eql)))
      (labels ((realize ()
                 (let ((tiles (terrain:realize-streaming-light-tiles scene keys cache)))
                   (dolist (tile tiles)
                     (setf (gethash (terrain::light-tile-key tile) cache) tile))
                   tiles))
               (compare-whole-domain (tiles)
                 (let* ((sources (loop for key in keys
                                       append (coerce (terrain::chunk-emitter-sources
                                                       (terrain::streaming-scene-chunk-material-table scene key)
                                                       palette) 'list)))
                        (oracle (luft:solve-voxel-light
                                 domain (terrain:material-cell-reader (terrain:scene-material-cells scene))
                                 (terrain:scene-authored-light-opacity-table scene) sources))
                        (field (terrain:realized-light-generation-field
                                (terrain::light-tiles-generation scene tiles 0)))
                        (mismatches 0))
                   (dotimes (x 128)
                     (dotimes (y 128)
                       (dotimes (z 6)
                         (unless (= (luft:voxel-light-at oracle x y z)
                                    (luft:voxel-light-at field x y z))
                           (incf mismatches)))))
                   (is = 0 mismatches))))
        (let* ((initial (realize))
               (old (terrain::light-tile-field (first initial)))
               (old-value (luft:voxel-light-at old 61 63 1)))
          (compare-whole-domain initial)
          (loop for (x y material) in
                  (list (list 61 63 nil) (list 64 63 *stone*) (list 61 63 blue)
                        (list 110 110 nil) (list 64 63 nil) (list 67 63 nil)
                        (list 61 63 nil) (list 110 110 *glow*))
                do (terrain:edit-streaming-scene-cell
                    scene (luft:make-site domain x y 1 luft:+cell-extent+ 1) material)
                   (let ((tiles (realize)))
                     (compare-whole-domain tiles)
                     (is = old-value (luft:voxel-light-at old 61 63 1))
                     (true (every #'eq tiles (realize)))))
          ;; Identity-only replacements of a table cannot dirty any light page.
          (let* ((before (realize))
                 (key (first keys))
                 (table (terrain::streaming-scene-chunk-material-table scene key)))
            (setf (gethash key (terrain::material-store-chunks (terrain:scene-material-cells scene)))
                  (alexandria:copy-hash-table table))
            (let ((after (realize)))
              (true (every #'eq (mapcar #'terrain::light-tile-field before)
                               (mapcar #'terrain::light-tile-field after))))))))))

(define-test streamed-sky-roof-skylight-and-replacement
  ;; An enclosed room spans the x=64 seam. Its central skylight admits
  ;; direct sun; the surrounding roof admits only attenuated lateral sky.
  (let ((builder (terrain:make-scene-builder
                  :horizontal-bits 7 :material-vocabulary (make-palette)
                  :ground-material *stone*)))
    (loop for x from 44 below 84 do
      (loop for y from 44 below 84 do
        (terrain:scene-builder-cell builder x y 0)
        (unless (and (= x 63) (= y 63))
          (terrain:scene-builder-cell builder x y 8))
        (when (or (= x 44) (= x 83) (= y 44) (= y 83))
          (loop for z from 1 below 8 do
            (terrain:scene-builder-cell builder x y z)))))
    (let* ((authored (terrain:finish-scene-builder builder))
           (scene (terrain:make-streaming-scene authored))
           (domain (terrain:scene-domain scene))
           (keys (loop for x in '(0 64) nconc
                   (loop for y in '(0 64) collect (luft:chunk-key-at x y))))
           (system (production:make-single-worker-production-system
                    :name "streamed sky test")))
      (dolist (key keys) (setf (gethash key (terrain:streaming-scene-loaded scene)) 1))
      (unwind-protect
           (labels ((sky (meshes key)
                      (luft:voxel-light-field-skylight
                       (luft:surface-mesh-voxel-light (cdr (assoc key meshes)))))
                    (compare-oracle (meshes)
                      ;; Independent whole-domain oracle: explicitly seed
                      ;; every directly lit cell, rather than use implicit
                      ;; columns or interface-only sources. No REGION prune.
                      (let ((sources nil)
                            (materials (terrain:scene-material-cells scene))
                            (opacity (terrain:scene-authored-light-opacity-table scene)))
                        (dotimes (x 128)
                          (dotimes (y 128)
                            (let ((level 15))
                              (loop for z from 9 downto 0
                                    for cell = (luft:make-site domain x y z luft:+cell-extent+ 1)
                                    do (multiple-value-bind (material present-p)
                                           (terrain:material-cell-at materials cell)
                                         (when present-p
                                           (setf level (max 0 (- level (aref opacity material))))))
                                       (when (plusp level)
                                         (push (luft:make-voxel-light-source cell level) sources))))))
                        (let ((oracle (luft:solve-voxel-light
                                       domain (terrain:material-cell-reader materials)
                                       opacity (coerce sources 'vector)))
                              (mismatches 0))
                          (dotimes (x 128)
                            (dotimes (y 128)
                              (dotimes (z 10)
                                (unless (= (luft:voxel-light-at oracle x y z)
                                           (luft:voxel-light-at
                                            (sky meshes (luft:chunk-key-at x y)) x y z))
                                  (incf mismatches)))))
                          (is = 0 mismatches)))))
             (let* ((meshes (stream-cohort scene system keys))
                    (field (sky meshes (first keys))))
               (is = 15 (luft:voxel-light-at field 63 63 1))
               (is = 14 (luft:voxel-light-at field 62 63 1))
               (is = 0 (luft:voxel-light-at field 48 48 1))
               (compare-oracle meshes)
               ;; A roof edit has no emitter, but must invalidate light on
               ;; both sides and preserve the cached geometry of other owners.
               (terrain:edit-streaming-scene-cell
                scene (luft:make-site domain 63 63 8 luft:+cell-extent+ 1) *stone*)
               (let ((dirty (terrain:streaming-light-dirty-owners scene keys)))
                 (is = 4 (length dirty))
                 (let ((closed (stream-cohort scene system dirty)))
                   (is = 0 (luft:voxel-light-at (sky closed (first keys)) 63 63 1))
                   ;; The immutable previous publication still reads open.
                   (is = 15 (luft:voxel-light-at field 63 63 1))
                   (compare-oracle closed)))
               ;; A partially opaque roof admits attenuated direct sky all
               ;; the way down, not one extra loss per vertical cell.
               (is eq :edited
                   (nth-value 1 (terrain:edit-streaming-scene-cell
                                 scene (luft:make-site domain 63 63 8 luft:+cell-extent+ 1) nil)))
               (terrain:edit-streaming-scene-cell
                scene (luft:make-site domain 63 63 8 luft:+cell-extent+ 1) *glow*)
               (let ((partial (stream-cohort scene system keys)))
                 (is = 14 (luft:voxel-light-at (sky partial (first keys)) 63 63 1))
                 (compare-oracle partial)))
             ;; A departure must not make a missing column an open sky source.
             (let* ((key (first keys))
                    (inputs (terrain::chunk-light-inputs scene key))
                    (unknown (copy-tree inputs)))
               (setf (second (first unknown)) nil)
               (let ((field (terrain::solve-sky-tile scene key unknown)))
                 (is = 0 (luft:voxel-light-at field 20 20 20)))))
        (production:stop-production-system system)))))

;;; The journal (#3F0SCZ) and the queue of sequenced events (#QQT86P).

(defun call-with-journal-scene (function)
  "Call FUNCTION with a finite streaming scene, its worker, and a cell maker."
  (let ((builder (make-builder)))
    (dotimes (x 8) (dotimes (y 8) (terrain:scene-builder-cell builder x y 0)))
    (let* ((scene (terrain:make-streaming-scene (terrain:finish-scene-builder builder)))
           (domain (terrain:scene-domain scene))
           (system (production:make-single-worker-production-system
                    :name "journal test")))
      (setf (gethash 0 (terrain:streaming-scene-loaded scene)) 1)
      (unwind-protect
           (funcall function scene system
                    (lambda (x y z) (luft:make-site domain x y z luft:+cell-extent+ 1)))
        (production:stop-production-system system)))))

(defun shown (scene cell)
  (terrain::scene-material-placement-at scene cell))

(define-test a-sequenced-event-waits-out-a-busy-cohort
  (call-with-journal-scene
   (lambda (scene system cell)
     (let ((here (funcall cell 2 2 1)))
       ;; The old materializer refuses while a cohort is in flight.
       (terrain:schedule-streaming-scene-cohort scene system '(0) 1 0)
       (is eq :busy (nth-value 1 (terrain:edit-streaming-scene-cell scene here *glow*)))
       ;; A sequenced event has happened anyway; the scene shows it later.
       (multiple-value-bind (event status)
           (terrain:edit-world-cell scene here *glow* :actor :alice
                                    :production-system system :bevel-width 1)
         (is eq :queued status)
         (is eq nil (terrain:world-event-before event))
         (is eq *glow* (terrain:world-event-after event))
         (is eq :alice (terrain:world-event-actor event))
         (is = 1 (terrain:world-event-sequence event)))
       (is eq nil (shown scene here))
       (is eq *glow* (terrain:world-cell-value scene here))
       ;; Bob saw the same empty cell; Alice's queued event got there first.
       (is eq :stale-before
           (nth-value 1 (terrain:edit-world-cell scene here *stone* :actor :bob
                                                 :production-system system
                                                 :bevel-width 1)))
       (is eq nil (terrain:apply-queued-world-events scene system 1))
       ;; The cohort publishes; at the next frame boundary the queue applies
       ;; and its own cohort is scheduled, as the next edit's would be.
       (loop for (result present-p)
               = (multiple-value-list
                  (production:receive-production-result-no-hang system))
             until present-p do (sleep 0.005)
             finally (terrain::receive-streaming-mesh-result scene result))
       (setf (terrain:streaming-scene-replacement scene) nil)
       (is = 1 (terrain:apply-queued-world-events scene system 1))
       (is eq *glow* (shown scene here))
       (is eq nil (terrain:streaming-scene-pending-events scene))
       (true (terrain:streaming-scene-replacement scene))
       (is = 1 (terrain:world-journal-head (terrain:streaming-scene-journal scene)))))))

(define-test the-sequencer-checks-before-values-and-applies-transactions-whole
  (call-with-journal-scene
   (lambda (scene system cell)
     (declare (ignore system))
     (let ((journal (terrain:streaming-scene-journal scene))
           (empty (funcall cell 3 3 1))
           (floor (funcall cell 3 3 0))
           (stranger (make-instance 'terrain:material-placement
                                    :name :stranger
                                    :kind (terrain:material-placement-kind *stone*))))
       (flet ((refusal (changes)
                (multiple-value-list
                 (terrain:sequence-world-events scene changes :actor :alice))))
         ;; The floor is stone, not air: the second change is stale, so the
         ;; first is not recorded either.
         (is equal (list nil :stale-before (list floor nil *glow*))
             (refusal (list (list empty nil *stone*) (list floor nil *glow*))))
         (is = 0 (terrain:world-journal-head journal))
         (is eq nil (terrain:world-cell-value scene empty))
         (is eq :unchanged (second (refusal (list (list empty nil nil)))))
         (is eq :unknown-material (second (refusal (list (list empty nil stranger)))))
         ;; A later change sees an earlier one's value in its transaction.
         (let ((events (terrain:sequence-world-events
                        scene (list (list empty nil *stone*) (list empty *stone* *glow*)
                                    (list floor *stone* nil))
                        :actor :alice :tick 7 :cause :test)))
           (is = 3 (length events))
           (is = 1 (length (remove-duplicates
                            (mapcar #'terrain:world-event-transaction events))))
           (is equal '(1 2 3) (mapcar #'terrain:world-event-sequence events))
           (is eql 7 (terrain:world-event-tick (first events))))
         (is = 3 (terrain:apply-queued-world-events scene nil 1))
         (is eq *glow* (shown scene empty))
         (is eq nil (shown scene floor))
         (is = 0 (terrain:scene-cell-bit scene 3 3 0))
         ;; The edit set is a view: one value per site, the latest.
         (is = 2 (hash-table-count (terrain:world-journal-edits journal)))
         (is eq *glow* (gethash empty (terrain:world-journal-edits journal))))))))

(define-test two-actors-undo-their-own-interleaved-edits
  (call-with-journal-scene
   (lambda (scene system cell)
     (declare (ignore system))
     (let ((a (funcall cell 1 1 1)) (b (funcall cell 2 1 1))
           (c (funcall cell 3 1 1)) (d (funcall cell 4 1 1)))
       (flet ((edit (actor site value)
                (is eq :edited
                    (nth-value 1 (terrain:edit-world-cell scene site value :actor actor
                                                                          :bevel-width 1))))
              (undo (actor)
                (nth-value 1 (terrain:undo-world-events scene actor :bevel-width 1)))
              (redo (actor)
                (nth-value 1 (terrain:redo-world-events scene actor :bevel-width 1))))
         (edit :alice a *stone*)
         (edit :alice c *glow*)
         (edit :bob b *glow*)
         ;; Alice's undo takes back her own latest edit, not Bob's later one.
         (is eq :edited (undo :alice))
         (is eq nil (shown scene c))
         (is eq *glow* (shown scene b))
         (is eq :edited (undo :bob))
         (is eq nil (shown scene b))
         (is eq *stone* (shown scene a))
         (is eq :edited (undo :alice))
         (is eq nil (shown scene a))
         (is eq :nothing (undo :alice))
         (is eq :edited (redo :alice))
         (is eq *stone* (shown scene a))
         (is eq :edited (redo :bob))
         (is eq *glow* (shown scene b))
         ;; Once Bob has removed Alice's block, her undo would undo his
         ;; work too: it is refused, and her next undo reaches further back.
         (edit :alice d *stone*)
         (edit :bob d nil)
         (is eq :stale-before (undo :alice))
         (is eq nil (shown scene d))
         (is eq :edited (undo :alice))
         (is eq nil (shown scene a))
         (let ((journal (terrain:streaming-scene-journal scene)))
           (is equal (list :undo (terrain:world-event-transaction
                                  (aref (terrain:world-journal-events journal) 0)))
               (terrain:world-event-cause
                (aref (terrain:world-journal-events journal)
                      (1- (length (terrain:world-journal-events journal))))))))))))

(define-test a-source-scene-journal-reaches-chunks-it-has-not-shown
  (let* ((domain (luft:make-world-domain :x-bits 7 :y-bits 7))
         (source (make-instance 'flat-source :domain domain
                                             :material-vocabulary (make-palette)))
         (scene (terrain:make-world-source-streaming-scene source))
         (key (luft:chunk-key-at 64 0))
         (site (luft:make-site domain 70 5 1 luft:+cell-extent+ 1)))
    (is eq (terrain:world-source-journal source) (terrain:streaming-scene-journal scene))
    ;; Nothing is resident, so nobody can know what the cell holds.
    (is eq :nonresident (nth-value 1 (terrain:edit-world-cell scene site nil :actor :alice)))
    ;; Resident but not shown: the event is checked against the resident value,
    ;; and the next materialization replays it from the source's edit view.
    (setf (gethash key (terrain:streaming-scene-store scene))
          (terrain:materialize-world-source-chunk source key 1))
    (is eq :edited (nth-value 1 (terrain:edit-world-cell scene site nil :actor :alice)))
    (is eq nil (gethash site (terrain:world-source-edits source)))
    (true (nth-value 1 (gethash site (terrain:world-source-edits source))))
    (let ((chunk (terrain:materialize-world-source-chunk
                  source key 2
                  :edits (terrain:capture-world-source-chunk-edits source key))))
      (is = 0 (luft:fibers-cell-bit (terrain::resident-cell-chunk-fibers chunk) 70 5 1)))))

;;; Interest (#I6O4J6): collision for every focus, publication for one.

(defun settle-streaming (scene system x y &key (until (constantly t)) (seconds 20))
  "Retarget SCENE around X, Y and install every result until UNTIL holds."
  (let ((deadline (+ (get-internal-real-time) (* seconds internal-time-units-per-second))))
    (flet ((idle-p ()
             (and (zerop (hash-table-count (terrain:streaming-scene-load-outstanding scene)))
                  (null (terrain:streaming-scene-replacement scene)))))
      (loop
        ;; Settled when a retarget with nothing in flight demands nothing.
        (let* ((idle-p (idle-p))
               (quiet-p (null (terrain:retarget-streaming-scene scene system 1 x y))))
          (when (and idle-p quiet-p (idle-p) (funcall until)) (return t)))
        (loop for (result present-p)
                = (multiple-value-list (production:receive-production-result-no-hang system))
              while present-p
              do (if (typep (production:production-result-request result)
                            'terrain::streaming-mesh-request)
                     (terrain::receive-streaming-mesh-result scene result)
                     (terrain::receive-streaming-source-result scene result)))
        (let ((replacement (terrain:streaming-scene-replacement scene)))
          (when (and replacement (terrain:streaming-replacement-result replacement))
            (when (terrain:streaming-replacement-request replacement)
              (setf (terrain:streaming-scene-mesh-cache scene)
                    (terrain:streaming-mesh-result-mesh-cache
                     (terrain::streaming-replacement-result replacement))))
            (setf (terrain:streaming-scene-replacement scene) nil)))
        (when (> (get-internal-real-time) deadline) (return nil))
        (sleep 0.005)))))

(define-test collision-follows-every-interest-and-publication-one
  (let* ((domain (luft:make-world-domain :x-bits 9 :y-bits 9))
         (source (make-instance 'flat-source :domain domain :material-vocabulary (make-palette)))
         (scene (terrain:make-world-source-streaming-scene source))
         (system (production:make-single-worker-production-system :name "interest test"))
         (body (make-instance 'terrain:physical-body)))
    (unwind-protect
         (flet ((collides-p (x y) (terrain:scene-collision-resident-p scene x y))
                (loaded () (sort (loop for key being the hash-keys
                                         of (terrain:streaming-scene-loaded scene)
                                       collect key)
                                 #'<)))
           (true (settle-streaming scene system 10 10))
           (true (collides-p 10 10))
           (true (not (collides-p 400 400)))
           ;; A second player six chunks away along both axes.
           (is = (luft:chunk-key-at 400 400)
               (terrain:set-streaming-scene-interest scene :bea 400.5 400.5))
           (true (settle-streaming scene system 10 10
                                   :until (lambda () (collides-p 400 400))))
           ;; Its whole gameplay radius collides, and nothing between.
           (dolist (x '(320 384 448))
             (dolist (y '(320 384 448))
               (true (collides-p x y) "collides at ~D ~D" x y)))
           (true (not (collides-p 256 256)))
           (true (not (collides-p 200 10)))
           (true (terrain:body-standable-cell-p body scene 400 400 2))
           (true (terrain:body-standable-cell-p body scene 10 10 2))
           ;; Meshes and publication follow only the local focus.
           (is equal (sort (list (luft:chunk-key-at 0 0) (luft:chunk-key-at 64 0)
                                 (luft:chunk-key-at 0 64) (luft:chunk-key-at 64 64))
                           #'<)
               (loaded))
           (true (null (gethash (luft:chunk-key-at 400 400)
                                (terrain:material-store-chunks (terrain:scene-material-cells scene)))))
           ;; The local focus moves; the other stays.
           (true (settle-streaming scene system 230 10))
           (true (collides-p 400 400))
           (true (collides-p 170 10))
           (true (not (member (luft:chunk-key-at 0 64) (loaded))))
           ;; Its player leaves: those chunks stop colliding and are evicted.
           (terrain:remove-streaming-scene-interest scene :bea)
           (true (settle-streaming scene system 230 10))
           (true (not (collides-p 400 400)))
           (true (not (nth-value 1 (gethash (luft:chunk-key-at 400 400)
                                            (terrain:streaming-scene-store scene))))))
      (production:stop-production-system system))))

(define-test an-edit-reaches-a-chunk-seen-but-not-collided-with
  ;; A peer edits a chunk this player sees but neither stands in nor
  ;; collides with: occupancy is the resident chunk's, not collision's.
  (let* ((domain (luft:make-world-domain :x-bits 7 :y-bits 7))
         (source (make-instance 'flat-source :domain domain :material-vocabulary (make-palette)))
         (scene (terrain:make-world-source-streaming-scene source))
         (near (luft:chunk-key-at 0 0))
         (far (luft:chunk-key-at 64 0))
         (dug (luft:make-site domain 70 5 1 luft:+cell-extent+ 1))
         (built (luft:make-site domain 71 5 2 luft:+cell-extent+ 1)))
    (loop for key in (list near far) for incarnation from 1
          do (setf (gethash key (terrain:streaming-scene-store scene))
                   (terrain:materialize-world-source-chunk source key incarnation)))
    (terrain:rebuild-authored-world-resident-values scene (list near far) (list near))
    (is eq :edited (nth-value 1 (terrain:edit-streaming-scene-cell scene dug nil)))
    (is eq :edited (nth-value 1 (terrain:edit-streaming-scene-cell scene built *stone*)))
    (is eq :occupied (nth-value 1 (terrain:edit-streaming-scene-cell scene built *stone*)))
    ;; Collision keeps its window; the resident chunk has both edits.
    (true (null (luft:fiber-store-chunk (terrain:scene-solid scene) far)))
    (let ((fibers (terrain::streaming-store-fibers scene far)))
      (is = 0 (luft:fibers-cell-bit fibers 70 5 1))
      (is = 1 (luft:fibers-cell-bit fibers 71 5 2)))
    ;; And collides with them once it is in a window.
    (terrain:install-streaming-collision scene (list near far))
    (is = 0 (terrain:scene-cell-bit scene 70 5 1))
    (is = 1 (terrain:scene-cell-bit scene 71 5 2))))
