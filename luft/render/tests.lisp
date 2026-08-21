(defpackage #:luft.render.tests
  (:use #:cl #:rove #:luft.render)
  (:local-nicknames (#:vec3 #:luv.arithmetic.lisp.vec3)))

(in-package #:luft.render.tests)

(defclass surface-release-probe ()
  ((name :initarg :name :reader surface-release-probe-name)
   (failures-remaining :initarg :failures-remaining :initform 0
                       :accessor surface-release-probe-failures-remaining)
   (attempts :initform 0 :accessor surface-release-probe-attempts)
   (writes :initform 0 :accessor surface-release-probe-writes)
   (released-p :initform nil :accessor surface-release-probe-released-p)))

(defmethod luv:destroy ((probe surface-release-probe))
  (when (surface-release-probe-released-p probe)
    (error "Surface release probe ~S was released twice."
           (surface-release-probe-name probe)))
  (incf (surface-release-probe-attempts probe))
  (when (plusp (surface-release-probe-failures-remaining probe))
    (decf (surface-release-probe-failures-remaining probe))
    (error "Synthetic failure releasing surface probe ~S."
           (surface-release-probe-name probe)))
  (setf (surface-release-probe-released-p probe) t)
  (values))

(defmethod luv:write-buffer
    ((probe surface-release-probe) data &key (offset 0))
  (declare (ignore data offset))
  (when (surface-release-probe-released-p probe)
    (error "Writing released surface probe ~S."
           (surface-release-probe-name probe)))
  (incf (surface-release-probe-writes probe))
  probe)

(define-condition surface-construction-probe-error (error) ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (write-string "Synthetic surface construction failure." stream))))

(defclass surface-publication-probe-device ()
  ((fail-shadow-bind-group-p
    :initform nil :accessor surface-publication-fail-shadow-bind-group-p)
   (fail-write-at :initform nil :accessor surface-publication-fail-write-at)
   (write-attempts :initform 0 :accessor surface-publication-write-attempts)
   (release-failure-labels
    :initform nil :accessor surface-publication-release-failure-labels)
   (resources :initform nil :accessor surface-publication-resources)))

(defclass surface-publication-probe-resource (surface-release-probe)
  ((device :initarg :device :reader surface-publication-resource-device)))

(defmethod luv:write-buffer
    ((resource surface-publication-probe-resource) data &key (offset 0))
  (let* ((device (surface-publication-resource-device resource))
         (attempt (incf (surface-publication-write-attempts device))))
    (when (eql attempt (surface-publication-fail-write-at device))
      (error 'surface-construction-probe-error)))
  (call-next-method resource data :offset offset))

(defmethod luv:create ((device surface-publication-probe-device) descriptor)
  (let ((label (luv::gpu-descriptor-label descriptor)))
    (when (and (surface-publication-fail-shadow-bind-group-p device)
               (typep descriptor 'luv::bind-group-descriptor)
               (string= label "luft orthographic shadow bindings"))
      (error 'surface-construction-probe-error))
    (let ((resource
            (make-instance
             'surface-publication-probe-resource
             :name label :device device
             :failures-remaining
             (if (member label
                         (surface-publication-release-failure-labels device)
                         :test #'string=)
                 1
                 0))))
      (push resource (surface-publication-resources device))
      resource)))

(defclass surface-construction-probe-device ()
  ((fail-on-create :initarg :fail-on-create
                   :reader surface-construction-probe-fail-on-create)
   (release-failure-indices :initarg :release-failure-indices :initform nil
                            :reader surface-construction-release-failures)
   (create-attempts :initform 0
                    :accessor surface-construction-create-attempts)
   (resources :initform nil :accessor surface-construction-resources)))

(defmethod luv:create ((device surface-construction-probe-device) descriptor)
  (declare (ignore descriptor))
  (let ((index (incf (surface-construction-create-attempts device))))
    (when (eql index (surface-construction-probe-fail-on-create device))
      (error 'surface-construction-probe-error))
    (let ((resource
            (make-instance
             'surface-release-probe
             :name index
             :failures-remaining
             (count index (surface-construction-release-failures device)))))
      (push resource (surface-construction-resources device))
      resource)))

(defun make-release-test-technique (&optional device)
  (make-instance 'surface-technique
                 :device device
                 :pipeline-styles '(:stock)
                 :target-formats '(:rgba16-float)
                 :temporal-p nil
                 :output-space :linear))

(defun make-release-test-renderer (failures)
  (let* ((device (make-instance 'surface-release-probe :name :device))
         (technique (make-release-test-technique device))
         (state (make-instance 'surface-frame-state :technique technique))
         (resource
           (make-instance 'surface-release-probe
                          :name :state-resource
                          :failures-remaining failures))
         (layout (make-instance 'surface-release-probe :name :layout))
         (renderer
           (make-instance 'renderer
                          :device device :owns-device-p t
                          :scene nil :camera nil :extent '(1 1)
                          :color-format :rgba8-unorm-srgb)))
    (setf (luft.render::surface-frame-state-uniform-buffer state) resource
          (surface-technique-layout technique) layout
          (renderer-surface-frame-state renderer) state
          (renderer-surface-technique renderer) technique)
    (luft.render::register-surface-frame-state state)
    (values renderer state technique resource layout device)))

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

(defun make-stock-edit-scene ()
  "A palette scene with one hidden cell and one four-page exposed cell."
  (let* ((domain (luft:make-world-domain :horizontal-bits 4))
         (solid (luft:make-solid-chain domain))
         (slots (make-array (luft:chain-cell-bit-count domain)
                            :element-type '(unsigned-byte 8)
                            :initial-element 0)))
    ;; (3,3,3) is wholly hidden inside this cube.
    (loop for x from 2 to 4
          do (loop for y from 2 to 4
                   do (loop for z from 2 to 4
                            do (setf (luft:solid-cell-p solid x y z) t))))
    ;; Each high face of (7,7,7) crosses one 8-cell page boundary, so this
    ;; single exposed owner exercises the base, +X, +Y, and +Z pages.
    (setf (luft:solid-cell-p solid 7 7 7) t)
    (make-scene domain :solid solid :slots slots
                        :stocks #(:turf :granite :oak :snow))))

(defun stock-cell-vector (&rest cells)
  (make-array (length cells) :element-type '(unsigned-byte 64)
                              :initial-contents cells))

(defun stock-slot-vector (&rest slots)
  (make-array (length slots) :element-type '(unsigned-byte 8)
                              :initial-contents slots))

(defun scene-cell-slot-nibble (scene cell)
  (let ((index (luft.render::scene-cell-bit-index scene cell)))
    (ldb (byte 4 (* 4 (mod index 8)))
         (aref (luft.render::scene-slot-words scene) (floor index 8)))))

(defun scene-exposed-cell-stock-p (scene cell stock)
  "Whether every final exposed face owned by CELL is stamped STOCK."
  (let ((matches-p t)
        (count 0)
        (sites (scene-sites scene)))
    (luft:map-site-boundary
     (lambda (face axis side)
       (declare (ignore axis side))
       (when (luft:chain-site-p (scene-surface scene) face)
         (incf count)
         (let ((packed (find face sites :test #'= :key #'packed-site)))
           (unless (and packed
                        (= stock
                           (ldb
                            (byte luft.render.shaders:+site-stock-bits+
                                  luft.render.shaders:+site-stock-shift+)
                            packed)))
             (setf matches-p nil)))))
     (scene-domain scene) (luft:site-geometry cell))
    (and (plusp count) matches-p)))

(defun fresh-scene-reference (scene)
  "Rebuild SCENE's authored solid and stock columns into a fresh scene."
  (let ((solid (luft:make-chain (scene-domain scene))))
    (loop for cell across (luft:chain-sites (scene-solid scene))
          do (luft:add-chain-site solid cell))
    (make-scene (scene-domain scene)
                :solid solid
                :slots (copy-seq (scene-slots scene))
                :stocks (copy-seq (scene-stocks scene)))))

(defun scene-equals-fresh-reference-p (scene)
  (let ((fresh (fresh-scene-reference scene)))
    (and (equalp (luft:chain-sites (scene-solid scene))
                 (luft:chain-sites (scene-solid fresh)))
         (equalp (luft:chain-sites (scene-surface scene))
                 (luft:chain-sites (scene-surface fresh)))
         (equalp (scene-cell-bits scene) (scene-cell-bits fresh))
         (equalp (scene-slots scene) (scene-slots fresh))
         (equalp (luft.render::scene-slot-words scene)
                 (luft.render::scene-slot-words fresh))
         (equalp (scene-sites scene) (scene-sites fresh)))))

(deftest stock-only-edit-restamps-exposed-pages-and-one-packed-nibble
  (let* ((scene (make-stock-edit-scene))
         (domain (scene-domain scene))
         (cell (luft:make-site domain 7 7 7 luft:+cell-extent+))
         (cell-index (luft.render::scene-cell-bit-index scene cell))
         (slot-word (floor cell-index 8))
         (revision (scene-revision scene)))
    (apply-scene-edit
     scene (luft:make-chain domain)
     :stock-cells (stock-cell-vector cell)
     :stock-slots (stock-slot-vector 2))
    (ok (= (1+ revision) (scene-revision scene)))
    (ok (= 2 (aref (scene-slots scene) cell-index)))
    (ok (= 2 (scene-cell-slot-nibble scene cell)))
    (ok (scene-exposed-cell-stock-p scene cell 2))
    (multiple-value-bind (chunks cell-words slot-words available-p)
        (luft.render::scene-changes-since scene revision)
      (ok available-p)
      (ok (equalp #(0 1 2 4) chunks))
      (ok (zerop (length cell-words)))
      (ok (equalp (vector slot-word) slot-words)))))

(deftest hidden-stock-edit-publishes-only-its-slot-word-and-no-op-stays-quiet
  (let* ((scene (make-stock-edit-scene))
         (domain (scene-domain scene))
         (cell (luft:make-site domain 3 3 3 luft:+cell-extent+))
         (cell-index (luft.render::scene-cell-bit-index scene cell))
         (slot-word (floor cell-index 8))
         (revision (scene-revision scene))
         (sites (copy-seq (luft.render::scene-site-pages scene)))
         (cell-bits (copy-seq (scene-cell-bits scene))))
    (apply-scene-edit
     scene (luft:make-chain domain)
     :stock-cells (stock-cell-vector cell)
     :stock-slots (stock-slot-vector 1))
    (ok (= (1+ revision) (scene-revision scene)))
    (ok (= 1 (aref (scene-slots scene) cell-index)))
    (ok (= 1 (scene-cell-slot-nibble scene cell)))
    (ok (equalp sites (luft.render::scene-site-pages scene)))
    (ok (equalp cell-bits (scene-cell-bits scene)))
    (multiple-value-bind (chunks cell-words slot-words available-p)
        (luft.render::scene-changes-since scene revision)
      (ok available-p)
      (ok (zerop (length chunks)))
      (ok (zerop (length cell-words)))
      (ok (equalp (vector slot-word) slot-words)))
    ;; Repainting the same slot is not a publication.
    (let ((published (scene-revision scene)))
      (apply-scene-edit
       scene (luft:make-chain domain)
       :stock-cells (stock-cell-vector cell)
       :stock-slots (stock-slot-vector 1))
      (ok (= published (scene-revision scene))))))

(deftest empty-neighbor-stock-edit-does-not-claim-an-opposite-owned-face
  (let* ((scene (make-stock-edit-scene))
         (domain (scene-domain scene))
         ;; This empty cell is immediately west of the exposed (7,7,7) solid.
         ;; Their shared face geometry exists in the surface with the solid's
         ;; opposite polarity, so geometry-only matching would restamp it.
         (cell (luft:make-site domain 6 7 7 luft:+cell-extent+))
         (cell-index (luft.render::scene-cell-bit-index scene cell))
         (slot-word (floor cell-index 8))
         (revision (scene-revision scene))
         (sites (copy-seq (luft.render::scene-site-pages scene))))
    (ok (not (scene-cell-p scene 6 7 7)))
    (apply-scene-edit
     scene (luft:make-chain domain)
     :stock-cells (stock-cell-vector cell)
     :stock-slots (stock-slot-vector 1))
    (ok (= (1+ revision) (scene-revision scene)))
    (ok (= 1 (aref (scene-slots scene) cell-index)))
    (ok (= 1 (scene-cell-slot-nibble scene cell)))
    (ok (equalp sites (luft.render::scene-site-pages scene)))
    (multiple-value-bind (chunks cell-words slot-words available-p)
        (luft.render::scene-changes-since scene revision)
      (ok available-p)
      (ok (zerop (length chunks)))
      (ok (zerop (length cell-words)))
      (ok (equalp (vector slot-word) slot-words)))))

(deftest stock-only-publication-stays-sparse-in-a-surface-frame-state
  (let* ((device (make-instance 'surface-publication-probe-device))
         (technique (make-release-test-technique device))
         (state nil)
         (scene (make-stock-edit-scene))
         (domain (scene-domain scene))
         (cell (luft:make-site domain 7 7 7 luft:+cell-extent+)))
    (unwind-protect
         (progn
           (setf state (make-surface-frame-state technique :scene scene))
           (let* ((group (surface-frame-state-bind-group state))
                  (sites
                    (luft.render::surface-frame-state-sites-buffer state))
                  (cells
                    (luft.render::surface-frame-state-cells-buffer state))
                  (slots
                    (luft.render::surface-frame-state-slots-buffer state))
                  (site-writes (surface-release-probe-writes sites))
                  (cell-writes (surface-release-probe-writes cells))
                  (slot-writes (surface-release-probe-writes slots)))
             (apply-scene-edit
              scene (luft:make-chain domain)
              :stock-cells (stock-cell-vector cell)
              :stock-slots (stock-slot-vector 1))
             (synchronize-surface-frame-state state scene)
             (ok (eq :incremental
                     (surface-frame-state-last-scene-upload-kind state)))
             (ok (= 5 (surface-frame-state-last-scene-upload-writes state)))
             (ok (> (surface-frame-state-last-scene-upload-bytes state) 4))
             (ok (eq group (surface-frame-state-bind-group state)))
             (ok (eq sites
                     (luft.render::surface-frame-state-sites-buffer state)))
             (ok (eq cells
                     (luft.render::surface-frame-state-cells-buffer state)))
             (ok (eq slots
                     (luft.render::surface-frame-state-slots-buffer state)))
             (ok (= (+ 4 site-writes)
                    (surface-release-probe-writes sites)))
             (ok (= cell-writes (surface-release-probe-writes cells)))
             (ok (= (1+ slot-writes)
                    (surface-release-probe-writes slots)))))
      (when state (destroy-surface-frame-state state))
      (destroy-surface-technique technique))))

(deftest combined-occupancy-and-stock-edit-publishes-one-exact-union
  (let* ((scene (make-stock-edit-scene))
         (domain (scene-domain scene))
         (existing (luft:make-site domain 7 7 7 luft:+cell-extent+))
         (added (luft:make-site domain 11 11 11 luft:+cell-extent+))
         (edit (luft:make-chain domain))
         (revision (scene-revision scene)))
    (luft:add-chain-site edit added)
    (apply-scene-edit
     scene edit
     :stock-cells (stock-cell-vector existing added)
     :stock-slots (stock-slot-vector 2 3))
    (ok (= (1+ revision) (scene-revision scene)))
    (ok (scene-exposed-cell-stock-p scene existing 2))
    (ok (scene-exposed-cell-stock-p scene added 3))
    (ok (= 2 (scene-cell-slot-nibble scene existing)))
    (ok (= 3 (scene-cell-slot-nibble scene added)))
    (multiple-value-bind (chunks cell-words slot-words available-p)
        (luft.render::scene-changes-since scene revision)
      (let ((added-index (luft.render::scene-cell-bit-index scene added))
            (existing-index
              (luft.render::scene-cell-bit-index scene existing)))
        (ok available-p)
        (ok (equalp #(0 1 2 4 7) chunks))
        (ok (equalp (vector (floor added-index 32)) cell-words))
        (ok (equalp
             (sort (vector (floor existing-index 8) (floor added-index 8)) #'<)
             slot-words))))
    (ok (scene-equals-fresh-reference-p scene))))

(deftest invalid-stock-edit-inputs-leave-every-scene-product-unmodified
  (let* ((scene (make-stock-edit-scene))
         (domain (scene-domain scene))
         (cell (luft:make-site domain 3 3 3 luft:+cell-extent+))
         (wide-domain (luft:make-world-domain :horizontal-bits 5))
         (noncanonical
           (luft:make-site wide-domain 16 3 3 luft:+cell-extent+))
         (face (luft:make-site domain 3 3 3 luft:+xy-face-extent+))
         (negative (luft:opposite-site cell)))
    (labels ((reject (target edit arguments)
               (let ((revision (scene-revision target))
                     (solid (luft:chain-sites (scene-solid target)))
                     (surface (luft:chain-sites (scene-surface target)))
                     (sites (copy-seq (luft.render::scene-site-pages target)))
                     (cell-bits (copy-seq (scene-cell-bits target)))
                     (slots (and (scene-slots target)
                                 (copy-seq (scene-slots target))))
                     (slot-words
                       (copy-seq (luft.render::scene-slot-words target)))
                     (changes (luft.render::scene-changes target)))
                 (ok (signals
                      (apply #'apply-scene-edit target edit arguments)
                      'error))
                 (ok (= revision (scene-revision target)))
                 (ok (equalp solid (luft:chain-sites (scene-solid target))))
                 (ok (equalp surface (luft:chain-sites (scene-surface target))))
                 (ok (equalp sites (luft.render::scene-site-pages target)))
                 (ok (equalp cell-bits (scene-cell-bits target)))
                 (ok (equalp slots (scene-slots target)))
                 (ok (equalp
                      slot-words (luft.render::scene-slot-words target)))
                 (ok (eq changes (luft.render::scene-changes target))))))
      (let ((empty (luft:make-chain domain)))
        (reject scene empty
                (list :stock-cells (stock-cell-vector cell)))
        (reject scene empty
                (list :stock-cells
                      (make-array 1 :element-type '(unsigned-byte 32)
                                    :initial-element 0)
                      :stock-slots (stock-slot-vector 1)))
        (reject scene empty
                (list :stock-cells (stock-cell-vector cell)
                      :stock-slots #(1)))
        (reject scene empty
                (list :stock-cells (stock-cell-vector cell cell)
                      :stock-slots (stock-slot-vector 1)))
        (reject scene empty
                (list :stock-cells (stock-cell-vector noncanonical)
                      :stock-slots (stock-slot-vector 1)))
        (reject scene empty
                (list :stock-cells (stock-cell-vector face)
                      :stock-slots (stock-slot-vector 1)))
        (reject scene empty
                (list :stock-cells (stock-cell-vector cell negative)
                      :stock-slots (stock-slot-vector 1 2)))
        (reject scene empty
                (list :stock-cells (stock-cell-vector cell)
                      :stock-slots (stock-slot-vector 4))))
      ;; A valid occupancy delta is still untouched when its stock half fails.
      (let ((edit (luft:make-chain domain)))
        (luft:add-chain-site
         edit (luft:make-site domain 11 11 11 luft:+cell-extent+))
        (reject scene edit
                (list :stock-cells (stock-cell-vector cell)
                      :stock-slots (stock-slot-vector 4))))
      (let ((slotless (make-scene domain)))
        (reject slotless (luft:make-chain domain)
                (list :stock-cells (stock-cell-vector cell)
                      :stock-slots (stock-slot-vector 0))))
      (let* ((wide-palette
               (make-array 17 :initial-element :stock))
             (slots (make-array (luft:chain-cell-bit-count domain)
                                :element-type '(unsigned-byte 8)
                                :initial-element 0))
             (too-wide
               (make-scene domain :slots slots :stocks wide-palette)))
        (reject too-wide (luft:make-chain domain)
                (list :stock-cells (stock-cell-vector cell)
                      :stock-slots (stock-slot-vector 16)))))))

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
  (let* ((world (make-world :horizontal-bits 4))
         ;; The global atelier may know more materials than one particular
         ;; world's four-bit palette.  Fill this world with any fifteen in
         ;; addition to its conventional turf slot.
         (names (subseq (remove :turf (material-names)) 0 15)))
    (ok (= 16 luft.render.shaders:+stock-slots+))
    ;; Slot zero is turf before anything asks for a slot at all.
    (ok (zerop (world-stock-slot world :turf)))
    ;; Each new stock takes the next slot, and asking twice is idempotent.
    (let ((slots (cons 0
                       (mapcar (lambda (name) (world-stock-slot world name))
                               names))))
      (ok (equal slots (mapcar (lambda (name) (world-stock-slot world name))
                               (cons :turf names))))
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

(deftest stock-table-packs-shape-grit-and-linear-emission-together
  ;; The ninth stock lane is deliberately shared across stages: the vertex
  ;; lattice reads X while the fragment material reads YZW.  Crystal owns
  ;; visible HDR emission; terminal is only its dark chassis and stays dark.
  (let* ((stocks #(:turf :granite :sand :terminal :tree :snow :crystal))
         (data (stock-table-data stocks)))
    (labels ((lane-start (name)
               (* 4
                  (+ (* (position name stocks)
                        luft.render.shaders:+stock-lanes+)
                     8)))
             (lane (name)
               (let ((start (lane-start name)))
                 (subseq data start (+ start 4)))))
      (let ((crystal (find-material :crystal))
            (terminal (find-material :terminal)))
        (ok (equalp (lane :crystal)
                    (coerce (cons (material-grit crystal)
                                  (material-emission crystal))
                            '(simple-array single-float (4)))))
        (ok (equalp (lane :terminal)
                    (coerce (cons (material-grit terminal)
                                  '(0.0 0.0 0.0))
                            '(simple-array single-float (4)))))
        (ok (some #'plusp (material-emission crystal)))
        (ok (every #'zerop (material-emission terminal)))))))

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
      (luft.render::standalone-render-options "clay")
    (ok (equal '(:clay :clay (:clay) nil)
               (list mode style pipelines effects))))
  (multiple-value-bind (mode style pipelines effects)
      (luft.render::standalone-render-options "full")
    (ok (eq :full mode))
    (ok (eq :stock style))
    (ok (equal '(:flat :bevel :chamfer :paper :stock :field :soft :ink :clay)
               pipelines))
    (ok (equal '(:sky :lens :taa) effects)))
  ;; A mode of its own selects only its own pipeline, the stock included.
  (multiple-value-bind (mode style pipelines effects)
      (luft.render::standalone-render-options "stock")
    (ok (equal '(:stock :stock (:stock) nil)
               (list mode style pipelines effects))))
  ;; And with nothing named at all, the atelier opens on the whole world.
  (multiple-value-bind (mode style)
      (luft.render::standalone-render-options nil)
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
           (ok (eq :presented
                   (surface-technique-output-space
                    (renderer-surface-technique renderer))))
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
           (ok (= 2 (renderer-last-scene-upload-writes renderer)))
           (ok (< (renderer-last-scene-upload-bytes renderer) full-bytes))
           (ok (= (scene-revision scene)
                  (luft.render::renderer-uploaded-scene-revision renderer))))
      (destroy-renderer renderer))))

(deftest surface-frame-states-have-independent-buffers-and-revision-cursors
  (let* ((scene (probe-scene))
         (renderer
           (make-renderer
            :scene scene
            :camera (make-fly-camera
                     :position (vec3:make-vec3 5.0 1.0 5.0)
                     :yaw 1.6 :pitch -0.6)
            :width 96 :height 64
            :style :flat :pipeline-styles '(:flat) :effects nil))
         (first (renderer-surface-frame-state renderer))
         (second
           (make-surface-frame-state
            (renderer-surface-technique renderer) :scene scene)))
    (unwind-protect
         (progn
           ;; A shared immutable technique must not imply shared mutable GPU
           ;; storage between acquired frames.
           (ok (not (eq (luft.render::surface-frame-state-uniform-buffer first)
                        (luft.render::surface-frame-state-uniform-buffer second))))
           (ok (not (eq (luft.render::surface-frame-state-sites-buffer first)
                        (luft.render::surface-frame-state-sites-buffer second))))
           (ok (not (eq (luft.render::surface-frame-state-cells-buffer first)
                        (luft.render::surface-frame-state-cells-buffer second))))
           (let ((shared-revision (scene-revision scene)))
             (ok (= shared-revision
                    (surface-frame-state-uploaded-scene-revision first)))
             (ok (= shared-revision
                    (surface-frame-state-uploaded-scene-revision second)))
             ;; FIRST advances while SECOND remains a valid independent
             ;; consumer cursor.  SECOND then folds both publications into
             ;; one incremental catch-up.
             (setf (scene-cell-p scene 5 4 1) t
                   (scene-cell-p scene 5 5 1) t)
             (synchronize-surface-frame-state first scene)
             (ok (= (scene-revision scene)
                    (surface-frame-state-uploaded-scene-revision first)))
             (ok (= shared-revision
                    (surface-frame-state-uploaded-scene-revision second)))
             (synchronize-surface-frame-state second scene)
             (ok (eq :incremental
                     (surface-frame-state-last-scene-upload-kind second)))
             (ok (= (scene-revision scene)
                    (surface-frame-state-uploaded-scene-revision second))))
           ;; Falling behind the bounded history is an ordinary full-upload
           ;; fallback local to the lagging state.
           (let ((luft.render::*scene-change-history-limit* 1))
             (setf (scene-cell-p scene 5 6 1) t
                   (scene-cell-p scene 5 7 1) t))
           (synchronize-surface-frame-state second scene)
           (ok (eq :full
                   (surface-frame-state-last-scene-upload-kind second)))
           (ok (= (scene-revision scene)
                  (surface-frame-state-uploaded-scene-revision second))))
      (destroy-surface-frame-state second)
      (destroy-renderer renderer))))

(deftest surface-frame-state-release-attempts-all-members-and-remains-retryable
  (let* ((technique (make-release-test-technique))
         (bind-group (make-instance 'surface-release-probe :name :bind-group))
         (sites (make-instance 'surface-release-probe
                               :name :sites :failures-remaining 1))
         (cells (make-instance 'surface-release-probe :name :cells))
         (stocks (make-instance 'surface-release-probe :name :stocks))
         (slots (make-instance 'surface-release-probe :name :slots))
         (uniform (make-instance 'surface-release-probe :name :uniform))
         (layout (make-instance 'surface-release-probe :name :layout))
         (state (make-instance 'surface-frame-state :technique technique))
         (scene (gensym "SCENE")))
    (setf (luft.render::surface-frame-state-bind-group state) bind-group
          (luft.render::surface-frame-state-sites-buffer state) sites
          (luft.render::surface-frame-state-cells-buffer state) cells
          (luft.render::surface-frame-state-stocks-buffer state) stocks
          (luft.render::surface-frame-state-slots-buffer state) slots
          (luft.render::surface-frame-state-uniform-buffer state) uniform
          (luft.render::surface-frame-state-sites-capacity state) 64
          (luft.render::surface-frame-state-cells-capacity state) 32
          (luft.render::surface-frame-state-slots-capacity state) 16
          (luft.render::surface-frame-state-uploaded-scene state) scene
          (luft.render::surface-frame-state-uploaded-scene-revision state) 7
          (surface-technique-layout technique) layout)
    (luft.render::register-surface-frame-state state)
    (let ((condition
            (handler-case
                (progn (destroy-surface-frame-state state) nil)
              (luft.render::surface-release-error (condition) condition))))
      (ok condition)
      (ok (= 1 (length
                (luft.render::surface-release-error-failures condition)))))
    ;; Every member was attempted even though SITES failed.  Successful
    ;; handles are forgotten immediately; only SITES remains retryable.
    (dolist (resource (list bind-group sites cells stocks slots uniform))
      (ok (= 1 (surface-release-probe-attempts resource))))
    (ok (eq sites (luft.render::surface-frame-state-sites-buffer state)))
    (ok (null (surface-frame-state-bind-group state)))
    (ok (null (luft.render::surface-frame-state-cells-buffer state)))
    (ok (null (luft.render::surface-frame-state-stocks-buffer state)))
    (ok (null (luft.render::surface-frame-state-slots-buffer state)))
    (ok (null (luft.render::surface-frame-state-uniform-buffer state)))
    (ok (eq scene (surface-frame-state-uploaded-scene state)))
    (ok (member state (luft.render::surface-technique-frame-states technique)
                :test #'eq))
    ;; The technique is the durable retry owner after an enclosing cache has
    ;; forgotten STATE: it finishes the state before invalidating its layout.
    (destroy-surface-technique technique)
    (ok (= 2 (surface-release-probe-attempts sites)))
    (ok (= 1 (surface-release-probe-attempts layout)))
    (ok (null (luft.render::surface-technique-frame-states technique)))
    (ok (null (surface-technique-layout technique)))
    (ok (zerop (luft.render::surface-frame-state-sites-capacity state)))
    (ok (null (surface-frame-state-uploaded-scene state)))
    ;; Fully released owners are idempotent and do not repeat native calls.
    (destroy-surface-technique technique)
    (ok (= 2 (surface-release-probe-attempts sites)))
    (ok (= 1 (surface-release-probe-attempts layout)))))

(deftest persistent-frame-state-release-blocks-technique-members
  (let* ((technique (make-release-test-technique))
         (resource
           (make-instance 'surface-release-probe
                          :name :persistent :failures-remaining 2))
         (layout (make-instance 'surface-release-probe :name :layout))
         (state (make-instance 'surface-frame-state :technique technique)))
    (setf (luft.render::surface-frame-state-uniform-buffer state) resource
          (surface-technique-layout technique) layout)
    (luft.render::register-surface-frame-state state)
    (ok (signals (destroy-surface-frame-state state)
                 'luft.render::surface-release-error))
    (ok (signals (destroy-surface-technique technique)
                 'luft.render::surface-release-error))
    (ok (= 2 (surface-release-probe-attempts resource)))
    (ok (member state (luft.render::surface-technique-frame-states technique)
                :test #'eq))
    (ok (zerop (surface-release-probe-attempts layout)))
    (ok (eq layout (surface-technique-layout technique)))
    ;; Let a later retry prove that the blocked owner can still finish.
    (destroy-surface-technique technique)
    (ok (= 3 (surface-release-probe-attempts resource)))
    (ok (= 1 (surface-release-probe-attempts layout)))))

(deftest standalone-renderer-aggregates-surface-release-and-retains-owners
  ;; One direct failure is reported, but the technique retries the registered
  ;; state in the same pass and lets the standalone owner finish completely.
  (multiple-value-bind (renderer state technique resource layout device)
      (make-release-test-renderer 1)
    (declare (ignore state technique))
    (let ((condition
            (handler-case
                (progn (destroy-renderer renderer) nil)
              (luft.render::surface-release-error (condition) condition))))
      (ok condition)
      (ok (= 1 (length
                (luft.render::surface-release-error-failures condition)))))
    (ok (= 2 (surface-release-probe-attempts resource)))
    (ok (= 1 (surface-release-probe-attempts layout)))
    (ok (= 1 (surface-release-probe-attempts device)))
    (ok (null (renderer-surface-frame-state renderer)))
    (ok (null (renderer-surface-technique renderer)))
    (ok (not (luft.render::renderer-owns-device-p renderer)))
    (destroy-renderer renderer)
    (ok (= 1 (surface-release-probe-attempts device))))
  ;; A failure which survives both state attempts blocks the technique and
  ;; device, retaining both renderer handles for the next owner retry.
  (multiple-value-bind (renderer state technique resource layout device)
      (make-release-test-renderer 2)
    (ok (signals (destroy-renderer renderer)
                 'luft.render::surface-release-error))
    (ok (= 2 (surface-release-probe-attempts resource)))
    (ok (eq state (renderer-surface-frame-state renderer)))
    (ok (eq technique (renderer-surface-technique renderer)))
    (ok (zerop (surface-release-probe-attempts layout)))
    (ok (zerop (surface-release-probe-attempts device)))
    (ok (luft.render::renderer-owns-device-p renderer))
    (destroy-renderer renderer)
    (ok (= 3 (surface-release-probe-attempts resource)))
    (ok (= 1 (surface-release-probe-attempts layout)))
    (ok (= 1 (surface-release-probe-attempts device)))
    (ok (null (renderer-surface-frame-state renderer)))
    (ok (null (renderer-surface-technique renderer)))
    (ok (not (luft.render::renderer-owns-device-p renderer)))))

(deftest surface-technique-release-aggregates-and-retains-failed-members
  (let* ((pipeline-failure
           (make-instance 'surface-release-probe
                          :name :pipeline-failure :failures-remaining 1))
         (pipeline-success
           (make-instance 'surface-release-probe :name :pipeline-success))
         (module-failure
           (make-instance 'surface-release-probe
                          :name :module-failure :failures-remaining 1))
         (module-success
           (make-instance 'surface-release-probe :name :module-success))
         (layout
           (make-instance 'surface-release-probe
                          :name :layout :failures-remaining 1))
         (technique (make-release-test-technique)))
    (setf (luft.render::surface-technique-pipelines technique)
          (list :stock pipeline-failure :flat pipeline-success)
          (luft.render::surface-technique-modules technique)
          (list module-failure module-success)
          (surface-technique-layout technique) layout)
    (let ((condition
            (handler-case
                (progn (destroy-surface-technique technique) nil)
              (luft.render::surface-release-error (condition) condition))))
      (ok condition)
      (ok (= 3 (length
                (luft.render::surface-release-error-failures condition)))))
    (dolist (resource (list pipeline-failure pipeline-success
                            module-failure module-success layout))
      (ok (= 1 (surface-release-probe-attempts resource))))
    (ok (equal (list :stock pipeline-failure)
               (luft.render::surface-technique-pipelines technique)))
    (ok (equal (list module-failure)
               (luft.render::surface-technique-modules technique)))
    (ok (eq layout (surface-technique-layout technique)))
    (destroy-surface-technique technique)
    (ok (= 2 (surface-release-probe-attempts pipeline-failure)))
    (ok (= 1 (surface-release-probe-attempts pipeline-success)))
    (ok (= 2 (surface-release-probe-attempts module-failure)))
    (ok (= 1 (surface-release-probe-attempts module-success)))
    (ok (= 2 (surface-release-probe-attempts layout)))
    (ok (null (luft.render::surface-technique-pipelines technique)))
    (ok (null (luft.render::surface-technique-modules technique)))
    (ok (null (surface-technique-layout technique)))
    (destroy-surface-technique technique)))

(deftest surface-constructor-unwind-preserves-creation-errors-and-retry-owners
  (let ((device
          (make-instance 'surface-construction-probe-device
                         :fail-on-create 4
                         :release-failure-indices '(2))))
    ;; :STOCK creates a layout, two modules, then its pipeline.  The pipeline
    ;; creation error must survive even when one cleanup member also fails.
    (let ((condition
            (handler-case
                (make-surface-technique
                 device :pipeline-styles '(:stock)
                 :target-formats '(:rgba16-float) :output-space :linear)
              (surface-technique-construction-error (condition)
                condition))))
      (ok condition)
      (ok (typep (surface-technique-construction-cause condition)
                 'surface-construction-probe-error))
      (let ((owner (surface-technique-construction-retry-owner condition)))
        (ok (luft.render::surface-technique-resources-live-p owner))
        (destroy-surface-technique owner)
        (ok (not (luft.render::surface-technique-resources-live-p owner)))))
    (ok (= 4 (surface-construction-create-attempts device)))
    (ok (= 3 (length (surface-construction-resources device))))
    (let ((failed
            (find 2 (surface-construction-resources device)
                  :key #'surface-release-probe-name)))
      (ok (= 2 (surface-release-probe-attempts failed)))
      (ok (surface-release-probe-released-p failed))))
  (let* ((device
           (make-instance 'surface-construction-probe-device
                          :fail-on-create 2
                          :release-failure-indices '(1)))
         (technique (make-release-test-technique device)))
    ;; A partial frame state has a durable retry owner even though its
    ;; constructor cannot return the state alongside the original error.
    (ok (signals (make-surface-frame-state technique)
                 'surface-construction-probe-error))
    (ok (= 1 (length
              (luft.render::surface-technique-frame-states technique))))
    (let ((resource (first (surface-construction-resources device))))
      (ok (= 1 (surface-release-probe-attempts resource)))
      (destroy-surface-technique technique)
      (ok (= 2 (surface-release-probe-attempts resource))))
    (ok (null (luft.render::surface-technique-frame-states technique)))))

(deftest standalone-construction-propagates-the-partial-technique-owner
  (let* ((device
           (make-instance 'surface-construction-probe-device
                          :fail-on-create 6
                          :release-failure-indices '(4 4)))
         (condition
           (handler-case
               (make-renderer
                :device device :scene nil :camera nil
                :style :stock :pipeline-styles '(:stock) :effects nil)
             (surface-technique-construction-error (condition)
               condition))))
    (ok condition)
    (ok (typep (surface-technique-construction-cause condition)
               'surface-construction-probe-error))
    (let* ((owner (surface-technique-construction-retry-owner condition))
           (resource
             (find 4 (surface-construction-resources device)
                   :key #'surface-release-probe-name)))
      ;; MAKE-SURFACE-TECHNIQUE tried once and MAKE-RENDERER's unwind tried
      ;; again.  The same propagated condition still roots the live third try.
      (ok (= 2 (surface-release-probe-attempts resource)))
      (ok (luft.render::surface-technique-resources-live-p owner))
      (destroy-surface-technique owner)
      (ok (= 3 (surface-release-probe-attempts resource)))
      (ok (not (luft.render::surface-technique-resources-live-p owner))))))

(deftest orthographic-shadow-frame-resources-release-independently-and-retry
  (let* ((technique
           (make-instance
            'surface-technique
            :device nil :pipeline-styles '(:stock)
            :target-formats '(:rgba16-float) :temporal-p nil
            :output-space :linear
            :orthographic-shadow-depth-format :depth32-float))
         (shadow-group
           (make-instance 'surface-release-probe
                          :name :shadow-group :failures-remaining 1))
         (group (make-instance 'surface-release-probe :name :group))
         (projector (make-instance 'surface-release-probe :name :projector))
         (uniform (make-instance 'surface-release-probe :name :uniform))
         (state (make-instance 'surface-frame-state :technique technique)))
    (setf (surface-frame-state-orthographic-shadow-bind-group state)
          shadow-group
          (surface-frame-state-bind-group state) group
          (surface-frame-state-orthographic-shadow-projector-buffer state)
          projector
          (luft.render::surface-frame-state-uniform-buffer state) uniform)
    (luft.render::register-surface-frame-state state)
    (ok (signals (destroy-surface-frame-state state)
                 'luft.render::surface-release-error))
    (ok (eq shadow-group
            (surface-frame-state-orthographic-shadow-bind-group state)))
    (ok (null (surface-frame-state-bind-group state)))
    (ok (null
         (surface-frame-state-orthographic-shadow-projector-buffer state)))
    (ok (= 1 (surface-release-probe-attempts projector)))
    (ok (member state (luft.render::surface-technique-frame-states technique)
                :test #'eq))
    ;; The technique remains the durable owner and retries the failed group
    ;; before retiring any of its own members.
    (destroy-surface-technique technique)
    (ok (= 2 (surface-release-probe-attempts shadow-group)))
    (ok (null (luft.render::surface-technique-frame-states technique)))))

(deftest frame-retirement-backlog-remains-owned-through-explicit-destroy
  (let* ((technique (make-release-test-technique))
         (layout (make-instance 'surface-release-probe :name :layout))
         (retired
           (make-instance 'surface-release-probe
                          :name :retired :failures-remaining 2))
         (state (make-instance 'surface-frame-state :technique technique)))
    (setf (surface-technique-layout technique) layout)
    (luft.render::register-surface-frame-state state)
    (ok (= 1
           (length
            (luft.render::retire-surface-frame-resources
             state (list (cons '(:retired-generation :probe) retired))))))
    (ok (= 1 (length
              (luft.render::surface-frame-state-retirements state))))
    (ok (luft.render::surface-frame-state-resources-live-p state))
    ;; The first technique attempt retries the backlog, retains the state, and
    ;; cannot invalidate a layout that the failed frame owner still names.
    (ok (signals (destroy-surface-technique technique)
                 'luft.render::surface-release-error))
    (ok (= 2 (surface-release-probe-attempts retired)))
    (ok (zerop (surface-release-probe-attempts layout)))
    (ok (member state (luft.render::surface-technique-frame-states technique)
                :test #'eq))
    ;; The next explicit owner retry drains the backlog and may retire layout.
    (destroy-surface-technique technique)
    (ok (= 3 (surface-release-probe-attempts retired)))
    (ok (= 1 (surface-release-probe-attempts layout)))
    (ok (null (luft.render::surface-frame-state-retirements state)))
    (ok (null (luft.render::surface-technique-frame-states technique)))))

(deftest orthographic-shadow-technique-members-are-retryable
  (let* ((technique
           (make-instance
            'surface-technique
            :device nil :pipeline-styles '(:stock)
            :target-formats '(:rgba16-float) :temporal-p nil
            :output-space :linear
            :orthographic-shadow-depth-format :depth32-float))
         (pipeline
           (make-instance 'surface-release-probe
                          :name :shadow-pipeline :failures-remaining 1))
         (module (make-instance 'surface-release-probe :name :shadow-module))
         (layout
           (make-instance 'surface-release-probe
                          :name :shadow-layout :failures-remaining 1)))
    (setf (surface-technique-orthographic-shadow-pipeline technique) pipeline
          (luft.render::surface-technique-orthographic-shadow-module technique)
          module
          (luft.render::surface-technique-orthographic-shadow-layout technique)
          layout)
    (ok (signals (destroy-surface-technique technique)
                 'luft.render::surface-release-error))
    (ok (eq pipeline
            (surface-technique-orthographic-shadow-pipeline technique)))
    (ok (null
         (luft.render::surface-technique-orthographic-shadow-module technique)))
    (ok (eq layout
            (luft.render::surface-technique-orthographic-shadow-layout
             technique)))
    (dolist (resource (list pipeline module layout))
      (ok (= 1 (surface-release-probe-attempts resource))))
    (destroy-surface-technique technique)
    (ok (= 2 (surface-release-probe-attempts pipeline)))
    (ok (= 1 (surface-release-probe-attempts module)))
    (ok (= 2 (surface-release-probe-attempts layout)))
    (ok (null
         (surface-technique-orthographic-shadow-pipeline technique)))
    (ok (null
         (luft.render::surface-technique-orthographic-shadow-layout
          technique)))))

(deftest shadow-frame-constructor-keeps-original-error-and-a-retry-owner
  (let* ((device
           (make-instance 'surface-construction-probe-device
                          :fail-on-create 3
                          :release-failure-indices '(2)))
         (technique
           (make-instance
            'surface-technique
            :device device :pipeline-styles '(:stock)
            :target-formats '(:rgba16-float) :temporal-p nil
            :output-space :linear
            :orthographic-shadow-depth-format :depth32-float)))
    ;; Uniform and stocks exist when projector creation fails.  Their cleanup
    ;; cannot replace the projector's original construction condition.
    (ok (signals (make-surface-frame-state technique)
                 'surface-construction-probe-error))
    (ok (= 1 (length
              (luft.render::surface-technique-frame-states technique))))
    (let ((stocks
            (find 2 (surface-construction-resources device)
                  :key #'surface-release-probe-name)))
      (ok stocks)
      (ok (= 1 (surface-release-probe-attempts stocks)))
      (destroy-surface-technique technique)
      (ok (= 2 (surface-release-probe-attempts stocks))))
    (ok (null (luft.render::surface-technique-frame-states technique)))))

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

(defun shader-specification-calls-p (specification function-name)
  "Whether SPECIFICATION's typed graph calls shader FUNCTION-NAME."
  (let ((definition
          (luv.shader:shader-function-definition-for function-name)))
    (find definition (luv.shader:shader-specification-expressions specification)
          :test #'eq
          :key (lambda (expression)
                 (and (typep expression 'luv.shader:shader-function-call)
                      (luv.shader:shader-function-call-definition
                       expression))))))

(deftest orthographic-shadow-reuses-stock-geometry-with-a-four-row-projector
  (let* ((specification
           (luft.render.shaders:orthographic-stock-shadow-vertex-shader))
         (resources
           (luv.shader:shader-specification-resources specification))
         (projector
           (find luft.render.shaders:+orthographic-shadow-projector-binding+
                 resources :key #'luv.shader:shader-resource-binding)))
    (ok (eq :vertex
            (luv.shader:shader-specification-stage specification)))
    (ok (= 1 (length
              (luv.shader:shader-specification-outputs specification))))
    (ok (eq :position
            (luv.shader:shader-interface-built-in
             (first
              (luv.shader:shader-specification-outputs specification)))))
    (ok (equal '(0 1 2 3 4 5)
               (mapcar #'luv.shader:shader-resource-binding resources)))
    (ok (typep projector 'luv.shader:shader-uniform-block))
    (ok (= 64 (luv.shader:shader-uniform-block-byte-size projector)))
    (ok (equal '("SHADOW-PROJECTOR-ROW-0"
                 "SHADOW-PROJECTOR-ROW-1"
                 "SHADOW-PROJECTOR-ROW-2"
                 "SHADOW-PROJECTOR-ROW-3")
               (mapcar
                (lambda (member)
                  (symbol-name (luv.shader:shader-object-name member)))
                (luv.shader:shader-uniform-block-members projector))))
    ;; Camera binding zero still chooses the nearest periodic image; only
    ;; final projection and face visibility differ from the stock pass.
    (ok (shader-specification-calls-p
         specification 'luft.render.shaders::camera-torus-anchor))
    (ok (shader-specification-calls-p
         specification 'luft.render.shaders:deform-point))
    (ok (not (shader-specification-calls-p
              specification 'luft.render.shaders::view-clip)))
    (ok (not (shader-specification-calls-p
              specification 'luft.render.shaders::jitter-clip)))))

(deftest linear-stock-radiance-leaves-presentation-to-its-frame-owner
  (let ((linear
          (luft.render.shaders:linear-stock-fragment-shader))
        (presented
          (luft.render.shaders:stock-fragment-shader)))
    (ok (= 1 (length (luv.shader:shader-specification-outputs linear))))
    (ok (shader-specification-calls-p
         linear 'luft.render.shaders::stock-radiance))
    (dolist (presentation
              '(luft.render.shaders::stock-lighting
                luft.render.shaders::present-stock-radiance
                luft.render.shaders::paper-tonemap))
      (ok (not (shader-specification-calls-p linear presentation))))
    ;; The standalone renderer still owns its original paper presentation.
    (ok (shader-specification-calls-p
         presented 'luft.render.shaders::stock-lighting))))

(deftest linear-surface-techniques-refuse-inexact-combinations-before-gpu-work
  (ok (signals
       (make-surface-technique
        nil :pipeline-styles '(:stock) :output-space :display)))
  (ok (signals
       (make-surface-technique
        nil :pipeline-styles '(:stock :flat) :output-space :linear)))
  (ok (signals
       (make-surface-technique
        nil :pipeline-styles '(:stock)
        :target-formats '(:rgba16-float :rg16-float)
        :temporal-p t :output-space :linear))))

#+darwin
(deftest linear-stock-surface-technique-compiles-for-an-embedded-metal-hdr-pass
  (let ((device
          (luv:request-gpu-device (make-instance 'luv:metal-gpu-provider)))
        (technique nil))
    (unwind-protect
         (progn
           (setf technique
                 (make-surface-technique
                  device
                  :pipeline-styles '(:stock)
                  :target-formats '(:rgba16-float)
                  :output-space :linear))
           (ok (eq :linear (surface-technique-output-space technique)))
           (ok (equal '(:stock)
                      (surface-technique-pipeline-styles technique)))
           (ok (equal '(:rgba16-float)
                      (surface-technique-target-formats technique)))
           (ok (not (surface-technique-temporal-p technique)))
           ;; Pipeline creation is where Metal lowers and compiles both
           ;; mathematical shader modules for this exact attachment format.
           (ok (typep (surface-technique-pipeline technique :stock)
                      'luv:metal-gpu-render-pipeline)))
      (when technique (destroy-surface-technique technique))
      (luv:destroy device))))

#+darwin
(deftest orthographic-stock-shadow-compiles-with-independent-metal-frames
  (let ((device
          (luv:request-gpu-device (make-instance 'luv:metal-gpu-provider)))
        (technique nil)
        (first nil)
        (second nil))
    (unwind-protect
         (let ((scene (probe-scene)))
           (setf technique
                 (make-surface-technique
                  device
                  :pipeline-styles '(:stock)
                  :target-formats '(:rgba16-float)
                  :output-space :linear
                  :orthographic-shadow-depth-format :depth32-float))
           (ok (eq :depth32-float
                   (surface-technique-orthographic-shadow-depth-format
                    technique)))
           ;; Pipeline creation lowers the position-only shader and links a
           ;; real Metal vertex-only depth pipeline with no color targets.
           (ok (typep
                (surface-technique-orthographic-shadow-pipeline technique)
                'luv:metal-gpu-render-pipeline))
           (setf first (make-surface-frame-state technique :scene scene)
                 second (make-surface-frame-state technique :scene scene))
           (ok (not
                (eq
                 (surface-frame-state-orthographic-shadow-projector-buffer
                  first)
                 (surface-frame-state-orthographic-shadow-projector-buffer
                  second))))
           (ok (not
                (eq (surface-frame-state-orthographic-shadow-bind-group first)
                    (surface-frame-state-orthographic-shadow-bind-group
                     second))))
           (let ((projector
                   (make-array 16 :element-type 'single-float
                                  :initial-element 0.0)))
             (setf (aref projector 0) 1.0
                   (aref projector 5) 1.0
                   (aref projector 10) 1.0
                   (aref projector 15) 1.0)
             (write-surface-shadow-projector first projector)
             (write-surface-shadow-projector second projector)))
      (when second (destroy-surface-frame-state second))
      (when first (destroy-surface-frame-state first))
      (when technique (destroy-surface-technique technique))
      (luv:destroy device))))

(deftest surface-vertices-lift-packed-sites-to-the-camera-nearest-torus-image
  (dolist (specification
            (list (luft.render.shaders:surface-vertex-shader)
                  (luft.render.shaders:stock-vertex-shader)))
    (ok (shader-specification-calls-p
         specification 'luft.render.shaders::camera-torus-anchor))))


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

(defun shadow-publication-scene ()
  "A one-chunk seed in a domain with room to force later page growth."
  (let* ((domain (luft:make-world-domain :horizontal-bits 5))
         (solid (luft:make-solid-chain domain)))
    (setf (luft:solid-cell-p solid 1 1 0) t)
    (make-scene domain :solid solid)))

(deftest shadow-and-surface-bind-groups-publish-as-one-frame-generation
  (let* ((device (make-instance 'surface-publication-probe-device))
         (technique
           (make-instance
            'surface-technique
            :device device :pipeline-styles '(:stock)
            :target-formats '(:rgba16-float) :temporal-p nil
            :output-space :linear
            :orthographic-shadow-depth-format :depth32-float))
         (state (make-instance 'surface-frame-state :technique technique))
         (scene (shadow-publication-scene)))
    (setf (luft.render::surface-frame-state-uniform-buffer state)
          (make-instance 'surface-release-probe :name :uniform)
          (luft.render::surface-frame-state-stocks-buffer state)
          (make-instance 'surface-release-probe :name :stocks)
          (surface-frame-state-orthographic-shadow-projector-buffer state)
          (make-instance 'surface-release-probe :name :projector))
    (luft.render::register-surface-frame-state state)
    (unwind-protect
         (progn
           (synchronize-surface-frame-state state scene)
           (write-surface-shadow-projector
            state (make-array 16 :element-type 'single-float
                                 :initial-element 0.0))
           (let ((ordinary (surface-frame-state-bind-group state))
                 (shadow
                   (surface-frame-state-orthographic-shadow-bind-group state)))
             ;; A same-page edit is sparse: neither group nor any buffer
             ;; binding moves under the frame in flight.
             (setf (scene-cell-p scene 2 1 0) t)
             (synchronize-surface-frame-state state scene)
             (ok (eq :incremental
                     (surface-frame-state-last-scene-upload-kind state)))
             (ok (eq ordinary (surface-frame-state-bind-group state)))
             (ok (eq shadow
                     (surface-frame-state-orthographic-shadow-bind-group state)))
             (let ((sites
                     (luft.render::surface-frame-state-sites-buffer state))
                   (cells
                     (luft.render::surface-frame-state-cells-buffer state))
                   (slots
                     (luft.render::surface-frame-state-slots-buffer state)))
               ;; Allocate face pages in many new chunks.  If shadow-group
               ;; creation fails, the unpublished ordinary group and buffers
               ;; are retired while every old live handle remains installed.
               (dolist (x '(8 16 24))
                 (dolist (y '(8 16 24))
                   (setf (scene-cell-p scene x y 0) t)))
               (setf (surface-publication-fail-shadow-bind-group-p device) t
                     (surface-publication-release-failure-labels device)
                     '("luft surface bindings" "luft surface sites"))
               (ok (signals (synchronize-surface-frame-state state scene)
                            'surface-construction-probe-error))
               (ok (eq ordinary (surface-frame-state-bind-group state)))
               (ok (eq shadow
                       (surface-frame-state-orthographic-shadow-bind-group
                        state)))
               (ok (eq sites
                       (luft.render::surface-frame-state-sites-buffer state)))
               (ok (eq cells
                       (luft.render::surface-frame-state-cells-buffer state)))
               (ok (eq slots
                       (luft.render::surface-frame-state-slots-buffer state)))
               (ok (= 2 (length
                         (luft.render::surface-frame-state-retirements state))))
               (ok (null
                    (set-exclusive-or
                     '((:unpublished :bind-group)
                       (:unpublished :sites-buffer))
                     (mapcar
                      #'luft.render::surface-frame-retirement-label
                      (luft.render::surface-frame-state-retirements state))
                     :test #'equal)))
               ;; The next synchronization first drains the unpublished
               ;; candidate debt.  It then publishes both new groups together.
               (setf (surface-publication-fail-shadow-bind-group-p device) nil
                     (surface-publication-release-failure-labels device) nil
                     (surface-release-probe-failures-remaining ordinary) 1)
               ;; Publication succeeds even though one member of the old
               ;; generation cannot retire.  That labelled handle stays owned
               ;; and the release condition reports the debt after publication.
               (ok (signals (synchronize-surface-frame-state state scene)
                            'luft.render::surface-release-error))
               (ok (eq :full
                       (surface-frame-state-last-scene-upload-kind state)))
               (ok (not (eq ordinary
                            (surface-frame-state-bind-group state))))
               (ok (not
                    (eq shadow
                        (surface-frame-state-orthographic-shadow-bind-group
                         state))))
               (ok (not (surface-release-probe-released-p ordinary)))
               (ok (surface-release-probe-released-p shadow))
               (ok (= 1 (length
                         (luft.render::surface-frame-state-retirements state))))
               (ok (equal
                    '(:retired-generation :bind-group)
                    (luft.render::surface-frame-retirement-label
                     (first
                      (luft.render::surface-frame-state-retirements state)))))
               ;; An otherwise up-to-date synchronize is still a retirement
               ;; service point and converges without another upload.
               (synchronize-surface-frame-state state scene)
               (ok (surface-release-probe-released-p ordinary))
               (ok (null
                    (luft.render::surface-frame-state-retirements state))))))
      (setf (surface-publication-fail-shadow-bind-group-p device) nil)
      (destroy-surface-frame-state state)
      (destroy-surface-technique technique))))

(deftest failed-full-upload-write-cannot-corrupt-the-published-generation
  (let* ((device (make-instance 'surface-publication-probe-device))
         (technique
           (make-instance
            'surface-technique
            :device device :pipeline-styles '(:stock)
            :target-formats '(:rgba16-float) :temporal-p nil
            :output-space :linear
            :orthographic-shadow-depth-format :depth32-float))
         (state (make-instance 'surface-frame-state :technique technique))
         (old-scene (shadow-publication-scene))
         (new-scene (shadow-publication-scene)))
    (setf (luft.render::surface-frame-state-uniform-buffer state)
          (make-instance 'surface-release-probe :name :uniform)
          (luft.render::surface-frame-state-stocks-buffer state)
          (make-instance 'surface-release-probe :name :stocks)
          (surface-frame-state-orthographic-shadow-projector-buffer state)
          (make-instance 'surface-release-probe :name :projector))
    (luft.render::register-surface-frame-state state)
    (unwind-protect
         (progn
           (synchronize-surface-frame-state state old-scene)
           (let ((ordinary (surface-frame-state-bind-group state))
                 (shadow
                   (surface-frame-state-orthographic-shadow-bind-group state))
                 (sites
                   (luft.render::surface-frame-state-sites-buffer state))
                 (cells
                   (luft.render::surface-frame-state-cells-buffer state))
                 (slots
                   (luft.render::surface-frame-state-slots-buffer state))
                 (revision
                   (surface-frame-state-uploaded-scene-revision state)))
             ;; NEW-SCENE fits the old capacities.  A full upload must still
             ;; use fresh buffers: failure on write two of three cannot touch
             ;; the generation which still advertises OLD-SCENE.
             (setf (surface-publication-write-attempts device) 0
                   (surface-publication-fail-write-at device) 2)
             (ok (signals (synchronize-surface-frame-state state new-scene)
                          'surface-construction-probe-error))
             (ok (eq old-scene
                     (surface-frame-state-uploaded-scene state)))
             (ok (= revision
                    (surface-frame-state-uploaded-scene-revision state)))
             (ok (eq ordinary (surface-frame-state-bind-group state)))
             (ok (eq shadow
                     (surface-frame-state-orthographic-shadow-bind-group state)))
             (ok (eq sites
                     (luft.render::surface-frame-state-sites-buffer state)))
             (ok (eq cells
                     (luft.render::surface-frame-state-cells-buffer state)))
             (ok (eq slots
                     (luft.render::surface-frame-state-slots-buffer state)))
             (ok (null
                  (luft.render::surface-frame-state-retirements state)))
             ;; The requested new scene is safely rejected by draw until a
             ;; complete cohort has actually been published.
             (ok (signals (draw-surface-frame nil state new-scene :stock)))
             (setf (surface-publication-write-attempts device) 0
                   (surface-publication-fail-write-at device) nil)
             (synchronize-surface-frame-state state new-scene)
             (ok (eq new-scene
                     (surface-frame-state-uploaded-scene state)))
             (ok (not (eq ordinary
                          (surface-frame-state-bind-group state))))
             (ok (not (eq shadow
                          (surface-frame-state-orthographic-shadow-bind-group
                           state))))
             (ok (not
                  (eq sites
                      (luft.render::surface-frame-state-sites-buffer state))))))
      (setf (surface-publication-fail-write-at device) nil)
      (destroy-surface-frame-state state)
      (destroy-surface-technique technique))))

(deftest disabled-shadow-capability-allocates-nothing-and-guards-its-api
  (let* ((technique (make-release-test-technique))
         (state (make-instance 'surface-frame-state :technique technique))
         (scene (probe-scene)))
    (ok (null
         (surface-technique-orthographic-shadow-depth-format technique)))
    (ok (null
         (surface-technique-orthographic-shadow-pipeline technique)))
    (ok (null
         (surface-frame-state-orthographic-shadow-projector-buffer state)))
    (ok (null
         (surface-frame-state-orthographic-shadow-bind-group state)))
    (ok (signals
         (write-surface-shadow-projector
          state (make-array 16 :element-type 'single-float
                               :initial-element 0.0))))
    (ok (signals (draw-surface-shadow-frame nil state scene)))))

(deftest shadow-projector-write-requires-an-exact-simple-float-block
  (let* ((technique
           (make-instance
            'surface-technique
            :device nil :pipeline-styles '(:stock)
            :target-formats '(:rgba16-float) :temporal-p nil
            :output-space :linear
            :orthographic-shadow-depth-format :depth32-float))
         (state (make-instance 'surface-frame-state :technique technique))
         (projector (make-instance 'surface-release-probe :name :projector)))
    (setf (surface-frame-state-orthographic-shadow-projector-buffer state)
          projector)
    (luft.render::register-surface-frame-state state)
    (unwind-protect
         (progn
           (ok (signals
                (write-surface-shadow-projector
                 state (make-array 16 :element-type '(unsigned-byte 32)
                                      :initial-element 0))))
           (ok (signals
                (write-surface-shadow-projector
                 state (make-array 15 :element-type 'single-float
                                      :initial-element 0.0))))
           (ok (signals
                (write-surface-shadow-projector
                 state (make-array 16 :element-type 'single-float
                                      :initial-element 0.0
                                      :adjustable t))))
           (ok (zerop (surface-release-probe-writes projector)))
           (write-surface-shadow-projector
            state (make-array 16 :element-type 'single-float
                                 :initial-element 0.0))
           (ok (= 1 (surface-release-probe-writes projector))))
      (destroy-surface-frame-state state)
      (destroy-surface-technique technique))))

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
