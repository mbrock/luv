(in-package #:luvcraft.tests)

(defvar *counting-solid-dispatches* 0)

(defclass counting-solid-block (block-kind) ()
  (:metaclass luv.arithmetic.records:quantity-class))

(defclass failing-solid-block (block-kind) ()
  (:metaclass luv.arithmetic.records:quantity-class))

(defclass mutable-solid-block (block-kind) ()
  (:metaclass luv.arithmetic.records:quantity-class))

(defvar *mutable-block-solid-p* t)
(defvar *mutable-block-solid-error-p* nil)

(defmethod block-solid-p ((block counting-solid-block))
  (incf *counting-solid-dispatches*)
  t)

(defmethod block-solid-p ((block failing-solid-block))
  (declare (ignore block))
  (error "Deliberate LUFT solid-table construction failure."))

(defmethod block-solid-p ((block mutable-solid-block))
  (declare (ignore block))
  (when *mutable-block-solid-error-p*
    (error "Deliberate post-attach LUFT solidity failure."))
  *mutable-block-solid-p*)

(defun make-counting-solid-block ()
  (make-instance 'counting-solid-block
                 :name :counting-solid
                 :face-tiles '(:all :stone)))

(defun luft-world-scenes-agree-p (left right)
  (and (equalp (luft:chain-sites (luft.render:scene-solid left))
               (luft:chain-sites (luft.render:scene-solid right)))
       (equalp (luft:chain-sites (luft.render:scene-surface left))
               (luft:chain-sites (luft.render:scene-surface right)))
       (equalp (luft.render:scene-cell-bits left)
               (luft.render:scene-cell-bits right))))

(deftest luvcraft-cells-map-to-wrapped-z-up-luft-sites
  (let* ((world (make-block-world))
         (materialization
           (luvcraft::make-luft-world-materialization
            world :horizontal-bits 2 :vertical-origin -3))
         (site (luvcraft::luft-world-cell-site
                materialization -1 -2 4 -1)))
    (ok (= 3 (luft:site-x site)))
    (ok (= 0 (luft:site-y site)))
    (ok (= 1 (luft:site-z site)))
    (ok (= luft:+cell-extent+ (luft:site-extent site)))
    (ok (luft:site-negative-p site))))

(deftest attaching-materializes-existing-solids-with-one-palette-dispatch
  (let* ((world (make-block-world :chunk-width 2
                                  :chunk-height 2
                                  :chunk-depth 2))
         (chunk (ensure-world-chunk world -1 0 1))
         (block (make-counting-solid-block)))
    (dotimes (offset 8)
      (setf (chunk-block-at-offset chunk offset) block))
    (let* ((*counting-solid-dispatches* 0)
           (materialization
             (luvcraft::attach-luft-world-materialization
              world :horizontal-bits 4))
           (scene
             (luvcraft::luft-world-materialization-scene materialization)))
      (ok (= 1 *counting-solid-dispatches*))
      (ok (= 8
             (hash-table-count
              (luvcraft::luft-world-materialization-pending-solid-cells
               materialization))))
      (multiple-value-bind (published changed)
          (luvcraft::reconcile-luft-world-materialization materialization)
        (ok (eq published scene))
        (ok (= 8 changed)))
      (ok (luft.render:scene-cell-p scene 14 2 0))
      (ok (luft.render:scene-cell-p scene 15 3 1))
      (ok (equalp #(-1 3)
                  (luvcraft::luft-world-materialization-resident-center
                   materialization))))))

(deftest solid-dispatch-is-cached-once-across-a-world-vocabulary
  (let* ((world (make-block-world :chunk-width 1
                                  :chunk-height 1
                                  :chunk-depth 1))
         (first (ensure-world-chunk world 0 0 0))
         (second (ensure-world-chunk world 1 0 0))
         (block (make-counting-solid-block)))
    (setf (chunk-block-at first 0 0 0) block
          (chunk-block-at second 0 0 0) block)
    (let* ((*counting-solid-dispatches* 0)
           (materialization
             (luvcraft::attach-luft-world-materialization
              world :horizontal-bits 3)))
      (ok (= 1 *counting-solid-dispatches*))
      (multiple-value-bind (scene changed)
          (luvcraft::reconcile-luft-world-materialization materialization)
        (ok (= 2 changed))
        (ok (luft.render:scene-cell-p scene 0 0 0))
        (ok (luft.render:scene-cell-p scene 1 0 0))))))

(deftest non-cubic-chunks-use-their-domain-offset-order
  (let* ((world (make-block-world :chunk-width 2
                                  :chunk-height 3
                                  :chunk-depth 4))
         (chunk (ensure-world-chunk world -1 2 3)))
    ;; Local (1,2,3) is offset 1 + 2*(2 + 3*3) = 23, the last dense cell.
    (setf (chunk-block-at-offset chunk 23) luvcraft::*stone-block*)
    (let ((materialization
            (luvcraft::attach-luft-world-materialization
             world :horizontal-bits 6)))
      (multiple-value-bind (scene changed)
          (luvcraft::reconcile-luft-world-materialization materialization)
        (ok (= 1 changed))
        ;; World (-1,8,15) becomes wrapped LUFT (63,15,8).
        (ok (luft.render:scene-cell-p scene 63 15 8))))))

(deftest failed-initial-scan-does-not-leave-a-world-observer
  (let* ((world (make-block-world :chunk-width 1
                                  :chunk-height 1
                                  :chunk-depth 1))
         (chunk (ensure-world-chunk world 0 0 0))
         (block (make-instance 'failing-solid-block
                               :name :failing-solid
                               :face-tiles '(:all :stone))))
    (setf (chunk-block-at chunk 0 0 0) block)
    (ok (signals (luvcraft::attach-luft-world-materialization world) 'error))
    (ok (zerop (length (block-world-observers world))))))

(deftest oversized-dense-luft-worlds-are-rejected-before-allocation
  (ok (signals
       (luvcraft::make-luft-world-materialization
        (make-block-world)
        :horizontal-bits
        (1+ luvcraft::+luft-world-maximum-horizontal-bits+))
       'error)))

(deftest explicit-solidity-invalidation-rebuilds-in-one-publication
  (let* ((*mutable-block-solid-p* t)
         (*mutable-block-solid-error-p* nil)
         (world (make-block-world :chunk-width 2
                                  :chunk-height 1
                                  :chunk-depth 1))
         (chunk (ensure-world-chunk world 0 0 0))
         (block (make-instance 'mutable-solid-block
                               :name :mutable-solid
                               :face-tiles '(:all :stone))))
    (setf (chunk-block-at chunk 0 0 0) block
          (chunk-block-at chunk 1 0 0) block)
    (let* ((materialization
             (luvcraft::attach-luft-world-materialization
              world :horizontal-bits 3))
           (scene
             (luvcraft::luft-world-materialization-scene materialization)))
      (luvcraft::reconcile-luft-world-materialization materialization)
      (ok (luft.render:scene-cell-p scene 0 0 0))
      (ok (luft.render:scene-cell-p scene 1 0 0))
      (let ((revision (luft.render:scene-revision scene)))
        (setf *mutable-block-solid-p* nil)
        (ok (eq materialization
                (luvcraft::invalidate-luft-world-solidity materialization)))
        (multiple-value-bind (published changed)
            (luvcraft::reconcile-luft-world-materialization materialization)
          (ok (eq published scene))
          (ok (= 2 changed)))
        (ok (= (1+ revision) (luft.render:scene-revision scene)))
        (ok (not (luft.render:scene-cell-p scene 0 0 0)))
        (ok (not (luft.render:scene-cell-p scene 1 0 0)))
        (ok (not
             (luvcraft::luft-world-materialization-solidity-rebuild-required-p
              materialization)))))))

(deftest post-attach-classification-errors-recover-transactionally
  (let* ((*mutable-block-solid-p* t)
         (*mutable-block-solid-error-p* nil)
         (world (make-block-world :chunk-width 1
                                  :chunk-height 1
                                  :chunk-depth 1))
         (chunk (ensure-world-chunk world 0 0 0))
         (materialization
           (luvcraft::attach-luft-world-materialization
            world :horizontal-bits 3))
         (scene
           (luvcraft::luft-world-materialization-scene materialization))
         (block (make-instance 'mutable-solid-block
                               :name :recoverable-solid
                               :face-tiles '(:all :stone))))
    (luvcraft::reconcile-luft-world-materialization materialization)
    (let ((old-chunks
            (luvcraft::luft-world-materialization-resident-chunk-solids
             materialization))
          (old-counts
            (luvcraft::luft-world-materialization-resident-solid-counts
             materialization))
          (revision (luft.render:scene-revision scene)))
      (setf *mutable-block-solid-error-p* t)
      (ok (signals (setf (chunk-block-at chunk 0 0 0) block) 'error))
      ;; Authored content was already written before observer classification.
      (ok (eq block (chunk-block-at chunk 0 0 0)))
      (ok
       (luvcraft::luft-world-materialization-solidity-rebuild-required-p
        materialization))
      ;; A failed candidate cannot disturb the last coherent dense state.
      (ok (signals
           (luvcraft::reconcile-luft-world-materialization materialization)
           'error))
      (ok (eq old-chunks
              (luvcraft::luft-world-materialization-resident-chunk-solids
               materialization)))
      (ok (eq old-counts
              (luvcraft::luft-world-materialization-resident-solid-counts
               materialization)))
      (ok (= revision (luft.render:scene-revision scene)))
      (ok (not (luft.render:scene-cell-p scene 0 0 0)))
      ;; Repairing the semantic method lets the next reconciliation recover.
      (setf *mutable-block-solid-error-p* nil)
      (multiple-value-bind (published changed)
          (luvcraft::reconcile-luft-world-materialization materialization)
        (ok (eq published scene))
        (ok (= 1 changed)))
      (ok (= (1+ revision) (luft.render:scene-revision scene)))
      (ok (luft.render:scene-cell-p scene 0 0 0))
      (ok (not
           (luvcraft::luft-world-materialization-solidity-rebuild-required-p
            materialization))))))

(deftest reconciliation-coalesces-authored-edits-into-one-revision
  (let* ((world (make-block-world :chunk-width 2
                                  :chunk-height 2
                                  :chunk-depth 2))
         (chunk (ensure-world-chunk world 0 0 0))
         (materialization
           (luvcraft::attach-luft-world-materialization
            world :horizontal-bits 3))
         (scene
           (luvcraft::luft-world-materialization-scene materialization)))
    (luvcraft::reconcile-luft-world-materialization materialization)
    (let ((revision (luft.render:scene-revision scene)))
      ;; The latest desired state for one authored cell wins, while the
      ;; publication remains one sparse LUFT revision.
      (setf (chunk-block-at chunk 1 0 0) luvcraft::*stone-block*)
      (setf (chunk-block-at chunk 1 0 0) nil)
      (setf (chunk-block-at chunk 1 0 0) luvcraft::*stone-block*)
      (multiple-value-bind (published changed)
          (luvcraft::reconcile-luft-world-materialization materialization)
        (ok (eq published scene))
        (ok (= 1 changed)))
      (ok (= (1+ revision) (luft.render:scene-revision scene)))
      (ok (luft.render:scene-cell-p scene 1 0 0))
      ;; Empty reconciliation and solid-to-solid authorship are no-ops.
      (let ((center
              (luvcraft::luft-world-materialization-resident-center
               materialization)))
        (luvcraft::reconcile-luft-world-materialization materialization)
        (setf (chunk-block-at chunk 1 0 0) luvcraft::*stone-block*)
        (luvcraft::reconcile-luft-world-materialization materialization)
        (ok (eq center
                (luvcraft::luft-world-materialization-resident-center
                 materialization))))
      (ok (= (1+ revision) (luft.render:scene-revision scene))))))

(deftest arrivals-departures-and-wrapped-replacements-stay-exact
  (let* ((world (make-block-world :chunk-width 2
                                  :chunk-height 2
                                  :chunk-depth 2))
         (first (ensure-world-chunk world 0 0 0)))
    (setf (chunk-block-at first 0 0 0) luvcraft::*stone-block*)
    (let* ((materialization
             (luvcraft::attach-luft-world-materialization
              world :horizontal-bits 2))
           (scene
             (luvcraft::luft-world-materialization-scene materialization)))
      (luvcraft::reconcile-luft-world-materialization materialization)
      (ok (luft.render:scene-cell-p scene 0 0 0))
      (let ((revision (luft.render:scene-revision scene)))
        ;; The old cell at world X=0 departs before its torus alias at X=4
        ;; arrives.  Its resident TRUE desire wins over the departed FALSE.
        (remove-world-chunk world 0 0 0)
        (let ((replacement (ensure-world-chunk world 2 0 0)))
          (setf (chunk-block-at replacement 0 0 0)
                luvcraft::*stone-block*))
        (multiple-value-bind (published changed)
            (luvcraft::reconcile-luft-world-materialization materialization)
          (ok (eq published scene))
          (ok (zerop changed)))
        (ok (= revision (luft.render:scene-revision scene)))
        (ok (luft.render:scene-cell-p scene 0 0 0)))
      (remove-world-chunk world 2 0 0)
      (multiple-value-bind (published changed)
          (luvcraft::reconcile-luft-world-materialization materialization)
        (ok (eq published scene))
        (ok (= 1 changed)))
      (ok (not (luft.render:scene-cell-p scene 0 0 0)))
      (ok (null
           (luvcraft::luft-world-materialization-resident-center
            materialization))))))

(deftest transient-wrapped-alias-cannot-erase-an-established-resident
  (let* ((world (make-block-world :chunk-width 1
                                  :chunk-height 1
                                  :chunk-depth 1))
         ;; World X=4 aliases X=0 in a four-column LUFT domain.
         (established (ensure-world-chunk world 4 0 0)))
    (setf (chunk-block-at established 0 0 0) luvcraft::*stone-block*)
    (let* ((materialization
             (luvcraft::attach-luft-world-materialization
              world :horizontal-bits 2))
           (scene
             (luvcraft::luft-world-materialization-scene materialization)))
      (luvcraft::reconcile-luft-world-materialization materialization)
      (ok (luft.render:scene-cell-p scene 0 0 0))
      (let ((revision (luft.render:scene-revision scene))
            (transient (ensure-world-chunk world 0 0 0)))
        (setf (chunk-block-at transient 0 0 0) luvcraft::*stone-block*)
        (remove-world-chunk world 0 0 0)
        (multiple-value-bind (published changed)
            (luvcraft::reconcile-luft-world-materialization materialization)
          (ok (eq published scene))
          (ok (zerop changed)))
        (ok (= revision (luft.render:scene-revision scene)))
        (ok (luft.render:scene-cell-p scene 0 0 0))))))

(deftest incremental-luft-world-agrees-with-a-fresh-rebuild
  (let* ((world (make-block-world :chunk-width 2
                                  :chunk-height 2
                                  :chunk-depth 2))
         (left (ensure-world-chunk world -1 0 -1))
         (middle (ensure-world-chunk world 0 0 0))
         (right (ensure-world-chunk world 1 1 1)))
    (setf (chunk-block-at left 1 0 1) luvcraft::*stone-block*
          (chunk-block-at middle 0 1 0) luvcraft::*grass-block*
          (chunk-block-at right 1 0 1) luvcraft::*crystal-block*)
    (let ((incremental
            (luvcraft::attach-luft-world-materialization
             world :horizontal-bits 4)))
      (luvcraft::reconcile-luft-world-materialization incremental)
      (setf (chunk-block-at left 1 0 1) nil
            (chunk-block-at middle 1 1 0) luvcraft::*stone-block*)
      (remove-world-chunk world 1 1 1)
      (ensure-world-chunk world 1 0 -1)
      (setf (world-block-at world 3 1 -1) luvcraft::*grass-block*)
      (luvcraft::reconcile-luft-world-materialization incremental)
      (let ((fresh
              (luvcraft::attach-luft-world-materialization
               world :horizontal-bits 4)))
        (luvcraft::reconcile-luft-world-materialization fresh)
        (ok (luft-world-scenes-agree-p
             (luvcraft::luft-world-materialization-scene incremental)
             (luvcraft::luft-world-materialization-scene fresh)))))))

(deftest resident-window-aliases-are-rejected-before-publication
  (let* ((world (make-block-world :chunk-width 2
                                  :chunk-height 2
                                  :chunk-depth 2)))
    ;; These chunks bound six X cells, wider than the four-cell LUFT torus.
    (ensure-world-chunk world 0 0 0)
    (ensure-world-chunk world 2 0 0)
    (let* ((materialization
             (luvcraft::attach-luft-world-materialization
              world :horizontal-bits 2))
           (scene
             (luvcraft::luft-world-materialization-scene materialization))
           (revision (luft.render:scene-revision scene)))
      (ok (signals
           (luvcraft::reconcile-luft-world-materialization materialization)
           'error))
      (ok (= revision (luft.render:scene-revision scene)))
      (ok (null
           (luvcraft::luft-world-materialization-resident-center
            materialization))))))

(deftest full-period-resident-window-is-rejected-before-incidence-wraps
  (let* ((world (make-block-world :chunk-width 1
                                  :chunk-height 1
                                  :chunk-depth 1))
         (first (ensure-world-chunk world 0 0 0))
         (last (ensure-world-chunk world 3 0 0)))
    ;; The four-cell span has injective anchors in a four-column torus, but
    ;; LUFT would identify X=3's high face with X=0's low face and make these
    ;; non-neighbouring authored cells share a boundary.
    (setf (chunk-block-at first 0 0 0) luvcraft::*stone-block*
          (chunk-block-at last 0 0 0) luvcraft::*stone-block*)
    (let* ((materialization
             (luvcraft::attach-luft-world-materialization
              world :horizontal-bits 2))
           (scene
             (luvcraft::luft-world-materialization-scene materialization))
           (revision (luft.render:scene-revision scene)))
      (ok (signals
           (luvcraft::reconcile-luft-world-materialization materialization)
           'error))
      (ok (= revision (luft.render:scene-revision scene)))
      (ok (zerop
           (luft:chain-count (luft.render:scene-solid scene)))))))

(deftest resident-vertical-bounds-are-rejected-before-publication
  (let* ((world (make-block-world :chunk-width 1
                                  :chunk-height 2
                                  :chunk-depth 1)))
    ;; Cells 254 and 255 would occupy LUFT rows 254 and 255; the latter cannot
    ;; be a cell anchor because its positive Z boundary would not exist.
    (ensure-world-chunk world 0 127 0)
    (let* ((materialization
             (luvcraft::attach-luft-world-materialization world))
           (scene
             (luvcraft::luft-world-materialization-scene materialization))
           (revision (luft.render:scene-revision scene)))
      (ok (signals
           (luvcraft::reconcile-luft-world-materialization materialization)
           'error))
      (ok (= revision (luft.render:scene-revision scene)))
      (ok (zerop
           (luft:chain-count (luft.render:scene-solid scene)))))))

(defclass luft-world-test-observer ()
  ((cell-events :initform 0 :accessor luft-world-test-observer-cell-events)))

(defmethod observe-block-world-cell-change
    ((observer luft-world-test-observer) world chunk x y z)
  (declare (ignore world chunk x y z))
  (incf (luft-world-test-observer-cell-events observer)))

(defmethod observe-block-world-residency-change
    ((observer luft-world-test-observer) world chunk event)
  (declare (ignore observer world chunk event)))

(deftest luft-materialization-coexists-with-other-world-observers
  (let* ((world (make-block-world :chunk-width 2
                                  :chunk-height 2
                                  :chunk-depth 2))
         (observer (make-instance 'luft-world-test-observer)))
    (add-block-world-observer world observer)
    (let ((materialization
            (luvcraft::attach-luft-world-materialization world)))
      (ok (= 2 (length (block-world-observers world))))
      (let ((chunk (ensure-world-chunk world 0 0 0)))
        (setf (chunk-block-at chunk 0 0 0) luvcraft::*stone-block*))
      (ok (= 1 (luft-world-test-observer-cell-events observer)))
      (luvcraft::reconcile-luft-world-materialization materialization)
      (luvcraft::detach-luft-world-materialization materialization)
      (ok (= 1 (length (block-world-observers world))))
      (ok (eq observer (aref (block-world-observers world) 0))))))
