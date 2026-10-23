;;;; Luvcraft on Luft against Luvcraft itself: the same seed, read through
;;;; Luvcraft's own streaming load path, must raise the same cells in Luft.

(defpackage #:luvcraft.luft.tests
  (:use #:cl)
  (:import-from #:parachute #:define-test #:true #:is)
  (:local-nicknames (#:game #:luvcraft.luft)
                    (#:terrain #:luft.terrain)))

(in-package #:luvcraft.luft.tests)

(defun luvcraft-oracle-world (seed relief height edits chunk-keys)
  "A Luvcraft world whose CHUNK-KEYS were loaded as Luvcraft streams them."
  (let* ((world (luvcraft:make-empty-little-block-world
                 :seed seed :relief relief :chunk-height height))
         (source (luvcraft:block-world-source world)))
    (loop for (block x y z) in edits
          do (luvcraft::record-block-edit
              (luvcraft:little-world-source-edits source) block x y z))
    (dolist (key chunk-keys world)
      (let* ((request (luvcraft::make-block-chunk-load-request source world key 1 0))
             (payload (luv.production:perform-production-request request)))
        (destructuring-bind (x y z) key
          (luvcraft::install-world-chunk-storage
           world x y z (luvcraft::block-chunk-load-payload-content payload)))))))

(defun compare-luft-chunk (seed relief height luft-key edits)
  "Count cells of Luft chunk LUFT-KEY that differ from Luvcraft's world."
  (let* ((offset 1024)
         (keys (let ((x0 (floor (- (luft:chunk-origin-x luft-key) offset) 16))
                     (z0 (floor (- (luft:chunk-origin-y luft-key) offset) 16)))
                 (loop for i below 4
                       nconc (loop for j below 4
                                   collect (list (+ x0 i) 0 (+ z0 j))))))
         (world (luvcraft-oracle-world seed relief height edits keys))
         (little (luvcraft:block-world-source world))
         (source (game::little-world-luft-source little height :offset offset))
         (chunk (terrain:materialize-world-source-chunk
                 source luft-key 1
                 :edits (terrain:capture-world-source-chunk-edits source luft-key)))
         (materials (terrain::resident-cell-chunk-material-cells chunk))
         (fibers (terrain::resident-cell-chunk-fibers chunk))
         (vocabulary (terrain:world-source-material-vocabulary source))
         (domain (terrain:world-source-domain source))
         (differences 0) (solid 0))
    (dolist (key keys)
      (destructuring-bind (cx cy cz) key
        (declare (ignore cy))
        (dotimes (lx 16)
          (dotimes (lz 16)
            (dotimes (y height)
              (let* ((x (+ (* cx 16) lx)) (z (+ (* cz 16) lz))
                     (block (luvcraft::world-block-at world x y z))
                     (site (luft:make-site domain (+ x offset) (+ z offset) y
                                           luft:+cell-extent+ 1))
                     (offset-code (gethash site materials))
                     (placement
                       (and offset-code
                            (luv.domains:identity-vocabulary-member
                             vocabulary offset-code))))
                (when block (incf solid))
                (unless (and (eq placement (game:block-placement block))
                             (= (if block 1 0)
                                (luft:fibers-cell-bit
                                 fibers (+ x offset) (+ z offset) y)))
                  (incf differences))))))))
    (values differences solid (hash-table-count materials))))

(define-test alpine-chunks-match-luvcraft-cell-for-cell
  ;; Two Luft chunks: one at Luvcraft's origin, one in rough ground.
  (dolist (origin '((1024 1024) (1216 896)))
    (multiple-value-bind (differences solid count)
        (compare-luft-chunk 5 :alpine 64
                            (luft:chunk-key-at (first origin) (second origin))
                            nil)
      (is = 0 differences)
      ;; The comparison saw real ground, and Luft holds nothing else.
      (true (< 20000 solid))
      (is = solid count))))

(define-test meadow-edits-replay-like-a-luvcraft-save
  ;; A short meadow world, as the oldest saves are, with negative coordinates,
  ;; a placed block, a carved one, and a placed block above the ground.
  (let ((edits (list (list luvcraft::*planks-block* -20 6 -30)
                     (list nil -21 3 -30)
                     (list luvcraft::*crystal-block* -22 12 -31))))
    (multiple-value-bind (differences solid count)
        (compare-luft-chunk 121 :meadow 16 (luft:chunk-key-at 960 960) edits)
      (is = 0 differences)
      (true (< 10000 solid))
      (is = solid count))))

(define-test the-palette-keeps-luvcraft-faces-and-light
  (flet ((kind (name)
           (luft.terrain:material-placement-kind
            (game:block-placement (luvcraft:block-kind-named name)))))
    (destructuring-bind (red green blue) (terrain::material-kind-top-tone (kind :grass))
      (true (> green red)) (true (> green blue)))
    ;; A grass block's bottom is dirt, redder than its top.
    (true (> (first (terrain::material-kind-bottom-tone (kind :grass)))
             (second (terrain::material-kind-bottom-tone (kind :grass)))))
    (true (plusp (reduce #'max (terrain::material-kind-light-emission (kind :crystal)))))
    (is equal '(0 0 0) (terrain::material-kind-light-emission (kind :stone)))))

(define-test the-spawn-stands-on-luvcraft-ground
  (let ((source (game:make-luvcraft-world-source :seed 5 :relief :alpine)))
    (multiple-value-bind (x y z) (terrain:world-source-spawn source)
      (let* ((key (luft:chunk-key-at x y))
             (fibers (terrain::resident-cell-chunk-fibers
                      (terrain:materialize-world-source-chunk source key 1))))
        (is = 1 (luft:fibers-cell-bit fibers x y (1- z)))
        (is = 0 (luft:fibers-cell-bit fibers x y z))
        (is = 0 (luft:fibers-cell-bit fibers x y (1+ z)))))))
