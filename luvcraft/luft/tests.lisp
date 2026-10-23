;;;; Luvcraft on Luft against Luvcraft itself: the same seed, read through
;;;; Luvcraft's own streaming load path, must raise the same cells in Luft.

(defpackage #:luvcraft.luft.tests
  (:use #:cl)
  (:import-from #:parachute #:define-test #:true #:is)
  (:local-nicknames (#:game #:luvcraft.luft)
                    (#:terrain #:luft.terrain)))

(in-package #:luvcraft.luft.tests)

(define-test distinctive-materials-reach-the-renderer-descriptors
  (let* ((palette game:*luvcraft-palette*)
         (vocabulary (game:luvcraft-palette-vocabulary palette))
         (words (terrain:compile-terrain-material-descriptors vocabulary)))
    ;; These are the wire codes consumed by the fixed star shader, not just
    ;; palette annotations.  All fit the existing two descriptor rows.
    (loop for (name relief pattern) in
            '((:hay-bale 10 12) (:thatch 10 0) (:cactus 11 0)
              (:iron-plate 12 8) (:terminal 8 0) (:urbit 8 0)
              (:tape 12 0) (:film 12 0) (:tapestry 9 11)
              (:fountain 8 0) (:lava-spring 8 0) (:marble 8 14) (:granite 2 15))
          for placement = (find name (luv.domains:identity-vocabulary-members vocabulary)
                                :key #'terrain:material-placement-name)
          for row = (* 8 (1+ (luv.domains:identity-vocabulary-offset
                             vocabulary placement)))
          do (is = relief (ldb (byte 4 0) (aref words (+ row 3))))
             (is = pattern (ldb (byte 4 0) (aref words (+ row 4)))))))

(define-test object-materials-use-fixture-bodies
  (let ((placements (luv.domains:identity-vocabulary-members
                     (game:luvcraft-palette-vocabulary game:*luvcraft-palette*))))
    (loop for (name body) in
            '((:barrel :barrel) (:crate :crate)
              (:tape :reel) (:film :reel)
              (:terminal :screen) (:urbit :screen)
              (:fountain :fountain) (:lava-spring :lava-spring)
              (:iron-plate :none) (:blue-wool :none))
          for placement = (find name placements :key #'terrain:material-placement-name)
          do (is eq body (terrain:material-kind-body
                          (terrain:material-placement-kind placement))))))

(defun luvcraft-oracle-world (little height edits chunk-keys)
  "A Luvcraft world over LITTLE whose CHUNK-KEYS were loaded as Luvcraft streams them."
  (let ((world (luvcraft::make-block-world :chunk-height height :source little)))
    (loop for (block x y z) in edits
          do (luvcraft::record-block-edit
              (luvcraft:little-world-source-edits little) block x y z))
    (dolist (key chunk-keys world)
      (let* ((request (luvcraft::make-block-chunk-load-request little world key 1 0))
             (payload (luv.production:perform-production-request request)))
        (destructuring-bind (x y z) key
          (luvcraft::install-world-chunk-storage
           world x y z (luvcraft::block-chunk-load-payload-content payload)))))))

(defun compare-luft-chunk (seed relief height luft-key edits
                           &key (class 'luvcraft:little-world-source))
  "Count cells of Luft chunk LUFT-KEY that differ from Luvcraft's world."
  (let* ((offset 1024)
         (keys (let ((x0 (floor (- (luft:chunk-origin-x luft-key) offset) 16))
                     (z0 (floor (- (luft:chunk-origin-y luft-key) offset) 16)))
                 (loop for i below 4
                       nconc (loop for j below 4
                                   collect (list (+ x0 i) 0 (+ z0 j))))))
         (world (luvcraft-oracle-world
                 (make-instance class :seed seed :relief relief)
                 height edits keys))
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

(define-test lanterns-stand-on-and-hang-from-their-neighbours
  ;; Above a meadow: a plank with a lantern on it and one hanging beneath,
  ;; and a lantern with nothing above or below, which stays a cube.
  (let* ((lantern luvcraft::*lantern-block*)
         (world (luvcraft:make-empty-little-block-world
                 :seed 121 :relief :meadow :chunk-height 16))
         (little (luvcraft:block-world-source world)))
    (loop for (block x y z) in (list (list luvcraft::*planks-block* -20 13 -30)
                                     (list lantern -20 14 -30)
                                     (list lantern -20 12 -30)
                                     (list lantern -24 14 -30))
          do (luvcraft::record-block-edit
              (luvcraft:little-world-source-edits little) block x y z))
    (let* ((source (game::little-world-luft-source little 16))
           (key (luft:chunk-key-at 1004 994))
           (chunk (terrain:materialize-world-source-chunk
                   source key 1
                   :edits (terrain:capture-world-source-chunk-edits source key)))
           (domain (terrain:world-source-domain source))
           (materials (terrain::resident-cell-chunk-material-cells chunk))
           (attachments (terrain:resident-cell-chunk-attachments chunk)))
      (flet ((cell (x y z) (luft:make-site domain (+ 1024 x) (+ 1024 z) y
                                           luft:+cell-extent+ 1)))
        (is = 2 (length attachments))
        (dolist (attachment attachments)
          (true (eq :lantern (terrain:material-kind-attachment-form
                              (terrain:surface-attachment-material attachment))))
          (is = (cell -20 13 -30) (terrain::surface-attachment-support-cell attachment))
          (true (null (gethash (terrain:surface-attachment-clearance-cell attachment)
                               materials))))
        (true (gethash (cell -20 13 -30) materials))
        (true (gethash (cell -24 14 -30) materials))))))

(define-test the-birthday-meadow-matches-luvcraft-cell-for-cell
  ;; The party lawn at the origin, its flowers and its ring of trees, with a
  ;; plank placed on the lawn the way the gazebo's edits are.
  (multiple-value-bind (differences solid count)
      (compare-luft-chunk 121 :alpine 32 (luft:chunk-key-at 1000 1000)
                          (list (list luvcraft::*planks-block* 3 7 -4))
                          :class 'luvcraft.birthday:birthday-world-source)
    (is = 0 differences)
    (true (< 10000 solid))
    (is = solid count)))

(defun write-save (world pathname &key resume)
  (luvcraft::write-luvcraft-save-description
   (luvcraft:make-luvcraft-save-description world) pathname)
  (when resume
    (let ((description (with-open-file (stream pathname) (read stream))))
      (setf (getf (rest description) :resume) resume)
      (luvcraft::write-luvcraft-save-description description pathname)))
  pathname)

(define-test a-relative-save-pathname-lands-where-it-names
  (let* ((directory (uiop:ensure-directory-pathname
                     (uiop:merge-pathnames*
                      (format nil "luvcraft-relative-save-~36R" (random (expt 36 8)
                                                                        (make-random-state t)))
                      (uiop:temporary-directory))))
         (*default-pathname-defaults* directory))
    (unwind-protect
         (progn
           (luvcraft::write-luvcraft-save-description '(:save) #p"nested/world.sexp")
           (true (probe-file (merge-pathnames "nested/world.sexp" directory)))
           (true (not (probe-file (merge-pathnames "nested/nested/world.sexp"
                                                     directory)))))
      (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore))))

(define-test luft-edits-save-back-into-a-luvcraft-save
  (uiop:with-temporary-file (:pathname pathname :type "sexp")
    (let ((world (luvcraft.birthday:make-birthday-world :seed 7 :chunk-radius nil)))
      ;; A film keeps its own reel; a block outside the Luft domain is kept.
      (luvcraft::record-block-edit
       (luvcraft:little-world-source-edits (luvcraft:block-world-source world))
       luvcraft::*crystal-block* 2 9 2)
      (luvcraft::record-block-edit
       (luvcraft:little-world-source-edits (luvcraft:block-world-source world))
       luvcraft::*stone-block* 5000 3 0)
      (write-save world pathname
                  :resume (list :player-position '(1.5d0 7d0 2.5d0)
                                :look '(:yaw 0.25d0 :pitch -0.1d0)
                                :selected-block '(:block :name :grass)
                                :carried '((:block :name :planks)))))
    (multiple-value-bind (source dropped world resume)
        (game:luvcraft-save-world-source pathname)
      (is = 1 dropped)
      (multiple-value-bind (position yaw pitch)
          (game:luvcraft-resume-luft-pose source resume)
        (is equal '(1025.5d0 1026.5d0 7d0) position)
        (true (< (abs (- yaw (- (/ pi 2) 0.25))) 1e-5))
        (true (< (abs (+ pitch 0.1)) 1e-5)))
      ;; Two edits in Luft: carve the crystal, build bricks above the lawn.
      (let ((edits (terrain:world-source-edits source)))
        (setf (gethash (game:luvcraft-cell-site source 2 9 2) edits) nil
              (gethash (game:luvcraft-cell-site source -3 8 4) edits)
              (game:block-placement luvcraft::*bricks-block*)))
      (luvcraft::write-luvcraft-save-description
       (game:luvcraft-save-description
        source world :resume resume
        :position '(1030.5 1020.5 9.0) :yaw 1.0 :pitch 0.2
        :selected (game:block-placement luvcraft::*bricks-block*))
       pathname))
    ;; Luvcraft itself reads the result: the same meadow, both edits, the
    ;; out-of-domain block, and the player where Luft left them.
    (multiple-value-bind (world resume) (luvcraft:read-luvcraft-save pathname)
      (let* ((little (luvcraft:block-world-source world))
             (entries (luvcraft::block-edit-overlay-entries
                       (luvcraft:little-world-source-edits little))))
        (true (typep little 'luvcraft.birthday:birthday-world-source))
        (is = 7 (luvcraft:little-world-source-seed little))
        (is = 3 (hash-table-count entries))
        (multiple-value-bind (block present-p) (gethash '(2 9 2) entries)
          (true present-p) (is eq nil block))
        (is eq luvcraft::*bricks-block* (gethash '(-3 8 4) entries))
        (is eq luvcraft::*stone-block* (gethash '(5000 3 0) entries)))
      (is equalp '(6.5d0 9d0 -3.5d0) (getf resume :player-position))
      (true (< (abs (- (getf (getf resume :look) :yaw) (- (/ pi 2) 1))) 1e-5))
      (is equal '(:block :name :bricks) (getf resume :selected-block))
      (is equal '((:block :name :planks)) (getf resume :carried)))))
