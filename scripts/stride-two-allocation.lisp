;;;; Fixed-work heap measurements for the ridge (#WQFOT0).
;;;; Run in a separate SBCL process: ./env sbcl --script scripts/stride-two-allocation.lisp --check
(require :asdf)
(pushnew (truename "./") asdf:*central-registry* :test #'equal)
(asdf:load-system "luvcraft/luft")
(load (merge-pathnames "stride-four-ridge-fixture.lisp" *load-truename*))

(defpackage #:stride-two-allocation
  (:use #:cl)
  (:local-nicknames (#:ter #:luft.terrain) (#:game #:luvcraft.luft)))
(in-package #:stride-two-allocation)

(defvar *check-p* (member "--check" (uiop:command-line-arguments) :test #'string=))

(defun measure (name thunk &key (count 12) byte-limit)
  "Warm the buffers, then measure fresh heap bytes and wall time per call."
  (funcall thunk)
  (sb-ext:gc :full t)
  (let ((bytes (sb-ext:get-bytes-consed)) (ticks (get-internal-real-time)))
    (dotimes (i count) (funcall thunk))
    (let ((mean-bytes (round (- (sb-ext:get-bytes-consed) bytes) count))
          (mean-ms (/ (* 1000d0 (- (get-internal-real-time) ticks))
                      internal-time-units-per-second count)))
      (format t "~&~A,~D,~,3F~%" name mean-bytes mean-ms)
      (when (and *check-p* byte-limit (> mean-bytes byte-limit))
        (error "~A allocated ~:D bytes per call (budget ~:D)." name mean-bytes byte-limit)))))

(defun checksum (array)
  (loop with hash of-type (unsigned-byte 32) = #x811c9dc5
        for word across array
        do (setf hash (ldb (byte 32 0) (* #x01000193 (logxor word hash))))
        finally (return hash)))

(let* ((source (cl-user::ridge-source))
       (game::*luvcraft-generation-threads* 1)
       (little (game::luvcraft-little-world-source source))
       (world (game::luvcraft-private-world source little))
       (key '(-16 0 0))
       (request (luvcraft:make-block-chunk-load-request little world key 1 0))
       (scratch (ter:make-coarse-tile-scratch (ter:world-source-domain source)))
       (grid (ter::coarse-tile-scratch-grid scratch))
       (emissions (ter::coarse-tile-scratch-emissions scratch))
       (fibers (ter::coarse-tile-scratch-fibers scratch)))
  (cl-user::carve-ridge source)
  (flet ((tile ()
           (ter:build-world-source-coarse-tile source 12 16 :scratch scratch)))
    (let ((tile (tile)))
      (format t "~&Ridge words/materials ~8,'0X ~8,'0X, fine grid/emission ~8,'0X ~8,'0X~%"
              (checksum (ter:coarse-voxel-tile-words tile)) (checksum (ter:coarse-voxel-tile-materials tile))
              (checksum grid) (checksum emissions)))
    (format t "~&work,bytes_per_call,ms_per_call~%")
    (measure "4096 surface heights"
             (lambda () (loop for x from -256 below -192 sum
                          (loop for z below 64 sum (luvcraft::little-world-surface-height little x z 128))))
             :byte-limit (* 256 1024))
    (measure "capture one 16x16 chunk request"
             (lambda () (luvcraft:make-block-chunk-load-request little world key 1 0))
             :count 100)
    (measure "generate one 16x16 chunk payload"
             (lambda () (luv.production:perform-production-request request))
             :count 100)
    (measure "reduce one 64x64 chunk"
             (lambda () (ter:downsample-chunk-to-coarse-tile
                         fibers grid 12 16 :emission-grid emissions :cull-interior-caves-p nil))
             :byte-limit (* 512 1024))
    (measure "generate and reduce one culled 64x64 tile" #'tile :byte-limit (* 4 1024 1024))))
