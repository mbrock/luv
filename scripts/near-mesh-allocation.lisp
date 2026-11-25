;;;; Fixed-work SBCL probe for one near ridge mesh (#ROKN9P). Run from the checkout:
;;;; ./env sbcl --script scripts/near-mesh-allocation.lisp --check
;;;; Add --stages to measure exclusive allocation in a separate instrumented batch.
(require :asdf)
(pushnew (truename "./") asdf:*central-registry* :test #'equal)
(asdf:load-system "luvcraft/luft")
(asdf:load-system "luft/renderer")
(load (merge-pathnames "stride-four-ridge-fixture.lisp" *load-truename*))

(defpackage #:near-mesh-allocation
  (:use #:cl)
  (:local-nicknames (#:ter #:luft.terrain) (#:r #:luft.render)))
(in-package #:near-mesh-allocation)

(defun ridge-snapshot ()
  (let* ((source (cl-user::ridge-source))
         (scene (ter:make-world-source-streaming-scene
                 source :mesh-preparer r:+render-population-preparer+))
         (owner (luft:chunk-key-at 780 1048))
         (witness (ter::streaming-scene-dependency-guard-keys scene (list owner)))
         (guard (ter::streaming-scene-dependency-guard-keys scene witness)))
    (cl-user::carve-ridge source)
    (dolist (key guard)
      (setf (gethash key (ter:streaming-scene-store scene))
            (ter:materialize-world-source-chunk source key 1)
            (gethash key (ter:streaming-scene-loaded scene)) 1))
    (ter::rebuild-authored-world-resident-values scene guard)
    ;; Hold light residency constant while rebuilding fresh geometry.
    (ter:install-streaming-light-tiles
     scene (ter:realize-streaming-light-tiles scene witness (ter:streaming-scene-light-tiles scene)))
    (ter::make-streaming-region-snapshot scene (list owner) 1)))

(defun build-owner (snapshot)
  (ter::mesh-streaming-snapshot snapshot :prepare-products-p t))

(defun checksum (array)
  (loop with hash of-type (unsigned-byte 32) = #x811c9dc5
        for value across array
        for bits = (if (typep value 'single-float) (sb-kernel:single-float-bits value) value)
        do (setf hash (ldb (byte 32 0) (* #x01000193 (logxor bits hash))))
        finally (return hash)))

(let* ((snapshot (ridge-snapshot))
       (product (first (nth-value 4 (build-owner snapshot))))
       (mesh (ter::streaming-star-product-mesh product))
       (population (r::prepared-render-mesh-population (ter::streaming-star-product-prepared product))))
  (format t "~&owner ~D, ~D stars; sites/edges/appearance/light/shadows ~{~8,'0X~^ ~}~%"
          (ter::streaming-star-product-key product) (r::render-population-site-count population)
          (mapcar #'checksum (list (r::render-population-instance-words population)
                                   (luft::surface-mesh-edge-shaping mesh)
                                   (r::render-population-appearance-words population)
                                   (r::render-population-light-words population)
                                   (r::render-population-shadow-rectangles population))))
  (sb-ext:gc :full t)
  (let ((n 12) (bytes (sb-ext:get-bytes-consed)) (start (get-internal-real-time)))
    (dotimes (i n) (build-owner snapshot))
    (let ((mean-bytes (round (- (sb-ext:get-bytes-consed) bytes) n)))
      (format t "Bytes/owner ~D, wall ms/owner ~,3F~%" mean-bytes
              (/ (* 1000d0 (- (get-internal-real-time) start)) n internal-time-units-per-second))
      (when (and (member "--check" (uiop:command-line-arguments) :test #'string=)
                 (> mean-bytes (* 20 1024 1024)))
        (error "The prepared ridge mesh exceeded its 20 MiB allocation budget."))))
  (when (member "--stages" (uiop:command-line-arguments) :test #'string=)
    (sb-profile:profile ter::mesh-streaming-snapshot luft:mesh-star-chunk
                        luft::%gather-star-sites luft::%materialize-star-selection-window
                        luft::%star-edge-shaping ter:compile-surface-mesh-appearance
                        ter::fibers-without-cells ter::material-chunk-translucent-cells
                        r::make-render-population r::merge-terrain-shadow-faces r::star-lattice-light-words)
    (unwind-protect
         (progn (dotimes (i 12) (build-owner snapshot)) (sb-profile:report))
      (sb-profile:unprofile))))
