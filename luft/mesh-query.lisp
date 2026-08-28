(in-package #:luft)

;;; A chunk becomes a list of active lattice stars.  This is deliberately the
;;; entire CPU mesher: chunk residency supplies dense vertical occupancy
;;; fibers, mixed eight-cell stars are selected a word at a time, and the mesh
;;; shader reads the atlas.

(defun %mesh-neighbor-chunk (domain key)
  "Resolve KEY once through the streaming boundary protocol."
  (restart-case (error 'missing-chunk :domain domain :key key)
    (use-chunk (chain)
      :report "Supply the resident chunk chain."
      (check-type chain chain)
      (unless (world-domain= domain (chain-domain chain))
        (error "Neighbor chunk belongs to another world domain."))
      chain)
    (treat-as-air ()
      :report "Treat this whole chunk as air."
      nil)
    (treat-as-solid ()
      :report "Treat this whole chunk as solid."
      (error "The star selector needs an enumerable chunk, not a solid sentinel."))))

(defun %mesh-star-neighborhood (chunk chunk-key)
  "Return the at-most-nine chains whose cells touch CHUNK-KEY's sites."
  (let* ((domain (chain-domain chunk))
         (grid-x (chunk-key-x chunk-key))
         (grid-y (chunk-key-y chunk-key))
         (x-chunks (ceiling (world-domain-x-limit domain) +chunk-size+))
         (y-chunks (ceiling (world-domain-y-limit domain) +chunk-size+)))
    (loop for x from (max 0 (1- grid-x)) to (min (1- x-chunks) (1+ grid-x))
          append
          (loop for y from (max 0 (1- grid-y)) to (min (1- y-chunks) (1+ grid-y))
                for key = (chunk-key-at (* x +chunk-size+) (* y +chunk-size+))
                for neighbor = (if (= key chunk-key)
                                   chunk
                                   (%mesh-neighbor-chunk domain key))
                when neighbor collect neighbor))))

(defconstant +packed-star-x-shift+ 8)
(defconstant +packed-star-y-shift+ 15)
(defconstant +packed-star-z-shift+ 22)

(declaim (inline %pack-star-site %packed-star-x %packed-star-y
                 %packed-star-z %packed-star-mask))
(defun %pack-star-site (local-x local-y z mask)
  "Pack one chunk-local 65x65x256 lattice record into thirty bits."
  (logior mask
          (ash local-x +packed-star-x-shift+)
          (ash local-y +packed-star-y-shift+)
          (ash z +packed-star-z-shift+)))

(defun %packed-star-x (record) (ldb (byte 7 +packed-star-x-shift+) record))
(defun %packed-star-y (record) (ldb (byte 7 +packed-star-y-shift+) record))
(defun %packed-star-z (record) (ldb (byte 8 +packed-star-z-shift+) record))
(defun %packed-star-mask (record) (ldb (byte 8 0) record))

(defun %gather-star-sites (chains x0 x1 y0 y1)
  "Return ordered packed records for every mixed occupancy star in the box."
  (let* ((window (%materialize-star-selection-window chains x0 x1 y0 y1))
         (below (star-selection-window-below window))
         (above (star-selection-window-above window))
         (active (make-array +star-fiber-word-count+
                             :element-type '(unsigned-byte 64)))
         (records (make-array 4096 :element-type '(unsigned-byte 32)
                                   :adjustable t :fill-pointer 0))
         (kernel (%star-active-kernel)))
    ;; Y/X/Z order is the numeric order of the former global lattice key and
    ;; therefore keeps uploads and tests deterministic without a sort.
    (loop for y fixnum from y0 to y1 do
      (loop for x fixnum from x0 to x1 do
        (let ((southwest (%star-window-fiber-base window (1- x) (1- y)))
              (southeast (%star-window-fiber-base window x (1- y)))
              (northwest (%star-window-fiber-base window (1- x) y))
              (northeast (%star-window-fiber-base window x y)))
          (funcall kernel below above
                   southwest southeast northwest northeast active)
          (dotimes (word +star-fiber-word-count+)
            (let ((bits (aref active word)))
              (loop while (plusp bits) do
                (let* ((lowest (logand bits (- bits)))
                       (bit (1- (integer-length lowest)))
                       (z (+ (* word 64) bit))
                       (mask (%star-mask-from-window
                              below above
                              southwest southeast northwest northeast
                              word bit)))
                  (vector-push-extend
                   (%pack-star-site (- x x0) (- y y0) z mask)
                   records)
                  (setf bits (logand bits (1- bits))))))))))
    records))

(defun %star-site-words (records x0 y0)
  "Expand compact local records directly into the public four-word GPU ABI."
  (let ((words (make-array (* 4 (length records))
                           :element-type '(unsigned-byte 32))))
    (loop for record across records
          for offset fixnum from 0 by 4 do
      (setf (aref words offset) (+ x0 (%packed-star-x record))
            (aref words (+ offset 1)) (+ y0 (%packed-star-y record))
            (aref words (+ offset 2)) (%packed-star-z record)
            (aref words (+ offset 3)) (%packed-star-mask record)))
    words))

(defun mesh-star-chunk (chunk chunk-key &key outside-domain-policy)
  "Turn one resident chunk neighborhood into the complete mesh-shader ABI."
  (check-type chunk chain)
  (check-type outside-domain-policy (member nil :air :solid))
  (let* ((domain (chain-domain chunk))
         (x0 (chunk-origin-x chunk-key))
         (y0 (chunk-origin-y chunk-key))
         (x1 (min (+ x0 +chunk-size+) (world-domain-x-limit domain)))
         (y1 (min (+ y0 +chunk-size+) (world-domain-y-limit domain))))
    (handler-bind
        ((outside-domain
           (lambda (condition)
             (declare (ignore condition))
             (ecase outside-domain-policy
               ((nil) nil)
               (:air (invoke-restart 'treat-as-air))
               (:solid (invoke-restart 'treat-as-solid))))))
      (let ((records
              (%gather-star-sites
               (%mesh-star-neighborhood chunk chunk-key) x0 x1 y0 y1)))
        (make-surface-mesh domain (%star-site-words records x0 y0))))))
