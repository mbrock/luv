(in-package #:luft)

;;; A chunk becomes a list of active lattice stars.  This is deliberately the
;;; entire CPU mesher: chunk residency supplies neighboring cells, the cells
;;; vote into their eight corners, and the mesh shader reads the atlas.

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

(defun %star-site-key (x y z)
  "A sortable integer key for one in-domain lattice point."
  (logior z (ash x 8) (ash y 27)))

(defun %star-site-key-x (key)
  (ldb (byte 19 8) key))

(defun %star-site-key-y (key)
  (ldb (byte 19 27) key))

(defun %star-site-key-z (key)
  (ldb (byte 8 0) key))

(defun %accumulate-chunk-stars (stars chain x0 x1 y0 y1)
  "Add CHAIN's positive cells to lattice STARS owned by the output box."
  (map-chain
   (lambda (cell)
     (unless (and (= +cell-extent+ (site-extent cell))
                  (site-positive-p cell))
       (error "A star mesh needs positive cells, not ~S." cell))
     (let ((cell-x (site-x cell))
           (cell-y (site-y cell))
           (cell-z (site-z cell)))
       (dotimes (dx 2)
         (let ((x (+ cell-x dx)))
           (when (<= x0 x x1)
             (dotimes (dy 2)
               (let ((y (+ cell-y dy)))
                 (when (<= y0 y y1)
                   (dotimes (dz 2)
                     (let* ((z (+ cell-z dz))
                            (key (%star-site-key x y z))
                            (sample (logior (if (zerop dx) 1 0)
                                            (if (zerop dy) 2 0)
                                            (if (zerop dz) 4 0))))
                       (setf (gethash key stars)
                             (logior (gethash key stars 0)
                                     (ash 1 sample)))))))))))))
   chain))

(defun %star-site-words (stars)
  "Pack deterministic (X Y Z STAR) records for every actual surface site."
  (let* ((keys
           (sort
            (loop for key being the hash-keys of stars
                  for star = (gethash key stars)
                  unless (or (zerop star) (= star #xff)) collect key)
            #'<))
         (words
           (make-array (* 4 (length keys))
                       :element-type '(unsigned-byte 32))))
    (loop for key in keys
          for offset from 0 by 4
          do (setf (aref words offset) (%star-site-key-x key)
                   (aref words (+ offset 1)) (%star-site-key-y key)
                   (aref words (+ offset 2)) (%star-site-key-z key)
                   (aref words (+ offset 3)) (gethash key stars)))
    words))

(defun mesh-star-chunk (chunk chunk-key &key outside-domain-policy)
  "Turn one resident chunk neighborhood into the complete mesh-shader ABI."
  (check-type chunk chain)
  (check-type outside-domain-policy (member nil :air :solid))
  (let* ((domain (chain-domain chunk))
         (x0 (chunk-origin-x chunk-key))
         (y0 (chunk-origin-y chunk-key))
         (x1 (min (+ x0 +chunk-size+) (world-domain-x-limit domain)))
         (y1 (min (+ y0 +chunk-size+) (world-domain-y-limit domain)))
         (stars (make-hash-table :test #'eql)))
    (handler-bind
        ((outside-domain
           (lambda (condition)
             (declare (ignore condition))
             (ecase outside-domain-policy
               ((nil) nil)
               (:air (invoke-restart 'treat-as-air))
               (:solid (invoke-restart 'treat-as-solid))))))
      (dolist (neighbor (%mesh-star-neighborhood chunk chunk-key))
        (%accumulate-chunk-stars stars neighbor x0 x1 y0 y1)))
    (make-surface-mesh domain (%star-site-words stars))))
