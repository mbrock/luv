(in-package #:luft.web)

(defun generation-form ()
  "Deterministic, DOM-free terrain sampling shared by workers and fixtures."
  '(progn
    (defvar chunk-size 16)
    (defvar world-height 64)
    ;; HAS, rather than truthiness, is important: zero is a durable removal.
    (defvar world-edits (new (|Map|)))

    (defun chunk-key (cx cy) (+ cx "," cy))
    (defun chunk-index (chunk x y z)
      (+ z (* world-height
              (+ (+ (- x (@ chunk x0)) 1)
                 (* (+ (- y (@ chunk y0)) 1) 18)))))
    (defun chunk-cell-at (chunk x y z)
      (if (or (< z 0) (>= z world-height)
              (< x (1- (@ chunk x0))) (> x (+ (@ chunk x0) chunk-size))
              (< y (1- (@ chunk y0))) (> y (+ (@ chunk y0) chunk-size)))
          0
          (aref (@ chunk data) (chunk-index chunk x y z))))

    (defun authored-cell-at (x y z)
      ;; CELLS remains the small, authored Map.  Its absent entries are
      ;; authored air, so do not fall through to terrain in this rectangle.
      (if (and (>= x 0) (< x 48) (>= y 0) (< y 48))
          (or ((@ cells get) (cell-key x y z)) 0)
          null))
    (defun terrain-height-at (x y)
      ;; This is the former demo floor, continued across the authored edge,
      ;; then blended into cheap, deterministic broad hills.
      (let* ((radius (sqrt (+ (* (- x 24) (- x 24))
                              (* (- y 24) (- y 24)))))
             (old (max 1 (floor (+ 4 (* 2 (sin (* x 0.17)))
                                    (* 2 (cos (* y 0.21)))
                                    (* 3 (max 0 (- 1 (/ radius 25))))))))
             (edge-x (if (< x 0) (- x) (if (> x 47) (- x 47) 0)))
             (edge-y (if (< y 0) (- y) (if (> y 47) (- y 47) 0)))
             (linear (min 1 (/ (max edge-x edge-y) 32)))
             (blend (* linear linear (- 3 (* 2 linear))))
             (large (max 6 (min 40 (floor (+ 21
                                               (* 8 (sin (* x 0.071)))
                                               (* 6 (cos (* y 0.053)))
                                               (* 5 (sin (* (+ x y) 0.031)))
                                               (* 3 (cos (* (- x y) 0.11)))))))))
        (floor (+ (* old (- 1 blend)) (* large blend)))))
    (defun source-cell-at (x y z)
      "Authoritative source, independent of which chunks happen to be loaded."
      (if (or (< z 0) (>= z world-height))
          0
          (let ((key (cell-key x y z)))
            (if ((@ world-edits has) key)
                ((@ world-edits get) key)
                (let ((authored (authored-cell-at x y z)))
                  (if (not (null authored))
                      authored
                      (if (< z (terrain-height-at x y)) 1 0)))))))
    (defun make-chunk (cx cy)
      (let* ((x0 (* cx chunk-size))
             (y0 (* cy chunk-size))
             (data (new (|Uint8Array| (* 18 18 world-height))))
             (chunk (create :cx cx :cy cy :key (chunk-key cx cy)
                            :x0 x0 :y0 y0 :data data)))
        ;; Terrain height is intentionally calculated once per XY column.
        ;; The inline lookup avoids recalculating it for all 64 Z samples.
        (loop for x from (1- x0) to (+ x0 chunk-size) do
          (loop for y from (1- y0) to (+ y0 chunk-size) do
            (let ((height (terrain-height-at x y)))
              (loop for z from 0 to (1- world-height) do
                (let* ((key (cell-key x y z))
                       (authored (authored-cell-at x y z)))
                  (setf (aref data (chunk-index chunk x y z))
                        (if ((@ world-edits has) key)
                            ((@ world-edits get) key)
                            (if (not (null authored)) authored
                                (if (< z height) 1 0)))))))))
        chunk))
    (defun chunk-sites (chunk)
      (let ((sites (array)))
        ;; Disjoint site ownership, with neighboring cells read from the halo.
        (loop for x from (@ chunk x0) to (1- (+ (@ chunk x0) chunk-size)) do
          (loop for y from (@ chunk y0) to (1- (+ (@ chunk y0) chunk-size)) do
            (loop for z from 0 to world-height do
              (let ((star 0))
                (dotimes (sample 8)
                  (when (chunk-cell-at chunk
                                       (+ x (if (logand sample 1) 0 -1))
                                       (+ y (if (logand sample 2) 0 -1))
                                       (+ z (if (logand sample 4) 0 -1)))
                    (setf star (logior star (ash 1 sample)))))
                (unless (or (= star 0) (= star 255))
                  ((@ sites push) (array x y z star)))))))
        sites))))

(defun residency-form ()
  "Main-thread residency policy. REQUEST-CHUNK is injectable for CPU fixtures."
  '(progn
    (defvar streaming-enabled false)
    (defvar chunks (new (|Map|)))
    (defvar chunk-added (lambda (chunk) chunk))
    (defvar chunk-removed (lambda (chunk) chunk))
    (defvar chunk-changed (lambda (chunk) chunk))
    (defvar invalidate-jobs (lambda () null))
    (defvar dispose-generator (lambda () null))
    (defvar demand-x 0)
    (defvar demand-y 0)
    (defvar request-chunk
      (lambda (cx cy)
        (let ((chunk (make-chunk cx cy)))
          ((@ chunks set) (@ chunk key) chunk)
          (chunk-added chunk)
          chunk)))
    (defun load-chunk (cx cy)
      (let* ((key (chunk-key cx cy)) (old ((@ chunks get) key)))
        (or old (request-chunk cx cy))))
    (defun stream-cell-at (x y z)
      (if (or (< z 0) (>= z world-height))
          0
          (let ((chunk ((@ chunks get)
                        (chunk-key (floor (/ x chunk-size))
                                   (floor (/ y chunk-size))))))
            ;; Do not let physics walk or fall into CPU work that has not
            ;; become renderable yet.
            (if chunk (chunk-cell-at chunk x y z) 1))))
    (defun stream-center-x (x) (floor (/ x chunk-size)))
    (defun stream-center-y (y) (floor (/ y chunk-size)))
    (defun load-radius (cx cy radius)
      ;; Concentric rings give startup the nearest chunks first.
      (loop for ring from 0 to radius do
        (loop for dx from (- ring) to ring do
          (loop for dy from (- ring) to ring do
            (when (= (max (abs dx) (abs dy)) ring)
              (load-chunk (+ cx dx) (+ cy dy)))))))
    (defun clear-streaming-chunks ()
      (invalidate-jobs)
      ((@ chunks for-each) (lambda (chunk key) (declare (ignore key)) (chunk-removed chunk)))
      ((@ chunks clear)))
    (defun start-streaming (x y)
      (clear-streaming-chunks)
      (setf streaming-enabled true
            demand-x (stream-center-x x) demand-y (stream-center-y y))
      (setf cell-source stream-cell-at)
      (load-radius (stream-center-x x) (stream-center-y y) 1))
    (defun update-streaming (x y)
      (when streaming-enabled
        (let ((cx (stream-center-x x)) (cy (stream-center-y y)))
          (setf demand-x cx demand-y cy)
          ((@ chunks for-each)
           (lambda (chunk key)
             (when (> (max (abs (- (@ chunk cx) cx)) (abs (- (@ chunk cy) cy))) 6)
               ((@ chunks delete) key)
               (chunk-removed chunk))))
          ;; Edited resident chunks take priority over distant expansion.
          ((@ chunks for-each)
           (lambda (chunk key)
             (declare (ignore key))
             (when (@ chunk dirty) (request-chunk (@ chunk cx) (@ chunk cy)))))
          ;; Transport bounds in-flight work; demand is recomputed each call.
          (block found
            (loop for ring from 0 to 5 do
              (loop for dx from (- ring) to ring do
                (loop for dy from (- ring) to ring do
                  (when (and (= (max (abs dx) (abs dy)) ring)
                             (not ((@ chunks has) (chunk-key (+ cx dx) (+ cy dy)))))
                    (load-chunk (+ cx dx) (+ cy dy))
                    (return-from found null)))))))))
    (defun stream-ready-radius (x y)
      (let ((cx (stream-center-x x)) (cy (stream-center-y y)) (ready -1))
        (loop for ring from 0 to 5 do
          (let ((complete true))
            (loop for dx from (- ring) to ring do
              (loop for dy from (- ring) to ring do
                (unless ((@ chunks has) (chunk-key (+ cx dx) (+ cy dy)))
                  (setf complete false))))
            (if complete (setf ready ring) (return))))
        ready))
    (defun stop-streaming ()
      (clear-streaming-chunks)
      (dispose-generator)
      (setf streaming-enabled false)
      (setf cell-source null))
    (defun edit-world-cell (x y z kind)
      (if (or (not ((@ |Number| is-safe-integer) x))
              (not ((@ |Number| is-safe-integer) y))
              (not ((@ |Number| is-safe-integer) z))
              (not ((@ |Number| is-safe-integer) kind))
              (< z 0) (>= z world-height) (< kind 0) (> kind 5))
          false
          (let ((key (cell-key x y z)))
            (invalidate-jobs)
            ((@ world-edits set) key kind)
            ((@ chunks for-each)
             (lambda (chunk ignored)
               (declare (ignore ignored))
               (when (and (>= x (1- (@ chunk x0))) (<= x (+ (@ chunk x0) chunk-size))
                          (>= y (1- (@ chunk y0))) (<= y (+ (@ chunk y0) chunk-size)))
                 (setf (aref (@ chunk data) (chunk-index chunk x y z)) kind)
                 (chunk-changed chunk))))
            true)))))

(defun streaming-form ()
  `(progn ,(generation-form) ,(residency-form)))
