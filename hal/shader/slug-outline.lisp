;;; CPU-owned quadratic outline and Slug band preprocessing.

(in-package #:luv.slug)

(define-condition slug-outline-error (error)
  ((reason :initarg :reason :reader slug-outline-error-reason)
   (details :initarg :details :initform nil
            :reader slug-outline-error-details))
  (:report
   (lambda (condition stream)
     (format stream "Cannot pack Slug outline: ~A~@[ (~S)~]."
             (slug-outline-error-reason condition)
             (slug-outline-error-details condition)))))

(defparameter *slug-maximum-band-count* 16
  "The most bands an axis may be cut into when the count is chosen
automatically.  The reference's format allows 256; more bands cost header
texels and, past a point, only overlap epsilon.")

(defparameter *slug-band-epsilon* 1/1024
  "How far, in em, a band reaches past its edges when deciding which curves
it holds, so a curve grazing a boundary is in both bands.  The reference
suggests 1/1024.")

(defstruct slug-point x y)
(defstruct slug-quadratic start control end)
(defstruct slug-outline contours)
(defstruct slug-band curve-indices ascending-curve-indices)
(defstruct slug-packed-outline
  curves min-x min-y max-x max-y horizontal-bands vertical-bands)

(defun make-slug-line (start end)
  "Encode a straight segment in Slug's {p1,p2,p2} quadratic form."
  (make-slug-quadratic :start start :control end :end end))

(defun slug-point= (left right)
  (and (= (slug-point-x left) (slug-point-x right))
       (= (slug-point-y left) (slug-point-y right))))

(defun validate-slug-contour (contour contour-index)
  (unless contour
    (error 'slug-outline-error
           :reason :empty-contour :details contour-index))
  (loop for curve in contour
        for next in (append (rest contour) (list (first contour)))
        for curve-index from 0
        unless (slug-point= (slug-quadratic-end curve)
                            (slug-quadratic-start next))
          do (error 'slug-outline-error
                    :reason :disconnected-contour
                    :details (list contour-index curve-index)))
  contour)

(defun slug-outline-curves (outline)
  "Return OUTLINE's validated curves in contour order."
  (loop for contour in (slug-outline-contours outline)
        for contour-index from 0
        append (validate-slug-contour contour contour-index)))

(defun slug-quadratic-signed-area (curve)
  (let* ((p0 (slug-quadratic-start curve))
         (p1 (slug-quadratic-control curve))
         (p2 (slug-quadratic-end curve))
         (c (slug-point-x p0))
         (f (slug-point-y p0))
         (b (* 2 (- (slug-point-x p1) c)))
         (e (* 2 (- (slug-point-y p1) f)))
         (a (+ c (- (* 2 (slug-point-x p1))) (slug-point-x p2)))
         (d (+ f (- (* 2 (slug-point-y p1))) (slug-point-y p2))))
    (* 1/2
       (+ (- (* c e) (* f b))
          (- (* c d) (* f a))
          (/ (- (* b d) (* a e)) 3)))))

(defun slug-contour-signed-area (contour)
  "Return the exact signed area swept by a connected quadratic CONTOUR."
  (validate-slug-contour contour 0)
  (reduce #'+ contour :key #'slug-quadratic-signed-area :initial-value 0))

(defun slug-contour-orientation (contour)
  (let ((area (slug-contour-signed-area contour)))
    (cond ((plusp area) :counterclockwise)
          ((minusp area) :clockwise)
          (t :degenerate))))

(defun slug-curve-coordinate-values (curve axis)
  (let ((reader (ecase axis (:x #'slug-point-x) (:y #'slug-point-y))))
    (mapcar reader
            (list (slug-quadratic-start curve)
                  (slug-quadratic-control curve)
                  (slug-quadratic-end curve)))))

(defun slug-curve-min (curve axis)
  (reduce #'min (slug-curve-coordinate-values curve axis)))

(defun slug-curve-max (curve axis)
  (reduce #'max (slug-curve-coordinate-values curve axis)))

(defun slug-curve-axis-parallel-p (curve axis)
  (apply #'= (slug-curve-coordinate-values curve axis)))

(defun slug-band-range
    (curve axis minimum maximum band-count &key (epsilon 1/1024))
  (let ((span (- maximum minimum)))
    (if (zerop span)
        (values 0 0)
        (let ((size (/ span band-count)))
          (values
           (max 0
                (floor (- (/ (- (slug-curve-min curve axis) minimum) size)
                          epsilon)))
           (min (1- band-count)
                (floor (+ (/ (- (slug-curve-max curve axis) minimum) size)
                          epsilon))))))))

(defun slug-index-order-p (curves axis direction left right)
  (let* ((key (ecase direction
                (:descending #'slug-curve-max)
                (:ascending #'slug-curve-min)))
         (left-value (funcall key (aref curves left) axis))
         (right-value (funcall key (aref curves right) axis)))
    (or (ecase direction
          (:descending (> left-value right-value))
          (:ascending (< left-value right-value)))
        (and (= left-value right-value) (< left right)))))

(defun make-slug-bands
    (curves membership-axis sort-axis minimum maximum band-count)
  (let ((members (make-array band-count :initial-element nil)))
    (loop for curve across curves
          for index from 0
          unless (slug-curve-axis-parallel-p curve membership-axis)
            do (multiple-value-bind (low high)
                   (slug-band-range curve membership-axis minimum maximum
                                    band-count :epsilon *slug-band-epsilon*)
                 (loop for band from low to high
                       do (push index (aref members band)))))
    (loop for indices across members
          collect
          (make-slug-band
           :curve-indices
           (sort (copy-list indices)
                 (lambda (left right)
                   (slug-index-order-p curves sort-axis :descending
                                       left right)))
           :ascending-curve-indices
           (sort (copy-list indices)
                 (lambda (left right)
                   (slug-index-order-p curves sort-axis :ascending
                                       left right)))))))

(defun slug-band-load (curves membership-axis minimum maximum band-count)
  "Return the largest number of curves any of BAND-COUNT bands cut along
MEMBERSHIP-AXIS holds, and the total over all bands."
  (let ((loads (make-array band-count :initial-element 0)))
    (loop for curve across curves
          unless (slug-curve-axis-parallel-p curve membership-axis)
            do (multiple-value-bind (low high)
                   (slug-band-range curve membership-axis minimum maximum
                                    band-count :epsilon *slug-band-epsilon*)
                 (loop for band from low to high
                       do (incf (aref loads band)))))
    (values (reduce #'max loads) (reduce #'+ loads))))

(defun choose-slug-band-count
    (curves membership-axis minimum maximum
     &optional (maximum-count *slug-maximum-band-count*))
  "The fewest bands in [1, MAXIMUM-COUNT] that minimize the most curves any
one band holds: the reference's advice for choosing a glyph's band counts,
with ties going to the cheaper header block."
  (let ((best-count 1) (best-load nil))
    (loop for count from 1 to (max 1 (min maximum-count (length curves)))
          do (let ((load (slug-band-load curves membership-axis
                                         minimum maximum count)))
               (when (or (null best-load) (< load best-load))
                 (setf best-count count best-load load))))
    best-count))

(defun pack-slug-outline
    (outline &key horizontal-band-count vertical-band-count)
  "Build conservative, sorted horizontal and vertical Slug curve bands.

A band count left unspecified is chosen per axis by CHOOSE-SLUG-BAND-COUNT."
  (let* ((curve-list (slug-outline-curves outline))
         (curves (coerce curve-list 'vector))
         (curve-count (length curves)))
    (when (zerop curve-count)
      (error 'slug-outline-error :reason :empty-outline))
    (let* ((min-x (loop for curve across curves
                        minimize (slug-curve-min curve :x)))
           (min-y (loop for curve across curves
                        minimize (slug-curve-min curve :y)))
           (max-x (loop for curve across curves
                        maximize (slug-curve-max curve :x)))
           (max-y (loop for curve across curves
                        maximize (slug-curve-max curve :y)))
           (horizontal-count
             (or horizontal-band-count
                 (choose-slug-band-count curves :y min-y max-y)))
           (vertical-count
             (or vertical-band-count
                 (choose-slug-band-count curves :x min-x max-x))))
      (unless (and (<= 1 horizontal-count 255)
                   (<= 1 vertical-count 255))
        (error 'slug-outline-error
               :reason :invalid-band-count
               :details (list horizontal-count vertical-count)))
      (make-slug-packed-outline
       :curves curves :min-x min-x :min-y min-y :max-x max-x :max-y max-y
       :horizontal-bands
       (make-slug-bands curves :y :x min-y max-y horizontal-count)
       :vertical-bands
       (make-slug-bands curves :x :y min-x max-x vertical-count)))))
