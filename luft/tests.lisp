(defpackage #:luft.tests
  (:use #:cl #:rove #:luft))

(in-package #:luft.tests)

(deftest sites-pack-anchors-and-extents-in-fixnums
  (let ((site (make-site -1 (ash 1 24) 37
                         (make-extent :x :z :t))))
    (ok (typep site 'site))
    (ok (typep site 'fixnum))
    (ok (= (1- (ash 1 24)) (site-x site)))
    (ok (zerop (site-y site)))
    (ok (= 37 (site-z site)))
    (ok (= #b1101 (site-extent site)))
    (ok (= 2 (site-spatial-dimension site)))
    (ok (= 3 (site-dimension site)))))

(deftest the-vertical-world-has-planes-not-wrapping-cells
  (ok (site-valid-p (make-site 0 0 255)))
  (ok (signals (make-site 0 0 256) 'type-error))
  (ok (signals (make-site 0 0 255 +z-edge-extent+) 'error))
  (multiple-value-bind (coface temporal-offset)
      (site-coface-forward (make-site 0 0 255) :z)
    (ok (null coface))
    (ok (zerop temporal-offset)))
  (multiple-value-bind (coface temporal-offset)
      (site-coface-backward (make-site 0 0 0) :z)
    (ok (null coface))
    (ok (zerop temporal-offset))))

(deftest neighboring-cells-share-one-face-site
  (let ((left (make-site 10 20 30 +cell-extent+))
        (right (make-site 11 20 30 +cell-extent+)))
    (multiple-value-bind (left-face left-time)
        (site-boundary-high left :x)
      (multiple-value-bind (right-face right-time)
          (site-boundary-low right :x)
        (ok (= left-face right-face))
        (ok (= +yz-face-extent+ (site-extent left-face)))
        (ok (zerop left-time))
        (ok (zerop right-time))))))

(deftest temporal-incidence-is-relative-to-an-ambient-origin
  (let* ((cell (make-site 2 3 4 +cell-extent+))
         (history (make-site 2 3 4
                             (logior +cell-extent+ +temporal-extent+))))
    (multiple-value-bind (now now-offset)
        (site-boundary-low history :t)
      (multiple-value-bind (next next-offset)
          (site-boundary-high history :t)
        (ok (= cell now))
        (ok (= cell next))
        (ok (zerop now-offset))
        (ok (= 1 next-offset))))
    (multiple-value-bind (future origin-offset)
        (site-coface-forward cell :t)
      (ok (= history future))
      (ok (zerop origin-offset)))
    (multiple-value-bind (past origin-offset)
        (site-coface-backward cell :t)
      (ok (= history past))
      (ok (= -1 origin-offset)))))

(deftest cofaces-invert-their-corresponding-boundaries
  (let ((face (make-site 17 23 31 +yz-face-extent+)))
    (multiple-value-bind (forward forward-time)
        (site-coface-forward face :x)
      (multiple-value-bind (boundary boundary-time)
          (site-boundary-low forward :x)
        (ok (= face boundary))
        (ok (zerop (+ forward-time boundary-time)))))
    (multiple-value-bind (backward backward-time)
        (site-coface-backward face :x)
      (multiple-value-bind (boundary boundary-time)
          (site-boundary-high backward :x)
        (ok (= face boundary))
        (ok (zerop (+ backward-time boundary-time)))))))

(defun twice-boundary-coefficients (site)
  (let ((coefficients (make-hash-table :test #'equal)))
    (map-site-boundary
     (lambda (first sign-1 time-1 axis-1 side-1)
       (declare (ignore axis-1 side-1))
       (map-site-boundary
        (lambda (second sign-2 time-2 axis-2 side-2)
          (declare (ignore axis-2 side-2))
          (incf (gethash (cons second (+ time-1 time-2)) coefficients 0)
                (* sign-1 sign-2)))
        first))
     site)
    coefficients))

(deftest the-oriented-boundary-of-a-boundary-is-zero
  (let ((coefficients
          (twice-boundary-coefficients
           (make-site 7 11 13 (make-extent :x :y :z :t)))))
    (ok (plusp (hash-table-count coefficients)))
    (maphash (lambda (site coefficient)
               (declare (ignore site))
               (ok (zerop coefficient)))
             coefficients)))
