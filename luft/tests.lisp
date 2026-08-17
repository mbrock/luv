(defpackage #:luft.tests
  (:use #:cl #:rove #:luft))

(in-package #:luft.tests)

(deftest world-domains-realize-independent-horizontal-widths
  (let ((domain (make-world-domain :horizontal-bits 16 :y-bits 12)))
    (ok (= 16 (world-domain-x-bits domain)))
    (ok (= 12 (world-domain-y-bits domain)))
    (ok (= #xffff (world-domain-x-mask domain)))
    (ok (= #xfff (world-domain-y-mask domain)))
    (ok (= (ash 1 16) (world-domain-x-period domain)))
    (ok (= (ash 1 12) (world-domain-y-period domain))))
  (ok (signals (make-world-domain :x-bits 0) 'type-error))
  (ok (signals (make-world-domain :y-bits 25) 'type-error)))

(deftest sites-pack-domain-canonical-anchors-and-extents-in-fixnums
  (let* ((domain (make-world-domain :x-bits 12 :y-bits 16))
         (site (make-site domain -1 (ash 1 16) 37
                          (make-extent :x :z :t))))
    (ok (typep site 'site))
    (ok (typep site 'fixnum))
    (ok (site-valid-p domain site))
    (ok (= (1- (ash 1 12)) (site-x site)))
    (ok (zerop (site-y site)))
    (ok (= 37 (site-z site)))
    (ok (= #b1101 (site-extent site)))
    (ok (= 2 (site-spatial-dimension site)))
    (ok (= 3 (site-dimension site)))))

(deftest canonical-site-equality-is-domain-relative
  (let* ((small (make-world-domain :horizontal-bits 4))
         (wide (make-world-domain :horizontal-bits 24))
         (canonical (make-site small 3 5 7))
         (alias (make-site small 19 21 7))
         (wide-site (make-site wide 19 21 7)))
    (ok (= canonical alias))
    (ok (site-valid-p small canonical))
    (ok (not (site-valid-p small wide-site)))
    (ok (site-valid-p wide wide-site))))

(deftest the-vertical-world-has-planes-not-wrapping-cells
  (let ((domain (make-world-domain)))
    (ok (site-valid-p domain (make-site domain 0 0 255)))
    (ok (signals (make-site domain 0 0 256) 'type-error))
    (ok (signals (make-site domain 0 0 255 +z-edge-extent+) 'error))
    (multiple-value-bind (coface temporal-offset)
        (site-coface-forward domain (make-site domain 0 0 255) :z)
      (ok (null coface))
      (ok (zerop temporal-offset)))
    (multiple-value-bind (coface temporal-offset)
        (site-coface-backward domain (make-site domain 0 0 0) :z)
      (ok (null coface))
      (ok (zerop temporal-offset)))))

(deftest horizontal-wrap-shares-the-same-face
  (let* ((domain (make-world-domain :x-bits 4 :y-bits 6))
         (last-x (world-domain-x-mask domain))
         (left (make-site domain last-x 20 30 +cell-extent+))
         (right (make-site domain 0 20 30 +cell-extent+))
         (last-y (world-domain-y-mask domain))
         (south (make-site domain 3 last-y 30 +cell-extent+))
         (north (make-site domain 3 0 30 +cell-extent+)))
    (multiple-value-bind (left-face left-time)
        (site-boundary-high domain left :x)
      (multiple-value-bind (right-face right-time)
          (site-boundary-low domain right :x)
        (ok (= left-face right-face))
        (ok (zerop left-time))
        (ok (zerop right-time))))
    (multiple-value-bind (south-face south-time)
        (site-boundary-high domain south :y)
      (multiple-value-bind (north-face north-time)
          (site-boundary-low domain north :y)
        (ok (= south-face north-face))
        (ok (zerop south-time))
        (ok (zerop north-time))))))

(deftest horizontal-translation-obeys-each-domain-period
  (let* ((domain (make-world-domain :x-bits 4 :y-bits 3))
         (site (make-site domain 3 6 78 +cell-extent+)))
    (multiple-value-bind (x-wrap time)
        (step-site domain site :x (world-domain-x-period domain))
      (ok (= site x-wrap))
      (ok (zerop time)))
    (multiple-value-bind (y-wrap time)
        (step-site domain site :y (world-domain-y-period domain))
      (ok (= site y-wrap))
      (ok (zerop time)))))

(deftest neighboring-cells-share-one-face-site
  (let* ((domain (make-world-domain :horizontal-bits 8))
         (left (make-site domain 10 20 30 +cell-extent+))
         (right (make-site domain 11 20 30 +cell-extent+)))
    (multiple-value-bind (left-face left-time)
        (site-boundary-high domain left :x)
      (multiple-value-bind (right-face right-time)
          (site-boundary-low domain right :x)
        (ok (= left-face right-face))
        (ok (= +yz-face-extent+ (site-extent left-face)))
        (ok (zerop left-time))
        (ok (zerop right-time))))))

(deftest temporal-incidence-is-relative-to-an-ambient-origin
  (let* ((domain (make-world-domain :horizontal-bits 8))
         (cell (make-site domain 2 3 4 +cell-extent+))
         (history (make-site domain 2 3 4
                             (logior +cell-extent+ +temporal-extent+))))
    (multiple-value-bind (now now-offset)
        (site-boundary-low domain history :t)
      (multiple-value-bind (next next-offset)
          (site-boundary-high domain history :t)
        (ok (= cell now))
        (ok (= cell next))
        (ok (zerop now-offset))
        (ok (= 1 next-offset))))
    (multiple-value-bind (future origin-offset)
        (site-coface-forward domain cell :t)
      (ok (= history future))
      (ok (zerop origin-offset)))
    (multiple-value-bind (past origin-offset)
        (site-coface-backward domain cell :t)
      (ok (= history past))
      (ok (= -1 origin-offset)))))

(deftest cofaces-invert-their-corresponding-boundaries
  (let* ((domain (make-world-domain :horizontal-bits 8))
         (face (make-site domain 17 23 31 +yz-face-extent+)))
    (multiple-value-bind (forward forward-time)
        (site-coface-forward domain face :x)
      (multiple-value-bind (boundary boundary-time)
          (site-boundary-low domain forward :x)
        (ok (= face boundary))
        (ok (zerop (+ forward-time boundary-time)))))
    (multiple-value-bind (backward backward-time)
        (site-coface-backward domain face :x)
      (multiple-value-bind (boundary boundary-time)
          (site-boundary-high domain backward :x)
        (ok (= face boundary))
        (ok (zerop (+ backward-time boundary-time)))))))

(defun twice-boundary-coefficients (domain site)
  (let ((coefficients (make-hash-table :test #'equal)))
    (map-site-boundary
     (lambda (first sign-1 time-1 axis-1 side-1)
       (declare (ignore axis-1 side-1))
       (map-site-boundary
        (lambda (second sign-2 time-2 axis-2 side-2)
          (declare (ignore axis-2 side-2))
          (incf (gethash (cons second (+ time-1 time-2)) coefficients 0)
                (* sign-1 sign-2)))
        domain first))
     domain site)
    coefficients))

(deftest the-oriented-boundary-of-a-boundary-is-zero
  (let* ((domain (make-world-domain :x-bits 4 :y-bits 5))
         (coefficients
           (twice-boundary-coefficients
            domain
            (make-site domain
                       (world-domain-x-mask domain)
                       (world-domain-y-mask domain)
                       13 (make-extent :x :y :z :t)))))
    (ok (plusp (hash-table-count coefficients)))
    (maphash (lambda (site coefficient)
               (declare (ignore site))
               (ok (zerop coefficient)))
             coefficients)))

(defun cube-chain (domain x y z)
  (let ((solid (make-solid-chain domain)))
    (setf (solid-cell-p solid x y z) t)
    solid))

(deftest a-single-cell-bounds-six-signed-faces
  (let* ((domain (make-world-domain :horizontal-bits 6))
         (surface (surface-chain (cube-chain domain 3 4 5))))
    (ok (= 6 (chain-count surface)))
    ;; The high boundary along X carries the sign of the first axis, +1;
    ;; the low boundary carries -1.  Along Y one earlier axis flips it.
    (ok (= 1 (chain-coefficient
              surface (make-site domain 4 4 5 +yz-face-extent+))))
    (ok (= -1 (chain-coefficient
               surface (make-site domain 3 4 5 +yz-face-extent+))))
    (ok (= -1 (chain-coefficient
               surface (make-site domain 3 5 5 +xz-face-extent+))))
    (ok (= 1 (chain-coefficient
              surface (make-site domain 3 4 5 +xz-face-extent+))))
    (ok (= 1 (chain-coefficient
              surface (make-site domain 3 4 6 +xy-face-extent+))))
    (ok (= -1 (chain-coefficient
               surface (make-site domain 3 4 5 +xy-face-extent+))))))

(deftest neighbouring-cells-cancel-their-shared-face
  (let* ((domain (make-world-domain :horizontal-bits 6))
         (solid (make-solid-chain domain)))
    (setf (solid-cell-p solid 3 4 5) t
          (solid-cell-p solid 4 4 5) t)
    (let ((surface (surface-chain solid)))
      (ok (= 10 (chain-count surface)))
      (ok (zerop (chain-coefficient
                  surface (make-site domain 4 4 5 +yz-face-extent+))))
      (ok (= 1 (chain-coefficient
                surface (make-site domain 5 4 5 +yz-face-extent+))))
      (ok (= 0 (chain-count (boundary-chain surface)))))))

(deftest solid-surfaces-wrap-with-the-torus
  (let* ((domain (make-world-domain :x-bits 3 :y-bits 3))
         (solid (make-solid-chain domain)))
    (loop for x below (world-domain-x-period domain)
          do (setf (solid-cell-p solid x 2 2) t))
    ;; A full ring around X has no YZ faces at all.
    (let ((surface (surface-chain solid)))
      (ok (= (* 4 (world-domain-x-period domain)) (chain-count surface)))
      (ok (zerop (chain-count (boundary-chain surface)))))))

(deftest packed-terms-carry-site-and-sign-in-one-fixnum
  (let* ((domain (make-world-domain :horizontal-bits 4))
         (surface (surface-chain (cube-chain domain 1 2 3)))
         (terms (chain-packed-terms surface)))
    (ok (typep terms '(simple-array (unsigned-byte 64) (*))))
    (ok (= 6 (length terms)))
    (ok (every (lambda (term) (typep term 'fixnum)) terms))
    (ok (loop for index from 1 below (length terms)
              always (< (packed-term-site (aref terms (1- index)))
                        (packed-term-site (aref terms index)))))
    (loop for term across terms
          do (ok (= (packed-term-coefficient term)
                    (chain-coefficient surface (packed-term-site term)))))
    (ok (signals (pack-term (make-site domain 0 0 0) 2) 'type-error))))

(deftest temporal-chains-have-no-packed-boundary
  (let* ((domain (make-world-domain :horizontal-bits 4))
         (chain (make-chain domain)))
    (add-chain-term chain (make-site domain 0 0 0 (make-extent :x :t)) 1)
    (ok (not (chain-spatial-p chain)))
    (ok (signals (boundary-chain chain) 'error))))
