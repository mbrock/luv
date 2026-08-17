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

(deftest sites-pack-domain-anchors-extents-and-polarity-in-fixnums
  (let* ((domain (make-world-domain :x-bits 12 :y-bits 16))
         (negative (make-site domain -1 (ash 1 16) 37
                              (make-extent :x :z) -1))
         (positive (opposite-site negative)))
    (ok (typep negative 'site))
    (ok (typep negative 'fixnum))
    (ok (site-valid-p domain negative))
    (ok (= (1- (ash 1 12)) (site-x negative)))
    (ok (zerop (site-y negative)))
    (ok (= 37 (site-z negative)))
    (ok (= #b0101 (site-extent negative)))
    (ok (= 2 (site-dimension negative)))
    (ok (site-negative-p negative))
    (ok (site-positive-p positive))
    (ok (= -1 (site-polarity negative)))
    (ok (= 1 (site-polarity positive)))
    (ok (= positive (site-geometry negative)))
    (ok (= negative (opposite-site positive)))))

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
    (ok (null (site-coface-forward domain (make-site domain 0 0 255) :z)))
    (ok (null (site-coface-backward domain (make-site domain 0 0 0) :z)))))

(deftest horizontal-wrap-shares-one-geometry-with-opposite-polarities
  (let* ((domain (make-world-domain :x-bits 4 :y-bits 6))
         (last-x (world-domain-x-mask domain))
         (left (make-site domain last-x 20 30 +cell-extent+))
         (right (make-site domain 0 20 30 +cell-extent+))
         (last-y (world-domain-y-mask domain))
         (south (make-site domain 3 last-y 30 +cell-extent+))
         (north (make-site domain 3 0 30 +cell-extent+))
         (left-face (site-boundary-high domain left :x))
         (right-face (site-boundary-low domain right :x))
         (south-face (site-boundary-high domain south :y))
         (north-face (site-boundary-low domain north :y)))
    (ok (= (site-geometry left-face) (site-geometry right-face)))
    (ok (= left-face (opposite-site right-face)))
    (ok (= (site-geometry south-face) (site-geometry north-face)))
    (ok (= south-face (opposite-site north-face)))))

(deftest horizontal-translation-obeys-each-domain-period
  (let* ((domain (make-world-domain :x-bits 4 :y-bits 3))
         (site (make-site domain 3 6 78 +cell-extent+ -1)))
    (ok (= site (step-site domain site :x (world-domain-x-period domain))))
    (ok (= site (step-site domain site :y (world-domain-y-period domain))))))

(deftest neighboring-cells-share-one-face-geometry
  (let* ((domain (make-world-domain :horizontal-bits 8))
         (left (make-site domain 10 20 30 +cell-extent+))
         (right (make-site domain 11 20 30 +cell-extent+))
         (left-face (site-boundary-high domain left :x))
         (right-face (site-boundary-low domain right :x)))
    (ok (= (site-geometry left-face) (site-geometry right-face)))
    (ok (= left-face (opposite-site right-face)))
    (ok (= +yz-face-extent+ (site-extent left-face)))))

(deftest signed-site-ledgers-annihilate-opposite-occurrences
  (let* ((domain (make-world-domain :horizontal-bits 4))
         (chain (make-chain domain))
         (positive (make-site domain 2 3 4 +xy-face-extent+))
         (negative (opposite-site positive)))
    (add-chain-site chain positive)
    (add-chain-site chain positive)
    (ok (= 2 (chain-site-count chain positive)))
    (add-chain-site chain negative)
    (ok (= 1 (chain-site-count chain positive)))
    (ok (zerop (chain-site-count chain negative)))
    (add-chain-site chain negative)
    (ok (zerop (chain-count chain)))))

(deftest cofaces-invert-their-corresponding-signed-boundaries
  (let* ((domain (make-world-domain :horizontal-bits 8))
         (face (make-site domain 17 23 31 +yz-face-extent+ -1))
         (forward (site-coface-forward domain face :x))
         (backward (site-coface-backward domain face :x)))
    (ok (= face (site-boundary-low domain forward :x)))
    (ok (= face (site-boundary-high domain backward :x)))))

(deftest the-oriented-boundary-of-a-boundary-annihilates
  (let* ((domain (make-world-domain :x-bits 4 :y-bits 5))
         (chain (make-chain domain)))
    (add-chain-site
     chain
     (make-site domain
                (world-domain-x-mask domain)
                (world-domain-y-mask domain)
                13 +cell-extent+))
    (ok (zerop (chain-count (boundary-chain (boundary-chain chain)))))))

(defun cube-chain (domain x y z)
  (let ((solid (make-solid-chain domain)))
    (setf (solid-cell-p solid x y z) t)
    solid))

(deftest a-single-cell-bounds-six-signed-faces
  (let* ((domain (make-world-domain :horizontal-bits 6))
         (surface (surface-chain (cube-chain domain 3 4 5))))
    (ok (= 6 (chain-count surface)))
    ;; X high is positive and X low negative.  Along Y one earlier axis flips
    ;; the convention; Z flips it back.
    (ok (chain-site-p
         surface (make-site domain 4 4 5 +yz-face-extent+ 1)))
    (ok (chain-site-p
         surface (make-site domain 3 4 5 +yz-face-extent+ -1)))
    (ok (chain-site-p
         surface (make-site domain 3 5 5 +xz-face-extent+ -1)))
    (ok (chain-site-p
         surface (make-site domain 3 4 5 +xz-face-extent+ 1)))
    (ok (chain-site-p
         surface (make-site domain 3 4 6 +xy-face-extent+ 1)))
    (ok (chain-site-p
         surface (make-site domain 3 4 5 +xy-face-extent+ -1)))))

(deftest a-negative-cell-reverses-every-boundary-polarity
  (let* ((domain (make-world-domain :horizontal-bits 6))
         (positive (make-chain domain))
         (negative (make-chain domain))
         (cell (make-site domain 3 4 5 +cell-extent+)))
    (add-chain-site positive cell)
    (add-chain-site negative (opposite-site cell))
    (let ((positive-boundary (boundary-chain positive))
          (negative-boundary (boundary-chain negative)))
      (ok (= 6 (chain-count negative-boundary)))
      (map-chain (lambda (site)
                   (ok (chain-site-p negative-boundary
                                     (opposite-site site))))
                 positive-boundary))))

(deftest neighbouring-cells-annihilate-their-shared-face
  (let* ((domain (make-world-domain :horizontal-bits 6))
         (solid (make-solid-chain domain)))
    (setf (solid-cell-p solid 3 4 5) t
          (solid-cell-p solid 4 4 5) t)
    (let* ((surface (surface-chain solid))
           (shared (make-site domain 4 4 5 +yz-face-extent+))
           (outer (make-site domain 5 4 5 +yz-face-extent+)))
      (ok (= 10 (chain-count surface)))
      (ok (not (chain-site-p surface shared)))
      (ok (not (chain-site-p surface (opposite-site shared))))
      (ok (chain-site-p surface outer))
      (ok (zerop (chain-count (boundary-chain surface)))))))

(deftest solid-surfaces-wrap-with-the-torus
  (let* ((domain (make-world-domain :x-bits 3 :y-bits 3))
         (solid (make-solid-chain domain)))
    (loop for x below (world-domain-x-period domain)
          do (setf (solid-cell-p solid x 2 2) t))
    ;; A full ring around X has no YZ faces at all.
    (let ((surface (surface-chain solid)))
      (ok (= (* 4 (world-domain-x-period domain)) (chain-count surface)))
      (ok (zerop (chain-count (boundary-chain surface)))))))

(deftest signed-sites-are-directly-gpu-ready-fixnums
  (let* ((domain (make-world-domain :horizontal-bits 4))
         (surface (surface-chain (cube-chain domain 1 2 3)))
         (sites (chain-sites surface)))
    (ok (typep sites '(simple-array (unsigned-byte 60) (*))))
    (ok (= 6 (length sites)))
    (ok (every (lambda (site) (typep site 'fixnum)) sites))
    (ok (loop for index from 1 below (length sites)
              always (< (aref sites (1- index)) (aref sites index))))
    (ok (every (lambda (site) (chain-site-p surface site)) sites))
    (ok (some #'site-positive-p sites))
    (ok (some #'site-negative-p sites))))
