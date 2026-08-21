;;; The discrete geometry ABI between a LUFT solid and a renderer.
;;;
;;; This is the renderer-facing part of luft-foundation.lisp: one exposed
;;; face becomes four u32 words.  The CPU classifies each shared edge and
;;; corner exactly once; a vertex stage can realize the resulting 4x4 patch
;;; without reading occupancy.  Topological sites remain the authority.

(in-package #:luft)

(defconstant +edge-balanced+ #b00)
(defconstant +edge-convex+ #b01)
(defconstant +edge-concave+ #b10)

(defconstant +edge-u-low-shift+ 0)
(defconstant +edge-u-high-shift+ 2)
(defconstant +edge-v-low-shift+ 4)
(defconstant +edge-v-high-shift+ 6)
(defconstant +corner-ll-shift+ 8)
(defconstant +corner-lh-shift+ 14)
(defconstant +corner-hl-shift+ 20)
(defconstant +corner-hh-shift+ 26)

(defconstant +face-record-words+ 4)
(defconstant +site-bits+ 60)
(defconstant +stock-bits+ 4)
(defconstant +stock-shift+ +site-bits+)

(declaim (inline foundation-axis-bit foundation-face-basis))

(defun foundation-axis-bit (axis)
  (ash 1 axis))

(defun foundation-face-basis (extent)
  "Return the canonical U, V, normal axis, and normal sign of EXTENT."
  (ecase extent
    (#.+xy-face-extent+ (values 0 1 2 1))
    (#.+xz-face-extent+ (values 0 2 1 -1))
    (#.+yz-face-extent+ (values 1 2 0 1))))

(defun foundation-map-site-star (function site)
  "Call FUNCTION with cell coordinates and directions in SITE's full star."
  (let ((extent (site-extent site))
        (anchor (vector (site-x site) (site-y site) (site-z site)))
        (absent '()))
    (dotimes (axis 3)
      (unless (logbitp axis extent)
        (push axis absent)))
    (setf absent (nreverse absent))
    (dotimes (choice (ash 1 (length absent)))
      (let ((cell (copy-seq anchor))
            (direction (vector 0 0 0)))
        (loop for axis in absent
              for bit from 0
              do (if (logbitp bit choice)
                     (setf (aref direction axis) 1)
                     (progn
                       (decf (aref cell axis))
                       (setf (aref direction axis) -1))))
        (funcall function
                 (aref cell 0) (aref cell 1) (aref cell 2)
                 (aref direction 0) (aref direction 1)
                 (aref direction 2))))))

(defun foundation-site-shape (site occupancy)
  "Return the signed strict-minority direction, reach numerator, and census."
  (let ((n 0) (k 0)
        (solid (vector 0 0 0))
        (air (vector 0 0 0)))
    (foundation-map-site-star
     (lambda (x y z dx dy dz)
       (incf n)
       (let ((sum (if (funcall occupancy x y z)
                      (progn (incf k) solid)
                      air)))
         (incf (aref sum 0) dx)
         (incf (aref sum 1) dy)
         (incf (aref sum 2) dz)))
     site)
    (let* ((moment (cond ((< (* 2 k) n) solid)
                         ((> (* 2 k) n) air)
                         (t #(0 0 0))))
           (qx (signum (aref moment 0)))
           (qy (signum (aref moment 1)))
           (qz (signum (aref moment 2)))
           (two-thirds-p
             (and (zerop (site-extent site))
                  (= 1 (abs (aref moment 0)))
                  (= 1 (abs (aref moment 1)))
                  (= 1 (abs (aref moment 2))))))
      (values qx qy qz (if two-thirds-p 2 1) k n))))

(defun foundation-face-edge (domain face which)
  (multiple-value-bind (u v normal normal-sign)
      (foundation-face-basis (site-extent face))
    (declare (ignore normal normal-sign))
    (let ((coordinates (vector (site-x face) (site-y face) (site-z face))))
      (flet ((bump (axis) (incf (aref coordinates axis))))
        (let ((along
                (ecase which
                  (:u-low v)
                  (:u-high (bump u) v)
                  (:v-low u)
                  (:v-high (bump v) u))))
          (make-site domain
                     (aref coordinates 0) (aref coordinates 1)
                     (aref coordinates 2) (foundation-axis-bit along)))))))

(defun foundation-face-corner (domain face u-high-p v-high-p)
  (multiple-value-bind (u v normal normal-sign)
      (foundation-face-basis (site-extent face))
    (declare (ignore normal normal-sign))
    (let ((coordinates (vector (site-x face) (site-y face) (site-z face))))
      (when u-high-p (incf (aref coordinates u)))
      (when v-high-p (incf (aref coordinates v)))
      (make-site domain
                 (aref coordinates 0) (aref coordinates 1)
                 (aref coordinates 2) +vertex-extent+))))

(defun foundation-edge-code (site occupancy)
  (multiple-value-bind (qx qy qz reach k n)
      (foundation-site-shape site occupancy)
    (declare (ignore qx qy qz reach))
    (unless (= n 4)
      (error "~S is not an edge star." site))
    (ecase k
      (1 +edge-convex+)
      (2 +edge-balanced+)
      (3 +edge-concave+))))

(defun foundation-corner-code (site occupancy)
  (multiple-value-bind (qx qy qz reach k n)
      (foundation-site-shape site occupancy)
    (declare (ignore k n))
    (logior (+ (1+ qx) (* 3 (1+ qy)) (* 9 (1+ qz)))
            (if (= reach 2) #x20 0))))

(defun face-shape-word (domain face occupancy)
  "Classify FACE into the foundation's four edge and four corner fields."
  (unless (and (site-valid-p domain face) (= 2 (site-dimension face)))
    (error "~S is not a canonical face in ~S." face domain))
  (flet ((edge (which)
           (foundation-edge-code
            (foundation-face-edge domain face which) occupancy))
         (corner (u-high-p v-high-p)
           (foundation-corner-code
            (foundation-face-corner domain face u-high-p v-high-p)
            occupancy)))
    (logior (ash (edge :u-low) +edge-u-low-shift+)
            (ash (edge :u-high) +edge-u-high-shift+)
            (ash (edge :v-low) +edge-v-low-shift+)
            (ash (edge :v-high) +edge-v-high-shift+)
            (ash (corner nil nil) +corner-ll-shift+)
            (ash (corner nil t) +corner-lh-shift+)
            (ash (corner t nil) +corner-hl-shift+)
            (ash (corner t t) +corner-hh-shift+))))

(defun decorated-site (site &optional (stock 0))
  "Attach a four-bit material slot above SITE's sixty topological bits."
  (check-type site site)
  (check-type stock (unsigned-byte 4))
  (logior site (ash stock +stock-shift+)))

(defun pack-face-record (words offset decorated shape-word)
  "Write DECORATED and SHAPE-WORD as one foundation four-u32 record."
  (setf (aref words offset) (ldb (byte 32 0) decorated)
        (aref words (+ offset 1)) (ldb (byte 32 32) decorated)
        (aref words (+ offset 2)) shape-word
        (aref words (+ offset 3)) 0)
  words)

(defun unpack-face-record (words offset)
  "Return decorated site, shape word, and reserved word at OFFSET."
  (values (logior (aref words offset) (ash (aref words (+ offset 1)) 32))
          (aref words (+ offset 2))
          (aref words (+ offset 3))))
