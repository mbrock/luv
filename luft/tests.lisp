(in-package #:luft)

;;; Focused executable claims for the retained topology and the replacement
;;; manifold-sheet mesher.

(defvar *luft-test-count* 0)
(defvar *luft-test-section* nil)

(defmacro %check (form &optional note)
  `(progn
     (incf *luft-test-count*)
     (unless ,form
       (error "LUFT test failed in ~A~@[ (~A)~]: ~S"
              *luft-test-section* ,note ',form))))

(defmacro %with-test-section ((name) &body body)
  `(let ((*luft-test-section* ,name)) ,@body))

(defun %signals-error-p (thunk)
  (handler-case (progn (funcall thunk) nil)
    (error () t)))

(defun %chain-from-sites (domain sites)
  (let ((builder (make-chain-builder domain :initial-capacity (length sites))))
    (dolist (site sites) (chain-builder-add-site builder site))
    (finish-chain-builder builder)))

(defun %boundary-sites (domain site)
  (let ((parts '()))
    (map-site-boundary
     (lambda (part axis side)
       (declare (ignore axis side))
       (push part parts))
     domain site)
    (nreverse parts)))

(defun %test-sites-and-chains ()
  (%with-test-section ("packed sites, chains, and boundary")
    (let* ((domain (make-world-domain :x-bits 3 :y-bits 3))
           (cell (make-site domain 2 3 7 +cell-extent+ 1))
           (neighbor (make-site domain 3 3 7 +cell-extent+ 1))
           (solid (%chain-from-sites domain (list cell neighbor)))
           (surface (surface-chain solid)))
      (%check (= 2 (chain-count solid)))
      (%check (= 10 (chain-count surface)))
      (%check (= 6 (length (%boundary-sites domain cell))))
      (%check (site-valid-p domain cell))
      (%check (= cell (opposite-site (opposite-site cell))))
      (%check (= 1 (chain-cell-occupancy-bit solid 2 3 7)))
      (%check (= 0 (chain-cell-occupancy-bit solid 2 3 6)))
      (%check (null (site-backward domain
                                   (make-site domain 2 3 0) :z)))
      (%check (%signals-error-p
               (lambda ()
                 (make-site domain 2 3 +top-z+ +z-edge-extent+))))
      ;; Boundary squared remains the chain-level topological invariant.
      (%check (chain-empty-p (boundary-chain (boundary-chain solid))))
      ;; The value is immutable through its public vector view.
      (let ((copy (chain-sites solid)))
        (setf (aref copy 0) 0)
        (%check (= 2 (chain-count solid)))))))

(defun %edge-set= (left right)
  (equal (sort (copy-list left) #'<)
         (sort (copy-list right) #'<)))

(defun %test-sheet-decomposition ()
  (%with-test-section ("manifold sheet decomposition")
    (let ((singular-count 0))
      (dotimes (mask 256)
        (when (star-singular-p mask) (incf singular-count))
        (let ((cycles (%star-sheet-cycles mask)))
          (%check (= (length (%star-boundary-edges mask))
                     (loop for cycle in cycles sum (length cycle))))
          (%check (%edge-set= (%star-boundary-edges mask)
                              (loop for cycle in cycles append cycle)))))
      (%check (= 128 singular-count)))
    ;; Representatives of the two singular mechanisms and the maximally
    ;; crossed checkerboard resolve into the intended ordinary sheets.
    (%check (equal '(#x02 #x04)
                   (sort (decompose-star-mask #x06) #'<)))
    (%check (equal '(#x08 #x10)
                   (sort (decompose-star-mask #x18) #'<)))
    (%check (equal '(#x01 #x08 #x20 #x40)
                   (sort (decompose-star-mask #x69) #'<)))
    ;; This is the deliberate spike boundary: its occupied-side cycle needs a
    ;; covered junction and cannot be disguised as an ordinary eight-bit star.
    (%check (%signals-error-p (lambda () (decompose-star-mask #x6f))))))

(defun %solid-for-star (mask &key (centre '(8 8 8)))
  (let* ((domain (make-world-domain :horizontal-bits 5))
         (builder (make-chain-builder domain :initial-capacity 8)))
    (dotimes (sample 8)
      (when (logbitp sample mask)
        (let ((coordinates
                (loop for axis-number below 3
                      collect (+ (nth axis-number centre)
                                 (if (logbitp axis-number sample) 0 -1)))))
          (chain-builder-add-site
           builder
           (make-site domain
                      (first coordinates) (second coordinates)
                      (third coordinates) +cell-extent+ 1)))))
    (finish-chain-builder builder)))

(defun %mesh-point (mesh index)
  (let* ((words (surface-mesh-vertex-words mesh))
         (base (* +mesh-vertex-word-count+ index)))
    (list (aref words base) (aref words (+ base 1))
          (aref words (+ base 2)))))

(defun %ordered-edge (left right)
  (if (or (< (first left) (first right))
          (and (= (first left) (first right))
               (or (< (second left) (second right))
                   (and (= (second left) (second right))
                        (< (third left) (third right))))))
      (list left right)
      (list right left)))

(defun %mesh-geometric-edge-counts (mesh)
  (let ((counts (make-hash-table :test #'equal))
        (indices (surface-mesh-indices mesh)))
    (loop for base from 0 below (length indices) by 3 do
      (let ((points
              (vector (%mesh-point mesh (aref indices base))
                      (%mesh-point mesh (aref indices (+ base 1)))
                      (%mesh-point mesh (aref indices (+ base 2))))))
        (dotimes (index 3)
          (incf (gethash (%ordered-edge
                          (aref points index)
                          (aref points (mod (1+ index) 3)))
                         counts 0)))))
    counts))

(defun %mesh-closed-p (mesh)
  (loop for count being the hash-values of (%mesh-geometric-edge-counts mesh)
        always (= count 2)))

(defun %test-surface-mesh ()
  (%with-test-section ("integer surface mesh")
    (%check (equal '(0 -1 0) (%normal-direction-code '(0 -2 0))))
    (let ((one (make-surface-mesh (%solid-for-star #x01))))
      ;; Six exposed faces, each with one square, four flaps, and four
      ;; triangles: fourteen triangles and forty-two indices per face.
      (%check (= (* 6 +mesh-face-template-triangle-count+)
                 (surface-mesh-face-triangle-count one)))
      (%check (zerop (surface-mesh-band-triangle-count one)))
      (%check (zerop (surface-mesh-junction-triangle-count one)))
      (%check (= 84 (surface-mesh-triangle-count one)))
      (%check (= (* 3 (surface-mesh-triangle-count one))
                 (surface-mesh-index-count one)))
      (%check (= (* 6 +mesh-face-template-index-count+)
                 (surface-mesh-index-count one)))
      (%check (= (* +mesh-vertex-word-count+
                    (surface-mesh-index-count one))
                 (length (surface-mesh-vertex-words one))))
      (%check (not (%mesh-closed-p one))))
    ;; The fixed face template remains deliberately open at every corner even
    ;; where two exposed faces are coplanar.
    (let ((pair (make-surface-mesh (%solid-for-star #x03))))
      (%check (zerop (surface-mesh-singular-star-count pair)))
      (%check (not (%mesh-closed-p pair))))
    (dolist (mask '(#x06 #x18 #x69))
      (let ((mesh (make-surface-mesh (%solid-for-star mask))))
        (%check (plusp (surface-mesh-singular-star-count mesh))
                (format nil "mask ~2,'0X" mask))
        (%check (not (%mesh-closed-p mesh))
                (format nil "mask ~2,'0X" mask))))))

(defun run-luft-tests (&key (stream *standard-output*))
  "Run the retained topology and replacement manifold-sheet mesh claims."
  (let ((*luft-test-count* 0)
        (*luft-test-section* nil))
    (%test-sites-and-chains)
    (%test-sheet-decomposition)
    (%test-surface-mesh)
    (when stream
      (format stream "~&LUFT: ~D checks passed.~%" *luft-test-count*))
    (values t *luft-test-count*)))
