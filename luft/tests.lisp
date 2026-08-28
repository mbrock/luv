(in-package #:luft)

(defvar *luft-test-count* 0)

(defmacro %check (form)
  `(progn
     (incf *luft-test-count*)
     (unless ,form (error "LUFT check failed: ~S" ',form))))

(defun %one-cell-chain ()
  (let* ((domain (make-world-domain :horizontal-bits 4))
         (builder (make-chain-builder domain)))
    (chain-builder-add-site builder (make-site domain 4 4 4 +cell-extent+ 1))
    (finish-chain-builder builder)))

(defun %test-one-cell-stars ()
  (let* ((chain (%one-cell-chain))
         (mesh
           (handler-bind
               ((missing-chunk
                  (lambda (condition)
                    (declare (ignore condition))
                    (invoke-restart 'treat-as-air))))
             (mesh-star-chunk chain (chunk-key-at 4 4)
                              :outside-domain-policy :air)))
         (words (surface-mesh-star-site-words mesh)))
    (%check (= 32 (length words)))
    (loop for offset from 3 below (length words) by 4
          for star = (aref words offset)
          do (%check (plusp star))
             (%check (zerop (logand star (1- star)))))
    (%check (= 44 (surface-mesh-triangle-count mesh)))))

(defun %test-atlas ()
  (%check (= 256 (length *star-atlas-owned-triangles*)))
  (dotimes (star 256)
    (%check (equal (star-atlas-owned-triangles star)
                   (svref *star-atlas-owned-triangles* star))))
  (%check (null (star-atlas-owned-triangles 0)))
  (%check (null (star-atlas-owned-triangles #xff))))

(defun %test-cubical-addressing ()
  (dotimes (star 256)
    (multiple-value-bind (representative transformation complemented-p)
        (star-canonical-form star :reflections t)
      (declare (ignore complemented-p))
      (%check (= star (transform-star transformation representative))))))

(defun run-luft-tests (&key (stream *standard-output*))
  (setf *luft-test-count* 0)
  (%test-one-cell-stars)
  (%test-atlas)
  (%test-cubical-addressing)
  (format stream "LUFT: ~D star checks passed.~%" *luft-test-count*)
  (values t *luft-test-count*))
