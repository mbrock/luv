(in-package #:luft)

;;; Instruction-level implementations of the mixed-occupancy predicate.
;;; STAR-SELECTION-INSTRUCTION-SET owns machine choice; the query itself sees
;;; only functions that map eight occupancy fibers to active-site words.  The
;;; ordinary kernels classify one four-word Z fiber; AVX-512 classifies two
;;; adjacent Y fibers at once.

(defun %star-active-words-scalar
    (below above southwest southeast northwest northeast active active-start)
  (declare (optimize (speed 3) (safety 0))
           (type fiber-vector below above active)
           (type fixnum southwest southeast northwest northeast active-start))
  (dotimes (word +fiber-word-count+ active)
    (setf (aref active (+ active-start word))
          (%mixed-occupancy-word
           (aref below (+ southwest word))
           (aref below (+ southeast word))
           (aref below (+ northwest word))
           (aref below (+ northeast word))
           (aref above (+ southwest word))
           (aref above (+ southeast word))
           (aref above (+ northwest word))
           (aref above (+ northeast word))))))

(defmacro define-star-active-simd-kernel (name package lanes)
  "Define NAME with SB-SIMD's public multi-element array operations."
  (flet ((sym (name) (intern name package)))
    (let* ((aref-wide (sym (format nil "U64.~D-ROW-MAJOR-AREF" lanes)))
           (and-wide (sym (format nil "U64.~D-AND" lanes)))
           (or-wide (sym (format nil "U64.~D-OR" lanes)))
           (not-wide (sym (format nil "U64.~D-NOT" lanes))))
      `(defun ,name
           (below above southwest southeast northwest northeast
            active active-start)
         (declare (optimize (speed 3) (safety 0))
                  (type fiber-vector below above active)
                  (type fixnum southwest southeast northwest northeast
                       active-start))
         (loop for word fixnum from 0 below +fiber-word-count+
               by ,lanes do
           (let ((below-southwest (,aref-wide below (+ southwest word)))
                 (below-southeast (,aref-wide below (+ southeast word)))
                 (below-northwest (,aref-wide below (+ northwest word)))
                 (below-northeast (,aref-wide below (+ northeast word)))
                 (above-southwest (,aref-wide above (+ southwest word)))
                 (above-southeast (,aref-wide above (+ southeast word)))
                 (above-northwest (,aref-wide above (+ northwest word)))
                 (above-northeast (,aref-wide above (+ northeast word))))
             (setf (,aref-wide active (+ active-start word))
                   (%mixed-occupancy-form
                       (,and-wide ,or-wide ,not-wide)
                     below-southwest below-southeast
                     below-northwest below-northeast
                     above-southwest above-southeast
                     above-northwest above-northeast))))
         active))))

#+x86-64
(defun %star-active-words-avx512
    (below above southwest southeast northwest northeast active active-start)
  "Classify two adjacent Y fibers through Luft's SBCL 2.6.7 AVX-512 VOP."
  (luft.avx512::active-words
   below above southwest southeast northwest northeast active active-start))
#+x86-64
(define-star-active-simd-kernel
    %star-active-words-avx2 #:sb-simd-avx2 4)
#+x86-64
(define-star-active-simd-kernel
    %star-active-words-sse2 #:sb-simd-sse2 2)
#+arm64
(define-star-active-simd-kernel
    %star-active-words-neon #:sb-simd-neon 2)

(defun %star-simd-available-p (instruction-set)
  (sb-simd-internals:instruction-set-available-p
   (sb-simd-internals:find-instruction-set instruction-set)))

(defvar *star-selection-instruction-set-override* nil
  "Private diagnostic override for comparing otherwise identical selectors.")

(defun star-selection-instruction-set ()
  "Return the best available implementation name without running the query."
  (when *star-selection-instruction-set-override*
    (return-from star-selection-instruction-set
      *star-selection-instruction-set-override*))
  #+x86-64
  ;; The 512-bit Boolean kernel is faster, but preserving Y/X/Z order makes
  ;; its two-row staging slower for the complete selector on Zen 4.  Retain
  ;; it as a diagnostic backend; prefer the measured end-to-end winner.
  (cond ((%star-simd-available-p :avx2) :avx2)
        ((luft.avx512::available-p) :avx512)
        ((%star-simd-available-p :sse2) :sse2)
        (t :scalar))
  #+arm64
  (if (%star-simd-available-p :neon) :neon :scalar)
  #-(or x86-64 arm64)
  :scalar)

(defun %star-active-kernel ()
  "Resolve instruction policy separately from star-selection semantics."
  (ecase (star-selection-instruction-set)
    (:scalar #'%star-active-words-scalar)
    #+x86-64 (:avx512 #'%star-active-words-avx512)
    #+x86-64 (:avx2 #'%star-active-words-avx2)
    #+x86-64 (:sse2 #'%star-active-words-sse2)
    #+arm64 (:neon #'%star-active-words-neon)))
