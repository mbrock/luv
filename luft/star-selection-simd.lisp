(in-package #:luft)

;;; Instruction-level implementations of the mixed-occupancy predicate.
;;; STAR-SELECTION-INSTRUCTION-SET owns machine choice; the query itself sees
;;; only a function that maps eight four-word fibers to four active-site words.

(defun %star-active-words-scalar
    (below above southwest southeast northwest northeast active)
  (declare (optimize (speed 3) (safety 0))
           (type star-fiber-vector below above active)
           (type fixnum southwest southeast northwest northeast))
  (dotimes (word +star-fiber-word-count+ active)
    (setf (aref active word)
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
  "Define NAME by changing only scalar load/logic/store spellings."
  (flet ((sym (name) (intern name package)))
    (let* ((aref-wide (sym (format nil "U64.~D-AREF" lanes)))
           (and-wide (sym (format nil "U64.~D-AND" lanes)))
           (or-wide (sym (format nil "U64.~D-OR" lanes)))
           (not-wide (sym (format nil "U64.~D-NOT" lanes)))
           ;; Loads and stores belong to the base ISA package (AVX for AVX2
           ;; arithmetic), which is the home package of its public AREF.
           (vop-package (symbol-package aref-wide))
           (load-wide (intern (format nil "%U64.~D-LOAD" lanes) vop-package))
           (store-wide (intern (format nil "%U64.~D-STORE" lanes) vop-package)))
      `(defun ,name
           (below above southwest southeast northwest northeast active)
         (declare (optimize (speed 3) (safety 0))
                  (type star-fiber-vector below above active)
                  (type fixnum southwest southeast northwest northeast))
         (loop for word fixnum from 0 below +star-fiber-word-count+
               by ,lanes do
           (let ((below-southwest (,load-wide below (+ southwest word) 0))
                 (below-southeast (,load-wide below (+ southeast word) 0))
                 (below-northwest (,load-wide below (+ northwest word) 0))
                 (below-northeast (,load-wide below (+ northeast word) 0))
                 (above-southwest (,load-wide above (+ southwest word) 0))
                 (above-southeast (,load-wide above (+ southeast word) 0))
                 (above-northwest (,load-wide above (+ northwest word) 0))
                 (above-northeast (,load-wide above (+ northeast word) 0)))
             (,store-wide
              (%mixed-occupancy-form
                  (,and-wide ,or-wide ,not-wide)
                below-southwest below-southeast below-northwest below-northeast
                above-southwest above-southeast above-northwest above-northeast)
              active word 0)))
         active))))

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

(defun star-selection-instruction-set ()
  "Return the best available implementation name without running the query."
  #+x86-64
  (cond ((%star-simd-available-p :avx2) :avx2)
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
    #+x86-64 (:avx2 #'%star-active-words-avx2)
    #+x86-64 (:sse2 #'%star-active-words-sse2)
    #+arm64 (:neon #'%star-active-words-neon)))
