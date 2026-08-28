(in-package #:luft)

;;; Dense occupancy for the star selector
;;;
;;; A chunk chain is excellent authored storage, but selecting stars wants a
;;; different view: 256 vertical occupancy bits for each XY column.  These
;;; facts are immutable and cached by immutable chain identity.  A mesh query
;;; copies the few relevant fibers into one padded window, then asks only
;;; whether each group of eight neighboring bits is mixed solid and air.

(defconstant +star-fiber-word-count+ 4)
(defconstant +star-fiber-word-mask+ #xffffffffffffffff)
(defconstant +star-chunk-column-count+ (* +chunk-size+ +chunk-size+))
(defconstant +star-chunk-word-count+
  (* +star-chunk-column-count+ +star-fiber-word-count+))

(deftype star-fiber-vector ()
  '(simple-array (unsigned-byte 64) (*)))

(defstruct (star-chain-facts
             (:constructor %make-star-chain-facts (chunk-key words))
             (:copier nil))
  "One chunk chain viewed as 64x64 columns of four occupancy words."
  (chunk-key -1 :type fixnum :read-only t)
  (words #.(make-array 0 :element-type '(unsigned-byte 64))
         :type star-fiber-vector :read-only t))

(declaim (inline %star-chunk-fiber-base))
(defun %star-chunk-fiber-base (x y)
  (declare (optimize (speed 3) (safety 1))
           (type fixnum x y))
  (* +star-fiber-word-count+
     (+ (logand y (1- +chunk-size+))
        (ash (logand x (1- +chunk-size+)) +chunk-bits+))))

(defun %derive-star-chain-facts (chain)
  "Materialize CHAIN's cells as dense vertical occupancy fibers."
  (let* ((sites (%chain-sites chain))
         (count (length sites))
         (chunk-key (if (plusp count) (site-chunk-key (aref sites 0)) -1))
         (words (make-array +star-chunk-word-count+
                            :element-type '(unsigned-byte 64)
                            :initial-element 0)))
    (when (plusp count)
      (unless (= chunk-key (site-chunk-key (aref sites (1- count))))
        (error "Star selection facts require a single-chunk chain."))
      (loop for cell across sites do
        (unless (and (= +cell-extent+ (site-extent cell))
                     (site-positive-p cell))
          (error "A star mesh needs positive cells, not ~S." cell))
        (let ((z (site-z cell)))
          (unless (< z +top-z+)
            (error "Cell ~S lies above the star-selection box." cell))
          (let ((index (+ (%star-chunk-fiber-base (site-x cell) (site-y cell))
                          (ash z -6))))
            (setf (aref words index)
                  (logior (aref words index)
                          (ash 1 (logand z 63))))))))
    (%make-star-chain-facts chunk-key words)))

(defvar *star-chain-facts-table*
  (make-hash-table :test #'eq :weakness :key :synchronized t)
  "Weak identity cache from immutable chains to immutable occupancy facts.")

(defun %star-chain-facts (chain)
  (or (gethash chain *star-chain-facts-table*)
      (setf (gethash chain *star-chain-facts-table*)
            (%derive-star-chain-facts chain))))

(defstruct (star-selection-window
             (:constructor %make-star-selection-window
                 (above below x0 y0 y-span))
             (:copier nil))
  "A chunk lattice box plus its one-cell negative XY halo.

ABOVE stores ordinary cell occupancy.  BELOW is the same occupancy shifted
one bit toward increasing Z, so bit Z denotes the cell at Z-1."
  (above #() :type star-fiber-vector :read-only t)
  (below #() :type star-fiber-vector :read-only t)
  (x0 0 :type fixnum :read-only t)
  (y0 0 :type fixnum :read-only t)
  (y-span 1 :type fixnum :read-only t))

(declaim (inline %star-window-fiber-base))
(defun %star-window-fiber-base (window x y)
  (declare (optimize (speed 3) (safety 1))
           (type star-selection-window window)
           (type fixnum x y))
  (* +star-fiber-word-count+
     (+ (- y (star-selection-window-y0 window))
        (* (star-selection-window-y-span window)
           (- x (star-selection-window-x0 window))))))

(defun %copy-star-facts-into-window (facts window x1 y1)
  "Copy the intersection of FACTS and WINDOW through lattice edge X1,Y1."
  (let ((key (star-chain-facts-chunk-key facts)))
    (when (minusp key)
      (return-from %copy-star-facts-into-window window))
    (let* ((source-x0 (chunk-origin-x key))
           (source-y0 (chunk-origin-y key))
           (copy-x0 (max source-x0 (star-selection-window-x0 window)))
           (copy-y0 (max source-y0 (star-selection-window-y0 window)))
           (copy-x1 (min (1- (+ source-x0 +chunk-size+)) x1))
           (copy-y1 (min (1- (+ source-y0 +chunk-size+)) y1))
           (source (star-chain-facts-words facts))
           (destination (star-selection-window-above window)))
      (when (and (<= copy-x0 copy-x1) (<= copy-y0 copy-y1))
        (loop for x fixnum from copy-x0 to copy-x1 do
          (loop for y fixnum from copy-y0 to copy-y1
                for source-base fixnum = (%star-chunk-fiber-base x y)
                for destination-base fixnum =
                  (%star-window-fiber-base window x y)
                do (replace destination source
                            :start1 destination-base
                            :start2 source-base
                            :end2 (+ source-base +star-fiber-word-count+))))))
    window))

(defun %shift-star-window-below (window)
  "Derive the Z-1 view from WINDOW's ordinary occupancy fibers."
  (let ((above (star-selection-window-above window))
        (below (star-selection-window-below window)))
    (loop for base fixnum from 0 below (length above)
          by +star-fiber-word-count+ do
      (let ((previous 0))
        (declare (type (unsigned-byte 64) previous))
        (dotimes (word +star-fiber-word-count+)
          (let ((current (aref above (+ base word))))
            (setf (aref below (+ base word))
                  (logand +star-fiber-word-mask+
                          (logior (ash current 1)
                                  (ldb (byte 1 63) previous)))
                  previous current)))))
    window))

(defun %materialize-star-selection-window (chains x0 x1 y0 y1)
  "Copy CHAINS into the output lattice box and its negative one-cell halo."
  (let* ((window-x0 (1- x0))
         (window-y0 (1- y0))
         (x-span (+ (- x1 x0) 2))
         (y-span (+ (- y1 y0) 2))
         (word-count (* x-span y-span +star-fiber-word-count+))
         (window
           (%make-star-selection-window
            (make-array word-count :element-type '(unsigned-byte 64)
                                   :initial-element 0)
            (make-array word-count :element-type '(unsigned-byte 64))
            window-x0 window-y0 y-span)))
    (dolist (chain chains)
      (%copy-star-facts-into-window
       (%star-chain-facts chain) window x1 y1))
    (%shift-star-window-below window)))

;;; The one geometric predicate in selection.  An eight-cell star has surface
;;; exactly when some incident cells are solid and some are air.  The macro is
;;; shared by scalar and SIMD kernels so the instruction spelling can vary
;;; without duplicating this definition.

(defmacro %mixed-occupancy-form
    ((and-operator or-operator not-operator) &rest occupancy)
  `(,and-operator
    (,or-operator ,@occupancy)
    (,not-operator (,and-operator ,@occupancy))))

(declaim (inline %mixed-occupancy-word))
(defun %mixed-occupancy-word
    (below-southwest below-southeast below-northwest below-northeast
     above-southwest above-southeast above-northwest above-northeast)
  (%mixed-occupancy-form
      (logand logior lognot)
    below-southwest below-southeast below-northwest below-northeast
    above-southwest above-southeast above-northwest above-northeast))

(declaim (inline %star-mask-from-window))
(defun %star-mask-from-window
    (below above southwest southeast northwest northeast word bit)
  "Read one eight-bit occupancy star from four paired vertical fibers."
  (declare (optimize (speed 3) (safety 0))
           (type star-fiber-vector below above)
           (type fixnum southwest southeast northwest northeast word bit))
  (logior
   (if (logbitp bit (aref below (+ southwest word))) #x01 0)
   (if (logbitp bit (aref below (+ southeast word))) #x02 0)
   (if (logbitp bit (aref below (+ northwest word))) #x04 0)
   (if (logbitp bit (aref below (+ northeast word))) #x08 0)
   (if (logbitp bit (aref above (+ southwest word))) #x10 0)
   (if (logbitp bit (aref above (+ southeast word))) #x20 0)
   (if (logbitp bit (aref above (+ northwest word))) #x40 0)
   (if (logbitp bit (aref above (+ northeast word))) #x80 0)))
