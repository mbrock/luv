(in-package #:luft)

;;; Star selection over chunk fibers
;;;
;;; A mesh query copies the relevant chunk fibers (see fibers.lisp) into one
;;; padded window, then asks only whether each group of eight neighboring
;;; bits is mixed solid and air.

(defstruct (star-selection-window
             (:constructor %make-star-selection-window
                 (above below x0 y0 y-span))
             (:copier nil))
  "A chunk lattice box plus its one-cell negative XY halo.

ABOVE stores ordinary cell occupancy.  BELOW is the same occupancy shifted
one bit toward increasing Z, so bit Z denotes the cell at Z-1."
  (above #() :type fiber-vector :read-only t)
  (below #() :type fiber-vector :read-only t)
  (x0 0 :type fixnum :read-only t)
  (y0 0 :type fixnum :read-only t)
  (y-span 1 :type fixnum :read-only t))

(declaim (inline %star-window-fiber-base))
(defun %star-window-fiber-base (window x y)
  (declare (optimize (speed 3) (safety 1))
           (type star-selection-window window)
           (type fixnum x y))
  (* +fiber-word-count+
     (+ (- y (star-selection-window-y0 window))
        (* (star-selection-window-y-span window)
           (- x (star-selection-window-x0 window))))))

(defun %copy-fibers-into-window (fibers window x1 y1)
  "Copy the intersection of FIBERS and WINDOW through lattice edge X1,Y1."
  (let ((key (chunk-fibers-key fibers)))
    (let* ((source-x0 (chunk-origin-x key))
           (source-y0 (chunk-origin-y key))
           (copy-x0 (max source-x0 (star-selection-window-x0 window)))
           (copy-y0 (max source-y0 (star-selection-window-y0 window)))
           (copy-x1 (min (1- (+ source-x0 +chunk-size+)) x1))
           (copy-y1 (min (1- (+ source-y0 +chunk-size+)) y1))
           (source (chunk-fibers-words fibers))
           (destination (star-selection-window-above window)))
      (when (and (<= copy-x0 copy-x1) (<= copy-y0 copy-y1))
        (loop for x fixnum from copy-x0 to copy-x1 do
          (loop for y fixnum from copy-y0 to copy-y1
                for source-base fixnum = (fiber-base x y)
                for destination-base fixnum =
                  (%star-window-fiber-base window x y)
                do (replace destination source
                            :start1 destination-base
                            :start2 source-base
                            :end2 (+ source-base +fiber-word-count+))))))
    window))

(defun %shift-star-window-below (window)
  "Derive the Z-1 view from WINDOW's ordinary occupancy fibers."
  (let ((above (star-selection-window-above window))
        (below (star-selection-window-below window)))
    (loop for base fixnum from 0 below (length above)
          by +fiber-word-count+ do
      (let ((previous 0))
        (declare (type (unsigned-byte 64) previous))
        (dotimes (word +fiber-word-count+)
          (let ((current (aref above (+ base word))))
            (setf (aref below (+ base word))
                  (logand +fiber-word-mask+
                          (logior (ash current 1)
                                  (ldb (byte 1 63) previous)))
                  previous current)))))
    window))

(defun %materialize-star-selection-window (chunks x0 x1 y0 y1)
  "Copy CHUNKS, chains or fibers, into the output lattice box and its negative one-cell halo."
  (let* ((window-x0 (1- x0))
         (window-y0 (1- y0))
         (x-span (+ (- x1 x0) 2))
         (y-span (+ (- y1 y0) 2))
         (word-count (* x-span y-span +fiber-word-count+))
         (window
           (%make-star-selection-window
            (make-array word-count :element-type '(unsigned-byte 64)
                                   :initial-element 0)
            (make-array word-count :element-type '(unsigned-byte 64))
            window-x0 window-y0 y-span)))
    (dolist (chunk chunks)
      (let ((fibers (etypecase chunk
                      (chunk-fibers chunk)
                      (chain (and (not (chain-empty-p chunk))
                                  (chain-fibers chunk))))))
        (when fibers
          (%copy-fibers-into-window fibers window x1 y1))))
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
           (type fiber-vector below above)
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
