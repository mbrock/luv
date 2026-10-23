(in-package #:luft.render)

;;; Fixture bodies are drawn with the terrain they stand in: each resident
;;; population carries its bodies sorted by code, and one instanced draw per
;;; code covers that code's run with its canonical triangles
;;; (fixture-body.lisp).  The terrain owns this drawing; frames bind it.

(defclass fixture-drawing (gpu-resource-owner)
  ((vertices :accessor fixture-drawing-vertices)
   ;; Per body code: (FIRST-VERTEX VERTEX-COUNT) in the shared stream.
   (ranges :accessor fixture-drawing-ranges)
   (scene-program :accessor fixture-drawing-scene-program)
   (shadow-program :accessor fixture-drawing-shadow-program)))

(defun make-fixture-drawing (device target-formats sample-count)
  (let ((drawing (make-instance 'fixture-drawing)))
    (with-gpu-construction (drawing)
      (multiple-value-bind (vertices ranges) (%make-fixture-body-vertex-data)
        (setf (fixture-drawing-ranges drawing) ranges
              (fixture-drawing-vertices drawing)
              (own-gpu-resource
               drawing device
               (make-buffer-descriptor
                :label "luft canonical fixture bodies"
                :size (* 4 (length vertices)) :usage '(:storage :copy-dst))))
        (write-buffer (fixture-drawing-vertices drawing) vertices))
      (setf (fixture-drawing-scene-program drawing)
            (own-gpu-object
             drawing
             (make-drawing-program
              device :label "luft fixture bodies"
              :vertex (shaders:fixture-body-vertex-specification)
              :fragment (shaders:fixture-body-fragment-specification)
              :targets (mapcar (lambda (format) `(:format ,format)) target-formats)
              :sample-count sample-count
              :depth-stencil '(:format :depth32-float :depth-write-enabled t
                               :depth-compare :less)))
            (fixture-drawing-shadow-program drawing)
            (own-gpu-object
             drawing
             (make-drawing-program
              device :label "luft fixture-body shadows"
              :vertex (shaders:fixture-body-shadow-vertex-specification)
              :depth-stencil '(:format :depth32-float :depth-write-enabled t
                               :depth-compare :less)))))))

(defun make-fixture-binding
    (drawing device bodies camera &key shadow-map shadow-sampler shadow-p)
  (apply #'make-program-binding
         (if shadow-p
             (fixture-drawing-shadow-program drawing)
             (fixture-drawing-scene-program drawing))
         device :fixture-bodies bodies
         :fixture-body-vertices (fixture-drawing-vertices drawing)
         :camera-state camera
         (unless shadow-p
           (list :shadow-map shadow-map :shadow-sampler shadow-sampler))))

(defun encode-fixture-bodies (drawing pass binding runs &key shadow-p)
  "Draw each (CODE FIRST COUNT) of RUNS with its body's canonical triangles."
  (let ((program (if shadow-p
                     (fixture-drawing-shadow-program drawing)
                     (fixture-drawing-scene-program drawing))))
    (loop for (code first count) in runs
          for (first-vertex vertex-count) = (aref (fixture-drawing-ranges drawing) code)
          when (plusp vertex-count)
            do (encode-program
                program pass binding
                (make-gpu-draw-command :vertex-count vertex-count
                                       :first-vertex first-vertex
                                       :instance-count count
                                       :first-instance first)))))

(defun fixture-body-runs (data)
  "The (CODE FIRST COUNT) runs of code-sorted body DATA."
  (let ((runs nil))
    (loop for offset from 0 below (length data)
            by luft.terrain::+fixture-body-scalar-count+
          for index from 0
          for code = (round (aref data (+ offset 3)))
          do (if (and runs (= code (first (first runs))))
                 (incf (third (first runs)))
                 (push (list code index 1) runs)))
    (nreverse runs)))

(defun fixture-body-bounds (data bounds)
  "BOUNDS, a cell box or NIL, grown to hold every body cell of DATA."
  (if (zerop (length data))
      bounds
      (let ((low (if bounds (subseq bounds 0 3) (list most-positive-single-float
                                                     most-positive-single-float
                                                     most-positive-single-float)))
            (high (if bounds (subseq bounds 3 6) (list most-negative-single-float
                                                      most-negative-single-float
                                                      most-negative-single-float))))
        (setf low (coerce low 'list) high (coerce high 'list))
        (loop for offset from 0 below (length data)
                by luft.terrain::+fixture-body-scalar-count+
              do (dotimes (axis 3)
                   (let ((value (aref data (+ offset axis))))
                     (setf (nth axis low) (min (nth axis low) value)
                           (nth axis high) (max (nth axis high) (+ value 1.0))))))
        (map '(simple-array single-float (6))
             (lambda (value) (coerce value 'single-float))
             (append low high)))))
