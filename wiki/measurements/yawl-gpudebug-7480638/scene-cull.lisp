;; Frozen-camera diagnostic: compact scene sites once during warmup.
;; This measures the GPU opportunity, not production per-frame culling cost.
;; Shadow geometry and visibility remain completely unchanged.
(in-package :luft.render)
(defvar *probe-culling-active* nil)
(defvar *probe-culling-cache* (make-hash-table :test #'equal))

(defun probe-culled-resident (renderer resident shadow-p)
  (or (gethash (list resident shadow-p) *probe-culling-cache*)
      (let* ((population (resident-population-population resident))
             (sites (render-population-instance-words population))
             (appearance (render-population-appearance-words population))
             (lights (render-population-light-words population))
             (clip (if shadow-p (light-clip-function *frame-uniform-data*)
                       (camera-clip-function *frame-uniform-data*)))
             (kept nil)
             (size (parse-integer (or (uiop:getenv "PROBE_CLUSTER_SIZE") "16")))
             (visibility (make-hash-table :test #'equal)))
        (loop for i from 0 below (length sites) by 4
              for key = (loop for axis below 3 collect (floor (aref sites (+ i axis)) size))
              do (multiple-value-bind (visible found) (gethash key visibility)
                   (unless found
                     (let* ((low (mapcar (lambda (n) (* n size)) key))
                            (bounds (coerce (append (mapcar #'1- low)
                                                    (mapcar (lambda (n) (+ n size)) low))
                                            'vector)))
                       (setf visible (not (box-outside-clip-p bounds clip))
                             (gethash key visibility) visible)))
                   (when visible (push (/ i 4) kept))))
        (setf kept (nreverse kept))
        (flet ((compact (words stride)
                 (let ((result (make-array (* stride (length kept))
                                           :element-type '(unsigned-byte 32))))
                   (loop for source in kept for dest from 0
                         do (replace result words :start1 (* stride dest)
                                     :start2 (* stride source) :end2 (* stride (1+ source))))
                   result)))
          (setf (gethash (list resident shadow-p) *probe-culling-cache*)
                (%upload-render-population
                 renderer
                 (%make-render-population
                  (compact sites 4) (compact appearance 2)
                  (render-population-descriptor-words population)
                  (length kept) (compact lights 4)
                  (render-population-body-data population)
                  (render-population-shadow-rectangles population))))))))

(let ((original (symbol-function 'renderer-frame-resident-bind-group)))
  (setf (symbol-function 'renderer-frame-resident-bind-group)
        (lambda (renderer frame resident shadow-p)
          (funcall original renderer frame
                   (if (and *probe-culling-active* (not shadow-p))
                       (probe-culled-resident renderer resident shadow-p) resident)
                   shadow-p))))

(let ((original (symbol-function 'draw-resident-opaque-population)))
  (setf (symbol-function 'draw-resident-opaque-population)
        (lambda (terrain pass resident binding &key shadow-p)
          (funcall original terrain pass
                   (if (and *probe-culling-active* (not shadow-p))
                       (or (gethash (list resident shadow-p) *probe-culling-cache*)
                           (error "Expected culling cache from binding construction"))
                       resident)
                   binding :shadow-p shadow-p))))

(let ((original (symbol-function 'gpu-fps::measure)))
  (setf (symbol-function 'gpu-fps::measure)
        (lambda (&rest args)
          (setf *probe-culling-active* t)
          (apply original args))))

(let ((original (symbol-function 'gpu-fps::run-case)))
  (setf (symbol-function 'gpu-fps::run-case)
        (lambda (&rest args)
          (unwind-protect (apply original args)
            (setf *probe-culling-active* nil)
            (maphash (lambda (key value) (declare (ignore key)) (destroy value))
                     *probe-culling-cache*)
            (clrhash *probe-culling-cache*)))))
(in-package :cl-user)
