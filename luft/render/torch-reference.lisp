(in-package #:luft.render)

;;; Scalar CPU references for the flame field/integral and transformed body
;;; vertices. These are numerical inspection oracles, not a second live scene.

(defun %torch-flame-clamp (value low high)
  (max low (min high value)))

(defun %torch-flame-smoothstep (low high value)
  (let ((amount (%torch-flame-clamp (/ (- value low) (- high low)) 0.0 1.0)))
    (* amount amount (- 3.0 (* 2.0 amount)))))

(defun %torch-flame-fract (value)
  (- value (floor value)))

(defun %torch-flame-reference-field
    (instance point-x point-y point-z time)
  (validate-torch-flame-frame instance)
  (let* ((origin-x (aref instance 0))
         (origin-y (aref instance 1))
         (origin-z (aref instance 2))
         (seed (aref instance 3))
         (nx (aref instance 4))
         (ny (aref instance 5))
         (nz (aref instance 6))
         (tx (aref instance 8))
         (ty (aref instance 9))
         (tz (aref instance 10))
         (scale (aref instance 11))
         ;; B = N x T, hence T x B = N for the validated orthonormal pair.
         (bx (- (* ny tz) (* nz ty)))
         (by (- (* nz tx) (* nx tz)))
         (bz (- (* nx ty) (* ny tx)))
         ;; Project world up into the actual surface tangent plane.  Its length
         ;; continuously replaces the old axis-specific wallness switch.
         (gravity-x (* (- nz) nx))
         (gravity-y (* (- nz) ny))
         (gravity-z (- 1.0 (* nz nz)))
         (gravity-strength
           (sqrt (max 0.0 (+ (* gravity-x gravity-x)
                             (* gravity-y gravity-y)
                             (* gravity-z gravity-z)))))
         (gravity-divisor (max gravity-strength 1.0e-6))
         (gravity-x (/ gravity-x gravity-divisor))
         (gravity-y (/ gravity-y gravity-divisor))
         (gravity-z (/ gravity-z gravity-divisor))
         (wick-distance (* +torch-flame-wick-offset+ scale))
         (flame-length (* +torch-flame-length+ scale))
         (wick-x (+ origin-x (* nx wick-distance)))
         (wick-y (+ origin-y (* ny wick-distance)))
         (wick-z (+ origin-z (* nz wick-distance)))
         (qx (- point-x wick-x))
         (qy (- point-y wick-y))
         (qz (- point-z wick-z))
         (axial (/ (+ (* qx nx) (* qy ny) (* qz nz)) flame-length))
         (height (%torch-flame-clamp axial 0.0 1.0))
         (height-squared (* height height))
         (sway-u (* scale 0.105 height-squared
                    (sin (+ (* time 5.1) (* seed 19.7) (* height 5.3)))))
         (sway-v (* scale 0.075 height-squared
                    (sin (+ (* time 6.7) (* seed 31.1) (* height 7.1)))))
         (gravity-bend (* scale +torch-flame-wall-bend+ gravity-strength
                          height-squared))
         (center-x (+ wick-x (* nx flame-length height)
                      (* tx sway-u) (* bx sway-v)
                      (* gravity-x gravity-bend)))
         (center-y (+ wick-y (* ny flame-length height)
                      (* ty sway-u) (* by sway-v)
                      (* gravity-y gravity-bend)))
         (center-z (+ wick-z (* nz flame-length height)
                      (* tz sway-u) (* bz sway-v)
                      (* gravity-z gravity-bend)))
         (rx (- point-x center-x))
         (ry (- point-y center-y))
         (rz (- point-z center-z))
         (radial (sqrt (+ (* rx rx) (* ry ry) (* rz rz))))
         (bulge (%torch-flame-smoothstep 0.0 0.22 height))
         (radius (* scale (- 1.0 height) (+ 0.055 (* 0.13 bulge))))
         (signed-distance (- radial radius))
         (begin (%torch-flame-smoothstep -0.02 0.08 axial))
         (end (- 1.0 (%torch-flame-smoothstep 0.78 1.04 axial)))
         (inside (- 1.0 (%torch-flame-smoothstep
                         -0.025 0.035 signed-distance)))
         (wave (+ 0.5 (* 0.5
                         (sin (+ (* point-x 17.1) (* point-y 13.7)
                                 (* point-z 19.3) (* time -8.1)
                                 (* seed 23.9))))))
         (fine (+ 0.5 (* 0.5
                         (sin (+ (* point-x -31.7) (* point-y 27.3)
                                 (* point-z 23.1) (* time 11.3)
                                 (* seed 7.7))))))
         (density (* begin end inside
                     (+ 0.68 (* 0.22 wave) (* 0.10 fine))))
         (centrality
           (%torch-flame-clamp
            (/ (- signed-distance) (max radius 0.001)) 0.0 1.0))
         (heat (%torch-flame-clamp
                (+ 0.20 (* 0.95 centrality) (* -0.32 height)) 0.0 1.0)))
    (values signed-distance density heat)))

(defun torch-flame-reference-signed-distance
    (instance point-x point-y point-z time)
  "Evaluate the scalar flame envelope used by the GPU at one world point."
  (nth-value 0 (%torch-flame-reference-field
                instance point-x point-y point-z time)))

(defun torch-flame-reference-density
    (instance point-x point-y point-z time)
  "Evaluate the animated scalar extinction density at one world point."
  (nth-value 1 (%torch-flame-reference-field
                instance point-x point-y point-z time)))

(defun torch-flame-reference-integrate-ray
    (instance origin-x origin-y origin-z ray-x ray-y ray-z time
     &key maximum-path-length)
  "Return the fixed-sample premultiplied HDR RGBA flame integral along RAY.

MAXIMUM-PATH-LENGTH, when supplied, clips the proxy chord to the visible
distance in front of opaque scene depth.  It is clamped to the canonical proxy
chord exactly as the fragment shader does, so zero is a deterministic empty
integral and callers can compare full, partial, and fully occluded rays."
  (validate-torch-flame-frame instance)
  (let* ((ray-length (sqrt (+ (* ray-x ray-x) (* ray-y ray-y)
                              (* ray-z ray-z)))))
    (when (zerop ray-length)
      (error "A torch-flame reference ray must have nonzero length."))
    (let* ((ray-x (/ ray-x ray-length))
           (ray-y (/ ray-y ray-length))
           (ray-z (/ ray-z ray-length))
           (full-path-length
             (* 2.0 +torch-flame-proxy-radius+ (aref instance 11)))
           (path-length
             (if maximum-path-length
                 (%torch-flame-clamp maximum-path-length
                                     0.0 full-path-length)
                 full-path-length))
           (step-length (/ path-length +torch-flame-sample-count+))
           (red 0.0) (green 0.0) (blue 0.0) (alpha 0.0))
      (multiple-value-bind (authored-red authored-green authored-blue)
          (%torch-flame-authored-hdr-radiance)
        (dotimes (sample +torch-flame-sample-count+)
          (let ((travel (* (+ sample 0.5) step-length)))
            (multiple-value-bind (distance density heat)
                (%torch-flame-reference-field
                 instance
                 (+ origin-x (* ray-x travel))
                 (+ origin-y (* ray-y travel))
                 (+ origin-z (* ray-z travel)) time)
              (declare (ignore distance))
              (let* ((sample-alpha
                       (- 1.0 (exp (- (* density +torch-flame-extinction+
                                          step-length)))))
                     (transmittance (- 1.0 alpha))
                     (weight (* transmittance sample-alpha))
                     (radiance-scale
                       (+ +torch-flame-cool-radiance-scale+
                          (* heat +torch-flame-heat-radiance-gain+))))
                (incf red (* weight authored-red radiance-scale))
                (incf green (* weight authored-green radiance-scale))
                (incf blue (* weight authored-blue radiance-scale))
                (incf alpha weight))))))
      (values red green blue alpha))))

(defun torch-body-reference-vertex (frame vertex-index)
  "Transform one canonical body vertex through arbitrary realized FRAME.

Return world position and normalized world normal.  This is the scalar oracle
for both body vertex shaders."
  (validate-torch-flame-frame frame)
  (check-type vertex-index (integer 0 *))
  (unless (< vertex-index (torch-body-vertex-count))
    (error "Torch body vertex ~D exceeds the ~D-vertex canonical body."
           vertex-index (torch-body-vertex-count)))
  (let* ((offset (* vertex-index +torch-body-vertex-scalar-count+))
         (local-x (aref *torch-body-vertex-data* offset))
         (local-y (aref *torch-body-vertex-data* (+ offset 1)))
         (local-z (aref *torch-body-vertex-data* (+ offset 2)))
         (local-normal-x (aref *torch-body-vertex-data* (+ offset 4)))
         (local-normal-y (aref *torch-body-vertex-data* (+ offset 5)))
         (local-normal-z (aref *torch-body-vertex-data* (+ offset 6)))
         (origin-x (aref frame 0))
         (origin-y (aref frame 1))
         (origin-z (aref frame 2))
         (normal-x (aref frame 4))
         (normal-y (aref frame 5))
         (normal-z (aref frame 6))
         (tangent-x (aref frame 8))
         (tangent-y (aref frame 9))
         (tangent-z (aref frame 10))
         (scale (aref frame 11))
         ;; B=NxT, so local X/Y/Z map to a right-handed T/B/N frame.
         (bitangent-x (- (* normal-y tangent-z)
                         (* normal-z tangent-y)))
         (bitangent-y (- (* normal-z tangent-x)
                         (* normal-x tangent-z)))
         (bitangent-z (- (* normal-x tangent-y)
                         (* normal-y tangent-x)))
         (world-x
           (+ origin-x
              (* scale (+ (* local-x tangent-x)
                          (* local-y bitangent-x)
                          (* local-z normal-x)))))
         (world-y
           (+ origin-y
              (* scale (+ (* local-x tangent-y)
                          (* local-y bitangent-y)
                          (* local-z normal-y)))))
         (world-z
           (+ origin-z
              (* scale (+ (* local-x tangent-z)
                          (* local-y bitangent-z)
                          (* local-z normal-z)))))
         (world-normal-x
           (+ (* local-normal-x tangent-x)
              (* local-normal-y bitangent-x)
              (* local-normal-z normal-x)))
         (world-normal-y
           (+ (* local-normal-x tangent-y)
              (* local-normal-y bitangent-y)
              (* local-normal-z normal-y)))
         (world-normal-z
           (+ (* local-normal-x tangent-z)
              (* local-normal-y bitangent-z)
              (* local-normal-z normal-z)))
         (world-normal-length
           (sqrt (+ (* world-normal-x world-normal-x)
                    (* world-normal-y world-normal-y)
                    (* world-normal-z world-normal-z)))))
    (values world-x world-y world-z
            (/ world-normal-x world-normal-length)
            (/ world-normal-y world-normal-length)
            (/ world-normal-z world-normal-length))))
