;;; The small coordinate and frame-ABI seam between Luvcraft and LUFT.
;;;
;;; Luvcraft remains an authored Y-up world.  LUFT remains a Z-up finite
;;; lattice.  One adapter owns one mutable LUFT camera and expresses each
;;; Luvcraft frame through it; the render loop never manufactures a second
;;; camera object merely to cross that boundary.

(in-package #:luvcraft)

(defconstant +luft-frame-float-count+ 104
  "The append-only LUFT surface frame ABI, in single floats.")

(defconstant +luft-frame-sun-lane+ 5)
(defconstant +luft-frame-sky-lane+ 6)
(defconstant +luft-frame-sun-colour-lane+ 8)
(defconstant +luft-frame-fill-lane+ 9)
(defconstant +luft-frame-ground-lane+ 10)

(defconstant +luvcraft-frame-shadow-row-offset+ 60
  "The first float of Luvcraft's four captured shadow-projector rows.")

(defconstant +luft-shadow-projector-float-count+ 16
  "Four vec4 rows, suitable for one 64-byte LUFT projector buffer.")

(defclass luft-frame-adapter ()
  ((camera
    :initform (luft.render:make-fly-camera)
    :reader luft-frame-adapter-camera
    :documentation
    "The one mutable Z-up LUFT camera reused by every adapted frame."))
  (:documentation
   "The bounded mutable view seam from one Luvcraft session into LUFT.

The adapter owns no world or GPU resource.  Those remain respectively with
LUFT-WORLD-MATERIALIZATION and the acquired renderer frame state."))

(defun make-luft-frame-adapter ()
  "Make a reusable Luvcraft-to-LUFT frame adapter."
  (make-instance 'luft-frame-adapter))

(defun update-luft-frame-adapter-camera
    (adapter source-camera vertical-origin)
  "Update and return ADAPTER's one LUFT camera from SOURCE-CAMERA.

Luvcraft is Y-up, while LUFT is Z-up.  A Luvcraft position (X,Y,Z) is thus
expressed as (X,Z,Y-VERTICAL-ORIGIN), and its yaw becomes PI/2-YAW.  That yaw
identity converts the complete right/up/forward basis, not merely forward.
Pitch and vertical field of view are invariant under the axis permutation."
  (check-type adapter luft-frame-adapter)
  (check-type source-camera fly-camera)
  (check-type vertical-origin integer)
  (let* ((target (luft-frame-adapter-camera adapter))
         (source-position (camera-position source-camera))
         (target-position (luft.render:camera-position target)))
    ;; Mutate the existing vector as well as the existing camera.  Acquired
    ;; frame objects may retain the camera's identity for inspection.
    (setf (vec3-x target-position)
          (coerce (vec3-x source-position) 'single-float)
          (vec3-y target-position)
          (coerce (vec3-z source-position) 'single-float)
          (vec3-z target-position)
          (coerce (- (vec3-y source-position) vertical-origin) 'single-float)
          (luft.render:camera-yaw target)
          (coerce (- (/ pi 2) (camera-yaw source-camera)) 'single-float)
          (luft.render:camera-pitch target)
          (coerce (camera-pitch source-camera) 'single-float)
          (luft.render:camera-field-of-view target)
          (coerce (camera-field-of-view source-camera) 'single-float))
    target))

(declaim (inline set-luft-frame-lane))
(defun set-luft-frame-lane (data lane x y z w)
  "Overwrite one named four-float lane of a LUFT frame block."
  (let ((offset (* 4 lane)))
    (setf (aref data offset) (coerce x 'single-float)
          (aref data (+ offset 1)) (coerce y 'single-float)
          (aref data (+ offset 2)) (coerce z 'single-float)
          (aref data (+ offset 3)) (coerce w 'single-float)))
  data)

(defun luvcraft-frame-shadow-projector-data (frame-data vertical-origin)
  "Convert FRAME-DATA's captured Luvcraft shadow rows into LUFT coordinates.

FRAME-DATA is the already packed 76-float Luvcraft frame block; this function
does not evaluate SHADOW-FRAME-ROWS again.  For authored coordinates (X,Y,Z)
and LUFT coordinates (X,Z,Y-VERTICAL-ORIGIN), each source row (A B C D) pulls
back to (A C B D+B*VERTICAL-ORIGIN).  The result is a fresh simple array of
sixteen single floats, exactly one 64-byte projector buffer payload."
  (check-type frame-data (simple-array single-float (76)))
  (check-type vertical-origin integer)
  (let ((projector
          (make-array +luft-shadow-projector-float-count+
                      :element-type 'single-float))
        (origin (coerce vertical-origin 'single-float)))
    (dotimes (row 4 projector)
      (let* ((source (+ +luvcraft-frame-shadow-row-offset+ (* 4 row)))
             (a (aref frame-data source))
             (b (aref frame-data (+ source 1)))
             (c (aref frame-data (+ source 2)))
             (d (aref frame-data (+ source 3))))
        (set-luft-frame-lane
         projector row a c b (+ d (* b origin)))))))

(defun apply-luvcraft-sky-to-luft-frame (data sky)
  "Put Luvcraft's linear sun and isotropic ambient into LUFT frame DATA.

LUFT represents ambient light as one strength times separate sky and ground
colours.  Luvcraft has one linear ambient RGB.  Factoring its largest channel
as the strength and putting the normalized colour into both hemispheres is
exact: their directional weights sum to the original isotropic ambient.
The spare LUFT fill light is disabled, and the direct colour includes
Luvcraft's twilight day factor.  Exposure and fog are deliberately untouched;
the enclosing Luvcraft HDR/post pipeline owns presentation."
  (check-type data (simple-array single-float (*)))
  (check-type sky sky-frame-parameters)
  (let* ((sun (sky-frame-parameters-sun-direction sky))
         (sun-colour (sky-frame-parameters-sun-color sky))
         (ambient-colour (sky-frame-parameters-ambient-color sky))
         (ambient-strength
           (max 0.0
                (aref ambient-colour 0)
                (aref ambient-colour 1)
                (aref ambient-colour 2)))
         (ambient-red
           (if (plusp ambient-strength)
               (/ (aref ambient-colour 0) ambient-strength)
               0.0))
         (ambient-green
           (if (plusp ambient-strength)
               (/ (aref ambient-colour 1) ambient-strength)
               0.0))
         (ambient-blue
           (if (plusp ambient-strength)
               (/ (aref ambient-colour 2) ambient-strength)
               0.0))
         (day-factor (sky-frame-parameters-day-factor sky)))
    ;; Directions receive the same Y-up -> Z-up permutation as the camera,
    ;; without the vertical-origin translation which only positions receive.
    (set-luft-frame-lane
     data +luft-frame-sun-lane+
     (vec3-x sun) (vec3-z sun) (vec3-y sun) ambient-strength)
    (set-luft-frame-lane
     data +luft-frame-sky-lane+
     ambient-red ambient-green ambient-blue
     (aref data (1- (* 4 (1+ +luft-frame-sky-lane+)))))
    (set-luft-frame-lane
     data +luft-frame-sun-colour-lane+
     (* day-factor (aref sun-colour 0))
     (* day-factor (aref sun-colour 1))
     (* day-factor (aref sun-colour 2))
     (aref data (1- (* 4 (1+ +luft-frame-sun-colour-lane+)))))
    (set-luft-frame-lane
     data +luft-frame-fill-lane+
     (aref data (* 4 +luft-frame-fill-lane+))
     (aref data (+ 1 (* 4 +luft-frame-fill-lane+)))
     (aref data (+ 2 (* 4 +luft-frame-fill-lane+)))
     0.0)
    (set-luft-frame-lane
     data +luft-frame-ground-lane+
     ambient-red ambient-green ambient-blue
     (aref data (1- (* 4 (1+ +luft-frame-ground-lane+))))))
  data)

(defun luft-frame-adapter-domain-uniform-data
    (adapter session domain vertical-origin width height &key (sky-p t))
  "Pack one 104-float LUFT stock-surface frame for a direct LUFT DOMAIN.

ADAPTER's camera is updated in place.  The stock chamfer and arris values come
from LUFT itself, retaining its procedural material and microtexture model.
With SKY-P, Luvcraft's evaluated sun and ambient replace LUFT's atelier light."
  (check-type adapter luft-frame-adapter)
  (check-type session luvcraft-session)
  (check-type domain luft:world-domain)
  (check-type vertical-origin integer)
  (check-type width (integer 1 *))
  (check-type height (integer 1 *))
  (let* ((camera
           (update-luft-frame-adapter-camera
            adapter
            (luvcraft-session-camera session)
            vertical-origin))
         (data
           (luft.render:frame-uniform-data
            camera width height
            domain
            luft.render:*chamfer-width*
            luft.render:*arris-softness*)))
    (unless (= +luft-frame-float-count+ (length data))
      (error "LUFT frame adapter expected ~D floats, but LUFT packed ~D."
             +luft-frame-float-count+ (length data)))
    (when sky-p
      (apply-luvcraft-sky-to-luft-frame
       data
       (sky-frame-parameters (luvcraft-session-sky-clock session)
                             (luvcraft-session-sky-profile session))))
    data))

(defun luft-frame-adapter-uniform-data
    (adapter session materialization width height &key (sky-p t))
  "Pack a frame for the temporary legacy-world-to-LUFT materialization."
  (luft-frame-adapter-domain-uniform-data
   adapter session
   (luft-world-materialization-domain materialization)
   (luft-world-materialization-vertical-origin materialization)
   width height :sky-p sky-p))
