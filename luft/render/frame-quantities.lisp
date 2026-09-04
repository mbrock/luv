;;; The semantic contract shared by Luft's fixed scene-uniform ABI.

(in-package #:luft.render.shaders)

;;; Every stage reading the scene environment declares the same uniform
;;; block.  Member order and offsets are representation; the component
;;; declarations beside them are the independently checked meanings of the
;;; lanes.  Projection rows and categorical controls intentionally remain
;;; raw where one fixed quantity would be dishonest.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *scene-uniform-members*
    '(;; CAMERA-POSITION.W is the categorical first-person HUD selector.
      (camera-position :vec4
       :components
       ((:xyz :quantity quantities:world-position
         :unit quantities:cell)))
      (camera-right :vec4
       :components
       ((:xyz :quantity quantities:world-direction :unit :one)))
      (camera-up :vec4
       :components
       ((:xyz :quantity quantities:world-direction :unit :one)))
      (camera-forward :vec4
       :components
       ((:xyz :quantity quantities:world-direction :unit :one)))
      ;; Perspective and isometric projection put unlike meanings in these
      ;; four lanes.  The projection operations, rather than the storage row,
      ;; own their interpretations.
      (camera-projection :vec4)
      ;; Projection selector Z is categorical.  The other lanes are stable
      ;; nonnegative proportions even where the current shader does not read
      ;; the bevel lane.
      (render-parameters :vec4
       :components
       ((:x :quantity quantities:bevel-proportion :unit :one)
        (:y :quantity quantities:construction-line-strength :unit :one)
        (:w :quantity quantities:inspection-ink-strength :unit :one)))
      (previous-camera-position :vec4
       :components
       ((:xyz :quantity quantities:world-position
         :unit quantities:cell)))
      (previous-camera-right :vec4
       :components
       ((:xyz :quantity quantities:world-direction :unit :one)))
      (previous-camera-up :vec4
       :components
       ((:xyz :quantity quantities:world-direction :unit :one)))
      (previous-camera-forward :vec4
       :components
       ((:xyz :quantity quantities:world-direction :unit :one)))
      (previous-camera-projection :vec4)
      (temporal-parameters :vec4
       :components
       ((:xy :quantity quantities:temporal-jitter :unit :one)
        (:zw :quantity quantities:temporal-jitter :unit :one)))
      (inspection-parameters :vec4
       :components
       ((:xy :quantity quantities:texture-coordinate :unit :one)
        (:zw :quantity quantities:texel-extent :unit :one)))
      (character-parameters :vec4
       :components
       ((:xyz :quantity quantities:world-position
         :unit quantities:cell)
        (:w :quantity quantities:gait-phase :unit :radian)))
      (sun-vector :vec4
       :components
       ((:xyz :quantity quantities:world-direction :unit :one)))
      (sun-color-vector :vec4
       :components
       ((:xyz :quantity quantities:scene-radiance :unit :one)))
      (sky-color-vector :vec4
       :components
       ((:xyz :quantity quantities:scene-radiance :unit :one)
        (:w :quantity quantities:exposure :unit :one)))
      (ground-color-vector :vec4
       :components
       ((:xyz :quantity quantities:scene-radiance :unit :one)))
      ;; Four dense rows represent the heterogeneous world-to-shadow map.
      (shadow-row-x :vec4)
      (shadow-row-y :vec4)
      (shadow-row-z :vec4)
      (shadow-row-w :vec4)
      (shadow-control :vec4
       :components
       ((:xy :quantity quantities:texel-extent :unit :one)
        (:z :quantity quantities:shadow-bias :unit :one)
        (:w :quantity quantities:shadow-filter-radius :unit :one)))
      (previous-character-parameters :vec4
       :components
       ((:xyz :quantity quantities:world-position
         :unit quantities:cell)
        (:w :quantity quantities:gait-phase :unit :radian)))
      ;; Current and previous two-dimensional headings share one row.
      (character-direction :vec4
       :components
       ((:xy :quantity quantities:horizontal-direction :unit :one)
        (:zw :quantity quantities:horizontal-direction :unit :one))))
    "The quantity-declared 100-float scene environment shared by all stages.")

  (defun scene-uniform-prefix (count)
    "The first COUNT members of the canonical scene uniform ledger."
    (subseq *scene-uniform-members* 0 count)))

(defun scene-swizzle-positions (swizzle)
  "Return the scalar positions selected by a four-lane SWIZZLE keyword."
  (flet ((position-for (character)
           (or (position character "xyzw")
               (position character "rgba")
               (error "Unsupported scene-uniform component ~S in ~S."
                      character swizzle))))
    (map 'list #'position-for (string-downcase (symbol-name swizzle)))))

(defun scene-uniform-product-layout ()
  "Build the host product layout from the shader-owned scene ledger."
  (let ((projections nil))
    (loop for member in *scene-uniform-members*
          for member-index from 0
          do (destructuring-bind (name type &rest options) member
               (declare (ignore name))
               (unless (eq type :vec4)
                 (error "Scene uniform member type ~S is not :VEC4." type))
               (dolist (component (getf options :components))
                 (destructuring-bind (swizzle &rest quantity-options) component
                   (let* ((local-positions
                            (scene-swizzle-positions swizzle))
                          (positions
                            (mapcar (lambda (position)
                                      (+ (* member-index 4) position))
                                    local-positions))
                          (specification
                            (math:make-declared-quantity-specification
                             (append quantity-options
                                     (list :tensor-order
                                           (if (= (length positions) 1)
                                               0 1))))))
                     (push
                      (math:make-quantity-projection
                       positions specification)
                      projections))))))
    (math:make-quantity-layout 100 (nreverse projections))))

(defmethod math:value-declaration-for
    ((name (eql 'luft.render::camera-uniform-data)))
  (declare (ignore name))
  (load-time-value
   (math:make-represented-value-declaration
    :representation-type '(simple-array single-float (100))
    :quantity-layout (scene-uniform-product-layout)
    :source-form
    '(luft.render::camera-uniform-data
      :type (simple-array single-float (100))
      :product *scene-uniform-members*))))

(defun shader-uniform-product-layout (block)
  "Flatten BLOCK's aligned Vec4 members into its scalar quantity product."
  (let ((bytes (shader-uniform-block-byte-size block))
        (projections nil))
    (unless (zerop (mod bytes 4))
      (error "Scene shader uniform size ~D is not a whole float lane count."
             bytes))
    (dolist (member (shader-uniform-block-members block))
      (let* ((byte-offset (shader-uniform-member-offset member))
             (width
               (shader-type-component-count
                (math:declaration-representation-type member)))
             (whole
               (math:declaration-quantity-specification member))
             (layout
               (math:declaration-quantity-layout member)))
        (unless (and width (zerop (mod byte-offset 4)))
          (error "Scene shader member ~S is not a 32-bit scalar-lane value."
                 (shader-object-name member)))
        (let ((base (/ byte-offset 4)))
          (when whole
            (push
             (math:make-quantity-projection
              (loop for position below width collect (+ base position))
              whole)
             projections))
          (when layout
            (unless (= width (math:quantity-layout-extent layout))
              (error "Scene shader member ~S has width ~D but layout ~D."
                     (shader-object-name member) width
                     (math:quantity-layout-extent layout)))
            (dolist (projection
                     (math:quantity-layout-projections layout))
              (push
               (math:make-quantity-projection
                (mapcar
                 (lambda (position) (+ base position))
                 (math:quantity-projection-positions projection))
                (math:quantity-projection-specification projection))
               projections))))))
    (math:make-quantity-layout
     (/ bytes 4) (nreverse projections))))

(defun scene-uniform-scalar-count ()
  "Return the scalar extent of the fixed host scene-uniform declaration."
  (math:quantity-layout-extent
   (math:declaration-quantity-layout
    (math:value-declaration-for
     'luft.render::camera-uniform-data))))

(defun scene-uniform-byte-size ()
  "Return the byte size of the fixed host scene-uniform declaration."
  (* 4 (scene-uniform-scalar-count)))
