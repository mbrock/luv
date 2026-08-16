;;; A presentation-oriented browser for mathematical shaders and lowered SSA.

(in-package #:mcluv)

(define-presentation-type shader-expression-presentation ())
(define-presentation-type shader-instruction-presentation ())
(define-presentation-type shader-basic-block-presentation ())
(define-presentation-type shader-specification-presentation ())
(define-presentation-type shader-definition-presentation ())
(define-presentation-type block-kind-presentation ())

(defparameter *shader-accent-ink* (make-rgb-color 0.10 0.38 0.78))
(defparameter *shader-call-ink* (make-rgb-color 0.44 0.20 0.68))
(defparameter *shader-reference-ink* (make-rgb-color 0.02 0.43 0.47))
(defparameter *shader-literal-ink* (make-rgb-color 0.76 0.30 0.08))
(defparameter *shader-muted-ink* (make-rgb-color 0.36 0.39 0.43))
(defparameter *shader-panel-ink* (make-rgb-color 0.95 0.965 0.98))
(defparameter *shader-selection-ink* (make-rgb-color 0.84 0.91 1.0))

(define-presentation-method presentation-typep
    (object (type shader-expression-presentation))
  (typep object 'luv.spir-v:shader-expression))

(define-presentation-method presentation-typep
    (object (type shader-instruction-presentation))
  (typep object 'luv.spir-v:instruction))

(define-presentation-method presentation-typep
    (object (type shader-basic-block-presentation))
  (typep object 'luv.spir-v:spir-v-basic-block))

(define-presentation-method presentation-typep
    (object (type shader-specification-presentation))
  (typep object 'luv.spir-v:shader-specification))

(define-presentation-method presentation-typep
    (object (type shader-definition-presentation))
  (typep object 'shader-definition-entry))

(define-presentation-method presentation-typep
    (object (type block-kind-presentation))
  (typep object 'luvcraft:block-kind))

(defclass shader-definition-entry ()
  ((role :initarg :role :reader shader-definition-entry-role)
   (stage :initarg :stage :reader shader-definition-entry-stage)
   (label :initarg :label :reader shader-definition-entry-label)
   (dependent :initarg :dependent :initform nil
              :reader shader-definition-entry-dependent)
   (pipeline :initarg :pipeline :initform nil
             :reader shader-definition-entry-pipeline)
   (specification :initarg :specification :initform nil
                  :accessor shader-definition-entry-specification)
   (lowering :initform nil :accessor shader-definition-entry-lowering)
   (status :initform :uncompiled :accessor shader-definition-entry-status)
   (diagnostic :initform nil :accessor shader-definition-entry-diagnostic)))

(defclass shader-lab-health-report ()
  ((status :initarg :status :reader shader-lab-health-report-status)
   (frame-state :initarg :frame-state
                :reader shader-lab-health-report-frame-state)
   (process-alive-p :initarg :process-alive-p
                    :reader shader-lab-health-report-process-alive-p)
   (mirror-count :initarg :mirror-count :initform 0
                 :reader shader-lab-health-report-mirror-count)
   (canvas-state :initarg :canvas-state :initform nil
                 :reader shader-lab-health-report-canvas-state)
   (latency :initarg :latency :initform nil
            :reader shader-lab-health-report-latency)
   (problems :initarg :problems :initform nil
             :reader shader-lab-health-report-problems)
   (backtrace :initarg :backtrace :initform nil
              :reader shader-lab-health-report-backtrace))
  (:documentation
   "One bounded diagnosis of frame, process, mirror, canvas, and responsiveness."))

(defmethod print-object ((report shader-lab-health-report) stream)
  (print-unreadable-object (report stream :type t)
    (format stream "~S, ~D mirror~:P, canvas ~S~@[; ~{~A~^, ~}~]"
            (shader-lab-health-report-status report)
            (shader-lab-health-report-mirror-count report)
            (shader-lab-health-report-canvas-state report)
            (shader-lab-health-report-problems report))))

(climi::define-event-class shader-lab-health-event (climi::standard-event)
  ((sheet :initarg :sheet :reader clim:event-sheet)
   (function :initarg :function :reader shader-lab-health-event-function)))

(defmethod handle-event ((sheet sheet) (event shader-lab-health-event))
  (funcall (shader-lab-health-event-function event)))

(defun refresh-shader-definition-entry (entry &key force)
  "Compile ENTRY's current method without discarding its last good lowering."
  (let ((dependent (shader-definition-entry-dependent entry)))
    (when (or force
              (and dependent
                   (luv.spir-v:shader-definition-change-pending-p dependent)))
      (multiple-value-bind (revision event)
          (if dependent
              (luv.spir-v:shader-definition-change-snapshot dependent)
              (values 0 nil))
        (declare (ignore event))
        (setf (shader-definition-entry-status entry) :compiling)
        (handler-case
            (let* ((specification
                     (if dependent
                         (luv.spir-v:shader-specification-for
                          (shader-definition-entry-role entry)
                          (shader-definition-entry-stage entry))
                         (shader-definition-entry-specification entry)))
                   (lowering
                     (luv.spir-v:compile-shader-specification specification)))
              (setf (shader-definition-entry-specification entry) specification
                    (shader-definition-entry-lowering entry) lowering
                    (shader-definition-entry-status entry) :ready
                    (shader-definition-entry-diagnostic entry) nil))
          (error (condition)
            (setf (shader-definition-entry-status entry) :failed
                  (shader-definition-entry-diagnostic entry) condition)))
        (when dependent
          (luv.spir-v:acknowledge-shader-definition-change
           dependent revision)))))
  entry)

(defun make-live-shader-definition-entry (role stage label pipeline)
  (let* ((dependent
           (luv.spir-v:make-shader-definition-dependent
            (fdefinition 'luv.spir-v:shader-specification-for)
            (list role stage)))
         (entry
           (make-instance 'shader-definition-entry
                          :role role :stage stage :label label
                          :dependent dependent :pipeline pipeline)))
    (refresh-shader-definition-entry entry :force t)))

(defun make-static-shader-definition-entry (specification)
  (let ((entry
          (make-instance
           'shader-definition-entry
           :role (luv.spir-v:shader-object-name specification)
           :stage (luv.spir-v:shader-specification-stage specification)
           :label (string-upcase
                   (symbol-name
                    (luv.spir-v:shader-object-name specification)))
           :specification specification)))
    (refresh-shader-definition-entry entry :force t)))

(defun release-shader-definition-entry (entry)
  (let ((dependent (shader-definition-entry-dependent entry)))
    (when dependent
      (luv.spir-v:release-shader-definition-dependent dependent)))
  nil)

(define-application-frame shader-lab ()
  ((lowering
    :initarg :lowering
    :initform (luvcraft.shaders:block-world-fragment-lowering)
    :accessor shader-lab-lowering)
   (definitions
    :initarg :definitions
    :reader shader-lab-definitions)
   (current-definition
    :initarg :current-definition
    :accessor shader-lab-current-definition)
   (materials
    :initarg :materials
    :initform (luvcraft:placeable-block-kinds)
    :reader shader-lab-materials)
   (atlas
    :initform (luvcraft:make-block-texture-atlas)
    :reader shader-lab-atlas)
   (process
    :initform nil
    :accessor shader-lab-process)
   (health-report
    :initform nil
    :accessor shader-lab-last-health-report)
   (selection
    :initform nil
    :accessor shader-lab-selection))
  (:menu-bar nil)
  (:panes
   (materials :application
              :display-function 'display-shader-materials
              :scroll-bars nil
              :default-text-style (make-text-style :sans-serif nil :normal))
   (source :application
           :display-function 'display-shader-source
           :scroll-bars :vertical
           :default-text-style (make-text-style :fix nil :normal)
           :text-margins '(:left 12 :right 12 :top 10 :bottom 10))
   (ssa :application
        :display-function 'display-shader-ssa
        :scroll-bars :vertical
        :default-text-style (make-text-style :fix nil :normal)
        :text-margins '(:left 12 :right 12 :top 10 :bottom 10))
   (details :application
            :display-function 'display-shader-selection
            :scroll-bars nil
            :default-text-style (make-text-style :fix nil :normal)
            :text-margins '(:left 12 :right 12 :top 8 :bottom 8)))
  (:layouts
   (default
    (vertically (:width 1120 :height 820 :spacing 8)
      (1/5 materials)
      (3/5 (horizontally (:spacing 8)
             (1/2 source)
             (1/2 ssa)))
      (1/5 details)))))

(defun shader-lab-specifications (frame)
  "Return the current successfully compiled shader method results in FRAME."
  (mapcar #'shader-definition-entry-specification
          (shader-lab-definitions frame)))

(defun selected-shader-expression (frame)
  (let ((selection (shader-lab-selection frame)))
    (cond ((typep selection 'luv.spir-v:shader-expression) selection)
          ((typep selection 'luv.spir-v:instruction)
           (first
            (gethash
             selection
             (luv.spir-v:shader-lowering-instruction-expressions
              (shader-lab-lowering frame))))))))

(defun selected-shader-instructions (frame)
  (let ((selection (shader-lab-selection frame)))
    (cond ((typep selection 'luv.spir-v:shader-expression)
           (gethash
            selection
            (luv.spir-v:shader-lowering-expression-instructions
             (shader-lab-lowering frame))))
          ((typep selection 'luv.spir-v:instruction) (list selection)))))

(defun shader-type-label (type)
  (string-downcase (symbol-name (luv.spir-v:shader-type-name type))))

(defun write-shader-name (name stream)
  (write-string (string-downcase (symbol-name name)) stream))

(defun shader-infix-operator-p (operator)
  ;; Shader operators are ordinary symbols; arithmetic is CL's own.
  (member operator '(+ - * /)))

(defun shader-operator-label (operator)
  (string-downcase (symbol-name operator)))

(defun shader-lab-current-specification (frame)
  (luv.spir-v:shader-lowering-specification
   (shader-lab-lowering frame)))

(defun shader-display-label (specification)
  (case (luv.spir-v:shader-object-name specification)
    (luvcraft.shaders:block-world-fragment-specification "BLOCK SURFACE")
    (luvcraft.shaders:block-world-crosshair-fragment-specification "CROSSHAIR INK")
    (otherwise
     (string-upcase
      (symbol-name (luv.spir-v:shader-object-name specification))))))

(defun block-material-preview-tile (block)
  (let ((tiles (luvcraft:block-kind-face-tiles block)))
    (or (getf tiles :top) (getf tiles :all) (getf tiles :side))))

(defun packed-block-ink (word)
  (make-rgb-color (/ (ldb (byte 8 0) word) 255.0)
                  (/ (ldb (byte 8 8) word) 255.0)
                  (/ (ldb (byte 8 16) word) 255.0)))

(defun draw-block-atlas-tile (stream atlas tile left top &key (scale 3))
  (dotimes (y 16)
    (dotimes (x 16)
      (draw-rectangle*
       stream
       (+ left (* x scale)) (+ top (* y scale))
       (+ left (* (1+ x) scale)) (+ top (* (1+ y) scale))
       :ink (packed-block-ink (aref atlas y (+ x (* tile 16))))))))

(defun draw-shader-tab (frame stream definition left top)
  (let ((selected-p (eq definition
                        (shader-lab-current-definition frame))))
    (with-output-as-presentation
        (stream definition 'shader-definition-presentation
                :single-box t)
      (draw-rectangle* stream left top (+ left 188) (+ top 25)
                       :ink (if selected-p
                                *shader-selection-ink*
                                *shader-panel-ink*))
      (draw-rectangle* stream left top (+ left 188) (+ top 25)
                       :ink (if selected-p
                                *shader-accent-ink*
                                *shader-muted-ink*)
                       :filled nil :line-thickness (if selected-p 2 1))
      (draw-text* stream (shader-definition-entry-label definition)
                  (+ left 10) (+ top 6)
                  :ink (if selected-p *shader-accent-ink* +black+)
                  :align-y :top))))

(defun draw-block-material-card (frame stream block number left top)
  (let* ((selected-p (eq block (shader-lab-selection frame)))
         (tile (block-material-preview-tile block)))
    (with-output-as-presentation
        (stream block 'block-kind-presentation :single-box t)
      (draw-rectangle* stream left top (+ left 132) (+ top 88)
                       :ink (if selected-p
                                *shader-selection-ink*
                                *shader-panel-ink*))
      (draw-block-atlas-tile stream (shader-lab-atlas frame) tile
                             (+ left 8) (+ top 8))
      (draw-rectangle* stream (+ left 7) (+ top 7)
                       (+ left 57) (+ top 57)
                       :ink (if selected-p
                                *shader-accent-ink*
                                *shader-muted-ink*)
                       :filled nil :line-thickness (if selected-p 2 1))
      (draw-text* stream
                  (format nil "~D  ~(~A~)" number
                          (luvcraft:block-kind-name block))
                  (+ left 8) (+ top 66)
                  :ink (if selected-p *shader-accent-ink* +black+)
                  :align-y :top))))

(defun display-shader-materials (frame stream)
  (draw-text* stream "LUVCRAFT MATERIAL WORKBENCH" 14 5
              :ink *shader-accent-ink* :align-y :top
              :text-style (make-text-style :sans-serif :bold :normal))
  (draw-text* stream "choose a live shader" 14 30
              :ink *shader-muted-ink* :align-y :top)
  (loop for definition in (shader-lab-definitions frame)
        for index from 0
        do (draw-shader-tab frame stream definition
                            (+ 205 (* index 198)) 24))
  (loop for block in (shader-lab-materials frame)
        for number from 1
        for left = (+ 14 (* (1- number) 144))
        do (draw-block-material-card frame stream block number left 62)))

(defun quantity-factors-label (factors)
  (if factors
      (format nil "~{~A~^ ~}"
              (mapcar (lambda (factor)
                        (if (= 1 (cdr factor))
                            (string-downcase (symbol-name (car factor)))
                            (format nil "~(~A~)^~A"
                                    (car factor) (cdr factor))))
                      factors))
      "1"))

(defun quantity-specification-label (specification)
  (if specification
      (format nil "~@[~(~A~) · ~]~@[kind ~(~A~) · ~]~A [~A] · ~A · ~A"
              (luv.arithmetic:quantity-specification-name specification)
              (luv.arithmetic:quantity-specification-kind specification)
              (quantity-factors-label
               (luv.arithmetic:dimension-factors
                (luv.arithmetic:quantity-specification-dimension
                 specification)))
              (quantity-factors-label
               (luv.arithmetic:unit-expression-factors
                (luv.arithmetic:quantity-specification-unit specification)))
              (case
                  (luv.arithmetic:quantity-specification-tensor-order
                   specification)
                (0 "scalar")
                (1 "vector")
                (otherwise
                 (format nil "tensor order ~D"
                         (luv.arithmetic:quantity-specification-tensor-order
                          specification))))
              (if (luv.arithmetic:quantity-specification-affine-p
                   specification)
                  "point"
                  "difference"))
      "unannotated"))

(defun quantity-projection-lanes-label (positions)
  (coerce (mapcar (lambda (position)
                    (or (nth position '(#\x #\y #\z #\w)) #\?))
                  positions)
          'string))

(defun quantity-layout-label (layout)
  (when layout
    (format nil "packed {~{~A~^; ~}}"
            (mapcar
             (lambda (projection)
               (format nil "~A -> ~A"
                       (quantity-projection-lanes-label
                        (luv.arithmetic:quantity-projection-positions
                         projection))
                       (quantity-specification-label
                        (luv.arithmetic:quantity-projection-specification
                         projection))))
             (luv.arithmetic:quantity-layout-projections layout)))))

(defun quantity-semantics-label (specification layout)
  (or (quantity-layout-label layout)
      (and specification (quantity-specification-label specification))
      "unannotated"))

(defun write-shader-expression (expression stream frame)
  "Render EXPRESSION as nested selectable mathematical presentations."
  (labels ((write-body ()
             (etypecase expression
               (luv.spir-v:shader-literal
                (format stream "~G"
                        (luv.spir-v:shader-literal-value expression)))
               (luv.spir-v:shader-reference
                (write-shader-name
                 (luv.spir-v:shader-object-name
                  (luv.spir-v:shader-reference-target expression))
                 stream))
               (luv.spir-v:shader-map-application
                (write-string "project-point[" stream)
                (write-shader-name
                 (luv.spir-v:shader-object-name
                  (luv.spir-v:shader-map-application-definition expression))
                 stream)
                (write-string "](" stream)
                (write-shader-expression
                 (luv.spir-v:shader-map-application-point expression)
                 stream frame)
                (write-string "; rows " stream)
                (loop for row in
                        (luv.spir-v:shader-map-application-rows expression)
                      for first-p = t then nil
                      unless first-p do (write-string ", " stream)
                      do (write-shader-expression row stream frame))
                (write-char #\) stream))
               (luv.spir-v:shader-map-projection
                (write-string "project-sample(" stream)
                (write-shader-expression
                 (luv.spir-v:shader-map-projection-application expression)
                 stream frame)
                (write-char #\) stream))
               (luv.spir-v:shader-interpretation
                (write-string "interpret(" stream)
                (write-shader-expression
                 (luv.spir-v:shader-interpretation-operand expression)
                 stream frame)
                (format stream ", ~A)"
                        (quantity-specification-label
                         (luv.spir-v:shader-expression-quantity-specification
                          expression))))
               (luv.spir-v:shader-quantity-construction
                (write-string "quantity(" stream)
                (write-shader-expression
                 (luv.spir-v:shader-quantity-construction-operand expression)
                 stream frame)
                (format stream ", ~A)"
                        (quantity-specification-label
                         (luv.spir-v:shader-expression-quantity-specification
                          expression))))
               (luv.spir-v:shader-quantity-assumption
                (write-string "assume(" stream)
                (write-shader-expression
                 (luv.spir-v:shader-quantity-assumption-operand expression)
                 stream frame)
                (format stream ", ~A)"
                        (quantity-specification-label
                         (luv.spir-v:shader-expression-quantity-specification
                          expression))))
               (luv.spir-v:shader-representation
                (write-string "representation(" stream)
                (write-shader-expression
                 (luv.spir-v:shader-representation-operand expression)
                 stream frame)
                (write-char #\) stream))
               (luv.spir-v:shader-unit-conversion
                (write-string "convert-unit(" stream)
                (write-shader-expression
                 (luv.spir-v:shader-unit-conversion-operand expression)
                 stream frame)
                (format stream ", ~A, scale ~A)"
                        (quantity-specification-label
                         (luv.spir-v:shader-expression-quantity-specification
                          expression))
                        (luv.spir-v:shader-unit-conversion-factor expression)))
               (luv.spir-v:shader-call
                (let ((operator (luv.spir-v:shader-call-operator expression))
                      (operands (luv.spir-v:shader-call-operands expression)))
                  (cond
                    ((eq operator :swizzle)
                     (write-shader-expression (first operands) stream frame)
                     (format stream ".~(~A~)"
                             (first
                              (luv.spir-v:shader-call-parameters expression))))
                    ((shader-infix-operator-p operator)
                     (write-char #\( stream)
                     (loop for operand in operands
                           for first-p = t then nil
                           unless first-p
                             do (format stream " ~A "
                                        (shader-operator-label operator))
                           do (write-shader-expression operand stream frame))
                     (write-char #\) stream))
                    (t
                     (write-string (shader-operator-label operator) stream)
                     (write-char #\( stream)
                     (loop for operand in operands
                           for first-p = t then nil
                           unless first-p do (write-string ", " stream)
                           do (write-shader-expression operand stream frame))
                     (write-char #\) stream))))))))
    (with-output-as-presentation
        (stream expression 'shader-expression-presentation :single-box t)
      (if (eq expression (selected-shader-expression frame))
          (with-drawing-options
              (stream :ink *shader-accent-ink* :text-face :bold)
            (write-body))
          (with-drawing-options
              (stream :ink
                      (etypecase expression
                        (luv.spir-v:shader-literal *shader-literal-ink*)
                        (luv.spir-v:shader-reference
                         *shader-reference-ink*)
                        (luv.spir-v:shader-map-application
                         *shader-call-ink*)
                        (luv.spir-v:shader-map-projection
                         *shader-call-ink*)
                        (luv.spir-v:shader-interpretation
                         *shader-accent-ink*)
                        (luv.spir-v:shader-quantity-construction
                         *shader-accent-ink*)
                        (luv.spir-v:shader-quantity-assumption
                         *shader-literal-ink*)
                        (luv.spir-v:shader-representation
                         *shader-accent-ink*)
                        (luv.spir-v:shader-unit-conversion
                         *shader-accent-ink*)
                        (luv.spir-v:shader-call *shader-call-ink*)))
            (write-body))))))

(defun display-shader-interface (stream declaration)
  (write-string "  " stream)
  (with-drawing-options
      (stream :ink (if (eq :input
                           (luv.spir-v:shader-interface-direction declaration))
                       *shader-reference-ink*
                       *shader-call-ink*)
              :text-face :bold)
    (format stream "~(~A~)"
            (luv.spir-v:shader-interface-direction declaration)))
  (write-char #\Space stream)
  (write-shader-name (luv.spir-v:shader-object-name declaration) stream)
  (format stream " : ~A  [~A]  ·  meaning ~A~%"
          (shader-type-label
           (luv.spir-v:shader-declaration-type declaration))
          (if (luv.spir-v:shader-interface-built-in declaration)
              (format nil "built-in ~(~A~)"
                      (luv.spir-v:shader-interface-built-in declaration))
              (format nil "location ~D"
                      (luv.spir-v:shader-interface-location declaration)))
          (quantity-semantics-label
           (luv.spir-v:shader-declaration-quantity-specification declaration)
           (luv.spir-v:shader-declaration-quantity-layout declaration))))

(defun display-shader-resource (stream resource)
  (write-string "  resource " stream)
  (write-shader-name (luv.spir-v:shader-object-name resource) stream)
  (format stream " : ~A  [set ~D, binding ~D]~%"
          (shader-type-label (luv.spir-v:shader-declaration-type resource))
          (luv.spir-v:shader-resource-descriptor-set resource)
          (luv.spir-v:shader-resource-binding resource))
  (when (or (luv.spir-v:shader-resource-sample-quantity-specification resource)
            (luv.spir-v:shader-resource-sample-quantity-layout resource))
    (format stream "    sample meaning ~A~%"
            (quantity-semantics-label
             (luv.spir-v:shader-resource-sample-quantity-specification
              resource)
             (luv.spir-v:shader-resource-sample-quantity-layout resource))))
  (when (typep resource 'luv.spir-v:shader-uniform-block)
    (dolist (member (luv.spir-v:shader-uniform-block-members resource))
      (format stream "    ~2D  "
              (luv.spir-v:shader-uniform-member-offset member))
      (write-shader-name (luv.spir-v:shader-object-name member) stream)
      (format stream " : ~A  ·  meaning ~A~%"
              (shader-type-label
               (luv.spir-v:shader-declaration-type member))
              (quantity-semantics-label
               (luv.spir-v:shader-declaration-quantity-specification member)
               (luv.spir-v:shader-declaration-quantity-layout member))))))

(defun display-shader-source (frame stream)
  (let* ((lowering (shader-lab-lowering frame))
         (specification
           (luv.spir-v:shader-lowering-specification lowering))
         (definition (shader-lab-current-definition frame))
         (dependent (shader-definition-entry-dependent definition))
         (pipeline (shader-definition-entry-pipeline definition)))
    (with-drawing-options (stream :text-size :large :text-face :bold)
      (with-drawing-options (stream :ink *shader-accent-ink*)
        (write-string (string-downcase (shader-display-label specification))
                      stream)))
    (format stream "  ~(~A~)~2%"
            (luv.spir-v:shader-specification-stage specification))
    (format stream "definition ~(~A~)/~(~A~)  ·  source ~(~A~)"
            (shader-definition-entry-role definition)
            (shader-definition-entry-stage definition)
            (shader-definition-entry-status definition))
    (when (and dependent
               (luv.spir-v:shader-definition-change-pending-p dependent))
      (write-string "  ·  changed" stream))
    (terpri stream)
    (when pipeline
      (format stream "GPU ~(~A~)  ·  installed revision ~D~%"
              (luvcraft:live-shader-pipeline-status pipeline)
              (luvcraft:live-shader-pipeline-installed-revision pipeline)))
    (let ((diagnostic
            (or (shader-definition-entry-diagnostic definition)
                (and pipeline
                     (luvcraft:live-shader-pipeline-diagnostic pipeline)))))
      (when diagnostic
        (with-drawing-options (stream :ink *shader-literal-ink*)
          (format stream "diagnostic: ~A~%" diagnostic))))
    (terpri stream)
    (with-drawing-options (stream :text-face :bold)
      (write-string "interface" stream))
    (terpri stream)
    (dolist (declaration
             (append (luv.spir-v:shader-specification-inputs specification)
                     (luv.spir-v:shader-specification-outputs specification)))
      (display-shader-interface stream declaration))
    (dolist (resource
             (luv.spir-v:shader-specification-resources specification))
      (display-shader-resource stream resource))
    (terpri stream)
    (with-drawing-options (stream :text-face :bold)
      (write-string "mathematical bindings" stream))
    (terpri stream)
    (dolist (binding
             (luv.spir-v:shader-specification-bindings specification))
      (write-string "  " stream)
      (with-drawing-options (stream :text-face :bold)
        (write-shader-name (luv.spir-v:shader-object-name binding) stream))
      (let ((expression (luv.spir-v:shader-binding-expression binding)))
        (format stream " : ~A  ·  meaning ~A = "
                (shader-type-label
                 (luv.spir-v:shader-expression-type expression))
                (quantity-semantics-label
                 (luv.spir-v:shader-expression-quantity-specification
                  expression)
                 (luv.spir-v:shader-expression-quantity-layout expression)))
        (write-shader-expression expression stream frame))
      (terpri stream))
    (terpri stream)
    (dolist (statement
             (luv.spir-v:shader-specification-statements specification))
      (write-string "  output " stream)
      (with-drawing-options (stream :text-face :bold)
        (write-shader-name
         (luv.spir-v:shader-object-name
          (luv.spir-v:shader-assignment-output statement))
         stream))
      (write-string " = " stream)
      (write-shader-expression
       (luv.spir-v:shader-assignment-value statement) stream frame)
      (terpri stream))))

(defun write-spir-v-form (form stream)
  (let ((*package* (find-package '#:luv.spir-v))
        (*print-pretty* nil)
        (*print-case* :downcase))
    (prin1 form stream)))

(defun display-shader-instruction (frame stream instruction)
  (with-output-as-presentation
      (stream instruction 'shader-instruction-presentation :single-box t)
    (if (member instruction (selected-shader-instructions frame) :test #'eq)
        (with-drawing-options
            (stream :ink *shader-accent-ink* :text-face :bold)
          (write-spir-v-form
           (luv.spir-v:instruction-form instruction) stream))
        (write-spir-v-form
         (luv.spir-v:instruction-form instruction) stream))))

(defun display-shader-ssa (frame stream)
  (with-drawing-options
      (stream :text-size :large :text-face :bold :ink *shader-accent-ink*)
    (write-string "lowered SSA" stream))
  (format stream "~%CLOS functions and basic blocks~2%")
  (let ((module
          (luv.spir-v:shader-lowering-module
           (shader-lab-lowering frame))))
    (let ((constants
            (remove-if-not
             (lambda (declaration)
               (and (typep declaration 'luv.spir-v:instruction)
                    (string-equal
                     (symbol-name
                      (luv.spir-v:instruction-name declaration))
                     "constant")))
             (luv.spir-v:spir-v-module-global-declarations module))))
      (when constants
        (write-string "module constants" stream)
        (terpri stream)
        (dolist (constant constants)
          (write-string "  " stream)
          (display-shader-instruction frame stream constant)
          (terpri stream))
        (terpri stream)))
    (dolist (function
             (luv.spir-v:spir-v-module-function-definitions module))
      (format stream "function ~A~%"
              (luv.spir-v:spir-v-function-result-id function))
      (dolist (block (luv.spir-v:spir-v-function-basic-blocks function))
        (with-output-as-presentation
            (stream block 'shader-basic-block-presentation :single-box t)
          (if (eq block (shader-lab-selection frame))
              (with-drawing-options
                  (stream :ink *shader-accent-ink* :text-face :bold)
                (format stream "  block ~A"
                        (luv.spir-v:spir-v-basic-block-label block)))
              (format stream "  block ~A"
                      (luv.spir-v:spir-v-basic-block-label block))))
        (terpri stream)
        (dolist (instruction
                 (luv.spir-v:spir-v-basic-block-instructions block))
          (write-string "    " stream)
          (display-shader-instruction frame stream instruction)
          (terpri stream))))))

(defun display-shader-selection (frame stream)
  (let* ((selection (shader-lab-selection frame))
         (lowering (shader-lab-lowering frame)))
    (with-drawing-options (stream :text-face :bold)
      (with-drawing-options (stream :ink *shader-accent-ink*)
        (write-string "selection" stream)))
    (cond
      ((null selection)
       (write-string
        " — click a material, mathematical expression, SSA instruction, or block"
        stream))
      ((typep selection 'luvcraft:block-kind)
       (let* ((materials (shader-lab-materials frame))
              (number (position selection materials :test #'eq))
              (tile (block-material-preview-tile selection)))
         (format stream " — material ~A  ~(~A~)~%"
                 (if number (1+ number) "?")
                 (luvcraft:block-kind-name selection))
         (format stream
                 "atlas tile ~D · number key ~A in luvcraft · right click places · middle click picks"
                 tile (if number (1+ number) "?"))))
      ((typep selection 'luv.spir-v:shader-expression)
       (format stream " — representation ~A  ·  meaning ~A  ·  source "
               (shader-type-label
                (luv.spir-v:shader-expression-type selection))
               (quantity-semantics-label
                (luv.spir-v:shader-expression-quantity-specification
                 selection)
                (luv.spir-v:shader-expression-quantity-layout selection)))
       (write-spir-v-form
        (luv.spir-v:shader-expression-source-form selection) stream)
       (let ((instructions
               (gethash
                selection
                (luv.spir-v:shader-lowering-expression-instructions lowering))))
         (format stream "~%~D associated SSA instruction~:P: "
                 (length instructions))
         (loop for instruction in instructions
               for first-p = t then nil
               unless first-p do (write-string "  ·  " stream)
               do (write-spir-v-form
                   (luv.spir-v:instruction-form instruction) stream))))
      ((typep selection 'luv.spir-v:instruction)
       (write-string " — SSA instruction  " stream)
       (write-spir-v-form (luv.spir-v:instruction-form selection) stream)
       (format stream "~%~D associated source expression~:P"
               (length
                (gethash
                 selection
                 (luv.spir-v:shader-lowering-instruction-expressions
                  lowering)))))
      ((typep selection 'luv.spir-v:spir-v-basic-block)
       (format stream " — basic block ~A, ~D instruction~:P"
               (luv.spir-v:spir-v-basic-block-label selection)
               (length
                (luv.spir-v:spir-v-basic-block-instructions selection)))))))

(define-shader-lab-command (com-select-shader-expression :name nil)
    ((expression 'shader-expression-presentation :gesture :select))
  (setf (shader-lab-selection *application-frame*) expression)
  (redisplay-frame-panes *application-frame* :force-p t))

(define-shader-lab-command (com-select-shader-instruction :name nil)
    ((instruction 'shader-instruction-presentation :gesture :select))
  ;; Keep the machine occurrence selected.  SELECTED-SHADER-EXPRESSION follows
  ;; reverse provenance so the source view still lights up at the same time.
  (setf (shader-lab-selection *application-frame*) instruction)
  (redisplay-frame-panes *application-frame* :force-p t))

(define-shader-lab-command (com-select-shader-basic-block :name nil)
    ((block 'shader-basic-block-presentation :gesture :select))
  (setf (shader-lab-selection *application-frame*) block)
  (redisplay-frame-panes *application-frame* :force-p t))

(define-shader-lab-command (com-select-shader-definition :name nil)
    ((definition 'shader-definition-presentation :gesture :select))
  ;; Recompile on the frame's command thread.  A broken edit remains visible as
  ;; diagnostic state while ENTRY retains its previous successful lowering.
  (refresh-shader-definition-entry definition :force t)
  (setf (shader-lab-current-definition *application-frame*) definition
        (shader-lab-lowering *application-frame*)
        (shader-definition-entry-lowering definition)
        (shader-lab-selection *application-frame*) nil)
  (redisplay-frame-panes *application-frame* :force-p t))

(define-shader-lab-command (com-select-block-kind :name nil)
    ((block 'block-kind-presentation :gesture :select))
  (setf (shader-lab-selection *application-frame*) block)
  (redisplay-frame-panes *application-frame* :force-p t))

(defun pipeline-for-shader-definition (role stage pipelines)
  (find-if (lambda (pipeline)
             (ecase stage
               (:vertex
                (eq role (luvcraft:live-shader-pipeline-vertex-role pipeline)))
               (:fragment
                (and (eq role (luvcraft:live-shader-pipeline-role pipeline))
                     (eq stage (luvcraft:live-shader-pipeline-stage pipeline))))))
           pipelines))

(defun make-default-shader-definitions (pipelines)
  (loop for (role stage label) in
        '((:block-surface :vertex "BLOCK GEOMETRY")
          (:block-surface :fragment "BLOCK SURFACE")
          (:block-crosshair :fragment "CROSSHAIR INK"))
        collect
        (make-live-shader-definition-entry
         role stage label
         (pipeline-for-shader-definition role stage pipelines))))

(defun refresh-shader-lab-now (frame)
  (dolist (definition (shader-lab-definitions frame))
    (refresh-shader-definition-entry definition :force t))
  (let ((current (shader-lab-current-definition frame)))
    (when (shader-definition-entry-lowering current)
      (setf (shader-lab-lowering frame)
            (shader-definition-entry-lowering current))))
  (redisplay-frame-panes frame :force-p t)
  frame)

(defun call-in-shader-lab-process (frame function timeout)
  "Queue FUNCTION through FRAME's ordinary event loop, with a bounded wait."
  (let ((process (shader-lab-process frame)))
    (if (null process)
        (handler-case (values t (funcall function) nil)
          (error (condition) (values t nil condition)))
        (let ((completion (sb-thread:make-semaphore :count 0))
              (result nil)
              (condition nil))
          (handler-case
              (let ((sheet (frame-top-level-sheet frame)))
                (climi::queue-append
                 (climi::frame-event-queue frame)
                 (make-instance
                  'shader-lab-health-event
                  :sheet sheet
                  :function
                  (lambda ()
                    (unwind-protect
                         (handler-case (setf result (funcall function))
                           (error (caught) (setf condition caught)))
                      (sb-thread:signal-semaphore completion))))))
            (error (caught)
              (setf condition caught)
              (sb-thread:signal-semaphore completion)))
          (if (sb-thread:wait-on-semaphore completion :timeout timeout)
              (values t result condition)
              (values nil nil nil))))))

(defun shader-lab-window-snapshot (frame)
  "Inspect FRAME's actual top-level mirror and native canvas on its process."
  (let* ((sheet (frame-top-level-sheet frame))
         (port (and sheet (port sheet)))
         (mirrors (and (typep port 'luv-port) (port-mirrors port)))
         (direct-mirror (and sheet (sheet-direct-mirror sheet)))
         (canvas (and (typep direct-mirror 'luv-mirror)
                      (mirror-target direct-mirror)))
         (problems nil))
    (unless sheet (push "no top-level sheet" problems))
    (unless (typep port 'luv-port) (push "frame is not on a luv port" problems))
    (unless (= (length mirrors) 1)
      (push (format nil "expected one mirror, found ~D" (length mirrors))
            problems))
    (unless (and direct-mirror (member direct-mirror mirrors :test #'eq))
      (push "direct mirror is not registered on the port" problems))
    (unless (typep direct-mirror 'luv-mirror)
      (push "top-level sheet has no luv mirror" problems))
    (unless canvas (push "mirror has no native canvas" problems))
    (when (and canvas (not (eq :open (luv:canvas-state canvas))))
      (push (format nil "canvas is ~S" (luv:canvas-state canvas)) problems))
    (when (and canvas (not (eq (luv:canvas-event-handler canvas)
                               direct-mirror)))
      (push "canvas event handler is not its mirror" problems))
    (list :mirror-count (length mirrors)
          :canvas-state (and canvas (luv:canvas-state canvas))
          :problems (nreverse problems))))

(defun capture-shader-lab-process-backtrace (process &key (timeout 0.25))
  "Best-effort bounded backtrace capture for an unresponsive frame process."
  (when (and process (sb-thread:thread-alive-p process))
    (let ((completion (sb-thread:make-semaphore :count 0))
          (backtrace nil))
      (handler-case
          (sb-thread:interrupt-thread
           process
           (lambda ()
             (unwind-protect
                  (setf backtrace (sb-debug:list-backtrace :count 30))
               (sb-thread:signal-semaphore completion))))
        (error () (return-from capture-shader-lab-process-backtrace nil)))
      (when (sb-thread:wait-on-semaphore completion :timeout timeout)
        backtrace))))

(defun shader-lab-health (frame &key (timeout 0.5))
  "Return a status and structured frame/process/mirror/canvas health report."
  (check-type frame shader-lab)
  (let* ((state (frame-state frame))
         (process (shader-lab-process frame))
         (alive-p (or (null process) (sb-thread:thread-alive-p process)))
         (start (get-internal-real-time))
         (units (coerce internal-time-units-per-second 'double-float))
         (report
           (if (eq :disowned state)
               (make-instance 'shader-lab-health-report
                              :status :closed :frame-state state
                              :process-alive-p alive-p)
               (multiple-value-bind (completed-p snapshot condition)
                   (call-in-shader-lab-process
                    frame (lambda () (shader-lab-window-snapshot frame)) timeout)
                 (let* ((latency (/ (- (get-internal-real-time) start) units))
                        (problems (and snapshot (getf snapshot :problems)))
                        (status (cond (condition :unresponsive)
                                      ((not completed-p) :unresponsive)
                                      (problems :degraded)
                                      (t :responsive))))
                   (make-instance
                    'shader-lab-health-report
                    :status status :frame-state state
                    :process-alive-p alive-p
                    :mirror-count (or (and snapshot
                                           (getf snapshot :mirror-count)) 0)
                    :canvas-state (and snapshot (getf snapshot :canvas-state))
                    :latency latency
                    :problems (append problems
                                      (when condition
                                        (list (princ-to-string condition))))
                    :backtrace
                    (when (eq status :unresponsive)
                      (capture-shader-lab-process-backtrace process))))))))
    (setf (shader-lab-last-health-report frame) report)
    (values (shader-lab-health-report-status report) report)))

(defun refresh-shader-lab (frame &key (timeout 1.0))
  "Refresh definitions on FRAME's process, returning FRAME and a health state."
  (check-type frame shader-lab)
  (if (eq :disowned (frame-state frame))
      (values frame :closed)
      (multiple-value-bind (completed-p result condition)
          (call-in-shader-lab-process
           frame (lambda () (refresh-shader-lab-now frame)) timeout)
        (declare (ignore result))
        (cond (condition (values frame :unresponsive condition))
              (completed-p (values frame :refreshed nil))
              (t (values frame :unresponsive nil))))))

(defun open-shader-lab
    (&key specification specifications pipelines
          (server-path '(:luv))
          (startup-timeout 10.0)
          (title "Luvcraft material and shader workbench"))
  "Open a live-definition, expression, and SSA workbench on luv's backend.

PIPELINES may contain a running demo's live shader artifacts; their installed
revision and last diagnostic then appear alongside the source definition."
  (when specification
    (check-type specification luv.spir-v:shader-specification))
  (let* ((definitions
           (cond
             (specifications
              (mapcar #'make-static-shader-definition-entry specifications))
             (specification
              (mapcar #'make-static-shader-definition-entry
                      (list specification
                            (luvcraft.shaders:block-world-crosshair-fragment-specification))))
             (t (make-default-shader-definitions pipelines))))
         (current-definition (first definitions))
         (port (find-port :server-path server-path))
         (manager (or (first (climi::frame-managers port))
                      (make-instance 'luv-frame-manager :port port)))
         (frame nil)
         (startup-error nil)
         (startup-signaled-p nil)
         (startup-completion (sb-thread:make-semaphore :count 0)))
    (labels ((run ()
               (handler-case
                   (progn
                     ;; Presentation gestures are read by the application
                     ;; frame's command loop, not SDL's native event thread.
                     ;; Construct and run the frame in one durable process.
                     (setf frame
                           (make-application-frame
                            'shader-lab
                            :frame-manager manager
                            :definitions definitions
                            :current-definition current-definition
                            :lowering
                            (shader-definition-entry-lowering
                             current-definition)))
                     (setf (frame-pretty-name frame) title)
                     (setf startup-signaled-p t)
                     (sb-thread:signal-semaphore startup-completion)
                     (unwind-protect (run-frame-top-level frame)
                       (dolist (definition definitions)
                         (release-shader-definition-entry definition))
                       (when (frame-manager frame)
                         (disown-frame manager frame))))
                 (error (condition)
                   (dolist (definition definitions)
                     (release-shader-definition-entry definition))
                   (unless startup-signaled-p
                     (setf startup-error condition)
                     (sb-thread:signal-semaphore startup-completion))
                   (error condition)))))
      (let ((process
              (clim-sys:make-process
               #'run :name "Luvcraft material and shader workbench")))
        (unless (sb-thread:wait-on-semaphore
                 startup-completion :timeout startup-timeout)
          (ignore-errors (clim-sys:destroy-process process))
          (dolist (definition definitions)
            (release-shader-definition-entry definition))
          (error "Shader lab did not create its frame within ~,2F seconds."
                 startup-timeout))
        (when startup-error
          (error startup-error))
        (setf (shader-lab-process frame) process)
        frame))))

(defun close-shader-lab (frame)
  "Destroy an OPEN-SHADER-LAB frame and its luv canvas."
  (check-type frame shader-lab)
  (unless (eq :disowned (frame-state frame))
    (let ((process (shader-lab-process frame)))
      (if process
          (clim-sys:process-interrupt
           process (lambda () (frame-exit frame)))
          (destroy-frame frame))))
  nil)
