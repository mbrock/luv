;;; A presentation-oriented browser for mathematical shaders and lowered SSA.

(in-package #:luv.mcclim)

(define-presentation-type shader-expression-presentation ())
(define-presentation-type shader-instruction-presentation ())
(define-presentation-type shader-basic-block-presentation ())
(define-presentation-type shader-specification-presentation ())
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
    (object (type block-kind-presentation))
  (typep object 'luv:block-kind))

(define-application-frame shader-lab ()
  ((lowering
    :initarg :lowering
    :initform (luv.spir-v:block-world-fragment-lowering)
    :accessor shader-lab-lowering)
   (specifications
    :initarg :specifications
    :initform (list (luv.spir-v:block-world-fragment-specification)
                    (luv.spir-v:block-world-crosshair-fragment-specification))
    :reader shader-lab-specifications)
   (materials
    :initarg :materials
    :initform (luv:placeable-block-kinds)
    :reader shader-lab-materials)
   (atlas
    :initform (luv:make-block-texture-atlas)
    :reader shader-lab-atlas)
   (process
    :initform nil
    :accessor shader-lab-process)
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
  (member operator '(:+ :- :* :/)))

(defun shader-operator-label (operator)
  (case operator
    (:+ "+") (:- "-") (:* "*") (:/ "/")
    (otherwise (string-downcase (symbol-name operator)))))

(defun shader-lab-current-specification (frame)
  (luv.spir-v:shader-lowering-specification
   (shader-lab-lowering frame)))

(defun shader-display-label (specification)
  (case (luv.spir-v:shader-object-name specification)
    (luv.spir-v:block-world-fragment-specification "BLOCK SURFACE")
    (luv.spir-v:block-world-crosshair-fragment-specification "CROSSHAIR INK")
    (otherwise
     (string-upcase
      (symbol-name (luv.spir-v:shader-object-name specification))))))

(defun block-material-preview-tile (block)
  (let ((tiles (luv:block-kind-face-tiles block)))
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

(defun draw-shader-tab (frame stream specification left top)
  (let ((selected-p (eq specification
                        (shader-lab-current-specification frame))))
    (with-output-as-presentation
        (stream specification 'shader-specification-presentation
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
      (draw-text* stream (shader-display-label specification)
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
                          (luv:block-kind-name block))
                  (+ left 8) (+ top 66)
                  :ink (if selected-p *shader-accent-ink* +black+)
                  :align-y :top))))

(defun display-shader-materials (frame stream)
  (draw-text* stream "LUVCRAFT MATERIAL WORKBENCH" 14 5
              :ink *shader-accent-ink* :align-y :top
              :text-style (make-text-style :sans-serif :bold :normal))
  (draw-text* stream "choose a live shader" 14 30
              :ink *shader-muted-ink* :align-y :top)
  (loop for specification in (shader-lab-specifications frame)
        for index from 0
        do (draw-shader-tab frame stream specification
                            (+ 205 (* index 198)) 24))
  (loop for block in (shader-lab-materials frame)
        for number from 1
        for left = (+ 14 (* (1- number) 144))
        do (draw-block-material-card frame stream block number left 62)))

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
  (format stream " : ~A  [location ~D]~%"
          (shader-type-label
           (luv.spir-v:shader-declaration-type declaration))
          (luv.spir-v:shader-interface-location declaration)))

(defun display-shader-resource (stream resource)
  (write-string "  resource " stream)
  (write-shader-name (luv.spir-v:shader-object-name resource) stream)
  (format stream " : ~A  [set ~D, binding ~D]~%"
          (shader-type-label (luv.spir-v:shader-declaration-type resource))
          (luv.spir-v:shader-resource-descriptor-set resource)
          (luv.spir-v:shader-resource-binding resource)))

(defun display-shader-source (frame stream)
  (let* ((lowering (shader-lab-lowering frame))
         (specification
           (luv.spir-v:shader-lowering-specification lowering)))
    (with-drawing-options (stream :text-size :large :text-face :bold)
      (with-drawing-options (stream :ink *shader-accent-ink*)
        (write-string (string-downcase (shader-display-label specification))
                      stream)))
    (format stream "  ~(~A~)~2%"
            (luv.spir-v:shader-specification-stage specification))
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
      (format stream " : ~A = "
              (shader-type-label
               (luv.spir-v:shader-expression-type
                (luv.spir-v:shader-binding-expression binding))))
      (write-shader-expression
       (luv.spir-v:shader-binding-expression binding) stream frame)
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
      ((typep selection 'luv:block-kind)
       (let* ((materials (shader-lab-materials frame))
              (number (position selection materials :test #'eq))
              (tile (block-material-preview-tile selection)))
         (format stream " — material ~A  ~(~A~)~%"
                 (if number (1+ number) "?")
                 (luv:block-kind-name selection))
         (format stream
                 "atlas tile ~D · number key ~A in luvcraft · right click places · middle click picks"
                 tile (if number (1+ number) "?"))))
      ((typep selection 'luv.spir-v:shader-expression)
       (format stream " — ~A expression  source "
               (shader-type-label
                (luv.spir-v:shader-expression-type selection)))
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

(define-shader-lab-command (com-select-shader-specification :name nil)
    ((specification 'shader-specification-presentation :gesture :select))
  (setf (shader-lab-lowering *application-frame*)
        (luv.spir-v:compile-shader-specification specification)
        (shader-lab-selection *application-frame*) nil)
  (redisplay-frame-panes *application-frame* :force-p t))

(define-shader-lab-command (com-select-block-kind :name nil)
    ((block 'block-kind-presentation :gesture :select))
  (setf (shader-lab-selection *application-frame*) block)
  (redisplay-frame-panes *application-frame* :force-p t))

(defun open-shader-lab
    (&key (specification
           (luv.spir-v:block-world-fragment-specification))
          (specifications
           (list specification
                 (luv.spir-v:block-world-crosshair-fragment-specification)))
          (server-path '(:luv))
          (title "Luvcraft material and shader workbench"))
  "Open a material, expression, and SSA workbench on luv's McCLIM backend."
  (check-type specification luv.spir-v:shader-specification)
  (let* ((port (find-port :server-path server-path))
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
                            :specifications specifications
                            :lowering
                            (luv.spir-v:compile-shader-specification
                             specification)))
                     (setf (frame-pretty-name frame) title)
                     (setf startup-signaled-p t)
                     (sb-thread:signal-semaphore startup-completion)
                     (unwind-protect
                          (run-frame-top-level frame)
                       (when (frame-manager frame)
                         (disown-frame manager frame))))
                 (error (condition)
                   (unless startup-signaled-p
                     (setf startup-error condition)
                     (sb-thread:signal-semaphore startup-completion))
                   (error condition)))))
      (let ((process
              (clim-sys:make-process
               #'run :name "Luvcraft material and shader workbench")))
        (sb-thread:wait-on-semaphore startup-completion)
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
