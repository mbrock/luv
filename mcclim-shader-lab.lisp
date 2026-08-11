;;; A presentation-oriented browser for mathematical shaders and lowered SSA.

(in-package #:luv.mcclim)

(define-presentation-type shader-expression-presentation ())
(define-presentation-type shader-instruction-presentation ())
(define-presentation-type shader-basic-block-presentation ())

(define-presentation-method presentation-typep
    (object (type shader-expression-presentation))
  (typep object 'luv.spir-v:shader-expression))

(define-presentation-method presentation-typep
    (object (type shader-instruction-presentation))
  (typep object 'luv.spir-v:instruction))

(define-presentation-method presentation-typep
    (object (type shader-basic-block-presentation))
  (typep object 'luv.spir-v:spir-v-basic-block))

(define-application-frame shader-lab ()
  ((lowering
    :initarg :lowering
    :initform (luv.spir-v:block-world-fragment-lowering)
    :reader shader-lab-lowering)
   (selection
    :initform nil
    :accessor shader-lab-selection))
  (:menu-bar nil)
  (:panes
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
    (vertically (:width 1120 :height 720 :spacing 8)
      (4/5 (horizontally (:spacing 8)
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
          (with-drawing-options (stream :ink +red+ :text-face :bold)
            (write-body))
          (write-body)))))

(defun display-shader-interface (stream declaration)
  (format stream "  ~(~A~) "
          (luv.spir-v:shader-interface-direction declaration))
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
      (write-shader-name (luv.spir-v:shader-object-name specification) stream))
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
        (with-drawing-options (stream :ink +red+ :text-face :bold)
          (write-spir-v-form
           (luv.spir-v:instruction-form instruction) stream))
        (write-spir-v-form
         (luv.spir-v:instruction-form instruction) stream))))

(defun display-shader-ssa (frame stream)
  (with-drawing-options (stream :text-size :large :text-face :bold)
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
              (with-drawing-options (stream :ink +red+ :text-face :bold)
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
      (write-string "selection" stream))
    (cond
      ((null selection)
       (write-string " — click a mathematical expression or SSA instruction"
                     stream))
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
         (format stream "~%~D associated SSA instruction~:P"
                 (length instructions))))
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
  (let ((expressions
          (gethash
           instruction
           (luv.spir-v:shader-lowering-instruction-expressions
            (shader-lab-lowering *application-frame*)))))
    (setf (shader-lab-selection *application-frame*)
          (or (first expressions) instruction)))
  (redisplay-frame-panes *application-frame* :force-p t))

(define-shader-lab-command (com-select-shader-basic-block :name nil)
    ((block 'shader-basic-block-presentation :gesture :select))
  (setf (shader-lab-selection *application-frame*) block)
  (redisplay-frame-panes *application-frame* :force-p t))

(defun open-shader-lab
    (&key (specification
           (luv.spir-v:block-world-fragment-specification))
          (server-path '(:luv))
          (title "Luv mathematical shader browser"))
  "Open a selectable expression-to-SSA browser on luv's McCLIM backend."
  (check-type specification luv.spir-v:shader-specification)
  (let* ((port (find-port :server-path server-path))
         (manager (or (first (climi::frame-managers port))
                      (make-instance 'standard-frame-manager :port port)))
         (frame
           (make-application-frame
            'shader-lab
            :frame-manager manager
            :lowering (luv.spir-v:compile-shader-specification specification)
            :enable t)))
    (setf (frame-pretty-name frame) title)
    frame))

(defun close-shader-lab (frame)
  "Destroy an OPEN-SHADER-LAB frame and its luv canvas."
  (check-type frame shader-lab)
  (unless (eq :disowned (frame-state frame))
    (destroy-frame frame))
  nil)
