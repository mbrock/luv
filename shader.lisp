;;; Structured shader IR above the deliberately literal SPIR-V assembler.
;;;
;;; SPIR-V.LISP is the small instruction vocabulary and binary encoder.  This
;;; file gives shaders enough shape to be pleasant live Lisp objects: modules
;;; contain functions, functions contain basic blocks, and lowering erases
;;; that structure into the linear instruction stream expected by ASSEMBLE.

(in-package #:luv.spir-v)

(defclass spir-v-structure () ())

(defclass spir-v-module (spir-v-structure)
  ((version
    :initarg :version
    :initform #x00010000
    :accessor spir-v-module-version)
   (generator
    :initarg :generator
    :initform 0
    :accessor spir-v-module-generator)
   (capabilities
    :initarg :capabilities
    :initform '(shader)
    :accessor spir-v-module-capabilities)
   (addressing-model
    :initarg :addressing-model
    :initform 'logical
    :accessor spir-v-module-addressing-model)
   (memory-model
    :initarg :memory-model
    :initform 'glsl-450
    :accessor spir-v-module-memory-model)
   (entry-points
    :initarg :entry-points
    :initform nil
    :accessor spir-v-module-entry-points)
   (execution-modes
    :initarg :execution-modes
    :initform nil
    :accessor spir-v-module-execution-modes)
   (debug-instructions
    :initarg :debug-instructions
    :initform nil
    :accessor spir-v-module-debug-instructions)
   (annotations
    :initarg :annotations
    :initform nil
    :accessor spir-v-module-annotations)
   (global-declarations
    :initarg :global-declarations
    :initform nil
    :accessor spir-v-module-global-declarations)
   (function-definitions
    :initarg :function-definitions
    :initform nil
    :accessor spir-v-module-function-definitions)))

(defclass spir-v-entry-point (spir-v-structure)
  ((execution-model
    :initarg :execution-model
    :initform 'gl-compute
    :accessor spir-v-entry-point-execution-model)
   (function
    :initarg :function
    :accessor spir-v-entry-point-function)
   (name
    :initarg :name
    :initform "main"
    :accessor spir-v-entry-point-name)
   (interfaces
    :initarg :interfaces
    :initform nil
    :accessor spir-v-entry-point-interfaces)))

(defclass spir-v-execution-mode (spir-v-structure)
  ((function
    :initarg :function
    :accessor spir-v-execution-mode-function)
   (name
    :initarg :name
    :accessor spir-v-execution-mode-name)
   (literals
    :initarg :literals
    :initform nil
    :accessor spir-v-execution-mode-literals)))

(defclass spir-v-function-definition (spir-v-structure)
  ((result-id
    :initarg :result-id
    :accessor spir-v-function-result-id)
   (return-type
    :initarg :return-type
    :accessor spir-v-function-return-type)
   (control
    :initarg :control
    :initform 'none
    :accessor spir-v-function-control)
   (function-type
    :initarg :function-type
    :accessor spir-v-function-type)
   (parameters
    :initarg :parameters
    :initform nil
    :accessor spir-v-function-parameters)
   (basic-blocks
    :initarg :basic-blocks
    :initform nil
    :accessor spir-v-function-basic-blocks)))

(defclass spir-v-basic-block (spir-v-structure)
  ((label
    :initarg :label
    :accessor spir-v-basic-block-label)
   (instructions
    :initarg :instructions
    :initform nil
    :accessor spir-v-basic-block-instructions)))

(defgeneric lower-spir-v (object)
  (:documentation
   "Lower one structured shader object to a list of instruction instances."))

(defmethod lower-spir-v ((object instruction))
  (list object))

(defun lower-spir-v-sequence (objects)
  (let ((instructions nil))
    (map nil
         (lambda (object)
           (setf instructions
                 (nconc instructions
                        (cond
                          ((typep object 'instruction)
                           (list object))
                          ((typep object 'spir-v-structure)
                           (lower-spir-v object))
                          ((consp object)
                           (list (parse-instruction object)))
                          (t
                           (error 'spir-v-error
                                  :form object
                                  :reason :cannot-lower))))))
         objects)
    instructions))

(defmethod lower-spir-v ((entry-point spir-v-entry-point))
  (list
   (parse-instruction
    (list* 'entry-point
           (spir-v-entry-point-execution-model entry-point)
           (spir-v-entry-point-function entry-point)
           (spir-v-entry-point-name entry-point)
           (spir-v-entry-point-interfaces entry-point)))))

(defmethod lower-spir-v ((mode spir-v-execution-mode))
  (list
   (parse-instruction
    (list* 'execution-mode
           (spir-v-execution-mode-function mode)
           (spir-v-execution-mode-name mode)
           (spir-v-execution-mode-literals mode)))))

(defmethod lower-spir-v ((block spir-v-basic-block))
  (cons (parse-instruction
         (list (spir-v-basic-block-label block) 'label))
        (lower-spir-v-sequence
         (spir-v-basic-block-instructions block))))

(defmethod lower-spir-v ((definition spir-v-function-definition))
  (append
   (list
    (parse-instruction
     (list (spir-v-function-result-id definition)
           'function
           (spir-v-function-return-type definition)
           (spir-v-function-control definition)
           (spir-v-function-type definition))))
   (lower-spir-v-sequence (spir-v-function-parameters definition))
   (lower-spir-v-sequence (spir-v-function-basic-blocks definition))
   (list (parse-instruction '(function-end)))))

(defmethod lower-spir-v ((module spir-v-module))
  (append
   (loop for capability in (spir-v-module-capabilities module)
         collect (parse-instruction (list 'capability capability)))
   (list
    (parse-instruction
     (list 'memory-model
           (spir-v-module-addressing-model module)
           (spir-v-module-memory-model module))))
   (lower-spir-v-sequence (spir-v-module-entry-points module))
   (lower-spir-v-sequence (spir-v-module-execution-modes module))
   (lower-spir-v-sequence (spir-v-module-debug-instructions module))
   (lower-spir-v-sequence (spir-v-module-annotations module))
   (lower-spir-v-sequence (spir-v-module-global-declarations module))
   (lower-spir-v-sequence (spir-v-module-function-definitions module))))

(defun assemble-spir-v-module (module)
  "Lower structured MODULE and assemble its linear instruction stream."
  (assemble (lower-spir-v module)
            :version (spir-v-module-version module)
            :generator (spir-v-module-generator module)))

(defun gradient-compute-module (&key (width 640) (height 480))
  "Make a structured compute shader which writes an XY gradient to RGBA8."
  (unless (and (integerp width) (plusp width)
               (integerp height) (plusp height))
    (error 'spir-v-error :form nil :reason :invalid-image-size
           :details (list width height)))
  (make-instance
   'spir-v-module
   :entry-points
   (list (make-instance 'spir-v-entry-point
                        :function '%main
                        :interfaces '(%global-id)))
   :execution-modes
   (list (make-instance 'spir-v-execution-mode
                        :function '%main
                        :name 'local-size
                        :literals '(8 8 1)))
   :annotations
   '((decorate %global-id built-in
      (enum built-in global-invocation-id))
     (decorate %output-image descriptor-set 0)
     (decorate %output-image binding 0))
   :global-declarations
   `((%void type-void)
     (%uint type-int 32 0)
     (%float type-float 32)
     (%uvec2 type-vector %uint 2)
     (%uvec3 type-vector %uint 3)
     (%vec4 type-vector %float 4)
     (%storage-image type-image %float 2d 0 0 0 2 rgba8)
     (%input-uvec3-pointer type-pointer input %uvec3)
     (%image-pointer type-pointer uniform-constant %storage-image)
     (%function-type type-function %void)
     (%inverse-width constant %float ,(/ 1.0 width))
     (%inverse-height constant %float ,(/ 1.0 height))
     (%blue constant %float 0.25)
     (%one constant %float 1.0)
     (%global-id variable %input-uvec3-pointer input)
     (%output-image variable %image-pointer uniform-constant))
   :function-definitions
   (list
    (make-instance
     'spir-v-function-definition
     :result-id '%main
     :return-type '%void
     :function-type '%function-type
     :basic-blocks
     (list
      (make-instance
       'spir-v-basic-block
       :label '%entry
       :instructions
       '((%global-value load %uvec3 %global-id)
         (%coordinate vector-shuffle %uvec2
                      %global-value %global-value 0 1)
         (%x composite-extract %uint %coordinate 0)
         (%y composite-extract %uint %coordinate 1)
         (%xf convert-u-to-f %float %x)
         (%yf convert-u-to-f %float %y)
         (%red f-mul %float %xf %inverse-width)
         (%green f-mul %float %yf %inverse-height)
         (%color composite-construct %vec4 %red %green %blue %one)
         (%image load %storage-image %output-image)
         (image-write %image %coordinate %color)
         (return))))))))

(defun gradient-compute-shader (&key (width 640) (height 480))
  "Assemble the structured RGBA8 gradient compute shader."
  (assemble-spir-v-module
   (gradient-compute-module :width width :height height)))
