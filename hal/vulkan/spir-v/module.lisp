;;; Structured shader IR above the deliberately literal SPIR-V assembler.
;;;
;;; instructions.lisp is the small instruction vocabulary and binary encoder.  This
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
   (extensions
    :initarg :extensions
    :initform nil
    :accessor spir-v-module-extensions)
   (extended-instruction-imports
    :initarg :extended-instruction-imports
    :initform nil
    :accessor spir-v-module-extended-instruction-imports)
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

(defclass spir-v-extended-instruction-import (spir-v-structure)
  ((result-id
    :initarg :result-id
    :accessor spir-v-extended-instruction-import-result-id)
   (name
    :initarg :name
    :initform "GLSL.std.450"
    :accessor spir-v-extended-instruction-import-name))
  (:documentation
   "A durable OpExtInstImport: one named instruction set the module uses.
Extended operations reference its result id, so repeated operators in one
module share a single import."))

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

(defmethod lower-spir-v ((import spir-v-extended-instruction-import))
  (list
   (parse-instruction
    (list (spir-v-extended-instruction-import-result-id import)
          'ext-inst-import
          (spir-v-extended-instruction-import-name import)))))

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
   ;; SPIR-V logical layout: extensions and extended-instruction imports sit
   ;; between the capabilities and the memory model.
   (loop for extension in (spir-v-module-extensions module)
         collect (parse-instruction (list 'extension extension)))
   (lower-spir-v-sequence
    (spir-v-module-extended-instruction-imports module))
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

(defun spinning-texture-vertex-module ()
  "Make a vertex shader for a vertexless, perspective-spinning texture quad.

Binding 2 is a uniform block containing sine, cosine, and the horizontal
aspect correction needed to preserve the source texture's shape."
  (make-instance
   'spir-v-module
   :entry-points
   (list (make-instance
          'spir-v-entry-point :execution-model 'vertex
          :function '%main
          :interfaces '(%vertex-index %position %uv-output)))
   :annotations
   '((decorate %vertex-index built-in (enum built-in vertex-index))
     (decorate %position built-in (enum built-in position))
     (decorate %uv-output location 0)
     (decorate %spin-state-block block)
     (member-decorate %spin-state-block 0 offset 0)
     (decorate %spin-state-buffer descriptor-set 0)
     (decorate %spin-state-buffer binding 2))
   :global-declarations
   '((%void type-void)
     (%uint type-int 32 0)
     (%float type-float 32)
     (%vec2 type-vector %float 2)
     (%vec4 type-vector %float 4)
     (%spin-state-block type-struct %vec4)
     (%vertex-index-pointer type-pointer input %uint)
     (%position-pointer type-pointer output %vec4)
     (%uv-output-pointer type-pointer output %vec2)
     (%spin-state-block-pointer type-pointer uniform %spin-state-block)
     (%spin-state-value-pointer type-pointer uniform %vec4)
     (%function-type type-function %void)
     (%zero-u constant %uint 0)
     (%one-u constant %uint 1)
     (%zero constant %float 0.0)
     (%one constant %float 1.0)
     (%two constant %float 2.0)
     (%scale constant %float 0.68)
     (%orbit constant %float 0.12)
     (%depth-base constant %float 0.45)
     (%depth-scale constant %float 0.22)
     (%base-w constant %float 1.18)
     (%perspective-scale constant %float 0.48)
     (%vertex-index variable %vertex-index-pointer input)
     (%position variable %position-pointer output)
     (%uv-output variable %uv-output-pointer output)
     (%spin-state-buffer variable %spin-state-block-pointer uniform))
   :function-definitions
   (list
    (make-instance
     'spir-v-function-definition
     :result-id '%main :return-type '%void
     :function-type '%function-type
     :basic-blocks
     (list
      (make-instance
       'spir-v-basic-block :label '%entry
       :instructions
       '((%vertex load %uint %vertex-index)
         (%x-bit bitwise-and %uint %vertex %one-u)
         (%y-bit shift-right-logical %uint %vertex %one-u)
         (%xf convert-u-to-f %float %x-bit)
         (%yf convert-u-to-f %float %y-bit)
         (%uv composite-construct %vec2 %xf %yf)
         (store %uv-output %uv)
         (%state-pointer access-chain
                         %spin-state-value-pointer
                         %spin-state-buffer %zero-u)
         (%state load %vec4 %state-pointer)
         (%sine composite-extract %float %state 0)
         (%cosine composite-extract %float %state 1)
         (%aspect composite-extract %float %state 2)
         (%x-twice f-mul %float %xf %two)
         (%center-x f-sub %float %x-twice %one)
         (%y-twice f-mul %float %yf %two)
         ;; A positive-height Vulkan viewport maps NDC -1 to its top edge.
         ;; Keep UV zero at that same top edge so the uploaded CLIM raster
         ;; remains upright.
         (%center-y f-sub %float %y-twice %one)
         (%scaled-x f-mul %float %center-x %scale)
         (%aspect-x f-mul %float %scaled-x %aspect)
         (%rotated-x f-mul %float %aspect-x %cosine)
         (%orbit-x f-mul %float %sine %orbit)
         (%clip-x f-add %float %rotated-x %orbit-x)
         (%clip-y f-mul %float %center-y %scale)
         (%depth-x f-mul %float %center-x %sine)
         (%depth-offset f-mul %float %depth-x %depth-scale)
         (%clip-z f-add %float %depth-base %depth-offset)
         (%perspective f-mul %float %depth-x %perspective-scale)
         (%clip-w f-add %float %base-w %perspective)
         (%clip-position composite-construct
                         %vec4 %clip-x %clip-y %clip-z %clip-w)
         (store %position %clip-position)
         (return))))))))

(defun lisp-machine-chassis-vertex-module ()
  "Make three nested perspective quads behind a sampled Lisp-machine screen."
  (make-instance
   'spir-v-module
   :entry-points
   (list (make-instance
          'spir-v-entry-point :execution-model 'vertex
          :function '%main
          :interfaces '(%vertex-index %position %color-output)))
   :annotations
   '((decorate %vertex-index built-in (enum built-in vertex-index))
     (decorate %position built-in (enum built-in position))
     (decorate %color-output location 0)
     (decorate %spin-state-block block)
     (member-decorate %spin-state-block 0 offset 0)
     (decorate %spin-state-buffer descriptor-set 0)
     (decorate %spin-state-buffer binding 2))
   :global-declarations
   '((%void type-void)
     (%bool type-bool)
     (%uint type-int 32 0)
     (%float type-float 32)
     (%vec4 type-vector %float 4)
     (%spin-state-block type-struct %vec4)
     (%vertex-index-pointer type-pointer input %uint)
     (%position-pointer type-pointer output %vec4)
     (%color-output-pointer type-pointer output %vec4)
     (%spin-state-block-pointer type-pointer uniform %spin-state-block)
     (%spin-state-value-pointer type-pointer uniform %vec4)
     (%function-type type-function %void)
     (%zero-u constant %uint 0)
     (%one-u constant %uint 1)
     (%two-u constant %uint 2)
     (%four-u constant %uint 4)
     (%zero constant %float 0.0)
     (%one constant %float 1.0)
     (%two constant %float 2.0)
     (%shadow-x constant %float 0.82)
     (%shadow-y constant %float 0.86)
     (%body-x constant %float 0.80)
     (%body-y constant %float 0.84)
     (%bezel-x constant %float 0.73)
     (%bezel-y constant %float 0.74)
     (%shadow-offset-x constant %float 0.035)
     (%shadow-offset-y constant %float 0.055)
     (%body-offset-y constant %float 0.012)
     (%orbit constant %float 0.12)
     (%depth-base constant %float 0.46)
     (%depth-scale constant %float 0.22)
     (%base-w constant %float 1.18)
     (%perspective-scale constant %float 0.48)
     (%shadow-r constant %float 0.035)
     (%shadow-g constant %float 0.045)
     (%shadow-b constant %float 0.043)
     (%body-r constant %float 0.50)
     (%body-g constant %float 0.47)
     (%body-b constant %float 0.38)
     (%bezel-r constant %float 0.075)
     (%bezel-g constant %float 0.095)
     (%bezel-b constant %float 0.085)
     (%vertex-index variable %vertex-index-pointer input)
     (%position variable %position-pointer output)
     (%color-output variable %color-output-pointer output)
     (%spin-state-buffer variable %spin-state-block-pointer uniform))
   :function-definitions
   (list
    (make-instance
     'spir-v-function-definition
     :result-id '%main :return-type '%void
     :function-type '%function-type
     :basic-blocks
     (list
      (make-instance
       'spir-v-basic-block :label '%entry
       :instructions
       '((%vertex load %uint %vertex-index)
         (%layer u-div %uint %vertex %four-u)
         (%local u-mod %uint %vertex %four-u)
         (%x-bit bitwise-and %uint %local %one-u)
         (%y-bit shift-right-logical %uint %local %one-u)
         (%xf convert-u-to-f %float %x-bit)
         (%yf convert-u-to-f %float %y-bit)
         (%shadow-p u-less-than %bool %layer %one-u)
         (%body-p u-less-than %bool %layer %two-u)
         (%non-shadow-x select %float %body-p %body-x %bezel-x)
         (%scale-x select %float %shadow-p %shadow-x %non-shadow-x)
         (%non-shadow-y select %float %body-p %body-y %bezel-y)
         (%scale-y select %float %shadow-p %shadow-y %non-shadow-y)
         (%non-shadow-offset-x select %float %body-p %zero %zero)
         (%offset-x select %float
                    %shadow-p %shadow-offset-x %non-shadow-offset-x)
         (%non-shadow-offset-y select %float
                    %body-p %body-offset-y %zero)
         (%offset-y select %float
                    %shadow-p %shadow-offset-y %non-shadow-offset-y)
         (%non-shadow-r select %float %body-p %body-r %bezel-r)
         (%non-shadow-g select %float %body-p %body-g %bezel-g)
         (%non-shadow-b select %float %body-p %body-b %bezel-b)
         (%color-r select %float %shadow-p %shadow-r %non-shadow-r)
         (%color-g select %float %shadow-p %shadow-g %non-shadow-g)
         (%color-b select %float %shadow-p %shadow-b %non-shadow-b)
         (%color composite-construct
                 %vec4 %color-r %color-g %color-b %one)
         (store %color-output %color)
         (%state-pointer access-chain
                         %spin-state-value-pointer
                         %spin-state-buffer %zero-u)
         (%state load %vec4 %state-pointer)
         (%sine composite-extract %float %state 0)
         (%cosine composite-extract %float %state 1)
         (%aspect composite-extract %float %state 2)
         (%x-twice f-mul %float %xf %two)
         (%center-x f-sub %float %x-twice %one)
         (%y-twice f-mul %float %yf %two)
         (%center-y f-sub %float %y-twice %one)
         (%scaled-x f-mul %float %center-x %scale-x)
         (%aspect-x f-mul %float %scaled-x %aspect)
         (%rotated-x f-mul %float %aspect-x %cosine)
         (%orbit-x f-mul %float %sine %orbit)
         (%orbited-x f-add %float %rotated-x %orbit-x)
         (%clip-x f-add %float %orbited-x %offset-x)
         (%scaled-y f-mul %float %center-y %scale-y)
         (%clip-y f-add %float %scaled-y %offset-y)
         (%depth-x f-mul %float %center-x %sine)
         (%depth-offset f-mul %float %depth-x %depth-scale)
         (%clip-z f-add %float %depth-base %depth-offset)
         (%perspective f-mul %float %depth-x %perspective-scale)
         (%clip-w f-add %float %base-w %perspective)
         (%clip-position composite-construct
                         %vec4 %clip-x %clip-y %clip-z %clip-w)
         (store %position %clip-position)
         (return))))))))

(defun lisp-machine-chassis-fragment-module ()
  "Pass the procedural chassis color through to the attachment."
  (make-instance
   'spir-v-module
   :entry-points
   (list (make-instance
          'spir-v-entry-point :execution-model 'fragment
          :function '%main :interfaces '(%color-input %color-output)))
   :execution-modes
   (list (make-instance 'spir-v-execution-mode
                        :function '%main :name 'origin-upper-left))
   :annotations
   '((decorate %color-input location 0)
     (decorate %color-output location 0))
   :global-declarations
   '((%void type-void)
     (%float type-float 32)
     (%vec4 type-vector %float 4)
     (%color-input-pointer type-pointer input %vec4)
     (%color-output-pointer type-pointer output %vec4)
     (%function-type type-function %void)
     (%color-input variable %color-input-pointer input)
     (%color-output variable %color-output-pointer output))
   :function-definitions
   (list
    (make-instance
     'spir-v-function-definition
     :result-id '%main :return-type '%void
     :function-type '%function-type
     :basic-blocks
     (list
      (make-instance
       'spir-v-basic-block :label '%entry
       :instructions
       '((%color load %vec4 %color-input)
         (store %color-output %color)
         (return))))))))

(defun spinning-texture-fragment-module ()
  "Make the fragment half of the spinning sampled-texture pipeline."
  (make-instance
   'spir-v-module
   :entry-points
   (list (make-instance
          'spir-v-entry-point :execution-model 'fragment
          :function '%main :interfaces '(%uv-input %color-output)))
   :execution-modes
   (list (make-instance 'spir-v-execution-mode
                        :function '%main :name 'origin-upper-left))
   :annotations
   '((decorate %uv-input location 0)
     (decorate %color-output location 0)
     (decorate %source-texture descriptor-set 0)
     (decorate %source-texture binding 0)
     (decorate %source-sampler descriptor-set 0)
     (decorate %source-sampler binding 1))
   :global-declarations
   '((%void type-void)
     (%float type-float 32)
     (%vec2 type-vector %float 2)
     (%vec4 type-vector %float 4)
     (%image type-image %float 2d 0 0 0 1 unknown)
     (%sampler type-sampler)
     (%sampled-image type-sampled-image %image)
     (%uv-input-pointer type-pointer input %vec2)
     (%color-output-pointer type-pointer output %vec4)
     (%image-pointer type-pointer uniform-constant %image)
     (%sampler-pointer type-pointer uniform-constant %sampler)
     (%function-type type-function %void)
     (%uv-input variable %uv-input-pointer input)
     (%color-output variable %color-output-pointer output)
     (%source-texture variable %image-pointer uniform-constant)
     (%source-sampler variable %sampler-pointer uniform-constant))
   :function-definitions
   (list
    (make-instance
     'spir-v-function-definition
     :result-id '%main :return-type '%void
     :function-type '%function-type
     :basic-blocks
     (list
      (make-instance
       'spir-v-basic-block :label '%entry
       :instructions
       '((%uv load %vec2 %uv-input)
         (%texture load %image %source-texture)
         (%sampler-value load %sampler %source-sampler)
         (%sampled sampled-image %sampled-image %texture %sampler-value)
         (%color image-sample-implicit-lod %vec4 %sampled %uv)
         (store %color-output %color)
         (return))))))))

(defun spinning-texture-vertex-shader ()
  (assemble-spir-v-module (spinning-texture-vertex-module)))

(defun spinning-texture-fragment-shader ()
  (assemble-spir-v-module (spinning-texture-fragment-module)))

(defun lisp-machine-chassis-vertex-shader ()
  (assemble-spir-v-module (lisp-machine-chassis-vertex-module)))

(defun lisp-machine-chassis-fragment-shader ()
  (assemble-spir-v-module (lisp-machine-chassis-fragment-module)))
