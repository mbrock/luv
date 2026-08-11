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

(defun spinning-texture-vertex-module ()
  "Make a vertex shader for a vertexless, perspective-spinning texture quad.

Binding 2 is a uniform block whose first vector contains sine and cosine."
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
     (%scale constant %float 0.82)
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
         (%x-twice f-mul %float %xf %two)
         (%center-x f-sub %float %x-twice %one)
         (%y-twice f-mul %float %yf %two)
         ;; A positive-height Vulkan viewport maps NDC -1 to its top edge.
         ;; Keep UV zero at that same top edge so the uploaded CLIM raster
         ;; remains upright.
         (%center-y f-sub %float %y-twice %one)
         (%scaled-x f-mul %float %center-x %scale)
         (%rotated-x f-mul %float %scaled-x %cosine)
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

(defun block-world-vertex-module ()
  "Make the vertex shader for textured, lit block vertices."
  (make-instance
   'spir-v-module
   :entry-points
   (list (make-instance
          'spir-v-entry-point :execution-model 'vertex
          :function '%main
          :interfaces '(%world-position %uv-shade %normal
                        %position %uv-shade-output %normal-output
                        %fog-output)))
   :annotations
   '((decorate %world-position location 0)
     (decorate %uv-shade location 1)
     (decorate %normal location 2)
     (decorate %position built-in (enum built-in position))
     (decorate %uv-shade-output location 0)
     (decorate %normal-output location 1)
     (decorate %fog-output location 2)
     (decorate %camera-block block)
     (member-decorate %camera-block 0 offset 0)
     (member-decorate %camera-block 1 offset 16)
     (member-decorate %camera-block 2 offset 32)
     (member-decorate %camera-block 3 offset 48)
     (member-decorate %camera-block 4 offset 64)
     (member-decorate %camera-block 5 offset 80)
     (decorate %camera-buffer descriptor-set 0)
     (decorate %camera-buffer binding 2))
   :global-declarations
   '((%void type-void)
     (%uint type-int 32 0)
     (%float type-float 32)
     (%vec3 type-vector %float 3)
     (%vec4 type-vector %float 4)
     (%camera-block type-struct %vec4 %vec4 %vec4 %vec4 %vec4 %vec4)
     (%input-vec3-pointer type-pointer input %vec3)
     (%output-vec3-pointer type-pointer output %vec3)
     (%output-vec4-pointer type-pointer output %vec4)
     (%camera-block-pointer type-pointer uniform %camera-block)
     (%uniform-vec4-pointer type-pointer uniform %vec4)
     (%function-type type-function %void)
     (%zero-u constant %uint 0)
     (%one-u constant %uint 1)
     (%two-u constant %uint 2)
     (%three-u constant %uint 3)
     (%four-u constant %uint 4)
     (%five-u constant %uint 5)
     (%one constant %float 1.0)
     (%world-position variable %input-vec3-pointer input)
     (%uv-shade variable %input-vec3-pointer input)
     (%normal variable %input-vec3-pointer input)
     (%position variable %output-vec4-pointer output)
     (%uv-shade-output variable %output-vec3-pointer output)
     (%normal-output variable %output-vec3-pointer output)
     (%fog-output variable %output-vec4-pointer output)
     (%camera-buffer variable %camera-block-pointer uniform))
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
       '((%world load %vec3 %world-position)
         (%uv-shade-value load %vec3 %uv-shade)
         (%normal-value load %vec3 %normal)
         (store %uv-shade-output %uv-shade-value)
         (store %normal-output %normal-value)
         (%camera-pointer access-chain
                          %uniform-vec4-pointer %camera-buffer %zero-u)
         (%right-pointer access-chain
                         %uniform-vec4-pointer %camera-buffer %one-u)
         (%up-pointer access-chain
                      %uniform-vec4-pointer %camera-buffer %two-u)
         (%forward-pointer access-chain
                           %uniform-vec4-pointer %camera-buffer %three-u)
         (%projection-pointer access-chain
                              %uniform-vec4-pointer %camera-buffer %four-u)
         (%fog-pointer access-chain
                       %uniform-vec4-pointer %camera-buffer %five-u)
         (%camera4 load %vec4 %camera-pointer)
         (%right4 load %vec4 %right-pointer)
         (%up4 load %vec4 %up-pointer)
         (%forward4 load %vec4 %forward-pointer)
         (%projection load %vec4 %projection-pointer)
         (%fog-state load %vec4 %fog-pointer)
         (%camera vector-shuffle %vec3 %camera4 %camera4 0 1 2)
         (%right vector-shuffle %vec3 %right4 %right4 0 1 2)
         (%up vector-shuffle %vec3 %up4 %up4 0 1 2)
         (%forward vector-shuffle %vec3 %forward4 %forward4 0 1 2)
         (%relative f-sub %vec3 %world %camera)
         (%view-x dot %float %relative %right)
         (%view-y dot %float %relative %up)
         (%view-z dot %float %relative %forward)
         (%inverse-far composite-extract %float %fog-state 3)
         (%fog-distance f-mul %float %view-z %inverse-far)
         (%fog-distance-squared f-mul %float %fog-distance %fog-distance)
         (%fog-factor f-sub %float %one %fog-distance-squared)
         (%sky-red composite-extract %float %fog-state 0)
         (%sky-green composite-extract %float %fog-state 1)
         (%sky-blue composite-extract %float %fog-state 2)
         (%fog-varying composite-construct
                       %vec4 %sky-red %sky-green %sky-blue %fog-factor)
         (store %fog-output %fog-varying)
         (%x-scale composite-extract %float %projection 0)
         (%y-scale composite-extract %float %projection 1)
         (%z-scale composite-extract %float %projection 2)
         (%z-offset composite-extract %float %projection 3)
         (%clip-x f-mul %float %view-x %x-scale)
         (%scaled-y f-mul %float %view-y %y-scale)
         ;; Vulkan's positive viewport height points screen Y downward.
         (%clip-y f-negate %float %scaled-y)
         (%scaled-z f-mul %float %view-z %z-scale)
         (%clip-z f-add %float %scaled-z %z-offset)
         (%clip-position composite-construct
                         %vec4 %clip-x %clip-y %clip-z %view-z)
         (store %position %clip-position)
         (return))))))))

(defun block-world-vertex-shader ()
  (assemble-spir-v-module (block-world-vertex-module)))

(defun block-world-crosshair-vertex-module ()
  "Pass screen-space crosshair vertices and their ink to the fragment stage."
  (make-instance
   'spir-v-module
   :entry-points
   (list (make-instance
          'spir-v-entry-point :execution-model 'vertex
          :function '%main
          :interfaces '(%screen-position %ink %position %ink-output)))
   :annotations
   '((decorate %screen-position location 0)
     (decorate %ink location 1)
     (decorate %position built-in (enum built-in position))
     (decorate %ink-output location 0))
   :global-declarations
   '((%void type-void)
     (%float type-float 32)
     (%vec3 type-vector %float 3)
     (%vec4 type-vector %float 4)
     (%input-vec3-pointer type-pointer input %vec3)
     (%output-vec3-pointer type-pointer output %vec3)
     (%output-vec4-pointer type-pointer output %vec4)
     (%function-type type-function %void)
     (%one constant %float 1.0)
     (%screen-position variable %input-vec3-pointer input)
     (%ink variable %input-vec3-pointer input)
     (%position variable %output-vec4-pointer output)
     (%ink-output variable %output-vec3-pointer output))
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
       '((%screen load %vec3 %screen-position)
         (%ink-value load %vec3 %ink)
         (%screen-x composite-extract %float %screen 0)
         (%screen-y composite-extract %float %screen 1)
         (%screen-z composite-extract %float %screen 2)
         (%clip-position composite-construct
                         %vec4 %screen-x %screen-y %screen-z %one)
         (store %position %clip-position)
         (store %ink-output %ink-value)
         (return))))))))

(defun block-world-crosshair-vertex-shader ()
  (assemble-spir-v-module (block-world-crosshair-vertex-module)))
