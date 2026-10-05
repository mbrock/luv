(defpackage #:luv.shader-user
  (:use #:cl #:luv.shader)
  ;; The language's STEP is the smoothstep family's edge test, not CL:STEP.
  (:shadowing-import-from #:luv.shader #:step)
  (:documentation
   "The package luv-shaderc reads shader source files in, unless a file
says IN-PACKAGE: Common Lisp plus the shader language, so a file can be
plain DEFINE-SHADER and DEFINE-SHADER-PROGRAM forms."))

(defpackage #:luv.shaderc
  (:use #:cl)
  (:local-nicknames (#:shader #:luv.shader)
                    (#:msl #:luv.msl)
                    (#:hlsl #:luv.hlsl))
  (:documentation
   "Ahead-of-time compilation of shader programs to MSL, HLSL, and the
reflection a native renderer builds its pipelines from.")
  (:export #:*targets*
           #:shaderc-error
           #:compiled-program
           #:compiled-program-name
           #:compiled-program-linkage
           #:compiled-program-stages
           #:compiled-stage
           #:compiled-stage-stage
           #:compiled-stage-entry-point
           #:compiled-stage-msl
           #:compiled-stage-hlsl
           #:load-shader-source
           #:compile-shader-program
           #:write-compiled-program
           #:compile-shader-files
           #:program-json
           #:program-header
           #:snake-identifier
           #:camel-identifier
           #:main))
