;;;; Isolated before/after runs; BASELINE_LOWERING=1 restores the old lowering.
(require :asdf)
(assert (uiop:getenv "BENCH_LIBRARY"))
(load "scripts/bench-gpu-fps.lisp")
(when (uiop:getenv "BASELINE_LOWERING")
  ;; Restore executable definitions only, never reset the live type registries.
  (dolist (entry '((:luv.shader "hal/shader/language.lisp")
                   (:luv.msl "hal/metal/msl/lowering.lisp")))
    (let ((*package* (find-package (first entry)))
          (source (uiop:run-program
                   (list "git" "show"
                         (format nil "4909081ce63d804bcb349dd06b15162ba8f623f7:~A"
                                 (second entry)))
                   :output :string)))
      (with-input-from-string (in source)
        (loop for form = (read in nil nil) while form
              when (or (and (eq (first form) 'defun)
                            (eq (second form) 'luv.shader::collect-shader-bindings))
                       (and (eq (first form) 'defmethod)
                            (eq (second form) 'luv.msl::lower-msl-expression)
                            (find 'luv.shader:shader-conditional (third form)
                                  :key #'second)))
                do (eval form))))))
(let ((luft.render.shaders::*terrain-cloud-visibility-function*
        'luft.render.shaders::luvcraft-cloud-visibility)
      (luft.render.shaders::*surface-atmosphere-function*
        'luft.render.shaders::luvcraft-atmospheric-surface))
  (luv.msl:write-msl
   (luv.msl:compile-msl (luft.render.shaders:star-fragment-specification))
   (concatenate 'string (uiop:getenv "BENCH_OUTPUT") ".metal")))
(let ((original (symbol-function 'gpu-fps::run-case)))
  (setf (symbol-function 'gpu-fps::run-case)
        (lambda (kind backend width height)
          (unless (eq backend :old)
            (funcall original kind backend width height)))))
(gpu-fps::main)
(sb-ext:exit :code 0 :abort t)
