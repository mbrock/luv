;;;; Lower every vertex and fragment shader the applications define to HLSL
;;;; and compile each with DXC, as make msl-validate does for Metal.
;;;;
;;;; The corpus is discovered rather than listed: DEFINE-SHADER values,
;;;; zero-argument *-SPECIFICATION functions (DEFINE-LIVE-SHADER), and the
;;;; EQL-specialized SHADER-SPECIFICATION-FOR methods.

(require :asdf)
(require :sb-introspect)

(dolist (file '("luv.asd" "luvcraft.asd" "luft.asd" "mqtt.asd"
                "openai.asd" "telegram.asd"))
  (asdf:load-asd (truename file)))
(handler-bind ((warning #'muffle-warning))
  (asdf:load-system "luv/hlsl")
  (asdf:load-system "luvcraft/agent")
  (asdf:load-system "luft/renderer"))

(defpackage #:luv.hlsl-validation
  (:use #:cl)
  (:local-nicknames (#:shader #:luv.shader) (#:hlsl #:luv.hlsl)))
(in-package #:luv.hlsl-validation)

(defun corpus ()
  (let ((specifications nil))
    (flet ((note (value)
             (when (and (typep value 'shader:shader-specification)
                        (member (shader:shader-specification-stage value)
                                '(:vertex :fragment)))
               (pushnew value specifications))))
      (do-all-symbols (symbol)
        (when (boundp symbol)
          (note (symbol-value symbol)))
        (when (and (fboundp symbol)
                   (not (macro-function symbol))
                   (not (special-operator-p symbol))
                   (uiop:string-suffix-p (symbol-name symbol) "-SPECIFICATION")
                   (null (sb-introspect:function-lambda-list
                          (fdefinition symbol))))
          (note (ignore-errors (funcall symbol)))))
      (dolist (method (closer-mop:generic-function-methods
                       #'shader:shader-specification-for))
        (let ((specializers (closer-mop:method-specializers method)))
          (when (every (lambda (specializer)
                         (typep specializer 'closer-mop:eql-specializer))
                       specializers)
            (note (apply #'shader:shader-specification-for
                         (mapcar #'closer-mop:eql-specializer-object
                                 specializers)))))))
    (sort specifications #'string<
          :key (lambda (specification)
                 (symbol-name (shader:shader-object-name specification))))))

(defun validate (&optional (directory #p"build/hlsl/"))
  (ensure-directories-exist directory)
  (let ((failures 0) (count 0))
    (dolist (specification (corpus))
      (let* ((document (hlsl:compile-hlsl specification))
             (name (format nil "~(~A~)-~(~A~)"
                           (shader:shader-object-name specification)
                           (shader:shader-specification-stage specification)))
             (pathname (merge-pathnames (make-pathname :name name
                                                       :type "hlsl")
                                        directory)))
        (incf count)
        (hlsl:write-hlsl document pathname)
        (multiple-value-bind (output error-output status)
            (uiop:run-program
             (list "dxc" "-HV" "2021" "-WX"
                   "-T" (hlsl:hlsl-document-profile document)
                   "-E" (hlsl:hlsl-entry-point-name
                         (hlsl:hlsl-document-entry-point document))
                   "-Fo" (uiop:native-namestring
                          (make-pathname :type "dxil" :defaults pathname))
                   (uiop:native-namestring pathname))
             :output :string :error-output :string :ignore-error-status t)
          (unless (zerop status)
            (incf failures)
            (format t "~&~A:~%~A~A~%" (file-namestring pathname)
                    output error-output)))))
    (format t "~&hlsl-validate: ~D of ~D shaders compile with DXC.~%"
            (- count failures) count)
    (uiop:quit (if (zerop failures) 0 1))))

(validate)
