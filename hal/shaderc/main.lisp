;;; The luv-shaderc command line.

(in-package #:luv.shaderc)

(defparameter *usage*
  "Usage: luv-shaderc --out DIR [--target msl] [--target hlsl] FILE.lisp...

Compile every DEFINE-SHADER-PROGRAM in the given files.  For a program NAME
it writes, into DIR:

  NAME.STAGE.metal   one MSL 4 document per stage
  NAME.STAGE.hlsl    one HLSL document per stage (DXC, shader model 6.0)
  NAME.json          stages, entry points, resources, and fragment outputs
  NAME.hh            the same reflection for C++ (moppe/nhal/reflection.hh)

Files are read in package LUV.SHADER-USER, which uses COMMON-LISP and the
shader language, unless they say IN-PACKAGE.  Without --target, both
languages are written.
")

(defun parse-command-line (arguments)
  "Return the output directory, the targets, and the source files."
  (let ((directory nil) (targets nil) (files nil))
    (loop while arguments
          do (let ((argument (pop arguments)))
               (cond
                 ((member argument '("-h" "--help") :test #'string=)
                  (write-string *usage*)
                  (uiop:quit 0))
                 ((member argument '("-o" "--out") :test #'string=)
                  (setf directory
                        (or (pop arguments)
                            (shaderc-fail "~A needs a directory." argument))))
                 ((string= argument "--target")
                  (let ((target (pop arguments)))
                    (pushnew
                     (cond ((equal target "msl") :msl)
                           ((equal target "hlsl") :hlsl)
                           (t (shaderc-fail "Unknown target ~S; expected ~
                                             msl or hlsl." target)))
                     targets)))
                 ((and (plusp (length argument))
                       (char= #\- (char argument 0)))
                  (shaderc-fail "Unknown option ~A.~%~%~A" argument *usage*))
                 (t (push argument files)))))
    (unless directory
      (shaderc-fail "Missing --out DIR.~%~%~A" *usage*))
    (unless files
      (shaderc-fail "No shader source files.~%~%~A" *usage*))
    (values directory
            (or (nreverse targets) *targets*)
            (nreverse files))))

(defun report-failure (condition &optional (stream *error-output*))
  (let ((*print-pretty* t)
        (*print-right-margin* 100)
        (*print-length* 12)
        (*print-level* 6)
        (*print-case* :downcase))
    (format stream "luv-shaderc: error~@[ in ~{~A~^, ~}~]:~%"
            (reverse *compilation-context*))
    (if (typep condition 'shader:shader-language-error)
        (format stream "  ~(~A~)~@[ ~S~]~@[~%  in ~S~]~%"
                (shader:shader-language-error-reason condition)
                (shader:shader-language-error-details condition)
                (shader:shader-language-error-form condition))
        (format stream "  ~A~%" condition))
    (finish-output stream)))

(defun main ()
  "Run luv-shaderc on the process's command-line arguments."
  (handler-case
      (handler-bind ((error (let ((stream *error-output*))
                              (lambda (condition)
                                (report-failure condition stream)
                                (uiop:quit 1)))))
        (multiple-value-bind (directory targets files)
            (parse-command-line (uiop:command-line-arguments))
          (multiple-value-bind (programs written)
              (compile-shader-files files :directory directory
                                          :targets targets)
            (format t "luv-shaderc: ~D program~:P, ~D file~:P in ~A~%"
                    (length programs) (length written)
                    (uiop:native-namestring
                     (uiop:ensure-directory-pathname directory)))))
        (uiop:quit 0))
    (sb-sys:interactive-interrupt ()
      (uiop:quit 130))))
