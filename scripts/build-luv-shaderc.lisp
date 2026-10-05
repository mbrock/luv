;;;; Build build/luv-shaderc: the shader language, its MSL and HLSL
;;;; lowerings, and their command line, as one executable.
;;;;
;;;; This runs both in the development shell and in the Nix package, whose
;;;; Lisp holds only Closer-MOP: nothing here loads SDL, Vulkan, Metal, or
;;;; McCLIM.

(require :asdf)

;;; luv.asd also defines the FFmpeg binding, whose DEFSYSTEM groves C headers
;;; through CFFI and so needs CFFI's groveller merely to be read.  The shader
;;; compiler never builds that system; where CFFI is absent, stand in for
;;; the groveller's system and component class so the definitions parse.
(unless (asdf:find-system "cffi-grovel" nil)
  (asdf:register-immutable-system "cffi-grovel")
  (defclass asdf::cffi-grovel-file (asdf:cl-source-file) ()))

(asdf:load-asd
 (merge-pathnames #P"../luv.asd"
                  (uiop:pathname-directory-pathname *load-truename*)))

;;; ASDF's PROGRAM-OP would dump the same image uncompressed; a compressed
;;; core is a fifth of the size and still starts in a fraction of a second.
(let ((system (asdf:find-system "luv/shaderc/program")))
  (handler-bind ((style-warning #'muffle-warning))
    (asdf:load-system system))
  (setf uiop:*image-entry-point*
        (uiop:ensure-function (asdf::component-entry-point system)))
  (uiop:dump-image (ensure-directories-exist
                    (asdf:output-file 'asdf:program-op system))
                   :executable t
                   :compression (and (member :sb-core-compression *features*)
                                     t)))
