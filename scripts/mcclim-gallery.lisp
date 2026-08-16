;;;; Capture upstream McCLIM examples through luv's direct GPU backend.

(require :asdf)

(defun gallery-project-root ()
  (merge-pathnames #P"../"
                   (uiop:pathname-directory-pathname *load-truename*)))

(asdf:load-asd (merge-pathnames #P"luv.asd" (gallery-project-root)))
(asdf:load-asd (merge-pathnames #P"mcluv.asd" (gallery-project-root)))
(asdf:load-system :mcluv/gallery)

(defun gallery-target ()
  (let ((arguments (uiop:command-line-arguments)))
    (when (> (length arguments) 1)
      (error "Usage: mcclim-gallery.lisp [TARGET-DIRECTORY]"))
    (uiop:ensure-directory-pathname
     (or (first arguments)
         (merge-pathnames #P"build/mcclim-gallery/"
                          (gallery-project-root))))))

(setf luv:*gpu-provider* (make-instance 'luv:metal-gpu-provider))

(luv:call-with-sdl-main-thread
 (lambda ()
   (mcluv:capture-mcclim-gallery (gallery-target))))
