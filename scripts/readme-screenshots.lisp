;;;; Rebuild the screenshots used by README.md from real luv render paths.

(require :asdf)

(defun readme-screenshot-project-root ()
  (merge-pathnames #P"../"
                   (uiop:pathname-directory-pathname *load-truename*)))

(asdf:load-asd
 (merge-pathnames #P"luv.asd" (readme-screenshot-project-root)))
(asdf:load-asd
 (merge-pathnames #P"luvcraft.asd" (readme-screenshot-project-root)))
(asdf:load-asd
 (merge-pathnames #P"mcluv.asd" (readme-screenshot-project-root)))
(asdf:load-system :luvcraft)
(asdf:load-system :mcluv/shader-lab)

(defun readme-screenshot-target ()
  (let ((arguments (uiop:command-line-arguments)))
    (when (> (length arguments) 1)
      (error "Usage: readme-screenshots.lisp [TARGET-DIRECTORY]"))
    (uiop:ensure-directory-pathname
     (or (first arguments)
         (merge-pathnames #P"screenshots/"
                          (readme-screenshot-project-root))))))

(defun capture-readme-screenshots (directory)
  (ensure-directories-exist directory)
  (dolist (view '(:shadow-forest :glow-floor))
    (format t "Capturing luvcraft ~A...~%" view)
    (luvcraft:capture-luvcraft-gazetteer-view view directory))
  (format t "Capturing the McCLIM shader lab...~%")
  (mcluv:capture-default-shader-lab-screenshot
   (merge-pathnames #P"mcclim-shader-lab.png" directory))
  (format t "README screenshots written under ~A~%" directory)
  directory)

(luv:call-with-sdl-main-thread
 (lambda ()
   (capture-readme-screenshots (readme-screenshot-target))))
