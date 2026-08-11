;;;; Capture hidden block-world frames from a fresh SBCL in the luv shell.

(require :asdf)

(defun script-project-root ()
  (merge-pathnames #P"../"
                   (uiop:pathname-directory-pathname *load-truename*)))

(defun parse-positive-integer (string name)
  (let ((value (parse-integer string :junk-allowed nil)))
    (unless (plusp value)
      (error "~A must be positive, got ~D." name value))
    value))

(defun call-luv (name &rest arguments)
  (apply (symbol-function (find-symbol name "LUV")) arguments))

(let* ((project-root (script-project-root))
       (arguments (uiop:command-line-arguments))
       (target (or (first arguments) "/tmp/luv-block-world.png"))
       (count (and (second arguments)
                   (parse-positive-integer (second arguments) "count"))))
  (asdf:load-asd (merge-pathnames #P"luv.asd" project-root))
  (asdf:load-system :luv/examples)
  (if count
      (dolist (pathname
                (call-luv "CAPTURE-HIDDEN-CUBE-WORLD-FRAMES"
                          (pathname target) :count count))
        (format t "~A~%" pathname))
      (format t "~A~%"
              (call-luv "CAPTURE-HIDDEN-CUBE-WORLD-SCREENSHOT"
                        (pathname target)))))
