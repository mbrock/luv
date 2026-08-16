(in-package #:luvcraft.tools)

(defun command-block-world (arguments)
  (multiple-value-bind (positionals options)
      (parse-keyword-options
       arguments
       `(("--backend" :backend ,#'parse-backend-option)
         ("--count" :count ,#'parse-integer-option)
         ("--width" :width ,#'parse-integer-option)
         ("--height" :height ,#'parse-integer-option)
         ("--yaw-step" :yaw-step ,#'parse-real-option)))
    (unless (= 1 (length positionals))
      (command-line-error "block-world expects exactly one TARGET pathname."))
    (let ((target (pathname (first positionals)))
          (count (getf options :count))
          (width (or (getf options :width) 960))
          (height (or (getf options :height) 640))
          (yaw-step (or (getf options :yaw-step) 0.35))
          (provider (make-backend-provider (getf options :backend))))
      (if count
          (dolist (pathname
                    (luvcraft:capture-hidden-luvcraft-frames
                     target
                     :count count
                     :width width
                     :height height
                     :provider provider
                     :yaw-step yaw-step))
            (format t "~A~%" pathname))
          (format t "~A~%"
                  (luvcraft:capture-hidden-luvcraft-screenshot
                   target
                   :width width
                   :height height
                   :provider provider))))))
