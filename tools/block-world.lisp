(in-package #:luv.tools)

(defun command-block-world (arguments)
  (multiple-value-bind (positionals options)
      (parse-keyword-options
       arguments
       `(("--count" :count ,#'parse-integer-option)
         ("--width" :width ,#'parse-integer-option)
         ("--height" :height ,#'parse-integer-option)
         ("--yaw-step" :yaw-step ,#'parse-real-option)))
    (unless (= 1 (length positionals))
      (command-line-error "block-world expects exactly one TARGET pathname."))
    (let ((target (pathname (first positionals)))
          (count (getf options :count))
          (width (or (getf options :width) 960))
          (height (or (getf options :height) 640))
          (yaw-step (or (getf options :yaw-step) 0.35)))
      (if count
          (dolist (pathname
                    (luv:capture-hidden-luvcraft-frames
                     target
                     :count count
                     :width width
                     :height height
                     :yaw-step yaw-step))
            (format t "~A~%" pathname))
          (format t "~A~%"
                  (luv:capture-hidden-luvcraft-screenshot
                   target
                   :width width
                   :height height))))))
