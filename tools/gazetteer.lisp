(in-package #:luv.tools)

(defun parse-gazetteer-view-name-option (string option)
  (declare (ignore option))
  (luv::normalize-luvcraft-gazetteer-view-name string))

(defun command-gazetteer (arguments)
  (multiple-value-bind (positionals options)
      (parse-keyword-options
       arguments
       `(("--view" :view ,#'parse-gazetteer-view-name-option)
         ("--width" :width ,#'parse-integer-option)
         ("--height" :height ,#'parse-integer-option)))
    (unless (= 1 (length positionals))
      (command-line-error "gazetteer expects exactly one TARGET directory."))
    (let ((target (pathname (first positionals)))
          (view (getf options :view))
          (width (getf options :width))
          (height (getf options :height)))
      (dolist (pathname
                (luv:capture-luvcraft-gazetteer
                 target
                 :views (and view (list view))
                 :width width
                 :height height))
        (format t "~A~%" pathname)))))
