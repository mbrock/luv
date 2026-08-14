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
         ("--height" :height ,#'parse-integer-option)
         ("--count" :count ,#'parse-integer-option)
         ("--forward-step" :forward-step ,#'parse-real-option)
         ("--yaw-step" :yaw-step ,#'parse-real-option)
         ("--day-start" :day-start ,#'parse-real-option)
         ("--day-step" :day-step ,#'parse-real-option)
         ("--difference-scale" :difference-scale ,#'parse-real-option)
         ("--shadow-only" :shadow-only ,#'parse-integer-option)))
    (unless (= 1 (length positionals))
      (command-line-error "gazetteer expects exactly one TARGET directory."))
    (let ((target (pathname (first positionals)))
          (view (getf options :view))
          (width (getf options :width))
          (height (getf options :height))
          (count (getf options :count)))
      (when (and count (null view))
        (command-line-error "gazetteer --count requires one --view."))
      (let ((pathnames
              (if count
                  (luv:capture-luvcraft-gazetteer-sequence
                   view target :count count
                   :forward-step (or (getf options :forward-step) 0.2)
                   :yaw-step (or (getf options :yaw-step) 0.0)
                   :day-start (getf options :day-start)
                   :day-step (or (getf options :day-step) 0.0)
                   :difference-scale (getf options :difference-scale)
                   :shadow-diagnostic-p (getf options :shadow-only)
                   :width width :height height)
                  (luv:capture-luvcraft-gazetteer
                   target
                   :views (and view (list view))
                   :width width
                   :height height))))
        (if (getf options :difference-scale)
            (format t "Wrote ~D temporal artifacts under ~A~%"
                    (length pathnames) target)
            (dolist (pathname pathnames)
              (format t "~A~%" pathname)))))))
