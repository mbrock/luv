(in-package #:luvcraft.tools)

(defun parse-host-option (value option)
  (declare (ignore option))
  (when (zerop (length value))
    (command-line-error "--host must not be empty."))
  value)

(defun command-body-gallery (arguments)
  (multiple-value-bind (positionals options)
      (parse-keyword-options
       arguments
       `(("--host" :host ,#'parse-host-option)
         ("--port" :port ,#'parse-integer-option)))
    (when positionals
      (command-line-error "body-gallery takes no positional arguments."))
    (luvcraft.body-gallery:serve-body-gallery
     :host (or (getf options :host) "127.0.0.1")
     :port (or (getf options :port) 8765))))
