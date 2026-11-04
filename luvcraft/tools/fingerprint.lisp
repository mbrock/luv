(in-package #:luvcraft.tools)

(defun parse-relief-option (string option)
  (cond ((string-equal string "alpine") :alpine)
        ((string-equal string "meadow") :meadow)
        (t (command-line-error "~A must be alpine or meadow, got ~S."
                               option string))))

(defun parse-natural-option (string option)
  (let ((value (ignore-errors (parse-integer string))))
    (unless (and value (<= 0 value))
      (command-line-error "~A must be a natural number, got ~S." option string))
    value))

(defun command-fingerprint (arguments)
  (multiple-value-bind (positionals options)
      (parse-keyword-options
       arguments
       `(("--seed" :seed ,#'parse-natural-option)
         ("--relief" :relief ,#'parse-relief-option)
         ("--radius" :radius ,#'parse-natural-option)))
    (when positionals
      (command-line-error "fingerprint takes no positional arguments."))
    (luvcraft.luft:write-luvcraft-world-fingerprint
     (luvcraft.luft:make-luvcraft-world-source
      :seed (or (getf options :seed) 121)
      :relief (or (getf options :relief) :alpine))
     :radius (or (getf options :radius) 1))))
