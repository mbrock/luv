;;;; A small CSS compiler: the stylesheet as Lisp definitions.
;;;;
;;;; The site's stylesheet is written as DEFINE-STYLE forms, groups of rules
;;;; in S-expression syntax, and compiled to CSS text when the site is
;;;; rendered, the way the shader language compiles to SPIR-V and MSL from
;;;; forms.  So the rules that draw a layout role can sit beside the layout
;;;; that assigns it, be re-evaluated in a running image, and be read and
;;;; rendered by the same tools as everything else here.
;;;;
;;;; A rule is (SELECTOR . BODY).  SELECTOR is a string, "a, b" for a group.
;;;; BODY alternates :property value declarations with nested rules, whose
;;;; selectors combine with the parent's: "h1" under ".library" is
;;;; ".library h1", and "&:hover" is ".library:hover".  (:media "query" . RULES)
;;;; wraps rules in a media query, at the top or nested inside a rule.
;;;;
;;;; A value is a string, written verbatim; a number; a symbol, downcased,
;;;; and if its name begins with -- a var() reference, so --ink is var(--ink);
;;;; or a list.  A list whose first element names a CSS function -- var, calc,
;;;; clamp, minmax, repeat, color-mix, url, rgb, rgba, hsl -- is that call with
;;;; its arguments comma-separated, and any other list is its values
;;;; space-separated: (1px solid --rule), (repeat 3 (minmax 0 1fr)),
;;;; (color-mix (in srgb) (--paper 94%) --accent).  Units read as symbols
;;;; (0.5fr, 2.75rem, 94%), so they are written bare; colours are strings
;;;; because #f is a reader macro.

(in-package #:luv.wiki)

(defclass style ()
  ((name :initarg :name :accessor style-name)
   (documentation :initarg :documentation :initform nil :accessor style-documentation)
   (rules :initarg :rules :initform '() :accessor style-rules))
  (:documentation "A named group of stylesheet rules, compiled in definition order."))

(defvar *styles* '()
  "STYLE objects in the order they were first defined; redefining a name
replaces its rules in place, so a re-evaluated group keeps its position.")

(defun ensure-style (name documentation rules)
  "Define or redefine the style NAME; the first definition fixes its order."
  (let ((existing (find name *styles* :key #'style-name)))
    (cond (existing
           (setf (style-documentation existing) documentation
                 (style-rules existing) rules)
           existing)
          (t
           (let ((style (make-instance 'style :name name :documentation documentation :rules rules)))
             (setf *styles* (append *styles* (list style)))
             style)))))

(defmacro define-style (name &body body)
  "Define the style group NAME: an optional docstring, then rules.  The
docstring becomes a comment in the compiled sheet."
  (let ((documentation (and (stringp (first body)) (pop body))))
    `(ensure-style ',name ,documentation ',body)))

;;; Values

(defparameter *css-functions*
  '("var" "calc" "clamp" "min" "max" "minmax" "repeat" "color-mix" "url"
    "rgb" "rgba" "hsl" "hsla" "translate" "rotate" "scale" "attr" "counter"
    "linear-gradient" "fit-content")
  "Names that make a list a function call rather than a run of values.")

(defun css-name (symbol)
  "The CSS spelling of SYMBOL: its name in lowercase."
  (string-downcase (symbol-name symbol)))

(defun css-value (value)
  "The CSS text of VALUE."
  (etypecase value
    (string value)
    (integer (princ-to-string value))
    (float (let ((*read-default-float-format* (type-of value)))
             (princ-to-string value)))
    (ratio (css-value (float value 1.0)))
    (symbol (let ((name (css-name value)))
              (if (starts-with "--" name)
                  (format nil "var(~A)" name)
                  name)))
    (cons (let ((head (first value)))
            (if (and (symbolp head) (member (css-name head) *css-functions* :test #'string=))
                (format nil "~A(~{~A~^, ~})" (css-name head) (mapcar #'css-value (rest value)))
                (format nil "~{~A~^ ~}" (mapcar #'css-value value)))))))

;;; Selectors

(defun split-selectors (selector)
  "The comma-separated selectors of SELECTOR, trimmed."
  (mapcar (lambda (s) (string-trim " " s))
          (uiop:split-string selector :separator ",")))

(defun combine-selectors (parent child)
  "CHILD nested under PARENT: each child selector against each parent
selector, & standing for the parent, otherwise a descendant."
  (if (null parent)
      child
      (format nil "~{~A~^, ~}"
              (loop for p in (split-selectors parent)
                    append (loop for c in (split-selectors child)
                                 collect (if (search "&" c)
                                             (replace-all c "&" p)
                                             (format nil "~A ~A" p c)))))))

(defun replace-all (string part replacement)
  (with-output-to-string (out)
    (loop with start = 0
          for position = (search part string :start2 start)
          do (write-string string out :start start :end (or position (length string)))
          while position
          do (write-string replacement out)
             (setf start (+ position (length part))))))

;;; Compilation

(defun rule-p (item)
  "A nested rule: a list whose first element is a selector string."
  (and (consp item) (stringp (first item))))

(defun media-p (item)
  (and (consp item) (eq (first item) :media)))

(defun write-declarations (declarations stream)
  (loop for (property value) on declarations by #'cddr
        do (format stream "  ~A: ~A;~%" (css-name property) (css-value value))))

(defun write-rule (selector body stream &key media)
  "Write the rule SELECTOR with BODY, then its nested rules; media queries
inside a rule are hoisted around the rule."
  (let ((declarations '())
        (nested '()))
    (loop while body
          do (let ((item (pop body)))
               (cond ((or (rule-p item) (media-p item)) (push item nested))
                     ((keywordp item) (push item declarations) (push (pop body) declarations))
                     (t (error "Not a declaration or rule in ~S: ~S" selector item)))))
    (setf declarations (nreverse declarations)
          nested (nreverse nested))
    (when declarations
      (format stream "~A {~%" selector)
      (write-declarations declarations stream)
      (format stream "}~%~%"))
    (dolist (item nested)
      (if (media-p item)
          (write-media (second item) (cddr item) stream :parent selector)
          (write-rule (combine-selectors selector (first item)) (rest item) stream :media media)))))

(defun write-media (query rules stream &key parent)
  (format stream "@media ~A {~%" query)
  (let ((inner (with-output-to-string (s)
                 (dolist (rule rules)
                   (write-rule (combine-selectors parent (first rule)) (rest rule) s :media query)))))
    ;; Indent the wrapped rules by two spaces.
    (dolist (line (uiop:split-string (string-right-trim '(#\Newline) inner) :separator '(#\Newline)))
      (if (string= line "")
          (terpri stream)
          (format stream "  ~A~%" line))))
  (format stream "}~%~%"))

(defun write-css (rules stream)
  "Write RULES, top-level rules and media queries, as CSS to STREAM."
  (dolist (rule rules)
    (cond ((media-p rule) (write-media (second rule) (cddr rule) stream))
          ((rule-p rule) (write-rule (first rule) (rest rule) stream))
          (t (error "Not a rule: ~S" rule)))))

(defun css (&rest rules)
  "The CSS text of RULES."
  (with-output-to-string (out) (write-css rules out)))

(defun stylesheet-text (&optional (styles *styles*))
  "The whole stylesheet: every style group in order, each under its
docstring as a comment."
  (with-output-to-string (out)
    (dolist (style styles)
      (when (style-documentation style)
        (format out "/* ~A */~%~%" (style-documentation style)))
      (write-css (style-rules style) out))))
