(in-package #:luv.application-agent)

;;; A command argument is semantic definition metadata captured from McCLIM's
;;; parser once.  It can drive provider JSON today and parameter prompting in
;;; M-x later without coupling either interface to the other.

(defclass command-argument-specification ()
  ((name :initarg :name :reader command-argument-name)
   (json-name :initarg :json-name :reader command-argument-json-name)
   (type :initarg :type :reader command-argument-type)
   (options :initarg :options :reader command-argument-options)
   (required-p :initarg :required-p :reader command-argument-required-p)
   (keyword-p :initarg :keyword-p :reader command-argument-keyword-p)))

(defun argument-json-name (argument-name)
  (string-downcase (symbol-name argument-name)))

(defun command-argument-active-p (options)
  (let ((when-form (getf options :when t)))
    (or (not (constantp when-form))
        (eval when-form))))

(defun make-command-argument-specification (description required-p keyword-p)
  (destructuring-bind (name type-form &rest options) description
    (make-instance 'command-argument-specification
                   :name name
                   :json-name (argument-json-name name)
                   :type (eval type-form)
                   :options options
                   :required-p required-p
                   :keyword-p keyword-p)))

(defun command-argument-specifications (command)
  "Return COMMAND's active required and named argument definitions."
  (let ((parsers (gethash command climi::*command-parser-table*)))
    (unless parsers
      (error "~S is not a CLIM command." command))
    (append
     (mapcar (lambda (description)
               (make-command-argument-specification description t nil))
             (climi::required-args parsers))
     (loop for description in (climi::keyword-args parsers)
           for options = (cddr description)
           when (command-argument-active-p options)
             collect
             (make-command-argument-specification
              description
              (not (member :default options))
              t)))))

(defun schema-with-argument-documentation (argument)
  (let* ((options (command-argument-options argument))
         (schema
           (presentation-type-specifier-json-schema
            (command-argument-type argument)))
         (documentation
           (or (getf options :documentation) (getf options :prompt))))
    (if (and documentation
             (not (assoc "description" schema :test #'string=)))
        (append schema `(("description" . ,documentation)))
        schema)))

(defun command-tool-parameters (arguments)
  "Return a strict JSON Schema object for ARGUMENT specifications."
  (let ((properties
          (mapcar (lambda (argument)
                    (cons (command-argument-json-name argument)
                          (schema-with-argument-documentation argument)))
                  arguments))
        (required
          (loop for argument in arguments
                when (command-argument-required-p argument)
                  collect (command-argument-json-name argument))))
    `(("type" . "object")
      ("properties" . ,(or properties (make-hash-table :test #'equal)))
      ("required" . ,(coerce required 'vector))
      ("additionalProperties" . ,openai:+json-false+))))

(defun command-tool-name (command)
  (let ((name (string-downcase (symbol-name command))))
    (if (and (> (length name) 4) (string= "com-" name :end2 4))
        (subseq name 4)
        name)))

(defun command-documentation (command)
  (or (ignore-errors (documentation command 'function))
      (command-tool-name command)))

(defclass command-tool (openai:tool)
  ((command :initarg :command :reader command-tool-command)
   (arguments :initarg :arguments
              :reader command-tool-argument-specifications)))

(defun make-command-tool (command &key description)
  (let ((arguments (command-argument-specifications command)))
    (make-instance 'command-tool
                   :command command :arguments arguments
                   :name (command-tool-name command)
                   :description (or description (command-documentation command))
                   :parameters (command-tool-parameters arguments))))

(define-condition tool-argument-error (error)
  ((name :initarg :name :reader tool-argument-error-name)
   (detail :initarg :detail :reader tool-argument-error-detail))
  (:report (lambda (condition stream)
             (format stream "argument ~A: ~A"
                     (tool-argument-error-name condition)
                     (tool-argument-error-detail condition)))))

(defun json-argument-cell (argument arguments)
  (assoc (command-argument-json-name argument) arguments
         :test #'string-equal))

(defun accept-command-argument (argument arguments)
  "Parse ARGUMENT from a decoded JSON alist with its presentation type."
  (let* ((json-name (command-argument-json-name argument))
         (cell (json-argument-cell argument arguments)))
    (unless cell
      (when (command-argument-required-p argument)
        (error 'tool-argument-error :name json-name :detail "missing"))
      (return-from accept-command-argument (values nil nil)))
    (let* ((value (cdr cell))
           (text (if (stringp value) value (princ-to-string value))))
      (handler-case
          (values (accept-from-string (command-argument-type argument) text) t)
        (error (condition)
          (error 'tool-argument-error
                 :name json-name
                 :detail (format nil "~S: ~A" text condition)))))))

(defun validate-command-tool-arguments (tool arguments)
  (unless (listp arguments)
    (error 'tool-argument-error :name "arguments"
           :detail "expected a JSON object"))
  (let ((known
          (mapcar #'command-argument-json-name
                  (command-tool-argument-specifications tool)))
        (seen (make-hash-table :test #'equalp)))
    (dolist (cell arguments)
      (unless (consp cell)
        (error 'tool-argument-error :name "arguments"
               :detail "expected name/value properties"))
      (let ((name (string-downcase (string (car cell)))))
        (unless (member name known :test #'string=)
          (error 'tool-argument-error :name name :detail "unknown"))
        (when (gethash name seen)
          (error 'tool-argument-error :name name :detail "duplicated"))
        (setf (gethash name seen) t))))
  arguments)

(defun command-tool-parse (tool arguments)
  "Return a validated CLIM command form for decoded JSON ARGUMENTS."
  (validate-command-tool-arguments tool arguments)
  (let ((required-values '())
        (keyword-values '()))
    (dolist (argument (command-tool-argument-specifications tool))
      (multiple-value-bind (value present-p)
          (accept-command-argument argument arguments)
        (when present-p
          (if (command-argument-keyword-p argument)
              (setf keyword-values
                    (nconc keyword-values
                           (list (intern (symbol-name
                                          (command-argument-name argument))
                                         :keyword)
                                 value)))
              (push value required-values)))))
    (list* (command-tool-command tool)
           (nconc (nreverse required-values) keyword-values))))

;;; The application side is deliberately just these two methods.  There is no
;;; unsafe default for a command that claims it needs the canvas thread.

(defgeneric application-command-frame (application)
  (:documentation "Return APPLICATION's McCLIM command frame."))

(defgeneric call-in-application-frame (application function)
  (:documentation
   "Run no-argument FUNCTION briefly at APPLICATION's frame boundary."))

(defgeneric call-with-agent-command-context (agent function)
  (:documentation "Call FUNCTION with AGENT's dynamic command context."))

(defmethod call-with-agent-command-context
    ((agent application-agent) function)
  (let ((*current-agent* agent)
        (*handles* (application-agent-handles agent)))
    (funcall function)))

(defgeneric command-tool-runs-on-canvas-p (command)
  (:documentation "Whether COMMAND must execute at its application's frame boundary.")
  (:method ((command t)) t))

(defgeneric command-result-presentation-type (command value)
  (:documentation "The presentation type used to show COMMAND's VALUE.")
  (:method ((command t) value)
    (declare (ignore command))
    (presentation-type-of value)))

(defgeneric command-output-line-limit (command)
  (:documentation "The provider line limit for COMMAND's presented output.")
  (:method ((command t)) *model-text-max-lines*))

(defgeneric command-provider-output (command values text)
  (:documentation "Return the provider result after VALUES produced TEXT.")
  (:method ((command t) values text)
    (declare (ignore command values))
    text))

(defgeneric settle-command-result (command value)
  (:documentation
   "Finish VALUE's off-canvas work on the provider worker before presentation.")
  (:method ((command t) value)
    (declare (ignore command))
    value))

(defun execute-command-for-agent (agent command)
  (let* ((application (application-agent-application agent))
         (run
           (lambda ()
             (call-with-agent-command-context
              agent
              (lambda ()
                (multiple-value-list
                 (execute-frame-command
                  (application-command-frame application) command)))))))
    (if (command-tool-runs-on-canvas-p (first command))
        (call-in-application-frame application run)
        (funcall run))))

(defmethod openai:call-tool
    ((tool command-tool) arguments (agent application-agent))
  (let ((call (make-instance 'tool-call :tool tool :arguments arguments))
        (provider-output nil)
        (*handles* (application-agent-handles agent)))
    (note-tool-call agent call)
    (handler-case
        (let* ((command (command-tool-parse tool arguments))
               (raw-values
                 (progn
                   (setf (tool-call-command call) command)
                   (execute-command-for-agent agent command)))
               (values
                 (mapcar (lambda (value)
                           (settle-command-result (first command) value))
                         raw-values))
               (*model-text-max-lines*
                 (command-output-line-limit (first command)))
               (output
                 (format nil "~{~A~^~%~}"
                         (mapcar
                          (lambda (value)
                            (model-text
                             value
                             :type (command-result-presentation-type
                                    (first command) value)))
                          values))))
          (let ((text (if (string= output "") "ok" output)))
            (setf (tool-call-result call) (first values)
                  (tool-call-output call) text
                  (tool-call-status call) :ok
                  provider-output
                  (command-provider-output (first command) values text))))
      (error (condition)
        (setf (tool-call-error call) condition
              (tool-call-output call) (format nil "error: ~A" condition)
              (tool-call-status call) :error)))
    (setf (tool-call-finished call) (get-internal-real-time))
    (note-tool-call-finished agent call)
    (or provider-output (tool-call-output call))))

(defun make-application-agent
    (&key application model commands instructions reasoning-summary
       reasoning-effort (api-key (openai:default-api-key))
       (class 'application-agent) initargs)
  "Open a provider-backed CLASS attached to APPLICATION and COMMANDS."
  (unless application
    (error ":APPLICATION is required."))
  (openai:make-agent
   :class class
   :initargs (append (list :application application) initargs)
   :model model :instructions instructions
   :reasoning-summary reasoning-summary :reasoning-effort reasoning-effort
   :api-key api-key :tools (mapcar #'make-command-tool commands)))
