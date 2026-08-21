(in-package #:luvcraft.agent)

;;; A tool is a CLIM command (#0IF4TY).
;;;
;;; The command's parser table already says what its arguments are called and
;;; what presentation type each one has.  That is enough to write the JSON
;;; schema the provider wants, and enough to parse what the model sends back
;;; with ACCEPT-FROM-STRING, so a wrong argument is a presentation parse error
;;; the model can read rather than a crash.  The command then runs where
;;; commands run -- on the canvas thread -- and its value is presented under
;;; the model view (#WS5OEX).

(defclass command-tool (openai:tool)
  ((command :initarg :command :reader command-tool-command
            :documentation "The CLIM command name this tool runs.")
   (arguments :initarg :arguments :reader command-tool-argument-specs
              :documentation
              "The command's required argument specs, (NAME TYPE . OPTIONS)."))
  (:documentation "An OpenAI function tool standing for one CLIM command."))

(defun command-argument-specs (name)
  "The required argument specs of CLIM command NAME, with types evaluated."
  (let ((parsers (gethash name climi::*command-parser-table*)))
    (unless parsers
      (error "~S is not a CLIM command." name))
    (loop for (argument-name type-form . options) in (climi::required-args parsers)
          collect (list* argument-name (eval type-form) options))))

(defun command-tool-name (command)
  "place-block-at for COM-PLACE-BLOCK-AT: the command's name without its prefix."
  (let ((name (string-downcase (symbol-name command))))
    (if (and (> (length name) 4) (string= "com-" name :end2 4))
        (subseq name 4)
        name)))

(defun command-argument-json-name (argument-name)
  (string-downcase (symbol-name argument-name)))

(defun command-tool-parameters (specs)
  "A JSON Schema object for SPECS, every argument required, none extra."
  (let ((properties
          (loop for (argument-name type . options) in specs
                collect
                (cons (command-argument-json-name argument-name)
                      (let ((schema (presentation-type-specifier-json-schema type))
                            (documentation (or (getf options :documentation)
                                               (getf options :prompt))))
                        (if (and documentation
                                 (not (assoc "description" schema :test #'string=)))
                            (append schema `(("description" . ,documentation)))
                            schema))))))
    `(("type" . "object")
      ("properties" . ,(or properties (make-hash-table)))
      ("required" . ,(coerce (mapcar #'car properties) 'vector))
      ("additionalProperties" . ,openai:+json-false+))))

(defun command-documentation (command)
  (or (ignore-errors (documentation command 'function))
      (let ((table 'luvcraft-agent))
        (command-line-name-for-command command table :errorp nil))
      (command-tool-name command)))

(defun make-command-tool (command &key description)
  "Make the tool standing for CLIM command COMMAND."
  (let ((specs (command-argument-specs command)))
    (make-instance 'command-tool
                   :command command
                   :arguments specs
                   :name (command-tool-name command)
                   :description (or description (command-documentation command))
                   :parameters (command-tool-parameters specs))))

;;; ---------------------------------------------------------------------
;;; Running one

(defgeneric command-tool-runs-on-canvas-p (command)
  (:documentation
   "Whether COMMAND must run on the canvas thread.

Commands that touch the world or the session do, like every other command;
an evaluator that may take its time runs on the turn's own thread instead,
the way ./sly eval already does.")
  (:method ((command t)) t))

(defgeneric command-result-presentation-type (command value)
  (:documentation
   "The presentation type COMMAND's VALUE is shown to the model under.

By default what PRESENTATION-TYPE-OF says; an evaluator answers EXPRESSION
so a list reads as Lisp rather than as a comma-separated sequence.")
  (:method ((command t) value) (presentation-type-of value)))

(defgeneric command-output-line-limit (command)
  (:documentation
   "How many lines COMMAND's result may run to before the model view folds it.")
  (:method ((command t)) *model-text-max-lines*))

(defgeneric command-provider-output (command values text)
  (:documentation
   "Return the provider value for COMMAND after VALUES have produced TEXT.

The default is the same textual transcript shown in the HUD.  An observational
command may wrap that text with image content without changing the retained
semantic result or its presentation.")
  (:method ((command t) values text)
    (declare (ignore command values))
    text))

(defgeneric settle-command-result (command value)
  (:documentation
   "Finish any off-canvas work represented by VALUE before presenting it.

The command itself has already returned from the canvas thread.  The default
is immediate; a long-lived action can wait here on the provider's turn thread
without ever freezing rendering.")
  (:method ((command t) value)
    (declare (ignore command))
    value))

(define-condition tool-argument-error (error)
  ((name :initarg :name :reader tool-argument-error-name)
   (detail :initarg :detail :reader tool-argument-error-detail))
  (:report (lambda (condition stream)
             (format stream "argument ~A: ~A"
                     (tool-argument-error-name condition)
                     (tool-argument-error-detail condition)))))

(defun command-json-key-token (name)
  "Canonicalize one provider-decoded object key for command lookup.

The JSON schema spells compact coordinate names X1 and Y2.  Jonathan decodes
those keys as keywords such as :X-1 and :Y-2; underscores can receive the same
word-boundary treatment.  Removing those separators at this one JSON boundary
restores the schema identity without changing CLIM argument names."
  (remove-if (lambda (character) (find character "-_"))
             (string-downcase (string name))))

(defun command-json-key= (left right)
  (string= (command-json-key-token left)
           (command-json-key-token right)))

(defun accept-command-argument (spec arguments)
  "Parse the JSON value for SPEC out of ARGUMENTS with its presentation type."
  (destructuring-bind (argument-name type &rest options) spec
    (declare (ignore options))
    (let* ((json-name (command-argument-json-name argument-name))
           (cell (assoc json-name arguments :test #'command-json-key=))
           (value (cdr cell)))
      (unless cell
        (error 'tool-argument-error :name json-name :detail "missing"))
      (let ((text (if (stringp value) value (princ-to-string value))))
        (handler-case (values (accept-from-string type text))
          (error (condition)
            (error 'tool-argument-error
                   :name json-name
                   :detail (format nil "~S: ~A" text condition))))))))

(defun command-tool-parse (tool arguments)
  "The CLIM command, (NAME . ARGUMENTS), for the model's JSON ARGUMENTS."
  (cons (command-tool-command tool)
        (mapcar (lambda (spec) (accept-command-argument spec arguments))
                (command-tool-argument-specs tool))))

(defun execute-command-for-agent (agent command)
  "Run COMMAND for AGENT where it must run, returning its values as a list."
  (let* ((session (world-agent-session agent))
         (frame (luvcraft.clim::luvcraft-session-frame session))
         (run (lambda ()
                (let ((*handles* (world-agent-handles agent))
                      (*current-agent* agent))
                  (multiple-value-list (execute-frame-command frame command))))))
    (if (command-tool-runs-on-canvas-p (car command))
        (luv:request-canvas-frame
         (luvcraft:luvcraft-session-canvas session)
         (lambda (timestamp) (declare (ignore timestamp)) (funcall run)))
        (funcall run))))

(defmethod openai:call-tool ((tool command-tool) arguments (agent world-agent))
  (let ((call (make-instance 'tool-call :tool tool :arguments arguments))
        (provider-output nil)
        (*handles* (world-agent-handles agent)))
    (note-tool-call agent call)
    (handler-case
        (let* ((command (command-tool-parse tool arguments))
               (raw-values (progn
                             (setf (tool-call-command call) command)
                             (execute-command-for-agent agent command)))
               (values (mapcar (lambda (value)
                                 (settle-command-result (car command) value))
                               raw-values))
               (*model-text-max-lines* (command-output-line-limit (car command)))
               (output (format nil "~{~A~^~%~}"
                               (mapcar (lambda (value)
                                         (model-text
                                          value
                                          :type (command-result-presentation-type
                                                 (car command) value)))
                                       values))))
          (let ((text (if (string= output "") "ok" output)))
            (setf (tool-call-result call) (first values)
                  (tool-call-output call) text
                  (tool-call-status call) :ok
                  provider-output
                  (command-provider-output (car command) values text))))
      (error (condition)
        (setf (tool-call-error call) condition
              (tool-call-output call) (format nil "error: ~A" condition)
              (tool-call-status call) :error)))
    (setf (tool-call-finished call) (get-internal-real-time))
    (note-tool-call-finished agent call)
    (or provider-output (tool-call-output call))))
