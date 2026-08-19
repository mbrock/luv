(in-package #:luvcraft.agent)

;;; Views, handles, and schemas: the vocabulary the model and the game share.
;;;
;;; A tool result is an object, and the model reads it the way anyone reads a
;;; CLIM object -- presented under a view.  The model view is compact text;
;;; the trace view is a preview for remembering an old turn; the cassette view
;;; is the card the HUD draws (#WS5OEX, #9K823O).  A handle is how the model
;;; names an object it was shown, so it can hand it back (#1W8K03).

;;; ---------------------------------------------------------------------
;;; Views

(defclass model-view (textual-view) ()
  (:documentation "The view a language model reads tool results under."))

(defclass trace-view (textual-view) ()
  (:documentation "A short preview of an old result, for remembering it."))

(defclass cassette-view (textual-view) ()
  (:documentation "The card a tool call is drawn as on the HUD."))

(defparameter +model-view+ (make-instance 'model-view))
(defparameter +trace-view+ (make-instance 'trace-view))
(defparameter +cassette-view+ (make-instance 'cassette-view))

;;; ---------------------------------------------------------------------
;;; Handles
;;;
;;; Four characters from an alphabet without look-alikes, minted in order so
;;; the model can see they are short names and not data.  One table per
;;; agent; *HANDLES* is bound while its turn runs so ACCEPT can resolve them.

(defparameter +handle-alphabet+ "ABCDEFGHJKLMNPQRSTUVWXYZ23456789")

(defclass handle-table ()
  ((objects :initform (make-hash-table :test #'equal) :reader handle-table-objects)
   (ids :initform (make-hash-table :test #'eq) :reader handle-table-ids)
   (counter :initform 0 :accessor handle-table-counter)
   (lock :initform (sb-thread:make-mutex :name "agent handles")
         :reader handle-table-lock)))

(defun make-handle-table ()
  (make-instance 'handle-table))

(defvar *handles* nil
  "The HANDLE-TABLE of the agent whose turn is running, or NIL.")

(defun handle-id-string (n)
  (let ((base (length +handle-alphabet+)))
    (coerce (loop repeat 4
                  collect (char +handle-alphabet+ (mod n base))
                  do (setf n (floor n base)))
            'string)))

(defun intern-handle (object &optional (table *handles*))
  "Return OBJECT's short id in TABLE, minting one on first mention."
  (unless table (error "No handle table is current."))
  (sb-thread:with-mutex ((handle-table-lock table))
    (or (gethash object (handle-table-ids table))
        (let ((id (handle-id-string
                   ;; Start past the single-digit range so the first handle
                   ;; already looks like one.
                   (+ 1024 (incf (handle-table-counter table))))))
          (setf (gethash id (handle-table-objects table)) object
                (gethash object (handle-table-ids table)) id)
          id))))

(defun handle-object (id &optional (table *handles*))
  "The object ID names in TABLE, or NIL.  A leading # is allowed."
  (when table
    (let ((id (string-upcase (string-left-trim "#" id))))
      (values (gethash id (handle-table-objects table))))))

(define-presentation-type handle ()
  :description "a short #ABCD handle naming something the agent was shown")

(define-presentation-method accept
    ((type handle) stream (view textual-view) &key)
  (let* ((string (read-token stream))
         (object (handle-object string)))
    (if (and (plusp (length string)) object)
        object
        (simple-parse-error "~S is not a handle I have shown you." string))))

(define-presentation-method present
    (object (type handle) stream (view textual-view) &key)
  (format stream "#~A" (intern-handle object)))

;;; ---------------------------------------------------------------------
;;; Block kinds are already CLOS objects; the class is the presentation type.

(define-presentation-method accept
    ((type luvcraft:block-kind) stream (view textual-view) &key)
  (let* ((string (read-token stream))
         (kind (luvcraft:block-kind-named
                (intern (string-upcase string) :keyword) nil)))
    (or kind
        (simple-parse-error "~S is not a block kind; try one of ~{~(~A~)~^, ~}."
                            string (placeable-block-kind-names)))))

(define-presentation-method present
    (object (type luvcraft:block-kind) stream (view textual-view) &key)
  (format stream "~(~A~)" (luvcraft:block-kind-name object)))

(defun placeable-block-kind-names ()
  (loop for kind in luvcraft::*block-kinds*
        when (luvcraft::block-kind-placeable-p kind)
          collect (luvcraft:block-kind-name kind)))

;;; ---------------------------------------------------------------------
;;; JSON schema from presentation types
;;;
;;; A command's argument types are the tool's schema.  Each method answers
;;; for one type name; the parameters of a parameterized type arrive as the
;;; rest arguments.  The default is a string, which ACCEPT will still check.

(defgeneric presentation-type-json-schema (name &rest parameters)
  (:documentation
   "The JSON Schema (a CL-JSON alist) for presentation type NAME with PARAMETERS.")
  (:method ((name t) &rest parameters)
    (declare (ignore parameters))
    '(("type" . "string"))))

(defmethod presentation-type-json-schema ((name (eql 'integer)) &rest parameters)
  (declare (ignore parameters))
  '(("type" . "integer")))

(defmethod presentation-type-json-schema ((name (eql 'real)) &rest parameters)
  (declare (ignore parameters))
  '(("type" . "number")))

(defmethod presentation-type-json-schema ((name (eql 'number)) &rest parameters)
  (declare (ignore parameters))
  '(("type" . "number")))

(defmethod presentation-type-json-schema ((name (eql 'float)) &rest parameters)
  (declare (ignore parameters))
  '(("type" . "number")))

(defmethod presentation-type-json-schema ((name (eql 'boolean)) &rest parameters)
  (declare (ignore parameters))
  '(("type" . "boolean")))

(defmethod presentation-type-json-schema ((name (eql 'member)) &rest values)
  `(("type" . "string")
    ("enum" . ,(mapcar (lambda (value) (string-downcase (princ-to-string value)))
                       values))))

(defmethod presentation-type-json-schema ((name (eql 'completion)) &rest parameters)
  (destructuring-bind (sequence &key (value-key 'identity) &allow-other-keys)
      parameters
    (declare (ignore value-key))
    `(("type" . "string")
      ("enum" . ,(mapcar (lambda (value) (string-downcase (princ-to-string value)))
                         sequence)))))

(defmethod presentation-type-json-schema
    ((name (eql 'luvcraft:block-kind)) &rest parameters)
  (declare (ignore parameters))
  `(("type" . "string")
    ("enum" . ,(mapcar (lambda (name) (string-downcase (symbol-name name)))
                       (placeable-block-kind-names)))))

(defmethod presentation-type-json-schema ((name (eql 'handle)) &rest parameters)
  (declare (ignore parameters))
  '(("type" . "string")
    ("pattern" . "^#?[A-Z2-9]{4}$")
    ("description" . "a #ABCD handle from an earlier result")))

(defun presentation-type-specifier-json-schema (specifier)
  "Schema for a full type specifier: a symbol, or (NAME . PARAMETERS)."
  (let ((specifier (expand-presentation-type-abbreviation specifier)))
    (if (consp specifier)
        (apply #'presentation-type-json-schema (car specifier) (cdr specifier))
        (presentation-type-json-schema specifier))))

;;; ---------------------------------------------------------------------
;;; Text for the model

(defparameter *model-text-max-lines* 60
  "Lines a result may run to before the model view folds it.")
(defparameter *model-text-head-lines* 30)
(defparameter *model-text-tail-lines* 10)

(defun fold-model-text (text)
  "Keep the head and tail of a long TEXT, naming the rest by handle.

Folding happens here and nowhere else: tool bodies never truncate, and the
model is told what was left out and how to get it."
  (let* ((lines (uiop:split-string text :separator '(#\Newline)))
         (count (length lines)))
    (if (or (<= count *model-text-max-lines*) (null *handles*))
        text
        (let ((omitted (- count *model-text-head-lines* *model-text-tail-lines*)))
          (format nil "~{~A~%~}[~D of ~D lines omitted; describe-handle #~A reads the whole text]~%~{~A~^~%~}"
                  (subseq lines 0 *model-text-head-lines*)
                  omitted count (intern-handle text)
                  (last lines *model-text-tail-lines*))))))

(defun handle-worthy-p (object)
  "Whether OBJECT is the kind of thing the model may want to come back to.

Instances and structures are; numbers, strings, symbols and lists are their
own spelling and need no name."
  (typep object '(or standard-object structure-object)))

(defun model-text (object &key (type (presentation-type-of object))
                            (view +model-view+))
  "Present OBJECT under the model VIEW and fold the result.

An object worth coming back to is followed by its handle, so the model can
name it to DESCRIBE-HANDLE or any command that accepts one."
  (let ((text (handler-case (present-to-string object type :view view)
                (error (condition)
                  (format nil "~A [unpresentable as ~S: ~A]"
                          (prin1-to-string object) type condition)))))
    (when (and *handles* (handle-worthy-p object))
      (setf text (format nil "~A #~A" text (intern-handle object))))
    (fold-model-text text)))
