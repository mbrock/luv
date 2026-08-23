(in-package #:luv.application-agent)

;;; A tool result remains a semantic object.  These views say how a model,
;;; trace, or retained developer surface reads it; no provider JSON leaks into
;;; the application command itself.

(defclass model-view (textual-view) ()
  (:documentation "The compact textual view a language model reads."))

(defclass trace-view (textual-view) ()
  (:documentation "A short view suitable for an old-turn trace."))

(defclass cassette-view (textual-view) ()
  (:documentation "The rich retained view used by an agent transcript UI."))

(defparameter +model-view+ (make-instance 'model-view))
(defparameter +trace-view+ (make-instance 'trace-view))
(defparameter +cassette-view+ (make-instance 'cassette-view))

;;; Handles are per-agent capabilities.  Releasing their table atomically
;;; revokes every capability and prevents an in-flight turn from retaining
;;; fresh application objects after its owner has stopped.

(defparameter +handle-alphabet+ "ABCDEFGHJKLMNPQRSTUVWXYZ23456789")

(define-condition handle-table-released (error)
  ((table :initarg :table :reader released-handle-table))
  (:report (lambda (condition stream)
             (format stream "The application-agent handle table ~S is released."
                     (released-handle-table condition)))))

(defclass handle-table ()
  ((objects :initform (make-hash-table :test #'equal)
            :reader handle-table-objects)
   (ids :initform (make-hash-table :test #'eq)
        :reader handle-table-ids)
   (counter :initform 0 :accessor handle-table-counter)
   (released-p :initform nil :accessor handle-table-released-p)
   (lock :initform (sb-thread:make-mutex :name "application agent handles")
         :reader handle-table-lock)))

(defun make-handle-table ()
  (make-instance 'handle-table))

(defvar *handles* nil
  "The HANDLE-TABLE dynamically current while an agent command is handled.")

(defun handle-id-string (number)
  (let ((base (length +handle-alphabet+)))
    (coerce (loop repeat 4
                  collect (char +handle-alphabet+ (mod number base))
                  do (setf number (floor number base)))
            'string)))

(defun intern-handle (object &optional (table *handles*))
  "Return OBJECT's short id in TABLE, minting one on first mention."
  (unless table
    (error "No application-agent handle table is current."))
  (sb-thread:with-mutex ((handle-table-lock table))
    (when (handle-table-released-p table)
      (error 'handle-table-released :table table))
    (or (gethash object (handle-table-ids table))
        (let ((id (handle-id-string
                   ;; Begin beyond the one-character range so the first id
                   ;; already reads as an opaque capability rather than data.
                   (+ 1024 (incf (handle-table-counter table))))))
          (setf (gethash id (handle-table-objects table)) object
                (gethash object (handle-table-ids table)) id)
          id))))

(defun handle-object (id &optional (table *handles*))
  "Return the object ID names in TABLE and whether it was present.

A leading # is accepted.  Released or absent tables resolve no capability."
  (when table
    (let ((id (string-upcase (string-left-trim "#" id))))
      (sb-thread:with-mutex ((handle-table-lock table))
        (unless (handle-table-released-p table)
          (gethash id (handle-table-objects table)))))))

(defun release-handle-table (table)
  "Revoke every handle in TABLE once, returning true for the releasing call."
  (sb-thread:with-mutex ((handle-table-lock table))
    (unless (handle-table-released-p table)
      (setf (handle-table-released-p table) t)
      (clrhash (handle-table-objects table))
      (clrhash (handle-table-ids table))
      t)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-presentation-type handle ()
    :description "a short #ABCD handle naming something the agent was shown"))

(define-presentation-method accept
    ((type handle) stream (view textual-view) &key)
  (let ((string (read-token stream)))
    (multiple-value-bind (object present-p) (handle-object string)
      (if (and (plusp (length string)) present-p)
          object
          (simple-parse-error
           "~S is not a live handle I have shown you." string)))))

(define-presentation-method present
    (object (type handle) stream (view textual-view) &key)
  (format stream "#~A" (intern-handle object)))

;;; JSON Schema is an open protocol over CLIM presentation-type names.  A new
;;; application domain contributes one EQL method; the command-tool code never
;;; needs a typecase or an application dependency.

(defgeneric presentation-type-json-schema (name &rest parameters)
  (:documentation
   "Return NAME with PARAMETERS as a CL-JSON alist containing JSON Schema.")
  (:method ((name t) &rest parameters)
    (declare (ignore parameters))
    '(("type" . "string"))))

(defmethod presentation-type-json-schema
    ((name (eql 'integer)) &rest parameters)
  (declare (ignore parameters))
  '(("type" . "integer")))

(defmethod presentation-type-json-schema
    ((name (eql 'real)) &rest parameters)
  (declare (ignore parameters))
  '(("type" . "number")))

(defmethod presentation-type-json-schema
    ((name (eql 'number)) &rest parameters)
  (declare (ignore parameters))
  '(("type" . "number")))

(defmethod presentation-type-json-schema
    ((name (eql 'float)) &rest parameters)
  (declare (ignore parameters))
  '(("type" . "number")))

(defmethod presentation-type-json-schema
    ((name (eql 'boolean)) &rest parameters)
  (declare (ignore parameters))
  '(("type" . "boolean")))

(defmethod presentation-type-json-schema
    ((name (eql 'member)) &rest values)
  `(("type" . "string")
    ("enum" . ,(mapcar (lambda (value)
                          (string-downcase (princ-to-string value)))
                        values))))

(defmethod presentation-type-json-schema
    ((name (eql 'completion)) &rest parameters)
  (destructuring-bind (sequence &key (value-key 'identity) &allow-other-keys)
      parameters
    `(("type" . "string")
      ("enum" . ,(mapcar (lambda (value)
                            (string-downcase
                             (princ-to-string (funcall value-key value))))
                          sequence)))))

(defmethod presentation-type-json-schema
    ((name (eql 'handle)) &rest parameters)
  (declare (ignore parameters))
  '(("type" . "string")
    ("pattern" . "^#?[A-Z2-9]{4}$")
    ("description" . "a #ABCD handle from an earlier result")))

(defun presentation-type-specifier-json-schema (specifier)
  "Return a schema for a symbol or parameterized presentation SPECIFIER."
  (let ((specifier (expand-presentation-type-abbreviation specifier)))
    (if (consp specifier)
        (apply #'presentation-type-json-schema
               (first specifier) (rest specifier))
        (presentation-type-json-schema specifier))))

;;; Text for the provider.  Folding is a presentation policy, never a command
;;; implementation policy, and the full object remains reachable by handle.

(defparameter *model-text-max-lines* 60)
(defparameter *model-text-head-lines* 30)
(defparameter *model-text-tail-lines* 10)

(defun fold-model-text (text)
  (let* ((lines (uiop:split-string text :separator '(#\Newline)))
         (count (length lines)))
    (if (or (<= count *model-text-max-lines*) (null *handles*))
        text
        (let ((omitted
                (- count *model-text-head-lines* *model-text-tail-lines*)))
          (format nil
                  "~{~A~%~}[~D of ~D lines omitted; #~A names the full text]~%~{~A~^~%~}"
                  (subseq lines 0 *model-text-head-lines*)
                  omitted count (intern-handle text)
                  (last lines *model-text-tail-lines*))))))

(defun handle-worthy-p (object)
  (typep object '(or standard-object structure-object)))

(defun model-text (object &key (type (presentation-type-of object))
                            (view +model-view+))
  "Present OBJECT under VIEW, append a capability handle, and fold long text."
  (let ((text
          (handler-case (present-to-string object type :view view)
            (error (condition)
              (format nil "~A [unpresentable as ~S: ~A]"
                      (prin1-to-string object) type condition)))))
    (when (and *handles* (handle-worthy-p object))
      (setf text (format nil "~A #~A" text (intern-handle object))))
    (fold-model-text text)))
