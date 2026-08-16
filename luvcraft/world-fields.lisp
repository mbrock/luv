;;; Inspectable definitions for distributed voxel facts.
;;;
;;; A field definition is aggregate metadata, not a wrapper around site
;;; values.  Existing chunks, light products, atlases, and snapshots retain
;;; their own specialized arrays and bind one definition object when they are
;;; materialized, making live redefinition visible without changing a lane.

(in-package #:luv.world.fields)

(defclass voxel-field-definition (math:represented-value-declaration)
  ((name
    :initarg :name
    :reader voxel-field-definition-name)
   (site-kind
    :initarg :site-kind
    :reader voxel-field-definition-site-kind)
   (missing-value-semantics
    :initarg :missing-value-semantics
    :reader voxel-field-definition-missing-value-semantics)
   (legal-value-type
    :initarg :legal-value-type
    :reader voxel-field-definition-legal-value-type)
   (representation-policy
    :initarg :representation-policy
    :reader voxel-field-definition-representation-policy)
   (revision
    :initform (gensym "FIELD-DEFINITION-")
    :reader voxel-field-definition-revision))
  (:documentation
   "One distributed fact's identity and per-site meaning. #FKAV5Y

The inherited represented-value declaration describes a reading after its
storage encoding has been resolved.  REPRESENTATION-POLICY separately names
the aggregate encoding, such as a palette-index column or a u8 level array."))

(defgeneric field-definition-for (name)
  (:documentation "Return the live voxel field definition named by NAME."))

(defmethod field-definition-for (name)
  (declare (ignore name))
  nil)

(defgeneric materialized-field-definition (materialization field-name)
  (:documentation
   "Return the definition object retained by MATERIALIZATION for FIELD-NAME."))

(defmethod materialized-field-definition (materialization field-name)
  (declare (ignore materialization field-name))
  nil)

(defun materialized-field-current-p (materialization field-name)
  "Whether MATERIALIZATION still carries FIELD-NAME's live definition object."
  (let ((materialized
          (materialized-field-definition materialization field-name)))
    (and materialized
         (eq materialized (field-definition-for field-name)))))

(defun make-voxel-field-definition
    (name site-kind value-type quantity missing-value-semantics
     legal-value-type representation-policy source-form)
  (unless (and (keywordp name) (symbolp site-kind) representation-policy)
    (error "Invalid voxel field definition ~S." source-form))
  (make-instance
   'voxel-field-definition
   :name name
   :site-kind site-kind
   :representation-type value-type
   :quantity-specification
   (math:make-declared-quantity-specification quantity)
   :missing-value-semantics missing-value-semantics
   :legal-value-type legal-value-type
   :representation-policy representation-policy
   :source-form source-form))

(defmacro define-voxel-field
    (name &key site-kind value-type quantity missing-value
               legal-values representation)
  "Define one inspectable voxel field through an EQL-specialized method."
  (unless (and (keywordp name) site-kind value-type legal-values representation)
    (error "DEFINE-VOXEL-FIELD needs identity, site, value, and storage metadata."))
  (let ((query-name (gensym "FIELD-NAME"))
        (source-form
          `(define-voxel-field ,name
             :site-kind ,site-kind
             :value-type ,value-type
             ,@(when quantity `(:quantity ,quantity))
             :missing-value ,missing-value
             :legal-values ,legal-values
             :representation ,representation)))
    `(progn
       (defmethod field-definition-for ((,query-name (eql ,name)))
         (declare (ignore ,query-name))
         (load-time-value
          (make-voxel-field-definition
           ,name ',site-kind ',value-type ',quantity ',missing-value
           ',legal-values ',representation ',source-form)))
       ,name)))

(define-voxel-field :block-content
  :site-kind :voxel-cell
  :value-type t
  :missing-value :unavailable
  :legal-values t
  :representation :palette-u16)
