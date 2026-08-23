;;; Small protocols shared by finite-domain materializations.

(in-package #:luv.domains)

(defgeneric domain-cardinality (domain)
  (:documentation
   "Return the exact number of sites in finite DOMAIN.

This deliberately says nothing about coordinate or offset representation.
Those mappings belong to concrete domain protocols and are added only when a
client needs to traverse them."))

(defclass identity-vocabulary-domain ()
  ((members :reader identity-vocabulary-members)
   (offsets :reader %identity-vocabulary-offsets)
   (revision :initform 0 :reader identity-vocabulary-revision)
   (limit :initarg :limit :initform nil :reader %identity-vocabulary-limit))
  (:documentation
   "An append-only finite domain whose semantic members have EQ identity.

MEMBERS is the adjustable vector interpreting every dense offset. Appending a
new identity increments REVISION without moving any old offset, so retained
dense columns stay meaningful. Clients borrow MEMBERS rather than copying it;
snapshots which cross an ownership boundary must freeze it explicitly."))

(defmethod initialize-instance :after
    ((domain identity-vocabulary-domain) &key (members #()))
  (let* ((source (coerce members 'simple-vector))
         (limit (%identity-vocabulary-limit domain))
         (vector (make-array (length source) :adjustable t
                                             :fill-pointer (length source)))
         (offsets (make-hash-table :test #'eq)))
    (when (and limit (> (length source) limit))
      (error "Vocabulary starts with ~D members but its limit is ~D."
             (length source) limit))
    (loop for member across source
          for offset from 0
          do (multiple-value-bind (old present-p) (gethash member offsets)
               (when present-p
                 (error "Vocabulary member ~S occurs at offsets ~D and ~D."
                        member old offset)))
             (setf (aref vector offset) member
                   (gethash member offsets) offset))
    (setf (slot-value domain 'members) vector
          (slot-value domain 'offsets) offsets)))

(defun make-identity-vocabulary-domain (&key members limit)
  "Return an append-only EQ vocabulary initially containing MEMBERS."
  (make-instance 'identity-vocabulary-domain :members members :limit limit))

(defmethod domain-cardinality ((domain identity-vocabulary-domain))
  (length (identity-vocabulary-members domain)))

(defun identity-vocabulary-member (domain offset)
  "Return the semantic member interpreted by OFFSET under DOMAIN."
  (let ((members (identity-vocabulary-members domain)))
    (unless (and (integerp offset) (<= 0 offset) (< offset (length members)))
      (error "Offset ~S is outside vocabulary ~S with cardinality ~D."
             offset domain (length members)))
    (aref members offset)))

(defun identity-vocabulary-offset (domain member &optional (intern-p t))
  "Return MEMBER's stable dense offset, appending it when INTERN-P and new."
  (let ((offsets (%identity-vocabulary-offsets domain)))
    (multiple-value-bind (offset present-p) (gethash member offsets)
      (cond (present-p offset)
            ((not intern-p) nil)
            (t
             (let* ((members (identity-vocabulary-members domain))
                    (next (length members))
                    (limit (%identity-vocabulary-limit domain)))
               (when (and limit (>= next limit))
                 (error "Vocabulary ~S cannot contain more than ~D members."
                        domain limit))
               (vector-push-extend member members)
               (setf (gethash member offsets) next)
               (incf (slot-value domain 'revision))
               next))))))

(defclass keyword-vocabulary-domain ()
  ((members :initarg :members :reader %keyword-vocabulary-members)
   (offsets :initarg :offsets :reader keyword-vocabulary-offsets))
  (:documentation
   "An immutable finite domain whose semantic sites are keyword symbols.

The member vector is the explicit interpretation of every dense offset.  A
shape change creates another domain rather than changing what an existing
offset means; persisted or borrowed indices therefore remain closed by the
domain they travel with."))

(defun make-keyword-vocabulary-domain (members)
  "Return an immutable keyword domain preserving MEMBERS' order exactly.

Duplicate or non-keyword members are rejected.  The resulting mapping is
appropriate for dense runtime columns and for explicit persisted vocabulary
tables; the keyword is semantic identity and its offset is only meaningful
under this domain."
  (let* ((members (copy-seq (coerce members 'simple-vector)))
         (offsets (make-hash-table :test #'eq)))
    (loop for keyword across members
          for offset from 0
          do (unless (keywordp keyword)
               (error "Vocabulary member ~S at offset ~D is not a keyword."
                      keyword offset))
             (multiple-value-bind (old present-p) (gethash keyword offsets)
               (when present-p
                 (error "Vocabulary keyword ~S occurs at offsets ~D and ~D."
                        keyword old offset)))
             (setf (gethash keyword offsets) offset))
    (make-instance 'keyword-vocabulary-domain
                   :members members :offsets offsets)))

(defmethod domain-cardinality ((domain keyword-vocabulary-domain))
  (length (%keyword-vocabulary-members domain)))

(defun keyword-vocabulary-members (domain)
  "Return a fresh vector containing DOMAIN's keywords in dense-offset order."
  (copy-seq (%keyword-vocabulary-members domain)))

(defun keyword-vocabulary-offset (domain keyword &optional (error-p t))
  "Return KEYWORD's dense offset under DOMAIN.

When ERROR-P is false, return NIL for a keyword outside the domain."
  (multiple-value-bind (offset present-p)
      (gethash keyword (keyword-vocabulary-offsets domain))
    (cond (present-p offset)
          (error-p (error "Keyword ~S is not a member of domain ~S."
                          keyword domain))
          (t nil))))

(defun keyword-vocabulary-keyword (domain offset)
  "Return the semantic keyword interpreted by OFFSET under DOMAIN."
  (let ((members (%keyword-vocabulary-members domain)))
    (unless (and (integerp offset) (<= 0 offset) (< offset (length members)))
      (error "Offset ~S is outside domain ~S with cardinality ~D."
             offset domain (length members)))
    (aref members offset)))
