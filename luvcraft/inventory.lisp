;;; Player-owned block inventory state, independent of any presentation system.

(in-package #:luvcraft)

(defclass block-inventory-entry ()
  ((block :initarg :block :reader block-inventory-entry-block)
   (quantity :initarg :quantity :initform nil
             :accessor block-inventory-entry-quantity))
  (:documentation
   "One material in an inventory.  A NIL quantity means unlimited supply."))

(defclass block-inventory ()
  ((entries :initarg :entries :accessor block-inventory-entries))
  (:documentation "The ordered materials available to one player."))

(defun make-block-inventory
    (&key (blocks (placeable-block-kinds)) (quantity nil))
  "Make an inventory containing BLOCKS in display and number-key order.

QUANTITY is the initial count of every material, or NIL for creative-mode
unlimited supply."
  (when quantity
    (check-type quantity (integer 0)))
  (make-instance
   'block-inventory
   :entries
   (mapcar (lambda (block)
             (check-type block block-kind)
             (make-instance 'block-inventory-entry
                            :block block :quantity quantity))
           blocks)))

(defun block-inventory-blocks (inventory)
  "Return INVENTORY's materials in stable display and selection order."
  (mapcar #'block-inventory-entry-block
          (block-inventory-entries inventory)))

(defun block-inventory-entry-for (inventory block)
  "Return INVENTORY's entry for BLOCK, or NIL when it is unavailable."
  (find block (block-inventory-entries inventory)
        :key #'block-inventory-entry-block :test #'eq))

(defun add-block-to-inventory (inventory block &optional (quantity 1))
  "Add QUANTITY of BLOCK to finite INVENTORY storage and return its entry.

Adding to an unlimited entry leaves it unlimited.  A previously unavailable
block is appended, preserving the stable order of existing number keys."
  (check-type quantity (integer 0))
  (let ((entry (block-inventory-entry-for inventory block)))
    (unless entry
      (setf entry (make-instance 'block-inventory-entry
                                 :block block :quantity 0)
            (block-inventory-entries inventory)
            (append (block-inventory-entries inventory) (list entry))))
    (when (block-inventory-entry-quantity entry)
      (incf (block-inventory-entry-quantity entry) quantity))
    entry))

(defun remove-block-from-inventory (inventory block &optional (quantity 1))
  "Consume QUANTITY of BLOCK and report success.

Unlimited entries always succeed.  Finite entries remain present at zero so
the view and number-key ordering do not jump as blocks are used."
  (check-type quantity (integer 0))
  (let ((entry (block-inventory-entry-for inventory block)))
    (when entry
      (let ((available (block-inventory-entry-quantity entry)))
        (when (or (null available) (<= quantity available))
          (when available
            (decf (block-inventory-entry-quantity entry) quantity))
          t)))))

(defgeneric toggle-luvcraft-inventory (session)
  (:documentation
   "Toggle SESSION's inventory view, returning true when one is available.

The renderer-independent game has no UI dependency.  A loaded presentation
extension such as MCLUV supplies the session method."))

(defmethod toggle-luvcraft-inventory ((session t))
  (declare (ignore session))
  nil)
