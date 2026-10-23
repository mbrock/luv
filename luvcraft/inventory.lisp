;;; Player-owned block inventory state, independent of any presentation system.

(in-package #:luvcraft)

(defgeneric player-inventory (player)
  (:documentation "The player's block stock and ordered building kits, independent of engine."))
(defgeneric player-selected-block (player))
(defgeneric (setf player-selected-block) (block player))
(defgeneric player-creative-p (player))
(defgeneric update-player-instruments (player)
  (:documentation "Publish a changed selection or kit to the engine's controls."))

(defclass block-inventory-entry ()
  ((block :initarg :block :reader block-inventory-entry-block)
   (quantity :initarg :quantity :initform nil
             :accessor block-inventory-entry-quantity))
  (:documentation
   "One material in an inventory.  A NIL quantity means unlimited supply."))

(defclass block-inventory ()
  ((entries :initarg :entries :accessor block-inventory-entries)
   (simple-p :initform nil :accessor block-inventory-simple-p)
   (kit :initform nil :accessor block-inventory-kit))
  (:documentation "The ordered materials available to one player."))

(defconstant +block-inventory-quickbar-size+ 10)

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

(defun block-inventory-quickbar-entries (inventory)
  "Return the simple building kit, or the full inventory's first ten entries."
  (if (block-inventory-simple-p inventory)
      (loop for block in (block-inventory-kit inventory)
            for entry = (block-inventory-entry-for inventory block)
            when entry collect entry)
      (let ((entries (block-inventory-entries inventory)))
        (subseq entries 0 (min +block-inventory-quickbar-size+
                              (length entries))))))

(defun block-inventory-quickbar-blocks (inventory)
  "Return INVENTORY's number-key materials in stable order."
  (mapcar #'block-inventory-entry-block
          (block-inventory-quickbar-entries inventory)))

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

(defgeneric block-kind-carried-p (block)
  (:documentation
   "Whether BLOCK is a thing a player picks up and carries, rather than a
material the palette gives without limit.  A film is; stone is not.")
  (:method (block)
    (declare (ignore block))
    nil))

(defun block-inventory-carried-blocks (inventory)
  "Return the carried blocks INVENTORY actually holds, in order."
  (loop for entry in (block-inventory-entries inventory)
        for block = (block-inventory-entry-block entry)
        for quantity = (block-inventory-entry-quantity entry)
        when (and (block-kind-carried-p block) quantity (plusp quantity))
          collect block))

(defgeneric toggle-luvcraft-inventory (session)
  (:documentation
   "Toggle SESSION's inventory view, returning true when one is available.

LUVCRAFT/CORE has no UI dependency.  LUVCRAFT/MCCLIM supplies the session
method."))

(defmethod toggle-luvcraft-inventory ((session t))
  (declare (ignore session))
  nil)

(defconstant +building-kit-size+ 6)

(defun add-building-kit-block (inventory block &optional replacement)
  "Add BLOCK once, or replace zero-based REPLACEMENT when the six-slot kit is full.
Return its slot and :ADDED, :PRESENT, or :FULL. Never discard a block silently."
  (unless (block-inventory-entry-for inventory block)
    (error "Block ~S is not in this inventory." block))
  (let* ((kit (block-inventory-kit inventory))
         (present (position block kit :test #'eq)))
    (cond (present (values present :present))
          ((< (length kit) +building-kit-size+)
           (setf (block-inventory-kit inventory) (append kit (list block)))
           (values (length kit) :added))
          ((and (typep replacement '(integer 0))
                (< replacement (length kit)))
           (setf (nth replacement (block-inventory-kit inventory)) block)
           (values replacement :added))
          (t (values nil :full)))))
