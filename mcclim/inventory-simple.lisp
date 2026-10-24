;;; Two remembered player profiles for the unified material browser.
(in-package #:mcluv)

(defvar *inventory-preferences-pathname* nil
  "Optional preferences file override for captures and isolated tests.")

(defun inventory-preferences-pathname ()
  (or *inventory-preferences-pathname*
      (merge-pathnames
       #P"../inventory-players.sexp"
       (uiop:pathname-directory-pathname
        (luvcraft::default-luvcraft-world-pathname)))))

(defun default-inventory-preferences ()
  (list :version 1 :active 0
        :players (loop repeat 2 collect
                   (list :full nil :kit '(:grass :planks :bricks)
                         :bar (copy-list luvcraft::*luvcraft-starting-quickbar*)))))

(defun validate-inventory-preferences (data)
  "Reject a damaged preferences file before it can replace either player's kit."
  (unless (and (listp data) (eql 1 (getf data :version))
               (typep (getf data :active) '(integer 0 1))
               (listp (getf data :players)) (= 2 (length (getf data :players)))
               (every (lambda (player)
                        (and (listp player)
                             (member (getf player :full) '(nil t))
                             (every (lambda (key)
                                      (let ((names (getf player key)))
                                        (and (listp names)
                                             (<= (length names)
                                                 (if (eq key :kit) 6 10))
                                             (every #'keywordp names)
                                             (= (length names)
                                                (length (remove-duplicates names))))))
                                    '(:kit :bar))))
                      (getf data :players)))
    (error "Invalid inventory player preferences: ~S" data))
  data)

(defun read-inventory-preferences (path)
  (if (probe-file path)
      (with-open-file (in path :external-format :utf-8)
        (let ((*read-eval* nil) (*readtable* (copy-readtable nil)))
          (let ((data (read in nil :eof)))
            (unless (eq :eof (read in nil :eof))
              (error "Extra data in inventory preferences ~A" path))
            (validate-inventory-preferences data))))
      (default-inventory-preferences)))

(defun inventory-data (frame)
  (luvcraft:player-inventory (inventory-session frame)))

(defun inventory-simple-p (frame)
  (luvcraft::block-inventory-simple-p (inventory-data frame)))

(defun inventory-active-player (frame)
  (getf (inventory-preferences frame) :active))

(defun inventory-profile-description (frame)
  (let ((inventory (inventory-data frame)))
    (list :full (not (inventory-simple-p frame))
          :kit (mapcar #'luvcraft:block-kind-name
                       (luvcraft::block-inventory-kit inventory))
          :bar (mapcar (lambda (entry)
                         (luvcraft:block-kind-name
                          (luvcraft:block-inventory-entry-block entry)))
                       (subseq (luvcraft:block-inventory-entries inventory)
                               0 (min 10 (length (luvcraft:block-inventory-entries
                                                 inventory))))))))

(defun remember-inventory-profile (frame)
  (when (inventory-preferences frame)
    (let* ((data (inventory-preferences frame))
           (players (getf data :players))
           (index (inventory-active-player frame))
           (description (inventory-profile-description frame)))
      (unless (equal description (nth index players))
        (setf (nth index players) description)
        (luvcraft::request-world-checkpoint
         (inventory-preferences-writer frame) (copy-tree data))))))

(defun apply-inventory-profile (frame)
  (let* ((data (inventory-preferences frame))
         (profile (nth (getf data :active) (getf data :players)))
         (inventory (inventory-data frame))
         (session (inventory-session frame)))
    (flet ((blocks (names)
             (loop for name in names
                   for block = (luvcraft:block-kind-named name nil)
                   when (and block (luvcraft:block-inventory-entry-for inventory block))
                     collect block)))
      (setf (luvcraft::block-inventory-simple-p inventory) nil
            (luvcraft::block-inventory-kit inventory)
            (or (blocks (getf profile :kit))
                (list (luvcraft:block-kind-named :grass))))
      (luvcraft::arrange-luvcraft-quickbar session (getf profile :bar)))
    (setf (inventory-scroll frame) 0.0
          (inventory-hovered-block frame) nil)
    (luvcraft:update-player-instruments session)))

(defun initialize-inventory-preferences (frame)
  (unless (inventory-preferences frame)
    (let* ((path (inventory-preferences-pathname))
           (data (read-inventory-preferences path)))
      (setf (inventory-preferences frame) data
            (inventory-preferences-writer frame)
            (luvcraft::make-world-checkpoint-writer path))
      (apply-inventory-profile frame)))
  frame)

(defun switch-inventory-player (frame)
  (remember-inventory-profile frame)
  (let ((data (inventory-preferences frame)))
    (setf (getf data :active) (- 1 (getf data :active)))
    (apply-inventory-profile frame)
    (luvcraft::request-world-checkpoint
     (inventory-preferences-writer frame) (copy-tree data)))
  (repaint-inventory frame))

(defmethod luvcraft:release-luvcraft-overlay :before
    ((overlay luvcraft-inventory-overlay))
  (let* ((frame (widget-overlay-frame overlay))
         (writer (inventory-preferences-writer frame)))
    (when writer
      (remember-inventory-profile frame)
      (luvcraft::stop-world-checkpoint-writer writer)
      (setf (inventory-preferences-writer frame) nil))))

(defmethod luvcraft:refresh-luvcraft-overlay :after
    ((overlay luvcraft-inventory-overlay) session)
  (declare (ignore session))
  (remember-inventory-profile (widget-overlay-frame overlay)))

(defun inventory-simple-state (frame)
  ;; The legacy name is retained for the inventory's visible-state protocol.
  (list (and (inventory-preferences frame) (inventory-active-player frame))))

(defmethod handle-repaint :around ((pane inventory-pane) region)
  (with-text-style (pane (make-text-style :sans-serif :roman (ui-text-size :body)))
    (call-next-method)))
