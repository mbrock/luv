(in-package #:luvcraft.clim)

;;; The verbs.  Each one is the body that HANDLE-CANVAS-EVENT used to run
;;; inline behind an EQ test on a key name, now carrying its own name, its own
;;; human label, and its own keystroke as data in a table.

(define-command (com-toggle-inventory :command-table luvcraft-world
                                      :name "Toggle Inventory"
                                      :keystroke (#\i))
    ()
  (luvcraft:toggle-luvcraft-inventory (luvcraft-command-session)))

(define-command (com-toggle-phone :command-table luvcraft-world
                                  :name "Toggle Phone"
                                  :keystroke (#\f))
    ()
  (luvcraft:toggle-luvcraft-phone (luvcraft-command-session)))

(define-command (com-toggle-metabar :command-table luvcraft-world
                                    :name "Toggle Metabar"
                                    :keystroke (:return))
    ()
  (luvcraft:toggle-luvcraft-metabar (luvcraft-command-session)))

(define-command (com-toggle-focus :command-table luvcraft-world
                                  :name "Toggle Focus"
                                  :keystroke (:tab))
    ()
  (luvcraft:toggle-luvcraft-session-focus (luvcraft-command-session)))

;;; Fullscreen belongs to the window rather than to anything inside it, which
;;; is why it was taken above the focus dispatch before and why it will live in
;;; a table that a focused surface cannot shadow.
(define-command (com-toggle-fullscreen :command-table luvcraft-world
                                       :name "Toggle Fullscreen"
                                       :keystroke (:f11))
    ()
  (let ((canvas (luvcraft:luvcraft-session-canvas (luvcraft-command-session))))
    (luv:set-canvas-fullscreen canvas (not (luv:canvas-fullscreen-p canvas)))))

(define-command (com-select-quickbar-slot :command-table luvcraft-world
                                          :name "Select Quickbar Slot")
    ((slot 'integer :prompt "quickbar slot"))
  (luvcraft:select-luvcraft-block (luvcraft-command-session) slot))

;;; The number row selects a quickbar slot.  Nine commands would say the same
;;; thing nine times; a keystroke item of type :FUNCTION instead builds the
;;; command object for the digit that was pressed, which is the ordinary CLIM
;;; way to bind a family of keys to one verb with an argument.
(loop for slot from 1 to 9
      do (add-keystroke-to-command-table
          'luvcraft-world (list (digit-char slot)) :function
          (let ((slot slot))
            (lambda (gesture numeric-argument)
              (declare (ignore gesture numeric-argument))
              (list 'com-select-quickbar-slot slot)))
          :documentation (format nil "Select quickbar slot ~D" slot)
          :errorp nil))
