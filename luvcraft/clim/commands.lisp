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

;;; The window layer.  Fullscreen belongs to the window rather than to
;;; anything inside it, and Shift-Tab is the gesture that gets the keyboard
;;; back from whatever has taken it -- including a shell reading raw keys,
;;; which is exactly the surface that must not be able to swallow it.

(define-command (com-toggle-fullscreen :command-table luvcraft-window
                                       :name "Toggle Fullscreen"
                                       :keystroke (:f11))
    ()
  (let ((canvas (luvcraft:luvcraft-session-canvas (luvcraft-command-session))))
    (luv:set-canvas-fullscreen canvas (not (luv:canvas-fullscreen-p canvas)))))

(define-command (com-leave-focus :command-table luvcraft-window
                                 :name "Leave Focus"
                                 :keystroke (:tab :shift))
    ()
  (luvcraft:unfocus-luvcraft-session (luvcraft-command-session)))

(define-command (com-release-pointer :command-table luvcraft-world
                                     :name "Release Pointer"
                                     :keystroke (:escape))
    ()
  (let ((session (luvcraft-command-session)))
    (when (luvcraft:luvcraft-session-pointer-captured-p session)
      (luv:set-canvas-relative-pointer-mode
       (luvcraft:luvcraft-session-canvas session) nil)
      (setf (luvcraft:luvcraft-session-pointer-captured-p session) nil))))

(define-command (com-select-quickbar-slot :command-table luvcraft-world
                                          :name "Select Quickbar Slot")
    ((slot 'integer :prompt "quickbar slot"))
  ;; The quickbar is how many slots the number row reaches, while the block a
  ;; slot names is looked up in the whole inventory: a slot past the end of the
  ;; bar selects nothing rather than something further down the list.
  (let ((session (luvcraft-command-session)))
    (when (<= 1 slot (length (luvcraft:block-inventory-quickbar-blocks
                              (luvcraft:luvcraft-session-inventory session))))
      (luvcraft:select-luvcraft-block session slot))))

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
