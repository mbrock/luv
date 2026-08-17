(defpackage #:luvcraft.clim.tests
  (:use #:cl #:rove #:luvcraft.clim)
  (:local-nicknames (#:luv #:luv)
                    (#:clim #:clim)
                    (#:climi #:clim-internals)))

(in-package #:luvcraft.clim.tests)

(defun key-press (key-name &key character modifiers)
  (make-instance 'luv:canvas-key-press-event
                 :timestamp 0
                 :key-name key-name
                 :character character
                 :unshifted-character character
                 :modifiers modifiers))

(defun test-frame ()
  (make-luvcraft-frame (make-instance 'luvcraft:luvcraft-session)))

(defclass recording-focus ()
  ((score :initarg :score :initform nil :reader recording-focus-score)
   (events :initform nil :accessor recording-focus-events)))

(defmethod luvcraft:luvcraft-focus-score
    ((focus recording-focus) (session luvcraft:luvcraft-session))
  (recording-focus-score focus))

(defmethod luvcraft:handle-luvcraft-focus-event
    ((focus recording-focus) session canvas event)
  (declare (ignore session canvas))
  (push event (recording-focus-events focus)))

(deftest a-keystroke-is-a-place-on-the-keyboard
  (ok (canvas-key-event-matches-gesture-p
       (key-press :i :character #\i) '(#\i)))
  (ok (canvas-key-event-matches-gesture-p (key-press :tab) '(:tab)))
  ;; Caps lock does not stop a player playing.
  (ok (canvas-key-event-matches-gesture-p
       (key-press :i :character #\i :modifiers '(:caps-lock)) '(#\i)))
  ;; A modifier the gesture does not name makes a different gesture.
  (ok (not (canvas-key-event-matches-gesture-p
            (key-press :i :character #\i :modifiers '(:control)) '(#\i))))
  (ok (not (canvas-key-event-matches-gesture-p (key-press :tab) '(:tab :shift))))
  (ok (canvas-key-event-matches-gesture-p
       (key-press :tab :modifiers '(:shift)) '(:tab :shift))))

(deftest keys-resolve-to-named-commands
  (let ((frame (test-frame)))
    (ok (equal '(com-toggle-inventory)
               (canvas-key-event-command frame (key-press :i :character #\i))))
    (ok (equal '(com-toggle-focus)
               (canvas-key-event-command frame (key-press :tab))))
    (ok (equal '(com-toggle-fullscreen)
               (canvas-key-event-command frame (key-press :f11))))
    ;; A key nothing binds is not a command and must not be mistaken for one:
    ;; LOOKUP-KEYSTROKE-COMMAND-ITEM answers with the gesture on a miss.
    (ok (null (canvas-key-event-command frame (key-press :w :character #\w))))))

(deftest a-digit-carries-its-slot-as-a-command-argument
  (let ((frame (test-frame)))
    (ok (equal '(com-select-quickbar-slot 3)
               (canvas-key-event-command frame (key-press :3 :character #\3))))
    (ok (equal '(com-select-quickbar-slot 1)
               (canvas-key-event-command frame (key-press :1 :character #\1))))))

(deftest a-command-runs-inline-on-the-calling-thread
  (let* ((session (make-instance 'luvcraft:luvcraft-session))
         (frame (make-luvcraft-frame session))
         (blocks (luvcraft:block-inventory-blocks
                  (luvcraft:luvcraft-session-inventory session))))
    ;; The whole design rests on this: with no FRAME-PROCESS of its own, the
    ;; frame applies a command on the thread that looked it up, so a command
    ;; may touch session state exactly as an event handler may.
    (ok (null (climi::frame-process frame)))
    (ok (execute-canvas-key-event-command frame (key-press :2 :character #\2)))
    (ok (eq (second blocks) (luvcraft:luvcraft-session-selected-block session)))
    ;; An unbound key changes nothing and says so.
    (ok (not (execute-canvas-key-event-command
              frame (key-press :w :character #\w))))
    (ok (eq (second blocks)
            (luvcraft:luvcraft-session-selected-block session)))))

;;; Dispatch: which layer a key reaches.

(deftest tab-enters-focus-and-shift-tab-always-leaves-it
  (let* ((session (make-instance 'luvcraft:luvcraft-session))
         (far (make-instance 'recording-focus :score 4.0))
         (near (make-instance 'recording-focus :score 1.0))
         (tab (key-press :tab))
         (tab-release (make-instance 'luv:canvas-key-release-event
                                     :timestamp 0 :key-name :tab))
         (shift-tab (key-press :tab :modifiers '(:shift))))
    (luvcraft:add-luvcraft-overlay session far)
    (luvcraft:add-luvcraft-overlay session near)
    (luv:handle-canvas-event session nil tab)
    (ok (eq near (luvcraft:luvcraft-session-modal-focus session)))
    ;; The release matching the focus keystroke is swallowed; ordinary Tab
    ;; afterwards belongs to the focus, which is where Bash completion lives.
    (luv:handle-canvas-event session nil tab-release)
    (luv:handle-canvas-event session nil tab)
    (luv:handle-canvas-event session nil tab-release)
    (ok (equal (list tab-release tab) (recording-focus-events near)))
    ;; Shift-Tab is in the window table, so it leaves even a focus that takes
    ;; every other key.
    (luv:handle-canvas-event session nil shift-tab)
    (ok (null (luvcraft:luvcraft-session-modal-focus session)))
    (ok (null (recording-focus-events far)))))

(deftest a-focus-with-no-table-of-its-own-takes-every-remaining-key
  (let ((session (make-instance 'luvcraft:luvcraft-session))
        (focus (make-instance 'recording-focus))
        (i (key-press :i :character #\i)))
    (luvcraft:focus-luvcraft-session session focus)
    (luv:handle-canvas-event session nil i)
    (ok (equal (list i) (recording-focus-events focus))
        "a shell being typed into gets its own I")
    ;; Unfocused, the same key is the world's verb rather than text, and never
    ;; reaches the object that used to hold focus.
    (luvcraft:unfocus-luvcraft-session session)
    (ok (equal '(com-toggle-inventory)
               (luvcraft.clim::luvcraft-key-command session i)))))

(deftest movement-keys-fall-through-to-the-held-key-state
  (let ((session (make-instance 'luvcraft:luvcraft-session))
        (w (key-press :w :character #\w))
        (space (key-press :space)))
    (luv:handle-canvas-event session nil w)
    (ok (gethash :w (luvcraft:luvcraft-session-pressed-keys session)))
    (luv:handle-canvas-event session nil space)
    (ok (luvcraft:luvcraft-session-jump-requested-p session))
    (luv:handle-canvas-event
     session nil (make-instance 'luv:canvas-key-release-event
                                :timestamp 0 :key-name :w))
    (ok (not (gethash :w (luvcraft:luvcraft-session-pressed-keys session))))))

(deftest the-number-row-selects-through-the-command-layer
  (let* ((session (make-instance 'luvcraft:luvcraft-session))
         (blocks (luvcraft:block-inventory-blocks
                  (luvcraft:luvcraft-session-inventory session))))
    (luv:handle-canvas-event session nil (key-press :8 :character #\8))
    (ok (eq (nth 7 blocks)
            (luvcraft:luvcraft-session-selected-block session)))
    ;; A repeat is the same keystroke still held, so a verb does not fire again.
    (luv:handle-canvas-event session nil (key-press :1 :character #\1))
    (ok (eq (first blocks)
            (luvcraft:luvcraft-session-selected-block session)))
    (luv:handle-canvas-event
     session nil (make-instance 'luv:canvas-key-press-event
                                :timestamp 0 :key-name :8 :character #\8
                                :unshifted-character #\8 :repeat-p t))
    (ok (eq (first blocks)
            (luvcraft:luvcraft-session-selected-block session)))))
