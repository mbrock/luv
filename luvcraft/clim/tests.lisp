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

(deftest modal-hud-panes-use-the-direct-presentation-compositor
  (dolist (class '(luvcraft.clim::luvcraft-legend-overlay
                   luvcraft.clim::luvcraft-command-menu-overlay
                   luvcraft.clim::luvcraft-tape-prompt-overlay))
    (ok (subtypep class 'mcluv:luvcraft-hud-widget-overlay))))

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
  (let ((session (make-instance 'luvcraft:luvcraft-session)))
    (ok (equal '(com-toggle-inventory)
               (luvcraft-key-command session (key-press :i :character #\i))))
    (ok (equal '(com-toggle-focus)
               (luvcraft-key-command session (key-press :tab))))
    (ok (equal '(com-toggle-fullscreen)
               (luvcraft-key-command session (key-press :f11))))
    (ok (equal '(com-toggle-tracy-capture)
               (luvcraft-key-command session (key-press :f9))))
    (ok (equal '(luvcraft.clim::com-quit)
               (luvcraft-key-command
                session (key-press :q :character #\q
                                      :modifiers '(:control)))))
    (ok (equal '(com-execute-command)
               (luvcraft-key-command
                session (key-press :x :character #\x :modifiers '(:meta)))))
    (ok (equal '(com-start-walking :forward)
               (luvcraft-key-command
                session (key-press :w
                                   :character
                                   (if (luvcraft.clim::dvorak-controls-p)
                                       #\, #\w)))))
    ;; A key nothing binds is not a command and must not be mistaken for one:
    ;; LOOKUP-KEYSTROKE-COMMAND-ITEM answers with the gesture on a miss.
    (ok (null (luvcraft-key-command session (key-press :f8))))))

(deftest dvorak-controls-use-the-wasd-places-without-the-e-collision
  (ok (equal '((#\, :forward) (#\o :backward) (#\a :left) (#\e :right)
               (:shift-left :sprint) (:shift-right :sprint))
             (luvcraft.clim::walk-keys-for-layout :dvorak)))
  (when (luvcraft.clim::dvorak-controls-p)
    (let ((session (make-instance 'luvcraft:luvcraft-session)))
      (ok (equal '(com-start-walking :right)
                 (luvcraft-key-command session
                                       (key-press :d :character #\e))))
      (ok (equal '(luvcraft.clim::com-place-block)
                 (luvcraft-key-command session
                                       (key-press :e :character #\.)))))))

(deftest m-x-searches-the-executable-command-vocabulary
  (let ((entries (luvcraft-command-menu-entries)))
    (ok (assoc "Toggle Phone" entries :test #'string=))
    (ok (assoc "Place Block" entries :test #'string=))
    (ok (assoc "Toggle Tracy Capture" entries :test #'string=))
    (ok (assoc "Export Presentation Timing" entries :test #'string=))
    ;; Commands needing arguments will join M-x when it can ask for them;
    ;; presenting them as executable before then would make a dishonest menu.
    (ok (null (assoc "Select Quickbar Slot" entries :test #'string=)))
    (ok (equal '("Toggle Phone")
               (mapcar #'car
                       (luvcraft.clim::matching-command-menu-entries
                        entries "phone"))))
    ;; Words need not be adjacent, so the same finder scales to longer names.
    (ok (equal '("Toggle Pointer Capture")
               (mapcar #'car
                       (luvcraft.clim::matching-command-menu-entries
                        entries "pointer toggle"))))))

(deftest a-digit-carries-its-slot-as-a-command-argument
  (let ((session (make-instance 'luvcraft:luvcraft-session)))
    (ok (equal '(com-select-quickbar-slot 3)
               (luvcraft-key-command session (key-press :3 :character #\3))))
    (ok (equal '(com-select-quickbar-slot 1)
               (luvcraft-key-command session (key-press :1 :character #\1))))
    ;; The 0 key ends the number row as the tenth slot.
    (ok (equal '(com-select-quickbar-slot 10)
               (luvcraft-key-command session (key-press :0 :character #\0))))))

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
    (ok (not (execute-canvas-key-event-command frame (key-press :f8))))
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

(defun key-release (key-name &key character modifiers)
  (make-instance 'luv:canvas-key-release-event
                 :timestamp 0 :key-name key-name
                 :character character :unshifted-character character
                 :modifiers modifiers))

(deftest walking-is-a-start-and-a-stop
  (let* ((session (make-instance 'luvcraft:luvcraft-session))
         (intent (luvcraft:luvcraft-session-movement-intent session))
         (dvorak (luvcraft.clim::dvorak-controls-p)))
    (luv:handle-canvas-event session nil
                             (key-press :w :character (if dvorak #\, #\w)))
    (ok (luvcraft:movement-urging-p intent :forward))
    (ok (= 1d0 (luvcraft:movement-intent-axis intent :forward :backward)))
    ;; Holding the opposite direction is standing still, and letting go of it
    ;; leaves the first one still held -- which one axis could not remember.
    (luv:handle-canvas-event session nil
                             (key-press :s :character (if dvorak #\o #\s)))
    (ok (zerop (luvcraft:movement-intent-axis intent :forward :backward)))
    (luv:handle-canvas-event session nil
                             (key-release :s :character (if dvorak #\o #\s)))
    (ok (= 1d0 (luvcraft:movement-intent-axis intent :forward :backward)))
    (luv:handle-canvas-event session nil
                             (key-release :w :character (if dvorak #\, #\w)))
    (ok (luvcraft:movement-intent-still-p intent))))

(deftest a-movement-key-means-the-same-thing-however-it-is-modified
  (let* ((session (make-instance 'luvcraft:luvcraft-session))
         (intent (luvcraft:luvcraft-session-movement-intent session)))
    ;; Shift makes a player sprint; it does not make W into another key.
    (luv:handle-canvas-event
     session nil (key-press :shift-left :modifiers '(:shift)))
    (luv:handle-canvas-event
     session nil (key-press :w
                            :character (if (luvcraft.clim::dvorak-controls-p)
                                           #\, #\w)
                            :modifiers '(:shift)))
    (ok (luvcraft:movement-intent-sprinting-p intent))
    (ok (luvcraft:movement-urging-p intent :forward))
    (luv:handle-canvas-event
     session nil (key-release :shift-left :modifiers '(:shift)))
    (ok (not (luvcraft:movement-intent-sprinting-p intent)))
    (ok (luvcraft:movement-urging-p intent :forward))))

(deftest space-owes-the-player-a-jump
  (let* ((session (make-instance 'luvcraft:luvcraft-session))
         (intent (luvcraft:luvcraft-session-movement-intent session)))
    (luv:handle-canvas-event session nil (key-press :space))
    (ok (luvcraft:movement-intent-jump-requested-p intent))))

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

;;; The legend says what the tables say.

(deftest the-legend-is-read-off-the-command-tables
  (let ((sections (luvcraft-legend-sections)))
    (ok (equal '("Moving" "In the world" "At a wall" "Any time")
               (mapcar #'car sections)))
    (flet ((keys-for (title label)
             (cdr (assoc label (cdr (assoc title sections :test #'string=))
                         :test #'string=))))
      ;; A direction is its own line, because walking forward and sprinting are
      ;; different things to do even though one command performs both.
      (ok (equal (if (luvcraft.clim::dvorak-controls-p) '(",") '("W"))
                 (keys-for "Moving" "walk forward")))
      (ok (equal (if (luvcraft.clim::dvorak-controls-p) '("E") '("D"))
                 (keys-for "Moving" "walk right")))
      ;; The arrows look rather than walk: one merged row, all four keys.
      (ok (equal '("↑" "↓" "←" "→") (keys-for "In the world" "look")))
      (ok (equal '("Shift") (keys-for "Moving" "sprint")))
      (ok (equal '("Space") (keys-for "Moving" "jump")))
      (ok (equal '("I") (keys-for "In the world" "toggle inventory")))
      (ok (equal '("Esc") (keys-for "In the world" "show keys")))
      (ok (equal '("Alt-X") (keys-for "In the world" "execute command")))
      (ok (equal '("F9") (keys-for "Any time" "toggle tracy capture")))
      (ok (equal '("F11") (keys-for "Any time" "toggle fullscreen")))
      ;; Modifiers are printed, and :ANY is not: it is noise on every row.
      (ok (equal '("Shift-Tab") (keys-for "Any time" "leave focus")))
      ;; A wall's modes are reachable, and say by which key.
      (ok (equal '("Cmd-1") (keys-for "At a wall" "shell mode")))
      (ok (equal '("Cmd-2") (keys-for "At a wall" "film mode")))
      ;; Ten slots share one line, because nobody needs to be told about each.
      (ok (equal '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0")
                 (keys-for "In the world" "select block")))
      ;; A command owned by the movement layer is not repeated under the world
      ;; that inherits it.
      (ok (null (keys-for "In the world" "walk forward"))))))

(deftest a-rebound-key-changes-what-the-legend-says
  (let ((table (clim:find-command-table 'luvcraft.clim::luvcraft-world)))
    (unwind-protect
         (progn
           (clim:add-keystroke-to-command-table
            table '(#\y) :command '(luvcraft.clim::com-toggle-inventory)
            :errorp nil)
           (ok (member "Y" (cdr (assoc "toggle inventory"
                                       (cdr (assoc "In the world"
                                                   (luvcraft-legend-sections)
                                                   :test #'string=))
                                       :test #'string=))
                       :test #'string=)
               "the legend follows the table rather than a written-down list"))
      (clim:remove-keystroke-from-command-table table '(#\y) :errorp nil))))
