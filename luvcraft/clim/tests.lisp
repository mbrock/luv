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
