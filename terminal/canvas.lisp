;;; Projection from luv's portable canvas key fact to Ghostty's W3C key fact.

(in-package #:luv.terminal)

(defun ghostty-writing-key (key)
  (cond
    ((member key '(:a :b :c :d :e :f :g :h :i :j :k :l :m
                   :n :o :p :q :r :s :t :u :v :w :x :y :z))
     key)
    ((member key '(:0 :1 :2 :3 :4 :5 :6 :7 :8 :9))
     (intern (format nil "DIGIT-~A" key) "KEYWORD"))
    ((symbolp key)
     (cdr (assoc (symbol-name key)
                 '(("-" . :minus) ("=" . :equal)
                   ("[" . :bracket-left) ("]" . :bracket-right)
                   ("\\" . :backslash) (";" . :semicolon)
                   ("'" . :quote) ("`" . :backquote)
                   ("," . :comma) ("." . :period) ("/" . :slash))
                 :test #'string=)))))

(defun canvas-key-name-to-ghostty-key (key)
  (or (ghostty-writing-key key)
      (case key
        (:return :enter)
        (:control-left :control-left) (:control-right :control-right)
        (:shift-left :shift-left) (:shift-right :shift-right)
        (:alt-left :alt-left) (:alt-right :alt-right)
        (:super-left :meta-left) (:super-right :meta-right)
        (:menu :context-menu)
        (:left :arrow-left) (:right :arrow-right)
        (:up :arrow-up) (:down :arrow-down)
        (:kp-plus :numpad-add) (:kp-minus :numpad-subtract)
        (:kp-multiply :numpad-multiply) (:kp-divide :numpad-divide)
        (:kp-period :numpad-decimal) (:kp-enter :numpad-enter)
        (:kp-equals :numpad-equal) (:kp-comma :numpad-comma)
        (:num-lock :num-lock)
        (otherwise
         (cond
           ((member key '(:backspace :caps-lock :delete :end :escape :help
                          :home :insert :page-down :page-up :pause
                          :print-screen :scroll-lock :space :tab
                          :f1 :f2 :f3 :f4 :f5 :f6 :f7 :f8 :f9 :f10 :f11
                          :f12 :f13 :f14 :f15 :f16 :f17 :f18 :f19 :f20
                          :f21 :f22 :f23 :f24))
            key)
           ((and (symbolp key)
                 (let ((name (symbol-name key)))
                   (and (= (length name) 4)
                        (string= name "KP-" :end1 3)
                        (digit-char-p (char name 3)))))
            (intern (format nil "NUMPAD-~C" (char (symbol-name key) 3))
                    "KEYWORD"))
           (t :unidentified))))))

(defun canvas-key-consumed-modifiers (event)
  (let ((character (luv:canvas-key-event-character event))
        (unshifted (luv:canvas-key-event-unshifted-character event))
        (modifiers (luv:canvas-key-event-modifiers event)))
    (when (and character unshifted
               (char/= character unshifted)
               (member :shift modifiers))
      '(:shift))))

(defun canvas-key-text (event)
  (let ((character (luv:canvas-key-event-character event)))
    (when (and character
               (not (or (< (char-code character) 32)
                        (= (char-code character) 127))))
      (string character))))

(defun send-pty-device-canvas-key-event (device event)
  "Project portable canvas key EVENT into DEVICE's mode-aware input queue."
  (check-type event luv:canvas-key-event)
  (let ((action
          (etypecase event
            (luv:canvas-key-press-event
             (if (luv:canvas-key-event-repeat-p event) :repeat :press))
            (luv:canvas-key-release-event :release)))
        (unshifted (luv:canvas-key-event-unshifted-character event)))
    (send-pty-device-key
     device action
     (canvas-key-name-to-ghostty-key
      (luv:canvas-key-event-key-name event))
     :modifiers (luv:canvas-key-event-modifiers event)
     :consumed-modifiers (canvas-key-consumed-modifiers event)
     :text (canvas-key-text event)
     :unshifted-codepoint (and unshifted (char-code unshifted)))))
