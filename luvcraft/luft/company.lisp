;;;; Company made comfortable (#3JTHM3, #LV16J7): what players say to one
;;;; another, and the shared things a guest asks its host for.
;;;;
;;;; A line of chat is a CHAT-MESSAGE on the reliable stream.  A guest's line
;;;; goes to its host, which names its speaker -- a guest speaks only for
;;;; itself -- shows it, and sends it to every guest, its speaker too; the
;;;; host's own lines take the same way out.  Each side shows a line once
;;;; its host has it, as an edit shows once its host confirms it.
(in-package #:luvcraft.luft.game)

(defparameter *chat-line-limit* 280
  "The most characters of one line of chat.")

(defparameter *chat-lines-shown* 6
  "How many of the latest lines a view shows.")

(defparameter *chat-seconds* 12
  "How long a view shows chat after its latest line.")

(defun chat-text (text)
  "TEXT as one line of chat: trimmed, on one line, and no longer than the limit."
  (let ((line (string-trim " " (substitute-if #\Space (lambda (c) (member c '(#\Newline #\Return #\Tab)))
                                              text))))
    (subseq line 0 (min (length line) *chat-line-limit*))))

(defun hear-chat (session speaker text)
  "Keep SPEAKER's line TEXT in SESSION, and show the latest lines in its view."
  (let ((line (format nil "~A: ~A" speaker text)))
    (push line (session-chat-lines session))
    (when (> (length (session-chat-lines session)) 100)
      (setf (session-chat-lines session) (subseq (session-chat-lines session) 0 50)))
    (session-log session "~A" line)
    (let* ((viewer (session-viewer session))
           (workbench (and viewer (luv.workbench:application-workbench viewer))))
      (when workbench
        (luv.workbench:show-workbench-message
         workbench (reverse (subseq (session-chat-lines session)
                                    0 (min *chat-lines-shown* (length (session-chat-lines session)))))
         :seconds *chat-seconds*)))
    line))

(defun say-in-world (world text)
  "Say TEXT to everyone in WORLD's session, as its local player."
  (let ((session (world-session world))
        (text (chat-text text)))
    (unless session (error "No one else is here to hear you."))
    (unless (eq (session-state session) :open) (error "The host is not connected."))
    (unless (string= text "")
      (let ((message (wire:make-chat-message
                      :speaker (player-name (session-local-player session)) :text text)))
        (etypecase session
          (host-session (hear-chat session (wire:chat-message-speaker message) text)
                        (session-broadcast session message))
          (guest-session (session-broadcast session message)))))
    text))

(defmethod handle-session-message ((session host-session) peer (message wire:chat-message))
  (let ((player (guest-peer-player session peer))
        (text (chat-text (wire:chat-message-text message))))
    (when (and player (string/= text ""))
      (hear-chat session (player-name player) text)
      (session-broadcast session (wire:make-chat-message :speaker (player-name player)
                                                         :text text)))))

(defmethod handle-session-message ((session guest-session) peer (message wire:chat-message))
  (declare (ignore peer))
  (hear-chat session (wire:chat-message-speaker message) (wire:chat-message-text message)))

(clim:define-command (com-chat :command-table luvcraft-world :name "Chat") ()
  (let ((viewer (render:viewer-command-viewer)))
    (luv.workbench:open-workbench-prompt
     (luv.workbench:application-workbench viewer)
     :label "Say:"
     :function (lambda (text)
                 (call-companion-ui viewer (lambda () (say-in-world (viewer-world viewer) text)))))))

;;; The day.  The host owns the clock and the day settings (#U4TBMP); a
;;; guest's knobs ask it with a SKY-REQUEST-MESSAGE.  The guest sets its own
;;; sky at once and keeps it so -- a wish -- against the host's snapshots
;;; until one shows the host has granted it, or for a second at most, so the
;;; knob neither waits for the round trip nor springs back meanwhile.

(defparameter *sky-wish-seconds* 1d0
  "How long a guest keeps its sky as it asked against snapshots that disagree.")

(defun set-sky-setting (world setting value)
  "Set WORLD's own SETTING of the day to VALUE."
  (let ((clock (world-sky-clock world)))
    (ecase setting
      (:hour (setf (luvcraft::sky-clock-hour clock) value))
      (:minutes-per-day (setf (luvcraft::sky-clock-minutes-per-day clock) (coerce value 'double-float)))
      (:paused (setf (luvcraft::sky-clock-paused-p clock) (and value t)))
      (:tilt (setf (world-sun-orbit-tilt world) (coerce value 'single-float))))
    value))

(defun sky-setting-holds-p (world setting value)
  "Whether WORLD's sky already has SETTING at VALUE, as near as the wire tells."
  (let ((clock (world-sky-clock world)))
    (ecase setting
      (:hour (< (abs (- (mod (+ (- (/ value 24d0) (luvcraft::sky-clock-day-fraction clock)) 0.5d0) 1d0)
                        0.5d0))
                1/240))
      (:minutes-per-day (< (abs (- (luvcraft::sky-clock-minutes-per-day clock) value))
                           (* 1d-4 (abs value))))
      (:paused (eq (and value t) (and (luvcraft::sky-clock-paused-p clock) t)))
      (:tilt (< (abs (- (world-sun-orbit-tilt world) value)) 1d-4)))))

(defun valid-sky-setting-p (setting value)
  (case setting
    (:hour (and (realp value) (<= 0 value 24)))
    (:minutes-per-day (and (realp value) (<= 0.1 value 1440)))
    (:paused (typep value 'boolean))
    (:tilt (and (realp value) (<= 0 value 1)))))

(defun set-world-sky (world setting value)
  "Set SETTING of WORLD's day to VALUE: in its own sky, and on a guest by
asking the host too."
  (set-sky-setting world setting value)
  (when (replica-world-p world)
    (let ((session (world-session world)))
      (session-broadcast session (wire:make-sky-request-message
                                  :setting setting
                                  :value (if (realp value) (coerce value 'double-float) value)))
      (setf (getf (living-sky-wishes (living world)) setting)
            (list value (+ (wall-seconds) *sky-wish-seconds*)))))
  value)

(defun keep-sky-wishes (world)
  "After a host's snapshot set WORLD's sky: hold each setting the guest has
asked for and the host has not yet granted, and forget the rest."
  (let ((living (living world)) (now (wall-seconds)))
    (setf (living-sky-wishes living)
          (loop for (setting (value deadline)) on (living-sky-wishes living) by #'cddr
                unless (or (sky-setting-holds-p world setting value) (> now deadline))
                  do (set-sky-setting world setting value)
                  and collect setting and collect (list value deadline)))))

(defmethod handle-session-message ((session host-session) peer (message wire:sky-request-message))
  (let ((player (guest-peer-player session peer))
        (setting (wire:sky-request-message-setting message))
        (value (wire:sky-request-message-value message)))
    (when player
      (if (valid-sky-setting-p setting value)
          (set-world-sky (session-world session) setting value)
          (session-log session "Refused ~A's sky request ~S ~S." (player-id player) setting value)))))
