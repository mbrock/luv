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
