(in-package #:luvcraft.agent)

;;; Gnomes and cats are inhabitants of the saved world (#9AONMH).  Each is
;;; written as its place, its heading, and the provider response its
;;; conversation continues from; the provider keeps the history itself, so a
;;; gnome brought back in a later game remembers what was said to it.

(defgeneric embodied-agent-save-kind (agent)
  (:documentation "The keyword naming AGENT's kind in a world save."))

(defmethod embodied-agent-save-kind ((agent gnome))
  (declare (ignore agent))
  :gnome)

(defmethod embodied-agent-save-kind ((agent cat))
  (declare (ignore agent))
  :cat)

(defun embodied-agent-conversation-id (agent)
  "The provider response AGENT's conversation continues from, or NIL."
  (let ((provider (gnome-agent agent)))
    (or (and provider (openai:agent-response-id provider))
        (embodied-agent-saved-conversation agent))))

(defun embodied-agent-save-description (agent)
  "AGENT as a tagged, printable inhabitant description."
  (list* (embodied-agent-save-kind agent)
         :cell (list (gnome-x agent) (gnome-y agent) (gnome-z agent))
         :facing-yaw (embodied-agent-facing-yaw agent)
         (append
          (alexandria:when-let ((id (embodied-agent-conversation-id agent)))
            (list :conversation id))
          (alexandria:when-let ((asked (gnome-asked agent)))
            (list :asked asked))
          (alexandria:when-let ((said (gnome-said agent)))
            (list :said said)))))

(defun session-embodied-agent-descriptions (session)
  "SESSION's gnomes and cats, oldest first."
  (mapcar #'embodied-agent-save-description
          (reverse (agents-in-session session))))

(pushnew 'session-embodied-agent-descriptions luvcraft:*inhabitant-describers*)

(defun described-embodied-agent (class session description)
  "A fresh CLASS in SESSION as DESCRIPTION describes it, not yet in its world."
  (let ((cell (luvcraft::description-value description :cell "agent"))
        (yaw (luvcraft::description-value description :facing-yaw "agent"
                                          :optional t :default 0d0))
        (conversation (luvcraft::description-value
                       description :conversation "agent" :optional t))
        (asked (luvcraft::description-value description :asked "agent"
                                            :optional t))
        (said (luvcraft::description-value description :said "agent"
                                           :optional t)))
    (unless (and (listp cell) (= 3 (length cell)) (every #'integerp cell))
      (luvcraft::invalid-luvcraft-save
       "An agent's cell must be three integers, not ~S." cell))
    (unless (realp yaw)
      (luvcraft::invalid-luvcraft-save
       "An agent's heading must be real, not ~S." yaw))
    (dolist (text (list conversation asked said))
      (unless (typep text '(or null string))
        (luvcraft::invalid-luvcraft-save
         "An agent's conversation and lines must be strings, not ~S." text)))
    (destructuring-bind (x y z) cell
      (let ((agent (make-embodied-agent class session x y z :facing-yaw yaw))
            (now (get-internal-real-time)))
        (setf (embodied-agent-saved-conversation agent) conversation)
        (when asked
          (setf (gnome-asked agent) asked (gnome-asked-at agent) now))
        (when said
          (setf (gnome-said agent) said (gnome-said-at agent) now))
        agent))))

(defun restore-embodied-agent (class session description)
  "Stand a CLASS described by DESCRIPTION in SESSION again and return it."
  (let ((agent (described-embodied-agent class session description)))
    (alexandria:when-let
        ((occupant (agent-at session (gnome-x agent) (gnome-y agent)
                             (gnome-z agent))))
      (error "Cell (~D ~D ~D) already holds ~A."
             (gnome-x agent) (gnome-y agent) (gnome-z agent)
             (embodied-agent-name occupant)))
    (install-embodied-agent agent)))

(defmethod luvcraft:restore-luvcraft-inhabitant
    ((kind (eql :gnome)) session description)
  (restore-embodied-agent 'gnome session description))

(defmethod luvcraft:restore-luvcraft-inhabitant
    ((kind (eql :cat)) session description)
  (restore-embodied-agent 'cat session description))
