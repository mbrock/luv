(in-package #:luvcraft.agent.tests)

;;; Standing an agent in a world needs a GPU body, so these tests build
;;; agents from their descriptions and register them without one.

(defun call-with-described-agents (descriptions function)
  "Call FUNCTION with a bare session holding agents made from DESCRIPTIONS."
  (let* ((session (make-instance 'luvcraft:luvcraft-session))
         (agents (loop for description in descriptions
                       collect (agent::described-embodied-agent
                                (ecase (first description)
                                  (:gnome 'agent:gnome)
                                  (:cat 'agent:cat))
                                session (rest description))))
         (agent::*agents* (reverse agents)))
    (funcall function session agents)))

(defun saved-and-read-back (world inhabitants)
  "The inhabitants of a save of WORLD, after printing and reading it."
  (let ((text (with-standard-io-syntax
                (prin1-to-string
                 (luvcraft:make-luvcraft-save-description
                  world :inhabitants inhabitants)))))
    (nth-value 2 (luvcraft::restore-luvcraft-save-description
                  (with-standard-io-syntax
                    (let ((*read-eval* nil)) (read-from-string text)))))))

(defparameter *saved-inhabitants*
  '((:gnome :cell (2 3 4) :facing-yaw 1.5d0 :conversation "resp_garden"
            :asked "What will you grow?" :said "Flowers.")
    (:cat :cell (5 3 4) :facing-yaw 0d0)))

(define-test gnomes-and-cats-are-saved-mid-conversation
  (call-with-described-agents
   *saved-inhabitants*
   (lambda (session agents)
     (destructuring-bind (gnome cat) agents
       (true (typep gnome 'agent:gnome))
       (true (typep cat 'agent:cat))
       (true (string= "resp_garden"
                      (agent::embodied-agent-conversation-id gnome)))
       (true (string= "Flowers." (agent::gnome-said gnome))))
     ;; Described again, through a printed save, they are what they were.
     (let ((saved (luvcraft:luvcraft-session-inhabitant-descriptions session))
           (world (luvcraft:make-empty-little-block-world :seed 7)))
       (true (equal *saved-inhabitants* saved))
       (true (equal saved (saved-and-read-back world saved)))))))

(define-test an-unknown-inhabitant-is-kept-as-written
  (let ((session (make-instance 'luvcraft:luvcraft-session))
        (dragon '(:dragon :cell (0 0 0) :hoard 12)))
    (luvcraft:restore-luvcraft-inhabitants session (list dragon))
    (true (equal (list dragon)
                 (luvcraft:luvcraft-session-inhabitant-descriptions
                  session)))))

(define-test an-old-save-without-inhabitants-still-reads
  (let ((world (luvcraft:make-empty-little-block-world :seed 7)))
    (true (eq :absent (getf (rest (luvcraft:make-luvcraft-save-description world))
                            :inhabitants :absent)))
    (true (null (saved-and-read-back world '())))))

(define-test a-restored-agent-opens-its-saved-conversation
  (call-with-described-agents
   (list (first *saved-inhabitants*))
   (lambda (session agents)
     (declare (ignore session))
     (let* ((gnome (first agents))
            (owner (list :opening gnome))
            (candidate (make-instance 'agent:world-agent
                                      :application gnome :model "test"
                                      :socket nil)))
       (setf (agent::embodied-agent-agent-opening-owner gnome) owner)
       (true (eq candidate
                 (agent::publish-opened-embodied-agent gnome owner candidate)))
       (true (string= "resp_garden" (openai:agent-response-id candidate)))))))
