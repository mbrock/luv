(in-package #:luvcraft.agent.tests)

(defclass scripted-opening-mixin ()
  ((opening-started :initform (sb-thread:make-semaphore :count 0)
                    :reader opening-started)
   (opening-finish :initform (sb-thread:make-semaphore :count 0)
                   :reader opening-finish)
   (turn-finished :initform (sb-thread:make-semaphore :count 0)
                  :reader scripted-turn-finished)
   (script-lock :initform (sb-thread:make-mutex :name "agent opening test")
                :reader scripted-opening-lock)
   (opening-count :initform 0 :accessor opening-count)
   (close-count :initform 0 :accessor close-count)
   (opened-thread-name :initform nil :accessor opened-thread-name)
   (opened-candidate :initform nil :accessor opened-candidate)
   (received-prompts :initform '() :accessor received-prompts)
   (opening-failure-p :initarg :opening-failure-p :initform nil
                      :reader opening-failure-p)))

(defclass opening-test-gnome (scripted-opening-mixin agent:gnome) ())
(defclass opening-test-cat (scripted-opening-mixin agent:cat) ())

(deftest agent-hud-respects-the-global-top-status-inset
  (let* ((session (make-instance 'luvcraft:luvcraft-session))
         (status
           (make-instance 'mcluv:luvcraft-status-bar-overlay
                          :session session :frame nil :mirror nil)))
    (setf (luvcraft:luvcraft-session-overlays session) (list status))
    (ok (= 48.0 (agent::agent-hud-top-margin session)))))

(defun make-opening-test-presence (class &rest initargs)
  (apply #'make-instance class
         :session nil :x 2 :y 3 :z 4
         :position (luvcraft::make-vec3 2.5d0 3d0 4.5d0)
         initargs))

(defun scripted-agent-response ()
  (make-instance 'openai:agent-response
                 :text "done" :reasoning "" :id "test" :usage nil))

(defun make-scripted-world-agent (presence)
  (make-instance
   'agent:world-agent
   :application presence :model "test" :socket nil
   :turn-function
   (lambda (provider prompt)
     (declare (ignore provider))
     (sb-thread:with-mutex ((scripted-opening-lock presence))
       (push prompt (received-prompts presence)))
     (sb-thread:signal-semaphore (scripted-turn-finished presence))
     (scripted-agent-response))
   :close-function
   (lambda (provider)
     (declare (ignore provider))
     (sb-thread:with-mutex ((scripted-opening-lock presence))
       (incf (close-count presence))))
   :observer-failure-function
   (lambda (failure) (declare (ignore failure)))))

(defun perform-scripted-opening (presence)
  (sb-thread:with-mutex ((scripted-opening-lock presence))
    (incf (opening-count presence))
    (setf (opened-thread-name presence)
          (sb-thread:thread-name sb-thread:*current-thread*)))
  (sb-thread:signal-semaphore (opening-started presence))
  (sb-thread:wait-on-semaphore (opening-finish presence))
  (when (opening-failure-p presence)
    (error "scripted provider opening failure"))
  (let ((candidate (make-scripted-world-agent presence)))
    (setf (opened-candidate presence) candidate)
    candidate))

(defmethod agent::open-embodied-agent-agent ((presence opening-test-gnome))
  (perform-scripted-opening presence))

(defmethod agent::open-embodied-agent-agent ((presence opening-test-cat))
  (perform-scripted-opening presence))

(defun join-opening-thread (presence thread)
  (declare (ignore presence))
  (when thread
    (sb-thread:join-thread thread :default :timeout :timeout 1.0)))

(defun join-agent-turns (provider)
  (dolist (turn (agent:world-agent-turns provider))
    (luv.application-agent:wait-for-turn turn :timeout 1.0)))

(deftest first-contact-is-one-named-opening-and-never-blocks-the-canvas
  (dolist (class '(opening-test-gnome opening-test-cat))
    (let ((presence (make-opening-test-presence class)))
      (unwind-protect
           (let* ((before (get-internal-real-time))
                  (answer (agent::gnome-ask presence "first"))
                  (elapsed
                    (/ (- (get-internal-real-time) before)
                       (float internal-time-units-per-second 1.0))))
             (ok (eq presence answer))
             (ok (< elapsed 0.1) "first contact only starts a worker")
             (ok (sb-thread:wait-on-semaphore
                  (opening-started presence) :timeout 1.0))
             (let ((worker (agent::embodied-agent-agent-opening-thread
                            presence)))
               (ok (search "agent opening" (opened-thread-name presence)
                           :test #'char-equal))
               (ok (search (if (eq class 'opening-test-cat) "cat" "gnome")
                           (opened-thread-name presence)
                           :test #'char-equal))
               (ok (eq presence (agent::gnome-ask presence "second")))
               (ok (= 1 (opening-count presence)))
               (sb-thread:signal-semaphore (opening-finish presence))
               (ok (not (eq :timeout (join-opening-thread presence worker))))
               (ok (sb-thread:wait-on-semaphore
                    (scripted-turn-finished presence) :timeout 1.0))
               (ok (sb-thread:wait-on-semaphore
                    (scripted-turn-finished presence) :timeout 1.0))
               (let ((provider (agent:gnome-agent presence)))
                 (ok (typep provider 'agent:world-agent))
                 (join-agent-turns provider)
                 (ok (equal '("first" "second")
                            (nreverse (received-prompts presence)))))))
        (sb-thread:signal-semaphore (opening-finish presence))
        (let ((provider (agent:gnome-agent presence)))
          (agent::release-embodied-agent-harness presence)
          (when provider
            (luv.application-agent:wait-for-application-agent-release
             provider :timeout 1.0)))))))

(deftest opening-failure-is-published-only-through-the-canvas-mailbox
  (let ((presence (make-opening-test-presence
                   'opening-test-gnome :opening-failure-p t)))
    (unwind-protect
         (progn
           (ok (eq presence (agent::gnome-ask presence "hello")))
           (ok (sb-thread:wait-on-semaphore
                (opening-started presence) :timeout 1.0))
           (let ((worker (agent::embodied-agent-agent-opening-thread
                          presence)))
             (sb-thread:signal-semaphore (opening-finish presence))
             (ok (not (eq :timeout (join-opening-thread presence worker)))))
           (multiple-value-bind (note received-p)
               (sb-concurrency:receive-message
                (agent::gnome-notes presence) :timeout 1.0)
             (ok received-p)
             (ok (eq :agent-failed (first note)))
             (ok (search "scripted provider opening failure" (second note)))
             (ok (null (agent::gnome-said presence))
                 "the provider worker never mutates canvas presentation")))
      (agent::release-embodied-agent-harness presence))))

(deftest release-during-opening-detaches-now-and-closes-the-late-candidate
  (let ((presence (make-opening-test-presence 'opening-test-cat)))
    (ok (eq presence (agent::gnome-ask presence "too late")))
    (ok (sb-thread:wait-on-semaphore
         (opening-started presence) :timeout 1.0))
    (let ((worker (agent::embodied-agent-agent-opening-thread presence)))
      (ok (agent::release-embodied-agent-harness presence))
      (ok (null (agent:gnome-agent presence)))
      (ok (null (agent::release-embodied-agent-harness presence)))
      (sb-thread:signal-semaphore (opening-finish presence))
      (ok (not (eq :timeout (join-opening-thread presence worker))))
      (ok (typep (opened-candidate presence) 'agent:world-agent))
      (ok (luv.application-agent:wait-for-application-agent-release
           (opened-candidate presence) :timeout 1.0))
      (ok (= 1 (close-count presence)))
      (ok (eq :released
              (luv.application-agent:application-agent-state
               (opened-candidate presence))))
      (multiple-value-bind (note received-p)
          (sb-concurrency:receive-message-no-hang (agent::gnome-notes presence))
        (declare (ignore note))
        (ng received-p "teardown suppresses stale opening failures")))))
