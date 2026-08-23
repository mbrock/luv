(in-package #:luv.application-agent.tests)

(clim:define-command-table application-agent-test-commands)

(clim:define-application-frame application-agent-test-frame ()
  ()
  (:command-table
   (application-agent-test-frame
    :inherit-from (application-agent-test-commands))))

(defvar *test-command-result* nil)

(clim:define-command (com-test-adjust
                      :command-table application-agent-test-commands
                      :name "Test Adjust")
    ((count 'integer :prompt "count")
     (mode '(member :fast :slow) :prompt "mode")
     &key
     (label 'string :prompt "label" :default "ordinary"))
  "Adjust a probe with a count, mode, and optional label."
  (setf *test-command-result* (list count mode label)))

(defclass test-application ()
  ((frame :initform
          (clim:make-application-frame 'application-agent-test-frame)
          :reader test-application-frame)
   (dispatch-count :initform 0 :accessor test-application-dispatch-count)))

(defmethod agent:application-command-frame
    ((application test-application))
  (test-application-frame application))

(defmethod agent:call-in-application-frame
    ((application test-application) function)
  (incf (test-application-dispatch-count application))
  (funcall function))

(defun test-response (&optional (text "done"))
  (make-instance 'openai:agent-response
                 :text text :reasoning "" :id "test" :usage nil))

(defun make-test-agent (&key turn-function close-function
                             close-failure-function)
  (make-instance 'agent:application-agent
                 :application (make-instance 'test-application)
                 :model "test" :socket nil
                 :turn-function (or turn-function
                                    (lambda (agent prompt)
                                      (declare (ignore agent prompt))
                                      (test-response)))
                 :close-function (or close-function
                                     (lambda (agent)
                                       (declare (ignore agent))))
                 :close-failure-function
                 (or close-failure-function
                     (lambda (failure) (declare (ignore failure))))
                 :observer-failure-function
                 (lambda (failure) (declare (ignore failure)))))

(defun join-test-turn (turn &optional (timeout 1.0))
  (or (agent:wait-for-turn turn :timeout timeout) :timeout))

(deftest ask-returns-before-provider-turn-completes
  (let ((started (sb-thread:make-semaphore :count 0))
        (finish (sb-thread:make-semaphore :count 0))
        (application-agent nil))
    (unwind-protect
         (progn
           (setf application-agent
                 (make-test-agent
                  :turn-function
                  (lambda (agent prompt)
                    (declare (ignore agent prompt))
                    (sb-thread:signal-semaphore started)
                    (sb-thread:wait-on-semaphore finish)
                    (test-response "unblocked"))))
           (let* ((before (get-internal-real-time))
                  (turn (agent:ask "hello" :agent application-agent))
                  (elapsed
                    (/ (- (get-internal-real-time) before)
                       (float internal-time-units-per-second 1.0))))
             (ok (< elapsed 0.1) "ASK only creates a worker")
             (ok (sb-thread:wait-on-semaphore started :timeout 1.0))
             (ok (member (agent:turn-status turn)
                         '(:thinking :working)))
             (sb-thread:signal-semaphore finish)
             (ok (not (eq :timeout (join-test-turn turn))))
             (ok (eq :done (agent:turn-status turn)))
             (ok (string= "unblocked" (agent:turn-text turn)))))
      (when application-agent
        (agent:release-application-agent application-agent)
        (agent:wait-for-application-agent-release
         application-agent :timeout 1.0)))))

(deftest asks-reach-the-provider-in-publication-order
  (let ((started (sb-thread:make-semaphore :count 0))
        (finish (sb-thread:make-semaphore :count 0))
        (received '())
        (received-lock (sb-thread:make-mutex :name "ordered test turns"))
        (application-agent nil))
    (unwind-protect
         (progn
           (setf application-agent
                 (make-test-agent
                  :turn-function
                  (lambda (agent prompt)
                    (declare (ignore agent))
                    (when (string= prompt "first")
                      (sb-thread:signal-semaphore started)
                      (sb-thread:wait-on-semaphore finish))
                    (sb-thread:with-mutex (received-lock)
                      (setf received (nconc received (list prompt))))
                    (test-response prompt))))
           (let ((first (agent:ask "first" :agent application-agent)))
             (ok (sb-thread:wait-on-semaphore started :timeout 1.0))
             (let ((second (agent:ask "second" :agent application-agent))
                   (third (agent:ask "third" :agent application-agent)))
               (ok (eq (agent:turn-thread first) (agent:turn-thread second)))
               (ok (eq (agent:turn-thread second) (agent:turn-thread third)))
               (sb-thread:signal-semaphore finish)
               (ok (agent:wait-for-turn first :timeout 1.0))
               (ok (agent:wait-for-turn second :timeout 1.0))
               (ok (agent:wait-for-turn third :timeout 1.0))
               (ok (equal '("first" "second" "third") received)))))
      (sb-thread:signal-semaphore finish)
      (when application-agent
        (agent:release-application-agent application-agent)
        (agent:wait-for-application-agent-release
         application-agent :timeout 1.0)))))

(deftest release-prompts-close-and-unblocks-an-active-turn
  (let ((started (sb-thread:make-semaphore :count 0))
        (provider-closed (sb-thread:make-semaphore :count 0))
        (events '())
        (events-lock (sb-thread:make-mutex :name "release order test"))
        (close-count 0)
        (application-agent nil))
    (unwind-protect
         (progn
           (setf application-agent
                 (make-test-agent
                  :turn-function
                  (lambda (agent prompt)
                    (declare (ignore agent prompt))
                    (sb-thread:signal-semaphore started)
                    ;; Model NEXT-AGENT-EVENT: only the provider close path,
                    ;; not the test or release caller, wakes this active turn.
                    (sb-thread:wait-on-semaphore provider-closed)
                    (sb-thread:with-mutex (events-lock)
                      (push :turn events))
                    (test-response))
                  :close-function
                  (lambda (agent)
                    (declare (ignore agent))
                    (sb-thread:with-mutex (events-lock)
                      (incf close-count)
                      (push :close events))
                    (sb-thread:signal-semaphore provider-closed))))
           (let ((active (agent:ask "active" :agent application-agent)))
             (ok (sb-thread:wait-on-semaphore started :timeout 1.0))
             (let* ((queued (agent:ask "queued" :agent application-agent))
                    (before (get-internal-real-time))
                    (released
                      (agent:release-application-agent application-agent))
                    (elapsed
                      (/ (- (get-internal-real-time) before)
                         (float internal-time-units-per-second 1.0))))
               (ok released)
               (ok (< elapsed 0.1) "release only publishes terminal work")
               (ok (eq :released
                       (agent:application-agent-state application-agent)))
               (ok (null (agent:application-agent-application
                          application-agent)))
               (ok (agent:wait-for-application-agent-release
                    application-agent :timeout 1.0))
               (ok (agent:wait-for-turn active :timeout 1.0))
               (ok (agent:wait-for-turn queued :timeout 1.0))
               (ok (eq :done (agent:turn-status active)))
               (ok (eq :failed (agent:turn-status queued)))
               (ok (typep (agent:turn-error queued)
                          'agent:application-agent-released))
               (ok (equal '(:close :turn) (nreverse events)))
               (ok (= 1 close-count))
               (ok (not (eq (agent:application-agent-close-thread
                             application-agent)
                            (agent:application-agent-worker-thread
                             application-agent))))
               (ok (agent:application-agent-release-finished-p
                    application-agent))
               (ok (null (agent:release-application-agent
                          application-agent))))))
      (sb-thread:signal-semaphore provider-closed)
      (when application-agent
        (agent:release-application-agent application-agent)
        (agent:wait-for-application-agent-release
         application-agent :timeout 1.0)))))

(deftest stalled-provider-close-has-a-bounded-observable-wait
  (let ((close-started (sb-thread:make-semaphore :count 0))
        (finish-close (sb-thread:make-semaphore :count 0))
        (application-agent nil))
    (unwind-protect
         (progn
           (setf application-agent
                 (make-test-agent
                  :close-function
                  (lambda (agent)
                    (declare (ignore agent))
                    (sb-thread:signal-semaphore close-started)
                    (sb-thread:wait-on-semaphore finish-close))))
           (ok (agent:release-application-agent application-agent))
           (ok (sb-thread:wait-on-semaphore close-started :timeout 1.0))
           (ok (eq :released
                   (agent:application-agent-state application-agent)))
           (ok (null (agent:application-agent-application
                      application-agent)))
           (ng (agent:wait-for-application-agent-release
                application-agent :timeout 0.05))
           (ng (agent:application-agent-close-finished-p application-agent))
           (ng (agent:application-agent-release-finished-p application-agent))
           (sb-thread:signal-semaphore finish-close)
           (ok (agent:wait-for-application-agent-release
                application-agent :timeout 1.0))
           (ok (agent:application-agent-close-finished-p application-agent))
           (ok (agent:application-agent-release-finished-p
                application-agent)))
      (sb-thread:signal-semaphore finish-close)
      (when application-agent
        (agent:release-application-agent application-agent)
        (agent:wait-for-application-agent-release
         application-agent :timeout 1.0)))))

(deftest one-failing-observer-does-not-suppress-the-others
  (let ((application-agent (make-test-agent))
        (events '()))
    (unwind-protect
         (progn
           (agent:add-agent-observer
            application-agent
            (lambda (agent kind object)
              (declare (ignore agent object))
              (push kind events)))
           (agent:add-agent-observer
            application-agent
            (lambda (agent kind object)
              (declare (ignore agent kind object))
              (error "broken observer")))
           (handler-bind ((agent:agent-observer-failure #'muffle-warning))
             (let ((turn (agent:ask "observe" :agent application-agent)))
               (ok (not (eq :timeout (join-test-turn turn))))))
           (ok (member :turn-started events))
           (ok (member :turn-finished events))
           (ok (= 2 (length (agent:application-agent-observer-failures
                             application-agent)))))
      (agent:release-application-agent application-agent)
      (agent:wait-for-application-agent-release
       application-agent :timeout 1.0))))

(deftest command-schema-and-parser-share-clim-argument-semantics
  (let* ((tool (agent:make-command-tool 'com-test-adjust))
         (arguments (agent:command-tool-argument-specifications tool))
         (parameters (openai:tool-parameters tool))
         (properties (cdr (assoc "properties" parameters :test #'string=)))
         (required (cdr (assoc "required" parameters :test #'string=))))
    (ok (equal '("count" "mode" "label")
               (mapcar #'agent:command-argument-json-name arguments)))
    (ok (equal '(t t nil)
               (mapcar #'agent:command-argument-required-p arguments)))
    (ok (equal '(nil nil t)
               (mapcar #'agent:command-argument-keyword-p arguments)))
    (ok (equal '("count" "mode") (coerce required 'list)))
    (ok (string= "integer"
                 (cdr (assoc "type" (cdr (assoc "count" properties
                                                  :test #'string=))
                             :test #'string=))))
    (ok (equal '("fast" "slow")
               (cdr (assoc "enum" (cdr (assoc "mode" properties
                                                :test #'string=))
                           :test #'string=))))
    (ok (equal '(com-test-adjust 7 :fast :label "named probe")
               (agent:command-tool-parse
                tool '(("count" . 7)
                       ("mode" . "fast")
                       ("label" . "named probe")))))
    (ok (equal '(com-test-adjust 8 :slow)
               (agent:command-tool-parse
                tool '(("count" . 8) ("mode" . "slow")))))
    (ok (signals
         (agent:command-tool-parse
          tool '(("count" . 1) ("mode" . "fast") ("bogus" . 2)))
         'agent:tool-argument-error))
    (ok (signals
         (agent:command-tool-parse tool '(("count" . 1)))
         'agent:tool-argument-error))))

(deftest command-effects-cross-the-application-frame-boundary-once
  (let* ((application-agent (make-test-agent))
         (application
           (agent:application-agent-application application-agent))
         (tool (agent:make-command-tool 'com-test-adjust))
         (*test-command-result* nil))
    (unwind-protect
         (let ((output
                 (openai:call-tool
                  tool
                  '(("count" . 12) ("mode" . "slow")
                    ("label" . "canvas"))
                  application-agent)))
           (ok (= 1 (test-application-dispatch-count application)))
           (ok (equal '(12 :slow "canvas") *test-command-result*))
           (ok (search "CANVAS" output :test #'char-equal)))
      (agent:release-application-agent application-agent)
      (agent:wait-for-application-agent-release
       application-agent :timeout 1.0))))

(deftest release-is-terminal-idempotent-and-concurrent-safe
  (let ((close-count 0)
        (close-lock (sb-thread:make-mutex :name "test agent close"))
        (application-agent nil))
    (setf application-agent
          (make-test-agent
           :close-function
           (lambda (agent)
             (declare (ignore agent))
             (sb-thread:with-mutex (close-lock)
               (incf close-count)))))
    (let ((threads
            (loop repeat 8
                  collect
                  (sb-thread:make-thread
                   (lambda ()
                     (agent:release-application-agent application-agent))))))
      (dolist (thread threads)
        (sb-thread:join-thread thread)))
    (ok (agent:wait-for-application-agent-release
         application-agent :timeout 1.0))
    (ok (= 1 close-count))
    (ok (eq :released (agent:application-agent-state application-agent)))
    (ok (null (agent:application-agent-application application-agent)))
    (ok (null (agent:release-application-agent application-agent)))
    (ok (signals
         (agent:ask "too late" :agent application-agent)
         'agent:application-agent-released))
    (ok (signals
         (agent:intern-handle
          (make-instance 'standard-object)
          (agent:application-agent-handles application-agent))
         'agent:handle-table-released))))

(deftest close-failure-does-not-reattach-a-released-agent
  (let* ((close-count 0)
        (application-agent
          (make-test-agent
           :close-function
           (lambda (agent)
             (declare (ignore agent))
             (incf close-count)
             (error "scripted close failure")))))
    (ok (agent:release-application-agent application-agent))
    (ok (agent:wait-for-application-agent-release
         application-agent :timeout 1.0))
    (ok (= 1 close-count))
    (ok (typep (agent:application-agent-close-error application-agent)
               'error))
    (ok (eq :released
            (agent:application-agent-state application-agent)))
    (ok (null (agent:application-agent-application application-agent)))
    (ok (null (agent:release-application-agent application-agent)))
    (ok (= 1 close-count))))
