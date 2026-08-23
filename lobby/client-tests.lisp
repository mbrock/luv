(defpackage #:luv.lobby.tests
  (:use #:cl #:rove))

(in-package #:luv.lobby.tests)

(defclass scripted-lobby-transport (luv.lobby:lobby-transport)
  ((lock :initform (sb-thread:make-mutex :name "scripted lobby transport")
         :reader scripted-transport-lock)
   (fail-opens :initarg :fail-opens :initform 0
               :accessor scripted-transport-fail-opens)
   (opens :initform 0 :accessor scripted-transport-opens)
   (closes :initform 0 :accessor scripted-transport-closes)
   (published :initform nil :accessor scripted-transport-published)))

(defmethod luv.lobby:open-lobby-transport
    ((transport scripted-lobby-transport) client)
  (declare (ignore client))
  (sb-thread:with-mutex ((scripted-transport-lock transport))
    (incf (scripted-transport-opens transport))
    (when (plusp (scripted-transport-fail-opens transport))
      (decf (scripted-transport-fail-opens transport))
      (error "scripted connection failure"))
    (scripted-transport-opens transport)))

(defmethod luv.lobby:subscribe-lobby-transport
    ((transport scripted-lobby-transport) connection client)
  (declare (ignore transport client))
  connection)

(defmethod luv.lobby:publish-lobby-transport
    ((transport scripted-lobby-transport) connection client payload)
  (declare (ignore connection client))
  (sb-thread:with-mutex ((scripted-transport-lock transport))
    (push payload (scripted-transport-published transport)))
  payload)

(defmethod luv.lobby:next-lobby-publication
    ((transport scripted-lobby-transport) connection client)
  (declare (ignore transport connection client))
  ;; A real transport has the same bounded idle poll.  This tiny pause keeps
  ;; the fake worker cooperative without turning the test into a busy loop.
  (sleep 1/100)
  nil)

(defmethod luv.lobby:close-lobby-transport
    ((transport scripted-lobby-transport) connection client)
  (declare (ignore connection client))
  (sb-thread:with-mutex ((scripted-transport-lock transport))
    (incf (scripted-transport-closes transport))))

(defun wait-for-lobby-test (predicate &key (timeout 1.0))
  (loop with deadline = (+ (get-internal-real-time)
                           (* timeout internal-time-units-per-second))
        when (funcall predicate) return t
        when (>= (get-internal-real-time) deadline) return nil
        do (sleep 1/200)))

(deftest lobby-snapshot-is-semantic-and-does-not-revise-for-duplicates
  (let ((client
          (luv.lobby:make-lobby-client
           :name "Mikael" :client-id-prefix "test"
           :transport (make-instance 'scripted-lobby-transport))))
    (luv.lobby:receive-lobby-publication
     client
     (format nil "luv/presence/~A" (luv.lobby:lobby-client-id client))
     "Mikael")
    (luv.lobby:receive-lobby-publication
     client "luv/presence/game-2" "Daniel")
    (let* ((snapshot (luv.lobby:lobby-client-snapshot client))
           (revision (luv.lobby:lobby-snapshot-revision snapshot)))
      (ok (eq :stopped (luv.lobby:lobby-snapshot-status snapshot)))
      (ok (equal '("Daniel")
                 (mapcar #'luv.lobby:lobby-peer-name
                         (luv.lobby:lobby-snapshot-peers snapshot))))
      (luv.lobby:receive-lobby-publication
       client "luv/presence/game-2" "Daniel")
      (ok (= revision
             (luv.lobby:lobby-snapshot-revision
              (luv.lobby:lobby-client-snapshot client)))))
    (luv.lobby:receive-lobby-publication
     client "luv/presence/game-2" luv.lobby:+lobby-offline-payload+)
    (ok (null (luv.lobby:lobby-snapshot-peers
               (luv.lobby:lobby-client-snapshot client))))))

(deftest lobby-summary-is-a-constant-work-frame-boundary-view
  (let ((client
          (luv.lobby:make-lobby-client
           :client-id-prefix "summary"
           :transport (make-instance 'scripted-lobby-transport))))
    (luv.lobby:receive-lobby-publication
     client "luv/presence/game-2" "Daniel")
    (multiple-value-bind (status peer-count last-error revision)
        (luv.lobby:lobby-client-summary client)
      (ok (eq :stopped status))
      (ok (= 1 peer-count))
      (ok (null last-error))
      (ok (plusp revision))
      ;; A duplicate publication changes neither the count nor the cheap
      ;; semantic revision.
      (luv.lobby:receive-lobby-publication
       client "luv/presence/game-2" "Daniel")
      (multiple-value-bind (later-status later-count later-error later-revision)
          (luv.lobby:lobby-client-summary client)
        (ok (eq status later-status))
        (ok (= peer-count later-count))
        (ok (eq last-error later-error))
        (ok (= revision later-revision))))
    (multiple-value-bind (status peer-count last-error revision)
        (luv.lobby:lobby-client-summary nil)
      (ok (equal '(:stopped 0 nil 0)
                 (list status peer-count last-error revision))))))

(deftest lobby-cache-is-connection-scoped
  (let ((client
          (luv.lobby:make-lobby-client
           :client-id-prefix "test"
           :transport (make-instance 'scripted-lobby-transport))))
    (luv.lobby:receive-lobby-publication
     client "luv/presence/game-2" "Daniel")
    (luv.lobby:receive-lobby-publication
     client "luv/store/OPENAI_API_KEY" "secret")
    (ok (string= "secret"
                 (luv.lobby:lobby-client-value client "OPENAI_API_KEY")))
    ;; A connecting transition begins a new subscription epoch.  Both peer
    ;; presence and retained values must be reconstructed by that connection.
    (luv.lobby::publish-lobby-status
     client :connecting :clear-cache-p t)
    (let ((snapshot (luv.lobby:lobby-client-snapshot client)))
      (ok (null (luv.lobby:lobby-snapshot-peers snapshot)))
      (ok (null (luv.lobby:lobby-client-value client "OPENAI_API_KEY"))))
    (luv.lobby:receive-lobby-publication
     client "luv/store/OPENAI_API_KEY" "new-secret")
    (luv.lobby:receive-lobby-publication
     client "luv/store/OPENAI_API_KEY" "")
    (ok (null (luv.lobby:lobby-client-value client "OPENAI_API_KEY")))))

(deftest lobby-worker-retries-stops-and-restarts
  (let* ((transport
           (make-instance 'scripted-lobby-transport :fail-opens 1))
         (client
           (luv.lobby:make-lobby-client
            :name "Mikael" :client-id-prefix "luft"
            :transport transport)))
    (unwind-protect
         (progn
           (luv.lobby:start-lobby-client client)
           (ok (wait-for-lobby-test
                (lambda ()
                  (eq :online
                      (luv.lobby:lobby-snapshot-status
                       (luv.lobby:lobby-client-snapshot client))))))
           (ok (= 2 (scripted-transport-opens transport)))
           (ok (luv.lobby:lobby-client-running-p client))
           (luv.lobby:stop-lobby-client client)
           (ok (eq :stopped
                   (luv.lobby:lobby-snapshot-status
                    (luv.lobby:lobby-client-snapshot client))))
           (ok (not (luv.lobby:lobby-client-running-p client)))
           (ok (= 1 (scripted-transport-closes transport)))
           (ok (member "Mikael" (scripted-transport-published transport)
                       :test #'string=))
           (ok (member "" (scripted-transport-published transport)
                       :test #'string=))
           ;; Restart reuses the semantic owner but starts a fresh worker and
           ;; subscription epoch instead of retaining STOPPING-P forever.
           (luv.lobby:start-lobby-client client)
           (ok (wait-for-lobby-test
                (lambda () (= 3 (scripted-transport-opens transport)))))
           (luv.lobby:stop-lobby-client client)
           (ok (= 2 (scripted-transport-closes transport))))
      (ignore-errors (luv.lobby:stop-lobby-client client)))))

(deftest lobby-stop-interrupts-backoff-without-network-work-on-caller
  (let* ((transport
           (make-instance 'scripted-lobby-transport :fail-opens 100))
         (client
           (luv.lobby:make-lobby-client
            :client-id-prefix "test" :transport transport)))
    (unwind-protect
         (progn
           (luv.lobby:start-lobby-client client)
           (ok (wait-for-lobby-test
                (lambda ()
                  (eq :offline
                      (luv.lobby:lobby-snapshot-status
                       (luv.lobby:lobby-client-snapshot client))))))
           (let ((started (get-internal-real-time)))
             (luv.lobby:stop-lobby-client client)
             (ok (< (/ (- (get-internal-real-time) started)
                       internal-time-units-per-second)
                    0.25)))
           (ok (zerop (scripted-transport-closes transport))))
      (ignore-errors (luv.lobby:stop-lobby-client client)))))
