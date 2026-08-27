(in-package #:luvcraft.clim.tests)

(defclass luvcraft-lobby-test-transport (luv.lobby:lobby-transport)
  ((opens :initform 0 :accessor luvcraft-lobby-test-opens)
   (closes :initform 0 :accessor luvcraft-lobby-test-closes)))

(defmethod luv.lobby:open-lobby-transport
    ((transport luvcraft-lobby-test-transport) client)
  (declare (ignore client))
  (incf (luvcraft-lobby-test-opens transport)))

(defmethod luv.lobby:subscribe-lobby-transport
    ((transport luvcraft-lobby-test-transport) connection client)
  (declare (ignore transport client))
  connection)

(defmethod luv.lobby:publish-lobby-transport
    ((transport luvcraft-lobby-test-transport) connection client payload)
  (declare (ignore transport connection client))
  payload)

(defmethod luv.lobby:next-lobby-publication
    ((transport luvcraft-lobby-test-transport) connection client)
  (declare (ignore transport connection client))
  (sleep 1/100)
  nil)

(defmethod luv.lobby:close-lobby-transport
    ((transport luvcraft-lobby-test-transport) connection client)
  (declare (ignore connection client))
  (incf (luvcraft-lobby-test-closes transport)))

(defclass test-luvcraft-lobby-overlay
    (mcluv::luvcraft-lobby-hud-overlay)
  ((released-p :initform nil :accessor test-lobby-overlay-released-p)))

(defmethod luvcraft:release-luvcraft-overlay
    ((overlay test-luvcraft-lobby-overlay))
  (setf (test-lobby-overlay-released-p overlay) t))

(defclass failing-luvcraft-lobby-overlay (test-luvcraft-lobby-overlay) ())

(defclass luvcraft-status-boundary-test-canvas (luv:canvas)
  ((requests :initform 0
             :accessor luvcraft-status-boundary-test-requests)))

(defmethod luv:request-canvas-frame
    ((canvas luvcraft-status-boundary-test-canvas) function)
  (incf (luvcraft-status-boundary-test-requests canvas))
  (funcall function 0d0))

(defmethod luvcraft:release-luvcraft-overlay
    ((overlay failing-luvcraft-lobby-overlay))
  (declare (ignore overlay))
  (error "scripted HUD release failure"))

(defun wait-for-luvcraft-lobby-test (predicate)
  (loop repeat 200
        when (funcall predicate) return t
        do (sleep 1/200)))

(defun make-test-luvcraft-lobby-session (overlay-class)
  (let* ((session (make-instance 'luvcraft:luvcraft-session))
         (transport (make-instance 'luvcraft-lobby-test-transport))
         (client
           (luv.lobby:make-lobby-client
            :client-id-prefix "luvcraft-test" :transport transport))
         (overlay
           (make-instance overlay-class
                          :session session :frame nil :mirror nil)))
    (setf (luvcraft:luvcraft-session-lobby-client session) client
          (luvcraft:luvcraft-session-overlays session) (list overlay))
    (values session client transport overlay)))

(define-test luvcraft-lobby-adapter-start-stop-and-restart-are-idempotent
  (multiple-value-bind (session client transport overlay)
      (make-test-luvcraft-lobby-session 'test-luvcraft-lobby-overlay)
    (unwind-protect
         (progn
           (true (eq client (luvcraft:start-luvcraft-lobby session)))
           (true (eq client (luvcraft:start-luvcraft-lobby session)))
           (true (wait-for-luvcraft-lobby-test
                  (lambda ()
                    (eq :online
                        (luv.lobby:lobby-snapshot-status
                         (luv.lobby:lobby-client-snapshot client))))))
           (true (null (luvcraft:stop-luvcraft-lobby session)))
           (true (test-lobby-overlay-released-p overlay))
           (true (null (luvcraft:luvcraft-session-overlays session)))
           (true (null (luvcraft:luvcraft-session-lobby-client session)))
           (true (not (luv.lobby:lobby-client-running-p client)))
           ;; Reinstalling the same stopped client exercises the adapter's
           ;; durable-image restart path without opening a real MQTT socket.
           (let ((fresh-overlay
                   (make-instance 'test-luvcraft-lobby-overlay
                                  :session session :frame nil :mirror nil)))
             (setf (luvcraft:luvcraft-session-lobby-client session) client
                   (luvcraft:luvcraft-session-overlays session)
                   (list fresh-overlay))
             (true (eq client (luvcraft:start-luvcraft-lobby session)))
             (true (wait-for-luvcraft-lobby-test
                    (lambda ()
                      (>= (luvcraft-lobby-test-opens transport) 2))))
             (true (null (luvcraft:stop-luvcraft-lobby session)))
             (true (test-lobby-overlay-released-p fresh-overlay)))
           (true (>= (luvcraft-lobby-test-closes transport) 2))
           (true (null (luvcraft:stop-luvcraft-lobby session))))
      (ignore-errors (luv.lobby:stop-lobby-client client)))))

(define-test luvcraft-lobby-radio-starts-without-the-detailed-panel
  (let* ((session (make-instance 'luvcraft:luvcraft-session))
         (transport (make-instance 'luvcraft-lobby-test-transport))
         (client
           (luv.lobby:make-lobby-client
            :client-id-prefix "luvcraft-test" :transport transport)))
    (setf (luvcraft:luvcraft-session-lobby-client session) client)
    (unwind-protect
         (progn
           (true (eq client (luvcraft:start-luvcraft-lobby session)))
           (true (wait-for-luvcraft-lobby-test
                  (lambda () (plusp (luvcraft-lobby-test-opens transport)))))
           (true (null (mcluv:luvcraft-lobby-hud-overlay session)))
           (true (null (luvcraft:luvcraft-session-overlays session))))
      (ignore-errors (luvcraft:stop-luvcraft-lobby session)))))

(define-test luvcraft-status-bar-is-the-default-shared-hud-without-a-lobby-panel
  (let* ((canvas (make-instance 'luvcraft-status-boundary-test-canvas))
         (session (make-instance 'luvcraft:luvcraft-session :canvas canvas))
         (overlay
           (make-instance 'mcluv:luvcraft-status-bar-overlay
                          :session session :frame nil :mirror nil)))
    (setf (luvcraft:luvcraft-session-overlays session) (list overlay))
    ;; Inventory's AROUND lifecycle and Status's AFTER lifecycle occupy
    ;; distinct coordinates instead of silently replacing one another.
    (true (find-method
           #'luvcraft:attach-luvcraft-hud '(:around)
           (list (find-class 'luvcraft:luvcraft-session)) nil))
    (true (find-method
           #'luvcraft:attach-luvcraft-hud '(:after)
           (list (find-class 'luvcraft:luvcraft-session)) nil))
    (true (eq overlay (mcluv:open-luvcraft-status-bar session)))
    (true (= 1 (luvcraft-status-boundary-test-requests canvas)))
    (true (eq :hud (luvcraft:luvcraft-overlay-stage overlay)))
    (true (eq overlay (mcluv:find-luvcraft-status-bar session)))
    (true (null (mcluv:luvcraft-lobby-hud-overlay session)))
    (true (equal '(:application :pid :fps :heap :lobby :worktree :chunks)
                 (mcluv:status-bar-channels-for session)))
    (true (string= "luvcraft"
                   (mcluv:status-bar-application-name session)))
    (true (null
           (luvcraft:handle-luvcraft-overlay-event
            overlay session canvas
            (make-instance 'luv:canvas-pointer-button-press-event
                           :timestamp 0 :x 12.0 :y 12.0
                           :button :left :clicks 1))))
    (true (null (luvcraft:luvcraft-session-modal-focus session)))
    (multiple-value-bind (left top right bottom)
        (luvcraft:luvcraft-overlay-focus-insets overlay session)
      (true (equal '(0.0 28 0.0 0.0) (list left top right bottom))))
    (setf (luvcraft:luvcraft-session-overlays session) nil)))

(define-test luvcraft-lobby-panel-close-crosses-the-canvas-boundary
  (let* ((canvas (make-instance 'luvcraft-status-boundary-test-canvas))
         (session (make-instance 'luvcraft:luvcraft-session :canvas canvas)))
    (true (null (mcluv:close-luvcraft-lobby-hud session)))
    (true (= 1 (luvcraft-status-boundary-test-requests canvas)))))

(define-test luvcraft-lobby-stop-attempts-radio-after-a-hud-release-error
  (multiple-value-bind (session client transport overlay)
      (make-test-luvcraft-lobby-session 'failing-luvcraft-lobby-overlay)
    (declare (ignore overlay))
    (unwind-protect
         (progn
           (luvcraft:start-luvcraft-lobby session)
           (true (wait-for-luvcraft-lobby-test
                  (lambda () (plusp (luvcraft-lobby-test-opens transport)))))
           (fail (luvcraft:stop-luvcraft-lobby session)
                 'luv:release-error)
           (true (null (luvcraft:luvcraft-session-overlays session)))
           (true (null (luvcraft:luvcraft-session-lobby-client session)))
           (true (not (luv.lobby:lobby-client-running-p client)))
           (true (plusp (luvcraft-lobby-test-closes transport))))
      (ignore-errors (luv.lobby:stop-lobby-client client)))))

(define-test telegram-credentials-resolve-from-the-playing-session-cache
  (let* ((client
           (luv.lobby:make-lobby-client
            :client-id-prefix "test"
            :transport (make-instance 'luv.lobby:lobby-transport)))
         (session (make-instance 'luvcraft:luvcraft-session))
         (luvcraft:*session* session))
    (setf (luvcraft:luvcraft-session-lobby-client session) client)
    (luv.lobby:receive-lobby-publication
     client "luv/store/TELEGRAM_API_ID" "12345")
    (true (string= "12345"
                   (luvcraft::lobby-telegram-credential "TELEGRAM_API_ID")))
    (true (null (luvcraft::lobby-telegram-credential "SOMETHING_ELSE")))))

(define-test telegram-console-recovers-when-radio-credentials-arrive-late
  (let ((console (make-instance 'mcluv::telegram-console))
        (telegram.client:*credential-files* nil)
        (telegram.client:*credential-fallbacks*
          (list (lambda (name)
                  (cond ((string= name "TELEGRAM_API_ID") "12345")
                        ((string= name "TELEGRAM_API_HASH") "deadbeef"))))))
    (setf (mcluv::console-login-stage console) :api-id)
    (true (mcluv::adopt-late-console-credentials console))
    (true (null (mcluv::console-login-stage console)))))
