(in-package #:luft.render.tests)

(defclass viewer-lobby-test-transport (luv.lobby:lobby-transport)
  ((closed-p :initform nil :accessor viewer-lobby-test-closed-p)))

(defmethod luv.lobby:open-lobby-transport
    ((transport viewer-lobby-test-transport) client)
  (declare (ignore transport client))
  :connection)

(defmethod luv.lobby:subscribe-lobby-transport
    ((transport viewer-lobby-test-transport) connection client)
  (declare (ignore transport client))
  connection)

(defmethod luv.lobby:publish-lobby-transport
    ((transport viewer-lobby-test-transport) connection client payload)
  (declare (ignore transport connection client))
  payload)

(defmethod luv.lobby:next-lobby-publication
    ((transport viewer-lobby-test-transport) connection client)
  (declare (ignore transport connection client))
  (sleep 1/100)
  nil)

(defmethod luv.lobby:close-lobby-transport
    ((transport viewer-lobby-test-transport) connection client)
  (declare (ignore connection client))
  (setf (viewer-lobby-test-closed-p transport) t))

(define-test viewer-lobby-is-a-low-priority-lifecycle-owned-instrument
  (let* ((viewer (gensym "VIEWER"))
         (transport (make-instance 'viewer-lobby-test-transport))
         (client
           (luv.lobby:start-lobby-client
            (luv.lobby:make-lobby-client
             :client-id-prefix "luft-test" :transport transport)))
         (instrument
           (make-instance 'render::viewer-lobby-instrument
                          :client client :frame nil :compositor nil)))
    (unwind-protect
         (progn
           (true (loop repeat 200
                       when (eq :online
                                (luv.lobby:lobby-snapshot-status
                                 (luv.lobby:lobby-client-snapshot client)))
                         return t
                       do (sleep 1/200)))
           (render:add-viewer-instrument viewer instrument)
           (true (eq instrument (render::viewer-lobby-attachment viewer)))
           (true (= 10 (render:viewer-instrument-priority instrument)))
           (true (not (render:viewer-instrument-present-p instrument viewer)))
           (true (render:remove-viewer-instrument viewer instrument))
           (true (null (render::viewer-lobby-attachment viewer)))
           (true (not (luv.lobby:lobby-client-running-p client)))
           (true (viewer-lobby-test-closed-p transport)))
      (ignore-errors (luv.lobby:stop-lobby-client client))
      (render::release-viewer-instruments viewer))))
