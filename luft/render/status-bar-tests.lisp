(in-package #:luft.render.tests)

(defclass viewer-status-boundary-test-canvas (luv:canvas)
  ((requests :initform 0 :accessor viewer-status-boundary-test-requests)))

(defmethod luv:canvas-state ((canvas viewer-status-boundary-test-canvas))
  (declare (ignore canvas))
  :open)

(defmethod luv:request-canvas-frame
    ((canvas viewer-status-boundary-test-canvas) function)
  (incf (viewer-status-boundary-test-requests canvas))
  (funcall function 0d0))

(define-test luft-status-bar-adds-game-fields-through-the-shared-clos-protocol
  (let ((viewer
          (clim:make-application-frame
           'render:viewer :source nil :player nil :bevel-width 2)))
    (true (equal '(:application :pid :fps :heap :lobby :worktree
                   :bevel :view :mode)
                 (mcluv:status-bar-channels-for viewer)))
    (true (string= "LUFT" (mcluv:status-bar-application-name viewer)))
    (true (string= "1/4"
                   (mcluv:status-bar-channel-value
                    :bevel viewer
                    (clim:make-application-frame
                     'mcluv:status-bar :owner viewer
                                        :logical-width 900 :worktree nil))))
    (true (string= "play"
                   (mcluv:status-bar-channel-value :mode viewer nil)))
    (setf (render:viewer-mode viewer) (make-instance 'render:world-edit-mode)
          (render::viewer-last-edit-status viewer) :selected)
    (true (string= "edit · terrain · selected"
                   (mcluv:status-bar-channel-value :mode viewer nil)))))

(define-test luft-status-bar-is-default-visible-instrument-above-radio
  (let ((viewer (gensym "VIEWER"))
        (status
          (make-instance 'render:viewer-status-bar
                         :frame nil :compositor nil))
        (radio
          (make-instance 'render::viewer-lobby-instrument
                         :client nil)))
    (unwind-protect
         (progn
           (render:add-viewer-instrument viewer radio)
           (render:add-viewer-instrument viewer status)
           (true (render:viewer-instrument-present-p status viewer))
           (true (not (render:viewer-instrument-present-p radio viewer)))
           (true (> (render:viewer-instrument-priority status)
                    (render:viewer-instrument-priority radio)))
           (true (equal (list status radio)
                        (render:viewer-instruments viewer))))
      ;; Frames are NIL in this lifecycle-only fixture; detach without calling
      ;; the release methods whose production contract owns real frames.
      (sb-thread:with-mutex (render::*viewer-instruments-lock*)
        (remhash viewer render::*viewer-instruments*)))))

(define-test luft-status-and-lobby-mutations-cross-the-canvas-boundary
  (let* ((canvas (make-instance 'viewer-status-boundary-test-canvas))
         (viewer
           (clim:make-application-frame
            'render:viewer :canvas canvas :source nil :player nil))
         (status
           (make-instance 'render:viewer-status-bar
                          :frame nil :compositor nil)))
    (unwind-protect
         (progn
           (render:add-viewer-instrument viewer status)
           (true (= 1 (viewer-status-boundary-test-requests canvas)))
           ;; An existing attachment lets this exercise the public frame-safe
           ;; path without constructing production GPU state in the fixture.
           (true (eq status (render:open-viewer-status-bar viewer)))
           (true (= 2 (viewer-status-boundary-test-requests canvas)))
           (true (null (render:close-viewer-lobby viewer)))
           (true (= 3 (viewer-status-boundary-test-requests canvas))))
      (sb-thread:with-mutex (render::*viewer-instruments-lock*)
        (remhash viewer render::*viewer-instruments*)))))
