(in-package #:luft.render.tests)

(defun make-luft-metabar-test-frame (viewer)
  (let* ((vocabulary (mcluv::capture-metabar-vocabulary viewer))
         (height (mcluv::metabar-natural-height-for viewer vocabulary)))
    (clim:make-application-frame
     'mcluv:metabar :owner viewer :vocabulary vocabulary
                     :logical-height height)))

(define-test luft-metabar-is-the-viewers-open-setting-protocol
  (let ((viewer
          (clim:make-application-frame
           'render:viewer :source nil :player nil
                          :speed 4.0 :sensitivity 0.0032))
        (render:*wireframe* 0.0))
    (true (equal '(:geometry :navigation)
                 (mcluv:metabar-groups-for viewer)))
    (true (equal '(:bevel-width :construction-lines)
                 (mcluv:metabar-controls-for viewer :geometry)))
    (true (equal '(:movement-speed :mouse-sensitivity)
                 (mcluv:metabar-controls-for viewer :navigation)))
    (true (equal '(:reset-view :quit)
                 (mcluv:metabar-actions-for viewer)))
    (true (eq :rebuild
              (mcluv:metabar-control-change-kind :bevel-width viewer)))
    (true (eq :commit-on-release
              (mcluv:metabar-control-update-policy :bevel-width viewer)))
    (true (string= "1/4"
                   (mcluv:metabar-control-value-label
                    :bevel-width viewer
                    (mcluv:metabar-control-value :bevel-width viewer))))
    (true (= 1/2
             (mcluv:metabar-control-fraction :bevel-width viewer 2)))
    (true (not (mcluv:metabar-control-value :construction-lines viewer)))
    (mcluv:perform-metabar-control-toggle :construction-lines viewer)
    (true (mcluv:metabar-control-value :construction-lines viewer))
    (mcluv:perform-metabar-control-step :movement-speed viewer 1 2)
    (true (= 5.0 (render::viewer-speed viewer)))
    (mcluv:perform-metabar-control-set-fraction
     :mouse-sensitivity viewer 1.0)
    (true (< (abs (- 0.01 (render::viewer-sensitivity viewer))) 1.0e-7))))

(define-test luft-metabar-input-does-not-run-viewer-work
  (let* ((viewer
           (clim:make-application-frame
            'render:viewer :source nil :player nil :speed 4.0))
         (frame (make-luft-metabar-test-frame viewer)))
    (mcluv::open-metabar-group frame :navigation)
    (let ((index
            (position :movement-speed (mcluv::metabar-rows frame)
                      :key #'mcluv::metabar-row-subject)))
      (true index)
      (mcluv::select-metabar-row frame index)
      (true (eq :continue
                (mcluv:handle-metabar-key-event
                 frame (key-press :right))))
      ;; Input has changed only the instrument's retained semantic state.
      (true (= 4.0 (render::viewer-speed viewer)))
      (true (= 1 (length (mcluv::metabar-pending-operations frame))))
      ;; The application method runs at the viewer's next refresh boundary.
      (mcluv:drain-metabar-operations frame)
      (true (= 4.5 (render::viewer-speed viewer)))
      (true (null (mcluv::metabar-pending-operations frame))))))

(define-test luft-metabar-shares-the-instrument-stack-below-m-x
  (let ((metabar
          (make-instance 'render::viewer-metabar-instrument
                         :frame nil :compositor nil))
        (command-menu
          (make-instance 'render::viewer-command-menu-instrument
                         :frame nil :compositor nil))
        (viewer
          (clim:make-application-frame
           'render:viewer :source nil :player nil)))
    (true (= 500 (render:viewer-instrument-priority metabar)))
    (true (> (render:viewer-instrument-priority command-menu)
             (render:viewer-instrument-priority metabar)))
    (setf (render::viewer-metabar-open-p metabar) nil
          (render::viewer-metabar-slide metabar) 0d0)
    ;; Even an immediately dismissed drawer receives the refresh which
    ;; detaches it; otherwise the dispatcher's present guard would leak it.
    (true (render:viewer-instrument-present-p metabar viewer))
    (let ((entries
            (mcluv:command-menu-entries-for-tables
             (mcluv:command-menu-tables-for viewer)
             :owner-frame viewer)))
      (true (find 'render::com-toggle-metabar entries
                  :key #'mcluv:command-menu-entry-command-name)))))
