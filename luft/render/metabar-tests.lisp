(in-package #:luft.render.tests)

(defun make-luft-metabar-test-frame (viewer)
  (let* ((vocabulary (mcluv::capture-metabar-vocabulary viewer))
         (height (mcluv::metabar-natural-height-for viewer vocabulary)))
    (clim:make-application-frame
     'mcluv:metabar :owner viewer :vocabulary vocabulary
                     :logical-height height)))

(deftest luft-metabar-is-the-viewers-open-setting-protocol
  (let ((viewer
          (clim:make-application-frame
           'render:viewer :source nil :player nil
                          :speed 4.0 :sensitivity 0.0032))
        (render:*wireframe* 0.0))
    (ok (equal '(:geometry :navigation)
               (mcluv:metabar-groups-for viewer)))
    (ok (equal '(:bevel-width :construction-lines)
               (mcluv:metabar-controls-for viewer :geometry)))
    (ok (equal '(:movement-speed :mouse-sensitivity)
               (mcluv:metabar-controls-for viewer :navigation)))
    (ok (equal '(:reset-view :quit)
               (mcluv:metabar-actions-for viewer)))
    (ok (eq :rebuild
            (mcluv:metabar-control-change-kind :bevel-width viewer)))
    (ok (eq :commit-on-release
            (mcluv:metabar-control-update-policy :bevel-width viewer)))
    (ok (string= "1/4"
                 (mcluv:metabar-control-value-label
                  :bevel-width viewer
                  (mcluv:metabar-control-value :bevel-width viewer))))
    (ok (= 1/2
           (mcluv:metabar-control-fraction :bevel-width viewer 2)))
    (ok (not (mcluv:metabar-control-value :construction-lines viewer)))
    (mcluv:perform-metabar-control-toggle :construction-lines viewer)
    (ok (mcluv:metabar-control-value :construction-lines viewer))
    (mcluv:perform-metabar-control-step :movement-speed viewer 1 2)
    (ok (= 5.0 (render::viewer-speed viewer)))
    (mcluv:perform-metabar-control-set-fraction
     :mouse-sensitivity viewer 1.0)
    (ok (< (abs (- 0.01 (render::viewer-sensitivity viewer))) 1.0e-7))))

(deftest luft-metabar-input-does-not-run-viewer-work
  (let* ((viewer
           (clim:make-application-frame
            'render:viewer :source nil :player nil :speed 4.0))
         (frame (make-luft-metabar-test-frame viewer)))
    (mcluv::open-metabar-group frame :navigation)
    (let ((index
            (position :movement-speed (mcluv::metabar-rows frame)
                      :key #'mcluv::metabar-row-subject)))
      (ok index)
      (mcluv::select-metabar-row frame index)
      (ok (eq :continue
              (mcluv:handle-metabar-key-event
               frame (key-press :right))))
      ;; Input has changed only the instrument's retained semantic state.
      (ok (= 4.0 (render::viewer-speed viewer)))
      (ok (= 1 (length (mcluv::metabar-pending-operations frame))))
      ;; The application method runs at the viewer's next refresh boundary.
      (mcluv:drain-metabar-operations frame)
      (ok (= 4.5 (render::viewer-speed viewer)))
      (ok (null (mcluv::metabar-pending-operations frame))))))

(deftest luft-metabar-shares-the-instrument-stack-below-m-x
  (let ((metabar
          (make-instance 'render::viewer-metabar-instrument
                         :frame nil :compositor nil))
        (command-menu
          (make-instance 'render::viewer-command-menu-instrument
                         :frame nil :compositor nil))
        (viewer
          (clim:make-application-frame
           'render:viewer :source nil :player nil)))
    (ok (= 500 (render:viewer-instrument-priority metabar)))
    (ok (> (render:viewer-instrument-priority command-menu)
           (render:viewer-instrument-priority metabar)))
    (setf (render::viewer-metabar-open-p metabar) nil
          (render::viewer-metabar-slide metabar) 0d0)
    ;; Even an immediately dismissed drawer receives the refresh which
    ;; detaches it; otherwise the dispatcher's present guard would leak it.
    (ok (render:viewer-instrument-present-p metabar viewer))
    (let ((entries
            (mcluv:command-menu-entries-for-tables
             (mcluv:command-menu-tables-for viewer)
             :owner-frame viewer)))
      (ok (find 'render::com-toggle-metabar entries
                :key #'mcluv:command-menu-entry-command-name)))))
