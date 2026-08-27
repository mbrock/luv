(in-package #:luft.render.tests)

(define-test luft-tracy-capture-commands-join-the-shared-m-x-vocabulary
  (let* ((viewer (clim:make-application-frame 'render:viewer))
         (entries
           (mcluv:command-menu-entries-for-tables
            (mcluv:command-menu-tables-for viewer)
            :owner-frame viewer))
         (labels (mapcar #'mcluv:command-menu-entry-label entries)))
    (true (equal '(render::com-toggle-tracy-capture)
                 (render::viewer-key-command
                  viewer (key-press :f9))))
    (dolist (label '("Toggle Tracy Capture"
                     "Start Tracy Capture"
                     "Stop Tracy Capture"
                     "Open Last Tracy Capture"
                     "Reveal Last Tracy Capture"))
      (true (find label labels :test #'string=)))))

(define-test idle-tracy-lifecycle-attachment-does-not-open-an-overlay-pass
  (let* ((controller
           (luv.tracy.capture:make-tracy-capture-controller
            :application-name "LUFT test"
            :directory (uiop:temporary-directory)
            :open-on-completion-p nil))
         (instrument
           (make-instance 'render::viewer-tracy-capture-instrument
                          :controller controller)))
    (unwind-protect
         (true (null
                (render::viewer-instrument-present-p instrument nil)))
      (render::release-viewer-instrument instrument nil))
    (true (luv.tracy.capture:tracy-capture-controller-released-p
           controller))))
