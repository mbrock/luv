(defpackage #:luv.lobby.mcclim.tests
  (:use #:cl)
  (:import-from #:parachute #:define-test #:true #:false #:fail #:group #:skip))

(in-package #:luv.lobby.mcclim.tests)

(defclass lobby-preparation-probe ()
  ((revisions :initform nil :accessor lobby-preparation-probe-revisions)))

(defmethod mcluv::prepare-mirror-compositor-revision
    ((probe lobby-preparation-probe)
     (mirror mcluv:luv-gpu-mirror) revision)
  (declare (ignore mirror))
  (push revision (lobby-preparation-probe-revisions probe)))

(defun mount-static-lobby-preparation-probe (frame)
  (let* ((sheet
           (make-instance
            'mcluv:transparent-gpu-top-level-sheet-pane
            :region (clim:make-bounding-rectangle 0 0 320 140)))
         (mirror
           (make-instance 'mcluv:luv-gpu-mirror
                          :sheet sheet :target nil :context nil
                          :embedded-p t))
         (probe (make-instance 'lobby-preparation-probe))
         (revision
           (mcluv::make-gpu-mirror-prepared-revision
            mirror (list (mcluv::make-gpu-solid-command))
            #() #() #() #() #() #())))
    (setf (slot-value frame 'clim-internals::top-level-sheet) sheet
          (clim:sheet-direct-mirror sheet) mirror)
    (mcluv::publish-gpu-mirror-prepared-revision mirror revision)
    (setf (mcluv:mirror-compositor mirror) probe)
    (values probe revision)))

(define-test static-lobby-hud-prepares-live-shader-revisions
  (let* ((client
           (luv.lobby:make-lobby-client
            :client-id-prefix "test"
            :transport (make-instance 'luv.lobby:lobby-transport)))
         (snapshot (luv.lobby:lobby-client-snapshot client))
         (frame
           (clim:make-application-frame
            'luv.lobby.mcclim::lobby-hud
            :client client :visible-snapshot snapshot)))
    (setf (luv.lobby.mcclim::lobby-hud-painted-revision frame)
          (luv.lobby:lobby-snapshot-revision snapshot))
    (multiple-value-bind (probe revision)
        (mount-static-lobby-preparation-probe frame)
      (luv.lobby.mcclim:refresh-lobby-hud frame)
      (true (equal (list revision)
                   (lobby-preparation-probe-revisions probe))))))

(define-test lobby-hud-is-authored-in-logical-pixels-at-native-destination
  (let* ((logical '(1344 840))
         (drawable '(2688 1680))
         (state
           (luv.lobby.mcclim:lobby-hud-screen-state nil logical))
         (half-width (aref state 4))
         (half-height (aref state 9)))
    (true (< (abs (- 320.0 (* half-width (first logical)))) 1.0e-4))
    (true (< (abs (- 140.0 (* half-height (second logical)))) 1.0e-4))
    ;; The same logical affine receives two physical samples per authored
    ;; coordinate on a 2x drawable; there is no 320x140 panel raster.
    (true (< (abs (- 640.0 (* half-width (first drawable)))) 1.0e-4))
    (true (< (abs (- 280.0 (* half-height (second drawable)))) 1.0e-4))))

(define-test lobby-hud-panel-is-translucent-analytic-media
  (let ((medium (make-instance 'mcluv:luv-gpu-medium)))
    (setf (clim:medium-ink medium)
          luv.lobby.mcclim::*lobby-hud-panel-ink*)
    (mcluv::medium-draw-analytic-rounded-rectangle*
     medium 0 0 320 140 12 t)
    (let ((vertices (mcluv::gpu-medium-analytic-vertices medium))
          (commands (mcluv::gpu-medium-commands medium)))
      (true (< (abs (- 0.84 (aref vertices 2))) 1.0e-5))
      (true (= 1 (length commands)))
      (true (typep (aref commands 0) 'mcluv::gpu-analytic-command))
      (true (null (mcluv:gpu-medium-fallback-report medium)))
      (let* ((sheet
               (make-instance
                'luv.lobby.mcclim::lobby-hud-pane
                :region (clim:make-bounding-rectangle 0 0 320 140)))
             (mirror
               (make-instance 'mcluv:luv-gpu-mirror
                              :sheet sheet :target nil :embedded-p t)))
        (multiple-value-bind (prepared text-data)
            (mcluv::prepare-gpu-frame-commands mirror commands)
          (declare (ignore text-data))
          (true (= 1 (length prepared)))
          (true (null
                 (find-if
                  (lambda (command)
                    (typep command 'mcluv::gpu-prepared-image-command))
                  prepared))))))))

(define-test lobby-hud-snapshot-is-a-copied-frame-boundary
  (let* ((client
           (luv.lobby:make-lobby-client
            :client-id-prefix "test"
            :transport (make-instance 'luv.lobby:lobby-transport)))
         (before (luv.lobby:lobby-client-snapshot client)))
    (luv.lobby:receive-lobby-publication
     client "luv/presence/peer" "Daniel")
    (let ((after (luv.lobby:lobby-client-snapshot client)))
      ;; Already-published frame state does not alias the client's table.
      (true (null (luv.lobby:lobby-snapshot-peers before)))
      (true (equal '("Daniel")
                   (mapcar #'luv.lobby:lobby-peer-name
                           (luv.lobby:lobby-snapshot-peers after)))))))
