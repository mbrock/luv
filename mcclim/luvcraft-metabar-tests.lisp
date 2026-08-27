(in-package #:mcluv.surveyor-tests)

(define-test luvcraft-metabar-adapts-the-existing-knob-and-action-objects
  (let* ((session
           (allocate-instance (find-class 'luvcraft:luvcraft-session)))
         (knob
           (find-if
            (lambda (candidate)
              (handler-case
                  (progn (luvcraft:knob-value candidate session) t)
                (error () nil)))
            luvcraft:*knobs*))
         (group (and knob (luvcraft:knob-group knob)))
         (action (first luvcraft:*actions*)))
    (true knob)
    (true (member knob (mcluv:metabar-controls-for session group)
                  :test #'eq))
    (true (eq :scalar (mcluv:metabar-control-kind knob session)))
    (true (string= (luvcraft:knob-label knob)
                   (mcluv:metabar-control-label knob session)))
    (true (equalp (luvcraft:knob-value knob session)
                  (mcluv:metabar-control-value knob session)))
    (true action)
    (true (member action (mcluv:metabar-actions-for session) :test #'eq))
    (true (string= (luvcraft:action-label action)
                   (mcluv:metabar-action-label action session)))))
