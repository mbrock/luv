(in-package #:luft.render.tests)

(define-test luft-agent-vocabulary-is-application-level-and-parameterized
  (let* ((tool
           (luv.application-agent:make-command-tool
            'render::com-set-projection))
         (parameters (openai:tool-parameters tool))
         (properties (cdr (assoc "properties" parameters :test #'string=)))
         (projection
           (cdr (assoc "projection" properties :test #'string=))))
    (true (equal '("perspective" "isometric")
                 (cdr (assoc "enum" projection :test #'string=))))
    (true (equal '(render::com-set-projection :perspective)
                 (luv.application-agent:command-tool-parse
                  tool '(("projection" . "perspective")))))
    (true (every (lambda (command)
                   (eq (symbol-package command) (find-package '#:luft.render)))
                 render::*viewer-agent-commands*))))

(define-test luft-agent-participates-in-viewer-instrument-release
  (let* ((viewer (clim:make-application-frame 'render:viewer))
         (close-count 0)
         (agent
           (make-instance 'render::viewer-agent
                          :application viewer :model "test" :socket nil
                          :turn-function
                          (lambda (agent prompt)
                            (declare (ignore agent prompt)))
                          :close-function
                          (lambda (agent)
                            (declare (ignore agent))
                            (incf close-count)))))
    (render::attach-viewer-agent viewer agent)
    (true (eq agent (render::viewer-agent viewer)))
    (true (find-if (lambda (instrument)
                     (typep instrument 'render::viewer-agent-instrument))
                   (render::viewer-instruments viewer)))
    (render::release-viewer-instruments viewer)
    (true (luv.application-agent:wait-for-application-agent-release
           agent :timeout 1.0))
    (true (= 1 close-count))
    (true (null (render::viewer-agent viewer)))
    (true (null (luv.application-agent:release-application-agent agent)))))
