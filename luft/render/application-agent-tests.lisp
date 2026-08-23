(in-package #:luft.render.tests)

(deftest luft-agent-vocabulary-is-application-level-and-parameterized
  (let* ((tool
           (luv.application-agent:make-command-tool
            'render::com-set-projection))
         (parameters (openai:tool-parameters tool))
         (properties (cdr (assoc "properties" parameters :test #'string=)))
         (projection
           (cdr (assoc "projection" properties :test #'string=))))
    (ok (equal '("perspective" "isometric")
               (cdr (assoc "enum" projection :test #'string=))))
    (ok (equal '(render::com-set-projection :perspective)
               (luv.application-agent:command-tool-parse
                tool '(("projection" . "perspective")))))
    (ok (every (lambda (command)
                 (eq (symbol-package command) (find-package '#:luft.render)))
               render::*viewer-agent-commands*))))

(deftest luft-agent-participates-in-viewer-instrument-release
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
    (ok (eq agent (render::viewer-agent viewer)))
    (ok (find-if (lambda (instrument)
                   (typep instrument 'render::viewer-agent-instrument))
                 (render::viewer-instruments viewer)))
    (render::release-viewer-instruments viewer)
    (ok (luv.application-agent:wait-for-application-agent-release
         agent :timeout 1.0))
    (ok (= 1 close-count))
    (ok (null (render::viewer-agent viewer)))
    (ok (null (luv.application-agent:release-application-agent agent)))))
