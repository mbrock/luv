(in-package #:luft.render)

;;; LUFT's agent is a named application service with no panel of its own. The
;;; shared harness supplies turns, tools, transcript, and teardown; this file
;;; contributes a viewer report, a small atelier command vocabulary, and the
;;; canvas/frame relationship.  There is deliberately no gnome, cat, body, or
;;; block-world language here.

(defclass viewer-agent (luv.application-agent:application-agent) ())

(defclass viewer-agent-report ()
  ((projection :initarg :projection :reader viewer-report-projection)
   (camera-position :initarg :camera-position
                    :reader viewer-report-camera-position)
   (bevel-width :initarg :bevel-width :reader viewer-report-bevel-width)
   (construction-lines-p :initarg :construction-lines-p
                         :reader viewer-report-construction-lines-p)
   (inspection :initarg :inspection :reader viewer-report-inspection)))

(clim:define-presentation-method clim:present
    (object (type viewer-agent-report) stream (view clim:textual-view) &key)
  (let ((position (viewer-report-camera-position object)))
    (format stream
            "LUFT uses ~(~A~) projection; camera ~,2F, ~,2F, ~,2F; bevel ~A; construction lines ~:[off~;on~]~:[.~;, inspecting a boundary site.~]"
            (viewer-report-projection object)
            (vec3:vec3-x position)
            (vec3:vec3-y position)
            (vec3:vec3-z position)
            (bevel-width-label (viewer-report-bevel-width object))
            (viewer-report-construction-lines-p object)
            (viewer-report-inspection object))))

(defun make-viewer-agent-report (viewer)
  (make-instance 'viewer-agent-report
                 :projection *projection*
                 :camera-position (camera-position (viewer-camera viewer))
                 :bevel-width (viewer-bevel-width viewer)
                 :construction-lines-p (plusp *wireframe*)
                 :inspection (viewer-inspection viewer)))

(clim:define-command (com-describe-viewer
                      :command-table luft-atelier
                      :name "Describe Viewer")
    ()
  "Report LUFT's current camera and construction presentation state."
  (make-viewer-agent-report (viewer-command-viewer)))

(clim:define-command (com-set-projection
                      :command-table luft-atelier
                      :name "Set Projection")
    ((projection '(member :perspective :isometric)
                 :prompt "perspective or isometric"))
  "Set LUFT's camera projection to perspective or isometric."
  (setf *projection* projection)
  (let ((viewer (viewer-command-viewer)))
    (when (viewer-renderer viewer)
      (setf (renderer-history-valid-p (viewer-renderer viewer)) nil))
    (make-viewer-agent-report viewer)))

(defparameter *viewer-agent-model* "gpt-5.6")

(defparameter *viewer-agent-commands*
  '(com-describe-viewer com-set-projection com-reset-view
    com-toggle-construction-lines com-toggle-bevel-width))

(defparameter *viewer-agent-instructions*
  "You are the application agent for LUFT, a live Common Lisp solid-model
atelier and game experiment. Use the viewer's own commands to inspect or alter
its developer presentation. Keep answers short and prefer acting through tools.")

(defmethod luv.application-agent:application-command-frame ((viewer viewer))
  viewer)

(defmethod luv.application-agent:call-in-application-frame
    ((viewer viewer) function)
  (request-canvas-frame
   (viewer-canvas viewer)
   (lambda (timestamp)
     (declare (ignore timestamp))
     (funcall function))))

;;; The viewer directly owns its environment-level agent. Provider work stays
;;; on the shared harness threads; the service lock protects only publication.

(defun viewer-agent (viewer)
  "Return VIEWER's live application agent, or NIL."
  (sb-thread:with-mutex ((viewer-service-lock viewer))
    (let ((agent (viewer-agent-service viewer)))
      (and agent
           (luv.application-agent:application-agent-open-p agent)
           agent))))

(defun attach-viewer-agent (viewer agent)
  "Attach already-open AGENT transactionally, releasing a concurrent loser."
  (let ((winner nil)
        (transferred-p nil)
        (released-p nil))
    (unwind-protect-releasing
        (progn
          (setf winner
                (call-with-running-stop-controller
                 (viewer-stop-controller viewer)
                 (lambda ()
                   (sb-thread:with-mutex ((viewer-service-lock viewer))
                     (or (viewer-agent-service viewer)
                         (progn
                           (setf transferred-p t
                                 (viewer-agent-service viewer) agent)
                           agent))))
                 :attachment agent))
          (unless transferred-p
            (setf released-p t)
            (luv.application-agent:release-application-agent agent))
          ;; A concurrent RELEASE-VIEWER-AGENT may detach and close a newly
          ;; published winner before this creator returns.
          (unless (luv.application-agent:application-agent-open-p winner)
            (error 'luv.application-agent:application-agent-released
                   :agent winner))
          winner)
      (unless (or transferred-p released-p)
        (releasing :unpublished-agent
          (luv.application-agent:release-application-agent agent))))))

(defun viewer-openai-api-key (viewer)
  (or (openai:default-api-key)
      (alexandria:when-let ((client (viewer-lobby-client viewer)))
        (luv.lobby:lobby-client-value client "OPENAI_API_KEY"))))

(defun make-viewer-agent
    (viewer &key (model *viewer-agent-model*)
              (commands *viewer-agent-commands*)
              (instructions *viewer-agent-instructions*)
              (reasoning-summary "auto") reasoning-effort
              (api-key (viewer-openai-api-key viewer)))
  "Open and attach VIEWER's environment-level agent.

Call this from SLY or another worker, never from a canvas command: opening a
provider connection is application setup, while ASK itself returns at once."
  (or (viewer-agent viewer)
      (attach-viewer-agent
       viewer
       (luv.application-agent:make-application-agent
        :class 'viewer-agent :application viewer
        :model model :commands commands :instructions instructions
        :reasoning-summary reasoning-summary
        :reasoning-effort reasoning-effort :api-key api-key))))

(defun ask-viewer-agent (viewer text)
  "Start a provider turn for VIEWER and return its TURN immediately."
  (luv.application-agent:ask
   text :agent
   (or (viewer-agent viewer)
       (error "No LUFT agent is open; call MAKE-VIEWER-AGENT off-canvas."))))

(defun release-viewer-agent (viewer &key wait)
  "Detach and close VIEWER's application agent if one exists.

With WAIT, require provider and worker quiescence before returning."
  (let ((agent nil))
    (sb-thread:with-mutex ((viewer-service-lock viewer))
      (setf agent (viewer-agent-service viewer)
            (viewer-agent-service viewer) nil))
    (when agent
      (luv.application-agent:release-application-agent agent)
      (when (and wait
                 (not (luv.application-agent:wait-for-application-agent-release
                       agent :timeout 3)))
        ;; This off-canvas teardown owner must not fence or destroy resources
        ;; while an active turn can still submit an application call. Three
        ;; seconds is a diagnostic threshold, not an unsafe teardown deadline.
        (format *error-output*
                "LUFT application agent is still quiescing after 3 seconds; ~
                 waiting before the terminal canvas fence.~%")
        (finish-output *error-output*)
        (luv.application-agent:wait-for-application-agent-release agent))
      t)))

(defun quiesce-viewer-services (viewer)
  "Detach and quiesce every named nonvisual service before the terminal fence."
  (with-release-report
    (releasing :application-agent
      (release-viewer-agent viewer :wait t))
    (releasing :tracy-capture
      (release-viewer-tracy-capture viewer))
    (releasing :lobby-radio
      (release-viewer-lobby viewer)))
  viewer)
