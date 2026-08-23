(in-package #:luft.render)

;;; LUFT's agent is an application instrument with no panel of its own.  The
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

;;; An invisible instrument gives the agent the viewer's ordinary transactional
;;; release path.  Registry mutation and provider close remain independently
;;; idempotent when stop, REPL cleanup, and error recovery race.

(defclass viewer-agent-instrument ()
  ((agent :initarg :agent :reader viewer-agent-instrument-agent)))

(defvar *viewer-agent-attachments*
  (make-hash-table :test #'eq :weakness :key))

(defvar *viewer-agent-attachments-lock*
  (sb-thread:make-mutex :name "LUFT viewer agents"))

(defmethod viewer-instrument-present-p
    ((instrument viewer-agent-instrument) viewer)
  (declare (ignore instrument viewer))
  nil)

(defmethod release-viewer-instrument
    ((instrument viewer-agent-instrument) viewer)
  (sb-thread:with-mutex (*viewer-agent-attachments-lock*)
    (when (eq instrument (gethash viewer *viewer-agent-attachments*))
      (remhash viewer *viewer-agent-attachments*)))
  (luv.application-agent:release-application-agent
   (viewer-agent-instrument-agent instrument)))

(defun viewer-agent-attachment (viewer)
  (sb-thread:with-mutex (*viewer-agent-attachments-lock*)
    (gethash viewer *viewer-agent-attachments*)))

(defun viewer-agent (viewer)
  "Return VIEWER's live application agent, or NIL."
  (alexandria:when-let ((attachment (viewer-agent-attachment viewer)))
    (let ((agent (viewer-agent-instrument-agent attachment)))
      (and (luv.application-agent:application-agent-open-p agent) agent))))

(defun attach-viewer-agent (viewer agent)
  "Attach already-open AGENT transactionally, releasing a concurrent loser."
  (let ((attachment (make-instance 'viewer-agent-instrument :agent agent))
        (installed-p nil)
        (winner nil))
    (sb-thread:with-mutex (*viewer-agent-attachments-lock*)
      (let ((present (gethash viewer *viewer-agent-attachments*)))
        (if present
            (setf winner (viewer-agent-instrument-agent present))
            (setf (gethash viewer *viewer-agent-attachments*) attachment
                  winner agent
                  installed-p t))))
    (if installed-p
        (progn
          (add-viewer-instrument viewer attachment)
          ;; A concurrent RELEASE-VIEWER-AGENT may have detached and closed it
          ;; between registry publication and instrument attachment.
          (unless (luv.application-agent:application-agent-open-p agent)
            (remove-viewer-instrument viewer attachment)
            (error 'luv.application-agent:application-agent-released
                   :agent agent))
          agent)
        (progn
          (luv.application-agent:release-application-agent agent)
          winner))))

(defun viewer-openai-api-key (viewer)
  (or (openai:default-api-key)
      (alexandria:when-let
          ((attachment (viewer-lobby-attachment viewer)))
        (luv.lobby:lobby-client-value
         (viewer-lobby-client attachment) "OPENAI_API_KEY"))))

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

(defun release-viewer-agent (viewer)
  "Detach and close VIEWER's application agent if one exists."
  (let ((attachment nil))
    (sb-thread:with-mutex (*viewer-agent-attachments-lock*)
      (setf attachment (gethash viewer *viewer-agent-attachments*))
      (when attachment
        (remhash viewer *viewer-agent-attachments*)))
    (when attachment
      ;; If creation has published the registry but not attached the instrument
      ;; yet, release it directly; the creator detects the terminal state.
      (unless (remove-viewer-instrument viewer attachment)
        (release-viewer-instrument attachment viewer))
      t)))
