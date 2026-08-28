(defpackage #:luv.workbench.tests
  (:use #:cl)
  (:import-from #:parachute
                #:define-test #:true #:false #:is #:fail)
  (:local-nicknames (#:workbench #:luv.workbench)
                    (#:mcluv #:mcluv)
                    (#:luv #:luv)))

(in-package #:luv.workbench.tests)

(defclass test-application (workbench:workbench-application)
  ((suspensions :initform nil :accessor test-suspensions)
   (resumptions :initform nil :accessor test-resumptions)))

(defmethod workbench:suspend-workbench-application-input
    ((application test-application))
  (let ((token (list :suspension)))
    (push token (test-suspensions application))
    token))

(defmethod workbench:resume-workbench-application-input
    ((application test-application) token)
  (push token (test-resumptions application)))

(defclass test-mirror (mcluv:luv-mirror)
  ((event-action :initform nil :accessor test-mirror-event-action)))

(defmethod luv:handle-canvas-event
    ((mirror test-mirror) canvas event)
  (declare (ignore canvas))
  (alexandria:when-let ((action (test-mirror-event-action mirror)))
    (funcall action event))
  nil)

(defclass test-workbench (workbench:workbench)
  ((hit-kind :initform nil :accessor test-hit-kind)))

(defmethod workbench::workbench-event-hits-layer-p
    ((shell test-workbench) layer event target)
  (declare (ignore event target))
  (eq (test-hit-kind shell) (workbench:workbench-layer-kind layer)))

(defun make-test-workbench ()
  (let* ((application (make-instance 'test-application))
         (canvas (make-instance 'luv:canvas))
         (events nil)
         (mirror (make-instance 'test-mirror :sheet nil :target canvas
                                             :embedded-p t))
         (layers
           (mapcar
            (lambda (kind)
              (make-instance 'workbench::workbench-layer
                             :kind kind
                             :pane (make-instance 'clim:basic-pane)))
            '(:passive :modeless :transient :modal)))
         (shell
           (make-instance
            'test-workbench :application application :frame nil :mirror mirror
            :compositor nil :layers layers
            :downstream-handler
            (lambda (ignored-canvas event)
              (declare (ignore ignored-canvas))
              (push event events)))))
    (values shell application canvas (lambda () events) mirror)))

(defun key-event (class key)
  (make-instance class :timestamp 0 :key-name key))

(defun button-event (class button)
  (make-instance class :timestamp 0 :x 12 :y 14 :button button))

(defun set-layer-visible (shell kind visible-p)
  (setf (workbench:workbench-layer-visible-p
         (workbench:workbench-layer shell kind))
        visible-p))

(define-test layer-state-is-semantic-not-numeric-priority
  (multiple-value-bind (shell application canvas events) (make-test-workbench)
    (declare (ignore application canvas events))
    (set-layer-visible shell :passive t)
    (false (workbench:workbench-active-layer shell))
    (set-layer-visible shell :modeless t)
    (is eq :modeless
        (workbench:workbench-layer-kind
         (workbench:workbench-active-layer shell)))
    (set-layer-visible shell :transient t)
    (is eq :transient
        (workbench:workbench-layer-kind
         (workbench:workbench-active-layer shell)))
    (set-layer-visible shell :modal t)
    (is eq :modal
        (workbench:workbench-layer-kind
         (workbench:workbench-active-layer shell)))))

(define-test transparent-layer-root-is-not-an-authored-hit
  (let* ((root (make-instance 'workbench::workbench-layout-pane))
         (child (make-instance 'clim:basic-pane))
         (layer (make-instance 'workbench::workbench-layer
                               :kind :modeless :pane root))
         (shell (make-instance 'workbench:workbench
                               :application nil :frame nil :mirror nil
                               :compositor nil :layers (list layer)
                               :downstream-handler nil)))
    (clim:sheet-adopt-child root child)
    (false (workbench::workbench-event-hits-layer-p shell layer nil root))
    (true (workbench::workbench-event-hits-layer-p shell layer nil child))))

(define-test shell-first-routing-obeys-layer-behavior
  (multiple-value-bind (shell application canvas events) (make-test-workbench)
    (declare (ignore application))
    (let ((press (button-event 'luv:canvas-pointer-button-press-event :left)))
      ;; Passive never stops application input.
      (set-layer-visible shell :passive t)
      (luv:handle-canvas-event shell canvas press)
      (true (= 1 (length (funcall events))))
      ;; Modeless consumes only McCLIM hits in its own sheet tree.
      (set-layer-visible shell :modeless t)
      (setf (test-hit-kind shell) nil)
      (luv:handle-canvas-event shell canvas press)
      (true (= 2 (length (funcall events))))
      (setf (test-hit-kind shell) :modeless)
      (luv:handle-canvas-event shell canvas press)
      (true (= 2 (length (funcall events))))
      ;; Modal captures independently of hit result.
      (set-layer-visible shell :modal t)
      (setf (test-hit-kind shell) nil)
      (luv:handle-canvas-event shell canvas press)
      (true (= 2 (length (funcall events)))))))

(define-test transient-outside-press-dismisses-and-swallows-release
  (multiple-value-bind (shell application canvas events) (make-test-workbench)
    (declare (ignore application))
    (set-layer-visible shell :transient t)
    (setf (test-hit-kind shell) nil)
    (luv:handle-canvas-event
     shell canvas
     (button-event 'luv:canvas-pointer-button-press-event :left))
    (false (workbench:workbench-layer-visible-p
            (workbench:workbench-layer shell :transient)))
    (luv:handle-canvas-event
     shell canvas
     (button-event 'luv:canvas-pointer-button-release-event :left))
    (false (funcall events))))

(define-test opening-interactive-ui-clears-held-input-and-swallows-release
  (multiple-value-bind (shell application canvas events) (make-test-workbench)
    (let ((press (key-event 'luv:canvas-key-press-event :w))
          (release (key-event 'luv:canvas-key-release-event :w)))
      (luv:handle-canvas-event shell canvas press)
      (workbench:show-workbench-layer shell :modeless)
      (true (workbench:workbench-input-suspended-p shell))
      (true (= 1 (length (test-suspensions application))))
      ;; Higher interactive state takes focus without acquiring a second
      ;; application token; hiding it restores the next semantic layer.
      (workbench:show-workbench-layer shell :modal)
      (true (= 1 (length (test-suspensions application))))
      (workbench:hide-workbench-layer shell :modal)
      (is eq :modeless
          (workbench:workbench-layer-kind
           (workbench:workbench-active-layer shell)))
      (true (workbench:workbench-input-suspended-p shell))
      (workbench:hide-workbench-layer shell :modeless)
      (luv:handle-canvas-event shell canvas release)
      (true (= 1 (length (funcall events))))
      (true (= 1 (length (test-resumptions application)))))))

(define-test shell-consumed-press-cannot-orphan-release-after-layer-closes
  (multiple-value-bind (shell application canvas events mirror)
      (make-test-workbench)
    (declare (ignore application))
    (workbench:show-workbench-layer shell :modal)
    ;; Model the authored action in the real synchronous mirror dispatch: it
    ;; closes its own final interactive layer while handling the press.
    (setf (test-mirror-event-action mirror)
          (lambda (event)
            (when (typep event 'luv:canvas-key-press-event)
              (workbench:hide-workbench-layer shell :modal))))
    (luv:handle-canvas-event
     shell canvas (key-event 'luv:canvas-key-press-event :escape))
    (false (workbench:workbench-active-layer shell))
    (luv:handle-canvas-event
     shell canvas (key-event 'luv:canvas-key-release-event :escape))
    (false (funcall events))
    (true (zerop (hash-table-count
                  (workbench::workbench-shell-held-input shell))))))

(define-test stop-controller-closes-workbench-admission-atomically
  (let ((controller (luv:make-stop-controller :name "workbench admission"))
        (published-p nil)
        (attachment (list :workbench)))
    (luv:call-with-stop-controller controller (lambda () :stopped))
    (fail
     (luv:call-with-running-stop-controller
      controller (lambda () (setf published-p t))
      :attachment attachment)
     'luv:application-attachment-closed)
    (false published-p)))

(defclass lifecycle-canvas (luv:canvas)
  ((events :initarg :events :reader lifecycle-events)))

(defmethod luv:request-canvas-frame ((canvas lifecycle-canvas) function)
  (push :fence (car (lifecycle-events canvas)))
  (funcall function 0))

(defclass lifecycle-application (test-application)
  ((canvas :initarg :canvas :reader lifecycle-application-canvas)
   (context :initform (make-instance 'luv:canvas-context)
            :reader lifecycle-application-context)
   (controller :initform (luv:make-stop-controller :name "lifecycle fixture")
               :reader lifecycle-application-controller)))

(defmethod workbench:workbench-application-canvas
    ((application lifecycle-application))
  (lifecycle-application-canvas application))

(defmethod workbench:workbench-application-context
    ((application lifecycle-application))
  (lifecycle-application-context application))

(defmethod workbench:workbench-application-stop-controller
    ((application lifecycle-application))
  (lifecycle-application-controller application))

(defclass lifecycle-workbench (workbench:workbench)
  ((events :initarg :events :reader lifecycle-workbench-events)))

(defmethod workbench:quiesce-workbench ((shell lifecycle-workbench))
  (push :quiesce (car (lifecycle-workbench-events shell)))
  shell)

(defmethod workbench::release-workbench-resources
    ((shell lifecycle-workbench))
  (push :release (car (lifecycle-workbench-events shell)))
  shell)

(define-test quiescence-precedes-fence-and-resources-release-before-device-owner
  (let* ((events (list nil))
         (canvas (make-instance 'lifecycle-canvas :events events))
         (application (make-instance 'lifecycle-application :canvas canvas))
         (shell
           (make-instance
            'lifecycle-workbench :application application :frame nil
            :mirror nil :compositor nil :layers nil :downstream-handler
            :application :events events)))
    (setf (luv:canvas-event-handler canvas) shell)
    (sb-thread:with-mutex (workbench::*application-workbenches-lock*)
      (setf (gethash application workbench::*application-workbenches*) shell))
    (workbench:stop-workbench shell)
    (true (equal '(:quiesce :fence :release) (nreverse (car events))))
    (is eq :application (luv:canvas-event-handler canvas))
    (false (workbench:application-workbench application))))
