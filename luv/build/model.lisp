(in-package #:luv.build)

;;; Retained state shared by headless and interactive presenters.

(defparameter +terminal-run-states+
  '(:succeeded :failed :cancelled :interrupted :deadline))

(defparameter +build-id-characters+ "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")

(defun make-build-id ()
  (let ((state (make-random-state t)))
    (coerce
     (loop repeat 6
           collect (char +build-id-characters+
                         (random (length +build-id-characters+) state)))
     'string)))

(defclass run ()
  ((id :initarg :id :initform (make-build-id) :reader run-id)
   (state :initarg :state :initform :pending :accessor run-state)
   (revision :initform 0 :accessor run-revision)
   (started-at :initform nil :accessor run-started-at)
   (ended-at :initform nil :accessor run-ended-at)
   (cancellation-requested-p
    :initform nil :accessor run-cancellation-requested-p)
   (lock :initform (sb-thread:make-mutex :name "retained run")
         :reader run-lock)
   (changed :initform (sb-thread:make-waitqueue :name "retained run changed")
            :reader run-changed)))

(defclass build-artifact ()
  ((kind :initarg :kind :reader build-artifact-kind)
   (pathname :initarg :pathname :reader build-artifact-pathname)))

(defclass build-plan-summary ()
  ((systems :initarg :systems :initform 0 :reader build-plan-summary-systems)
   (compiles :initarg :compiles :initform 0 :reader build-plan-summary-compiles)
   (loads :initarg :loads :initform 0 :reader build-plan-summary-loads)))

(defclass build-source-location ()
  ((namestring :initarg :namestring :initform nil
               :reader build-source-location-namestring)
   (form-number :initarg :form-number :initform nil
                :reader build-source-location-form-number)
   (toplevel-form-number :initarg :toplevel-form-number :initform nil
                         :reader build-source-location-toplevel-form-number)
   (plist :initarg :plist :initform nil :reader build-source-location-plist)))

(defclass build-diagnostic ()
  ((id :initarg :id :reader build-diagnostic-id)
   (severity :initarg :severity :reader build-diagnostic-severity)
   (kind :initarg :kind :reader build-diagnostic-kind)
   (report :initarg :report :reader build-diagnostic-report)
   (condition-type :initarg :condition-type
                   :reader build-diagnostic-condition-type)
   (condition :initarg :condition :initform nil
              :reader build-diagnostic-condition)
   (action :initarg :action :initform nil :reader build-diagnostic-action)
   (source-location :initarg :source-location :initform nil
                    :reader build-diagnostic-source-location)
   (backtrace :initarg :backtrace :initform nil
              :reader build-diagnostic-backtrace)))

(defclass build-action ()
  ((id :initarg :id :reader build-action-id)
   (kind :initarg :kind :reader build-action-kind)
   (label :initarg :label :reader build-action-label)
   (system :initarg :system :initform nil :reader build-action-system)
   (pathname :initarg :pathname :initform nil :reader build-action-pathname)
   (state :initform :running :accessor build-action-state)
   (started-at :initarg :started-at :reader build-action-started-at)
   (ended-at :initform nil :accessor build-action-ended-at)
   (seconds :initform nil :accessor build-action-seconds)
   (log-artifact :initarg :log-artifact :initform nil
                 :reader build-action-log-artifact)
   (diagnostics :initform nil :accessor build-action-diagnostics)))

(defclass build-run (run)
  ((project-root :initarg :project-root :reader build-run-project-root)
   (system :initarg :system :initform nil :reader build-run-system)
   (systems :initarg :systems :initform nil :reader build-run-systems)
   (operation :initarg :operation :initform 'asdf:load-op
              :reader build-run-operation)
   ;; ASDF plans are executor inputs, not durable snapshot values.
   (plan :initarg :plan :initform nil :reader build-run-plan)
   (plan-summary :initform nil :accessor build-run-plan-summary)
   (log-directory :initarg :log-directory :initform nil
                  :accessor build-run-log-directory)
   (actions :initform nil :accessor build-run-actions)
   (current-action :initform nil :accessor build-run-current-action)
   (diagnostics :initform nil :accessor build-run-diagnostics)
   (terminal-diagnostic :initform nil
                        :accessor build-run-terminal-diagnostic)
   (seen-conditions :initform (make-hash-table :test #'eq)
                    :reader build-run-seen-conditions)))

(defclass build-diagnostic-snapshot ()
  ((id :initarg :id :reader build-diagnostic-snapshot-id)
   (severity :initarg :severity :reader build-diagnostic-snapshot-severity)
   (kind :initarg :kind :reader build-diagnostic-snapshot-kind)
   (report :initarg :report :reader build-diagnostic-snapshot-report)
   (condition-type :initarg :condition-type
                   :reader build-diagnostic-snapshot-condition-type)
   (action-id :initarg :action-id :initform nil
              :reader build-diagnostic-snapshot-action-id)
   (source-location :initarg :source-location :initform nil
                    :reader build-diagnostic-snapshot-source-location)
   (backtrace :initarg :backtrace :initform nil
              :reader build-diagnostic-snapshot-backtrace)))

(defclass build-action-snapshot ()
  ((id :initarg :id :reader build-action-snapshot-id)
   (kind :initarg :kind :reader build-action-snapshot-kind)
   (label :initarg :label :reader build-action-snapshot-label)
   (system :initarg :system :reader build-action-snapshot-system)
   (pathname :initarg :pathname :reader build-action-snapshot-pathname)
   (state :initarg :state :reader build-action-snapshot-state)
   (started-at :initarg :started-at :reader build-action-snapshot-started-at)
   (ended-at :initarg :ended-at :reader build-action-snapshot-ended-at)
   (seconds :initarg :seconds :reader build-action-snapshot-seconds)
   (log-artifact :initarg :log-artifact
                 :reader build-action-snapshot-log-artifact)
   (diagnostics :initarg :diagnostics
                :reader build-action-snapshot-diagnostics)))

(defclass run-snapshot ()
  ((id :initarg :id :reader run-snapshot-id)
   (state :initarg :state :reader run-snapshot-state)
   (revision :initarg :revision :reader run-snapshot-revision)
   (started-at :initarg :started-at :reader run-snapshot-started-at)
   (ended-at :initarg :ended-at :reader run-snapshot-ended-at)
   (actions :initarg :actions :reader run-snapshot-actions)
   (diagnostics :initarg :diagnostics :reader run-snapshot-diagnostics)
   (plan-summary :initarg :plan-summary :reader run-snapshot-plan-summary)
   (current-action :initarg :current-action
                   :reader run-snapshot-current-action)
   (terminal-diagnostic :initarg :terminal-diagnostic
                        :reader run-snapshot-terminal-diagnostic)))

(defun make-build-run
    (project-root &key system systems (operation 'asdf:load-op) plan id
                         log-directory)
  (make-instance
   'build-run :project-root (uiop:ensure-directory-pathname project-root)
   :system system :systems (or systems (and system (list system)))
   :operation operation :plan plan
   :id (or id (make-build-id)) :log-directory log-directory))

(defun run-terminal-p (run)
  (member (run-state run) +terminal-run-states+))

(defun publish-run-change (run)
  "Advance RUN's revision while its lock is held and wake snapshot observers."
  (incf (run-revision run))
  (sb-thread:condition-broadcast (run-changed run))
  run)

(defun begin-run (run)
  (sb-thread:with-mutex ((run-lock run))
    (when (eq :pending (run-state run))
      (setf (run-state run) :running
            (run-started-at run) (get-internal-real-time))
      (publish-run-change run)))
  run)

(defun finish-run (run state &optional diagnostic)
  (sb-thread:with-mutex ((run-lock run))
    (unless (run-terminal-p run)
      (setf (run-state run) state
            (run-ended-at run) (get-internal-real-time))
      (when diagnostic
        (setf (build-run-terminal-diagnostic run) diagnostic))
      (publish-run-change run)))
  run)

(defun diagnostic-snapshot (diagnostic)
  (when diagnostic
    (let ((action (build-diagnostic-action diagnostic)))
      (make-instance
       'build-diagnostic-snapshot
       :id (build-diagnostic-id diagnostic)
       :severity (build-diagnostic-severity diagnostic)
       :kind (build-diagnostic-kind diagnostic)
       :report (build-diagnostic-report diagnostic)
       :condition-type (build-diagnostic-condition-type diagnostic)
       :action-id (and action (build-action-id action))
       :source-location (build-diagnostic-source-location diagnostic)
       :backtrace (build-diagnostic-backtrace diagnostic)))))

(defun action-snapshot (action)
  (when action
    (make-instance
     'build-action-snapshot
     :id (build-action-id action)
     :kind (build-action-kind action)
     :label (build-action-label action)
     :system (build-action-system action)
     :pathname (build-action-pathname action)
     :state (build-action-state action)
     :started-at (build-action-started-at action)
     :ended-at (build-action-ended-at action)
     :seconds (build-action-seconds action)
     :log-artifact (build-action-log-artifact action)
     :diagnostics (mapcar #'diagnostic-snapshot
                          (reverse (build-action-diagnostics action))))))

(defgeneric run-snapshot (run))

(defmethod run-snapshot ((run build-run))
  (sb-thread:with-mutex ((run-lock run))
    (make-instance
     'run-snapshot
     :id (run-id run) :state (run-state run) :revision (run-revision run)
     :started-at (run-started-at run) :ended-at (run-ended-at run)
     :actions (mapcar #'action-snapshot (reverse (build-run-actions run)))
     :diagnostics (mapcar #'diagnostic-snapshot
                          (reverse (build-run-diagnostics run)))
     :plan-summary (build-run-plan-summary run)
     :current-action (action-snapshot (build-run-current-action run))
     :terminal-diagnostic
     (diagnostic-snapshot (build-run-terminal-diagnostic run)))))

(defgeneric await-run (run &key timeout))

(defmethod await-run ((run run) &key timeout)
  (let ((deadline
          (and timeout (+ (get-internal-real-time)
                          (* timeout internal-time-units-per-second)))))
    (sb-thread:with-mutex ((run-lock run))
      (loop until (run-terminal-p run)
            for remaining = (and deadline
                                 (/ (- deadline (get-internal-real-time))
                                    (float internal-time-units-per-second)))
            when (and remaining (not (plusp remaining)))
              do (return)
            do (sb-thread:condition-wait
                (run-changed run) (run-lock run) :timeout remaining))))
  (run-snapshot run))

(defgeneric request-run-cancellation (run))

(defmethod request-run-cancellation ((run run))
  (sb-thread:with-mutex ((run-lock run))
    (setf (run-cancellation-requested-p run) t)
    (publish-run-change run))
  run)

(define-condition build-cancelled (error)
  ((run :initarg :run :reader build-cancelled-run))
  (:report (lambda (condition stream)
             (format stream "Build ~A was cancelled between actions."
                     (run-id (build-cancelled-run condition))))))

(defclass executor () ())

(defgeneric execute-run (executor run))
