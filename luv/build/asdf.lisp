(in-package #:luv.build)

;;; ASDF instrumentation is inert unless a build run is dynamically current.

(defvar *current-build-run* nil)
(defvar *build-event-function* nil)
(defvar *build-action-output-wrapper* nil)

(defun current-build-run () *current-build-run*)

(defun emit-build-event (&rest event)
  (when *build-event-function*
    (apply *build-event-function* event)))

(defun elapsed-seconds (start)
  (/ (float (- (get-internal-real-time) start) 1.0)
     internal-time-units-per-second))

(defun capture-condition-report (condition)
  (handler-case (princ-to-string condition)
    (error () (format nil "~S" (type-of condition)))))

(defun capture-condition-backtrace ()
  (ignore-errors
   (with-output-to-string (stream)
     (sb-debug:print-backtrace :stream stream :count 100))))

(defun capture-build-source-location ()
  (ignore-errors
   (let ((location (sb-c:source-location)))
     (when location
       (make-instance
        'build-source-location
        :namestring (sb-c:definition-source-location-namestring location)
        :form-number (sb-c:definition-source-location-form-number location)
        :toplevel-form-number
        (sb-c:definition-source-location-toplevel-form-number location)
        :plist (copy-list (sb-c:definition-source-location-plist location)))))))

(defun condition-severity (condition)
  (cond ((typep condition 'error) :error)
        ((typep condition 'sb-ext:compiler-note) :note)
        ((typep condition 'style-warning) :style-warning)
        ((typep condition 'warning) :warning)
        (t :condition)))

(defun condition-kind (condition)
  (cond ((typep condition 'sb-ext:compiler-note) :compiler-note)
        ((typep condition 'warning) :compiler-warning)
        ((typep condition 'error) :error)
        (t :condition)))

(defun record-build-condition (run condition &key action terminal-p)
  "Retain CONDITION while its dynamic compiler context still exists."
  (sb-thread:with-mutex ((run-lock run))
    (let ((diagnostic (gethash condition (build-run-seen-conditions run)))
          (changed-p nil))
      (unless diagnostic
        (setf diagnostic
              (make-instance
               'build-diagnostic
               :id (1+ (length (build-run-diagnostics run)))
               :severity (condition-severity condition)
               :kind (condition-kind condition)
               :report (capture-condition-report condition)
               :condition-type (class-name (class-of condition))
               :condition condition :action action
               :source-location (capture-build-source-location)
               :backtrace (and (typep condition 'error)
                               (capture-condition-backtrace))))
        (setf (gethash condition (build-run-seen-conditions run)) diagnostic)
        (push diagnostic (build-run-diagnostics run))
        (when action (push diagnostic (build-action-diagnostics action)))
        (setf changed-p t))
      (when (and terminal-p
                 (not (eq diagnostic (build-run-terminal-diagnostic run))))
        (setf (build-run-terminal-diagnostic run) diagnostic
              changed-p t))
      (when changed-p (publish-run-change run))
      diagnostic)))

(defun summarize-asdf-actions (actions)
  (let ((system-actions (make-hash-table :test #'equal))
        (compile-actions (make-hash-table :test #'equal))
        (load-actions (make-hash-table :test #'equal)))
    (dolist (action actions)
      (let* ((operation (asdf/action:action-operation action))
             (component (asdf/action:action-component action))
             (key (list (class-name (class-of operation)) component)))
        (cond ((and (typep operation 'asdf:prepare-op)
                    (typep component 'asdf:system))
               (setf (gethash key system-actions) t))
              ((not (typep component 'asdf:cl-source-file)))
              ((typep operation 'asdf:compile-op)
               (setf (gethash key compile-actions) t))
              ((typep operation 'asdf:load-op)
               (setf (gethash key load-actions) t)))))
    (make-instance
     'build-plan-summary
     :systems (hash-table-count system-actions)
     :compiles (hash-table-count compile-actions)
     :loads (hash-table-count load-actions))))

(defun record-build-plan-summary (run summary)
  (sb-thread:with-mutex ((run-lock run))
    (unless (build-run-plan-summary run)
      (setf (build-run-plan-summary run) summary)
      (publish-run-change run)))
  (build-run-plan-summary run))

(defun component-label (run component)
  (let ((path (ignore-errors (asdf:component-pathname component))))
    (if (and path (uiop:subpathp path (build-run-project-root run)))
        (namestring
         (uiop:enough-pathname path (build-run-project-root run)))
        (format nil "~A/~A"
                (asdf:component-name (asdf:component-system component))
                (asdf:component-name component)))))

(defun action-log-artifact (run component)
  (let ((directory (build-run-log-directory run)))
    (when directory
      (make-instance
       'build-artifact :kind :verbose-log
       :pathname
       (merge-pathnames
        (concatenate 'string
                     (asdf:component-name (asdf:component-system component))
                     ".log")
        directory)))))

(defun begin-build-action (run kind component)
  (let* ((started-at (get-internal-real-time))
         (label (component-label run component))
         (system (asdf:component-name (asdf:component-system component)))
         (path (ignore-errors (asdf:component-pathname component)))
         (artifact (action-log-artifact run component))
         (action
           (sb-thread:with-mutex ((run-lock run))
             (let ((action
                     (make-instance
                      'build-action :id (1+ (length (build-run-actions run)))
                      :kind kind :label label :system system :pathname path
                      :started-at started-at :log-artifact artifact)))
               (push action (build-run-actions run))
               (setf (build-run-current-action run) action)
               (publish-run-change run)
               action))))
    (emit-build-event
     :begin kind label
     (and artifact
          (namestring
           (uiop:enough-pathname
            (build-artifact-pathname artifact)
            (build-run-project-root run)))))
    action))

(defun finish-build-action (run action state started-at)
  (let ((seconds (elapsed-seconds started-at)))
    (sb-thread:with-mutex ((run-lock run))
      (setf (build-action-state action) state
            (build-action-ended-at action) (get-internal-real-time)
            (build-action-seconds action) seconds)
      (when (eq action (build-run-current-action run))
        (setf (build-run-current-action run) nil))
      (publish-run-change run))
    ;; A failed action leaves the console adapter's current item in place, as
    ;; the old reporter did, so its terminal report can name the failed source.
    (when (eq state :succeeded)
      (emit-build-event
       :end (build-action-kind action) (build-action-label action) seconds))
    action))

(defun call-with-build-action (run kind component thunk)
  (when (run-cancellation-requested-p run)
    (error 'build-cancelled :run run))
  (let* ((started-at (get-internal-real-time))
         (action (begin-build-action run kind component))
         (completed-p nil))
    (unwind-protect
         (handler-bind
             ((sb-ext:compiler-note
                (lambda (condition)
                  (record-build-condition run condition :action action)))
              (warning
                (lambda (condition)
                  (record-build-condition run condition :action action)))
              (error
                (lambda (condition)
                  (record-build-condition run condition :action action))))
           (multiple-value-prog1
               (if *build-action-output-wrapper*
                   (funcall *build-action-output-wrapper* action thunk)
                   (funcall thunk))
             (setf completed-p t)))
      (finish-build-action run action
                           (if completed-p :succeeded :failed) started-at))))

(defmethod asdf:perform :around
    ((operation asdf:compile-op) (component asdf:cl-source-file))
  (if *current-build-run*
      (call-with-build-action
       *current-build-run* :compile component #'call-next-method)
      (call-next-method)))

(defmethod asdf:perform :around
    ((operation asdf:load-op) (component asdf:cl-source-file))
  (if *current-build-run*
      (call-with-build-action
       *current-build-run* :load component #'call-next-method)
      (call-next-method)))

(defmethod asdf:perform :before
    ((operation asdf:prepare-op) (system asdf:system))
  (declare (ignore operation))
  (when *current-build-run*
    (emit-build-event :system-begin (asdf:component-name system))))

(defmethod asdf:perform :after
    ((operation asdf:load-op) (system asdf:system))
  (declare (ignore operation))
  (when *current-build-run*
    (emit-build-event :system-end (asdf:component-name system))))

(defclass asdf-build-executor (executor)
  ((signal-errors-p :initarg :signal-errors-p :initform t
                    :reader asdf-build-executor-signal-errors-p)
   (finalize-p :initarg :finalize-p :initform t
               :reader asdf-build-executor-finalize-p)
   (event-function :initarg :event-function :initform *build-event-function*
                   :reader asdf-build-executor-event-function)
   (action-output-wrapper
    :initarg :action-output-wrapper :initform *build-action-output-wrapper*
                          :reader asdf-build-executor-action-output-wrapper)))

(defun make-combined-plan (run)
  (let* ((systems (or (build-run-systems run)
                      (error "Build ~A has no systems or supplied plan."
                             (run-id run))))
         (operation (asdf:make-operation (build-run-operation run)))
         (plan
           (asdf/plan:make-plan
            'asdf/plan:sequential-plan operation
            (asdf:find-system (first systems)))))
    (dolist (system (rest systems))
      (asdf/plan:traverse-action plan operation (asdf:find-system system) t))
    plan))

(defmethod execute-run ((executor asdf-build-executor) (run build-run))
  (begin-run run)
  (let ((*current-build-run* run)
        (*build-event-function*
          (asdf-build-executor-event-function executor))
        (*build-action-output-wrapper*
          (asdf-build-executor-action-output-wrapper executor)))
    (handler-case
        (let ((plan (or (build-run-plan run) (make-combined-plan run))))
          (when (run-cancellation-requested-p run)
            (error 'build-cancelled :run run))
          (record-build-plan-summary
           run (summarize-asdf-actions (asdf/plan:plan-actions plan)))
          (asdf/plan:perform-plan plan)
          (when (asdf-build-executor-finalize-p executor)
            (finish-run run :succeeded)))
      (build-cancelled (condition)
        (let ((diagnostic
                (record-build-condition run condition :terminal-p t)))
          (when (asdf-build-executor-finalize-p executor)
            (finish-run run :cancelled diagnostic)))
        (when (asdf-build-executor-signal-errors-p executor)
          (error condition)))
      (error (condition)
        (let ((diagnostic
                (record-build-condition run condition :terminal-p t)))
          (when (asdf-build-executor-finalize-p executor)
            (finish-run run :failed diagnostic)))
        (when (asdf-build-executor-signal-errors-p executor)
          (error condition)))))
  run)

(defun call-with-build-run (run function)
  "Execute arbitrary ASDF work as RUN while retaining actions and diagnostics."
  (begin-run run)
  (let ((*current-build-run* run))
    (handler-case
        (multiple-value-prog1 (funcall function)
          (finish-run run :succeeded))
      (build-cancelled (condition)
        (let ((diagnostic
                (record-build-condition run condition :terminal-p t)))
          (finish-run run :cancelled diagnostic))
        (error condition))
      (error (condition)
        (let ((diagnostic
                (record-build-condition run condition :terminal-p t)))
          (finish-run run :failed diagnostic))
        (error condition)))))
