(defpackage #:sly-client.tests
  (:use #:cl #:rove))

(in-package #:sly-client.tests)

(defun journal-event (kind &rest fields)
  (list
   (cons "timestamp" "2026-08-24T10:00:00Z")
   (cons "fields"
         (list* (cons "SWASH_EVENT" kind) fields))))

(defun fixed-journal-provider (tagged-events &optional exited-events)
  (lambda (&rest filters)
    (cond
      ((equal filters '("--field" "LUV_KIND=LISP")) tagged-events)
      ((equal filters '("--event" "exited")) exited-events)
      (t (error "Unexpected test journal query: ~S" filters)))))

(deftest stale-ready-lisps-are-not-running
  (let* ((endpoint-probed-p nil)
         (instance
           (sly-client::make-lisp-instance
            :id "OLD123" :name "old" :root "/tmp/luv/"
            :state :ready :pid 12345 :port 4005))
         (sly-client::*process-alive-probe* (constantly nil))
         (sly-client::*endpoint-alive-probe*
           (lambda (port)
             (declare (ignore port))
             (setf endpoint-probed-p t))))
    (sly-client::reconcile-lisp-instance instance)
    (ok (eq :stale (sly-client::lisp-instance-state instance)))
    (ng (sly-client::running-lisp-p instance))
    (ng endpoint-probed-p)))

(deftest live-ready-lisps-pass-both-probes
  (let* ((instance
           (sly-client::make-lisp-instance
            :id "LIVE123" :name "live" :root "/tmp/luv/"
            :state :ready :pid 12345 :port 4005))
         (sly-client::*process-alive-probe* (constantly t))
         (sly-client::*endpoint-alive-probe* (constantly t)))
    (sly-client::reconcile-lisp-instance instance)
    (ok (eq :ready (sly-client::lisp-instance-state instance)))
    (ok (sly-client::running-lisp-p instance))))

(deftest starting-lisps-expire-at-the-startup-timeout
  (let ((sly-client::*universal-time-provider* (constantly 1000)))
    (let ((current
            (sly-client::make-lisp-instance
             :state :starting :started-universal-time 900))
          (expired
            (sly-client::make-lisp-instance
             :state :starting :started-universal-time 800))
          (legacy
            (sly-client::make-lisp-instance :state :starting)))
      (sly-client::reconcile-lisp-instance current)
      (sly-client::reconcile-lisp-instance expired)
      (sly-client::reconcile-lisp-instance legacy)
      (ok (eq :starting (sly-client::lisp-instance-state current)))
      (ok (eq :stale (sly-client::lisp-instance-state expired)))
      (ok (eq :stale (sly-client::lisp-instance-state legacy))))))

(deftest status-does-not-present-another-checkouts-lisp
  (let* ((sly-client::*project-root* #P"/tmp/luv/")
         (other
           (sly-client::make-lisp-instance
            :id "OTHER1" :name "other" :root "/tmp/luv2/"
            :state :ready :pid 12345 :port 4005))
         (selected (sly-client::choose-lisp :instances (list other)))
         (output
           (with-output-to-string (*standard-output*)
             (sly-client::report-managed-server-status selected))))
    (ok (null selected))
    (ok (string= (format nil "This checkout has no running Lisp.~%")
                 output))))

(deftest tagged-ready-event-discovers-an-orphaned-start
  (let* ((ready
           (journal-event
            "slynk-ready"
            (cons "SWASH_SESSION" "ORP123")
            (cons "LUV_KIND" "LISP")
            (cons "LUV_ROOT" "/tmp/luv/")
            (cons "LUV_NAME" "orphan")
            (cons "LUV_STARTED_AT" "1000")
            (cons "LUV_SLYNK_PORT" "45123")
            (cons "LUV_SLYNK_PID" "12345")))
         (sly-client::*swash-events-provider*
           (fixed-journal-provider (list ready)))
         (sly-client::*process-alive-probe* (constantly t))
         (sly-client::*endpoint-alive-probe* (constantly t))
         (instances (sly-client::lisp-instances))
         (instance (first instances)))
    (ok (= 1 (length instances)))
    (ok (string= "ORP123" (sly-client::lisp-instance-id instance)))
    (ok (string= "/tmp/luv/" (sly-client::lisp-instance-root instance)))
    (ok (eq :ready (sly-client::lisp-instance-state instance)))))

(deftest stale-lisps-require-explicit-lifecycle-selection
  (let* ((sly-client::*project-root* #P"/tmp/luv/")
         (stale
           (sly-client::make-lisp-instance
            :id "STA123" :name "stale" :root "/tmp/luv/"
            :state :stale :pid 12345 :port 4005)))
    (let ((sly-client::*lisp-selector* nil))
      (ng (sly-client::choose-lisp :instances (list stale)))
      (ok (signals
           (sly-client::choose-lisp
            :instances (list stale) :start-if-missing t)
           'error)))
    (let ((sly-client::*lisp-selector* "STA123"))
      (ok (eq stale
              (sly-client::choose-lisp
               :instances (list stale) :allow-explicit-stale t)))
      (ok (signals
           (sly-client::choose-lisp :instances (list stale))
           'error)))
    (let ((sly-client::*lisp-selector* "STA"))
      (ok (signals
           (sly-client::choose-lisp
            :instances (list stale) :allow-explicit-stale t)
           'error)))))

(deftest dead-stale-lisp-retires-without-signalling-a-reused-process
  (let* ((instance
           (sly-client::make-lisp-instance
            :id "DED123" :name "dead" :root "/tmp/luv/"
            :state :stale :pid 12345 :port 4005))
         (stop-called-p nil)
         (retired-event nil)
         (sly-client::*swash-session-running-probe*
           (lambda (candidate)
             (declare (ignore candidate))
             nil))
         (sly-client::*swash-stop-runner*
           (lambda (candidate)
             (declare (ignore candidate))
             (setf stop-called-p t)))
         (sly-client::*managed-lisp-event-emitter*
           (lambda (candidate event message &rest fields)
             (declare (ignore candidate message fields))
             (setf retired-event event))))
    (ok (sly-client::stop-managed-lisp instance))
    (ng stop-called-p)
    (ok (string= "lisp-retired" retired-event))))

(deftest live-stale-lisp-stops-through-swash-before-retirement
  (let* ((instance
           (sly-client::make-lisp-instance
            :id "BAD123" :name "unhealthy" :root "/tmp/luv/"
            :state :stale :pid 12345 :port 4005))
         (actions nil)
         (sly-client::*swash-session-running-probe*
           (lambda (candidate)
             (declare (ignore candidate))
             t))
         (sly-client::*swash-stop-runner*
           (lambda (candidate)
             (declare (ignore candidate))
             (push :stop actions)))
         (sly-client::*managed-lisp-event-emitter*
           (lambda (candidate event message &rest fields)
             (declare (ignore candidate event message fields))
             (push :retire actions))))
    (ok (sly-client::stop-managed-lisp instance))
    (ok (equal '(:stop :retire) (nreverse actions)))))

(deftest empty-lisp-selector-never-selects-globally
  (let* ((sly-client::*lisp-selector* "")
         (other
           (sly-client::make-lisp-instance
            :id "OTH123" :name "other" :root "/tmp/luv2/"
            :state :ready :pid 12345 :port 4005)))
    (ng (sly-client::matching-lisps "" (list other)))
    (ok (signals
         (sly-client::choose-lisp
          :instances (list other) :allow-explicit-stale t)
         'error))))

(deftest activity-events-carry-registry-identity
  (let* ((sly-client::*current-command* "eval")
         (instance
           (sly-client::make-lisp-instance
            :id "ACT123" :name "active" :root "/tmp/luv/"
            :state :ready :pid 12345 :port 4005))
         (fields (sly-client::lisp-activity-fields instance)))
    (ok (member "LUV_KIND=LISP" fields :test #'string=))
    (ok (member "LUV_ROOT=/tmp/luv/" fields :test #'string=))
    (ok (member "LUV_NAME=active" fields :test #'string=))
    (ok (member "LUV_COMMAND=eval" fields :test #'string=))))

(deftest load-form-does-not-require-the-luv-package-at-read-time
  (let ((form (sly-client::load-systems-form '("luft"))))
    (ng (search "luv:" form :test #'char-equal))
    (multiple-value-bind (object end)
        (read-from-string form)
      (ok object)
      (ok (= end (length form))))))
