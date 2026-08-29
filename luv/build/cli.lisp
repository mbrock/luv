;;;; Console presentation and disposable-process deadline policy for build runs.
;;;;
;;;; Two threads.  The build thread compiles; a display thread owns the
;;;; console and is the only thing that prints.  They speak over an
;;;; SB-CONCURRENCY mailbox, which the display thread reads with a timeout, so
;;;; that it keeps a heartbeat even while the build says nothing.  Past a
;;;; policy deadline the display records a violation but lets the compiler
;;;; finish.  At twice that deadline it interrupts the build thread.  A
;;;; completed build with violations still exits unsuccessfully: a file that
;;;; takes more than a few seconds to compile is a defect, not a slow step.
;;;;
;;;; Nothing is muted.  Compiler chatter and the C toolchain's pkg-config and
;;;; gcc noise are redirected -- at the file descriptor level, since
;;;; CFFI-GROVEL runs its subprocesses with :OUTPUT :INTERACTIVE and so writes
;;;; past any rebound Lisp stream -- into one log file per source file under
;;;; build/logs/.  The display thread writes to a dup of the original stderr,
;;;; so it keeps the console while everything else is being recorded.
;;;;
;;;; The console gets one plain line per compiled file, printed before the work
;;;; starts, so the stamp says when the file began and a file that never
;;;; finishes has still named itself:
;;;;
;;;;   000.1s   1/150  domains/package.lisp
;;;;
;;;; Prose about the build -- what it is making, how it ended, what it cost --
;;;; is commented, so the whole console reads as Lisp.  Everything else is
;;;; remembered rather than printed: every action with its place and duration
;;;; goes into a record, which becomes the closing remarks and is saved as
;;;; build.sexp beside the logs.

(in-package #:luv.build)

;;; Configuration

(defvar *deadline-seconds*
  (let ((spec (uiop:getenv "LUV_BUILD_DEADLINE")))
    (if spec
        (let ((n (ignore-errors (parse-integer spec))))
          (if (and n (plusp n)) n nil))
        3))
  "Seconds a file may compile before it violates policy.
Compilation is interrupted at twice this limit. NIL disables both limits;
LUV_BUILD_DEADLINE overrides the default, and 0 disables it.")

(defparameter *hard-deadline-multiplier* 2
  "How much longer than policy a compiler may run before it is interrupted.")

(defvar *log-directory* nil)
(defvar *project-root* nil)
(defvar *build-id* nil)
(defvar *system* nil
  "What this build is making, as ASDF names it.")
(defvar *invocation* "make"
  "How the narrated operation names itself in its opening comment.")
(defvar *policy-violated-p* nil)
(defvar *policy-status-path* nil)

(defun build-log ()
  (merge-pathnames "build.log" *log-directory*))

(defun logs-root ()
  (merge-pathnames "build/logs/" *project-root*))

(defparameter *tick-interval* 0.25
  "How often the display thread wakes when no event has arrived.")

(defparameter *quiet-seconds* 0.05
  "Durations below this are noise.")

;;; The grid

(defvar *counter-width* 8
  "The column a file's place in the plan is right-aligned in.")

;;; The console: a dup of stderr, immune to the redirection below.

(defvar *console* nil)
(defvar *saved-standard-output-fd* nil)
(defvar *saved-error-output-fd* nil)

;;; Threads

(defvar *mailbox* nil)
(defvar *build-thread* nil)
(defvar *display-thread* nil)

(define-condition deadline-exceeded (error)
  ((label :initarg :label :reader deadline-exceeded-label)
   (seconds :initarg :seconds :reader deadline-exceeded-seconds))
  (:report (lambda (condition stream)
             (format stream "~A exceeded the ~Ds build deadline"
                     (deadline-exceeded-label condition)
                     (deadline-exceeded-seconds condition)))))

(defun policy-violated-p ()
  *policy-violated-p*)

(defun hard-deadline-seconds ()
  (and *deadline-seconds*
       (* *deadline-seconds* *hard-deadline-multiplier*)))

(defun mark-policy-violation (label)
  (setf *policy-violated-p* t)
  (when *policy-status-path*
    (ensure-directories-exist *policy-status-path*)
    (with-open-file (out *policy-status-path*
                         :direction :output
                         :if-exists :append
                         :if-does-not-exist :create)
      (format out "~A~%" label))))

(defun send (&rest message)
  (when *mailbox*
    (sb-concurrency:send-message *mailbox* message)))

(defun failed (failure)
  "Tell the display why the build died, from outside the display thread."
  (when (and *current-build-run* (typep failure 'condition))
    (record-build-condition *current-build-run* failure :terminal-p t))
  (send :failure (princ-to-string failure)))

(defun elapsed (since)
  (/ (float (- (get-internal-real-time) since) 1.0)
     internal-time-units-per-second))

;;; Rendering

(defun format-seconds (seconds)
  "A short duration for prose; NIL when there is nothing to say."
  (cond ((null seconds) nil)
        ((< seconds *quiet-seconds*) "0s")
        ((< seconds 10) (format nil "~,1Fs" seconds))
        ((< seconds 600) (format nil "~Ds" (round seconds)))
        (t (multiple-value-bind (minutes secs) (floor (round seconds) 60)
             (format nil "~D:~2,'0D" minutes secs)))))



;;; Display thread

(defstruct (record (:constructor make-record (kind label system at seconds)))
  kind label system at seconds)

(defstruct display
  (history '())
  (systems-total nil) (systems-done 0)
  (compiles-total nil) (compiles-done 0)
  (loads-total nil) (loads-done 0)
  (system nil) (system-start nil)
  (current nil) (current-kind nil) (current-start nil)
  (current-violation nil) (violations '())
  (current-log nil) (failed nil) (failed-log nil) (failed-seconds nil)
  (failure nil)
  (tripped nil)
  (start (get-internal-real-time)))

(defun since-start (state)
  (elapsed (display-start state)))

(defun say (text)
  (format *console* "~A~%" text)
  (finish-output *console*))

(defun remark (&optional control &rest arguments)
  "One commented line of the build's own prose, or a bare comment for a gap."
  (say (if control
           (concatenate 'string ";; " (apply #'format nil control arguments))
           ";;")))

(defun action-line (state label)
  "Where a file stands in the plan, said before it is compiled."
  (let ((place
          (if (display-compiles-total state)
              (format nil "~D/~D" (1+ (display-compiles-done state))
                      (display-compiles-total state))
              (format nil "~D" (1+ (display-compiles-done state))))))
    (say (format nil "~5,1,,,'0Fs~V@A  ~A"
                 (since-start state) *counter-width* place label))))

(defun here (path)
  "A path in the checkout, said the way one would type it."
  (format nil "./~A" path))

(defun in-logs (name)
  (namestring (uiop:enough-pathname (merge-pathnames name *log-directory*)
                                    *project-root*)))

(defun tick (state)
  "Watch the clock over whatever the build thread is doing."
  (let ((tripped (display-tripped state)))
    (when (and tripped (> (elapsed tripped) 5))
      ;; The build thread ignored its interrupt; do not leave it running.
      (report state :deadline)
      (sb-ext:exit :code 1 :abort t)))
  (let ((start (display-current-start state)))
    (when (and start *deadline-seconds*
               (eq (display-current-kind state) :compile))
      (let ((seconds (elapsed start)))
        (when (and (> seconds *deadline-seconds*)
                   (null (display-current-violation state)))
          (let ((violation
                  (make-record :violation
                               (display-current state)
                               (display-system state)
                               (- (since-start state) seconds)
                               nil)))
            (setf (display-current-violation state) violation)
            (push violation (display-violations state))
            (mark-policy-violation (display-current state))
            (remark "Deadline violation: ~A passed ~Ds; allowing up to ~Ds."
                    (here (display-current state)) *deadline-seconds*
                    (hard-deadline-seconds))))
        (when (> seconds (hard-deadline-seconds))
          (trip-deadline state seconds))))))

(defun trip-deadline (state seconds)
  (let ((label (display-current state)))
    (when (display-current-violation state)
      (setf (record-seconds (display-current-violation state)) seconds))
    (setf (display-failed-seconds state) seconds)
    ;; Stop watching and let the build thread die of the error; the report is
    ;; printed when it comes back through FINISH.  The display loop must not
    ;; block here, so the grace period is checked on a later tick.
    (setf (display-failed state) label
          (display-failed-log state) (display-current-log state)
          (display-current state) nil
          (display-current-start state) nil
          (display-tripped state) (get-internal-real-time))
    (when *build-thread*
      (sb-thread:interrupt-thread
       *build-thread*
       (lambda ()
         ;; Where the compiler was when the clock ran out: the one sample
         ;; that names a slow file's culprit without anyone waiting for it.
         (ignore-errors
          (with-open-file (out (merge-pathnames "deadline-backtrace.txt"
                                                *log-directory*)
                               :direction :output :if-exists :supersede)
            (format out ";; ~A after ~,1Fs~%" label seconds)
            (sb-debug:print-backtrace :stream out :count 200)))
         (error 'deadline-exceeded :label label
                                   :seconds (hard-deadline-seconds)))))))

;;; What the build is worth saying about itself afterwards

(defun records-of (state kind)
  (remove-if-not (lambda (r) (eq (record-kind r) kind)) (display-history state)))

(defun total-seconds (records)
  (reduce #'+ records :key #'record-seconds :initial-value 0))

(defun failure-log (state)
  (or (display-failed-log state)
      (display-current-log state)
      (namestring (uiop:enough-pathname (build-log) *project-root*))))

(defun write-sexp-log (state reason)
  "Save the whole account of the build, so a later question can be answered."
  (let ((path (merge-pathnames "build.sexp" *log-directory*)))
    (ignore-errors
     (with-open-file (out path :direction :output :if-exists :supersede
                               :if-does-not-exist :create)
       (let ((*print-right-margin* 100)
             (*print-case* :downcase)
             (*print-pretty* t))
         (prin1 (list :build *build-id*
                      :reason reason
                      :seconds (since-start state)
                      :systems (display-systems-done state)
                      :compiled (display-compiles-done state)
                      :loaded (display-loads-done state)
                      :failed (display-failed state)
                      :violations
                      (mapcar (lambda (r)
                                (list :name (record-label r)
                                      :system (record-system r)
                                      :seconds (record-seconds r)))
                              (reverse (display-violations state)))
                      :actions
                      (mapcar (lambda (r)
                                (list :kind (record-kind r)
                                      :name (record-label r)
                                      :system (record-system r)
                                      :at (record-at r)
                                      :seconds (record-seconds r)))
                              (reverse (display-history state))))
                out)
         (terpri out))))
    path))

(defun report (state reason)
  "What the build has to say for itself, once it is over."
  (let ((sexp (write-sexp-log
               state
               (if (and (eq reason :done) (display-violations state))
                   :deadline-violations
                   reason)))
        (raw (directory-bytes *log-directory*)))
    (say "")
    (ecase reason
      (:deadline
       (remark "Error: DREADFUL COMPILATION UNIT!")
       (remark)
       (remark "Compilation of ~A was aborted after ~A."
               (here (display-failed state))
               (format-seconds (display-failed-seconds state)))
       (remark "Project policy requires under ~Ds per file and stops work at ~Ds."
               *deadline-seconds* (hard-deadline-seconds)))
      (:error
       (remark "Error: THE BUILD FAILED!")
       (remark)
       (when (display-failed state)
         (remark "Compilation of ~A was aborted." (here (display-failed state))))
       (when (display-failure state)
         (remark "~A" (display-failure state))))
      (:interrupted
       (remark "Interrupted!")
       (remark)
       (remark "~D file~:P had been compiled." (display-compiles-done state)))
      (:done
       (report-work state)))
    (remark)
    (case reason
      (:done (report-logs state raw))
      (t
       (when (display-failed-log state)
         (remark "See verbose log for that system in ~A."
                 (display-failed-log state)))
       (when (eq reason :deadline)
         (remark "Where the compiler was when the clock ran out: ~A."
                 (in-logs "deadline-backtrace.txt")))
       (remark "The whole build's structured log is in ~A."
               (namestring (uiop:enough-pathname sexp *project-root*)))
       (remark)
       (report-cost)))
    (when (and (eq reason :done) (display-violations state))
      (report-policy-violations state))
    (say "")))

(defun report-policy-violations (state)
  (let ((violations (reverse (display-violations state))))
    (remark "Error: COMPILATION DEADLINE VIOLATIONS!")
    (remark)
    (remark "Project policy requires under ~Ds per file; ~D file~:P exceeded it."
            *deadline-seconds* (length violations))
    (dolist (violation violations)
      (remark "~7A  ~A"
              (format-seconds (record-seconds violation))
              (here (record-label violation))))
    (remark)
    (remark "All files completed and outputs were written, but the build fails policy.")))

(defun report-work (state)
  "What a successful build did, and what took the time."
  (let* ((compiles (records-of state :compile))
         (loads (records-of state :load))
         (slowest (first (sort (copy-list compiles) #'> :key #'record-seconds))))
    (remark "Built ~(~S~) in ~A." (or *system* :the-program)
            (format-seconds (since-start state)))
    (remark)
    (remark "~D file~:P compiled, ~D loaded, ~D system~:P."
            (length compiles) (display-loads-done state)
            (display-systems-done state))
    (remark "Spent ~A compiling and ~A loading."
            (format-seconds (total-seconds compiles))
            (format-seconds (total-seconds loads)))
    (when (and slowest (> (record-seconds slowest) *quiet-seconds*))
      (remark "Slowest was ~A at ~A." (here (record-label slowest))
              (format-seconds (record-seconds slowest))))))

(defun report-logs (state raw)
  "Keep the build's verbose output inspectable for troubleshooting."
  (declare (ignore state raw))
  (remark "Logs are in ~A." (in-logs ""))
  (remark)
  (report-cost))

(defun report-cost ()
  (remark "Note: Build logs take ~A of the ~A build directory."
          (human-bytes (directory-bytes (logs-root)))
          (human-bytes (directory-bytes (merge-pathnames "build/" *project-root*)))))

(defun handle (state message)
  (destructuring-bind (kind &rest args) message
    (ecase kind
      (:header
       (remark "~A ~(~S~)" *invocation* *system*)
       (remark "logs ~A" (here (string-right-trim
                                "/" (namestring
                                     (uiop:enough-pathname *log-directory*
                                                           *project-root*)))))
       (say ""))
      (:plan
       (destructuring-bind (&key systems compiles loads) args
         (setf (display-systems-total state) systems
               (display-compiles-total state) compiles
               (display-loads-total state) loads
               ;; Wide enough for the last file's place, and the two spaces
               ;; that separate it from the name.
               *counter-width* (+ 2 (* 2 (length (princ-to-string compiles)))))))
      (:system-begin
       (destructuring-bind (name) args
         (setf (display-system state) name
               (display-system-start state) (get-internal-real-time))))
      (:system-end
       (destructuring-bind (name) args
         (declare (ignore name))
         (incf (display-systems-done state))
         (setf (display-system state) nil
               (display-system-start state) nil)))
      (:begin
       (destructuring-bind (action-kind label &optional log) args
         (setf (display-current-log state) log
               (display-current state) label
               (display-current-kind state) action-kind
               (display-current-start state) (get-internal-real-time)
               (display-current-violation state) nil)
         ;; Announced before the work, so the stamp says when it began and a
         ;; file that never finishes has still named itself.
         (when (eq action-kind :compile)
           (action-line state label))))
      (:end
       (destructuring-bind (action-kind label seconds) args
         (when (display-current-violation state)
           (setf (record-seconds (display-current-violation state)) seconds))
         (push (make-record action-kind label (display-system state)
                            (- (since-start state) seconds) seconds)
               (display-history state))
         (setf (display-current state) nil
               (display-current-start state) nil
               (display-current-violation state) nil)
         (ecase action-kind
           (:compile (incf (display-compiles-done state)))
           (:load (incf (display-loads-done state))))))
      (:note
       (destructuring-bind (text) args
         (remark "~A" text)))
      (:failure
       (destructuring-bind (text) args
         (setf (display-failure state) text)))
      (:finish
       (destructuring-bind (reason) args
         (when (and (eq reason :error) (display-current state))
           (setf (display-failed state) (display-current state)
                 (display-failed-log state) (display-current-log state)))
         (report state reason))))))

(defun display-loop ()
  (let ((state (make-display)))
    (loop
      (let ((message (sb-concurrency:receive-message *mailbox*
                                                     :timeout *tick-interval*)))
        (cond ((null message) (tick state))
              (t
               ;; A display that dies takes the whole console with it and
               ;; leaves the build looking wedged; say so and carry on.
               (handler-case (handle state message)
                 (error (e) (remark "display: ~A" e)))
               (when (eq (first message) :finish) (return))))))))

;;; Per-action logs, captured at the file descriptor level

(defvar *opened-logs* (make-hash-table :test #'equal))

(defun link-latest-logs ()
  "Point build/logs/latest at this build, so tailing a log needs no id."
  (let ((link (merge-pathnames "build/logs/latest" *project-root*)))
    (ignore-errors (sb-posix:unlink (sb-ext:native-namestring link)))
    (ignore-errors (sb-posix:symlink *build-id* (sb-ext:native-namestring link)))))

(defun redirect-output-to (path)
  "Point file descriptors 1 and 2 at PATH for good."
  (ensure-directories-exist path)
  (let ((log (sb-posix:open (sb-ext:native-namestring path)
                            (logior sb-posix:o-wronly sb-posix:o-creat
                                    sb-posix:o-trunc)
                            #o644)))
    (setf (gethash (namestring path) *opened-logs*) t)
    (sb-posix:dup2 log 1)
    (sb-posix:dup2 log 2)
    (sb-posix:close log)))

(defun call-with-output-logged-to (path thunk &optional banner)
  "Run THUNK with file descriptors 1 and 2 pointing at PATH."
  (ensure-directories-exist path)
  (let* ((fresh (not (gethash (namestring path) *opened-logs*)))
         (flags (logior sb-posix:o-wronly sb-posix:o-creat
                        (if fresh sb-posix:o-trunc sb-posix:o-append)))
         (log (sb-posix:open (sb-ext:native-namestring path) flags #o644))
         (saved-out (sb-posix:dup 1))
         (saved-err (sb-posix:dup 2)))
    (setf (gethash (namestring path) *opened-logs*) t)
    (flet ((flush ()
             (ignore-errors (finish-output *standard-output*))
             (ignore-errors (finish-output *error-output*))))
      (flush)
      (sb-posix:dup2 log 1)
      (sb-posix:dup2 log 2)
      (when banner
        ;; Several files share one system's log; say where each one begins.
        (format *standard-output* "~&~%;;;; ~A~%" banner)
        (finish-output *standard-output*))
      (unwind-protect (funcall thunk)
        (flush)
        (sb-posix:dup2 saved-out 1)
        (sb-posix:dup2 saved-err 2)
        (sb-posix:close saved-out)
        (sb-posix:close saved-err)
        (sb-posix:close log)))))

(defun call-with-cli-action-output (action thunk)
  (let* ((artifact (build-action-log-artifact action))
         (log (and artifact (build-artifact-pathname artifact)))
         ;; Everything the compiler has to say is worth keeping, now that it is
         ;; written to this action's log rather than to the console: the form it
         ;; is on, and the notes ASDF would otherwise muffle.
         (*compile-verbose* t)
         (*compile-print* t)
         (*load-verbose* t)
         (*load-print* t)
         (uiop:*uninteresting-conditions* '())
         (uiop:*uninteresting-compiler-conditions* '()))
    (if log
        (call-with-output-logged-to
         log
         (lambda ()
           (handler-bind
               ((sb-ext:compiler-note
                  (lambda (condition)
                    (record-build-condition
                     *current-build-run* condition :action action)
                    (format *error-output* "~%; note: ~A~%" condition)
                    (muffle-warning condition))))
             (funcall thunk)))
         (format nil "~(~A~) ~A"
                 (build-action-kind action) (build-action-label action)))
        (funcall thunk))))

;;; Plan

(defun report-actions (actions)
  "Count ACTIONS by the kinds narrated by the progress display."
  (let ((summary (summarize-asdf-actions actions)))
    (when *current-build-run*
      (record-build-plan-summary *current-build-run* summary))
    (send :plan :systems (build-plan-summary-systems summary)
                :compiles (build-plan-summary-compiles summary)
                :loads (build-plan-summary-loads summary))))

(defun report-plan (systems operation)
  "Plan SYSTEMS and report their distinct progress actions."
  (handler-case
      (let ((actions nil))
        (dolist (system (if (listp systems) systems (list systems)))
          (setf actions
                (nconc actions
                       (asdf/plan:plan-actions
                        (asdf/plan:make-plan
                         nil (asdf:make-operation operation)
                         (asdf:find-system system))))))
        (report-actions actions))
    (error (e)
      (send :note (format nil "plan unavailable: ~A" e)))))

;;; Keeping the logs

(defun file-bytes (path)
  (or (ignore-errors
       (with-open-file (in path :element-type '(unsigned-byte 8))
         (file-length in)))
      0))

(defun directory-bytes (path)
  "Bytes under PATH, following nothing and counting each file once."
  (let ((total 0))
    (labels ((walk (dir)
               (dolist (file (uiop:directory-files dir))
                 (incf total (file-bytes file)))
               (dolist (sub (uiop:subdirectories dir))
                 (walk sub))))
      (when (uiop:directory-exists-p path)
        (walk (uiop:ensure-directory-pathname path))))
    total))

(defun human-bytes (bytes)
  (cond ((< bytes 1024) (format nil "~D B" bytes))
        ((< bytes (* 1024 1024)) (format nil "~,1F KB" (/ bytes 1024.0)))
        ((< bytes (* 1024 1024 1024)) (format nil "~,1F MB" (/ bytes 1048576.0)))
        (t (format nil "~,1F GB" (/ bytes 1073741824.0)))))

;;; Entry points

(defun start (project-root &key run system plan plan-systems
                                  (plan-operation 'asdf:build-op)
                                  (invocation "make")
                                  (redirect-output-p t)
                                  (report-plan-p t))
  (setf run
        (or run
            (make-build-run project-root :system system
                                         :systems plan-systems
                                         :operation plan-operation
                                         :plan plan)))
  (let ((status (uiop:getenv "LUV_BUILD_POLICY_STATUS")))
    (setf *policy-status-path*
          (and status (merge-pathnames status project-root))))
  (when *policy-status-path*
    (ignore-errors (delete-file *policy-status-path*)))
  (setf *policy-violated-p* nil
        *project-root* project-root
        *system* (or system (build-run-system run))
        *invocation* invocation
        *build-id* (run-id run)
        *log-directory* (merge-pathnames (format nil "build/logs/~A/" *build-id*)
                                         project-root)
        *saved-standard-output-fd* (and redirect-output-p (sb-posix:dup 1))
        *saved-error-output-fd* (and redirect-output-p (sb-posix:dup 2))
        *console* (sb-sys:make-fd-stream (sb-posix:dup 2)
                                         :output t :buffering :line
                                         :external-format :utf-8)
        *mailbox* (sb-concurrency:make-mailbox :name "build progress")
        *build-thread* sb-thread:*current-thread*
        *display-thread* (sb-thread:make-thread #'display-loop
                                                :name "build display"))
  (setf (build-run-log-directory run) *log-directory*
        *current-build-run* run
        *build-event-function* #'send
        *build-action-output-wrapper* #'call-with-cli-action-output)
  (begin-run run)
  ;; Quiet by default: the compiler is made verbose only inside an action,
  ;; where its output is being recorded.  Left on globally it would narrate
  ;; every .asd it loads to the console.
  (setf *compile-verbose* nil *compile-print* nil
        *load-verbose* nil *load-print* nil)
  ;; From here the process writes to build.log, and each action redirects
  ;; again to its own file's log.  Output that belongs to no single file --
  ;; ASDF's chatter, a compilation unit's abort summary -- lands there rather
  ;; than interrupting the display.
  (when redirect-output-p
    (redirect-output-to (build-log)))
  (link-latest-logs)
  (when report-plan-p
    (cond (plan (report-actions (asdf/plan:plan-actions plan)))
          ((or plan-systems system)
           (report-plan (or plan-systems system) plan-operation))))
  (send :header)
  run)

(defun finish (reason)
  "Ask the display thread for its report, and wait for it to be done.
Must run before SAVE-LISP-AND-DIE, which refuses to dump an image while
another thread is alive."
  (when (and *display-thread* (sb-thread:thread-alive-p *display-thread*))
    (send :finish reason)
    (sb-thread:join-thread *display-thread* :timeout 60 :default nil))
  (when *current-build-run*
    (finish-run *current-build-run*
                (case reason
                  (:done (if *policy-violated-p* :failed :succeeded))
                  (:deadline :deadline)
                  (:interrupted :interrupted)
                  (otherwise :failed))))
  ;; Standalone program builds die immediately after this, but the SLY image
  ;; stays alive.  Give that durable process its ordinary log streams back.
  (ignore-errors (finish-output *standard-output*))
  (ignore-errors (finish-output *error-output*))
  (when *saved-standard-output-fd*
    (sb-posix:dup2 *saved-standard-output-fd* 1)
    (sb-posix:close *saved-standard-output-fd*))
  (when *saved-error-output-fd*
    (sb-posix:dup2 *saved-error-output-fd* 2)
    (sb-posix:close *saved-error-output-fd*))
  (when *console* (ignore-errors (close *console*)))
  (setf *mailbox* nil
        *display-thread* nil
        *console* nil
        *saved-standard-output-fd* nil
        *saved-error-output-fd* nil
        *current-build-run* nil
        *build-event-function* nil
        *build-action-output-wrapper* nil)
  *policy-violated-p*)
