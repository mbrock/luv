;;;; Progress reporting for the luvcraft build.
;;;;
;;;; Two threads.  The build thread compiles; a display thread owns the
;;;; console and is the only thing that prints.  They speak over an
;;;; SB-CONCURRENCY mailbox, which the display thread reads with a timeout, so
;;;; that a file taking longer than a second starts ticking its own elapsed
;;;; time rather than going silent.  Past a deadline the display thread
;;;; interrupts the build thread and the build fails: a file that takes half a
;;;; minute to compile is a defect, not a slow step.
;;;;
;;;; Nothing is muted.  Compiler chatter and the C toolchain's pkg-config and
;;;; gcc noise are redirected -- at the file descriptor level, since
;;;; CFFI-GROVEL runs its subprocesses with :OUTPUT :INTERACTIVE and so writes
;;;; past any rebound Lisp stream -- into one log file per source file under
;;;; build/logs/.  The display thread writes to a dup of the original stderr,
;;;; so it keeps the console while everything else is being recorded.
;;;;
;;;; Every line is one row of the same grid, so the kinds, the counters, the
;;;; names and the times each keep their own column:
;;;;
;;;;   ;; system  [22/59]  luvcraft
;;;;   ;; compile [ 6/71]  luvcraft/physics.lisp                 12s
;;;;   ;; loaded  [22/59]  luvcraft                            31.2s

(defpackage #:luv-build
  (:use #:cl)
  (:export #:start #:finish #:note #:deadline-exceeded #:deadline-exceeded-label
           #:*log-directory*))

(in-package #:luv-build)

;;; Configuration

(defvar *deadline-seconds*
  (let ((spec (uiop:getenv "LUV_BUILD_DEADLINE")))
    (if spec
        (let ((n (ignore-errors (parse-integer spec))))
          (if (and n (plusp n)) n nil))
        30))
  "Seconds a single file may compile before the build is failed.
NIL disables the deadline; LUV_BUILD_DEADLINE overrides it, 0 disables it.")

(defvar *log-directory* nil)
(defvar *project-root* nil)

(defun build-log ()
  (merge-pathnames "build.log" *log-directory*))

(defparameter *tick-interval* 0.25
  "How often the display thread wakes to redraw when no event has arrived.")

(defparameter *quiet-seconds* 0.05
  "Timings below this are not worth a column.")

(defparameter *load-line-seconds* 0.5
  "Loading a file only earns a line of its own when it takes this long.")

;;; The grid

(defparameter *kind-width* 8)
(defparameter *name-width* 44)
(defparameter *time-width* 7)
(defvar *counter-width* 2
  "Digits per side of the [i/j] column, widened once the plan is known.")

;;; The console: a dup of stderr, immune to the redirection below.

(defvar *console* nil)
(defvar *tty-p* nil)

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

(defun send (&rest message)
  (when *mailbox*
    (sb-concurrency:send-message *mailbox* message)))

(defun note (text)
  "Say something on the console from outside the display thread."
  (send :note text))

(defun elapsed (since)
  (/ (float (- (get-internal-real-time) since) 1.0)
     internal-time-units-per-second))

;;; Rendering

(defun format-seconds (seconds)
  "A short duration, or NIL when it is too small to be worth a column."
  (cond ((null seconds) nil)
        ((< seconds *quiet-seconds*) nil)
        ((< seconds 10) (format nil "~,1Fs" seconds))
        ((< seconds 600) (format nil "~Ds" (round seconds)))
        (t (multiple-value-bind (minutes secs) (floor (round seconds) 60)
             (format nil "~D:~2,'0D" minutes secs)))))

(defun counter (index total)
  (when total
    (format nil "[~VD/~VD]" *counter-width* index *counter-width* total)))

(defun blank-counter ()
  (make-string (+ 3 (* 2 *counter-width*)) :initial-element #\Space))

(defun row (kind counter name &optional seconds)
  "One line of the grid: kind, counter, name and time, each in its column."
  (string-right-trim
   " "
   (format nil ";; ~VA ~A  ~VA ~V@A"
           *kind-width* (or kind "")
           (or counter (blank-counter))
           *name-width* name
           *time-width* (or (format-seconds seconds) ""))))

;;; Display thread

(defstruct (record (:constructor make-record (kind label seconds system)))
  kind label seconds system)

(defstruct display
  (history '())
  (systems-total nil) (systems-done 0)
  (compiles-total nil) (compiles-done 0)
  (loads-total nil) (loads-done 0)
  (system nil) (system-start nil)
  (current nil) (current-kind nil) (current-start nil) (next-tick nil)
  (transient nil) (tripped nil) (failed nil)
  (start (get-internal-real-time)))

(defun action-row (state kind label &optional seconds)
  "The row an action gets, whether it is still running or finished."
  (ecase kind
    (:compile (row "compile" (counter (1+ (display-compiles-done state))
                                      (display-compiles-total state))
                   label seconds))
    (:load (row "load" (counter (1+ (display-loads-done state))
                                (display-loads-total state))
                label seconds))))

(defun clear-transient (state)
  (when (display-transient state)
    (when *tty-p*
      (format *console* "~C[2K~C" #\Escape #\Return))
    (setf (display-transient state) nil)))

(defun emit (state text)
  (clear-transient state)
  (format *console* "~A~%" text)
  (finish-output *console*))

(defun show-transient (state text)
  (cond (*tty-p*
         (format *console* "~C[2K~A~C" #\Escape text #\Return)
         (setf (display-transient state) t))
        (t
         ;; Without a terminal there is nothing to overwrite, so tick sparsely
         ;; rather than write one line per second into a log.
         (format *console* "~A~%" text)))
  (finish-output *console*))

(defun tick (state)
  "Redraw the in-flight action, and enforce the deadline."
  (let ((tripped (display-tripped state)))
    (when (and tripped (> (elapsed tripped) 5))
      ;; The build thread ignored its interrupt; do not leave it running.
      (summarize state :deadline)
      (sb-ext:exit :code 1 :abort t)))
  (let ((start (display-current-start state)))
    (when start
      (let ((seconds (elapsed start)))
        (when (and *deadline-seconds* (> seconds *deadline-seconds*))
          (return-from tick (trip-deadline state seconds)))
        (when (>= seconds (or (display-next-tick state) 1))
          (setf (display-next-tick state)
                (if *tty-p*
                    (+ (floor seconds) 1)
                    (max 5 (* 2 (floor seconds)))))
          (show-transient state (action-row state
                                            (display-current-kind state)
                                            (display-current state)
                                            seconds)))))))

(defun trip-deadline (state seconds)
  (let ((label (display-current state)))
    (emit state (row "DEADLINE" nil label seconds))
    (emit state (row "note" nil
                     (format nil "over the ~Ds limit; LUV_BUILD_DEADLINE=0 disables it"
                             *deadline-seconds*)))
    ;; Stop ticking and let the build thread die of the error; the summary is
    ;; printed when it comes back through FINISH.  The display loop must not
    ;; block here, so the grace period is checked on a later tick.
    (setf (display-failed state) label
          (display-current state) nil
          (display-current-start state) nil
          (display-tripped state) (get-internal-real-time))
    (when *build-thread*
      (sb-thread:interrupt-thread
       *build-thread*
       (lambda () (error 'deadline-exceeded :label label
                                            :seconds *deadline-seconds*))))))

(defun failure-log (state)
  (let ((label (or (display-failed state) (display-current state))))
    (namestring
     (uiop:enough-pathname
      (if label
          (merge-pathnames (concatenate 'string label ".log") *log-directory*)
          (build-log))
      *project-root*))))

(defun summarize (state reason)
  (clear-transient state)
  (let* ((history (display-history state))
         (compiles (remove-if-not (lambda (r) (eq (record-kind r) :compile))
                                  history))
         (total (elapsed (display-start state))))
    (emit state ";;")
    (emit state (row (ecase reason
                       (:done "built")
                       (:interrupted "stopped")
                       ((:deadline :error) "FAILED"))
                     nil
                     (format nil "~D compiled, ~D loaded, ~D system~:P"
                             (length compiles)
                             (display-loads-done state)
                             (display-systems-done state))
                     total))
    (dolist (record (let ((slowest (sort (copy-list compiles) #'>
                                         :key #'record-seconds)))
                      (subseq slowest 0 (min 3 (length slowest)))))
      (when (> (record-seconds record) 1)
        (emit state (row "slowest" nil (record-label record)
                         (record-seconds record)))))
    (when (and *log-directory* (member reason '(:deadline :error)))
      (emit state (row "log" nil (failure-log state))))))

(defun handle (state message)
  (destructuring-bind (kind &rest args) message
    (ecase kind
      (:plan
       (destructuring-bind (&key systems compiles loads) args
         (setf (display-systems-total state) systems
               (display-compiles-total state) compiles
               (display-loads-total state) loads
               *counter-width* (reduce #'max (list systems compiles loads)
                                       :key (lambda (n)
                                              (length (princ-to-string n)))))
         (emit state (row "plan" nil
                          (format nil "~D systems, ~D to compile, ~D to load"
                                  systems compiles loads)))))
      (:system-begin
       (destructuring-bind (name) args
         (setf (display-system state) name
               (display-system-start state) (get-internal-real-time))
         (emit state (row "system"
                          (counter (1+ (display-systems-done state))
                                   (display-systems-total state))
                          name))))
      (:system-end
       (destructuring-bind (name) args
         (let ((seconds (and (display-system-start state)
                             (elapsed (display-system-start state)))))
           (incf (display-systems-done state))
           ;; A system that took real time says so; the quick ones do not.
           (when (and seconds (>= seconds 1))
             (emit state (row "loaded"
                              (counter (display-systems-done state)
                                       (display-systems-total state))
                              name seconds))))
         (setf (display-system state) nil
               (display-system-start state) nil)))
      (:begin
       (destructuring-bind (action-kind label) args
         (setf (display-current state) label
               (display-current-kind state) action-kind
               (display-current-start state) (get-internal-real-time)
               (display-next-tick state) 1)))
      (:end
       (destructuring-bind (action-kind label seconds) args
         (setf (display-current state) nil
               (display-current-start state) nil)
         (push (make-record action-kind label seconds (display-system state))
               (display-history state))
         (ecase action-kind
           (:compile
            (emit state (action-row state :compile label seconds))
            (incf (display-compiles-done state)))
           (:load
            ;; Loading a fasl is usually instant; say so only when it is not.
            (when (>= seconds *load-line-seconds*)
              (emit state (action-row state :load label seconds)))
            (incf (display-loads-done state))))))
      (:note
       (destructuring-bind (text) args
         (emit state (row "note" nil text))))
      (:finish
       (destructuring-bind (reason) args
         (when (and (eq reason :error) (display-current state))
           (setf (display-failed state) (display-current state)))
         (summarize state reason))))))

(defun display-loop ()
  (let ((state (make-display)))
    (loop
      (let ((message (sb-concurrency:receive-message *mailbox*
                                                     :timeout *tick-interval*)))
        (cond ((null message) (tick state))
              (t (handle state message)
                 (when (eq (first message) :finish) (return))))))))

;;; Per-file logs, captured at the file descriptor level

(defvar *opened-logs* (make-hash-table :test #'equal))

(defun log-file-for (component)
  (let* ((path (ignore-errors (asdf:component-pathname component)))
         (relative (and path (uiop:enough-pathname path *project-root*)))
         (name (if (and relative (not (uiop:absolute-pathname-p relative)))
                   (namestring relative)
                   (format nil "systems/~A"
                           (asdf:component-name
                            (asdf:component-system component))))))
    (merge-pathnames (concatenate 'string name ".log") *log-directory*)))

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

(defun call-with-output-logged-to (path thunk)
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
      (unwind-protect (funcall thunk)
        (flush)
        (sb-posix:dup2 saved-out 1)
        (sb-posix:dup2 saved-err 2)
        (sb-posix:close saved-out)
        (sb-posix:close saved-err)
        (sb-posix:close log)))))

;;; Instrumentation

(defun component-label (component)
  (let ((path (ignore-errors (asdf:component-pathname component))))
    (if (and path (uiop:subpathp path *project-root*))
        (namestring (uiop:enough-pathname path *project-root*))
        (format nil "~A/~A"
                (asdf:component-name (asdf:component-system component))
                (asdf:component-name component)))))

(defun call-as-action (kind component thunk)
  (let ((label (component-label component))
        (start (get-internal-real-time))
        ;; Everything the compiler has to say is worth keeping, now that it is
        ;; written to this file's log rather than to the console.
        (*compile-verbose* t)
        (*compile-print* nil)
        (*load-verbose* t)
        (*load-print* nil))
    (send :begin kind label)
    (multiple-value-prog1
        (call-with-output-logged-to (log-file-for component) thunk)
      (send :end kind label (elapsed start)))))

(defmethod asdf:perform :around ((op asdf:compile-op) (c asdf:cl-source-file))
  (call-as-action :compile c #'call-next-method))

(defmethod asdf:perform :around ((op asdf:load-op) (c asdf:cl-source-file))
  (call-as-action :load c #'call-next-method))

;;; PREPARE-OP rather than LOAD-OP for the heading: the latter is performed
;;; after a system's own files, which would print each heading below the work
;;; it announces.
(defmethod asdf:perform :before ((op asdf:prepare-op) (system asdf:system))
  (send :system-begin (asdf:component-name system)))

(defmethod asdf:perform :after ((op asdf:load-op) (system asdf:system))
  (send :system-end (asdf:component-name system)))

;;; Plan

(defun report-plan (system)
  "Count what the build is about to do, so progress can be a fraction."
  (handler-case
      (let ((systems 0) (compiles 0) (loads 0))
        (dolist (action (asdf/plan:plan-actions
                         (asdf/plan:make-plan
                          nil (asdf:make-operation 'asdf:build-op)
                          (asdf:find-system system))))
          (let ((op (asdf/action:action-operation action))
                (component (asdf/action:action-component action)))
            (cond ((and (typep op 'asdf:prepare-op)
                        (typep component 'asdf:system))
                   (incf systems))
                  ((not (typep component 'asdf:cl-source-file)))
                  ((typep op 'asdf:compile-op) (incf compiles))
                  ((typep op 'asdf:load-op) (incf loads)))))
        (send :plan :systems systems :compiles compiles :loads loads))
    (error (e)
      (send :note (format nil "plan unavailable: ~A" e)))))

;;; Entry points

(defun start (project-root &key system)
  (setf *project-root* project-root
        *log-directory* (merge-pathnames "build/logs/" project-root)
        *console* (sb-sys:make-fd-stream (sb-posix:dup 2)
                                         :output t :buffering :line
                                         :external-format :utf-8)
        *tty-p* (plusp (sb-unix:unix-isatty 2))
        *mailbox* (sb-concurrency:make-mailbox :name "build progress")
        *build-thread* sb-thread:*current-thread*
        *display-thread* (sb-thread:make-thread #'display-loop
                                                :name "build display"))
  ;; Quiet by default: the compiler is made verbose only inside an action,
  ;; where its output is being recorded.  Left on globally it would narrate
  ;; every .asd it loads to the console.
  (setf *compile-verbose* nil *compile-print* nil
        *load-verbose* nil *load-print* nil)
  ;; From here the process writes to build.log, and each action redirects
  ;; again to its own file's log.  Output that belongs to no single file --
  ;; ASDF's chatter, a compilation unit's abort summary -- lands there rather
  ;; than interrupting the display.
  (redirect-output-to (build-log))
  (when system (report-plan system))
  (values))

(defun finish (reason)
  "Stop the display thread, after it has printed the summary.
Must run before SAVE-LISP-AND-DIE, which refuses to dump with threads alive."
  (when (and *display-thread* (sb-thread:thread-alive-p *display-thread*))
    (send :finish reason)
    (sb-thread:join-thread *display-thread* :timeout 10 :default nil))
  (setf *display-thread* nil)
  (values))
