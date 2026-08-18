;;;; Progress reporting for the luvcraft build.
;;;;
;;;; Two threads.  The build thread compiles; a display thread owns the
;;;; console and is the only thing that prints.  They speak over an
;;;; SB-CONCURRENCY mailbox, which the display thread reads with a timeout, so
;;;; that it keeps a heartbeat even while the build says nothing.  Past a
;;;; deadline the display thread interrupts the build thread and the build
;;;; fails: a file that takes more than five seconds to compile is a defect,
;;;; not a slow step.
;;;;
;;;; Nothing is muted.  Compiler chatter and the C toolchain's pkg-config and
;;;; gcc noise are redirected -- at the file descriptor level, since
;;;; CFFI-GROVEL runs its subprocesses with :OUTPUT :INTERACTIVE and so writes
;;;; past any rebound Lisp stream -- into one log file per source file under
;;;; build/logs/.  The display thread writes to a dup of the original stderr,
;;;; so it keeps the console while everything else is being recorded.
;;;;
;;;; The console gets one plain line per compiled file -- no cursor tricks, no
;;;; per-file durations, nothing to redraw -- stamped with how far into the
;;;; build it happened:
;;;;
;;;;   ;; T+012.7s   65/150  luvcraft/package.lisp
;;;;
;;;; Everything else is remembered rather than printed: every action with its
;;;; place and duration goes into a record, which becomes the stats at the end
;;;; and is saved as build.sexp beside the logs.

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
        5))
  "Seconds a single file may compile before the build is failed.
NIL disables the deadline; LUV_BUILD_DEADLINE overrides it, 0 disables it.")

(defvar *log-directory* nil)
(defvar *project-root* nil)
(defvar *build-id* nil)

(defparameter *id-characters* "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  "The wiki's alphabet for short references; a build gets one of the same shape.")

(defun make-build-id ()
  (let ((state (make-random-state t)))
    (coerce (loop repeat 6
                  collect (char *id-characters*
                                (random (length *id-characters*) state)))
            'string)))

(defun build-log ()
  (merge-pathnames "build.log" *log-directory*))

(defun logs-root ()
  (merge-pathnames "build/logs/" *project-root*))

(defparameter *tick-interval* 0.25
  "How often the display thread wakes when no event has arrived.")

(defparameter *quiet-seconds* 0.05
  "Durations below this are noise.")

;;; The grid

(defparameter *gutter-width* 9
  "The column holding either a word or a file's place in the plan.")

;;; The console: a dup of stderr, immune to the redirection below.

(defvar *console* nil)

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
  (current-log nil) (failed nil) (failed-log nil)
  (tripped nil)
  (start (get-internal-real-time)))

(defun since-start (state)
  (elapsed (display-start state)))

(defun emit (state gutter text &key stamp)
  "One line: when the work started, a gutter, and what it is.
Only the operations themselves are stamped; what frames them is not."
  (declare (ignorable state))
  (format *console* ";; ~8A ~V@A  ~A~%"
          (if stamp (format nil "T+~5,1,,,'0Fs" stamp) "")
          *gutter-width* (or gutter "") text)
  (finish-output *console*))

(defun compile-gutter (state)
  (format nil "~D/~D" (1+ (display-compiles-done state))
          (or (display-compiles-total state) 0)))

(defun tick (state)
  "Watch the clock over whatever the build thread is doing."
  (let ((tripped (display-tripped state)))
    (when (and tripped (> (elapsed tripped) 5))
      ;; The build thread ignored its interrupt; do not leave it running.
      (report state :deadline)
      (sb-ext:exit :code 1 :abort t)))
  (let ((start (display-current-start state)))
    (when (and start *deadline-seconds*
               (> (elapsed start) *deadline-seconds*))
      (trip-deadline state (elapsed start)))))

(defun trip-deadline (state seconds)
  (let ((label (display-current state)))
    (emit state "DEADLINE"
          (format nil "~A took ~A, over the ~Ds limit"
                  label (format-seconds seconds) *deadline-seconds*)
          :stamp (since-start state))
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
       (lambda () (error 'deadline-exceeded :label label
                                            :seconds *deadline-seconds*))))))

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
  "The stats a build is worth after the fact, and where its account is kept."
  (let* ((compiles (records-of state :compile))
         (loads (records-of state :load))
         (slowest (sort (copy-list compiles) #'> :key #'record-seconds))
         (sexp (write-sexp-log state reason)))
    (emit state (ecase reason
                  (:done "built")
                  (:interrupted "stopped")
                  ((:deadline :error) "FAILED"))
          (format nil "~D compiled, ~D loaded, ~D system~:P in ~A"
                  (length compiles) (display-loads-done state)
                  (display-systems-done state)
                  (format-seconds (since-start state))))
    (emit state "spent" (format nil "~A compiling, ~A loading"
                                (or (format-seconds (total-seconds compiles)) "0s")
                                (or (format-seconds (total-seconds loads)) "0s")))
    (dolist (record (subseq slowest 0 (min 3 (length slowest))))
      (when (> (record-seconds record) 0.5)
        (emit state "slowest" (format nil "~A ~A" (record-label record)
                                      (format-seconds (record-seconds record))))))
    (when (member reason '(:deadline :error))
      (emit state "log" (failure-log state)))
    (report-disk state sexp reason)))

(defun handle (state message)
  (destructuring-bind (kind &rest args) message
    (ecase kind
      (:plan
       (destructuring-bind (&key systems compiles loads) args
         (setf (display-systems-total state) systems
               (display-compiles-total state) compiles
               (display-loads-total state) loads
               *gutter-width* (max *gutter-width*
                                   (+ 1 (* 2 (length (princ-to-string compiles))))))
         (emit state "plan" (format nil "~D systems, ~D to compile, ~D to load"
                                    systems compiles loads))))
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
               (display-current-start state) (get-internal-real-time))
         ;; Announced before the work, so the stamp says when it began and a
         ;; file that never finishes has still named itself.
         (when (eq action-kind :compile)
           (emit state (compile-gutter state) label
                 :stamp (since-start state)))))
      (:end
       (destructuring-bind (action-kind label seconds) args
         (push (make-record action-kind label (display-system state)
                            (- (since-start state) seconds) seconds)
               (display-history state))
         (setf (display-current state) nil
               (display-current-start state) nil)
         (ecase action-kind
           (:compile (incf (display-compiles-done state)))
           (:load (incf (display-loads-done state))))))
      (:note
       (destructuring-bind (text &optional gutter) args
         (emit state (or gutter "note") text)))
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
              (t (handle state message)
                 (when (eq (first message) :finish) (return))))))))

;;; Per-file logs, captured at the file descriptor level

(defvar *opened-logs* (make-hash-table :test #'equal))

(defun log-file-for (component)
  "One log per system.  A system's name may contain slashes, which nest the
logs the way the systems themselves nest: luv.log beside luv/domains.log."
  (merge-pathnames
   (concatenate 'string
                (asdf:component-name (asdf:component-system component))
                ".log")
   *log-directory*))

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

;;; Instrumentation

(defun component-label (component)
  (let ((path (ignore-errors (asdf:component-pathname component))))
    (if (and path (uiop:subpathp path *project-root*))
        (namestring (uiop:enough-pathname path *project-root*))
        (format nil "~A/~A"
                (asdf:component-name (asdf:component-system component))
                (asdf:component-name component)))))

(defun call-as-action (kind component thunk)
  (let* ((label (component-label component))
         (log (log-file-for component))
         (start (get-internal-real-time))
         ;; Everything the compiler has to say is worth keeping, now that it is
        ;; written to this file's log rather than to the console: the form it
        ;; is on, and the notes ASDF would otherwise muffle.
         (*compile-verbose* t)
         (*compile-print* t)
         (*load-verbose* t)
         (*load-print* t)
         (uiop:*uninteresting-conditions* '()))
    (send :begin kind label (namestring (uiop:enough-pathname log *project-root*)))
    (multiple-value-prog1
        (call-with-output-logged-to log thunk (format nil "~(~A~) ~A" kind label))
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
  (cond ((< bytes 1024) (format nil "~DB" bytes))
        ((< bytes (* 1024 1024)) (format nil "~,1FK" (/ bytes 1024.0)))
        ((< bytes (* 1024 1024 1024)) (format nil "~,1FM" (/ bytes 1048576.0)))
        (t (format nil "~,1FG" (/ bytes 1073741824.0)))))

(defun log-directory-id (directory)
  (car (last (pathname-directory directory))))

(defun archive-log-directory (directory)
  "tar.zst one build's logs and remove the directory it came from."
  (let* ((id (log-directory-id directory))
         (archive (merge-pathnames (format nil "~A.tar.zst" id) (logs-root))))
    (uiop:run-program (list "tar" "--use-compress-program" "zstd -9"
                            "-cf" (sb-ext:native-namestring archive)
                            "-C" (sb-ext:native-namestring (logs-root))
                            id)
                      :output :interactive :error-output :interactive)
    (uiop:delete-directory-tree directory :validate t)
    archive))

(defun compact-logs ()
  "Archive every build's logs, this one and any failures left lying about.
Returns the current build's archive, and how many were made."
  (let ((current nil) (count 0))
    ;; The symlink would otherwise be walked as a directory of its own.
    (ignore-errors
     (sb-posix:unlink (sb-ext:native-namestring
                       (merge-pathnames "latest" (logs-root)))))
    (dolist (directory (uiop:subdirectories (logs-root)))
      (let ((archive (ignore-errors (archive-log-directory directory))))
        (when archive
          (incf count)
          (when (equal (log-directory-id directory) *build-id*)
            (setf current archive)))))
    (when current
      (ignore-errors
       (sb-posix:symlink (file-namestring current)
                         (sb-ext:native-namestring
                          (merge-pathnames "latest" (logs-root))))))
    (values current count)))

(defun report-disk (state sexp reason)
  "Say what this build's account cost, and what the tree costs now.
A build that worked earns the right to pack away every build's logs, including
the failures that led up to it.  This runs on the display thread, after the
sexp log is written, so the archive contains it."
  (let ((raw (directory-bytes *log-directory*)))
    (emit state "sexp" (namestring (uiop:enough-pathname sexp *project-root*)))
    (if (eq reason :done)
        (multiple-value-bind (archive count) (compact-logs)
          (emit state "logs"
                (if archive
                    (format nil "this build ~A -> ~A, ~D build~:P archived"
                            (human-bytes raw) (human-bytes (file-bytes archive))
                            count)
                    (format nil "~A, not packed" (human-bytes raw)))))
        (emit state "logs"
              (format nil "~A in ~A" (human-bytes raw)
                      (namestring (uiop:enough-pathname *log-directory*
                                                        *project-root*)))))
    (emit state "disk"
          (format nil "build/logs ~A, build/ ~A"
                  (human-bytes (directory-bytes (logs-root)))
                  (human-bytes (directory-bytes
                                (merge-pathnames "build/" *project-root*)))))))

;;; Entry points

(defun start (project-root &key system)
  (setf *project-root* project-root
        *build-id* (make-build-id)
        *log-directory* (merge-pathnames (format nil "build/logs/~A/" *build-id*)
                                         project-root)
        *console* (sb-sys:make-fd-stream (sb-posix:dup 2)
                                         :output t :buffering :line
                                         :external-format :utf-8)
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
  (link-latest-logs)
  ;; After the plan, so the id lines up with the columns it sets.
  (when system (report-plan system))
  (send :note (format nil "~A in ~A" *build-id*
                      (uiop:enough-pathname *log-directory* *project-root*))
        "build")
  (values))

(defun finish (reason)
  "Ask the display thread for its report, and wait for it to be done.
Must run before SAVE-LISP-AND-DIE, which refuses to dump an image while
another thread is alive."
  (when (and *display-thread* (sb-thread:thread-alive-p *display-thread*))
    (send :finish reason)
    (sb-thread:join-thread *display-thread* :timeout 60 :default nil))
  (setf *display-thread* nil)
  (values))
