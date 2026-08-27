;;;; Discover, prepare, and run the repository's ASDF test systems in parallel.

(handler-bind ((warning #'muffle-warning))
  (require :asdf)
  (require :sb-concurrency)
  (require :sb-posix))

(defparameter *project-root*
  (truename
   (merge-pathnames #P"../"
                    (uiop:pathname-directory-pathname *load-truename*))))

(defparameter *timings-path* (merge-pathnames #P"build/test-timings.sexp"
                                               *project-root*))
(defparameter *progress-interval* 5.0)
(defparameter *worker-mode* nil)
(defparameter *coordinator-mailbox* nil)
(defparameter *shardable-test-packages*
  '(("luft/render/test" . "LUFT.RENDER.TESTS")
    ("luvcraft/core/test" . "LUVCRAFT.TESTS")
    ("telegram/test" . "TELEGRAM.TESTS")))

(defstruct test-task
  name system package shard-index shard-count weight)

(defstruct test-worker
  id tasks pid stream thread status)

(defmethod asdf:perform :around ((operation asdf:compile-op)
                                 (component asdf:component))
  (if (and *worker-mode* (asdf:output-files operation component))
      (error "Parallel test worker attempted to compile ~A; preparation is incomplete."
             component)
      (call-next-method)))

(defun load-project-definitions ()
  (dolist (file (uiop:directory-files *project-root* #P"*.asd"))
    (asdf:load-asd file)))

(defun local-test-systems ()
  "Return every local ASDF system whose name ends in /test."
  (sort
   (loop for name in (asdf:registered-systems)
         for system = (asdf:find-system name nil)
         for source = (and system (asdf:system-source-file system))
         when (and source
                   (uiop:subpathp source *project-root*)
                   (uiop:string-suffix-p name "/test"))
           collect name)
   #'string<))

(defun suite-skip-reason (name)
  (when (and (member name '("luv/ghostty/test" "luv/terminal/test")
                          :test #'string=)
             (not (uiop:getenv "LUV_GHOSTTY_LIBRARY")))
    "libghostty-vt unavailable"))

(defun test-op-dependencies (name test-systems)
  (loop for action in
          (asdf/plan:plan-actions
           (asdf/plan:make-plan 'asdf/plan:sequential-plan
                                'asdf:test-op
                                (asdf:find-system name)))
        for operation = (asdf/action:action-operation action)
        for component = (asdf/action:action-component action)
        for dependency = (and (typep component 'asdf:system)
                              (asdf:component-name component))
        when (and (typep operation 'asdf:test-op)
                  dependency
                  (not (string= dependency name))
                  (member dependency test-systems :test #'string=))
          collect dependency))

(defun validate-test-catalog (test-systems)
  (dolist (name test-systems)
    (let ((dependencies (test-op-dependencies name test-systems)))
      (when dependencies
        (error "Test system ~A is not a leaf; it delegates to ~{~A~^, ~}."
               name dependencies)))))

(defun prepare-suites (suites)
  (load (merge-pathnames #P"luvcraft/build-progress.lisp" *project-root*))
  (asdf/session:with-asdf-session ()
    (asdf/forcing:make-forcing :performable-p t :system (first suites))
    (let* ((operation (asdf:make-operation 'asdf:load-op))
           (plan (asdf/plan:make-plan 'asdf/plan:sequential-plan
                                      operation
                                      (asdf:find-system (first suites)))))
      (dolist (suite (rest suites))
        (asdf/plan:traverse-action plan operation (asdf:find-system suite) t))
      (uiop:symbol-call :luv-build :start *project-root*
                        :system :test-suites
                        :plan plan
                        :invocation "make test prepares")
      (handler-case
          (progn
            (asdf/plan:perform-plan plan)
            (if (uiop:symbol-call :luv-build :finish :done) 1 0))
        (error (condition)
          (uiop:symbol-call :luv-build :failed (princ-to-string condition))
          (uiop:symbol-call :luv-build :finish :error)
          1)))))

(defun suite-log-path (directory name)
  (merge-pathnames
   (format nil "~A.log"
           (map 'string
                (lambda (character)
                  (if (or (alphanumericp character)
                          (find character "-_."))
                      character
                      #\-))
                name))
   directory))

(defun call-with-fd-output-to (path thunk)
  "Call THUNK with Lisp and inherited process output captured in PATH."
  (ensure-directories-exist path)
  (let ((log (sb-posix:open (uiop:native-namestring path)
                            (logior sb-posix:o-wronly sb-posix:o-creat
                                    sb-posix:o-trunc)
                            #o644))
        (saved-output (sb-posix:dup 1))
        (saved-error (sb-posix:dup 2)))
    (labels ((flush-output ()
               (ignore-errors (finish-output *standard-output*))
               (ignore-errors (finish-output *error-output*))))
      (unwind-protect
           (progn
             (flush-output)
             (sb-posix:dup2 log 1)
             (sb-posix:dup2 log 2)
             (funcall thunk))
        (flush-output)
        (sb-posix:dup2 saved-output 1)
        (sb-posix:dup2 saved-error 2)
        (sb-posix:close saved-output)
        (sb-posix:close saved-error)
        (sb-posix:close log)))))

(defun suite-failure-p (condition)
  (let* ((package (find-package :luv.test-support))
         (symbol (and package (find-symbol "SUITE-FAILED" package)))
         (class (and symbol (find-class symbol nil))))
    (and class (typep condition class))))

(defun emit-worker-event (event)
  (let ((*print-readably* nil)
        (*print-pretty* nil))
    (write event :stream *standard-output* :escape t)
    (terpri *standard-output*)
    (finish-output *standard-output*)))

(defun run-one-task (task directory)
  (let* ((name (test-task-name task))
         (log (suite-log-path directory name))
         (start (get-internal-real-time))
         (status :passed))
    (emit-worker-event (list :suite-started name))
    (handler-case
        (call-with-fd-output-to
         log
         (lambda ()
           (let ((*error-output* *standard-output*)
                 (*trace-output* *standard-output*))
             (handler-bind
                 ((error
                    (lambda (condition)
                      (unless (suite-failure-p condition)
                        (uiop:print-condition-backtrace
                         condition :stream *standard-output* :count 30)))))
               (if (test-task-package task)
                   (uiop:symbol-call
                    :luv.test-support :call-with-test-shard
                    (test-task-package task)
                    (test-task-shard-index task)
                    (test-task-shard-count task)
                    (lambda () (asdf:test-system (test-task-system task))))
                   (asdf:test-system (test-task-system task)))))))
      (error ()
        (setf status :failed)))
    (let ((seconds
            (/ (float (- (get-internal-real-time) start) 1.0)
               internal-time-units-per-second)))
      (emit-worker-event
       (list :suite-finished name status seconds (namestring log))))
    status))

(defun run-worker (directory tasks)
  (let ((*worker-mode* t)
        (failed nil))
    (dolist (task tasks)
      (when (eq :failed (run-one-task task directory))
        (setf failed t)))
    (if failed 1 0)))

(defun read-timings ()
  (handler-case
      (with-open-file (stream *timings-path*)
        (let ((*read-eval* nil)
              (value (read stream nil nil)))
          (if (and (listp value)
                   (every (lambda (entry)
                            (and (consp entry) (stringp (car entry))
                                 (realp (cdr entry)) (not (minusp (cdr entry)))))
                          value))
              value
              nil)))
    (error () nil)))

(defun write-timings (timings)
  (ensure-directories-exist *timings-path*)
  (let ((temporary
          (merge-pathnames
           (format nil "test-timings.~D.tmp" (sb-posix:getpid))
           (uiop:pathname-directory-pathname *timings-path*))))
    (unwind-protect
         (progn
           (with-open-file (stream temporary :direction :output
                                           :if-exists :supersede)
             (let ((*print-pretty* t))
               (write (sort (copy-list timings) #'string< :key #'car)
                      :stream stream)
               (terpri stream)))
           (uiop:rename-file-overwriting-target temporary *timings-path*))
      (when (probe-file temporary)
        (delete-file temporary)))))

(defun suite-weight (name timings)
  (or (cdr (assoc name timings :test #'string=)) 1.0))

(defun make-test-tasks (suites jobs timings)
  (let ((target (/ (reduce #'+ suites :key (lambda (name)
                                                  (suite-weight name timings)))
                           jobs)))
    (loop for system in suites
          for package = (cdr (assoc system *shardable-test-packages*
                                    :test #'string=))
          for weight = (suite-weight system timings)
          for shard-count =
            (if package
                (min (uiop:symbol-call :luv.test-support :test-root-count package)
                     (max (if (> jobs 1) 2 1)
                          (ceiling weight target)))
                1)
          append
          (progn
            (when (> shard-count 1)
              (unless (uiop:symbol-call :luv.test-support
                                        :shardable-test-package-p package)
                (error "Configured test package ~A is not independently shardable."
                       package)))
            (loop for shard-index below shard-count
                  for name = (if (= shard-count 1)
                                 system
                                 (format nil "~A [~D/~D]"
                                         system (1+ shard-index) shard-count))
                  collect
                  (make-test-task
                   :name name :system system
                   :package (and (> shard-count 1) package)
                   :shard-index shard-index :shard-count shard-count
                   :weight (/ weight shard-count)))))))

(defun assign-tasks (tasks worker-count)
  (let ((weights (make-array worker-count :initial-element 0.0))
        (assignments (make-array worker-count :initial-element nil)))
    (dolist (task (sort (copy-list tasks) #'> :key #'test-task-weight))
      (let ((worker
              (loop with lightest = 0
                    for index from 1 below worker-count
                    when (< (aref weights index) (aref weights lightest))
                      do (setf lightest index)
                    finally (return lightest))))
        (push task (aref assignments worker))
        (incf (aref weights worker) (test-task-weight task))))
    (loop for assignment across assignments
          collect (nreverse assignment))))

(defun test-run-directory ()
  (merge-pathnames
   (format nil "build/test-runs/~D-~D/"
           (get-universal-time) (sb-posix:getpid))
   *project-root*))

(defun fork-worker (id tasks directory)
  (multiple-value-bind (read-fd write-fd) (sb-posix:pipe)
    (finish-output *standard-output*)
    (finish-output *error-output*)
    (let ((pid (sb-posix:fork)))
      (if (zerop pid)
          (progn
            (sb-posix:close read-fd)
            (sb-posix:dup2 write-fd 1)
            (sb-posix:dup2 write-fd 2)
            (sb-posix:close write-fd)
            (let ((status
                    (handler-case (run-worker directory tasks)
                      (error (condition)
                        (uiop:print-condition-backtrace
                         condition :stream *standard-output* :count 30)
                        2))))
              (finish-output *standard-output*)
              (sb-posix:_exit status)))
          (progn
            (sb-posix:close write-fd)
            (make-test-worker
             :id id :tasks tasks :pid pid
             :stream (sb-sys:make-fd-stream
                      read-fd :input t :buffering :line :auto-close t
                      :external-format :utf-8
                      :name (format nil "test worker ~D output" id))))))))

(defun start-worker-reader (worker mailbox)
  (setf (test-worker-thread worker)
        (sb-thread:make-thread
         (lambda () (read-worker-events worker mailbox))
         :name (format nil "test worker ~D output" (test-worker-id worker))))
  worker)

(defun wait-worker (worker)
  (multiple-value-bind (pid status) (sb-posix:waitpid (test-worker-pid worker) 0)
    (declare (ignore pid))
    (setf (test-worker-status worker)
          (cond ((sb-posix:wifexited status) (sb-posix:wexitstatus status))
                ((sb-posix:wifsignaled status)
                 (+ 128 (sb-posix:wtermsig status)))
                (t status)))))

(defun read-worker-events (worker mailbox)
  (let ((stream (test-worker-stream worker)))
    (unwind-protect
         (loop for line = (read-line stream nil nil)
               while line
               do (handler-case
                      (let ((*read-eval* nil))
                        (multiple-value-bind (event end)
                            (read-from-string line)
                          (if (every (lambda (character)
                                       (find character " \t\r\n"))
                                     (subseq line end))
                              (sb-concurrency:send-message
                               mailbox
                               (list :event (test-worker-id worker) event))
                              (error "Trailing data"))))
                    (error ()
                      (sb-concurrency:send-message
                       mailbox
                       (list :worker-output (test-worker-id worker) line)))))
      (close stream)
      (sb-concurrency:send-message
       mailbox
       (list :worker-done (test-worker-id worker)
             (wait-worker worker))))))

(defun copy-log-to-console (path)
  (handler-case
      (with-open-file (stream path)
        (loop for line = (read-line stream nil nil)
              while line do (write-line line)))
    (error (condition)
      (format t "Could not read suite log ~A: ~A~%" path condition))))

(defun update-timing (name seconds timings)
  (let ((entry (assoc name timings :test #'string=)))
    (if entry
        (setf (cdr entry) seconds)
        (push (cons name seconds) timings)))
  timings)

(defun stop-workers (workers)
  (dolist (worker workers)
    (unless (test-worker-status worker)
      (ignore-errors (sb-posix:kill (test-worker-pid worker) sb-posix:sigterm))))
  (dolist (worker workers)
    (if (test-worker-thread worker)
        (ignore-errors (sb-thread:join-thread (test-worker-thread worker)
                                              :timeout 10))
        (ignore-errors (wait-worker worker)))))

(defun elapsed-seconds (start)
  (/ (float (- (get-internal-real-time) start) 1.0)
     internal-time-units-per-second))

(defun report-running-suites (active start)
  (let ((entries
          (sort (loop for worker-id being the hash-keys of active
                      using (hash-value suite)
                      collect (list worker-id (car suite)
                                    (elapsed-seconds (cdr suite))))
                #'< :key #'first)))
    (when entries
      (format t "~5,1,,,'0Fs       RUN    " (elapsed-seconds start))
      (loop for (worker-id name seconds) in entries
            for first = t then nil
            unless first do (write-string ", ")
            do (format t "~A (~,1Fs, worker ~D)" name seconds worker-id))
      (terpri)
      (finish-output))))

(defun update-aggregate-timings (tasks durations results timings)
  (dolist (system (remove-duplicates (mapcar #'test-task-system tasks)
                                     :test #'string=))
    (let ((system-tasks
            (remove-if-not (lambda (task)
                             (string= system (test-task-system task)))
                           tasks)))
      (when (and (> (length system-tasks) 1)
                 (every (lambda (task)
                          (let ((name (test-task-name task)))
                            (and (eq :passed (gethash name results))
                                 (assoc name durations :test #'string=))))
                        system-tasks))
        (setf timings
              (update-timing
               system
               (reduce #'+ system-tasks
                       :key (lambda (task)
                              (cdr (assoc (test-task-name task) durations
                                          :test #'string=))))
               timings)))))
  timings)

(defun run-parallel-suites (tasks jobs timings directory)
  (let* ((*coordinator-mailbox*
           (sb-concurrency:make-mailbox :name "test coordinator"))
         (assignments (assign-tasks tasks jobs))
         (workers nil)
         (results (make-hash-table :test #'equal))
         (active (make-hash-table))
         (finished-workers 0)
         (failures nil)
         (durations nil)
         (parallel-start (get-internal-real-time))
         (last-output parallel-start))
    (ensure-directories-exist directory)
    (unwind-protect
         (progn
           (loop for assignment in assignments
                 for id from 1
                 do (push (fork-worker id assignment directory) workers))
           ;; SB-POSIX:FORK requires a single Lisp thread. Start readers only
           ;; after every worker has inherited the quiescent prepared image.
           (dolist (worker workers)
             (start-worker-reader worker *coordinator-mailbox*))
           (loop until (= finished-workers (length workers))
                 do (multiple-value-bind (message received-p)
                        (sb-concurrency:receive-message
                         *coordinator-mailbox* :timeout 1)
                      (if received-p
                          (case (first message)
                            (:event
                             (destructuring-bind (kind worker-id event) message
                               (declare (ignore kind))
                               (case (first event)
                                 (:suite-started
                                  (let ((name (second event)))
                                    (setf (gethash worker-id active)
                                          (cons name (get-internal-real-time))
                                          last-output (get-internal-real-time))
                                    (format t "~5,1,,,'0Fs       START  ~A (worker ~D)~%"
                                            (elapsed-seconds parallel-start)
                                            name worker-id)
                                    (finish-output)))
                                 (:suite-finished
                                  (destructuring-bind
                                      (event-kind name status seconds log) event
                                    (declare (ignore event-kind))
                                    (remhash worker-id active)
                                    (setf last-output (get-internal-real-time))
                                    (if (gethash name results)
                                        (progn
                                          (push name failures)
                                          (format t "Duplicate result for ~A from worker ~D.~%"
                                                  name worker-id))
                                        (progn
                                          (setf (gethash name results) status
                                                timings
                                                (if (eq status :passed)
                                                    (update-timing name seconds
                                                                   timings)
                                                    timings))
                                          (push (cons name seconds) durations)
                                          (when (eq status :failed)
                                            (push name failures))
                                          (format t "~5,1,,,'0Fs ~2D/~D  ~6A ~A (~,1Fs, worker ~D)~%"
                                                  (elapsed-seconds parallel-start)
                                                  (hash-table-count results)
                                                  (length tasks)
                                                  (if (eq status :passed)
                                                      "PASS" "FAIL")
                                                  name seconds worker-id)
                                          (when (eq status :failed)
                                            (copy-log-to-console log))
                                          (finish-output))))))))
                            (:worker-output
                             (destructuring-bind (kind worker-id line) message
                               (declare (ignore kind))
                               (setf last-output (get-internal-real-time))
                               (format t "worker ~D: ~A~%" worker-id line)))
                            (:worker-done
                             (destructuring-bind (kind worker-id status) message
                               (declare (ignore kind))
                               (incf finished-workers)
                               (remhash worker-id active)
                               (when (and (not (zerop status))
                                          (notany
                                           (lambda (name)
                                             (eq :failed (gethash name results)))
                                           (mapcar #'test-task-name
                                                   (test-worker-tasks
                                                    (find worker-id workers
                                                          :key #'test-worker-id)))))
                                 (push (format nil "worker ~D" worker-id) failures)
                                 (format t "worker ~D exited unexpectedly (status ~D).~%"
                                         worker-id status)))))
                          (when (>= (elapsed-seconds last-output)
                                    *progress-interval*)
                            (report-running-suites active parallel-start)
                            (setf last-output (get-internal-real-time)))))))
      (stop-workers workers))
    (dolist (task tasks)
      (unless (gethash (test-task-name task) results)
        (push (test-task-name task) failures)
        (format t "~A: no result returned~%" (test-task-name task))))
    (values failures (update-aggregate-timings tasks durations results timings)
            durations)))

(defun report-slowest-suites (durations &optional (limit 5))
  (let ((slowest (subseq (sort (copy-list durations) #'> :key #'cdr)
                         0 (min limit (length durations)))))
    (when slowest
      (format t ";; Slowest test tasks:~%")
      (dolist (suite slowest)
        (format t ";; ~6,1Fs  ~A~%" (cdr suite) (car suite))))))

(defun parse-positive-integer (value option)
  (let ((integer (ignore-errors (parse-integer value :junk-allowed nil))))
    (unless (and integer (plusp integer))
      (error "~A requires a positive integer, not ~S." option value))
    integer))

(defun run-coordinator (jobs)
  (let* ((catalog (local-test-systems))
         (skipped
           (loop for suite in catalog
                 for reason = (suite-skip-reason suite)
                 when reason collect (cons suite reason)))
         (suites
           (remove-if (lambda (suite) (assoc suite skipped :test #'string=))
                      catalog))
         (timings (read-timings))
         (start (get-internal-real-time))
         (run-directory (test-run-directory)))
    (validate-test-catalog catalog)
    (dolist (skip skipped)
      (format t "~A (~A; skipped)~%" (car skip) (cdr skip)))
    (format t "Preparing ~D test suites for parallel execution...~%"
            (length suites))
    (finish-output)
    (unless (zerop (prepare-suites suites))
      (return-from run-coordinator 1))
    (let* ((tasks (make-test-tasks suites jobs timings))
           (worker-count (min jobs (length tasks))))
      (format t "Forking ~D prepared SBCL workers for ~D test tasks...~%"
              worker-count (length tasks))
      (finish-output)
      (multiple-value-bind (failures new-timings durations)
          (run-parallel-suites tasks worker-count timings run-directory)
        (write-timings new-timings)
        (format t "~&~%;; Tested ~D suite~:P in ~,1Fs with ~D SBCL worker~:P.~%"
                (length suites)
                (/ (float (- (get-internal-real-time) start) 1.0)
                   internal-time-units-per-second)
                worker-count)
        (when skipped
          (format t ";; Skipped ~D unavailable suite~:P.~%" (length skipped)))
        (report-slowest-suites durations)
        (if failures
            (progn
              (format t ";; Failed: ~{~A~^, ~}.~%"
                      (remove-duplicates failures :test #'equal))
              1)
            0)))))

(defun main ()
  (let ((arguments (uiop:command-line-arguments)))
    (unless (and (= (length arguments) 2)
                 (string= (first arguments) "--jobs"))
      (error "Expected --jobs N."))
    (uiop:quit
     (run-coordinator (parse-positive-integer (second arguments) "--jobs")))))

(handler-bind ((warning #'muffle-warning)
               (sb-ext:compiler-note #'muffle-warning))
  (load-project-definitions))

(main)
