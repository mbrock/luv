;;; A shared, retained source-update instrument: fetch and review Git changes,
;;; explicitly fast-forward to the reviewed commit, then assimilate selected
;;; ASDF systems into the running image.

(in-package #:mcluv)

(defconstant +source-update-width+ 760)
(defconstant +source-update-height+ 620)
(defconstant +source-update-margin+ 24)
(defconstant +source-update-line-height+ 22)
(defconstant +source-update-content-top+ 92)
(defconstant +source-update-button-top+ 548)
(defconstant +source-update-button-bottom+ 590)
(defconstant +source-update-viewport-margin+ 18)

(defun source-update-alpha-ink (red green blue alpha)
  (compose-in (make-rgb-color red green blue) (make-opacity alpha)))

(defparameter *source-update-shadow-ink*
  (source-update-alpha-ink 0.0 0.0 0.0 0.38))
(defparameter *source-update-edge-ink*
  (source-update-alpha-ink 0.36 0.43 0.48 0.94))
(defparameter *source-update-panel-ink*
  (source-update-alpha-ink 0.055 0.065 0.075 0.96))
(defparameter *source-update-row-ink*
  (source-update-alpha-ink 0.11 0.13 0.15 0.82))
(defparameter *source-update-button-ink*
  (source-update-alpha-ink 0.22 0.48 0.37 0.96))
(defparameter *source-update-button-muted-ink*
  (source-update-alpha-ink 0.18 0.20 0.22 0.88))
(defparameter *source-update-text-ink* (make-rgb-color 0.91 0.93 0.94))
(defparameter *source-update-muted-ink* (make-rgb-color 0.59 0.64 0.68))
(defparameter *source-update-accent-ink* (make-rgb-color 0.57 0.86 0.70))
(defparameter *source-update-error-ink* (make-rgb-color 0.96 0.49 0.43))

(defstruct source-update-snapshot
  state
  heading
  lines
  footer
  base
  target)

(defgeneric source-update-root-for (owner)
  (:documentation "Return OWNER's managed source checkout directory."))

(defmethod source-update-root-for ((owner t))
  (declare (ignore owner))
  (or (and (boundp 'cl-user::*luv-project-root*)
           cl-user::*luv-project-root*)
      (asdf:system-source-directory "luv")))

(defgeneric source-update-systems-for (owner)
  (:documentation "Return the ASDF systems assimilated after updating OWNER."))

(defmethod source-update-systems-for ((owner t))
  (declare (ignore owner))
  '("luv"))

(defgeneric source-update-title-for (owner)
  (:documentation "Return OWNER's short source-update panel title."))

(defmethod source-update-title-for ((owner t))
  (format nil "~(~A~) source update" (type-of owner)))

(defclass source-update-session ()
  ((root :initarg :root :reader source-update-root)
   (systems :initarg :systems :reader source-update-systems)
   (loader :initarg :loader :reader source-update-loader)
   (build-run :initform nil :accessor source-update-build-run)
   (lock :initform (sb-thread:make-mutex :name "source update session")
         :reader source-update-lock)
   (revision :initform 0 :accessor source-update-revision)
   (snapshot
    :initform (make-source-update-snapshot
               :state :idle :heading "Source update"
               :lines '("Preparing source update…")
               :footer "Please wait.")
    :accessor source-update-session-snapshot)
   (worker :initform nil :accessor source-update-worker)
   (process :initform nil :accessor source-update-process)
   (accepting-p :initform t :accessor source-update-accepting-p)))

(define-condition source-update-git-error (error)
  ((arguments :initarg :arguments :reader source-update-git-arguments)
   (status :initarg :status :reader source-update-git-status)
   (output :initarg :output :reader source-update-git-output))
  (:report
   (lambda (condition stream)
     (format stream "git ~{~A~^ ~} failed (~D)~@[ — ~A~]"
             (source-update-git-arguments condition)
             (source-update-git-status condition)
             (let ((output (source-update-git-output condition)))
               (and (plusp (length output)) output))))))

(defun source-update-clean-text (text &optional (limit 1000))
  "Return bounded single-purpose display text without terminal controls."
  (let* ((text (or text ""))
         (clean
           (map 'string
                (lambda (character)
                  (if (or (graphic-char-p character)
                          (member character '(#\Newline #\Tab)))
                      character
                      #\Space))
                text)))
    (if (> (length clean) limit)
        (concatenate 'string (subseq clean 0 (- limit 1)) "…")
        clean)))

(defun source-update-lines (text)
  (remove-if (lambda (line) (zerop (length line)))
             (uiop:split-string (source-update-clean-text text)
                                :separator '(#\Newline))))

(defun source-update-line (text &optional (limit 92))
  (let ((text (source-update-clean-text text limit)))
    (substitute #\Space #\Tab text)))

(defun source-update-command (arguments)
  (append '("env" "GIT_TERMINAL_PROMPT=0"
            "GIT_SSH_COMMAND=ssh -oBatchMode=yes" "git")
          arguments))

(defun source-update-finish-git
    (arguments output error-output status accepted-statuses)
  (let ((combined
          (string-trim '(#\Space #\Tab #\Newline #\Return)
                       (format nil "~A~@[~%~A~]" output
                               (and error-output
                                    (plusp (length error-output))
                                    error-output)))))
    (unless (member status accepted-statuses)
      (error 'source-update-git-error
             :arguments arguments :status status
             :output (source-update-clean-text combined 600)))
    (values (string-trim '(#\Space #\Tab #\Newline #\Return) output)
            combined status)))

(defun source-update-cancellable-git
    (session arguments accepted-statuses)
  "Run Git while allowing session quiescence to terminate the process."
  (let ((process
          (uiop:launch-program
           (source-update-command arguments)
           :directory (source-update-root session)
           :input nil :output :stream :error-output :output)))
    (unwind-protect
         (progn
           (sb-thread:with-mutex ((source-update-lock session))
             (if (source-update-accepting-p session)
                 (setf (source-update-process session) process)
                 (uiop:terminate-process process)))
           (let ((output
                   (uiop:slurp-stream-string
                    (uiop:process-info-output process)))
                 (status (uiop:wait-process process)))
             (source-update-finish-git
              arguments output nil status accepted-statuses)))
      (sb-thread:with-mutex ((source-update-lock session))
        (when (eq process (source-update-process session))
          (setf (source-update-process session) nil))))))

(defun source-update-git
    (session arguments &key (accepted-statuses '(0)) cancellable-p)
  "Run Git without a shell or interactive credential prompts."
  (if cancellable-p
      (source-update-cancellable-git session arguments accepted-statuses)
      (multiple-value-bind (output error-output status)
          (uiop:run-program
           (source-update-command arguments)
           :directory (source-update-root session)
           :input nil :output :string :error-output :string
           :ignore-error-status t)
        (source-update-finish-git
         arguments output error-output status accepted-statuses))))

(defun source-update-git-path-present-p (session name)
  (multiple-value-bind (path) (source-update-git session (list "rev-parse" "--git-path" name))
    (let ((pathname (pathname path)))
      (let ((resolved
              (if (uiop:absolute-pathname-p pathname)
                  pathname
                  (merge-pathnames pathname (source-update-root session)))))
        (or (uiop:file-exists-p resolved)
            (uiop:directory-exists-p resolved))))))

(defun source-update-preflight (session)
  "Return current HEAD after verifying the checkout is safe to advance."
  (multiple-value-bind (top)
      (source-update-git session '("rev-parse" "--show-toplevel"))
    (unless (string= (namestring (truename (source-update-root session)))
                     (namestring
                      (truename (uiop:ensure-directory-pathname top))))
      (error "Git top level ~A is not the managed source root ~A"
             top (source-update-root session))))
  (multiple-value-bind (branch output status)
      (source-update-git
       session '("symbolic-ref" "--quiet" "--short" "HEAD")
       :accepted-statuses '(0 1))
    (declare (ignore output))
    (unless (and (zerop status) (string= branch "main"))
      (error "Source update requires branch main; found ~A"
             (if (zerop status) branch "a detached HEAD"))))
  (multiple-value-bind (status)
      (source-update-git
       session '("status" "--porcelain=v1" "--untracked-files=normal"))
    (when (plusp (length status))
      (error "Source update requires a clean worktree: ~A"
             (source-update-line status 240))))
  (dolist (marker '("MERGE_HEAD" "CHERRY_PICK_HEAD" "REVERT_HEAD"
                    "rebase-apply" "rebase-merge"))
    (when (source-update-git-path-present-p session marker)
      (error "Source update is blocked by Git operation state ~A" marker)))
  (source-update-git session '("rev-parse" "HEAD")))

(defun source-update-abbreviated-oid (oid)
  (subseq oid 0 (min 10 (length oid))))

(defun source-update-bounded-lines (lines limit)
  (append (subseq lines 0 (min limit (length lines)))
          (when (> (length lines) limit)
            (list (format nil "… and ~D more" (- (length lines) limit))))))

(defun %publish-source-update (session snapshot)
  "Publish SNAPSHOT while SESSION's lock is already held."
  (setf (source-update-session-snapshot session) snapshot)
  (incf (source-update-revision session))
  snapshot)

(defun publish-source-update (session snapshot)
  (sb-thread:with-mutex ((source-update-lock session))
    (%publish-source-update session snapshot)))

(defun current-source-update-snapshot (session)
  "Return SESSION's immutable snapshot and revision."
  (sb-thread:with-mutex ((source-update-lock session))
    (values (source-update-session-snapshot session)
            (source-update-revision session))))

(defun source-update-condition-text (condition)
  (source-update-line (princ-to-string condition) 420))

(defun run-source-update-fetch (session)
  (handler-case
      (let ((base (source-update-preflight session)))
        (source-update-git
         session '("fetch" "--no-tags" "origin" "main")
         :cancellable-p t)
        ;; FETCH_HEAD is the exact object fetched by this operation, independent
        ;; of later remote-tracking-ref movement.
        (let ((target (source-update-git session '("rev-parse" "FETCH_HEAD"))))
          (if (string= base target)
              (publish-source-update
               session
               (make-source-update-snapshot
                :state :current :heading "Source is current"
                :lines
                (list (format nil "main at ~A"
                              (source-update-abbreviated-oid base))
                      "origin/main has no commits to apply.")
                :footer "R fetches again · Esc closes"
                :base base :target target))
              (progn
                (source-update-git
                 session (list "merge-base" "--is-ancestor" base target))
                (let* ((count
                         (source-update-git
                          session (list "rev-list" "--count"
                                        (format nil "~A..~A" base target))))
                       (commits
                         (source-update-lines
                          (source-update-git
                           session
                           (list "log" "--format=%h  %ad  %s"
                                 "--date=short"
                                 (format nil "~A..~A" base target)))))
                       (paths
                         (source-update-lines
                          (source-update-git
                           session
                           (list "diff" "--name-status" base target))))
                       (lines
                         (append
                          (list
                           (format nil "main ~A → ~A"
                                   (source-update-abbreviated-oid base)
                                   (source-update-abbreviated-oid target))
                           ""
                           (format nil "Commits (~A)" count))
                          (mapcar #'source-update-line
                                  (source-update-bounded-lines commits 6))
                          (list "" "Changed paths")
                          (mapcar #'source-update-line
                                  (source-update-bounded-lines paths 8)))))
                  (publish-source-update
                   session
                   (make-source-update-snapshot
                    :state :review
                    :heading (format nil "Review ~A incoming commit~:P"
                                     (parse-integer count))
                    :lines lines
                    :footer "A applies reviewed commit · R refetches · Esc closes"
                    :base base :target target)))))))
    (error (condition)
      (publish-source-update
       session
       (make-source-update-snapshot
        :state :failed :heading "Could not review source update"
        :lines (list (source-update-condition-text condition))
        :footer "R retries · Esc closes")))))

(defun source-update-fence-lines (fences)
  (if fences
      (loop for (canvas . outcome) in fences
            collect
            (source-update-line
             (format nil "Frame ~A: ~(~A~)" (luv:canvas-title canvas) outcome)))
      '("No open canvases required a frame fence.")))

(defun source-update-build-diagnostic-lines (snapshot)
  (let* ((diagnostic
           (or (build:run-snapshot-terminal-diagnostic snapshot)
               (first (last (build:run-snapshot-diagnostics snapshot)))))
         (action
           (and diagnostic
                (find (build:build-diagnostic-snapshot-action-id diagnostic)
                      (build:run-snapshot-actions snapshot)
                      :key #'build:build-action-snapshot-id)))
         (location
           (and diagnostic
                (build:build-diagnostic-snapshot-source-location diagnostic))))
    (when diagnostic
      (append
       (when action
         (list (format nil "Failed ~(~A~): ~A"
                       (build:build-action-snapshot-kind action)
                       (build:build-action-snapshot-label action))))
       (when (and location
                  (build:build-source-location-namestring location))
         (list (format nil "At ~A~@[: form ~D~]"
                       (build:build-source-location-namestring location)
                       (build:build-source-location-form-number location))))
       (source-update-lines
        (build:build-diagnostic-snapshot-report diagnostic))))))

(defun install-source-update-build-run (session run)
  (sb-thread:with-mutex ((source-update-lock session))
    (setf (source-update-build-run session) run)
    (incf (source-update-revision session)))
  run)

(defun clear-source-update-build-run (session)
  (sb-thread:with-mutex ((source-update-lock session))
    (setf (source-update-build-run session) nil)
    (incf (source-update-revision session)))
  session)

(defun execute-source-update-build (session run)
  "Execute RUN through the injected test loader or the live ASDF executor."
  (let ((loader (source-update-loader session)))
    (if loader
        (build:call-with-build-run
         run
         (lambda ()
           (funcall loader (source-update-systems session))))
        (multiple-value-bind (result fences)
            (luv:call-with-live-system-load
             (lambda ()
               (build:execute-run
                (make-instance 'build:asdf-build-executor
                               :signal-errors-p nil)
                run)))
          (declare (ignore result))
          fences))))

(defun publish-source-update-assimilation-failure
    (session base target run &optional condition)
  (let* ((snapshot (build:run-snapshot run))
         (diagnostic-lines (source-update-build-diagnostic-lines snapshot)))
    (publish-source-update
     session
     (make-source-update-snapshot
      :state :failed :heading "Checkout advanced; assimilation failed"
      :lines
      (append
       (list
        (format nil "main advanced to ~A; image may be partially redefined."
                (source-update-abbreviated-oid target)))
       diagnostic-lines
       (when (and condition (null diagnostic-lines))
         (source-update-lines (princ-to-string condition))))
      :footer "A retries assimilation · R checks source · Esc closes"
      :base base :target target))))

(defun run-source-update-assimilation (session base target)
  (let* ((run
           (install-source-update-build-run
            session
            (build:make-build-run
             (source-update-root session)
             :systems (source-update-systems session))))
         (shader-before (shader:shader-source-revision))
         (fences nil)
         (failure nil))
    (handler-case
        (setf fences (execute-source-update-build session run))
      (error (condition)
        (setf failure condition)))
    (let ((run-snapshot (build:run-snapshot run)))
      (if (eq :succeeded (build:run-snapshot-state run-snapshot))
          (let ((shader-after (shader:shader-source-revision)))
            (publish-source-update
             session
             (make-source-update-snapshot
              :state :complete :heading "Source update assimilated"
              :lines
              (append
               (list
                (format nil "main now at ~A"
                        (source-update-abbreviated-oid target))
                (format nil "Loaded ~{~A~^, ~}"
                        (source-update-systems session))
                (if (= shader-before shader-after)
                    (format nil "Shader source revision unchanged at ~D"
                            shader-after)
                    (format nil "Shader source revision ~D → ~D"
                            shader-before shader-after)))
               (source-update-fence-lines fences))
              :footer "R checks for another update · Esc closes"
              :base base :target target)))
          (publish-source-update-assimilation-failure
           session base target run failure)))))

(defun publish-source-update-apply-failure
    (session base target condition)
  (let ((advanced-p
          (ignore-errors
           (string= target
                    (source-update-git session '("rev-parse" "HEAD"))))))
    (if (and advanced-p (source-update-build-run session))
        (publish-source-update-assimilation-failure
         session base target (source-update-build-run session) condition)
        (publish-source-update
         session
         (make-source-update-snapshot
          :state :failed
          :heading (if advanced-p
                       "Checkout advanced; assimilation did not start"
                       "Source update failed before assimilation")
          :lines
          (append
           (when advanced-p
             (list
              (format nil "main advanced to ~A; image may be partially redefined."
                      (source-update-abbreviated-oid target))))
           (source-update-lines (princ-to-string condition)))
          :footer (if advanced-p
                      "A retries assimilation · R checks source · Esc closes"
                      "R retries review · Esc closes")
          :base base :target target)))))

(defun run-source-update-apply (session base target)
  (handler-case
      (progn
        (let ((head (source-update-preflight session)))
          (unless (string= head base)
            (error "HEAD moved since review (~A instead of ~A); fetch again"
                   (source-update-abbreviated-oid head)
                   (source-update-abbreviated-oid base))))
        (source-update-git
         session (list "cat-file" "-e" (format nil "~A^{commit}" target)))
        (source-update-git
         session (list "merge-base" "--is-ancestor" base target))
        (source-update-git session (list "merge" "--ff-only" target))
        (clear-source-update-build-run session)
        (publish-source-update
         session
         (make-source-update-snapshot
          :state :loading :heading "Checkout advanced; assimilating Lisp"
          :lines
          (list
           (format nil "main ~A → ~A"
                   (source-update-abbreviated-oid base)
                   (source-update-abbreviated-oid target))
           (format nil "Loading ~{~A~^, ~} with canvas frames held…"
                   (source-update-systems session)))
          :footer "ASDF loading cannot be cancelled"
          :base base :target target))
        (run-source-update-assimilation session base target))
    (error (condition)
      (publish-source-update-apply-failure
       session base target condition))))

(defun source-update-start-worker (session function name)
  "Install a worker while SESSION's lock is held, before it can publish."
  (let ((thread (sb-thread:make-thread function :name name)))
    (setf (source-update-worker session) thread)
    thread))

(defun request-source-update-fetch (session)
  "Start a non-mutating fetch and review unless SESSION is already busy."
  (sb-thread:with-mutex ((source-update-lock session))
    (when (and (source-update-accepting-p session)
               (not
                (member (source-update-snapshot-state
                         (source-update-session-snapshot session))
                        '(:fetching :applying :loading))))
      (%publish-source-update
       session
       (make-source-update-snapshot
        :state :fetching :heading "Fetching origin/main"
        :lines '("Checking the worktree and contacting origin…")
        :footer "Rendering remains live while Git works."))
      (source-update-start-worker
       session (lambda () (run-source-update-fetch session))
       "source update fetch"))))

(defun request-source-update-apply (session)
  "Apply the exact commit in SESSION's current review snapshot."
  (sb-thread:with-mutex ((source-update-lock session))
    (let ((snapshot (source-update-session-snapshot session)))
      (when (and (source-update-accepting-p session)
                 (eq :review (source-update-snapshot-state snapshot)))
        (let ((base (source-update-snapshot-base snapshot))
              (target (source-update-snapshot-target snapshot)))
          (%publish-source-update
           session
           (make-source-update-snapshot
            :state :applying :heading "Applying reviewed fast-forward"
            :lines
            (list
             (format nil "Revalidating main at ~A before advancing to ~A…"
                     (source-update-abbreviated-oid base)
                     (source-update-abbreviated-oid target)))
            :footer "Git and ASDF work cannot be cancelled"
            :base base :target target))
          (source-update-start-worker
           session (lambda () (run-source-update-apply session base target))
           "source update apply"))))))

(defun run-source-update-retry (session base target)
  (handler-case
      (let ((head (source-update-preflight session)))
        (unless (string= head target)
          (error "Retry requires main at ~A; found ~A"
                 (source-update-abbreviated-oid target)
                 (source-update-abbreviated-oid head)))
        (run-source-update-assimilation session base target))
    (error (condition)
      (publish-source-update-apply-failure
       session base target condition))))

(defun request-source-update-retry (session)
  "Retry assimilation when SESSION already advanced to its reviewed commit."
  (sb-thread:with-mutex ((source-update-lock session))
    (let ((snapshot (source-update-session-snapshot session)))
      (when (and (source-update-accepting-p session)
                 (eq :failed (source-update-snapshot-state snapshot))
                 (source-update-snapshot-target snapshot)
                 (source-update-build-run session))
        (let ((base (source-update-snapshot-base snapshot))
              (target (source-update-snapshot-target snapshot)))
          (%publish-source-update
           session
           (make-source-update-snapshot
            :state :loading :heading "Retrying Lisp assimilation"
            :lines
            (list
             (format nil "main remains at ~A"
                     (source-update-abbreviated-oid target))
             (format nil "Loading ~{~A~^, ~} with canvas frames held…"
                     (source-update-systems session)))
            :footer "ASDF loading cannot be cancelled"
            :base base :target target))
          (source-update-start-worker
           session (lambda () (run-source-update-retry session base target))
           "source update assimilation retry"))))))

(defun make-source-update-session
    (root systems &key loader (start-p t))
  "Create a source update session, optionally starting its first fetch."
  (let ((session
          (make-instance
           'source-update-session
           :root (uiop:ensure-directory-pathname (truename root))
           :systems (mapcar #'string systems)
           :loader loader)))
    (when start-p (request-source-update-fetch session))
    session))

(defun start-source-update (frame)
  "Begin FRAME's first fetch after its host has successfully attached it."
  (request-source-update-fetch (source-update-frame-session frame))
  frame)

(defun source-update-busy-p (session)
  (multiple-value-bind (snapshot) (current-source-update-snapshot session)
    (member (source-update-snapshot-state snapshot)
            '(:fetching :applying :loading))))

(defun wait-source-update-session (session)
  "Wait for SESSION's current worker, if any, and return its snapshot."
  (let ((worker
          (sb-thread:with-mutex ((source-update-lock session))
            (source-update-worker session))))
    (when (and worker (not (eq worker sb-thread:*current-thread*)))
      (sb-thread:join-thread worker)))
  (current-source-update-snapshot session))

(defun quiesce-source-update-session (session)
  "Close operation admission, cancel an active fetch, and join the worker.

Merge and ASDF assimilation are never interrupted; callers wait for them."
  (multiple-value-bind (worker process)
      (sb-thread:with-mutex ((source-update-lock session))
        (setf (source-update-accepting-p session) nil)
        (values (source-update-worker session)
                (source-update-process session)))
    (when process
      (ignore-errors (uiop:terminate-process process)))
    (when (and worker (not (eq worker sb-thread:*current-thread*)))
      (sb-thread:join-thread worker)))
  session)

;;; ---------------------------------------------------------------------
;;; Retained direct-GPU panel.

(defclass source-update-pane (transparent-gpu-application-pane) ())

(defclass source-update-state ()
  ((session :initarg :session :initform nil
            :reader source-update-frame-session)
   (snapshot :initform nil :accessor source-update-frame-snapshot)
   (observed-revision :initform -1 :accessor source-update-observed-revision)
   (build-snapshot :initform nil :accessor source-update-frame-build-snapshot)
   (observed-build-id :initform nil :accessor source-update-observed-build-id)
   (observed-build-revision
    :initform -1 :accessor source-update-observed-build-revision)
   (dirty-p :initform t :accessor source-update-dirty-p)))

(define-application-frame source-update
    (standard-application-frame source-update-state)
  ()
  (:menu-bar nil)
  (:panes
   (panel (make-pane 'source-update-pane
                     :background +transparent-ink+
                     :width +source-update-width+
                     :height +source-update-height+
                     :min-width +source-update-width+
                     :min-height +source-update-height+
                     :max-width +source-update-width+
                     :max-height +source-update-height+)))
  (:layouts (default panel)))

(defmethod initialize-instance :after ((frame source-update-state) &key)
  (when (source-update-frame-session frame)
    (multiple-value-bind (snapshot revision)
        (current-source-update-snapshot (source-update-frame-session frame))
      (setf (source-update-frame-snapshot frame) snapshot
            (source-update-observed-revision frame) revision))))

(defun source-update-state-label (state)
  (ecase state
    (:idle "idle")
    (:fetching "fetching")
    (:current "current")
    (:review "review")
    (:applying "applying")
    (:loading "loading")
    (:complete "complete")
    (:failed "failed")))

(defun source-update-action-label (snapshot)
  (case (source-update-snapshot-state snapshot)
    (:review "Apply reviewed update")
    ((:fetching :applying :loading) "Working…")
    (:failed
     (if (source-update-snapshot-target snapshot)
         "Retry assimilation"
         "Fetch again"))
    (t "Fetch again")))

(defun source-update-action-enabled-p (snapshot)
  (not (member (source-update-snapshot-state snapshot)
               '(:fetching :applying :loading))))

(defun source-update-build-progress-lines (snapshot)
  (when snapshot
    (let* ((actions (build:run-snapshot-actions snapshot))
           (completed
             (count-if (lambda (action)
                         (member (build:build-action-snapshot-state action)
                                 '(:succeeded :failed)))
                       actions))
           (plan (build:run-snapshot-plan-summary snapshot))
           (current (build:run-snapshot-current-action snapshot)))
      (append
       (list
        (format nil "Build ~A · ~(~A~) · ~D action~:P finished"
                (build:run-snapshot-id snapshot)
                (build:run-snapshot-state snapshot)
                completed))
       (when plan
         (list
          (format nil "Plan: ~D system~:P · ~D compile~:P · ~D load~:P"
                  (build:build-plan-summary-systems plan)
                  (build:build-plan-summary-compiles plan)
                  (build:build-plan-summary-loads plan))))
       (when current
         (list
          (format nil "Now ~(~A~): ~A"
                  (build:build-action-snapshot-kind current)
                  (build:build-action-snapshot-label current))))))))

(defun source-update-display-lines (frame snapshot)
  (let ((build-snapshot (source-update-frame-build-snapshot frame)))
    (case (source-update-snapshot-state snapshot)
      (:loading
       (append (source-update-snapshot-lines snapshot)
               (source-update-build-progress-lines build-snapshot)))
      (:failed
       (if (and build-snapshot
                (eq :failed (build:run-snapshot-state build-snapshot)))
           (append
            (subseq (source-update-snapshot-lines snapshot)
                    0 (min 1 (length (source-update-snapshot-lines snapshot))))
            (source-update-build-progress-lines build-snapshot)
            (source-update-build-diagnostic-lines build-snapshot)
            (alexandria:when-let
                ((diagnostic
                   (build:run-snapshot-terminal-diagnostic build-snapshot)))
              (alexandria:when-let
                  ((backtrace
                     (build:build-diagnostic-snapshot-backtrace diagnostic)))
                (cons "Backtrace"
                      (source-update-bounded-lines
                       (source-update-lines backtrace) 7)))))
           (source-update-snapshot-lines snapshot)))
      (otherwise (source-update-snapshot-lines snapshot)))))

(defmethod handle-repaint ((pane source-update-pane) region)
  (declare (ignore region))
  (let* ((frame (pane-frame pane))
         (snapshot (source-update-frame-snapshot frame))
         (state (source-update-snapshot-state snapshot))
         (error-p (eq state :failed)))
    (with-bounding-rectangle* (left top right bottom) pane
      (with-sheet-medium (medium pane)
        (unless (typep medium 'luv-gpu-medium)
          (error "The source update panel requires retained direct GPU media."))
        (draw-analytic-rounded-rectangle*
         medium (+ left 8) (+ top 10) right bottom :radius 18
         :ink *source-update-shadow-ink*)
        (draw-analytic-rounded-rectangle*
         medium left top right bottom :radius 18 :ink *source-update-edge-ink*)
        (draw-analytic-rounded-rectangle*
         medium (+ left 2) (+ top 2) (- right 2) (- bottom 2)
         :radius 16 :ink *source-update-panel-ink*)
        (draw-text* pane "Source Update" +source-update-margin+ 31
                    :align-y :center :text-size 22 :text-face :bold
                    :ink *source-update-text-ink*)
        (draw-text* pane (source-update-state-label state)
                    (- +source-update-width+ +source-update-margin+) 31
                    :align-x :right :align-y :center :text-size 14
                    :ink (if error-p
                             *source-update-error-ink*
                             *source-update-accent-ink*))
        (draw-text* pane (source-update-snapshot-heading snapshot)
                    +source-update-margin+ 67
                    :align-y :center :text-size 18 :text-face :bold
                    :ink (if error-p
                             *source-update-error-ink*
                             *source-update-text-ink*))
        (draw-analytic-rounded-rectangle*
         medium +source-update-margin+ 82
         (- +source-update-width+ +source-update-margin+) 532
         :radius 9 :ink *source-update-row-ink*)
        (loop for line in (source-update-display-lines frame snapshot)
              for index from 0 below 19
              for y = (+ +source-update-content-top+
                         (* index +source-update-line-height+))
              do (draw-text* pane line (+ +source-update-margin+ 13) y
                             :align-y :top :text-size 14
                             :ink (if (and error-p (zerop index))
                                      *source-update-error-ink*
                                      *source-update-text-ink*)))
        (let ((enabled-p (source-update-action-enabled-p snapshot)))
          (draw-analytic-rounded-rectangle*
           medium +source-update-margin+ +source-update-button-top+
           315 +source-update-button-bottom+ :radius 9
           :ink (if enabled-p
                    *source-update-button-ink*
                    *source-update-button-muted-ink*))
          (draw-text* pane (source-update-action-label snapshot)
                      169 (/ (+ +source-update-button-top+
                                +source-update-button-bottom+) 2)
                      :align-x :center :align-y :center :text-size 16
                      :text-face :bold :ink *source-update-text-ink*))
        (draw-text* pane (source-update-snapshot-footer snapshot)
                    (- +source-update-width+ +source-update-margin+)
                    (/ (+ +source-update-button-top+
                          +source-update-button-bottom+) 2)
                    :align-x :right :align-y :center :text-size 12
                    :ink *source-update-muted-ink*)))))

(defun source-update-mirror (frame &key (errorp t))
  (let* ((sheet (frame-top-level-sheet frame))
         (mirror (and sheet (sheet-direct-mirror sheet))))
    (cond ((typep mirror 'luv-gpu-mirror) mirror)
          (errorp (error "The source update panel has no GPU mirror."))
          (t nil))))

(defun repaint-source-update (frame)
  (alexandria:when-let ((mirror (source-update-mirror frame :errorp nil)))
    (unless (and (mirror-embedded-p mirror) (null (mirror-texture mirror)))
      (error "The source update panel requires an embedded direct mirror."))
    (repaint-gpu-mirror mirror)
    (setf (source-update-dirty-p frame) nil))
  frame)

(defun refresh-source-update (frame)
  "Adopt SESSION's newest immutable snapshot outside repaint."
  (let ((session (source-update-frame-session frame)))
    (multiple-value-bind (snapshot revision)
        (current-source-update-snapshot session)
      (unless (= revision (source-update-observed-revision frame))
        (setf (source-update-frame-snapshot frame) snapshot
              (source-update-observed-revision frame) revision
              (source-update-dirty-p frame) t)))
    (let* ((run (source-update-build-run session))
           (build-snapshot (and run (build:run-snapshot run)))
           (id (and build-snapshot (build:run-snapshot-id build-snapshot)))
           (revision (and build-snapshot
                          (build:run-snapshot-revision build-snapshot))))
      (unless (and (equal id (source-update-observed-build-id frame))
                   (eql revision
                        (source-update-observed-build-revision frame)))
        (setf (source-update-frame-build-snapshot frame) build-snapshot
              (source-update-observed-build-id frame) id
              (source-update-observed-build-revision frame)
              (or revision -1)
              (source-update-dirty-p frame) t))))
  frame)

(defun prepare-source-update (frame)
  (let ((mirror (source-update-mirror frame)))
    (if (or (source-update-dirty-p frame)
            (null (gpu-mirror-prepared-commands mirror)))
        (repaint-source-update frame)
        (prepare-gpu-mirror-compositor mirror)))
  frame)

(defun source-update-panel-scale (viewport-extent)
  (destructuring-bind (viewport-width viewport-height) viewport-extent
    (min 1.0
         (/ (max 1.0 (- viewport-width (* 2 +source-update-viewport-margin+)))
            +source-update-width+)
         (/ (max 1.0 (- viewport-height (* 2 +source-update-viewport-margin+)))
            +source-update-height+))))

(defun source-update-screen-state (frame viewport-logical-extent)
  (declare (ignore frame))
  (destructuring-bind (viewport-width viewport-height) viewport-logical-extent
    (let* ((scale (source-update-panel-scale viewport-logical-extent))
           (half-width (/ (* +source-update-width+ scale) viewport-width))
           (half-height (/ (* +source-update-height+ scale) viewport-height)))
      (make-array
       12 :element-type 'single-float
       :initial-contents
       (mapcar (lambda (value) (coerce value 'single-float))
               (list 0.0 0.0 0.0 1.0
                     half-width 0.0 0.0 0.0
                     0.0 half-height 0.0 0.0))))))

(defun source-update-local-coordinate
    (frame pointer-x pointer-y viewport-logical-extent)
  (declare (ignore frame))
  (destructuring-bind (viewport-width viewport-height) viewport-logical-extent
    (let* ((scale (source-update-panel-scale viewport-logical-extent))
           (display-width (* +source-update-width+ scale))
           (display-height (* +source-update-height+ scale))
           (left (* 0.5 (- viewport-width display-width)))
           (top (* 0.5 (- viewport-height display-height))))
      (when (and (<= left pointer-x (+ left display-width))
                 (<= top pointer-y (+ top display-height)))
        (values (/ (- pointer-x left) scale)
                (/ (- pointer-y top) scale))))))

(defun invoke-source-update-action (frame)
  (let* ((session (source-update-frame-session frame))
         (snapshot (source-update-frame-snapshot frame)))
    (case (source-update-snapshot-state snapshot)
      (:review (request-source-update-apply session))
      ((:fetching :applying :loading) nil)
      (:failed
       (if (and (source-update-snapshot-target snapshot)
                (source-update-build-run session))
           (request-source-update-retry session)
           (request-source-update-fetch session)))
      (t (request-source-update-fetch session))))
  frame)

(defun handle-source-update-key-event (frame event)
  "Handle source-update keys and return :CONTINUE or :DISMISS."
  (check-type event luv:canvas-key-press-event)
  (when (luv:canvas-key-event-repeat-p event)
    (return-from handle-source-update-key-event :continue))
  (case (luv:canvas-key-event-key-name event)
    (:escape
     (if (source-update-busy-p (source-update-frame-session frame))
         :continue
         :dismiss))
    (:a
     (when (member
            (source-update-snapshot-state
             (source-update-frame-snapshot frame))
            '(:review :failed))
       (invoke-source-update-action frame))
     :continue)
    (:r
     (request-source-update-fetch (source-update-frame-session frame))
     :continue)
    (t :continue)))

(defun handle-source-update-pointer-press (frame x y button)
  "Invoke the primary action when a left click lands on its button."
  (when (and (eq button :left)
             (<= +source-update-margin+ x 315)
             (<= +source-update-button-top+ y +source-update-button-bottom+))
    (invoke-source-update-action frame))
  :continue)

(defun make-embedded-source-update
    (owner canvas context device &key root systems title loader)
  "Create OWNER's direct-GPU source update panel without starting I/O.

The application attaches the returned frame to its host before calling
START-SOURCE-UPDATE, so a failed attachment never leaves a Git worker behind."
  (let* ((session
           (if loader
               (make-source-update-session
                (or root (source-update-root-for owner))
                (or systems (source-update-systems-for owner))
                :loader loader :start-p nil)
               (make-source-update-session
                (or root (source-update-root-for owner))
                (or systems (source-update-systems-for owner))
                :start-p nil)))
         (port (find-port :server-path '(:luv-gpu)))
         (manager (or (first (climi::frame-managers port))
                      (make-instance 'luv-frame-manager :port port)))
         (frame
           (let ((*embedded-mirror-target* canvas)
                 (*embedded-mirror-context* context)
                 (*embedded-mirror-device* device))
             (make-application-frame
              'source-update :frame-manager manager :enable t
              :session session))))
    (setf (frame-pretty-name frame)
          (or title (source-update-title-for owner)))
    (make-gpu-frame-background-transparent frame)
    (handler-case
        (progn
          (repaint-source-update frame)
          frame)
      (error (condition)
        (quiesce-source-update-session session)
        (unless (eq :disowned (frame-state frame))
          (destroy-frame frame))
        (error condition)))))

(defun destroy-source-update (frame)
  "Quiesce source work, then release FRAME and its retained resources."
  (check-type frame source-update)
  (quiesce-source-update-session (source-update-frame-session frame))
  (unless (eq :disowned (frame-state frame))
    (destroy-frame frame))
  nil)
