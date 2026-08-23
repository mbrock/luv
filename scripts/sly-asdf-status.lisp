;;;; Read-only reports about the ASDF world inside a managed Sly image.

(defpackage #:luv.sly.asdf
  (:use #:cl)
  (:export
   #:print-system
   #:print-systems
   #:print-stale-systems))

(in-package #:luv.sly.asdf)

(defstruct system-status
  system
  name
  source-file
  version
  loaded-p
  actions
  planning-error)

(defun load-actions (system)
  "Return what ASDF would perform for LOAD-OP on SYSTEM, without performing it."
  (handler-case
      (values
       (asdf/plan:plan-actions
        (asdf/plan:make-plan
         'asdf/plan:sequential-plan 'asdf:load-op system))
       nil)
    (error (condition)
      (values nil condition))))

(defun make-status (system loaded-systems &key plan-unloaded)
  (let ((loaded-p (not (null (member (asdf:component-name system)
                                     loaded-systems :test #'string-equal)))))
    (multiple-value-bind (actions planning-error)
        (if (or loaded-p plan-unloaded)
            (load-actions system)
            (values nil nil))
      (make-system-status
       :system system
       :name (asdf:component-name system)
       :source-file (asdf:system-source-file system)
       :version (asdf:component-version system)
       :loaded-p loaded-p
       :actions actions
       :planning-error planning-error))))

(defun status-kind (status)
  (cond
    ((system-status-planning-error status) :error)
    ((not (system-status-loaded-p status)) :unloaded)
    ((system-status-actions status) :dirty)
    (t :current)))

(defun pathname-under-p (pathname root)
  (and pathname
       (ignore-errors
         (uiop:subpathp (truename pathname) (truename root)))))

(defun registered-system-objects ()
  (loop for name in (asdf/system-registry:registered-systems)
        for system = (asdf:find-system name nil)
        when system collect system))

(defun system-statuses (&key root all loaded-only)
  (let ((loaded (asdf/operate:already-loaded-systems)))
    (sort
     (loop for system in (registered-system-objects)
           for source = (asdf:system-source-file system)
           for loaded-p = (member (asdf:component-name system)
                                  loaded :test #'string-equal)
           when (and (or all (pathname-under-p source root))
                     (or (not loaded-only) loaded-p))
             collect (make-status system loaded))
     #'string< :key #'system-status-name)))

(defun display-pathname (pathname root)
  (cond
    ((null pathname) "-")
    ((pathname-under-p pathname root)
     (enough-namestring pathname root))
    (t (namestring pathname))))

(defun print-systems (&key root all)
  (let* ((root (uiop:ensure-directory-pathname (or root *default-pathname-defaults*)))
         (statuses (system-statuses :root root :all all))
         (loaded-count (length (asdf/operate:already-loaded-systems)))
         (registered-count (length (asdf/system-registry:registered-systems))))
    (format t "ASDF ~A — ~D registered, ~D loaded in this image.~%"
            (asdf:asdf-version) registered-count loaded-count)
    (format t "~:[Systems rooted at ~A~;All registered systems~]~%~%"
            all (namestring root))
    (if (null statuses)
        (format t "No registered systems have definitions under this root.~%")
        (progn
          (format t "~9A  ~5A  ~6A  ~28A  ~A~%"
                  "STATE" "PLAN" "LOADED" "SYSTEM" "DEFINITION")
          (dolist (status statuses)
            (format t "~9A  ~5A  ~6A  ~28A  ~A~%"
                    (string-downcase (symbol-name (status-kind status)))
                    (if (system-status-loaded-p status)
                        (length (system-status-actions status))
                        "-")
                    (if (system-status-loaded-p status) "yes" "no")
                    (system-status-name status)
                    (display-pathname (system-status-source-file status) root)))
          (terpri)
          (format t "CURRENT means a fresh ASDF LOAD-OP plan is empty.~%")
          (format t "DIRTY means ASDF would perform the reported number of actions.~%")
          (format t "UNLOADED means the system has not successfully loaded in this image; no plan was computed.~%")))
    (values)))

(defun universal-time-string (universal-time)
  (if (and universal-time (integerp universal-time) (plusp universal-time))
      (multiple-value-bind (second minute hour day month year)
          (decode-universal-time universal-time)
        (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
                year month day hour minute second))
      "-"))

(defun operation-name (operation)
  (string-downcase (symbol-name (class-name (class-of operation)))))

(defun component-display-name (component)
  (format nil "~{~A~^/~}" (asdf:component-find-path component)))

(defun print-action (action)
  (format t "  ~12A  ~A~%"
          (operation-name (car action))
          (component-display-name (cdr action))))

(defun direct-dependencies (system)
  (remove-duplicates
   (append (copy-list (asdf:system-defsystem-depends-on system))
           (copy-list (asdf:system-depends-on system))
           (copy-list (asdf:system-weakly-depends-on system)))
   :test #'equal))

(defun print-operation-times (system)
  (let ((entries nil))
    (maphash (lambda (operation time)
               (push (cons operation time) entries))
             (asdf/component:component-operation-times system))
    (if entries
        (dolist (entry (sort entries #'< :key #'cdr))
          (format t "  ~12A  ~A~%"
                  (operation-name (car entry))
                  (universal-time-string (cdr entry))))
        (format t "  none recorded~%"))))

(defun print-system (name &key root (action-limit 24))
  (let ((system (asdf:find-system name nil)))
    (unless system
      (error "ASDF has no registered system named ~A" name))
    (let* ((root (uiop:ensure-directory-pathname
                  (or root *default-pathname-defaults*)))
           (status (make-status system (asdf/operate:already-loaded-systems)
                                :plan-unloaded t))
           (actions (system-status-actions status))
           (dependencies (direct-dependencies system)))
      (format t "System ~A~@[ ~A~]~%" name (system-status-version status))
      (format t "State:      ~A~%"
              (string-downcase (symbol-name (status-kind status))))
      (format t "Loaded:     ~:[no~;yes~]~%" (system-status-loaded-p status))
      (format t "Definition: ~A~%"
              (display-pathname (system-status-source-file status) root))
      (format t "Directory:  ~A~%" (asdf:component-pathname system))
      (when (asdf:system-description system)
        (format t "Description: ~A~%" (asdf:system-description system)))
      (format t "Dependencies (~D):~%" (length dependencies))
      (if dependencies
          (dolist (dependency dependencies)
            (format t "  ~S~%" dependency))
          (format t "  none~%"))
      (format t "~%ASDF operation timestamps:~%")
      (print-operation-times system)
      (cond
        ((system-status-planning-error status)
         (format t "~%ASDF could not construct a LOAD-OP plan:~%  ~A~%"
                 (system-status-planning-error status)))
        ((null actions)
         (format t "~%LOAD-OP plan: empty; ASDF considers this system current.~%"))
        (t
         (format t "~%LOAD-OP plan: ~D action~:P pending.~%" (length actions))
         (dolist (action (subseq actions 0 (min action-limit (length actions))))
           (print-action action))
         (when (> (length actions) action-limit)
           (format t "  … ~D more action~:P~%"
                   (- (length actions) action-limit)))))
      (values))))

(defun print-stale-systems (&key root)
  (let* ((root (uiop:ensure-directory-pathname (or root *default-pathname-defaults*)))
         (dirty
           (remove-if-not
            (lambda (status) (eq (status-kind status) :dirty))
            (system-statuses :root root :loaded-only t))))
    (if (null dirty)
        (format t "No loaded project system has pending ASDF LOAD-OP actions.~%")
        (progn
          (format t "Loaded project systems with nonempty ASDF LOAD-OP plans:~%~%")
          (dolist (status dirty)
            (format t "~28A  ~D action~:P~%"
                    (system-status-name status)
                    (length (system-status-actions status))))))
    (values)))
