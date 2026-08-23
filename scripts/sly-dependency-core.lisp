;;;; Build the stable dependency layer used by the managed Sly image.

(require :asdf)
(require :sb-concurrency)
(require :sb-posix)

(defparameter *project-root* (uiop:ensure-directory-pathname (truename "./")))
(defparameter *root-systems* '(:luv-workbench))

(defun dependency-name (specification)
  (cond
    ((or (stringp specification) (symbolp specification)) specification)
    ((and (consp specification) (eq (first specification) :feature))
     (and (uiop:featurep (second specification))
          (dependency-name (third specification))))
    ((and (consp specification) (eq (first specification) :version))
     (dependency-name (second specification)))
    ;; ASDF handles :REQUIRE dependencies without a system definition.
    (t nil)))

(defun local-system-p (system)
  (let ((source (asdf:system-source-file system)))
    (and source (uiop:subpathp source *project-root*))))

(defun external-system-boundary ()
  (let ((seen (make-hash-table :test #'equal))
        (external nil))
    (labels ((visit (name)
               (let ((key (string-downcase (string name))))
                 (unless (gethash key seen)
                   (setf (gethash key seen) t)
                   (let ((system (asdf:find-system name)))
                     (if (local-system-p system)
                         (dolist (specification (asdf:system-depends-on system))
                           (let ((dependency (dependency-name specification)))
                             (when dependency
                               (visit dependency))))
                         (push (asdf:component-name system) external)))))))
      (dolist (system *root-systems*)
        (visit system)))
    (sort external #'string-lessp)))

(defun clear-local-systems ()
  (let ((local
          (loop for name in (asdf:registered-systems)
                for system = (asdf:find-system name nil)
                when (and system (local-system-p system))
                  collect name)))
    (format t "Clearing ~D checkout system definitions before the dump.~%"
            (length local))
    (force-output)
    (dolist (name local)
      (asdf:clear-system name))))

(let* ((slynk-root
         (uiop:ensure-directory-pathname
          (or (uiop:getenv "LUV_SLYNK_DIR")
              (error "LUV_SLYNK_DIR is not set"))))
       (output
         (or (uiop:getenv "LUV_SLY_DEPENDENCY_CORE_OUTPUT")
             (error "LUV_SLY_DEPENDENCY_CORE_OUTPUT is not set"))))
  (asdf:initialize-source-registry
   `(:source-registry
     (:tree ,(namestring slynk-root))
     (:directory ,(namestring *project-root*))
     :inherit-configuration))
  (asdf:load-asd (merge-pathnames #P"slynk.asd" slynk-root))
  (format t "Loading Slynk into the dependency core.~%")
  (force-output)
  (asdf:load-system :slynk)
  (let ((external (external-system-boundary)))
    (format t "Loading ~D external ASDF boundary systems.~%" (length external))
    (force-output)
    (dolist (system external)
      (format t "  ~A~%" system)
      (force-output)
      (asdf:load-system system)))
  (clear-local-systems)
  (format t "Saving dependency core to ~A~%" output)
  (force-output)
  (sb-ext:save-lisp-and-die output :purify t))
