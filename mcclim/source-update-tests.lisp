(in-package #:mcluv.tests)

(defun source-update-test-directory ()
  (let ((directory
          (merge-pathnames
           (format nil "luv-source-update-~D-~D/"
                   (get-universal-time) (random 1000000))
           (uiop:temporary-directory))))
    (ensure-directories-exist directory)
    directory))

(defun source-update-test-git (directory &rest arguments)
  (uiop:run-program
   (cons "git" arguments)
   :directory directory :input nil :output :string :error-output :string))

(defun source-update-test-write (directory contents)
  (with-open-file (stream (merge-pathnames "story.txt" directory)
                          :direction :output :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string contents stream)))

(defun make-source-update-test-repositories ()
  "Return ROOT, SEED, and WORK with one incoming commit for WORK."
  (let* ((root (source-update-test-directory))
         (origin (merge-pathnames "origin.git/" root))
         (seed (merge-pathnames "seed/" root))
         (work (merge-pathnames "work/" root)))
    (ensure-directories-exist seed)
    (source-update-test-git root "init" "--bare" (namestring origin))
    (source-update-test-git seed "init")
    (source-update-test-git seed "config" "user.name" "Luv Test")
    (source-update-test-git seed "config" "user.email" "luv@example.invalid")
    (source-update-test-write seed "first\n")
    (source-update-test-git seed "add" "story.txt")
    (source-update-test-git seed "commit" "-m" "First story")
    (source-update-test-git seed "branch" "-M" "main")
    (source-update-test-git seed "remote" "add" "origin" (namestring origin))
    (source-update-test-git seed "push" "-u" "origin" "main")
    (source-update-test-git
     root "clone" "--branch" "main" (namestring origin) (namestring work))
    (source-update-test-write seed "first\nsecond\n")
    (source-update-test-git seed "add" "story.txt")
    (source-update-test-git seed "commit" "-m" "Second story")
    (source-update-test-git seed "push" "origin" "main")
    (values root seed work)))

(define-test source-update-reviews-and-applies-the-pinned-fast-forward
  (multiple-value-bind (root seed work)
      (make-source-update-test-repositories)
    (declare (ignore seed))
    (unwind-protect
         (let ((loads nil)
               (session nil))
           (setf session
                 (mcluv:make-source-update-session
                  work '("luft/render")
                  :loader
                  (lambda (systems)
                    (push systems loads)
                    nil)))
           (let ((review (mcluv:wait-source-update-session session)))
             (true (eq :review
                       (mcluv:source-update-snapshot-state review)))
             (true (find "Second story"
                         (mcluv:source-update-snapshot-lines review)
                         :test #'search)))
           (mcluv:request-source-update-apply session)
           (let ((complete (mcluv:wait-source-update-session session)))
             (true (eq :complete
                       (mcluv:source-update-snapshot-state complete)))
             (true (equal '(("luft/render")) loads))
             (true (string=
                    (mcluv::source-update-snapshot-target complete)
                    (string-trim '(#\Newline #\Return)
                                 (source-update-test-git
                                  work "rev-parse" "HEAD"))))))
      (uiop:delete-directory-tree
       root :validate t :if-does-not-exist :ignore))))

(define-test source-update-refuses-a-dirty-worktree-before-fetch
  (multiple-value-bind (root seed work)
      (make-source-update-test-repositories)
    (declare (ignore seed))
    (unwind-protect
         (progn
           (with-open-file (stream (merge-pathnames "local-note.txt" work)
                                   :direction :output
                                   :if-does-not-exist :create)
             (write-line "not committed" stream))
           (let* ((session
                    (mcluv:make-source-update-session
                     work '("luft/render") :loader (lambda (systems)
                                                     (declare (ignore systems)))))
                  (snapshot (mcluv:wait-source-update-session session)))
             (true (eq :failed
                       (mcluv:source-update-snapshot-state snapshot)))
             (true (search
                    "clean worktree"
                    (first (mcluv:source-update-snapshot-lines snapshot))))))
      (uiop:delete-directory-tree
       root :validate t :if-does-not-exist :ignore))))
