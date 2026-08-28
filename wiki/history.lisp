;;;; Git history as another disposable view of the workshop.

(in-package #:luv.wiki)

(defclass commit-change ()
  ((path :initarg :path :reader commit-change-path)
   (additions :initarg :additions :reader commit-change-additions)
   (deletions :initarg :deletions :reader commit-change-deletions)))

(defclass repository-commit ()
  ((id :initarg :id :reader repository-commit-id)
   (short-id :initarg :short-id :reader repository-commit-short-id)
   (date :initarg :date :reader repository-commit-date)
   (author :initarg :author :reader repository-commit-author)
   (subject :initarg :subject :reader repository-commit-subject)
   (changes :initarg :changes :reader repository-commit-changes)))

(defun split-character (character string)
  (uiop:split-string string :separator (list character)))

(defun parse-count (text)
  (unless (string= text "-")
    (parse-integer text :junk-allowed nil)))

(defun parse-commit-change (line)
  (let ((fields (split-character #\Tab line)))
    (when (= (length fields) 3)
      (make-instance 'commit-change
                     :additions (parse-count (first fields))
                     :deletions (parse-count (second fields))
                     :path (third fields)))))

(defun parse-git-history (text)
  "Read the machine-delimited output requested by READ-GIT-HISTORY."
  (loop for record in (split-character (code-char 30) text)
        for lines = (uiop:split-string record :separator '(#\Newline))
        for header = (first lines)
        for fields = (and header (split-character (code-char 31) header))
        when (= (length fields) 5)
          collect (make-instance
                   'repository-commit
                   :id (first fields)
                   :short-id (second fields)
                   :date (third fields)
                   :author (fourth fields)
                   :subject (fifth fields)
                   :changes (remove nil (mapcar #'parse-commit-change (rest lines))))))

(defun read-git-history (directory &key (limit 50))
  "The recent commits available in DIRECTORY's checkout, or NIL when it is
not a Git work tree.  A shallow checkout is useful evidence too: callers say
what range was actually available rather than pretending it is complete."
  (when directory
    (handler-case
        (parse-git-history
         (uiop:run-program
          (list "git" "-C" (namestring directory) "log"
                (format nil "-~D" limit) "--date=iso-strict"
                "--format=%x1e%H%x1f%h%x1f%aI%x1f%an%x1f%s" "--numstat"
                "--no-renames")
          :output :string :error-output :string :ignore-error-status nil))
      (error () nil))))

(defun commit-page-name (commit)
  (format nil "commits/~A.html" (repository-commit-id commit)))

(defun commit-github-url (commit)
  (format nil "https://github.com/mbrock/luv/commit/~A" (repository-commit-id commit)))

(defun commit-patch (commit directory &key (limit 600000))
  "COMMIT's patch when Git can provide it and it is small enough for a page."
  (when directory
    (handler-case
        (let ((patch
                (uiop:run-program
                 (list "git" "-C" (namestring directory) "show" "--format="
                       "--find-renames" (repository-commit-id commit))
                 :output :string :error-output :string :ignore-error-status nil)))
          (and (<= (length patch) limit) patch))
      (error () nil))))
