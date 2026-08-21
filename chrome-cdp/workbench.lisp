(in-package #:chrome-cdp)

(defvar *chrome* nil
  "The browser connection retained by ENSURE-CHROME in a durable Lisp image.")

(defun ensure-chrome (&rest connect-arguments)
  "Return the durable workbench browser, connecting it when necessary."
  (when (and *chrome* (not (browser-open-p *chrome*)))
    (setf *chrome* nil))
  (or *chrome*
      (setf *chrome* (apply #'connect-chrome connect-arguments))))

(defun close-chrome ()
  "Close and forget the durable workbench browser."
  (when *chrome*
    (close-browser *chrome*)
    (setf *chrome* nil))
  nil)

(defun chrome-pages ()
  "The page targets in the durable workbench browser."
  (remove-if-not (lambda (target) (string= "page" (target-type target)))
                 (browser-targets (ensure-chrome))))

(defun chrome-page (&key title url predicate)
  "Find a page in the durable workbench browser."
  (find-page (ensure-chrome) :title title :url url :predicate predicate))

(defun chrome-session (&key title url predicate)
  "Find and attach to a page in the durable workbench browser."
  (attach-target (chrome-page :title title :url url :predicate predicate)))
