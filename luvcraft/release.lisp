;;; Releasing owned resources without choosing between finishing and telling.

(in-package #:luvcraft)

;;; Releasing a session, out loud.
;;;
;;; Teardown has two obligations that pull against each other.  It has to
;;; finish -- the window is closed last, so a step that signals halfway through
;;; leaves a dead game on the desktop with nothing left holding a handle to it
;;; -- and it has to be honest, because a release that quietly failed is a leak
;;; nobody will ever be told about.  Wrapping the whole thing in IGNORE-ERRORS
;;; satisfies the first and betrays the second; letting errors fly satisfies
;;; the second and betrays the first.
;;;
;;; So each step is contained and named, every failure is collected, and once
;;; everything has actually been released the collected failures are signalled
;;; together.  The window always closes, and every failure is still seen.

(defvar *release-failures* nil
  "Failures collected by RELEASING within the current WITH-RELEASE-REPORT.")

(define-condition luvcraft-release-error (error)
  ((failures :initarg :failures :reader luvcraft-release-error-failures))
  (:documentation "Steps that failed while a session was being released.")
  (:report
   (lambda (condition stream)
     (let ((failures (luvcraft-release-error-failures condition)))
       (format stream "~D of the session's release steps failed.~
                       ~:{~2%~A:~%  ~A~}"
               (length failures)
               (mapcar (lambda (failure)
                         (list (car failure) (cdr failure)))
                       failures))))))

(defun call-releasing (name function)
  "Call FUNCTION, recording rather than propagating any error it signals.

NAME identifies the step in the report.  Returns FUNCTION's value, or NIL
when it failed."
  (handler-case (funcall function)
    (error (condition)
      (push (cons name condition) *release-failures*)
      nil)))

(defmacro releasing (name &body body)
  "Run BODY as one named, contained release step."
  `(call-releasing ,name (lambda () ,@body)))

(defmacro with-release-report (&body body)
  "Run BODY, then signal LUVCRAFT-RELEASE-ERROR if any step within it failed."
  `(let ((*release-failures* nil))
     (multiple-value-prog1 (progn ,@body)
       (when *release-failures*
         (error 'luvcraft-release-error
                :failures (reverse *release-failures*))))))

(defmacro with-release-warnings (&body body)
  "Run BODY, then warn about any step that failed.

This is the variant for cleanup that runs while another error is already on
its way out: replacing that error with this one would hide the failure the
caller actually needs to see, so the release trouble is reported as a warning
alongside it instead."
  `(let ((*release-failures* nil))
     (multiple-value-prog1 (progn ,@body)
       (dolist (failure (reverse *release-failures*))
         (warn "Releasing ~A failed while unwinding: ~A"
               (car failure) (cdr failure))))))
