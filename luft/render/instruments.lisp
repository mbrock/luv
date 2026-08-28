(in-package #:luft.render)

;;; A viewer instrument is a sparse application attachment: a metabar, M-x,
;;; inspector-like tool, or other developer surface which participates in the
;;; existing game presentation without owning another canvas or frame loop.
;;; Attachments are ordered once at the boundary; their dense GPU state remains
;;; inside their renderer or McCLIM compositor.

(defvar *viewer-instruments*
  (make-hash-table :test #'eq :weakness :key)
  "Weak viewer-to-instrument-list ownership registry.")

(defvar *viewer-instruments-lock*
  (sb-thread:make-mutex :name "LUFT viewer instruments"))

(defgeneric viewer-instrument-canvas (viewer)
  (:documentation
   "Return VIEWER's live mutation owner, or NIL before native startup.")
  (:method (viewer)
    (declare (ignore viewer))
    nil)
  (:method ((viewer viewer))
    (viewer-canvas viewer)))

(defgeneric viewer-instrument-stop-controller (viewer)
  (:documentation
   "Return VIEWER's terminal attachment gate, or NIL for a bare test owner.")
  (:method (viewer)
    (declare (ignore viewer))
    nil)
  (:method ((viewer viewer))
    (viewer-stop-controller viewer)))

(defgeneric viewer-instrument-priority (instrument)
  (:documentation
   "Return INSTRUMENT's input and visual stacking priority.")
  (:method (instrument)
    (declare (ignore instrument))
    0))

(defgeneric viewer-instrument-present-p (instrument viewer)
  (:documentation "Whether INSTRUMENT currently participates in VIEWER.")
  (:method (instrument viewer)
    (declare (ignore instrument viewer))
    t))

(defgeneric refresh-viewer-instrument (instrument viewer)
  (:documentation
   "Publish INSTRUMENT's already-owned state at VIEWER's frame boundary.")
  (:method (instrument viewer)
    (declare (ignore viewer))
    instrument))

(defgeneric encode-viewer-instrument
    (instrument viewer pass surface-texture physical-extent)
  (:documentation
   "Encode INSTRUMENT into VIEWER's final PASS at PHYSICAL-EXTENT.")
  (:method (instrument viewer pass surface-texture physical-extent)
    (declare (ignore viewer pass surface-texture physical-extent))
    instrument))

(defgeneric handle-viewer-instrument-event
    (instrument viewer canvas event)
  (:documentation
   "Handle EVENT for INSTRUMENT and return true when world input must stop.")
  (:method (instrument viewer canvas event)
    (declare (ignore instrument viewer canvas event))
    nil))

(defgeneric release-viewer-instrument (instrument viewer)
  (:documentation "Release INSTRUMENT while VIEWER's GPU device is live.")
  (:method (instrument viewer)
    (declare (ignore instrument viewer))
    nil))

(defgeneric quiesce-viewer-instrument (instrument viewer)
  (:documentation
   "Stop off-canvas work before VIEWER crosses its final frame boundary.")
  (:method (instrument viewer)
    (declare (ignore instrument viewer))
    nil))

(defun viewer-instruments (viewer)
  "Return a priority-descending snapshot of VIEWER's instruments."
  (sb-thread:with-mutex (*viewer-instruments-lock*)
    (copy-list (gethash viewer *viewer-instruments*))))

(defun sort-viewer-instruments (instruments)
  (stable-sort instruments #'> :key #'viewer-instrument-priority))

(defun call-with-viewer-instrument-mutation (viewer function)
  "Run FUNCTION at VIEWER's native frame boundary when one exists.

Before the canvas is open, and after it has quiesced, the caller still owns
the attachment list directly.  An open canvas owns both mutation and release:
REQUEST-CANVAS-FRAME serializes this call after any frame which borrowed an
instrument snapshot and propagates FUNCTION's values or condition back to the
caller."
  (check-type function function)
  (let ((canvas (viewer-instrument-canvas viewer)))
    (if (and canvas (eq :open (canvas-state canvas)))
        (request-canvas-frame
         canvas
         (lambda (timestamp)
           (declare (ignore timestamp))
           (funcall function)))
        (funcall function))))

(defun %add-viewer-instrument (viewer instrument)
  (let ((installed-p nil)
        (controller (viewer-instrument-stop-controller viewer)))
    (unwind-protect-releasing
        (progn
          (flet ((present-p ()
                   (sb-thread:with-mutex (*viewer-instruments-lock*)
                     (member instrument
                             (gethash viewer *viewer-instruments*)
                             :test #'eq)))
                 (publish ()
                   (sb-thread:with-mutex (*viewer-instruments-lock*)
                     (let ((instruments
                             (gethash viewer *viewer-instruments*)))
                       ;; Recheck under the registry lock so bare owners and
                       ;; same-object concurrent adds remain duplicate-free.
                       (unless (member instrument instruments :test #'eq)
                         (setf (gethash viewer *viewer-instruments*)
                               (sort-viewer-instruments
                                (cons instrument (copy-list instruments)))))))
                   instrument))
            (if controller
                (call-with-running-stop-controller
                 controller #'publish :attachment instrument
                 :already-attached-p #'present-p)
                (publish)))
          (setf installed-p t)
          instrument)
      ;; The registry consumes a newly offered instrument on success or
      ;; rejection.  Release on this same owner boundary so an opening
      ;; constructor can unwind without stranding partial resources.
      (unless installed-p
        (releasing :rejected-instrument
          (release-viewer-instrument instrument viewer))))))

(defun add-viewer-instrument (viewer instrument)
  "Attach INSTRUMENT at VIEWER's frame boundary in explicit priority order.

ADD consumes a newly offered INSTRUMENT on both success and terminal
rejection.  Once VIEWER begins stopping, the rejected instrument is released
exactly once before APPLICATION-ATTACHMENT-CLOSED is signalled."
  ;; A duplicate is already application-owned.  Resolve it before asking a
  ;; closing native canvas to run a callback, because pre-callback rejection
  ;; must never release an object which remains in the viewer registry.
  (when (member instrument (viewer-instruments viewer) :test #'eq)
    (return-from add-viewer-instrument instrument))
  (let ((consumed-p nil))
    (unwind-protect-releasing
        (progn
          (handler-case
              (call-with-viewer-instrument-mutation
               viewer
               (lambda ()
                 (setf consumed-p t)
                 (%add-viewer-instrument viewer instrument)))
            (application-attachment-closed (condition)
              (error condition))
            (error (condition)
              ;; Preserve the application's semantic terminal rejection when
              ;; the native canvas refuses the request before our callback.
              (let* ((controller (viewer-instrument-stop-controller viewer))
                     (state (and controller
                                 (stop-controller-state controller))))
                (cond
                  ((and (not consumed-p)
                        (member instrument (viewer-instruments viewer)
                                :test #'eq))
                   (setf consumed-p t)
                   instrument)
                  ((and (not consumed-p)
                        state
                        (not (eq :running state)))
                   (error 'application-attachment-closed
                          :controller controller
                          :attachment instrument
                          :state state))
                  (t (error condition))))))
          instrument)
      ;; A closing canvas may reject the owner request before the callback
      ;; starts.  In that case the registry never consumed INSTRUMENT, so no
      ;; frame can have borrowed it.  GPU DESTROY remains safe on this caller:
      ;; backend retirement locks serialize it with terminal device teardown.
      (unless consumed-p
        (releasing :unpublished-instrument
          (release-viewer-instrument instrument viewer))))))

(defun %remove-viewer-instrument (viewer instrument)
  (let ((removed-p nil))
    (sb-thread:with-mutex (*viewer-instruments-lock*)
      (let* ((instruments (gethash viewer *viewer-instruments*))
             (remaining (remove instrument instruments :test #'eq)))
        (unless (= (length instruments) (length remaining))
          (setf removed-p t)
          (if remaining
              (setf (gethash viewer *viewer-instruments*) remaining)
              (remhash viewer *viewer-instruments*)))))
    (when removed-p
      (release-viewer-instrument instrument viewer))
    removed-p))

(defun remove-viewer-instrument (viewer instrument)
  "Detach and release INSTRUMENT at a frame boundary.

Return true when it was attached.  Release is synchronous with the caller and
therefore cannot overlap a native frame which still borrows the instrument."
  (call-with-viewer-instrument-mutation
   viewer (lambda () (%remove-viewer-instrument viewer instrument))))

(defun viewer-instruments-present-p (viewer)
  (some (lambda (instrument)
          (viewer-instrument-present-p instrument viewer))
        (viewer-instruments viewer)))

(defun refresh-viewer-instruments (viewer)
  "Refresh each present instrument outside the render-pass encoder."
  (dolist (instrument (viewer-instruments viewer))
    (when (viewer-instrument-present-p instrument viewer)
      (refresh-viewer-instrument instrument viewer)))
  viewer)

(defun encode-viewer-instruments
    (viewer pass surface-texture physical-extent)
  "Encode lower-priority instruments first and modal surfaces last."
  (dolist (instrument (reverse (viewer-instruments viewer)))
    (when (viewer-instrument-present-p instrument viewer)
      (encode-viewer-instrument
       instrument viewer pass surface-texture physical-extent)))
  viewer)

(defun dispatch-viewer-instrument-event (viewer canvas event)
  "Offer EVENT from highest to lowest priority until one consumes it."
  (loop for instrument in (viewer-instruments viewer)
        when (and (viewer-instrument-present-p instrument viewer)
                  (handle-viewer-instrument-event
                   instrument viewer canvas event))
          do (return t)
        finally (return nil)))

(defun %release-viewer-instruments (viewer)
  (let ((instruments nil)
        (errors nil))
    (sb-thread:with-mutex (*viewer-instruments-lock*)
      (setf instruments (gethash viewer *viewer-instruments*))
      (remhash viewer *viewer-instruments*))
    (dolist (instrument instruments)
      (handler-case
          (release-viewer-instrument instrument viewer)
        (error (condition)
          (push (cons instrument condition) errors))))
    (when errors
      (error "LUFT instrument release failed for ~{~S~^, ~}: ~A"
             (mapcar #'car (reverse errors))
             (cdar errors))))
  nil)

(defun quiesce-viewer-instruments (viewer)
  "Stop asynchronous work owned by VIEWER's current instruments."
  (dolist (instrument (viewer-instruments viewer))
    (quiesce-viewer-instrument instrument viewer))
  viewer)

(defun release-viewer-instruments (viewer)
  "Atomically detach then release every instrument at a frame boundary."
  (call-with-viewer-instrument-mutation
   viewer (lambda () (%release-viewer-instruments viewer))))

;; This is deliberately the only application-level event wrapper.  Individual
;; instruments add methods on HANDLE-VIEWER-INSTRUMENT-EVENT, so independently
;; loaded adapters never replace one another at an identical method coordinate.
(defmethod handle-canvas-event :around
    ((viewer viewer) canvas (event canvas-event))
  (if (dispatch-viewer-instrument-event viewer canvas event)
      nil
      (call-next-method)))
