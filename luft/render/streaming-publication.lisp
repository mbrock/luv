(in-package #:luft.render)

;;; The owner/worker handoff for one regional replacement.
;;;
;;; Schedule -> accept the complete current result -> publish atomically.
;;; A removal-only replacement skips the worker. GPU staging failure leaves
;;; the ready replacement intact for retry. Resetting the scene detaches it;
;;; any later result from that old request is then harmlessly ignored.

(defconstant +streaming-cohort-production-key+ :luft-streaming-cohort)

(defvar *streaming-mesh-snapshot-observer* nil
  "Optional test instrumentation called with each snapshot before scheduling.")

(zdefun (schedule-streaming-scene-cohort :zone :luft/schedule-mesh-region
                                         :value (length output-keys))
    (scene production-system output-keys bevel-width priority
     &key (realize-torch-light-p t) removals)
  "Schedule one region and retain its whole replacement after submission succeeds."
  (when (streaming-scene-replacement scene)
    (error "Cannot schedule another region while a streaming replacement is active."))
  (unless output-keys
    (error "A mesh request needs output owners; use MAKE-STREAMING-REMOVAL for departures."))
  (let* ((snapshot
           (make-streaming-region-snapshot
            scene output-keys bevel-width
            :realize-torch-light-p realize-torch-light-p))
         (request
           (make-instance 'streaming-mesh-request
                          :key +streaming-cohort-production-key+
                          :priority priority :snapshot snapshot))
         (replacement
           (make-streaming-replacement
            (streaming-mesh-snapshot-output-keys snapshot) removals request)))
    (when *streaming-mesh-snapshot-observer*
      (funcall *streaming-mesh-snapshot-observer* snapshot))
    (production:schedule-production-request production-system request)
    (setf (streaming-scene-replacement scene) replacement)
    request))

(defun accept-streaming-mesh-result (scene request result)
  "Accept one complete result only for the pending request and current inputs."
  (check-type result streaming-mesh-result)
  (when (and (pending-streaming-mesh-request-p scene request)
             (current-streaming-mesh-request-p scene request))
    (let* ((replacement (streaming-scene-replacement scene))
           (keys (streaming-replacement-output-keys replacement))
           (meshes (streaming-mesh-result-meshes result))
           (generation (streaming-mesh-result-generation result)))
      (unless (equal keys (mapcar #'car meshes))
        (error "Streaming cohort returned owners ~S, expected ~S."
               (mapcar #'car meshes) keys))
      (unless (equalp (streaming-mesh-snapshot-stamp
                       (streaming-mesh-request-snapshot request))
                      (scene-mesh-generation-request-stamp generation))
        (error "Streaming light result stamp does not match its mesh request."))
      (setf (streaming-replacement-result replacement) result)
      t)))

(defun ready-streaming-scene-meshes (scene)
  "Return the complete current meshes, generation, and readiness flag."
  (let* ((replacement (streaming-scene-replacement scene))
         (result (and replacement (streaming-replacement-result replacement))))
    (if (and result
             (or (null (streaming-replacement-request replacement))
                 (current-streaming-mesh-request-p
                  scene (streaming-replacement-request replacement))))
        (values (streaming-mesh-result-meshes result)
                (streaming-mesh-result-generation result) t)
        (values nil nil nil))))

(zdefun (publish-ready-streaming-scene :zone :luft/publish-ready-region)
    (scene renderer)
  "Install a whole replacement, then make its light and CPU cache reusable."
  (multiple-value-bind (meshes generation ready-p)
      (ready-streaming-scene-meshes scene)
    (when ready-p
      (let ((replacement (streaming-scene-replacement scene)))
        (renderer-update-meshes
         renderer meshes (streaming-replacement-removals replacement)
         :scene-generation generation)
        ;; Only a successful GPU publication advances these owner-thread values.
        (setf (streaming-scene-light-generation scene)
              (scene-mesh-generation-light-generation generation))
        (when (streaming-replacement-request replacement)
          (setf (streaming-scene-mesh-cache scene)
                (streaming-mesh-result-mesh-cache
                 (streaming-replacement-result replacement))))
        (setf (streaming-scene-replacement scene) nil)
        (length meshes)))))

(zdefun (drain-streaming-scene-production :zone :luft/drain-production)
    (scene renderer production-system &key (limit 2))
  "Drain bounded worker results, then try the pending renderer publication."
  (loop repeat limit
        do (multiple-value-bind (result present-p)
               (production:receive-production-result-no-hang production-system)
             (unless present-p (return))
             (etypecase (production:production-result-request result)
               (authored-chunk-load-request
                (receive-streaming-source-result scene result))
               (streaming-mesh-request
                (receive-streaming-mesh-result scene result)))))
  (publish-ready-streaming-scene scene renderer))

;;; Request identity and freshness are separate questions. Identity guards the
;;; pending transaction; the snapshot stamp guards its borrowed world inputs.

(defun pending-streaming-mesh-request-p (scene request)
  (let ((replacement (streaming-scene-replacement scene)))
    (and replacement
         (null (streaming-replacement-result replacement))
         (null (streaming-replacement-failure replacement))
         (eq request (streaming-replacement-request replacement)))))

(defun current-streaming-mesh-request-p (scene request)
  (let ((snapshot (streaming-mesh-request-snapshot request)))
    (and (eq scene (streaming-mesh-snapshot-scene snapshot))
         (equalp (streaming-mesh-snapshot-stamp snapshot)
                 (streaming-scene-mesh-stamp
                  scene
                  (streaming-mesh-snapshot-output-keys snapshot)
                  (streaming-mesh-snapshot-bevel-width snapshot))))))

(defun streaming-scene-pending-mesh-count (scene)
  "Number of output owners still being computed, for status displays."
  (let ((replacement (streaming-scene-replacement scene)))
    (if (and replacement
             (null (streaming-replacement-result replacement))
             (null (streaming-replacement-failure replacement)))
        (length (streaming-replacement-output-keys replacement))
        0)))

(defun make-streaming-removal (removals generation)
  "A ready replacement with departures and no worker-produced output owners."
  (make-streaming-replacement
   nil removals nil (%make-streaming-mesh-result nil generation)))

(defun receive-streaming-mesh-result (scene result)
  (let ((request (production:production-result-request result)))
    (when (pending-streaming-mesh-request-p scene request)
      (if (production:production-result-condition result)
          (progn
            (setf (streaming-replacement-failure
                   (streaming-scene-replacement scene))
                  (production:production-result-condition result))
            (push result (streaming-scene-production-errors scene))
            (error "LUFT mesh production for cohort ~S failed: ~A"
                   (streaming-replacement-output-keys
                    (streaming-scene-replacement scene))
                   (production:production-result-condition result)))
          (accept-streaming-mesh-result
           scene request (production:production-result-value result))))))

(defun receive-streaming-source-result (scene result)
  (let* ((request (production:production-result-request result))
         (key (authored-chunk-load-request-chunk-key request))
         (ticket (production:production-request-ticket request)))
    (when (eql ticket (gethash key (streaming-scene-load-outstanding scene)))
      (if (production:production-result-condition result)
          (progn
            (remhash key (streaming-scene-load-outstanding scene))
            (push result (streaming-scene-production-errors scene))
            (error "LUFT source production for chunk ~D failed: ~A"
                   key (production:production-result-condition result)))
          (unless (accept-authored-chunk-load-result
                   scene request (production:production-result-value result))
            (remhash key (streaming-scene-load-outstanding scene)))))))

;;; An edit changes the shared light field, so all resident owners must receive
;;; the resulting geometry/light generation together. Camera retargeting uses
;;; the same scheduling interface but may choose a smaller affected region.

(defun schedule-streaming-scene-edit
    (scene production-system changed-source-key bevel-width)
  "Remesh the resident scene after one already-published authored edit."
  (check-type scene streaming-scene)
  (when (streaming-scene-replacement scene)
    (error "Cannot schedule an edit while another streaming replacement is active."))
  (let ((loaded (streaming-scene-loaded scene)))
    (unless (nth-value 1 (gethash changed-source-key loaded))
      (when (loop for key being the hash-keys of loaded
                  thereis (chunk-keys-neighbor-p key changed-source-key))
        (setf (gethash changed-source-key loaded) bevel-width)))
    (let* ((source-keys
             (sort (loop for key being the hash-keys of loaded collect key) #'<))
           (affected (streaming-scene-canonical-owner-closure scene source-keys)))
      (when affected
        (schedule-streaming-scene-cohort
         scene production-system affected bevel-width
         (if (streaming-scene-focus scene)
             (reduce #'min affected
                     :key (lambda (key)
                            (streaming-scene-key-distance
                             key (streaming-scene-focus scene))))
             0)
         :realize-torch-light-p t)
        (setf (streaming-scene-frame-counter scene) 0))
      affected)))
