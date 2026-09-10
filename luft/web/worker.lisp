(in-package #:luft.web)

(defun worker-javascript ()
  "Standalone CPU worker: no Three, DOM, residency policy or GPU resources."
  (ps:ps*
   `(progn
      (defvar atlas ,(array-form (atlas-data)))
      (defvar initial-cells (array))
      ,(core-form)
      ,(generation-form)
      ,(meshing-form)
      (setf (@ self onmessage)
            (lambda (event)
              (let ((job (@ event data)))
                (if (= (@ job type) "init")
                    (setf cells (new (|Map| (@ job cells))))
                    (progn
                      (setf world-edits (new (|Map| (@ job edits))))
                      (let* ((chunk (make-chunk (@ job cx) (@ job cy)))
                             (product (mesh-data (chunk-sites chunk)
                                                 (lambda (x y z) (chunk-cell-at chunk x y z)))))
                        ((@ self post-message)
                         (create :epoch (@ job epoch) :chunk chunk :product product)
                         (array (@ chunk data buffer) (@ product positions buffer)
                                (@ product normals buffer) (@ product colors buffer))))))))))))

(defun worker-transport-form ()
  "One outstanding job, no backlog. Epochs reject superseded edit/lifetime
snapshots; current demand rejects results from old destinations. GPU objects
never cross this boundary. An old resident mesh stays visible during remesh."
  '(progn
    (defvar generator null)
    (defvar worker-state (create :busy false :epoch 0 :accepted 0 :discarded 0 :error null))
    (defun dispose-worker ()
      (when generator ((@ generator terminate)))
      (setf generator null (@ worker-state busy) false)
      (incf (@ worker-state epoch)))
    (defun fail-worker (event)
      (dispose-worker)
      (setf (@ worker-state error) (or (@ event message) "Chunk worker could not load"))
      (set-status (+ "World generation stopped: " (@ worker-state error) ". Reload to retry.")))
    (defun ensure-worker ()
      (unless generator
        (setf generator (new (|Worker| "luft-worker.js")))
        (setf (@ generator onerror) fail-worker
              (@ generator onmessageerror) fail-worker
              (@ generator onmessage)
              (lambda (event)
                (setf (@ worker-state busy) false)
                (let* ((reply (@ event data)) (chunk (@ reply chunk))
                       (old ((@ chunks get) (@ chunk key))))
                  (if (and streaming-enabled
                           (= (@ reply epoch) (@ worker-state epoch))
                           (<= (max (abs (- (@ chunk cx) demand-x))
                                    (abs (- (@ chunk cy) demand-y))) 6))
                      (progn
                        ;; Realize first so an upload failure cannot publish
                        ;; collision cells without their visible counterpart.
                        (setf (@ chunk mesh) (if old (@ old mesh) null)
                              (@ chunk product) (@ reply product))
                        (chunk-added chunk)
                        ((@ chunks set) (@ chunk key) chunk)
                        (incf (@ worker-state accepted)))
                      (incf (@ worker-state discarded))))))
        ((@ generator post-message)
         (create :type "init" :cells ((@ |Array| from) cells)))))
    (defun request-worker-chunk (cx cy)
      (unless (or (@ worker-state busy) (@ worker-state error))
        (ensure-worker)
        (setf (@ worker-state busy) true)
        ((@ generator post-message)
         (create :type "chunk" :cx cx :cy cy :epoch (@ worker-state epoch)
                 :edits ((@ |Array| from) world-edits)))))
    (defun install-worker-generation ()
      (setf request-chunk request-worker-chunk
            invalidate-jobs (lambda () (incf (@ worker-state epoch)))
            dispose-generator dispose-worker
            chunk-changed (lambda (chunk) (setf (@ chunk dirty) true))))
    (defun await-startup-chunks (x y)
      (new (|Promise|
            (lambda (resolve reject)
              (labels ((poll ()
                         (cond
                           ((@ worker-state error)
                            (reject (new (|Error| (@ worker-state error)))))
                           ((not streaming-enabled)
                            (reject (new (|Error| "World startup cancelled"))))
                           ((>= (stream-ready-radius x y) 1) (resolve true))
                           (t (update-streaming x y) (set-timeout poll 10)))))
                (poll))))))))
