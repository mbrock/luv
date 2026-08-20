(in-package #:luv.tests)

(defclass vulkan-retirement-probe (luv::gpu-object luv::vulkan-gpu-object)
  ((attempts :initarg :attempts :reader vulkan-retirement-probe-attempts)
   (fail-p :initarg :fail-p :reader vulkan-retirement-probe-fail-p)))

(defmethod luv::vulkan-native-teardown-closure
    ((probe vulkan-retirement-probe))
  ;; Capture only the test's explicit state boxes, as a real Vulkan teardown
  ;; captures raw handles rather than its public wrapper.
  (let ((attempts (vulkan-retirement-probe-attempts probe))
        (fail-p (vulkan-retirement-probe-fail-p probe)))
    (luv::make-gpu-retirement-sequence
     (lambda () (incf (first attempts)))
     (lambda ()
       (incf (second attempts))
       (when (car fail-p)
         (setf (car fail-p) nil)
         (error "injected Vulkan retirement failure")))
     (lambda () (incf (third attempts))))))

(defun make-vulkan-submit-probe-texture
    (device handle &key semaphore semaphore-state (semaphore-value 0)
                        submitted owner (layout :general))
  (let ((texture
          (make-instance
           'luv::vulkan-gpu-texture
           :label "Vulkan submit probe texture"
           :size '(4 4 1) :usage '(:texture-binding) :dimensions :2d
           :format :r8-unorm :vk-format :r8-unorm
           :handle handle :device device :owned-p nil
           :layout layout :external-semaphore semaphore
           :external-semaphore-state semaphore-state
           :external-semaphore-value semaphore-value
           :external-owner owner
           :external-submitted submitted)))
    #+sbcl (sb-ext:cancel-finalization texture)
    texture))

(defun make-vulkan-submit-probe-command-buffer
    (device handle &key resources textures final-layouts)
  (let ((command-buffer
          (make-instance
           'luv::vulkan-gpu-command-buffer
           :label "Vulkan submit probe command buffer"
           :handle handle :device device :command-pool :fake-command-pool
           :initial-texture-layouts nil
           :final-texture-layouts final-layouts
           :textures textures :resources resources :native-resources nil)))
    #+sbcl (sb-ext:cancel-finalization command-buffer)
    command-buffer))

(deftest vulkan-destroy-transfers-before-invalidation-and-retries
  (let* ((luv::*gpu-retirement-ledger-custodians*
           (make-hash-table :test #'eq))
         (luv::*gpu-retirement-custodian-service-enabled-p* nil)
         (device
           (make-instance
            'luv::vulkan-gpu-device
            :handle :fake-device :instance :fake-instance
            :physical-device :fake-physical-device :queue-family 0))
         (queue
           (make-instance
            'luv::vulkan-gpu-queue
            :handle :fake-queue :device device :family 0
            :timeline :fake-timeline))
         (attempts (list 0 0 0))
         (fail-p (list t))
         (probe
           (make-instance
            'vulkan-retirement-probe
            :handle :fake-resource :attempts attempts :fail-p fail-p))
         (frontier-function 'luv::vulkan-queue-completed-frontier)
         (original-frontier (symbol-function frontier-function)))
    (setf (luv::vulkan-device-queue device) queue)
    (unwind-protect
         (progn
           ;; Keep this ownership test backend-free: queue maintenance sees a
           ;; completed frontier without asking a Vulkan loader or device.
           (setf (symbol-function frontier-function) (lambda (queue)
                                                       (declare (ignore queue))
                                                       0))
           (handler-bind
               ((luv::gpu-native-retirement-warning #'muffle-warning))
             (luv::vulkan-destroy-or-defer
              probe device
              (lambda ()
                (ok (eq probe
                        (luv::gpu-retirement-entry-resource
                         (first
                          (luv::gpu-retirement-ledger-entries
                           (luv::vulkan-queue-retirement-ledger queue))))))
                (setf (luv::vulkan-object-destroyed-p probe) t)
                #+sbcl (sb-ext:cancel-finalization probe))))
           (ok (luv::vulkan-object-destroyed-p probe))
           (ok (equal '(1 1 0) attempts))
           (ok (= 1
                  (length
                   (luv::gpu-retirement-ledger-entries
                    (luv::vulkan-queue-retirement-ledger queue)))))
           (ok (eq queue
                   (gethash
                    (luv::vulkan-queue-retirement-ledger queue)
                    luv::*gpu-retirement-ledger-custodians*)))
           (luv::maintain-vulkan-queue queue)
           ;; The cached production closure resumes at its failed native call.
           (ok (equal '(1 2 1) attempts))
           (ok (null
                (luv::gpu-retirement-ledger-entries
                 (luv::vulkan-queue-retirement-ledger queue))))
           (ok (not (nth-value
                     1 (gethash
                        (luv::vulkan-queue-retirement-ledger queue)
                        luv::*gpu-retirement-ledger-custodians*)))))
      (setf (symbol-function frontier-function) original-frontier))))

(deftest vulkan-device-queues-non-gpu-native-owners-through-the-generic
  (let* ((device
           (make-instance
            'luv::vulkan-gpu-device
            :handle :fake-device :instance :fake-instance
            :physical-device :fake-physical-device :queue-family 0))
         (queue
           (make-instance
            'luv::vulkan-gpu-queue
            :handle :fake-queue :device device :family 0
            :timeline :fake-timeline))
         (frontier-symbol 'luv::vulkan-queue-completed-frontier)
         (original-frontier (symbol-function frontier-symbol))
         (attempts 0)
         (invalidated-p nil)
         (fail-p t))
    (setf (luv::vulkan-device-queue device) queue)
    (unwind-protect
         (progn
           (setf (symbol-function frontier-symbol)
                 (lambda (queue) (declare (ignore queue)) 0))
           (handler-bind
               ((luv:gpu-native-retirement-warning #'muffle-warning))
             (luv::retire-gpu-native-owner
              device :video-importer
              (lambda ()
                (incf attempts)
                (when fail-p
                  (setf fail-p nil)
                  (error "injected importer close failure")))
              (lambda () (setf invalidated-p t))))
           (ok invalidated-p)
           (ok (= 1 attempts))
           (ok (equal '(:video-importer)
                      (mapcar
                       #'luv::gpu-retirement-entry-resource
                       (luv::gpu-retirement-ledger-entries
                        (luv::vulkan-queue-retirement-ledger queue)))))
           (luv::maintain-vulkan-queue queue)
           (ok (= 2 attempts))
           (ok (null
                (luv::gpu-retirement-ledger-entries
                 (luv::vulkan-queue-retirement-ledger queue)))))
      (setf (symbol-function frontier-symbol) original-frontier))))

(deftest vulkan-custodian-service-retires-an-unobserved-completion
  (let* ((luv::*gpu-retirement-ledger-custodians*
           (make-hash-table :test #'eq))
         (luv::*gpu-retirement-custodian-service-failures*
           (make-hash-table :test #'eq))
         (luv::*gpu-retirement-custodian-service-enabled-p* nil)
         (frontier 0)
         (native-submits 0)
         (frontier-symbol 'luv::vulkan-queue-completed-frontier)
         (submit-symbol 'luv.vulkan:submit-command-buffers)
         (original-frontier (symbol-function frontier-symbol))
         (original-submit (symbol-function submit-symbol)))
    (unwind-protect
         (progn
           (setf (symbol-function frontier-symbol)
                 (lambda (queue)
                   (declare (ignore queue))
                   frontier)
                 (symbol-function submit-symbol)
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   (incf native-submits)))
           (multiple-value-bind (weak-queue ledger attempts)
               (let* ((device
                        (make-instance
                         'luv::vulkan-gpu-device
                         :handle :fake-device :instance :fake-instance
                         :physical-device :fake-physical-device
                         :queue-family 0))
                      (queue
                        (make-instance
                         'luv::vulkan-gpu-queue
                         :handle :fake-queue :device device :family 0
                         :timeline :fake-timeline))
                      (attempts (list 0 0 0))
                      (probe
                        (make-instance
                         'vulkan-retirement-probe
                         :handle :fake-resource :attempts attempts
                         :fail-p (list nil)))
                      (command-buffer
                        (make-vulkan-submit-probe-command-buffer
                         device :fake-command-buffer
                         :resources (list probe))))
                 (setf (luv::vulkan-device-queue device) queue)
                 #+sbcl (sb-ext:cancel-finalization device)
                 #+sbcl (sb-ext:cancel-finalization queue)
                 (ok (= 1 (luv:submit queue command-buffer)))
                 (luv::vulkan-destroy-or-defer
                  probe device
                  (lambda ()
                    (setf (luv::vulkan-object-destroyed-p probe) t)
                    #+sbcl (sb-ext:cancel-finalization probe)))
                 (ok (equal '(0 0 0) attempts))
                 (let ((ledger
                         (luv::vulkan-queue-retirement-ledger queue)))
                   (ok (eq queue
                           (gethash
                            ledger
                            luv::*gpu-retirement-ledger-custodians*)))
                   ;; From here no caller retains queue/device/resource access.
                   (values #+sbcl (sb-ext:make-weak-pointer queue)
                           #-sbcl nil
                           ledger attempts)))
             #+sbcl (progn
                      (sb-ext:gc :full t)
                      (ok (sb-ext:weak-pointer-value weak-queue)))
             (setf frontier 1)
             (ok (luv::service-gpu-retirement-custodians-once))
             (ok (= 1 native-submits))
             (ok (equal '(1 1 1) attempts))
             (ok (null (luv::gpu-retirement-ledger-entries ledger)))
             (ok (zerop
                  (hash-table-count
                   luv::*gpu-retirement-ledger-custodians*)))))
      (setf (symbol-function frontier-symbol) original-frontier
            (symbol-function submit-symbol) original-submit))))

(deftest vulkan-custodian-service-does-no-ffi-after-admission-closes
  (let* ((luv::*gpu-retirement-ledger-custodians*
           (make-hash-table :test #'eq))
         (luv::*gpu-retirement-custodian-service-enabled-p* nil)
         (device
           (make-instance
            'luv::vulkan-gpu-device
            :handle :fake-device :instance :fake-instance
            :physical-device :fake-physical-device :queue-family 0))
         (queue
           (make-instance
            'luv::vulkan-gpu-queue
            :handle :fake-queue :device device :family 0
            :timeline :fake-timeline))
         (frontier-symbol 'luv::vulkan-queue-completed-frontier)
         (original-frontier (symbol-function frontier-symbol))
         (frontier-calls 0))
    (setf (luv::vulkan-device-queue device) queue
          (luv::vulkan-device-retiring-p device) t)
    (luv::retain-gpu-retirement-ledger-custodian
     (luv::vulkan-queue-retirement-ledger queue) queue)
    (unwind-protect
         (progn
           (setf (symbol-function frontier-symbol)
                 (lambda (queue)
                   (declare (ignore queue))
                   (incf frontier-calls)
                   0))
           (ok (luv::service-gpu-retirement-custodians-once))
           (ok (zerop frontier-calls))
           (ok (zerop
                (hash-table-count
                 luv::*gpu-retirement-ledger-custodians*))))
      (setf (symbol-function frontier-symbol) original-frontier))))

(deftest vulkan-submit-publishes-before-fallible-owner-callbacks-and-retries
  (let* ((luv::*gpu-retirement-ledger-custodians*
           (make-hash-table :test #'eq))
         (luv::*gpu-retirement-custodian-service-enabled-p* nil)
         (device
           (make-instance
            'luv::vulkan-gpu-device
            :handle :fake-device :instance :fake-instance
            :physical-device :fake-physical-device :queue-family 0))
         (queue
           (make-instance
            'luv::vulkan-gpu-queue
            :handle :fake-queue :device device :family 0
            :timeline :queue-timeline))
         (state
           (luv::make-vulkan-external-semaphore-state
            :semaphore :external-timeline :value 4 :references 2))
         (first-attempts 0)
         (second-attempts 0)
         (fail-first-p t)
         (first-values '())
         (second-values '())
         (first-texture
           (make-vulkan-submit-probe-texture
            device :plane-0 :semaphore :external-timeline
            :semaphore-state state
            :submitted
            (lambda (layout value)
              (declare (ignore layout))
              (incf first-attempts)
              (when fail-first-p
                (setf fail-first-p nil)
                (error "injected plane callback failure"))
              (push value first-values))))
         (second-texture
           (make-vulkan-submit-probe-texture
            device :plane-1 :semaphore :external-timeline
            :semaphore-state state
            :submitted
            (lambda (layout value)
              (declare (ignore layout))
              (incf second-attempts)
              (push value second-values))))
         (command-buffer
           (make-vulkan-submit-probe-command-buffer
            device :fake-command-buffer
            :resources (list first-texture second-texture)
            :textures (list first-texture second-texture)
            :final-layouts
            (list (cons first-texture :shader-read-only-optimal)
                  (cons second-texture :shader-read-only-optimal))))
         (frontier 0)
         (native-submits 0)
         (native-waits nil)
         (native-signals nil)
         (frontier-symbol 'luv::vulkan-queue-completed-frontier)
         (submit-symbol 'luv.vulkan:submit-command-buffers)
         (original-frontier (symbol-function frontier-symbol))
         (original-submit (symbol-function submit-symbol)))
    (setf (luv::vulkan-device-queue device) queue
          (luv::vulkan-queue-external-semaphore-states queue) (list state))
    (unwind-protect
         (progn
           (setf (symbol-function frontier-symbol)
                 (lambda (queue)
                   (declare (ignore queue))
                   frontier)
                 (symbol-function submit-symbol)
                 (lambda (native-queue native-command-buffers
                          &key wait-semaphores signal-semaphores)
                   (declare (ignore native-queue native-command-buffers))
                   (incf native-submits)
                   (setf native-waits wait-semaphores
                         native-signals signal-semaphores)))
           (ok (signals
                (luv:submit queue command-buffer)
                'luv::vulkan-gpu-error))
           ;; Native work, portable state, dependencies, and the retry closure
           ;; are all queue-owned before the injected owner callback escapes.
           (ok (= 1 native-submits))
           (ok (= 1 (luv::vulkan-queue-submission-counter queue)))
           (ok (eq :submitted
                   (luv::vulkan-command-buffer-state command-buffer)))
           (ok (= 1 (length (luv::vulkan-queue-live-submissions queue))))
           (ok (eq queue
                   (gethash
                    (luv::vulkan-queue-retirement-ledger queue)
                    luv::*gpu-retirement-ledger-custodians*)))
           (ok (= 5 (luv::vulkan-texture-external-semaphore-value
                     first-texture)))
           (ok (= 5 (luv::vulkan-texture-external-semaphore-value
                     second-texture)))
           (ok (= 1 first-attempts))
           (ok (= 1 second-attempts))
           (ok (equal '(5) second-values))
           (ok (= 4 (third (aref native-waits 0))))
           (ok (= 5 (third (aref native-signals 0))))
           (setf frontier 1)
           (ok (luv::service-gpu-retirement-custodians-once))
           (ok (= 1 native-submits))
           (ok (= 2 first-attempts))
           (ok (= 1 second-attempts))
           (ok (equal '(5) first-values))
           (ok (null (luv::vulkan-queue-live-submissions queue)))
           (ok (zerop
                (hash-table-count
                 luv::*gpu-retirement-ledger-custodians*))))
      (setf (symbol-function frontier-symbol) original-frontier
            (symbol-function submit-symbol) original-submit))))

(deftest vulkan-shared-external-timeline-advances-across-plane-subsets
  (let* ((luv::*gpu-retirement-ledger-custodians*
           (make-hash-table :test #'eq))
         (luv::*gpu-retirement-custodian-service-enabled-p* nil)
         (device
           (make-instance
            'luv::vulkan-gpu-device
            :handle :fake-device :instance :fake-instance
            :physical-device :fake-physical-device :queue-family 0))
         (queue
           (make-instance
            'luv::vulkan-gpu-queue
            :handle :fake-queue :device device :family 0
            :timeline :queue-timeline))
         (state
           (luv::retain-vulkan-external-semaphore-state
            queue :shared-timeline 10))
         (same-state
           (luv::retain-vulkan-external-semaphore-state
            queue :shared-timeline 9))
         (first-values '())
         (second-values '())
         (first-texture
           (make-vulkan-submit-probe-texture
            device :plane-0 :semaphore :shared-timeline
            :semaphore-state state
            :submitted
            (lambda (layout value)
              (declare (ignore layout))
              (push value first-values))))
         (second-texture
           (make-vulkan-submit-probe-texture
            device :plane-1 :semaphore :shared-timeline
            :semaphore-state same-state
            :submitted
            (lambda (layout value)
              (declare (ignore layout))
              (push value second-values))))
         (both
           (make-vulkan-submit-probe-command-buffer
            device :both
            :resources (list first-texture second-texture)
            :textures (list first-texture second-texture)
            :final-layouts
            (list (cons first-texture :shader-read-only-optimal)
                  (cons second-texture :shader-read-only-optimal))))
         (first-only
           (make-vulkan-submit-probe-command-buffer
            device :first-only :resources (list first-texture)
            :textures (list first-texture)
            :final-layouts
            (list (cons first-texture :general))))
         (second-only
           (make-vulkan-submit-probe-command-buffer
            device :second-only :resources (list second-texture)
            :textures (list second-texture)
            :final-layouts
            (list (cons second-texture :general))))
         (frontier 0)
         (native-frontiers '())
         (frontier-symbol 'luv::vulkan-queue-completed-frontier)
         (submit-symbol 'luv.vulkan:submit-command-buffers)
         (original-frontier (symbol-function frontier-symbol))
         (original-submit (symbol-function submit-symbol)))
    (ok (eq state same-state))
    (setf (luv::vulkan-device-queue device) queue)
    (unwind-protect
         (progn
           (setf (symbol-function frontier-symbol)
                 (lambda (queue)
                   (declare (ignore queue))
                   frontier)
                 (symbol-function submit-symbol)
                 (lambda (native-queue native-command-buffers
                          &key wait-semaphores signal-semaphores)
                   (declare (ignore native-queue native-command-buffers))
                   (push
                    (list (third (aref wait-semaphores 0))
                          (third (aref signal-semaphores 0)))
                    native-frontiers)))
           (ok (= 1 (luv:submit queue both)))
           (ok (= 2 (luv:submit queue first-only)))
           (ok (= 3 (luv:submit queue second-only)))
           ;; Both planes share one generation state.  A subset advances it for
           ;; its absent sibling, so rejoining never repeats a timeline signal.
           (ok (equal '((10 11) (11 12) (12 13))
                      (nreverse native-frontiers)))
           (ok (= 13 (luv::vulkan-texture-external-semaphore-value
                      first-texture)))
           (ok (= 13 (luv::vulkan-texture-external-semaphore-value
                      second-texture)))
           (ok (equal '(12 11) first-values))
           (ok (equal '(13 11) second-values))
           (setf frontier 3)
           (ok (luv::service-gpu-retirement-custodians-once))
           (ok (null (luv::vulkan-queue-live-submissions queue)))
           (luv::release-vulkan-external-semaphore-state queue state)
           (luv::release-vulkan-external-semaphore-state queue state)
           (ok (null (luv::vulkan-queue-external-semaphore-states queue)))
           ;; Reusing the same raw handle starts a fresh state generation.
           (let ((reused
                   (luv::retain-vulkan-external-semaphore-state
                    queue :shared-timeline 2)))
             (ok (not (eq reused state)))
             (ok (= 2 (luv::vulkan-external-semaphore-state-value reused)))
             (luv::release-vulkan-external-semaphore-state queue reused)))
      (setf (symbol-function frontier-symbol) original-frontier
            (symbol-function submit-symbol) original-submit))))

(deftest vulkan-leak-warning-nonlocal-exit-cannot-preempt-custody
  (let ((luv::*gpu-finalizer-retirement-ledger*
          (luv::make-gpu-retirement-ledger))
        (luv::*leaked-gpu-resources* nil)
        (attempts 0)
        (fail-p t))
    (handler-bind
        ((luv:gpu-native-retirement-warning #'muffle-warning))
      (ok (signals
           (handler-bind
               ((luv:gpu-resource-leaked
                  (lambda (condition)
                    (declare (ignore condition))
                    (error "promoted leak warning"))))
             (luv::retire-vulkan-leaked-native-owner
              'vulkan-gpu-texture "leaked probe" nil :leaked-owner
              (lambda ()
                (incf attempts)
                (when fail-p
                  (error "injected native close failure")))))
           'simple-error)))
    (ok (= 1 attempts))
    (ok (equal '(:leaked-owner)
               (mapcar
                #'luv::gpu-retirement-entry-resource
                (luv::gpu-retirement-ledger-entries
                 luv::*gpu-finalizer-retirement-ledger*))))
    (setf fail-p nil)
    (luv::maintain-gpu-finalizer-retirements)
    (ok (= 2 attempts))
    (ok (null
         (luv::gpu-retirement-ledger-entries
          luv::*gpu-finalizer-retirement-ledger*)))))

(deftest vulkan-post-device-texture-destroy-runs-device-independent-tail
  (let* ((device
           (make-instance
            'luv::vulkan-gpu-device
            :handle :retired-device :instance :fake-instance
            :physical-device :fake-physical-device :queue-family 0))
         (queue
           (make-instance
            'luv::vulkan-gpu-queue
            :handle :retired-queue :device device :family 0
            :timeline :retired-timeline))
         (state
           (luv::make-vulkan-external-semaphore-state
            :semaphore :retired-external-timeline
            :value 7 :references 1))
         (owner-releases 0)
         (texture nil))
    (setf (luv::vulkan-device-queue device) queue
          (luv::vulkan-queue-external-semaphore-states queue) (list state)
          texture
          (make-vulkan-submit-probe-texture
           device :external-plane
           :semaphore :retired-external-timeline
           :semaphore-state state
           :owner (lambda () (incf owner-releases)))
          (luv::vulkan-device-retiring-p device) t
          (luv::vulkan-device-native-retired-p device) t)
    (luv:destroy texture)
    (ok (= 1 owner-releases))
    (ok (zerop
         (luv::vulkan-external-semaphore-state-references state)))
    (ok (null (luv::vulkan-queue-external-semaphore-states queue)))
    (ok (luv::vulkan-object-destroyed-p texture))))

(deftest vulkan-render-pass-cache-rechecks-admission-under-queue-lock
  (let* ((device
           (make-instance
            'luv::vulkan-gpu-device
            :handle :fake-device :instance :fake-instance
            :physical-device :fake-physical-device :queue-family 0))
         (queue
           (make-instance
            'luv::vulkan-gpu-queue
            :handle :fake-queue :device device :family 0
            :timeline :fake-timeline))
         (create-symbol 'luv.vulkan:create-color-render-pass)
         (original-create (symbol-function create-symbol))
         (native-creates 0)
         (descriptor
           (luv:make-texture-descriptor
            :size '(4 4) :dimensions :2d :format :rgba8-unorm
            :usage :render-attachment)))
    (setf (luv::vulkan-device-queue device) queue
          (luv::vulkan-device-retiring-p device) t)
    (unwind-protect
         (progn
           (setf (symbol-function create-symbol)
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   (incf native-creates)
                   :impossible-render-pass))
           (ok (signals
                (luv::vulkan-render-pass-for-format
                 device :rgba8-unorm descriptor)
                'luv:gpu-object-destroyed-error))
           (ok (zerop native-creates))
           (ok (zerop
                (hash-table-count
                 (luv::vulkan-device-render-passes device)))))
      (setf (symbol-function create-symbol) original-create))))

(deftest vulkan-finish-retains-ended-encoder-ownership-on-wrapper-failure
  (let* ((luv::*gpu-retirement-ledger-custodians*
           (make-hash-table :test #'eq))
         (luv::*gpu-retirement-custodian-service-enabled-p* nil)
         (device
           (make-instance
            'luv::vulkan-gpu-device
            :handle :fake-device :instance :fake-instance
            :physical-device :fake-physical-device :queue-family 0))
         (queue
           (make-instance
            'luv::vulkan-gpu-queue
            :handle :fake-queue :device device :family 0
            :timeline :fake-timeline))
         (encoder
           (make-instance
            'luv::vulkan-gpu-command-encoder
            :label "finish handoff probe" :device device
            :command-pool :fake-command-pool
            :command-buffer :fake-command-buffer))
         (wrapper nil)
         (end-symbol 'luv.vulkan:end-command-buffer)
         (destroy-pool-symbol 'luv.vulkan:destroy-command-pool)
         (frontier-symbol 'luv::vulkan-queue-completed-frontier)
         (constructor-symbol 'luv::make-vulkan-finished-command-buffer)
         (symbols (list end-symbol destroy-pool-symbol
                        frontier-symbol constructor-symbol))
         (originals (mapcar #'symbol-function symbols))
         (ends 0)
         (pool-destroys 0)
         (fail-construction-p t))
    (setf (luv::vulkan-device-queue device) queue)
    (luv::install-vulkan-encoder-leak-finalizer encoder)
    (unwind-protect
         (progn
           (setf (symbol-function end-symbol)
                 (lambda (command-buffer)
                   (declare (ignore command-buffer))
                   (incf ends))
                 (symbol-function destroy-pool-symbol)
                 (lambda (native-device pool)
                   (declare (ignore native-device pool))
                   (incf pool-destroys))
                 (symbol-function frontier-symbol)
                 (lambda (queue)
                   (declare (ignore queue))
                   0)
                 (symbol-function constructor-symbol)
                 (let ((original (fourth originals)))
                   (lambda (&rest arguments)
                     (when fail-construction-p
                       (setf fail-construction-p nil)
                       (error "injected command-buffer wrapper failure"))
                     (apply original arguments))))
           (ok (signals (luv:finish encoder) 'simple-error))
           (ok (= 1 ends))
           (ok (eq :ended (luv::vulkan-command-encoder-state encoder)))
           (ok (eq :fake-command-pool
                   (luv::vulkan-command-encoder-command-pool encoder)))
           (setf wrapper (luv:finish encoder))
           (ok (= 1 ends))
           (ok (eq :finished (luv::vulkan-command-encoder-state encoder)))
           (ok (null (luv::vulkan-command-encoder-command-pool encoder)))
           (luv:destroy wrapper)
           (setf wrapper nil)
           (ok (= 1 pool-destroys)))
      (when wrapper (luv:destroy wrapper))
      (luv:destroy encoder)
      (loop for symbol in symbols
            for original in originals
            do (setf (symbol-function symbol) original)))))

(deftest vulkan-device-destroy-resumes-native-progress-after-ledger-barrier
  (let* ((device
           (make-instance
            'luv::vulkan-gpu-device
            :handle :fake-device :instance :fake-instance
            :physical-device :fake-physical-device :queue-family 0))
         (queue
           (make-instance
            'luv::vulkan-gpu-queue
            :handle :fake-queue :device device :family 0
            :timeline :fake-timeline))
         (frontier-symbol 'luv::vulkan-queue-completed-frontier)
         (wait-symbol 'luv.vulkan:device-wait-idle)
         (semaphore-symbol 'luv.vulkan:destroy-semaphore)
         (render-pass-symbol 'luv.vulkan:destroy-render-pass)
         (device-symbol 'luv.vulkan:destroy-device)
         (instance-symbol 'luv.vulkan:destroy-instance)
         (symbols (list frontier-symbol wait-symbol semaphore-symbol
                        render-pass-symbol device-symbol instance-symbol))
         (originals (mapcar #'symbol-function symbols))
         (waits 0)
         (semaphores 0)
         (render-a 0)
         (render-b 0)
         (native-devices 0)
         (instances 0)
         (external-releases 0)
         (fail-ledger-p t)
         (fail-render-b-p t)
         (fail-instance-p t))
    (setf (luv::vulkan-device-queue device) queue)
    ;; Production installs both shared closures before any render-pass cache
    ;; entries exist.  The pass drain must therefore snapshot lazily.
    (luv::ensure-vulkan-device-retirement-teardowns device queue)
    (setf
          (gethash :a (luv::vulkan-device-render-passes device)) :render-a
          (gethash :b (luv::vulkan-device-render-passes device)) :render-b)
    (luv::enqueue-gpu-retirement
     (luv::vulkan-queue-retirement-ledger queue) :barrier 0
     (lambda ()
       (when fail-ledger-p
         (setf fail-ledger-p nil)
         (error "injected ledger barrier failure"))))
    (unwind-protect
         (progn
           (setf (symbol-function frontier-symbol)
                 (lambda (queue) (declare (ignore queue)) 0)
                 (symbol-function wait-symbol)
                 (lambda (native-device)
                   (declare (ignore native-device))
                   (incf waits))
                 (symbol-function semaphore-symbol)
                 (lambda (native-device semaphore)
                   (declare (ignore native-device semaphore))
                   (incf semaphores))
                 (symbol-function render-pass-symbol)
                 (lambda (native-device render-pass)
                   (declare (ignore native-device))
                   (ecase render-pass
                     (:render-a (incf render-a))
                     (:render-b
                      (incf render-b)
                      (when fail-render-b-p
                        (setf fail-render-b-p nil)
                        (error "injected render-pass failure")))))
                 (symbol-function device-symbol)
                 (lambda (native-device)
                   (declare (ignore native-device))
                   (incf native-devices))
                 (symbol-function instance-symbol)
                 (lambda (instance)
                   (declare (ignore instance))
                   (incf instances)
                   (when fail-instance-p
                     (setf fail-instance-p nil)
                     (error "injected instance failure"))))
           ;; The first attempt completes the idle call but cannot close
           ;; admission while a ledger owner remains failed.
           (ok (signals
                (handler-bind
                    ((luv:gpu-native-retirement-warning #'muffle-warning))
                  (luv:destroy device))
                'luv:gpu-native-retirement-error))
           (ok (= 1 waits))
           (ok (not (luv::vulkan-device-retiring-p device)))
           (ok (zerop semaphores))
           ;; The next attempt reuses the completed wait, drains the barrier,
           ;; closes admission, and stops at one native render-pass failure.
           (ok (signals (luv:destroy device) 'simple-error))
           (ok (= 1 waits))
           (ok (luv::vulkan-device-retiring-p device))
           (ok (= 1 semaphores))
           (ok (not (luv::vulkan-device-native-retired-p device)))
           ;; Model the wrapper being dropped here: its installed finalizer
           ;; owns the exact native progress sequence already advanced by
           ;; explicit DESTROY.  Its own idle step is safe and runs once, then
           ;; retirement resumes at the failed pass rather than at semaphore.
           (ok (signals
                (funcall (luv::vulkan-device-finalizer-teardown device))
                'simple-error))
           (ok (= 2 waits))
           (ok (= 1 render-a))
           (ok (= 2 render-b))
           (ok (= 1 native-devices))
           (ok (= 1 instances))
           (ok (luv::vulkan-device-native-retired-p device))
           (ok (not (luv::vulkan-object-destroyed-p device)))
           ;; A texture finalized in this post-vkDestroyDevice window still
           ;; runs its non-device owner callback while guarded Vk calls skip.
           (let ((texture
                   (make-instance
                    'luv::vulkan-gpu-texture
                    :size '(1 1 1) :usage '(:texture-binding)
                    :dimensions :2d :format :r8-unorm
                    :handle :retired-image :device device :vk-format :r8-unorm
                    :owned-p nil
                    :external-owner (lambda () (incf external-releases)))))
             (luv:destroy texture))
           (ok (= 1 external-releases))
           ;; Retrying that same durable finalizer closure resumes at the
           ;; failed instance call without repeating its idle or device steps.
           (funcall (luv::vulkan-device-finalizer-teardown device))
           (luv:destroy device)
           (ok (= 2 waits))
           (ok (= 1 semaphores))
           (ok (= 1 render-a))
           (ok (= 2 render-b))
           (ok (= 1 native-devices))
           (ok (= 2 instances))
           (ok (luv::vulkan-object-destroyed-p device))
           (ok (luv::vulkan-object-destroyed-p queue)))
      (loop for symbol in symbols
            for original in originals
            do (setf (symbol-function symbol) original)))))

(deftest default-provider-prefers-the-native-apple-backend
  #+darwin
  (ok (typep luv:*gpu-provider* 'luv:metal-gpu-provider))
  #-darwin
  (ok (typep luv:*gpu-provider* 'luv:vulkan-gpu-provider)))

(deftest renderer-readbacks-use-compressed-png-output
  (let ((pixels (make-array (* 64 64 4)
                            :element-type '(unsigned-byte 8)
                            :initial-element 255)))
    (uiop:with-temporary-file
        (:pathname pathname :prefix "luv-readback-" :suffix ".png")
      (luv:write-rgba-png pathname pixels 64 64 :rgba8-unorm)
      (with-open-file (stream pathname :element-type '(unsigned-byte 8))
        (ok (< (file-length stream) 1024))))))

(deftest cadence-clock-wakes-before-deadlines-and-preserves-phase
  (let* ((timestamps nil)
         (clock
           (luv:make-cadence-clock
            (lambda (canvas timestamp)
              (declare (ignore canvas))
              (push timestamp timestamps))
            :frames-per-second 60)))
    (luv:service-canvas-clock clock nil 10d0)
    (ok (equal timestamps '(10d0)))
    ;; The millisecond SDL wait must wake before the fractional deadline.
    (ok (= 16 (luv:clock-wait-timeout clock 10d0)))
    ;; An ordinary late wake does not move the cadence's original phase.
    (luv:service-canvas-clock clock nil 10.017d0)
    (ok (= 16 (luv:clock-wait-timeout clock 10.017d0)))
    (ok (< (abs (- (luv::cadence-clock-next-frame-time clock)
                   (+ 10d0 (/ 2d0 60d0))))
           1d-12))
    ;; A long pause skips missed frames rather than replaying them.
    (luv:service-canvas-clock clock nil 10.1d0)
    (ok (= 3 (length timestamps)))
    (ok (> (luv::cadence-clock-next-frame-time clock) 10.1d0))))

(deftest lazy-clock-gives-one-time-to-a-whole-turn
  (let ((samples '(10d0 20d0 30d0))
        (calls 0))
    (let ((clock
            (luv:make-lazy-clock
             :source (lambda ()
                       (incf calls)
                       (pop samples)))))
      (ok (= 10d0 (luv:lazy-clock-now clock)))
      (ok (= 10d0 (luv:lazy-clock-now clock)))
      (ok (= 1 calls))
      (luv:call-with-lazy-clock-time
       clock 99d0
       (lambda () (ok (= 99d0 (luv:lazy-clock-now clock)))))
      (ok (= 10d0 (luv:lazy-clock-now clock)))
      (ok (= 20d0 (luv:lazy-clock-now-unadjusted clock)))
      (ok (= 10d0 (luv:lazy-clock-now clock)))
      (luv:clear-lazy-clock clock)
      (ok (= 30d0 (luv:lazy-clock-now clock)))
      (ok (= 3 calls)))))

(deftest cadence-presentation-time-is-the-following-display-beat
  (let* ((clock (luv:make-lazy-clock :source (lambda () 10d0)))
         (cadence
           (luv:make-cadence-clock
            (lambda (canvas timestamp)
              (declare (ignore canvas timestamp)))
            :frames-per-second 60))
         (canvas
           (luv:make-sdl-canvas :clock cadence :time clock)))
    (ok (< (abs (- (luv:canvas-presentation-time canvas)
                   (+ 10d0 (/ 1d0 60d0))))
           1d-12))))

(deftest presentation-clock-asks-once-and-leaves-pacing-to-the-frame
  (let* ((timestamps nil)
        (clock
          (luv:make-presentation-clock
           (lambda (canvas timestamp)
             (declare (ignore canvas))
             (push timestamp timestamps)))))
    (ok (= 0 (luv:clock-wait-timeout clock 4d0)))
    (ok (luv:service-canvas-clock clock nil 4d0))
    (ok (equal timestamps '(4d0)))))

(deftest canvas-loop-failure-preserves-the-actionable-condition
  (let* ((canvas (luv:make-sdl-canvas))
         (root-cause (make-condition 'simple-error
                                     :format-control "event dispatch failed"))
         (request (luv::make-sdl-canvas-request :function #'identity)))
    (setf (luv::sdl-canvas-startup-error canvas) root-cause
          (luv::sdl-canvas-requests canvas) (list request))
    (luv::fail-sdl-canvas-requests
     canvas (luv::sdl-canvas-terminal-error canvas))
    (ok (eq root-cause (luv::sdl-canvas-request-error request)))
    (ok (null (luv::sdl-canvas-requests canvas)))))

(deftest slug-formats-retain-the-exact-vulkan-abi-values
  (ok (= 81 (cffi:foreign-enum-value 'lvk::format :r16g16-uint)))
  (ok (= 97
         (cffi:foreign-enum-value
          'lvk::format :r16g16b16a16-sfloat))))

(deftest video-planes-retain-the-exact-vulkan-abi-values
  (ok (= 9 (cffi:foreign-enum-value 'lvk::format :r8-unorm)))
  (ok (= 16 (cffi:foreign-enum-value 'lvk::format :r8g8-unorm)))
  (ok (= #x10
         (cffi:foreign-bitfield-value 'lvk::image-aspect-flags '(:plane-0))))
  (ok (= #x20
         (cffi:foreign-bitfield-value 'lvk::image-aspect-flags '(:plane-1))))
  (ok (= #x20
         (cffi:foreign-bitfield-value 'lvk::queue-flags '(:video-decode)))))

(deftest premultiplied-alpha-retains-the-exact-vulkan-blend-factor
  (ok (= 7
         (cffi:foreign-enum-value
          'lvk::blend-factor :one-minus-src-alpha))))

(deftest sampled-texture-layouts-do-not-require-a-sampler
  (let* ((entries '((:binding 0 :type :texture)
                    (:binding 1 :type :texture)
                    (:binding 2 :type :uniform-buffer)))
         (descriptor
           (luv:make-bind-group-layout-descriptor :entries entries)))
    (ok (equal entries
               (luv::texture-sampler-uniform-layout-entries descriptor)))))

(deftest definitions-retain-abi-metadata-without-call-classes
  (let ((description (lvk:vulkan-function-description 'vk:create-instance)))
    (ok (equal (getf description :foreign-name) "vkCreateInstance"))
    (ok (eq (getf description :return-type) 'lvk::checked-result))
    (ok (equal (mapcar #'first (getf description :arguments))
               '(lvk::create-info lvk::allocator lvk::instance)))
    (ok (not (getf description :command-p)))
    (ok (null (find-class 'vk:create-instance nil)))))

(deftest vulkan-device-contract-includes-shader-int64
  (let ((description
          (lvk:vulkan-function-description
           'vk:get-physical-device-features)))
    (ok (equal (getf description :foreign-name)
               "vkGetPhysicalDeviceFeatures"))
    (ok (= (* 55 (cffi:foreign-type-size :uint32))
           (cffi:foreign-type-size
            '(:struct lvk::physical-device-features))))
    (ok (< (cffi:foreign-slot-offset
            '(:struct lvk::physical-device-features) 'lvk::shader-float64)
           (cffi:foreign-slot-offset
            '(:struct lvk::physical-device-features) 'lvk::shader-int64)))))

(deftest present-timing-abi-is-explicit-and-inspectable
  (let ((description
          (lvk:vulkan-function-description
           'vk:get-past-presentation-timing-ext)))
    (ok (equal (getf description :foreign-name)
               "vkGetPastPresentationTimingEXT"))
    (ok (equal (mapcar #'first (getf description :arguments))
               '(lvk::device lvk::past-presentation-timing-info
                 lvk::past-presentation-timing-properties))))
  (ok (= 16
         (cffi:foreign-type-size '(:struct lvk::present-stage-time))))
  (ok (= 32
         (cffi:foreign-type-size
          '(:struct lvk::swapchain-timing-properties))))
  (ok (= #x240
         (cffi:foreign-bitfield-value
          'lvk::swapchain-create-flags
          '(:present-id-2-khr :present-timing-ext)))))

(cffi:defcallback test-past-presentation-timing-device-procedure :int32
    ((device :pointer)
     (past-presentation-timing-info :pointer)
     (past-presentation-timing-properties :pointer))
  (declare (ignore device past-presentation-timing-info
                   past-presentation-timing-properties))
  0)

(deftest presentation-timing-commands-use-device-procedure-dispatch
  (let ((original (symbol-function 'lvk::device-procedure))
        (lookups nil))
    (unwind-protect
         (progn
           (setf (symbol-function 'lvk::device-procedure)
                 (lambda (device name)
                   (push (list device name) lookups)
                   (cffi:callback test-past-presentation-timing-device-procedure)))
           (let ((device (cffi:make-pointer 42)))
             (ok (eq :success
                     (vk:get-past-presentation-timing-ext
                      device (cffi:null-pointer) (cffi:null-pointer))))
             (ok (equal `((,device "vkGetPastPresentationTimingEXT"))
                        lookups))))
      (setf (symbol-function 'lvk::device-procedure) original))))

(deftest presentation-timeline-correlates-predictions-with-display-results
  (let ((timeline
          (make-instance 'luv::vulkan-presentation-timeline
                         :stage :image-first-pixel-visible)))
    (setf (luv::vulkan-presentation-timeline-status timeline) :recording
          (luv::vulkan-presentation-timeline-time-domain timeline)
          :clock-monotonic
          (luv::vulkan-presentation-timeline-time-domain-id timeline) 7
          (luv::vulkan-presentation-timeline-refresh-duration timeline)
          16666667
          (luv::vulkan-presentation-timeline-refresh-interval timeline)
          16666667)
    (dotimes (index 3)
      (let ((present-id (1+ index)))
        (luv::note-vulkan-presentation-submission
         timeline present-id (+ 10d0 (/ index 60d0)) (+ 9.99d0 (/ index 60d0)))
        (ok (luv::note-vulkan-presentation-result
             timeline present-id (+ 1000000000 (* index 16666667))
             :clock-monotonic 7))))
    (let* ((snapshot (luv::snapshot-vulkan-presentation-timeline timeline))
           (observations
             (coerce (luv:presentation-timing-snapshot-observations snapshot)
                     'list))
           (intervals
             (luv::presentation-timing-interval-milliseconds observations))
           (drift
             (luv::presentation-timing-phase-errors-milliseconds
              observations)))
      (ok (eq :recording
              (luv:presentation-timing-snapshot-status snapshot)))
      (ok (equal '(1 2 3)
                 (mapcar #'luv:presentation-timing-observation-present-id
                         observations)))
      (ok (= 2 (length intervals)))
      (ok (every (lambda (value) (< (abs (- value 16.666667d0)) 1d-9))
                 intervals))
      (ok (< (abs (car (last drift))) 1d-3)))))

(deftest presentation-prediction-marches-over-native-display-beats
  (let ((timeline
          (make-instance 'luv::vulkan-presentation-timeline
                         :stage :image-first-pixel-visible
                         :absolute-time-p t)))
    (setf (luv::vulkan-presentation-timeline-status timeline) :recording
          (luv::vulkan-presentation-timeline-time-domain timeline)
          :present-stage-local-ext
          (luv::vulkan-presentation-timeline-time-domain-id timeline) 7
          (luv::vulkan-presentation-timeline-refresh-duration timeline)
          16666667)
    ;; Present 7 targeted 1.000 s but appeared one refresh later.  Native
    ;; scheduling catches up from the actual display beat, while the unrelated
    ;; host animation prediction remains untouched.
    (luv::note-vulkan-presentation-submission
     timeline 7 10d0 9.99d0 1000000000)
    (ok (luv::note-vulkan-presentation-result
         timeline 7 1016666667 :present-stage-local-ext 7))
    (multiple-value-bind (host target)
        (luv::predict-vulkan-presentation-target timeline 99d0)
      (ok (= host 99d0))
      (ok (= target 1016666667)))
    (multiple-value-bind (host target)
        (luv::predict-vulkan-presentation-target timeline 100d0)
      (ok (= host 100d0))
      (ok (= target 1016666667))
      (luv::note-vulkan-presentation-submission
       timeline 8 host 99.99d0 target))
    ;; Feedback for 7 may still be the newest result when frame 9 is queued.
    ;; The queued target, not a cross-clock lateness estimate, owns cadence.
    (multiple-value-bind (host target)
        (luv::predict-vulkan-presentation-target timeline 101d0)
      (ok (= host 101d0))
      (ok (= target 1033333334)))
    ;; Frame 8 then misses its target by one whole refresh.  That lateness is
    ;; diagnostic feedback; it must not turn frame 9 into another 33 ms gap.
    (ok (luv::note-vulkan-presentation-result
         timeline 8 1033333334 :present-stage-local-ext 7))
    (multiple-value-bind (host target)
        (luv::predict-vulkan-presentation-target timeline 102d0)
      (ok (= host 102d0))
      (ok (= target 1033333334)))))

(deftest presentation-timeline-ring-retains-only-its-newest-minute
  (let ((timeline
          (make-instance 'luv::vulkan-presentation-timeline
                         :stage :image-first-pixel-out)))
    (loop for present-id from 1
          to (+ luv::+vulkan-presentation-timing-capacity+ 5)
          do (luv::note-vulkan-presentation-submission
              timeline present-id (float present-id 1d0)
              (float present-id 1d0)))
    (let ((observations
            (luv:presentation-timing-snapshot-observations
             (luv::snapshot-vulkan-presentation-timeline timeline))))
      (ok (= luv::+vulkan-presentation-timing-capacity+
             (length observations)))
      (ok (= 6
             (luv:presentation-timing-observation-present-id
              (aref observations 0))))
      (ok (= (+ luv::+vulkan-presentation-timing-capacity+ 5)
             (luv:presentation-timing-observation-present-id
              (aref observations (1- (length observations)))))))))

(deftest real-loader-calls-allocate-events-only-in-an-explicit-trace
  (ok (null (lvk:current-vulkan-trace)))
  (ok (plusp (length (lvk:enumerate-instance-extension-names))))
  (let (trace)
    (lvk:with-vulkan-trace (active-trace)
      (setf trace active-trace)
      (ok (plusp (length (lvk:enumerate-instance-extension-names)))))
    (ok (null (lvk:current-vulkan-trace)))
    (let ((events (lvk:vulkan-trace-events trace)))
      (ok (= (length events) 2))
      (dolist (event events)
        (ok (equal (lvk:vulkan-call-event-foreign-name event)
                   "vkEnumerateInstanceExtensionProperties"))
        (ok (eq (lvk:vulkan-call-event-status event) :returned))))))
