(in-package #:luvcraft.tests)

(defclass video-interop-test-device ()
  ((events :initarg :events :reader video-interop-test-device-events)
   (next-plane :initform nil :accessor video-interop-test-device-next-plane)
   (view-release-failures
    :initarg :view-release-failures :initform 0
    :accessor video-interop-test-device-view-release-failures)))

(defclass video-interop-test-resource ()
  ((events :initarg :events :reader video-interop-test-resource-events)
   (owner :initarg :owner :reader video-interop-test-resource-owner)
   (kind :initarg :kind :reader video-interop-test-resource-kind)
   (plane :initarg :plane :reader video-interop-test-resource-plane)
   (failures :initarg :failures :initform 0
             :accessor video-interop-test-resource-failures)))

(defmethod destroy ((resource video-interop-test-resource))
  (vector-push-extend
   (list :destroy
         (video-interop-test-resource-owner resource)
         (video-interop-test-resource-kind resource)
         (video-interop-test-resource-plane resource))
   (video-interop-test-resource-events resource))
  (when (plusp (video-interop-test-resource-failures resource))
    (decf (video-interop-test-resource-failures resource))
    (error "Injected decoded-video resource release failure."))
  (values))

(defmethod create ((device video-interop-test-device) descriptor)
  (declare (ignore descriptor))
  (make-instance
   'video-interop-test-resource
   :events (video-interop-test-device-events device)
   :owner :candidate :kind :view
   :plane (video-interop-test-device-next-plane device)
   :failures
   (prog1 (video-interop-test-device-view-release-failures device)
     (setf (video-interop-test-device-view-release-failures device) 0))))

(defclass video-interop-test-importer (luvcraft::video-frame-importer)
  ((fail-plane :initarg :fail-plane :initform nil
               :reader video-interop-test-importer-fail-plane)))

(defmethod luvcraft::adopt-decoded-video-frame
    ((importer video-interop-test-importer) frame width height)
  (declare (ignore frame width height))
  (let ((device (luvcraft::video-frame-importer-device importer)))
    (luvcraft::make-decoded-video-picture-from-planes
     device 2
     (lambda (plane)
       (when (eql plane (video-interop-test-importer-fail-plane importer))
         (error "Injected decoded-video plane ~D failure." plane))
       (setf (video-interop-test-device-next-plane device) plane)
       (make-instance
        'video-interop-test-resource
        :events (video-interop-test-device-events device)
        :owner :candidate :kind :texture :plane plane)))))

;;; A backend-free model of the HAL's logical/native retirement split.  These
;;; resources disappear from a picture as soon as DESTROY transfers their
;;; teardown into the device ledger; native failure cannot return ownership to
;;; that now-invalid wrapper.

(defclass video-interop-retirement-test-device
    (video-interop-test-device)
  ((retirement-ledger
    :initform (luv::make-gpu-retirement-ledger)
    :reader video-interop-retirement-test-device-ledger)
   (resources
    :initform nil
    :accessor video-interop-retirement-test-device-resources)
   (view-fail-plane
    :initarg :view-fail-plane :initform nil
    :reader video-interop-retirement-test-device-view-fail-plane)
   (texture-native-failures
    :initarg :texture-native-failures :initform 0
    :reader video-interop-retirement-test-device-texture-native-failures)))

(defclass video-interop-retirement-test-resource
    (video-interop-test-resource)
  ((device
    :initarg :device
    :reader video-interop-retirement-test-resource-device)
   (native-owner-release
    :initarg :native-owner-release :initform nil
    :reader video-interop-retirement-test-resource-native-owner-release)
   (native-failures
    :initarg :native-failures :initform 0
    :accessor video-interop-retirement-test-resource-native-failures)
   (destroyed-p
    :initform nil
    :accessor video-interop-retirement-test-resource-destroyed-p)))

(defun note-video-interop-retirement-event (resource stage)
  (vector-push-extend
   (list stage
         (video-interop-test-resource-kind resource)
         (video-interop-test-resource-plane resource))
   (video-interop-test-resource-events resource)))

(defmethod destroy ((resource video-interop-retirement-test-resource))
  (unless (video-interop-retirement-test-resource-destroyed-p resource)
    (let ((device (video-interop-retirement-test-resource-device resource)))
      (luv::transfer-gpu-retirement
       (video-interop-retirement-test-device-ledger device)
       resource 0
       (lambda ()
         (note-video-interop-retirement-event resource :native)
         (when (plusp
                (video-interop-retirement-test-resource-native-failures
                 resource))
           (decf
            (video-interop-retirement-test-resource-native-failures resource))
           (error "Injected native decoded-video retirement failure."))
         (when (video-interop-retirement-test-resource-native-owner-release
                resource)
           (funcall
            (video-interop-retirement-test-resource-native-owner-release
             resource))))
       (lambda ()
         (note-video-interop-retirement-event resource :logical)
         (setf (video-interop-retirement-test-resource-destroyed-p resource)
               t)))))
  (values))

(defmethod create
    ((device video-interop-retirement-test-device) descriptor)
  (declare (ignore descriptor))
  (let ((plane (video-interop-test-device-next-plane device)))
    (when (eql plane
               (video-interop-retirement-test-device-view-fail-plane device))
      (error "Injected decoded-video view ~D construction failure." plane))
    (let ((resource
            (make-instance
             'video-interop-retirement-test-resource
             :device device
             :events (video-interop-test-device-events device)
             :owner :candidate :kind :view :plane plane)))
      (push resource
            (video-interop-retirement-test-device-resources device))
      resource)))

(defclass video-interop-retirement-test-importer
    (luvcraft::video-frame-importer)
  ((events :initarg :events :reader video-interop-retirement-test-importer-events)
   (fail-plane :initarg :fail-plane :initform nil
               :reader video-interop-retirement-test-importer-fail-plane)
   (native-state-failures
    :initarg :native-state-failures :initform 0
    :accessor video-interop-retirement-test-importer-native-state-failures)))

(defmethod luvcraft::release-video-frame-importer-native-state
    ((importer video-interop-retirement-test-importer))
  (vector-push-extend
   '(:native :importer nil)
   (video-interop-retirement-test-importer-events importer))
  (when (plusp
         (video-interop-retirement-test-importer-native-state-failures
          importer))
    (decf
     (video-interop-retirement-test-importer-native-state-failures importer))
    (error "Injected video importer native-state release failure."))
  (values))

(defmethod luvcraft::adopt-decoded-video-frame
    ((importer video-interop-retirement-test-importer) frame width height)
  (declare (ignore frame width height))
  (let ((device (luvcraft::video-frame-importer-device importer)))
    (luvcraft::make-decoded-video-picture-from-planes
     device 2
     (lambda (plane)
       (when (eql plane
                  (video-interop-retirement-test-importer-fail-plane importer))
         (error "Injected decoded-video plane ~D construction failure." plane))
       (setf (video-interop-test-device-next-plane device) plane)
       (let* ((events (video-interop-test-device-events device))
              (release-owner
                (luvcraft::make-video-frame-importer-owner-release
                 importer
                 (lambda ()
                   (vector-push-extend
                    (list :native-owner :texture plane) events))))
              (resource
                (make-instance
                 'video-interop-retirement-test-resource
                 :device device :events events
                 :owner :candidate :kind :texture :plane plane
                 :native-owner-release release-owner
                 :native-failures
                 (if (zerop plane)
                     (video-interop-retirement-test-device-texture-native-failures
                      device)
                     0))))
         (push resource
               (video-interop-retirement-test-device-resources device))
         resource)))))

(defun maintain-video-interop-retirement-test-device (device)
  (handler-bind ((luv::gpu-native-retirement-warning #'muffle-warning))
    (luv::maintain-gpu-retirement-ledger
     (video-interop-retirement-test-device-ledger device) 0
     :operation :video-interop-test)))

(defmethod luv::retire-gpu-native-owner
    ((device video-interop-retirement-test-device)
     owner teardown invalidate)
  ;; Model the live backend contract: transfer before logical invalidation,
  ;; then make one immediate maintenance attempt.  Recursive entry from a
  ;; plane callback leaves this newly enqueued owner for the next outer pass.
  (luv::transfer-gpu-retirement
   (video-interop-retirement-test-device-ledger device)
   owner 0 teardown invalidate)
  (maintain-video-interop-retirement-test-device device)
  owner)

(defun make-video-interop-test-events ()
  (make-array 0 :adjustable t :fill-pointer 0))

(defun make-video-interop-test-picture
    (events owner plane-count &key (view-failures 0))
  (make-instance
   'luvcraft::decoded-video-picture
   :textures
   (loop for plane below plane-count
         collect (make-instance
                  'video-interop-test-resource
                  :events events :owner owner :kind :texture :plane plane))
   :views
   (loop for plane below plane-count
         collect (make-instance
                  'video-interop-test-resource
                  :events events :owner owner :kind :view :plane plane
                  :failures (if (zerop plane) view-failures 0)))))

(defun make-video-interop-test-screen (importer picture)
  (make-instance
   'luvcraft::video-screen
   :video nil :width 64 :height 32
   :importer importer :picture picture :hardware-p t
   :sampler nil :layout nil :pipeline nil
   :vertex-buffer nil :instance-buffer nil :resources nil))

(deftest decoded-video-second-plane-failure-rolls-back-without-publication
  (let* ((events (make-video-interop-test-events))
         (device (make-instance 'video-interop-test-device :events events))
         (importer
           (make-instance 'video-interop-test-importer
                          :device device :fail-plane 1))
         (previous (make-video-interop-test-picture events :previous 2))
         (screen (make-video-interop-test-screen importer previous)))
    (ok (signals (luvcraft::install-hardware-video-picture screen :frame)
                 'error))
    (ok (eq previous (luvcraft::video-screen-picture screen)))
    (ok (null (luvcraft::video-screen-retired-pictures screen)))
    (ok (equal '((:destroy :candidate :view 0)
                 (:destroy :candidate :texture 0))
               (coerce events 'list)))))

(deftest failed-picture-construction-release-enters-a-retryable-backlog
  (let* ((luvcraft::*decoded-video-picture-release-backlog* nil)
         (events (make-video-interop-test-events))
         (device
           (make-instance
            'video-interop-test-device
            :events events :view-release-failures 1))
         (importer
           (make-instance 'video-interop-test-importer
                          :device device :fail-plane 1)))
    (handler-bind ((warning #'muffle-warning))
      (ok (signals
           (luvcraft::adopt-decoded-video-frame importer :frame 64 32)
           'error)))
    (ok (= 1 (length luvcraft::*decoded-video-picture-release-backlog*)))
    (let ((picture
            (first luvcraft::*decoded-video-picture-release-backlog*)))
      (ok (not (luvcraft::decoded-video-picture-released-p picture)))
      (luvcraft::retry-decoded-video-picture-release-backlog)
      (ok (null luvcraft::*decoded-video-picture-release-backlog*))
      (ok (luvcraft::decoded-video-picture-released-p picture)))
    (ok (equal '((:destroy :candidate :view 0)
                 (:destroy :candidate :view 0)
                 (:destroy :candidate :texture 0))
               (coerce events 'list)))))

(deftest decoded-video-picture-releases-every-view-before-any-texture
  (let* ((events (make-video-interop-test-events))
         (picture (make-video-interop-test-picture events :picture 2)))
    (luv:with-release-report
      (luvcraft::release-decoded-video-picture picture))
    (ok (equal '((:destroy :picture :view 0)
                 (:destroy :picture :view 1)
                 (:destroy :picture :texture 0)
                 (:destroy :picture :texture 1))
               (coerce events 'list)))
    (ok (luvcraft::decoded-video-picture-released-p picture))))

(deftest failed-published-picture-retirement-remains-screen-owned
  (let* ((events (make-video-interop-test-events))
         (device (make-instance 'video-interop-test-device :events events))
         (importer
           (make-instance 'video-interop-test-importer :device device))
         (previous
           (make-video-interop-test-picture
            events :previous 1 :view-failures 1))
         (screen (make-video-interop-test-screen importer previous)))
    (handler-bind ((warning #'muffle-warning))
      (luvcraft::install-hardware-video-picture screen :frame))
    (ok (not (eq previous (luvcraft::video-screen-picture screen))))
    (ok (equal (list previous)
               (luvcraft::video-screen-retired-pictures screen)))
    (ok (equal '((:destroy :previous :view 0))
               (coerce events 'list)))
    (luvcraft::retry-video-screen-retired-pictures screen)
    (ok (null (luvcraft::video-screen-retired-pictures screen)))
    (ok (equal '((:destroy :previous :view 0)
                 (:destroy :previous :view 0)
                 (:destroy :previous :texture 0))
               (coerce events 'list)))))

(deftest terminal-film-stop-retains-a-screen-until-logical-release-succeeds
  (let* ((events (make-video-interop-test-events))
         (bind-group
           (make-instance
            'video-interop-test-resource
            :events events :owner :screen :kind :bind-group :plane 0
            :failures 1))
         (resource
           (make-instance
            'video-interop-test-resource
            :events events :owner :screen :kind :buffer :plane 0))
         (screen (make-video-interop-test-screen nil nil))
         (display (allocate-instance (find-class 'terminal-display)))
         (session (allocate-instance (find-class 'luvcraft-session))))
    (setf (luvcraft::video-screen-bind-group screen) bind-group
          (luvcraft::video-screen-resources screen) (list resource)
          (terminal-display-film-screen display) screen
          (luvcraft::luvcraft-session-video-screen session) screen)
    (ok (signals (luvcraft::stop-terminal-display-film display session)
                 'luv:release-error))
    (ok (eq screen (terminal-display-film-screen display)))
    (ok (eq screen (luvcraft::luvcraft-session-video-screen session)))
    (ok (eq bind-group (luvcraft::video-screen-bind-group screen)))
    (ok (null (luvcraft::video-screen-resources screen)))
    (ok (not (luvcraft::video-screen-released-p screen)))
    (luvcraft::stop-terminal-display-film display session)
    (ok (null (terminal-display-film-screen display)))
    (ok (null (luvcraft::luvcraft-session-video-screen session)))
    (ok (luvcraft::video-screen-released-p screen))
    (ok (equal '((:destroy :screen :bind-group 0)
                 (:destroy :screen :buffer 0)
                 (:destroy :screen :bind-group 0))
               (coerce events 'list)))))

(deftest failed-startup-screen-release-enters-a-retryable-backlog
  (let* ((luvcraft::*video-screen-release-backlog* nil)
         (events (make-video-interop-test-events))
         (resource
           (make-instance
            'video-interop-test-resource
            :events events :owner :startup :kind :buffer :plane 0
            :failures 1))
         (screen (make-video-interop-test-screen nil nil)))
    (setf (luvcraft::video-screen-resources screen) (list resource))
    (handler-bind ((warning #'muffle-warning))
      (luv:with-release-warnings
        (luvcraft::release-video-screen-or-retain screen)))
    (ok (equal (list screen) luvcraft::*video-screen-release-backlog*))
    (ok (not (luvcraft::video-screen-released-p screen)))
    (luvcraft::retry-video-screen-release-backlog)
    (ok (null luvcraft::*video-screen-release-backlog*))
    (ok (luvcraft::video-screen-released-p screen))
    (ok (equal '((:destroy :startup :buffer 0)
                 (:destroy :startup :buffer 0))
               (coerce events 'list)))))

(deftest failed-native-plane-retirement-keeps-importer-until-ledger-retry
  (let* ((events (make-video-interop-test-events))
         (device
           (make-instance
            'video-interop-retirement-test-device
            :events events :texture-native-failures 1))
         (importer
           (make-instance
            'video-interop-retirement-test-importer
            :device device :events events :fail-plane 1)))
    (handler-bind ((warning #'muffle-warning))
      (ok (signals
           (luvcraft::adopt-decoded-video-frame importer :frame 64 32)
           'error)))
    ;; The failed candidate owns no retryable wrapper.  Logical invalidation
    ;; succeeded, and the backend ledger is now the durable native owner.
    (ok (every #'video-interop-retirement-test-resource-destroyed-p
               (video-interop-retirement-test-device-resources device)))
    (ok (= 2
           (length
            (luv::gpu-retirement-ledger-entries
             (video-interop-retirement-test-device-ledger device)))))
    (ok (= 1
           (luvcraft::video-frame-importer-native-owner-retainers importer)))
    (luvcraft::release-video-frame-importer importer)
    (ok (eq :requested
            (luvcraft::video-frame-importer-release-state importer)))
    ;; View retirement succeeds; plane 0's native teardown does not.  The
    ;; texture entry and its importer retainer both survive the attempt.
    (maintain-video-interop-retirement-test-device device)
    (ok (= 1
           (length
            (luv::gpu-retirement-ledger-entries
             (video-interop-retirement-test-device-ledger device)))))
    (ok (= 1
           (luvcraft::video-frame-importer-native-owner-retainers importer)))
    (ok (eq :requested
            (luvcraft::video-frame-importer-release-state importer)))
    (ok (not (member '(:native-owner :texture 0)
                     (coerce events 'list) :test #'equal)))
    ;; Maintenance retries the durable texture entry.  Its owner succeeds and
    ;; drops the final retainer, but importer retirement is deliberately a new
    ;; FIFO entry rather than part of this texture callback's success.
    (maintain-video-interop-retirement-test-device device)
    (ok (= 1
           (length
            (luv::gpu-retirement-ledger-entries
             (video-interop-retirement-test-device-ledger device)))))
    (ok (zerop
         (luvcraft::video-frame-importer-native-owner-retainers importer)))
    (ok (eq :queued
            (luvcraft::video-frame-importer-release-state importer)))
    (ok (not (member '(:native :importer nil)
                     (coerce events 'list) :test #'equal)))
    ;; The next pass owns only the importer and closes it independently.
    (maintain-video-interop-retirement-test-device device)
    (ok (null
         (luv::gpu-retirement-ledger-entries
          (video-interop-retirement-test-device-ledger device))))
    (ok (eq :released
            (luvcraft::video-frame-importer-release-state importer)))
    (ok (equal
         '((:logical :view 0)
           (:logical :texture 0)
           (:native :view 0)
           (:native :texture 0)
           (:native :texture 0)
           (:native-owner :texture 0)
           (:native :importer nil))
         (coerce events 'list)))))

(deftest importer-native-failure-retries-without-releasing-plane-owner-twice
  (let* ((events (make-video-interop-test-events))
         (device
           (make-instance
            'video-interop-retirement-test-device :events events))
         (importer
           (make-instance
            'video-interop-retirement-test-importer
            :device device :events events :fail-plane 1
            :native-state-failures 1)))
    (handler-bind ((warning #'muffle-warning))
      (ok (signals
           (luvcraft::adopt-decoded-video-frame importer :frame 64 32)
           'error)))
    (luvcraft::release-video-frame-importer importer)
    ;; The plane and its owner retire first.  The last callback enqueues a
    ;; separate importer entry with a consumed (zero) retainer; it does not run
    ;; importer-native closure recursively inside texture retirement.
    (maintain-video-interop-retirement-test-device device)
    (ok (= 1
           (length
            (luv::gpu-retirement-ledger-entries
             (video-interop-retirement-test-device-ledger device)))))
    (ok (zerop
         (luvcraft::video-frame-importer-native-owner-retainers importer)))
    (ok (eq :queued
            (luvcraft::video-frame-importer-release-state importer)))
    (ok (= 1 (count '(:native-owner :texture 0)
                    (coerce events 'list) :test #'equal)))
    (ok (zerop (count '(:native :importer nil)
                      (coerce events 'list) :test #'equal)))
    ;; The importer entry now runs by itself and fails without returning
    ;; ownership to the already successful plane callback.
    (maintain-video-interop-retirement-test-device device)
    (ok (= 1
           (length
            (luv::gpu-retirement-ledger-entries
             (video-interop-retirement-test-device-ledger device)))))
    (ok (eq :queued
            (luvcraft::video-frame-importer-release-state importer)))
    (ok (= 1 (count '(:native-owner :texture 0)
                    (coerce events 'list) :test #'equal)))
    (ok (= 1 (count '(:native :importer nil)
                    (coerce events 'list) :test #'equal)))
    ;; Retrying the importer entry cannot revisit plane teardown or its owner.
    (maintain-video-interop-retirement-test-device device)
    (ok (null
         (luv::gpu-retirement-ledger-entries
          (video-interop-retirement-test-device-ledger device))))
    (ok (eq :released
            (luvcraft::video-frame-importer-release-state importer)))
    (ok (= 1 (count '(:native-owner :texture 0)
                    (coerce events 'list) :test #'equal)))
    (ok (= 2 (count '(:native :importer nil)
                    (coerce events 'list) :test #'equal)))))

(deftest zero-plane-unwind-transfers-importer-before-local-ownership-drops
  (let* ((events (make-video-interop-test-events))
         (device
           (make-instance
            'video-interop-retirement-test-device :events events)))
    ;; Model failure before the first plane is adopted.  The constructor's
    ;; unwind requests release and then drops its only lexical importer owner.
    (ok (signals
         (flet ((construct-and-fail-before-first-plane ()
                  (let ((importer
                          (make-instance
                           'video-interop-retirement-test-importer
                           :device device :events events
                           :native-state-failures 1)))
                    (unwind-protect
                         (error "Injected zero-plane video construction failure.")
                      (luvcraft::release-video-frame-importer importer)))))
           (construct-and-fail-before-first-plane))
         'error))
    (ok (null (video-interop-retirement-test-device-resources device)))
    ;; Recover IMPORTER only through the durable ledger entry.  Its immediate
    ;; native close failed after transfer, so no vanished constructor local is
    ;; needed to retry it.
    (let* ((entries
             (luv::gpu-retirement-ledger-entries
              (video-interop-retirement-test-device-ledger device)))
           (importer
             (luv::gpu-retirement-entry-resource (first entries))))
      (ok (= 1 (length entries)))
      (ok (typep importer 'video-interop-retirement-test-importer))
      (ok (zerop
           (luvcraft::video-frame-importer-native-owner-retainers importer)))
      (ok (eq :queued
              (luvcraft::video-frame-importer-release-state importer)))
      (ok (signals
           (luvcraft::make-video-frame-importer-owner-release
            importer (lambda () (values)))
           'error))
      (ok (= 1 (count '(:native :importer nil)
                      (coerce events 'list) :test #'equal)))
      (maintain-video-interop-retirement-test-device device)
      (ok (null
           (luv::gpu-retirement-ledger-entries
            (video-interop-retirement-test-device-ledger device))))
      (ok (eq :released
              (luvcraft::video-frame-importer-release-state importer)))
      (ok (= 2 (count '(:native :importer nil)
                      (coerce events 'list) :test #'equal))))))

(deftest later-view-construction-failure-retires-both-importer-owners
  (let* ((events (make-video-interop-test-events))
         (device
           (make-instance
            'video-interop-retirement-test-device
            :events events :view-fail-plane 1))
         (importer
           (make-instance
            'video-interop-retirement-test-importer
            :device device :events events)))
    (handler-bind ((warning #'muffle-warning))
      (ok (signals
           (luvcraft::adopt-decoded-video-frame importer :frame 64 32)
           'error)))
    (ok (= 2
           (luvcraft::video-frame-importer-native-owner-retainers importer)))
    (luvcraft::release-video-frame-importer importer)
    (maintain-video-interop-retirement-test-device device)
    (ok (= 1
           (length
            (luv::gpu-retirement-ledger-entries
             (video-interop-retirement-test-device-ledger device)))))
    (ok (zerop
         (luvcraft::video-frame-importer-native-owner-retainers importer)))
    (ok (eq :queued
            (luvcraft::video-frame-importer-release-state importer)))
    (maintain-video-interop-retirement-test-device device)
    (ok (null
         (luv::gpu-retirement-ledger-entries
          (video-interop-retirement-test-device-ledger device))))
    (ok (eq :released
            (luvcraft::video-frame-importer-release-state importer)))
    (ok (= 1 (count '(:native-owner :texture 0)
                    (coerce events 'list) :test #'equal)))
    (ok (= 1 (count '(:native-owner :texture 1)
                    (coerce events 'list) :test #'equal)))
    (ok (equal '(:native :importer nil)
               (aref events (1- (length events)))))))
