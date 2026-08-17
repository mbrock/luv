;;;; A portal: another luvcraft, running in another process, on a world screen.
;;;;
;;;; The child draws into an IOSurface (see mirror.lisp); this side wraps the
;;;; same surfaces as textures and draws one on a rectangle in front of the
;;;; camera through the video screen's own quad shaders.  A small thread keeps
;;;; asking the child for frames, one ring slot at a time.  The scene shows the
;;;; slot the child finished last; the pump never asks the child to draw into
;;;; that slot or the one shown before it, which a parent frame in flight may
;;;; still be sampling.  So the ring has three slots and nobody tears.

(in-package #:luvcraft)

(defclass luvcraft-portal ()
  ((mirror :initarg :mirror :reader luvcraft-portal-mirror)
   (session :initarg :session :reader luvcraft-portal-session)
   ;; One texture and view per ring slot.
   (textures :initarg :textures :reader luvcraft-portal-textures)
   (views :initarg :views :reader luvcraft-portal-views)
   ;; The slot the scene draws, and the slot it drew before that.
   (shown-slot :initform 0 :accessor luvcraft-portal-shown-slot)
   (previous-slot :initform 0 :accessor luvcraft-portal-previous-slot)
   (sampler :initarg :sampler :reader luvcraft-portal-sampler)
   (layout :initarg :layout :reader luvcraft-portal-layout)
   (pipeline :initarg :pipeline :reader luvcraft-portal-pipeline)
   (vertex-buffer :initarg :vertex-buffer :reader luvcraft-portal-vertex-buffer)
   (instance-buffer :initarg :instance-buffer
                    :reader luvcraft-portal-instance-buffer)
   (frame-bind-groups :initform (make-hash-table :test #'eq)
                      :reader luvcraft-portal-frame-bind-groups)
   (pump :initform nil :accessor luvcraft-portal-pump)
   (running-p :initform t :accessor luvcraft-portal-running-p)
   (frames-per-second :initarg :frames-per-second :initform 30
                      :reader luvcraft-portal-frames-per-second))
  (:documentation "A child game's frames on a rectangle in this game's world."))

(defun luvcraft-portal-frame-bind-groups-for (portal frame)
  "One bind group per ring slot for FRAME, made on first use."
  (or (gethash frame (luvcraft-portal-frame-bind-groups portal))
      (setf (gethash frame (luvcraft-portal-frame-bind-groups portal))
            (map 'vector
                 (lambda (view)
                   (create (luvcraft-session-device (luvcraft-portal-session portal))
                           (make-bind-group-descriptor
                            :label "luvcraft portal frame bindings"
                            :layout (luvcraft-portal-layout portal)
                            :entries `((:binding 0 :resource ,view)
                                       (:binding 1 :resource ,(luvcraft-portal-sampler portal))
                                       (:binding 2 :resource
                                                  ,(luvcraft-frame-uniform-buffer frame))))))
                 (luvcraft-portal-views portal)))))

(defmethod encode-luvcraft-overlay
    ((portal luvcraft-portal) session pass surface-texture)
  (let* ((frame (luvcraft-frame-state session surface-texture))
         (mirror (luvcraft-portal-mirror portal))
         (slot (or (luvcraft-mirror-completed-slot mirror) 0)))
    ;; Publish which slot this frame samples, so the pump steers clear of it
    ;; and of the one the previous frame may still be reading.
    (unless (= slot (luvcraft-portal-shown-slot portal))
      (setf (luvcraft-portal-previous-slot portal) (luvcraft-portal-shown-slot portal)
            (luvcraft-portal-shown-slot portal) slot))
    (set-pipeline pass (live-shader-pipeline-native-pipeline
                        (luvcraft-portal-pipeline portal)))
    (set-vertex-buffer pass 0 (luvcraft-portal-vertex-buffer portal))
    (set-vertex-buffer pass 1 (luvcraft-portal-instance-buffer portal))
    (set-bind-group pass 0 (aref (luvcraft-portal-frame-bind-groups-for portal frame) slot))
    (draw pass 6 1)))

(defun luvcraft-portal-free-slot (portal)
  "A ring slot no parent frame is showing, was showing a frame ago, or is
about to show (the child's last completed slot); NIL when the ring is full,
which means the parent has not yet caught up and the pump should wait."
  (let* ((mirror (luvcraft-portal-mirror portal))
         (shown (luvcraft-portal-shown-slot portal))
         (previous (luvcraft-portal-previous-slot portal))
         (completed (luvcraft-mirror-completed-slot mirror))
         (count (luvcraft-mirror-slot-count mirror)))
    (loop for candidate from (1+ shown)
          for slot = (mod candidate count)
          repeat count
          unless (or (= slot shown) (= slot previous) (eql slot completed))
            return slot)))

(defmethod luvcraft-overlay-live-shader-pipelines ((portal luvcraft-portal))
  (list (luvcraft-portal-pipeline portal)))

(defmethod release-luvcraft-overlay ((portal luvcraft-portal))
  (setf (luvcraft-portal-running-p portal) nil)
  (let ((pump (luvcraft-portal-pump portal)))
    (when (and pump (sb-thread:thread-alive-p pump))
      (sb-thread:join-thread pump :default nil)))
  (stop-luvcraft-mirror (luvcraft-portal-mirror portal))
  (with-release-warnings
    (loop for groups being the hash-values of (luvcraft-portal-frame-bind-groups portal)
          do (loop for group across groups
                   do (releasing :portal-bind-group (destroy group))))
    (clrhash (luvcraft-portal-frame-bind-groups portal))
    (releasing :portal-pipeline
      (release-live-shader-pipeline (luvcraft-portal-pipeline portal)))
    (dolist (resource (append (list (luvcraft-portal-instance-buffer portal)
                                    (luvcraft-portal-vertex-buffer portal)
                                    (luvcraft-portal-sampler portal)
                                    (luvcraft-portal-layout portal))
                              (coerce (luvcraft-portal-views portal) 'list)
                              (coerce (luvcraft-portal-textures portal) 'list)))
      (releasing :portal-resource (destroy resource))))
  (values))

(defun adopt-luvcraft-mirror-texture (device mirror slot format)
  "This process's texture over MIRROR's surface SLOT, for sampling."
  (let* ((surface (luvcraft-mirror-surface mirror slot))
         (native (luv.metal:new-metal-texture-for-iosurface
                  (luv::metal-native-object device) surface
                  (luv::metal-resource-pixel-format format nil)
                  luv.metal:+texture-usage-shader-read+
                  :label "luvcraft portal picture")))
    (adopt-native-texture
     device native (luv.objective-c:objective-c-pointer native)
     (make-texture-descriptor
      :label "luvcraft portal picture"
      :size (list (luvcraft-mirror-width mirror) (luvcraft-mirror-height mirror))
      :dimensions :2d :format format :usage '(:texture-binding)))))

(defun open-luvcraft-portal (session &key (width 640) (height 400)
                                          (distance 6.0) (lift 1.5)
                                          (screen-height 3.0)
                                          (frames-per-second 30))
  "Spawn a child game and hang its picture SCREEN-HEIGHT cells tall before the
camera.  Returns the portal, already added to SESSION's overlays."
  (let* ((device (luvcraft-session-device session))
         (camera (luvcraft-session-camera session))
         (mirror (spawn-luvcraft-mirror :width width :height height))
         (format (canvas-format (luvcraft-session-context session)))
         (textures #()) (views #()) (sampler nil) (layout nil) (pipeline nil)
         (vertex-buffer nil) (instance-buffer nil) (portal nil))
    (unwind-protect
         (let* ((aspect (/ (luvcraft-mirror-width mirror)
                           (luvcraft-mirror-height mirror)))
                (screen-width (* screen-height aspect)))
           (setf textures (coerce (loop for slot below (luvcraft-mirror-slot-count mirror)
                                        collect (adopt-luvcraft-mirror-texture
                                                 device mirror slot format))
                                  'vector)
                 views (map 'vector
                            (lambda (texture)
                              (create device (make-texture-view-descriptor
                                              :texture texture)))
                            textures)
                 sampler (create device (make-sampler-descriptor
                                         :label "luvcraft portal sampler"
                                         :mag-filter :linear :min-filter :linear
                                         :mipmap-filter :nearest))
                 layout (create device (make-bind-group-layout-descriptor
                                        :label "luvcraft portal layout"
                                        :entries '((:binding 0 :type :texture)
                                                   (:binding 1 :type :sampler)
                                                   (:binding 2 :type :uniform-buffer))))
                 vertex-buffer (create device (make-buffer-descriptor
                                               :label "luvcraft portal quad"
                                               :size (* 4 18) :usage '(:vertex :copy-dst)))
                 instance-buffer (create device (make-buffer-descriptor
                                                 :label "luvcraft portal instance"
                                                 :size (* 4 9) :usage '(:vertex :copy-dst))))
           (write-buffer vertex-buffer (make-world-text-quad-vertices))
           (multiple-value-bind (origin right-edge up-edge)
               (video-screen-rectangle-before-camera
                camera distance lift screen-width screen-height)
             (write-buffer instance-buffer
                           (make-video-screen-instances origin right-edge up-edge)))
           (setf pipeline
                 (make-live-shader-pipeline
                  :role :video-screen :vertex-role :video-screen
                  :label "luvcraft portal pipeline"
                  :device device :layout layout
                  :vertex-buffers
                  '((:array-stride 12
                     :attributes ((:shader-location 0 :offset 0 :format :float32x3)))
                    (:array-stride 36 :step-mode :instance
                     :attributes ((:shader-location 1 :offset 0 :format :float32x3)
                                  (:shader-location 2 :offset 12 :format :float32x3)
                                  (:shader-location 3 :offset 24 :format :float32x3))))
                  :target-format +luvcraft-scene-color-format+
                  :primitive '(:topology :triangle-list)
                  :depth-stencil '(:format :depth32-float
                                   :depth-write-enabled t
                                   :depth-compare :less)))
           (setf portal (make-instance 'luvcraft-portal
                                       :mirror mirror :session session
                                       :textures textures :views views :sampler sampler
                                       :layout layout :pipeline pipeline
                                       :vertex-buffer vertex-buffer
                                       :instance-buffer instance-buffer
                                       :frames-per-second frames-per-second))
           (setf (luvcraft-portal-pump portal)
                 (sb-thread:make-thread
                  (lambda () (pump-luvcraft-portal portal))
                  :name "luvcraft portal pump"))
           (add-luvcraft-overlay session portal)
           portal)
      (unless portal
        (with-release-warnings
          (when pipeline (releasing :portal-pipeline (release-live-shader-pipeline pipeline)))
          (dolist (resource (append (list instance-buffer vertex-buffer sampler layout)
                                    (coerce views 'list) (coerce textures 'list)))
            (when resource (releasing :portal-resource (destroy resource))))
          (releasing :portal-mirror (stop-luvcraft-mirror mirror)))))))

(defun pump-luvcraft-portal (portal)
  "Ask the child for frames at the portal's rate until the portal is released."
  (let ((period (/ 1.0 (luvcraft-portal-frames-per-second portal))))
    (handler-case
        (loop while (luvcraft-portal-running-p portal)
              do (let ((start (get-internal-real-time))
                       (slot (luvcraft-portal-free-slot portal)))
                   (cond
                     (slot
                      (request-luvcraft-mirror-frame (luvcraft-portal-mirror portal) slot)
                      (let ((elapsed (/ (- (get-internal-real-time) start)
                                        (float internal-time-units-per-second))))
                        (when (< elapsed period)
                          (sleep (- period elapsed)))))
                     ;; Every slot is spoken for until the parent draws again.
                     (t (sleep 0.001)))))
      (error (condition)
        (warn "The luvcraft portal pump stopped: ~A" condition)))))

(defun close-luvcraft-portal (session portal)
  (remove-luvcraft-overlay session portal))
