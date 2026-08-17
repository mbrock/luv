;;;; A portal: another luvcraft, running in another process, on a world screen.
;;;;
;;;; The child draws into an IOSurface (see mirror.lisp); this side wraps the
;;;; same surfaces as textures and draws one on a world panel through the
;;;; video screen's quad vertex stage and a fragment stage of its own, the
;;;; :PORTAL-SCREEN role in shaders.lisp, which sets the picture on a matte
;;;; inside the panel and marks it as a transmission.  A small thread keeps
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
   (uniform-buffer :initarg :uniform-buffer :reader luvcraft-portal-uniform-buffer)
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
                      :reader luvcraft-portal-frames-per-second)
   (on-stop :initarg :on-stop :initform nil :reader luvcraft-portal-on-stop))
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
                                                  ,(luvcraft-frame-uniform-buffer frame))
                                       (:binding 3 :resource
                                                  ,(luvcraft-portal-uniform-buffer portal))))))
                 (luvcraft-portal-views portal)))))

(defmethod encode-luvcraft-overlay
    ((portal luvcraft-portal) session pass surface-texture)
  (encode-luvcraft-portal-picture portal session pass surface-texture))

(defun encode-luvcraft-portal-picture (portal session pass surface-texture)
  "Draw PORTAL's panel into the open scene PASS.  Overlays that own a portal
(the terminal wall) call this from their own drawing so their glass goes on
top; a free portal is an overlay itself and calls it from ENCODE-LUVCRAFT-OVERLAY."
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
    ;; The pump itself may be the one releasing us, through ON-STOP.
    (when (and pump (sb-thread:thread-alive-p pump)
               (not (eq pump sb-thread:*current-thread*)))
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
                                    (luvcraft-portal-uniform-buffer portal)
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

(defun luvcraft-portal-uniform-data (picture-rect opened texel-u texel-v)
  "The two vec4s of *PORTAL-UNIFORM-MEMBERS*: picture u0 v0 u1 v1, then the
moment the link opened and the picture's texel size."
  (let ((data (make-array 8 :element-type 'single-float)))
    (loop for value in (append picture-rect (list opened texel-u texel-v 0.0))
          for index from 0
          do (setf (aref data index) (coerce value 'single-float)))
    data))

(defun luvcraft-session-elapsed-seconds (session)
  "The value the frame environment carries as elapsed time, for shaders."
  (mod (or (luvcraft-session-last-frame-time session) 0d0) 3600d0))

(defun open-luvcraft-portal (session &key mirror
                                          (width 640) (height 400)
                                          (distance 6.0) (lift 1.5)
                                          (screen-height 3.0)
                                          rectangle on-stop
                                          (attach-p t)
                                          (frames-per-second 30))
  "Show a child game on a world panel.  With no MIRROR, spawn a child of
WIDTH x HEIGHT points; otherwise MIRROR is a negotiated, ready one (say, from
the portal server).

RECTANGLE, when given, is called with the picture's aspect and returns the
panel's world origin, right edge, and up edge, and optionally a fourth value:
the picture's rectangle inside the panel as (u0 v0 u1 v1), the whole panel by
default.  Without RECTANGLE the picture is its own panel, SCREEN-HEIGHT cells
tall before the camera.

ON-STOP, when given, is called with the portal if its pump stops on its own,
which is to say the child went away.  With ATTACH-P the portal is added to
SESSION's overlays and draws itself; an overlay that owns the portal passes
NIL and draws it through ENCODE-LUVCRAFT-PORTAL-PICTURE.  Returns the portal."
  (let* ((device (luvcraft-session-device session))
         (camera (luvcraft-session-camera session))
         (mirror (or mirror (spawn-luvcraft-mirror :width width :height height)))
         (format (canvas-format (luvcraft-session-context session)))
         (textures #()) (views #()) (sampler nil) (layout nil) (pipeline nil)
         (uniform-buffer nil)
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
                 uniform-buffer (create device (make-buffer-descriptor
                                                :label "luvcraft portal uniform"
                                                :size (* 4 8) :usage '(:uniform)))
                 layout (create device (make-bind-group-layout-descriptor
                                        :label "luvcraft portal layout"
                                        :entries '((:binding 0 :type :texture)
                                                   (:binding 1 :type :sampler)
                                                   (:binding 2 :type :uniform-buffer)
                                                   (:binding 3 :type :uniform-buffer))))
                 vertex-buffer (create device (make-buffer-descriptor
                                               :label "luvcraft portal quad"
                                               :size (* 4 18) :usage '(:vertex :copy-dst)))
                 instance-buffer (create device (make-buffer-descriptor
                                                 :label "luvcraft portal instance"
                                                 :size (* 4 9) :usage '(:vertex :copy-dst))))
           (write-buffer vertex-buffer (make-world-text-quad-vertices))
           (multiple-value-bind (origin right-edge up-edge picture-rect)
               (if rectangle
                   (funcall rectangle aspect)
                   (video-screen-rectangle-before-camera
                    camera distance lift screen-width screen-height))
             (write-buffer instance-buffer
                           (make-video-screen-instances origin right-edge up-edge))
             (write-buffer uniform-buffer
                           (luvcraft-portal-uniform-data
                            (or picture-rect '(0.0 0.0 1.0 1.0))
                            (luvcraft-session-elapsed-seconds session)
                            (/ 1.0 (luvcraft-mirror-width mirror))
                            (/ 1.0 (luvcraft-mirror-height mirror)))))
           (setf pipeline
                 (make-live-shader-pipeline
                  :role :portal-screen :vertex-role :video-screen
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
                                       :uniform-buffer uniform-buffer
                                       :layout layout :pipeline pipeline
                                       :vertex-buffer vertex-buffer
                                       :instance-buffer instance-buffer
                                       :frames-per-second frames-per-second
                                       :on-stop on-stop))
           (setf (luvcraft-portal-pump portal)
                 (sb-thread:make-thread
                  (lambda () (pump-luvcraft-portal portal))
                  :name "luvcraft portal pump"))
           (when attach-p
             (add-luvcraft-overlay session portal))
           portal)
      (unless portal
        (with-release-warnings
          (when pipeline (releasing :portal-pipeline (release-live-shader-pipeline pipeline)))
          (dolist (resource (append (list instance-buffer vertex-buffer uniform-buffer
                                          sampler layout)
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
        (when (luvcraft-portal-running-p portal)
          (warn "The luvcraft portal pump stopped: ~A" condition)
          (setf (luvcraft-portal-running-p portal) nil)
          (alexandria:when-let ((on-stop (luvcraft-portal-on-stop portal)))
            (funcall on-stop portal)))))))

(defun close-luvcraft-portal (session portal)
  "Stop PORTAL and release it, whether or not it was one of SESSION's overlays."
  (if (member portal (luvcraft-session-overlays session))
      (remove-luvcraft-overlay session portal)
      (release-luvcraft-overlay portal)))
