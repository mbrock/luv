;;; A decoded video playing on a rectangle in the block world.
;;;
;;; The screen is the plainest drawable luvcraft has: one instanced quad, one
;;; sampled texture, no shading model.  Everything interesting happens between
;;; frames rather than inside them -- a decoder is a clock, and the question is
;;; what to do when the wall clock and the film's clock disagree.
;;;
;;; The answer here is the simple one: catch up by decoding, never by waiting.
;;; ADVANCE-VIDEO-SCREEN decodes as many pictures as have come due since the
;;; last frame and uploads only the last of them, so a slow render drops
;;; pictures instead of falling behind, and a fast render uploads nothing.
;;; That is what makes playback keep time with the world rather than with the
;;; frame rate the world happens to be running at.
;;;
;;; This is the software decode path: libav hands back planes in ordinary
;;; memory, swscale converts them to the same packed RGBA words the block
;;; atlas uses, and the queue uploads them.  Hardware decode would replace
;;; the upload with an import of a platform surface and leave everything else
;;; here alone.

(in-package #:luvcraft)

(defclass video-screen ()
  ((video :initarg :video :reader video-screen-video)
   (width :initarg :width :reader video-screen-texture-width)
   (height :initarg :height :reader video-screen-texture-height)
   ;; The RGBA words swscale writes into, reused for every picture.
   (words :initarg :words :reader video-screen-words)
   (texture :initarg :texture :reader video-screen-texture)
   (view :initarg :view :reader video-screen-view)
   (sampler :initarg :sampler :reader video-screen-sampler)
   (layout :initarg :layout :reader video-screen-layout)
   (pipeline :initarg :pipeline :reader video-screen-pipeline)
   (vertex-buffer :initarg :vertex-buffer :reader video-screen-vertex-buffer)
   (instance-buffer :initarg :instance-buffer
                    :reader video-screen-instance-buffer)
   ;; Playback state.  START is the internal real time the film began at, and
   ;; SHOWN is the index of the picture currently on the texture.
   (start :initform nil :accessor video-screen-start)
   (shown :initform -1 :accessor video-screen-shown)
   (loop-p :initarg :loop-p :initform t :reader video-screen-loop-p)
   (resources :initarg :resources :accessor video-screen-resources))
  (:documentation "A video file playing on one world rectangle."))

(defparameter *video-screen-texture-width* 512
  "The width every picture is scaled to before upload.

A fixed size keeps one texture and one swscale context alive for the whole
film, and costs nothing a video wall would notice: the screen is a few metres
of world at most.")

(defun video-screen-texture-size (video)
  "Return the texture width and height for VIDEO, preserving its aspect."
  (let* ((width *video-screen-texture-width*)
         (height (max 1 (round (* width (/ (libav:video-height video)
                                           (libav:video-width video)))))))
    ;; Keep the height even: swscale is happier, and a video is never an odd
    ;; number of lines in practice anyway.
    (values width (if (oddp height) (1+ height) height))))

(defun make-video-screen-instances (origin right-edge up-edge)
  "Return the one 36-byte instance record describing the screen rectangle."
  (let ((data (make-array 9 :element-type 'single-float)))
    (loop for value in (list (vec3-x origin) (vec3-y origin) (vec3-z origin)
                             (vec3-x right-edge) (vec3-y right-edge)
                             (vec3-z right-edge)
                             (vec3-x up-edge) (vec3-y up-edge)
                             (vec3-z up-edge))
          for index from 0
          do (setf (aref data index) (coerce value 'single-float)))
    data))

(defun video-screen-rectangle-before-camera (camera distance lift width height)
  "Return the lower-left corner and edge vectors of a screen facing CAMERA.

The screen hangs DISTANCE ahead of the camera and LIFT above its eye, sized
WIDTH by HEIGHT in cells, square to the camera's own basis."
  (multiple-value-bind (right up forward) (camera-basis camera)
    (let* ((center (world-text-point (camera-position camera)
                                     forward up distance lift 1.0))
           (origin (world-text-point center right up
                                     (- (/ width 2.0)) (- (/ height 2.0))
                                     1.0))
           (right-edge (make-vec3 (* width (vec3-x right))
                                  (* width (vec3-y right))
                                  (* width (vec3-z right))))
           (up-edge (make-vec3 (* height (vec3-x up))
                               (* height (vec3-y up))
                               (* height (vec3-z up)))))
      (values origin right-edge up-edge))))

(defun make-video-screen
    (device camera pathname target-format
     &key (distance 12.0) (lift 4.0) (height 4.5) (loop-p t))
  "Open PATHNAME and build a world screen playing it, facing CAMERA.

HEIGHT is the screen's height in cells; its width follows the film's aspect."
  (let* ((video (libav:open-video pathname))
         (resources nil)
         (pipeline nil)
         (completed-p nil))
    (flet ((keep (resource) (push resource resources) resource))
      (unwind-protect
           (multiple-value-bind (texture-width texture-height)
               (video-screen-texture-size video)
             (let* ((aspect (/ (libav:video-width video)
                               (libav:video-height video)))
                    (width (* height aspect))
                    (vertex-data (make-world-text-quad-vertices)))
               (multiple-value-bind (origin right-edge up-edge)
                   (video-screen-rectangle-before-camera
                    camera distance lift width height)
                 (let* ((instance-data
                          (make-video-screen-instances
                           origin right-edge up-edge))
                        (texture
                          (keep
                           (create device
                                   (make-texture-descriptor
                                    :label "world video screen picture"
                                    :size (list texture-width texture-height)
                                    :dimensions :2d
                                    :format :rgba8-unorm-srgb
                                    :usage '(:copy-dst :texture-binding)))))
                        (view
                          (keep
                           (create device
                                   (make-texture-view-descriptor
                                    :texture texture))))
                        (sampler
                          (keep
                           (create device
                                   (make-sampler-descriptor
                                    :label "world video screen sampler"
                                    :mag-filter :linear
                                    :min-filter :linear
                                    :mipmap-filter :nearest))))
                        (layout
                          (keep
                           (create device
                                   (make-bind-group-layout-descriptor
                                    :label "world video screen layout"
                                    :entries '((:binding 0 :type :texture)
                                               (:binding 1 :type :sampler)
                                               (:binding 2
                                                :type :uniform-buffer))))))
                        (vertex-buffer
                          (keep
                           (create device
                                   (make-buffer-descriptor
                                    :label "world video screen quad"
                                    :size (* 4 (length vertex-data))
                                    :usage '(:vertex :copy-dst)))))
                        (instance-buffer
                          (keep
                           (create device
                                   (make-buffer-descriptor
                                    :label "world video screen instance"
                                    :size (* 4 (length instance-data))
                                    :usage '(:vertex :copy-dst))))))
                   (setf pipeline
                         (make-live-shader-pipeline
                          :role :video-screen
                          :vertex-role :video-screen
                          :label "world video screen pipeline"
                          :device device :layout layout
                          :vertex-buffers
                          '((:array-stride 12
                             :attributes
                             ((:shader-location 0 :offset 0
                               :format :float32x3)))
                            (:array-stride 36 :step-mode :instance
                             :attributes
                             ((:shader-location 1 :offset 0
                               :format :float32x3)
                              (:shader-location 2 :offset 12
                               :format :float32x3)
                              (:shader-location 3 :offset 24
                               :format :float32x3))))
                          :target-format target-format
                          :primitive '(:topology :triangle-list)
                          :depth-stencil
                          '(:format :depth32-float
                            :depth-write-enabled t
                            :depth-compare :less)))
                   (write-buffer vertex-buffer vertex-data)
                   (write-buffer instance-buffer instance-data)
                   (let ((screen
                           (make-instance
                            'video-screen
                            :video video
                            :width texture-width :height texture-height
                            :words (make-array
                                    (list texture-height texture-width)
                                    :element-type '(unsigned-byte 32))
                            :texture texture :view view :sampler sampler
                            :layout layout :pipeline pipeline
                            :vertex-buffer vertex-buffer
                            :instance-buffer instance-buffer
                            :loop-p loop-p
                            :resources resources)))
                     (setf completed-p t)
                     screen)))))
        (unless completed-p
          (when pipeline (ignore-errors (release-live-shader-pipeline pipeline)))
          (dolist (resource resources) (ignore-errors (destroy resource)))
          (ignore-errors (libav:close-video video)))))))

(defun release-video-screen (screen)
  (release-live-shader-pipeline (video-screen-pipeline screen))
  (dolist (resource (video-screen-resources screen))
    (ignore-errors (destroy resource)))
  (setf (video-screen-resources screen) nil)
  (libav:close-video (video-screen-video screen))
  (values))

(defun video-screen-native-pipeline (screen)
  (live-shader-pipeline-native-pipeline (video-screen-pipeline screen)))

(defun make-video-screen-bind-group (screen device uniform-buffer)
  "Bind the screen's picture, its sampler, and one drawable frame's uniform."
  (create device
          (make-bind-group-descriptor
           :label "world video screen frame bindings"
           :layout (video-screen-layout screen)
           :entries `((:binding 0 :resource ,(video-screen-view screen))
                      (:binding 1 :resource ,(video-screen-sampler screen))
                      (:binding 2 :resource ,uniform-buffer)))))

(defun video-screen-due-picture (screen)
  "Return the index of the picture that should be showing now.

The film starts on the first call rather than when the screen was built, so a
slow world load does not begin the film in the middle."
  (let ((rate (libav:video-frame-rate (video-screen-video screen)))
        (now (get-internal-real-time)))
    (unless (video-screen-start screen)
      (setf (video-screen-start screen) now))
    (if (and rate (plusp rate))
        (floor (* (/ (- now (video-screen-start screen))
                     internal-time-units-per-second)
                  rate))
        (1+ (video-screen-shown screen)))))

(defun upload-video-screen-picture (screen device)
  (let ((video (video-screen-video screen))
        (width (video-screen-texture-width screen))
        (height (video-screen-texture-height screen)))
    (libav:frame-rgba-words video width height
                            :array (video-screen-words screen))
    (write-texture (device-queue device)
                   (make-texture-copy :texture (video-screen-texture screen))
                   (video-screen-words screen)
                   (make-texture-data-layout :bytes-per-row (* width 4)
                                             :rows-per-image height)
                   (list width height))))

(defparameter *video-screen-catch-up-limit* 4
  "How many pictures one world frame may decode to catch up.

Catching up has to be bounded, because the debt is not: a world frame that
stalls for a second owes a second of film, and paying that back inside the
next frame stalls the world again.  Past the limit the film simply slips,
which nobody watching a screen on a wall will mind.")

(defun advance-video-screen (screen device)
  "Decode up to the picture that is due now and upload it.  Return true if so.

Pictures that came due while the last world frame was being drawn are decoded
and discarded rather than shown, so the film keeps the world's time instead of
the renderer's."
  (let ((video (video-screen-video screen))
        (due (video-screen-due-picture screen))
        (decoded-p nil)
        (rewound-p nil))
    (loop repeat *video-screen-catch-up-limit*
          while (< (video-screen-shown screen) due)
          do (cond ((libav:decode-next-frame video)
                    (incf (video-screen-shown screen))
                    (setf decoded-p t))
                   ;; At most one rewind per call.  A film that decodes
                   ;; nothing at all would otherwise rewind and fail forever
                   ;; inside one world frame, which is a hang rather than a
                   ;; dropped picture.
                   ((and (video-screen-loop-p screen) (not rewound-p))
                    (libav:rewind-video video)
                    (setf rewound-p t
                          (video-screen-start screen) (get-internal-real-time)
                          (video-screen-shown screen) -1
                          due 0))
                   (t (setf (video-screen-shown screen) due))))
    ;; Whatever the loop managed, the film's clock now reads whatever SHOWN
    ;; says, so the next call asks for the picture after this one instead of
    ;; trying to make up the same difference all over again.
    (when (> due (video-screen-shown screen))
      (setf (video-screen-start screen)
            (- (get-internal-real-time)
               (video-screen-picture-span screen (video-screen-shown screen)))))
    (when decoded-p
      (upload-video-screen-picture screen device))
    decoded-p))

(defun video-screen-picture-span (screen count)
  "Return the internal-time span COUNT pictures of SCREEN's film occupy."
  (let ((rate (libav:video-frame-rate (video-screen-video screen))))
    (if (and rate (plusp rate) (plusp count))
        (round (* count (/ internal-time-units-per-second rate)))
        0)))
