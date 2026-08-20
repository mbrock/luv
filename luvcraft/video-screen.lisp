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
;;; Software pictures pass through swscale and one reusable RGBA texture.
;;; Hardware pictures cross the backend video-import protocol as one retained
;;; two-plane cohort.  Both arrive here as the same atomically published
;;; DECODED-VIDEO-PICTURE; platform surfaces and synchronization stay in the
;;; bridge files beside their GPU backends.

(in-package #:luvcraft)

(defclass video-screen ()
  ((video :initarg :video :accessor video-screen-video)
   (width :initarg :width :reader video-screen-texture-width)
   (height :initarg :height :reader video-screen-texture-height)
   ;; The RGBA words swscale writes into, reused for every picture.
   (words :initarg :words :initform nil :reader video-screen-words)
   (importer :initarg :importer :initform nil
             :accessor video-screen-importer)
   (picture :initarg :picture :accessor video-screen-picture)
   (retired-pictures :initform nil
                     :accessor video-screen-retired-pictures)
   (hardware-p :initarg :hardware-p :initform nil
               :reader video-screen-hardware-p)
   (sampler :initarg :sampler :reader video-screen-sampler)
   (layout :initarg :layout :reader video-screen-layout)
   ;; The argument table remains live through submission, then the next
   ;; world's frame replaces it after that submission has been committed.
   (bind-group :initform nil :accessor video-screen-bind-group)
   (pipeline :initarg :pipeline :accessor video-screen-pipeline)
   (vertex-buffer :initarg :vertex-buffer :reader video-screen-vertex-buffer)
   (instance-buffer :initarg :instance-buffer
                    :reader video-screen-instance-buffer)
   ;; Playback state.  START is the internal real time the film began at, and
   ;; SHOWN is the index of the picture currently on the texture.
   (start :initform nil :accessor video-screen-start)
   (shown :initform -1 :accessor video-screen-shown)
   (loop-p :initarg :loop-p :initform t :reader video-screen-loop-p)
   ;; The soundtrack, when the film has one; then it is also the clock.
   (sound :initarg :sound :initform nil :accessor video-screen-sound)
   (passes :initform 0 :accessor video-screen-passes
           :documentation "Which pass of the sound the picture is on.")
   (center :initarg :center :initform nil :reader video-screen-center
           :documentation "The screen's middle in the world: where its sound
comes from.")
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
     &key (distance 12.0) (lift 4.0) (height 4.5) rectangle (loop-p t)
       (hardware :auto))
  "Open PATHNAME and build a world screen playing it, facing CAMERA.

HEIGHT is the screen's height in cells; its width follows the film's aspect.
When RECTANGLE is supplied, it is called with that aspect and returns the
lower-left origin, right edge, and up edge of an authored world rectangle.
HARDWARE is NIL, :AUTO, or :REQUIRED.  :REQUIRED rejects a file unless its
first decoded picture actually lives in a hardware surface."
  (check-type hardware (member nil :auto :required))
  (multiple-value-bind (decoder-hardware decoder-configuration)
      (if hardware
          (video-decode-configuration device hardware)
          (values nil nil))
    (let* ((video (libav:open-video
                   pathname
                   :hardware decoder-hardware
                   :hardware-configuration decoder-configuration))
           (resources nil)
           (pipeline nil)
           (importer nil)
           (picture nil)
           (sound nil)
           (completed-p nil))
      (flet ((keep (resource) (push resource resources) resource))
        (unwind-protect
             (let* ((first-frame (libav:decode-next-frame video))
                    (hardware-p
                      (not (null (and first-frame
                                      (libav:frame-hardware-p first-frame))))))
               (when (and (eq hardware :required) (not hardware-p))
                 (error "FFmpeg could not hardware-decode ~A on this device."
                        pathname))
               (when hardware-p
                 (setf importer (make-video-frame-importer device))
                 (unless importer
                   (error "No hardware-video importer exists for ~S." device)))
               (multiple-value-bind (texture-width texture-height)
                   (if hardware-p
                       (values (libav:video-width video)
                               (libav:video-height video))
                       (video-screen-texture-size video))
                 (let* ((aspect (/ (libav:video-width video)
                                   (libav:video-height video)))
                        (width (* height aspect))
                        (vertex-data (make-world-text-quad-vertices)))
                   (multiple-value-bind (origin right-edge up-edge)
                       (if rectangle
                           (funcall rectangle aspect)
                           (video-screen-rectangle-before-camera
                            camera distance lift width height))
                     (let* ((instance-data
                              (make-video-screen-instances
                               origin right-edge up-edge))
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
                                        :entries
                                        (if hardware-p
                                            '((:binding 0 :type :texture)
                                              (:binding 1 :type :sampler)
                                              (:binding 2 :type :uniform-buffer)
                                              (:binding 3 :type :texture))
                                            '((:binding 0 :type :texture)
                                              (:binding 1 :type :sampler)
                                              (:binding 2 :type :uniform-buffer)))))))
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
                       (setf picture
                             (if hardware-p
                                 (adopt-decoded-video-frame
                                  importer first-frame
                                  texture-width texture-height)
                                 (make-decoded-video-picture-from-planes
                                  device 1
                                  (lambda (plane)
                                    (declare (ignore plane))
                                    (create
                                     device
                                     (make-texture-descriptor
                                      :label "world video screen picture"
                                      :size (list texture-width texture-height)
                                      :dimensions :2d
                                      :format :rgba8-unorm-srgb
                                      :usage
                                      '(:copy-dst :texture-binding)))))))
                       (setf pipeline
                             (make-live-shader-pipeline
                              :role (if hardware-p
                                        :video-screen-hardware :video-screen)
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
                                :words (unless hardware-p
                                         (make-array
                                          (list texture-height texture-width)
                                          :element-type '(unsigned-byte 32)))
                                :importer importer :picture picture
                                :sampler sampler :hardware-p hardware-p
                                :layout layout :pipeline pipeline
                                :vertex-buffer vertex-buffer
                                :instance-buffer instance-buffer
                                :loop-p loop-p
                                :center (make-vec3
                                         (+ (vec3-x origin)
                                            (* 0.5 (+ (vec3-x right-edge)
                                                      (vec3-x up-edge))))
                                         (+ (vec3-y origin)
                                            (* 0.5 (+ (vec3-y right-edge)
                                                      (vec3-y up-edge))))
                                         (+ (vec3-z origin)
                                            (* 0.5 (+ (vec3-z right-edge)
                                                      (vec3-z up-edge)))))
                                :resources resources)))
                         ;; The sound starts as soon as the screen exists; the
                         ;; first picture goes up against its clock.
                         (setf sound
                               (open-film-sound pathname :loop-p loop-p)
                               (video-screen-sound screen) sound)
                         (when first-frame
                           (unless hardware-p
                             (upload-video-screen-picture screen device))
                           (setf (video-screen-shown screen) 0))
                         (setf completed-p t)
                         screen))))))
          (unless completed-p
            ;; The caller is already being told why the screen could not be
            ;; built, so trouble unwinding it is a warning beside that.
            (with-release-warnings
              (release-video-screen-or-retain
               (make-instance
                'video-screen
                :video video :width 0 :height 0
                :importer importer :picture picture
                :sampler nil :layout nil :pipeline pipeline
                :vertex-buffer nil :instance-buffer nil
                :sound sound :resources resources)))))))))

(defun release-video-screen (screen)
  "Release SCREEN's pipeline, GPU resources, and open film.

Each step is contained so that a failure early on cannot strand the film's
decoder or the resources after it; the failures travel out through whatever
release report is running.  See WITH-RELEASE-REPORT."
  (when (video-screen-bind-group screen)
    (when (releasing :video-screen-bind-group
            (destroy (video-screen-bind-group screen))
            t)
      (setf (video-screen-bind-group screen) nil)))
  (when (video-screen-pipeline screen)
    (when (releasing :video-screen-pipeline
            (release-live-shader-pipeline (video-screen-pipeline screen))
            t)
      (setf (video-screen-pipeline screen) nil)))
  (setf (video-screen-resources screen)
        (delete-if
         (lambda (resource)
           (releasing :video-screen-resource
             (destroy resource)
             t))
         (video-screen-resources screen)))
  (release-decoded-video-picture (video-screen-picture screen))
  (when (decoded-video-picture-released-p (video-screen-picture screen))
    (setf (video-screen-picture screen) nil))
  (let ((remaining nil))
    (dolist (picture (video-screen-retired-pictures screen))
      (release-decoded-video-picture picture)
      (unless (decoded-video-picture-released-p picture)
        (push picture remaining)))
    (setf (video-screen-retired-pictures screen) (nreverse remaining)))
  ;; Picture handles can disappear as soon as their native teardown transfers
  ;; into the HAL retirement ledger.  This therefore requests importer closure
  ;; after logical retirement; each adopted plane's owner callback keeps the
  ;; backend-native importer state alive until physical retirement succeeds.
  (when (and (decoded-video-picture-released-p
              (video-screen-picture screen))
             (null (video-screen-retired-pictures screen))
             (video-screen-importer screen))
    (when (releasing :video-frame-importer
            (release-video-frame-importer (video-screen-importer screen))
            t)
      (setf (video-screen-importer screen) nil)))
  (when (video-screen-sound screen)
    (when (releasing :video-screen-sound
            (close-film-sound (video-screen-sound screen))
            t)
      (setf (video-screen-sound screen) nil)))
  (when (video-screen-video screen)
    (when (releasing :video-screen-film
            (libav:close-video (video-screen-video screen))
            t)
      (setf (video-screen-video screen) nil)))
  (values))

(defun video-screen-released-p (screen)
  "True when SCREEN retains no logical owner which a caller could retry."
  (and (null (video-screen-bind-group screen))
       (null (video-screen-pipeline screen))
       (null (video-screen-resources screen))
       (decoded-video-picture-released-p (video-screen-picture screen))
       (null (video-screen-retired-pictures screen))
       (null (video-screen-importer screen))
       (null (video-screen-sound screen))
       (null (video-screen-video screen))))

(defvar *video-screen-release-backlog* nil
  "Exceptional startup-owned screens whose logical release needs a retry.")

(defvar *video-screen-release-backlog-lock*
  (sb-thread:make-mutex :name "luvcraft video screen release backlog"))

(defun retain-video-screen-release-backlog (screen)
  "Process-root an incompletely released startup SCREEN for later retry."
  (sb-thread:with-mutex (*video-screen-release-backlog-lock*)
    (pushnew screen *video-screen-release-backlog* :test #'eq))
  screen)

(defun release-video-screen-or-retain (screen)
  "Release SCREEN, retaining it globally across an exceptional unwind."
  (unwind-protect
       (release-video-screen screen)
    (unless (video-screen-released-p screen)
      (retain-video-screen-release-backlog screen)))
  screen)

(defun retry-video-screen-release-backlog ()
  "Retry every screen retained by an earlier failed startup unwind."
  (let ((screens
          (sb-thread:with-mutex (*video-screen-release-backlog-lock*)
            (prog1 *video-screen-release-backlog*
              (setf *video-screen-release-backlog* nil)))))
    (dolist (screen screens)
      (with-release-warnings
        (release-video-screen-or-retain screen))))
  (values))

(defun video-screen-native-pipeline (screen)
  (live-shader-pipeline-native-pipeline (video-screen-pipeline screen)))

(defun refresh-video-screen-bind-group (screen device uniform-buffer)
  "Bind SCREEN's current picture for this frame and retain it through submit."
  (when (video-screen-bind-group screen)
    ;; Encoding has already committed the preceding frame before the next
    ;; callback begins.  Destroying the old semantic argument table now is
    ;; safe; destroying the new one before SUBMIT is not.
    (destroy (video-screen-bind-group screen))
    (setf (video-screen-bind-group screen) nil))
  (setf (video-screen-bind-group screen)
        (create device
                (make-bind-group-descriptor
                 :label "world video screen frame bindings"
                 :layout (video-screen-layout screen)
                 :entries `((:binding 0 :resource
                              ,(decoded-video-picture-view
                                (video-screen-picture screen) 0))
                            (:binding 1 :resource ,(video-screen-sampler screen))
                            (:binding 2 :resource ,uniform-buffer)
                            ,@(when (video-screen-hardware-p screen)
                                `((:binding 3 :resource
                                   ,(decoded-video-picture-view
                                     (video-screen-picture screen) 1)))))))))

(defun retry-video-screen-retired-pictures (screen)
  "Retry SCREEN's previously failed picture retirements, retaining failures."
  (let ((remaining nil))
    (with-release-warnings
      (dolist (picture (video-screen-retired-pictures screen))
        (release-decoded-video-picture picture)
        (unless (decoded-video-picture-released-p picture)
          (push picture remaining))))
    (setf (video-screen-retired-pictures screen) (nreverse remaining)))
  screen)

(defun install-hardware-video-picture (screen &optional frame)
  "Adopt and atomically publish SCREEN's current decoded hardware frame.

The complete candidate is built before SCREEN changes.  If adoption signals,
the preceding picture remains published; after publication its replacement is
retired with warnings so teardown trouble cannot roll the screen backward."
  (let* ((decoded-frame
           (or frame (libav:video-frame (video-screen-video screen))))
         (candidate
           (adopt-decoded-video-frame
            (video-screen-importer screen) decoded-frame
            (video-screen-texture-width screen)
            (video-screen-texture-height screen)))
         (previous (video-screen-picture screen)))
    (setf (video-screen-picture screen) candidate)
    ;; Put PREVIOUS somewhere durable before attempting teardown.  A failed
    ;; destroy therefore remains owned and retryable rather than falling out
    ;; of reach behind the newly published candidate.
    (push previous (video-screen-retired-pictures screen))
    (retry-video-screen-retired-pictures screen)
    screen))

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
                   (make-texture-copy
                    :texture (first
                              (decoded-video-picture-textures
                               (video-screen-picture screen))))
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

(defun video-screen-sound-due-picture (screen sound)
  "The picture due by the soundtrack's clock, or NIL to hold the last one.

When the sound has started over, the picture starts over with it; while
the previous pass's tail is still sounding, the last picture holds."
  (let ((rate (libav:video-frame-rate (video-screen-video screen)))
        (time (film-sound-time sound)))
    (cond ((/= (film-sound-passes sound) (video-screen-passes screen))
           (when (>= time 0)
             (libav:rewind-video (video-screen-video screen))
             (setf (video-screen-shown screen) -1
                   (video-screen-passes screen) (film-sound-passes sound))
             (if (and rate (plusp rate)) (floor (* time rate)) 0)))
          ((< time 0) nil)
          ((and rate (plusp rate)) (floor (* time rate)))
          (t (1+ (video-screen-shown screen))))))

(defun advance-video-screen (screen device)
  "Decode up to the picture that is due now and upload it.  Return true if so.

Pictures that came due while the last world frame was being drawn are decoded
and discarded rather than shown, so the film keeps the world's time instead of
the renderer's.  A film with sound keeps the sound's time instead: the ear is
the stricter judge, so the speaker is the clock and the picture follows."
  (let* ((video (video-screen-video screen))
         (sound (video-screen-sound screen))
         (due (if sound
                  (or (video-screen-sound-due-picture screen sound)
                      (return-from advance-video-screen nil))
                  (video-screen-due-picture screen)))
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
                   ;; dropped picture.  With sound, the sound says when to
                   ;; start over; a picture that runs out first just holds.
                   ((and (video-screen-loop-p screen) (not rewound-p)
                         (null sound))
                    (libav:rewind-video video)
                    (setf rewound-p t
                          (video-screen-start screen) (get-internal-real-time)
                          (video-screen-shown screen) -1
                          due 0))
                   (t (setf (video-screen-shown screen) due))))
    ;; Whatever the loop managed, the film's clock now reads whatever SHOWN
    ;; says, so the next call asks for the picture after this one instead of
    ;; trying to make up the same difference all over again.  (The sound's
    ;; clock is not ours to move: it simply drops the pictures it must.)
    (when (and (null sound) (> due (video-screen-shown screen)))
      (setf (video-screen-start screen)
            (- (get-internal-real-time)
               (video-screen-picture-span screen (video-screen-shown screen)))))
    (when decoded-p
      (if (video-screen-hardware-p screen)
          (install-hardware-video-picture screen)
          (upload-video-screen-picture screen device)))
    decoded-p))

(defun video-screen-picture-span (screen count)
  "Return the internal-time span COUNT pictures of SCREEN's film occupy."
  (let ((rate (libav:video-frame-rate (video-screen-video screen))))
    (if (and rate (plusp rate) (plusp count))
        (round (* count (/ internal-time-units-per-second rate)))
        0)))

(defun place-video-screen-listener (screen camera)
  "Hear SCREEN's film from where CAMERA stands, facing as it faces."
  (alexandria:when-let ((sound (video-screen-sound screen)))
    (when (video-screen-center screen)
      (multiple-value-bind (right up forward) (camera-basis camera)
        (declare (ignore up forward))
        (place-film-sound-listener sound (video-screen-center screen)
                                   (camera-position camera) right))))
  screen)

(defun hush-video-screen (screen)
  "Stop SCREEN's sound now, leaving the picture to keep its own time."
  (alexandria:when-let ((sound (video-screen-sound screen)))
    (close-film-sound sound)
    (setf (video-screen-sound screen) nil))
  screen)
