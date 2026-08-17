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

#-darwin
(defun vulkan-video-configuration (device)
  "Describe DEVICE and its graphics/video queues to FFmpeg."
  (when (typep device 'luv::vulkan-gpu-device)
    (let* ((graphics-family (luv::vulkan-device-queue-family device))
           (video-family (luv::vulkan-device-video-queue-family device))
           (families (luv.vulkan:physical-device-queue-families
                      (luv::vulkan-device-physical-device device)))
           (graphics-flags (luv.vulkan:queue-family-flags
                            (nth graphics-family families)))
           (video-flags (and video-family
                             (luv.vulkan:queue-family-flags
                              (nth video-family families))))
           (extensions (luv::vulkan-device-extension-names device)))
      (when (and video-family
                 (member "VK_KHR_video_queue" extensions :test #'string=)
                 (member "VK_KHR_video_decode_queue" extensions :test #'string=))
        (list
         :instance (luv::vulkan-device-instance device)
         :physical-device (luv::vulkan-device-physical-device device)
         :device (luv::vulkan-handle device)
         :get-instance-proc-addr
         (cffi:foreign-symbol-pointer
          "vkGetInstanceProcAddr" :library 'luv.vulkan::vulkan-loader)
         :instance-extensions (luv::vulkan-device-instance-extension-names device)
         :device-extensions (luv::vulkan-device-extension-names device)
         :queue-families
         (list
          (list :index graphics-family
                :flags (vulkan-queue-flags-value graphics-flags))
          (list :index video-family
                :flags (vulkan-queue-flags-value video-flags)
                :video-capabilities
                (logior
                 (if (member "VK_KHR_video_decode_h264" extensions
                             :test #'string=) #x1 0)
                 (if (member "VK_KHR_video_decode_h265" extensions
                             :test #'string=) #x2 0)))))))))

#-darwin
(defun vulkan-queue-flags-value (flags)
  (loop for flag in flags
        sum (ecase flag
              (:graphics #x1) (:compute #x2) (:transfer #x4)
              (:sparse-binding #x8) (:video-decode #x20)
              (:video-encode #x40))))

#+darwin
(defun vulkan-video-configuration (device)
  (declare (ignore device))
  nil)

#+darwin
(progn
  (cffi:define-foreign-library core-video
    (:darwin (:framework "CoreVideo")))
  (cffi:define-foreign-library core-foundation
    (:darwin (:framework "CoreFoundation")))
  (cffi:defcfun ("CVMetalTextureCacheCreate" %cv-metal-texture-cache-create)
      :int
    (allocator :pointer) (cache-attributes :pointer) (device :pointer)
    (texture-attributes :pointer) (cache :pointer))
  (cffi:defcfun ("CVMetalTextureCacheCreateTextureFromImage"
                 %cv-metal-texture-from-image) :int
    (allocator :pointer) (cache :pointer) (image :pointer)
    (texture-attributes :pointer) (pixel-format :uint64)
    (width :size) (height :size) (plane :size) (texture :pointer))
  (cffi:defcfun ("CVMetalTextureGetTexture" %cv-metal-texture-get-texture)
      :pointer (texture :pointer))
  (cffi:defcfun ("CVPixelBufferGetPlaneCount" %cv-pixel-buffer-plane-count)
      :size (buffer :pointer))
  (cffi:defcfun ("CVPixelBufferGetWidthOfPlane" %cv-pixel-buffer-plane-width)
      :size (buffer :pointer) (plane :size))
  (cffi:defcfun ("CVPixelBufferGetHeightOfPlane" %cv-pixel-buffer-plane-height)
      :size (buffer :pointer) (plane :size)))

(defclass video-screen ()
  ((video :initarg :video :reader video-screen-video)
   (width :initarg :width :reader video-screen-texture-width)
   (height :initarg :height :reader video-screen-texture-height)
   ;; The RGBA words swscale writes into, reused for every picture.
   (words :initarg :words :initform nil :reader video-screen-words)
   (texture :initarg :texture :initform nil :accessor video-screen-texture)
   (view :initarg :view :initform nil :accessor video-screen-view)
   (chroma-texture :initform nil :accessor video-screen-chroma-texture)
   (chroma-view :initform nil :accessor video-screen-chroma-view)
   (texture-cache :initarg :texture-cache :initform nil
                  :accessor video-screen-texture-cache)
   (hardware-p :initarg :hardware-p :initform nil
               :reader video-screen-hardware-p)
   (sampler :initarg :sampler :reader video-screen-sampler)
   (layout :initarg :layout :reader video-screen-layout)
   ;; The argument table remains live through submission, then the next
   ;; world's frame replaces it after that submission has been committed.
   (bind-group :initform nil :accessor video-screen-bind-group)
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
     &key (distance 12.0) (lift 4.0) (height 4.5) rectangle (loop-p t)
       (hardware :auto))
  "Open PATHNAME and build a world screen playing it, facing CAMERA.

HEIGHT is the screen's height in cells; its width follows the film's aspect.
When RECTANGLE is supplied, it is called with that aspect and returns the
lower-left origin, right edge, and up edge of an authored world rectangle.
HARDWARE is NIL, :AUTO, or :REQUIRED.  :REQUIRED rejects a file unless its
first decoded picture actually lives in a hardware surface."
  (check-type hardware (member nil :auto :required))
  (let* ((vulkan-configuration (vulkan-video-configuration device))
         (video (libav:open-video
                 pathname
                 :hardware (cond #+darwin
                                 ((and hardware
                                       (typep device 'luv::metal-gpu-device))
                                  hardware)
                                 ((and hardware vulkan-configuration) :vulkan)
                                 (t nil))
                 :hardware-configuration vulkan-configuration))
         (resources nil)
         (pipeline nil)
         (completed-p nil))
    (flet ((keep (resource) (push resource resources) resource))
      (unwind-protect
           (let* ((first-frame (libav:decode-next-frame video))
                  (hardware-p
                    (not (null
                          (and first-frame
                               (or (libav:frame-videotoolbox-pixel-buffer
                                    first-frame)
                                   (libav:frame-vulkan-frame first-frame)))))))
             (when (and (eq hardware :required) (not hardware-p))
               (error "FFmpeg could not hardware-decode ~A on this device."
                      pathname))
             (multiple-value-bind (texture-width texture-height)
                 (if hardware-p
                     (values (libav:video-width video) (libav:video-height video))
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
                          (texture
                            (unless hardware-p
                              (keep
                               (create device
                                       (make-texture-descriptor
                                        :label "world video screen picture"
                                        :size (list texture-width texture-height)
                                        :dimensions :2d
                                        :format :rgba8-unorm-srgb
                                        :usage '(:copy-dst :texture-binding))))))
                          (view
                            (when texture
                              (keep
                               (create device
                                       (make-texture-view-descriptor
                                        :texture texture)))))
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
                              :texture texture :view view :sampler sampler
                              :hardware-p hardware-p
                              :layout layout :pipeline pipeline
                              :vertex-buffer vertex-buffer
                              :instance-buffer instance-buffer
                              :loop-p loop-p
                              :resources resources)))
                       (when first-frame
                         (if hardware-p
                             (install-hardware-video-picture screen device)
                             (upload-video-screen-picture screen device))
                         (setf (video-screen-shown screen) 0))
                       (setf completed-p t)
                       screen))))))
        (unless completed-p
          ;; The caller is already being told why the screen could not be
          ;; built, so trouble unwinding it is a warning beside that.
          (with-release-warnings
            (when pipeline
              (releasing :video-screen-pipeline
                (release-live-shader-pipeline pipeline)))
            (dolist (resource resources)
              (releasing :video-screen-resource (destroy resource)))
            (releasing :video-screen-film (libav:close-video video))))))))

(defun release-video-screen (screen)
  "Release SCREEN's pipeline, GPU resources, and open film.

Each step is contained so that a failure early on cannot strand the film's
decoder or the resources after it; the failures travel out through whatever
release report is running.  See WITH-RELEASE-REPORT."
  (when (video-screen-bind-group screen)
    (releasing :video-screen-bind-group
      (destroy (video-screen-bind-group screen)))
    (setf (video-screen-bind-group screen) nil))
  (releasing :video-screen-pipeline
    (release-live-shader-pipeline (video-screen-pipeline screen)))
  (dolist (resource (video-screen-resources screen))
    (releasing :video-screen-resource (destroy resource)))
  (setf (video-screen-resources screen) nil)
  (release-video-screen-picture screen)
  #+darwin
  (when (video-screen-texture-cache screen)
    (cffi:foreign-funcall "CFRelease" :pointer
                          (video-screen-texture-cache screen) :void)
    (setf (video-screen-texture-cache screen) nil))
  (releasing :video-screen-film (libav:close-video (video-screen-video screen)))
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
                 :entries `((:binding 0 :resource ,(video-screen-view screen))
                            (:binding 1 :resource ,(video-screen-sampler screen))
                            (:binding 2 :resource ,uniform-buffer)
                            ,@(when (video-screen-hardware-p screen)
                                `((:binding 3 :resource
                                   ,(video-screen-chroma-view screen)))))))))

(defun release-video-screen-picture (screen)
  (when (video-screen-view screen) (destroy (video-screen-view screen)))
  (when (video-screen-chroma-view screen)
    (destroy (video-screen-chroma-view screen)))
  (when (and (video-screen-hardware-p screen) (video-screen-texture screen))
    (destroy (video-screen-texture screen)))
  (when (video-screen-chroma-texture screen)
    (destroy (video-screen-chroma-texture screen)))
  (when (video-screen-hardware-p screen)
    (setf (video-screen-view screen) nil
          (video-screen-texture screen) nil))
  (setf (video-screen-chroma-view screen) nil
        (video-screen-chroma-texture screen) nil))

#+darwin
(defun ensure-video-screen-texture-cache (screen device)
  (or (video-screen-texture-cache screen)
      (progn
        (cffi:use-foreign-library core-video)
        (cffi:use-foreign-library core-foundation)
        (cffi:with-foreign-object (cell :pointer)
          (let ((status
                  (%cv-metal-texture-cache-create
                   (cffi:null-pointer) (cffi:null-pointer)
                   (luv.objective-c:objective-c-pointer
                    (luv::metal-native-object device))
                   (cffi:null-pointer) cell)))
            (unless (zerop status)
              (error "CVMetalTextureCacheCreate failed with status ~D." status))
            (setf (video-screen-texture-cache screen)
                  (cffi:mem-ref cell :pointer)))))))

#+darwin
(defun make-video-plane-texture (screen device pixel-buffer plane format native-format)
  (let ((width (%cv-pixel-buffer-plane-width pixel-buffer plane))
        (height (%cv-pixel-buffer-plane-height pixel-buffer plane)))
    (cffi:with-foreign-object (cell :pointer)
      (let ((status
              (%cv-metal-texture-from-image
               (cffi:null-pointer)
               (ensure-video-screen-texture-cache screen device) pixel-buffer
               (cffi:null-pointer) native-format width height plane cell)))
        (unless (zerop status)
          (error "CVMetalTexture creation for plane ~D failed with status ~D."
                 plane status))
        (let* ((owner (cffi:mem-ref cell :pointer))
               (native
                 (luv.objective-c:wrap-objective-c-object
                  (%cv-metal-texture-get-texture owner)
                  :ownership :borrowed :protocol-name "MTLTexture"))
               (texture
                 (adopt-native-texture
                  device native owner
                  (make-texture-descriptor
                   :label (format nil "VideoToolbox plane ~D" plane)
                   :size (list width height) :dimensions :2d :format format
                   :usage '(:texture-binding)))))
          (values texture
                  (create device
                          (make-texture-view-descriptor :texture texture))))))))

(defun install-videotoolbox-picture (screen device)
  #-darwin (declare (ignore screen device))
  #+darwin
  (let ((buffer
          (libav:frame-videotoolbox-pixel-buffer
           (libav:video-frame (video-screen-video screen)))))
    (unless (and buffer (= 2 (%cv-pixel-buffer-plane-count buffer)))
      (error "VideoToolbox did not return the expected two-plane NV12 surface."))
    (multiple-value-bind (luma luma-view)
        (make-video-plane-texture screen device buffer 0 :r8-unorm 10)
      (multiple-value-bind (chroma chroma-view)
          (make-video-plane-texture screen device buffer 1 :rg8-unorm 30)
        (release-video-screen-picture screen)
        (setf (video-screen-texture screen) luma
              (video-screen-view screen) luma-view
              (video-screen-chroma-texture screen) chroma
              (video-screen-chroma-view screen) chroma-view))))
  screen)

#-darwin
(defun vulkan-frame-element (pointer slot type index)
  (cffi:mem-aref
   (cffi:foreign-slot-pointer pointer '(:struct libav::av-vulkan-frame) slot)
   type index))

#-darwin
(defun install-vulkan-picture (screen device)
  (let* ((decoded (libav:video-frame (video-screen-video screen)))
         (frame (libav:frame-vulkan-frame decoded)))
    (unless frame
      (error "FFmpeg did not return an AVVkFrame."))
    (let* ((image (vulkan-frame-element frame 'libav::images :pointer 0))
           (layout-value
             (vulkan-frame-element frame 'libav::layouts :uint32 0))
           (layout (or (cffi:foreign-enum-keyword
                        'luv.vulkan::image-layout layout-value :errorp nil)
                       :general))
           (semaphore
             (vulkan-frame-element frame 'libav::semaphores :pointer 0))
           (semaphore-value
             (vulkan-frame-element frame 'libav::semaphore-values :uint64 0)))
      (labels ((plane (plane format aspect width height)
                 (let* ((retained (libav:clone-frame decoded))
                        (retained-vulkan-frame
                          (libav:frame-vulkan-frame retained))
                        (release (lambda () (libav:release-frame retained)))
                        (submitted
                          (lambda (new-layout new-value)
                            (setf (cffi:mem-aref
                                   (cffi:foreign-slot-pointer
                                    retained-vulkan-frame
                                    '(:struct libav::av-vulkan-frame)
                                    'libav::layouts)
                                   :uint32 0)
                                  (cffi:foreign-enum-value
                                   'luv.vulkan::image-layout new-layout)
                                  (cffi:mem-aref
                                   (cffi:foreign-slot-pointer
                                    retained-vulkan-frame
                                    '(:struct libav::av-vulkan-frame)
                                    'libav::semaphore-values)
                                   :uint64 0)
                                  new-value)))
                        (texture
                          (adopt-native-texture
                           device
                           (list :image image :format format :aspect aspect
                                 :layout layout :semaphore semaphore
                                 :semaphore-value semaphore-value
                                 :submitted submitted)
                           release
                           (make-texture-descriptor
                            :label (format nil "FFmpeg Vulkan plane ~D" plane)
                            :size (list width height) :dimensions :2d
                            :format (ecase plane (0 :r8-unorm) (1 :rg8-unorm))
                            :usage '(:texture-binding)))))
                   (values texture
                           (create device
                                   (make-texture-view-descriptor
                                    :texture texture))))))
        (multiple-value-bind (luma luma-view)
            (plane 0 :r8-unorm :plane-0
                   (video-screen-texture-width screen)
                   (video-screen-texture-height screen))
          (multiple-value-bind (chroma chroma-view)
              (plane 1 :r8g8-unorm :plane-1
                     (ceiling (video-screen-texture-width screen) 2)
                     (ceiling (video-screen-texture-height screen) 2))
            (release-video-screen-picture screen)
            (setf (video-screen-texture screen) luma
                  (video-screen-view screen) luma-view
                  (video-screen-chroma-texture screen) chroma
                  (video-screen-chroma-view screen) chroma-view))))))
  screen)

(defun install-hardware-video-picture (screen device)
  (if (libav:frame-vulkan-frame
       (libav:video-frame (video-screen-video screen)))
      #-darwin (install-vulkan-picture screen device)
      #+darwin (error "A Vulkan frame reached the Metal backend.")
      (install-videotoolbox-picture screen device)))

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
      (if (video-screen-hardware-p screen)
          (install-hardware-video-picture screen device)
          (upload-video-screen-picture screen device)))
    decoded-p))

(defun video-screen-picture-span (screen count)
  "Return the internal-time span COUNT pictures of SCREEN's film occupy."
  (let ((rate (libav:video-frame-rate (video-screen-video screen))))
    (if (and rate (plusp rate) (plusp count))
        (round (* count (/ internal-time-units-per-second rate)))
        0)))
