;;;; A luvcraft that draws for another luvcraft.
;;;;
;;;; The experiment: the shipped executable is a boot core -- a Lisp with the
;;;; whole game loaded and nothing yet asked of the GPU.  A playing game can
;;;; spawn one as a child, hand it the integer name of an IOSurface over the
;;;; child's own stdin, and the child renders its own hidden game into that
;;;; surface while the parent samples the very same pixels as a texture.  No
;;;; ports, no shared memory ceremony: a pipe carrying one number each way.
;;;;
;;;; The protocol is lockstep and tiny.  Parent writes "size W H" (points);
;;;; child opens its hidden game and answers "extent W H" (pixels).  Parent
;;;; creates a ring of surfaces that size and writes "surfaces ID ID ID";
;;;; child wraps each and answers "ready".  Parent writes "frame K", child
;;;; renders one frame into slot K, waits for the GPU, and answers "done K".
;;;; Parent writes "quit" or closes the pipe; child stops.  The ring is what
;;;; keeps the picture from tearing: the parent only ever asks for a slot it
;;;; is not showing and was not showing a frame ago.

(in-package #:luvcraft)

;;; The child: render into a surface somebody else named.

(defun attach-luvcraft-frame-mirror (session surface)
  "Return an adopted texture over the IOSurface SURFACE, sized like SESSION's
frame, fit to be SESSION's frame mirror.  The caller destroys it."
  #-darwin (declare (ignore session surface))
  #-darwin (error "Frame mirrors need IOSurface, which is Darwin only.")
  #+darwin
  (let* ((device (luvcraft-session-device session))
         (context (luvcraft-session-context session))
         (format (canvas-format context))
         (extent (canvas-extent context))
         (width (luv.metal:iosurface-width surface))
         (height (luv.metal:iosurface-height surface)))
    (unless (and (= width (first extent)) (= height (second extent)))
      (error "The mirror surface is ~Dx~D but the game frame is ~Dx~D."
             width height (first extent) (second extent)))
    (let* ((native
             (luv.metal:new-metal-texture-for-iosurface
              (luv::metal-native-object device) surface
              (luv::metal-resource-pixel-format format nil)
              (logior luv.metal:+texture-usage-shader-read+
                      luv.metal:+texture-usage-render-target+)
              :label "luvcraft frame mirror"))
           ;; The +1 from NEW is the owner reference DESTROY will CFRelease.
           (mirror
             (adopt-native-texture
              device native
              (luv.objective-c:objective-c-pointer native)
              (make-texture-descriptor
               :label "luvcraft frame mirror"
               :size (list width height) :dimensions :2d :format format
               :usage '(:copy-dst :texture-binding)))))
      mirror)))

(defun serve-luvcraft-mirror (&key (input *standard-input*)
                                   (output *standard-output*)
                                   (provider (make-instance 'luv:metal-gpu-provider)))
  "Be the child: open a hidden game of the size named on INPUT, tell OUTPUT the
frame's pixel extent, and from then on render into whatever surface INPUT names."
  (flet ((say (control &rest arguments)
           (apply #'format output control arguments)
           (terpri output)
           (finish-output output))
         (hear ()
           (let ((line (read-line input nil nil)))
             (and line (uiop:split-string (string-trim " " line))))))
    (let* ((hello (hear))
           (width (and (equal (first hello) "size")
                       (parse-integer (second hello) :junk-allowed t)))
           (height (and width (parse-integer (third hello) :junk-allowed t))))
      (unless height
        (error "Expected \"size W H\" on standard input, got ~S." hello))
      (let ((surfaces nil) (session nil) (mirrors nil))
        (unwind-protect
             (progn
               (setf session
                     (start-luvcraft
                      :title "luvcraft mirror child"
                      :width width :height height
                      :frames-per-second nil :visible-p nil
                      :provider provider))
               (wait-for-luvcraft-products session)
               ;; The window is in points; the frame is in pixels, and the
               ;; surfaces have to be the frame's size, so the parent learns
               ;; the pixel extent from us and creates the ring after.
               (let ((extent (canvas-extent (luvcraft-session-context session))))
                 (say "extent ~D ~D" (first extent) (second extent)))
               (let ((message (hear)))
                 (unless (and (equal (first message) "surfaces") (rest message))
                   (error "Expected \"surfaces ID...\", got ~S." message))
                 (setf surfaces
                       (map 'vector
                            (lambda (word)
                              (let ((id (parse-integer word :junk-allowed t)))
                                (or (and id (luv.metal:lookup-iosurface id))
                                    (error "No IOSurface is named ~A." word))))
                            (rest message)))
                 (setf mirrors
                       (map 'vector
                            (lambda (surface)
                              (attach-luvcraft-frame-mirror session surface))
                            surfaces)))
               (say "ready")
               (loop for message = (hear)
                     while (and message (not (equal (first message) "quit")))
                     do (cond
                          ((equal (first message) "frame")
                           (let ((slot (parse-integer (or (second message) "0")
                                                      :junk-allowed t)))
                             (unless (and slot (< -1 slot (length mirrors)))
                               (error "No ring slot ~S." (second message)))
                             (setf (luvcraft-session-frame-mirror session)
                                   (aref mirrors slot))
                             (render-luvcraft-frame
                              session (/ (get-internal-real-time)
                                         (float internal-time-units-per-second 1d0)))
                             (submitted-work-done
                              (device-queue (luvcraft-session-device session)))
                             (say "done ~D" slot)))
                          (t (say "what ~{~A~^ ~}" message)))))
          (when session
            (setf (luvcraft-session-frame-mirror session) nil)
            (when mirrors (map nil #'destroy mirrors))
            (stop-luvcraft session))
          (when surfaces (map nil #'luv.metal:release-iosurface surfaces)))))))

;;; The parent: a child process and the surface it draws into.

(defclass luvcraft-mirror ()
  ((surfaces :initform #() :accessor luvcraft-mirror-surfaces)
   (process :initarg :process :reader luvcraft-mirror-process)
   (width :initform nil :accessor luvcraft-mirror-width)
   (height :initform nil :accessor luvcraft-mirror-height)
   (frames :initform 0 :accessor luvcraft-mirror-frames)
   ;; The slot the child most recently finished, or NIL before the first.
   (completed-slot :initform nil :accessor luvcraft-mirror-completed-slot))
  (:documentation "A child luvcraft rendering into a ring of IOSurfaces this
process owns.  WIDTH and HEIGHT are the surfaces', in pixels."))

(defun luvcraft-mirror-surface (mirror &optional slot)
  "Surface SLOT of MIRROR's ring; with no SLOT, the last completed one."
  (aref (luvcraft-mirror-surfaces mirror)
        (or slot (luvcraft-mirror-completed-slot mirror) 0)))

(defun luvcraft-mirror-slot-count (mirror)
  (length (luvcraft-mirror-surfaces mirror)))

(defun luvcraft-mirror-executable ()
  (let ((path (merge-pathnames "build/luvcraft"
                               (asdf:system-source-directory "luvcraft"))))
    (unless (probe-file path)
      (error "There is no ~A to spawn; run make luvcraft first." path))
    path))

(defun tell-luvcraft-mirror (mirror control &rest arguments)
  (let ((stream (sb-ext:process-input (luvcraft-mirror-process mirror))))
    (apply #'format stream control arguments)
    (terpri stream)
    (finish-output stream)))

(defun spawn-luvcraft-mirror (&key (width 640) (height 400) (slots 3) executable)
  "Spawn a child game in a hidden WIDTH x HEIGHT window (in points), give it a
ring of SLOTS surfaces the size of its pixel frame, and return the mirror once
it is drawing."
  (let* ((process
           (sb-ext:run-program
            (namestring (or executable (luvcraft-mirror-executable)))
            '("--serve-surface")
            :input :stream :output :stream :wait nil
            :error (namestring
                    (merge-pathnames "build/luvcraft-mirror.log"
                                     (asdf:system-source-directory "luvcraft")))
            :if-error-exists :supersede))
         (mirror (make-instance 'luvcraft-mirror :process process)))
    (handler-bind ((error (lambda (condition)
                            (declare (ignore condition))
                            (stop-luvcraft-mirror mirror))))
      (tell-luvcraft-mirror mirror "size ~D ~D" width height)
      (let ((extent (hear-luvcraft-mirror mirror "extent")))
        (unless extent
          (error "The mirror child never reported its frame extent; see build/luvcraft-mirror.log."))
        (destructuring-bind (word pixel-width pixel-height)
            (uiop:split-string extent)
          (declare (ignore word))
          (setf (luvcraft-mirror-width mirror) (parse-integer pixel-width)
                (luvcraft-mirror-height mirror) (parse-integer pixel-height)
                (luvcraft-mirror-surfaces mirror)
                (coerce (loop repeat slots
                              collect (luv.metal:create-iosurface
                                       (luvcraft-mirror-width mirror)
                                       (luvcraft-mirror-height mirror)))
                        'vector))))
      (tell-luvcraft-mirror mirror "surfaces~{ ~D~}"
                            (map 'list #'luv.metal:iosurface-id
                                 (luvcraft-mirror-surfaces mirror)))
      (unless (hear-luvcraft-mirror mirror "ready")
        (error "The mirror child did not become ready; see build/luvcraft-mirror.log.")))
    mirror))

(defun hear-luvcraft-mirror (mirror word)
  "Read the child's lines until one starts with WORD; NIL if it never does.
The child's stdout also carries the ordinary startup chatter of the game."
  (loop for line = (read-line (sb-ext:process-output
                               (luvcraft-mirror-process mirror))
                              nil nil)
        while line
        when (uiop:string-prefix-p word line)
          return line))

(defun request-luvcraft-mirror-frame (mirror &optional slot)
  "Ask the child for one frame into SLOT (default: the slot after the last
completed one) and return the slot once its pixels are in the surface."
  (let ((slot (or slot
                  (mod (1+ (or (luvcraft-mirror-completed-slot mirror) -1))
                       (luvcraft-mirror-slot-count mirror)))))
    (tell-luvcraft-mirror mirror "frame ~D" slot)
    (let ((answer (hear-luvcraft-mirror mirror "done")))
      (unless answer
        (error "The mirror child did not finish a frame; see build/luvcraft-mirror.log."))
      (incf (luvcraft-mirror-frames mirror))
      (setf (luvcraft-mirror-completed-slot mirror) slot)
      slot)))

(defun stop-luvcraft-mirror (mirror)
  (let ((process (luvcraft-mirror-process mirror)))
    (when (sb-ext:process-alive-p process)
      (ignore-errors (tell-luvcraft-mirror mirror "quit"))
      (sb-ext:process-wait process)
      (sb-ext:process-close process))
    (map nil #'luv.metal:release-iosurface (luvcraft-mirror-surfaces mirror))
    (setf (luvcraft-mirror-surfaces mirror) #())
    (values)))

(defun luvcraft-mirror-pixel (mirror x y)
  "The child's pixel at X, Y as (B G R A) bytes."
  (luv.metal:read-iosurface-pixel (luvcraft-mirror-surface mirror) x y))

(defun luvcraft-mirror-pixels (mirror)
  "The child's whole frame as tightly packed BGRA bytes, copied out of the surface."
  (let* ((surface (luvcraft-mirror-surface mirror))
         (width (luvcraft-mirror-width mirror))
         (height (luvcraft-mirror-height mirror))
         (pixels (make-array (* 4 width height) :element-type '(unsigned-byte 8))))
    (luv.metal:with-locked-iosurface (surface :read-only t)
      (let ((base (luv.metal:iosurface-base-address surface))
            (stride (luv.metal:iosurface-bytes-per-row surface)))
        (dotimes (y height)
          (dotimes (i (* 4 width))
            (setf (aref pixels (+ (* y 4 width) i))
                  (cffi:mem-ref base :uint8 (+ (* y stride) i)))))))
    pixels))

(defun save-luvcraft-mirror-png (mirror pathname)
  "Write the child's current frame to PATHNAME."
  (ensure-directories-exist pathname)
  (write-rgba-png pathname (luvcraft-mirror-pixels mirror)
                  (luvcraft-mirror-width mirror) (luvcraft-mirror-height mirror)
                  :bgra8-unorm))
