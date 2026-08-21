(in-package #:luv.showcase)

;;; A world terminal photographed through its existing display ownership.
;;; Shell fixtures cross the real PTY/Ghostty path; Film fixtures cross the
;;; existing video-screen path and remain under the terminal faceplate.
;;; #2TMUKK #TWKA93

(defconstant +terminal-cinema-width+ 1200)
(defconstant +terminal-cinema-height+ 800)
(defconstant +terminal-cinema-film-seconds+ 3)
(defconstant +terminal-cinema-film-frame-rate+ 12)
(defconstant +terminal-cinema-clip-width+ 480)
(defconstant +terminal-cinema-clip-height+ 270)

(defun terminal-cinema-look-pose
    (x y z target-x target-y target-z field-of-view)
  "Return an unrolled camera pose looking from X/Y/Z at the target point."
  (let* ((dx (- target-x x))
         (dy (- target-y y))
         (dz (- target-z z))
         (flat (sqrt (+ (* dx dx) (* dz dz)))))
    (luvcraft::make-camera-pose
     (luvcraft::make-vec3 x y z)
     (atan dx dz)
     (atan dy flat)
     field-of-view)))

(defun terminal-cinema-environment-pose ()
  "The oblique shelter view shared by the shell and Film wall."
  (terminal-cinema-look-pose
   1.4d0 4.7d0 2.0d0
   7.0d0 3.4d0 12.0d0
   (* luvcraft::+luvcraft-camera-vertical-field-of-view+ 0.88d0)))

(defun make-terminal-cinema-camera ()
  (let ((camera (make-instance 'luvcraft:fly-camera)))
    (luvcraft::set-camera-pose camera (terminal-cinema-environment-pose))
    camera))

(defun make-terminal-cinema-world ()
  "Build the compact stone-and-wood shelter around one four-by-three wall."
  (let ((world
          (luvcraft::make-block-world
           :source (make-instance 'luvcraft::gazetteer-open-sky-source))))
    (luvcraft::ensure-world-chunk world 0 0 0)
    ;; Meadow floor, stone threshold, and a warm timber path pull the terminal
    ;; into a place without competing with its luminous surface.
    (loop for x below 16 do
      (loop for z below 16 do
        (setf (luvcraft:world-block-at world x 0 z)
              luvcraft::*grass-block*)))
    (loop for z from 3 to 12 do
      (loop for x from 5 to 9 do
        (setf (luvcraft:world-block-at world x 1 z)
              (if (oddp (+ x z))
                  luvcraft::*stone-block*
                  luvcraft::*bricks-block*))))
    ;; Back wall and deep frame.  The terminal sits one cell in front so its
    ;; :BACK face remains exposed toward the camera.
    (loop for x from 3 to 11 do
      (loop for y from 1 to 6 do
        (setf (luvcraft:world-block-at world x y 13)
              (if (or (= x 3) (= x 11) (= y 1) (= y 6))
                  luvcraft::*wood-block*
                  luvcraft::*stone-block*))))
    (loop for z from 10 to 14 do
      (loop for x from 2 to 12 do
        (setf (luvcraft:world-block-at world x 7 z)
              luvcraft::*planks-block*)))
    (dolist (post '((2 10) (12 10) (2 14) (12 14)))
      (destructuring-bind (x z) post
        (loop for y from 1 to 6 do
          (setf (luvcraft:world-block-at world x y z)
                luvcraft::*wood-block*))))
    (luvcraft:place-terminal-block-rectangle
     world 5 2 12 :back 4 3)
    (luvcraft:relight-block-world world)
    world))

(defun terminal-cinema-shell-script ()
  "Return the bounded credential-free shell program for the showcase PTY."
  (let ((escape (code-char 27)))
    (with-output-to-string (stream)
      (format stream "printf '~C[2J~C[H'~%" escape escape)
      (format stream
              "printf '%s\\r\\n' '~C[48;5;24m~C[1;38;5;230m  LUVCRAFT FIELD TERMINAL  ~C[0m'~%"
              escape escape escape)
      (format stream
              "printf '%s\\r\\n' '~C[38;5;109mstone shelter / terminal wall~C[0m'~%"
              escape escape)
      (format stream "printf '\\r\\n'~%")
      (format stream
              "printf '%s\\r\\n' '~C[1;38;5;221m$ scene inspect terminal~C[0m'~%"
              escape escape)
      (format stream
              "printf '%s\\r\\n' 'surface   ~C[38;5;81m4 x 3 blocks~C[0m'~%"
              escape escape)
      (format stream
              "printf '%s\\r\\n' 'grid      ~C[38;5;120mwhole-face fitted~C[0m'~%"
              escape escape)
      (format stream
              "printf '%s\\r\\n' 'renderer  ~C[38;5;213mGhostty + Slug~C[0m'~%"
              escape escape)
      (format stream
              "printf '%s\\r\\n' 'glass     ~C[1;38;5;117mfaceplate last~C[0m'~%"
              escape escape)
      (format stream "printf '\\r\\n'~%")
      (format stream
              "printf '%s\\r\\n' '~C[1;38;5;221m$ printf ready~C[0m'~%"
              escape escape)
      (format stream
              "printf '%s\\r\\n' '~C[1;32mready: source-owned PTY~C[0m'~%"
              escape escape)
      (format stream
              "printf '%s\\r\\n' '~C[38;5;245moffline / credential-free~C[0m'~%"
              escape escape))))

(defun attach-terminal-cinema-shell-fixture (display)
  "Run the finite styled fixture through DISPLAY's owned PTY and wait for it."
  (luvcraft:attach-terminal-display-pty
   display
   :program (or (uiop:getenv "LUV_BASH") "/bin/sh")
   :arguments (list "-c" (terminal-cinema-shell-script))
   :directory #P"/"
   :environment '("LC_ALL=C" "LANG=C"))
  (let* ((device (luvcraft:terminal-display-device display))
         (state (luv.terminal:wait-for-pty-device device :timeout 3.0)))
    (unless (eq state :exited)
      (error "Terminal cinema fixture did not exit cleanly: ~S." state)))
  display)

(defun call-with-terminal-cinema-session
    (function &key (day-fraction 0.72) (shell-p nil))
  "Call FUNCTION with SESSION and its source-owned world TERMINAL-DISPLAY."
  (call-with-gallery-session
   (lambda (session)
     (let ((display nil))
       (unwind-protect
            (progn
              (setf display
                    (luvcraft:open-terminal-display
                     session 5 2 12 :back
                     :rows-per-block 5 :fixture "" :font-scale 0.96))
              (when shell-p
                (format t "capture terminal cinema: running PTY fixture...~%")
                (finish-output)
                (attach-terminal-cinema-shell-fixture display)
                (format t "capture terminal cinema: PTY fixture ready~%")
                (finish-output))
              (funcall function session display))
         (when (and display
                    (member display
                            (luvcraft:luvcraft-session-overlays session)))
           (luvcraft:remove-luvcraft-overlay session display)))))
   :title "terminal cinema shelter"
   :width +terminal-cinema-width+ :height +terminal-cinema-height+
   :clean-p t :residency-radius 0
   :world (make-terminal-cinema-world)
   :camera (make-terminal-cinema-camera)
   :sky-clock (luvcraft::make-pinned-sky-clock day-fraction)
   :sky-profile (luvcraft:make-default-sky-profile)
   :exposure 0.58 :bloom-gain 0.20 :shaft-gain 0.16))

(defun fill-terminal-cinema-clip-frame (pixels frame frame-count)
  "Fill PIXELS with one deterministic stone-age car crossing a dusk cartoon."
  (let* ((width +terminal-cinema-clip-width+)
         (height +terminal-cinema-clip-height+)
         (progress (/ frame (coerce (max 1 (1- frame-count)) 'double-float)))
         (car-x (round (+ -100 (* progress (+ width 190))))))
    (dotimes (y height)
      (dotimes (x width)
        (let* ((sky-red (+ 48 (floor (* y 112) 220)))
               (sky-green (+ 32 (floor (* y 54) 220)))
               (sky-blue (- 112 (floor (* y 48) 220)))
               (sun-dx (- x 382))
               (sun-dy (- y 72))
               (hill-top (+ 146
                            (floor (abs (- (mod (+ x 73) 210) 105)) 5)))
               (wheel-a-x (+ car-x 23))
               (wheel-b-x (+ car-x 82))
               (wheel-y 221)
               (wheel-a-dx (- x wheel-a-x))
               (wheel-b-dx (- x wheel-b-x))
               (wheel-dy (- y wheel-y))
               (wheel-p (or (<= (+ (* wheel-a-dx wheel-a-dx)
                                   (* wheel-dy wheel-dy))
                                 169)
                            (<= (+ (* wheel-b-dx wheel-b-dx)
                                   (* wheel-dy wheel-dy))
                                 169)))
               (driver-dx (- x (+ car-x 57)))
               (driver-dy (- y 174))
               (driver-p (<= (+ (* driver-dx driver-dx)
                                (* driver-dy driver-dy))
                              144))
               (body-p (and (<= car-x x (+ car-x 105))
                            (<= 187 y 218)))
               (body-top-p
                 (and (<= (+ car-x 18) x (+ car-x 88))
                      (<= (+ 169 (floor (abs (- x (+ car-x 54))) 5))
                          y 198)))
               (offset (* 4 (+ x (* y width))))
               red green blue)
          (cond
            (wheel-p
             (setf red 55 green 43 blue 37))
            (driver-p
             (setf red 239 green 174 blue 108))
            ((or body-p body-top-p)
             (setf red 187 green 153 blue 103))
            ((>= y 220)
             (setf red 37 green 30 blue 42))
            ((>= y hill-top)
             (setf red 64 green 43 blue 73))
            ((<= (+ (* sun-dx sun-dx) (* sun-dy sun-dy)) 1024)
             (setf red 255 green 178 blue 82))
            ((and (< y 118)
                  (zerop (mod (+ (* x 17) (* y 31)) 997)))
             (setf red 242 green 222 blue 184))
            (t
             (setf red sky-red green sky-green blue sky-blue)))
          (setf (aref pixels offset) red
                (aref pixels (+ offset 1)) green
                (aref pixels (+ offset 2)) blue
                (aref pixels (+ offset 3)) 255))))
    pixels))

(defun render-terminal-cinema-clip (pathname)
  "Write the small deterministic H.264 fixture played by the world wall."
  (let* ((frame-rate +terminal-cinema-film-frame-rate+)
         (frame-count (* +terminal-cinema-film-seconds+ frame-rate))
         (pixels
           (make-array
            (* 4 +terminal-cinema-clip-width+ +terminal-cinema-clip-height+)
            :element-type '(unsigned-byte 8))))
    (format t "capture terminal cinema: generating ~D-frame wall clip...~%"
            frame-count)
    (finish-output)
    (luv:with-video-encoder
        (write-frame pathname
                     +terminal-cinema-clip-width+
                     +terminal-cinema-clip-height+
                     :frame-rate frame-rate)
      (dotimes (frame frame-count)
        (when (zerop (mod frame frame-rate))
          (format t "capture terminal cinema clip: second ~D/~D~%"
                  (1+ (/ frame frame-rate))
                  +terminal-cinema-film-seconds+)
          (finish-output))
        (fill-terminal-cinema-clip-frame pixels frame frame-count)
        (write-frame pixels)))
    pathname))

(defun call-with-terminal-cinema-clip (function)
  "Call FUNCTION with one generated clip, then remove its unique directory."
  (uiop:with-temporary-file
      (:pathname marker :prefix "luv-showcase-terminal-cinema-"
       :type :unspecific)
    (uiop:delete-file-if-exists marker)
    (let* ((directory (uiop:ensure-directory-pathname marker))
           (clip (merge-pathnames "flintstones-cinema-at-dusk.mp4" directory)))
      (ensure-directories-exist directory)
      (unwind-protect
           (progn
             (render-terminal-cinema-clip clip)
             (funcall function clip))
        (when (probe-file directory)
          (uiop:delete-directory-tree
           directory :validate t :if-does-not-exist :ignore))))))

(defun play-terminal-cinema-clip (display clip)
  "Put CLIP on DISPLAY through the bounded software Film path."
  (let ((screen
          (luvcraft:play-terminal-display-film
           display clip :hardware nil)))
    ;; Chapel's RADV H.264 video queue currently loses the whole graphics
    ;; device on its first decoded picture.  This authored 480x270 fixture is
    ;; intentionally forced through the ordinary software upload path until
    ;; that backend failure has its own proof and fix; :AUTO would still try
    ;; the guilty hardware path first.
    (when (luvcraft::video-screen-hardware-p screen)
      (error "Terminal cinema Film mode unexpectedly used hardware decode."))
    screen))

(luv:define-capture terminal-camp-golden-hour
    (:figure 2TMUKK :kind :image :extension "png"
     :description
     "An actual styled PTY glowing obliquely inside its stone-and-wood shelter.")
    (pathname)
  (call-with-terminal-cinema-session
   (lambda (session display)
     (declare (ignore display))
     (luvcraft:capture-luvcraft-screenshot
      session pathname :include-hud-p nil :include-viewmodel-p nil))
   :day-fraction 0.72 :shell-p t))

(luv:define-capture terminal-shell-closeup
    (:figure 2TMUKK :kind :image :extension "png"
     :description
     "The same styled PTY from the terminal's ordinary off-axis focus framing.")
    (pathname)
  (call-with-terminal-cinema-session
   (lambda (session display)
     (luvcraft:capture-luvcraft-screenshot
      session pathname
      :camera-pose
      (luvcraft::terminal-focus-camera-pose
       (luvcraft:terminal-display-surface display)
       +terminal-cinema-width+ +terminal-cinema-height+)
      :include-hud-p nil :include-viewmodel-p nil))
   :day-fraction 0.72 :shell-p t))

(luv:define-capture wall-cinema-at-dusk
    (:figure TWKA93 :kind :image :extension "png"
     :description
     "An authored stone-age cartoon letterboxed inside the dusk terminal wall.")
    (pathname)
  (call-with-terminal-cinema-clip
   (lambda (clip)
     (call-with-terminal-cinema-session
      (lambda (session display)
        (play-terminal-cinema-clip display clip)
        (luvcraft:capture-luvcraft-screenshot
         session pathname :include-hud-p nil :include-viewmodel-p nil))
      :day-fraction 0.76))))

(luv:define-capture wall-cinema-at-dusk-film
    (:figure TWKA93 :kind :video :extension "mp4"
     :description
     "Three seconds of authored video moving behind the terminal faceplate.")
    (pathname)
  (call-with-terminal-cinema-clip
   (lambda (clip)
     (call-with-terminal-cinema-session
      (lambda (session display)
        (play-terminal-cinema-clip display clip)
        (luvcraft:film-luvcraft-session
         session pathname
         :seconds +terminal-cinema-film-seconds+
         :frame-rate +terminal-cinema-film-frame-rate+
         :include-hud-p nil :include-viewmodel-p nil
         :before-frame
         (lambda (frame)
           (setf (luvcraft::luvcraft-session-last-frame-time session)
                 (/ frame
                    (coerce +terminal-cinema-film-frame-rate+ 'double-float)))
           (when (zerop (mod frame +terminal-cinema-film-frame-rate+))
             (format t "capture wall-cinema-at-dusk-film: second ~D/~D~%"
                     (1+ (/ frame +terminal-cinema-film-frame-rate+))
                     +terminal-cinema-film-seconds+)
             (finish-output)))))
      :day-fraction 0.76))))
