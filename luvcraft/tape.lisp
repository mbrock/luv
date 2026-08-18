;;; The tape: a film reel that fetches a YouTube video and becomes a film.
;;;
;;; A tape is a placeable block.  Focusing one asks for a YouTube code (the
;;; question itself is a McCLIM panel supplied by LUVCRAFT/CLIM through
;;; OPEN-TAPE-PROMPT); answering starts yt-dlp in the background and hangs a
;;; progress orb over the block -- a sphere of bright motes that fills from
;;; the bottom as the download comes in.  When the file lands the tape is
;;; replaced by a FILM: a per-instance block kind carrying the video's
;;; metadata and the path of the media, which the player picks up into the
;;; inventory by hitting it, and can put down again anywhere.
;;;
;;; The download job is an overlay that draws nothing: it hangs on the session
;;; so REFRESH-LUVCRAFT-OVERLAY polls the process once per frame, on the
;;; render thread, which is where the world may be edited and particles emitted.

(in-package #:luvcraft)

;;; ---------------------------------------------------------------------
;;; Where films live and how yt-dlp is found.

(defun luvcraft-films-directory ()
  "The directory downloaded films are kept in, beside the saved worlds."
  (merge-pathnames
   #P"luvcraft/films/"
   (let ((data-home (uiop:getenv "XDG_DATA_HOME")))
     (if data-home
         (uiop:ensure-directory-pathname (pathname data-home))
         (merge-pathnames #P".local/share/" (user-homedir-pathname))))))

(defun yt-dlp-program ()
  "The yt-dlp executable: the one Nix names in LUV_YT_DLP, else whichever
is on the PATH."
  (let ((named (uiop:getenv "LUV_YT_DLP")))
    (if (and named (probe-file named))
        named
        "yt-dlp")))

(defparameter *tape-format*
  "b[ext=mp4][vcodec^=avc1][height<=720]/bv*[ext=mp4][vcodec^=avc1][height<=720]+ba[ext=m4a]/b[ext=mp4]/b"
  "The yt-dlp format selector for a tape: an H.264 MP4 no taller than 720
lines, whole if the site has one, merged from picture and sound if not,
whatever it has otherwise.  A whole file first because it is what the
plainest player clients hand out without a fight; see *TAPE-PLAYER-CLIENTS*.")

(defparameter *tape-player-clients* "mweb,android,default"
  "Which YouTube player clients yt-dlp asks, in order of precedence.

The default client's segmented formats answer 403 as often as not this
season, and yt-dlp does not fall back once a download has begun; the mobile
web and Android clients hand out a plain progressive MP4 that arrives.  A
knob rather than a constant, since this is YouTube's weather, not ours.")

(defun youtube-video-id (text)
  "Extract the eleven-character video id from TEXT, which may be a bare code
or any of the usual YouTube URLs; NIL when there is none."
  (let* ((text (string-trim '(#\Space #\Tab #\Newline #\Return) text))
         (id-char-p (lambda (c) (or (alphanumericp c) (find c "-_")))))
    (flet ((id-at (start)
             (let ((end (or (position-if-not id-char-p text :start start)
                            (length text))))
               (when (= (- end start) 11)
                 (subseq text start end)))))
      (cond ((and (= (length text) 11) (every id-char-p text)) text)
            ((search "v=" text)
             (id-at (+ 2 (search "v=" text))))
            ((search "youtu.be/" text)
             (id-at (+ 9 (search "youtu.be/" text))))
            ((search "/shorts/" text)
             (id-at (+ 8 (search "/shorts/" text))))
            ((search "/embed/" text)
             (id-at (+ 7 (search "/embed/" text))))
            (t nil)))))

;;; ---------------------------------------------------------------------
;;; The film: a block kind per video.

(defclass film-block-kind (block-kind)
  ((video-id :initarg :video-id :reader film-video-id)
   (title :initarg :title :initform nil :accessor film-title)
   (uploader :initarg :uploader :initform nil :accessor film-uploader)
   (duration :initarg :duration :initform nil :accessor film-duration
             :documentation "Seconds, or NIL when the site did not say.")
   (upload-date :initarg :upload-date :initform nil :accessor film-upload-date
                :documentation "YYYYMMDD as a string, or NIL.")
   (pathname :initarg :pathname :initform nil :accessor film-pathname
             :documentation "Where the media is, or NIL if it never landed."))
  (:metaclass luv.arithmetic.records:quantity-class)
  (:documentation
   "One downloaded video as a thing in the world and in the bag.

Every film shares the name :FILM and the reel's tiles; what differs is the
video, so a film is its own block kind rather than one entry in the palette,
and the world's palette-by-identity storage keeps them apart for free."))

(defmethod block-kind-carried-p ((block film-block-kind)) t)

(defvar *films* (make-hash-table :test 'equal)
  "Every film this image knows, by video id, so a saved world and a saved
bag that name the same video get the same object back.")

(defun ensure-film (video-id &key title uploader duration upload-date pathname)
  "Return the film for VIDEO-ID, making it or filling in what is now known."
  (check-type video-id string)
  (let ((film (gethash video-id *films*)))
    (unless film
      (setf film
            (make-instance
             'film-block-kind
             :name :film :video-id video-id
             :face-tiles '(:front 32 :back 32 :top 31 :bottom 31
                           :left 31 :right 31)
             :categories '(:building) :display-color '(0.62 0.55 0.42)
             :placeable-p nil)
            (gethash video-id *films*) film))
    (when title (setf (film-title film) title))
    (when uploader (setf (film-uploader film) uploader))
    (when duration (setf (film-duration film) duration))
    (when upload-date (setf (film-upload-date film) upload-date))
    (when pathname (setf (film-pathname film) pathname))
    film))

(defun film-label (film)
  "A short line naming FILM for a title bar or a bag."
  (or (film-title film) (film-video-id film)))

(defmethod block-save-description ((film film-block-kind))
  (list :film
        :video-id (film-video-id film)
        :title (film-title film)
        :uploader (film-uploader film)
        :duration (film-duration film)
        :upload-date (film-upload-date film)
        :pathname (and (film-pathname film)
                       (namestring (film-pathname film)))))

(defmethod restore-block-save-description ((kind (eql :film)) description)
  (let ((video-id (description-value description :video-id "film value")))
    (unless (stringp video-id)
      (invalid-luvcraft-save "A film's video id must be a string, not ~S."
                             video-id))
    (flet ((field (key) (description-value description key "film value"
                                           :optional t :default nil)))
      (ensure-film video-id
                   :title (field :title) :uploader (field :uploader)
                   :duration (field :duration) :upload-date (field :upload-date)
                   :pathname (let ((name (field :pathname)))
                               (and name (pathname name)))))))

;;; ---------------------------------------------------------------------
;;; The download: yt-dlp in the background, read by one thread.

(defclass tape-download ()
  ((session :initarg :session :reader tape-download-session)
   (x :initarg :x :reader tape-download-x)
   (y :initarg :y :reader tape-download-y)
   (z :initarg :z :reader tape-download-z)
   (video-id :initarg :video-id :reader tape-download-video-id)
   (process :initform nil :accessor tape-download-process)
   (reader :initform nil :accessor tape-download-reader)
   ;; Written by the reader thread, read by the render thread: each is one
   ;; word or one fresh object, never a structure edited in place.
   (fraction :initform 0.0 :accessor tape-download-fraction
             :documentation "0..1 of the current stream.")
   (streams-done :initform 0 :accessor tape-download-streams-done)
   (metadata :initform nil :accessor tape-download-metadata
             :documentation "A plist of the video's facts once printed.")
   (file :initform nil :accessor tape-download-file
         :documentation "The final media pathname once yt-dlp has moved it.")
   (log :initform nil :accessor tape-download-log
        :documentation "The last few lines yt-dlp said, newest first.")
   (finished-p :initform nil :accessor tape-download-finished-p)
   (age :initform 0.0 :accessor tape-download-age)
   (spin :initform 0.0 :accessor tape-download-spin)
   (last-refresh :initform nil :accessor tape-download-last-refresh
                 :documentation "Internal real time of the last frame."))
  (:documentation
   "One yt-dlp run for the tape at X,Y,Z, hung on SESSION as an overlay."))

(defmethod luvcraft-overlay-stage ((download tape-download))
  ;; Drawn by no pass: the orb is particles, and there is nothing else.
  :none)

(defun tape-download-progress (download)
  "How much of the whole download is done, 0..1.

yt-dlp fetches sound and picture as two streams for most videos and then
merges them, so a stream's own percentage runs to a hundred twice.  Until
the second one starts we assume there will be two; a single-stream video
simply arrives early."
  (let ((done (tape-download-streams-done download))
        (fraction (tape-download-fraction download)))
    (min 1.0 (/ (+ done fraction) (max 2 (1+ done))))))

(defun parse-yt-dlp-line (download line)
  "Fold one LINE of yt-dlp's stdout into DOWNLOAD's progress and facts."
  (let ((line (string-right-trim '(#\Return #\Newline) line)))
    (cond ((uiop:string-prefix-p "META" line)
           (destructuring-bind (&optional id title uploader duration date)
               (rest (uiop:split-string line :separator '(#\Tab)))
             (declare (ignore id))
             (setf (tape-download-metadata download)
                   (list :title title :uploader uploader
                         :duration (ignore-errors (parse-integer duration))
                         :upload-date date))))
          ((uiop:string-prefix-p "FILE" line)
           (let ((name (second (uiop:split-string line :separator '(#\Tab)))))
             (when (and name (plusp (length name)))
               (setf (tape-download-file download) (pathname name)))))
          ((uiop:string-prefix-p "[download]" line)
           (let* ((percent-end (position #\% line))
                  (percent-start
                    (and percent-end
                         (position #\Space line :end percent-end
                                                :from-end t))))
             (when (and percent-start percent-end)
               (let ((percent
                       (ignore-errors
                        (let ((*read-default-float-format* 'single-float))
                          (read-from-string
                           (subseq line (1+ percent-start) percent-end))))))
                 (when (realp percent)
                   (let ((fraction (/ (coerce percent 'single-float) 100.0)))
                     ;; A stream's summary line reads "100% of ... in ..." --
                     ;; that is the stream done, and the next starts at zero.
                     (if (search " in " line)
                         (setf (tape-download-fraction download) 0.0
                               (tape-download-streams-done download)
                               (1+ (tape-download-streams-done download)))
                         (setf (tape-download-fraction download)
                               fraction)))))))))
    (setf (tape-download-log download)
          (cons line (subseq (tape-download-log download)
                             0 (min 8 (length (tape-download-log download))))))
    download))

(defun start-tape-download (download)
  "Run yt-dlp for DOWNLOAD and start the thread that listens to it."
  (let* ((directory (luvcraft-films-directory))
         (video-id (tape-download-video-id download))
         (process
           (progn
             (ensure-directories-exist directory)
             (sb-ext:run-program
              (yt-dlp-program)
              (list "--no-playlist" "--no-simulate" "--progress" "--newline"
                    "--no-colors"
                    "--extractor-args"
                    (format nil "youtube:player_client=~A" *tape-player-clients*)
                    "-f" *tape-format*
                    "--merge-output-format" "mp4"
                    "-o" (namestring (merge-pathnames "%(id)s.%(ext)s"
                                                      directory))
                    "--print" (format nil "pre_process:META~C%(id)s~C%(title)s~C%(uploader)s~C%(duration)s~C%(upload_date)s"
                                      #\Tab #\Tab #\Tab #\Tab #\Tab)
                    "--print" (format nil "after_move:FILE~C%(filepath)s" #\Tab)
                    (format nil "https://www.youtube.com/watch?v=~A" video-id))
              :search t :input nil :output :stream :error :output :wait nil))))
    (setf (tape-download-process download) process
          (tape-download-reader download)
          (sb-thread:make-thread
           (lambda ()
             (unwind-protect
                  (with-open-stream (output (sb-ext:process-output process))
                    (loop for line = (read-line output nil nil)
                          while line
                          do (parse-yt-dlp-line download line)))
               (sb-ext:process-wait process)
               (setf (tape-download-finished-p download) t)))
           :name (format nil "tape ~A" video-id)))
    download))

(defun tape-download-succeeded-p (download)
  (let ((process (tape-download-process download))
        (file (tape-download-file download)))
    (and file
         (eql 0 (sb-ext:process-exit-code process))
         (probe-file file))))

;;; ---------------------------------------------------------------------
;;; The orb: motes on a sphere over the tape, filled to the progress.

(defparameter *tape-orb-height* 1.55
  "How far above the tape's floor the orb's centre floats.")
(defparameter *tape-orb-radius* 0.28)
(defparameter *tape-orb-motes-per-second* 600
  "How thickly the orb is sown; each mote lasts a fraction of a second.")

(defun emit-tape-orb (download seconds)
  "Sow this frame's motes over DOWNLOAD's tape: a slowly turning shell,
lit from the bottom up as far as the progress has come, with a faint
fountain of sparks off the top."
  (let* ((session (tape-download-session download))
         (system (luvcraft-session-particle-system session))
         (progress (tape-download-progress download))
         (age (incf (tape-download-age download) (coerce seconds 'single-float)))
         (spin (incf (tape-download-spin download) (* 0.9 (coerce seconds 'single-float))))
         (cx (+ (tape-download-x download) 0.5))
         (cy (+ (tape-download-y download) *tape-orb-height*
                (* 0.05 (sin (* 2.0 age)))))
         (cz (+ (tape-download-z download) 0.5))
         (count (round (* *tape-orb-motes-per-second* seconds))))
    (dotimes (index count)
      (let* ((u (fractional-part (+ (* index 0.618034) (* 7.13 age))))
             (v (fractional-part (+ (* index 0.414214) (* 3.71 age))))
             ;; Uniform on the sphere: height from the cosine, then a ring.
             (height (- (* 2.0 v) 1.0))
             (ring (sqrt (max 0.0 (- 1.0 (* height height)))))
             (angle (+ spin (* 2.0 pi u)))
             (lit-p (<= (* 0.5 (+ height 1.0)) (+ progress 0.02)))
             ;; A few sparks leave the crown, whichever way round they were.
             (spark-p (< (fractional-part (* 13.0 (+ u v))) 0.06)))
        (cond (lit-p
               (emit-block-mote
                system *orb-mote-block*
                (+ cx (* *tape-orb-radius* ring (cos angle)))
                (+ cy (* *tape-orb-radius* height))
                (+ cz (* *tape-orb-radius* ring (sin angle)))
                :size 0.045 :lifetime 0.28))
              (spark-p
               (emit-block-mote
                system *orb-mote-block*
                (+ cx (* 0.4 *tape-orb-radius* ring (cos angle)))
                (+ cy *tape-orb-radius*)
                (+ cz (* 0.4 *tape-orb-radius* ring (sin angle)))
                :size 0.02 :lifetime 0.5
                :velocity-x (* 0.3 (cos angle)) :velocity-y 0.7
                :velocity-z (* 0.3 (sin angle)))))))
    download))

(defun pop-tape-orb (download)
  "The orb bursts: one last spray of motes going every way."
  (let* ((session (tape-download-session download))
         (system (luvcraft-session-particle-system session))
         (cx (+ (tape-download-x download) 0.5))
         (cy (+ (tape-download-y download) *tape-orb-height*))
         (cz (+ (tape-download-z download) 0.5)))
    (dotimes (index 60)
      (let* ((u (fractional-part (* index 0.618034)))
             (v (fractional-part (* index 0.414214)))
             (height (- (* 2.0 v) 1.0))
             (ring (sqrt (max 0.0 (- 1.0 (* height height)))))
             (angle (* 2.0 pi u))
             (speed 2.5))
        (emit-block-mote system *orb-mote-block* cx cy cz
                         :velocity-x (* speed ring (cos angle))
                         :velocity-y (* speed height)
                         :velocity-z (* speed ring (sin angle))
                         :size 0.05 :lifetime 0.6 :gravity 3.0)))
    download))

;;; ---------------------------------------------------------------------
;;; The job on the session.

(defun find-tape-download (session x y z)
  (find-if (lambda (overlay)
             (and (typep overlay 'tape-download)
                  (= (tape-download-x overlay) x)
                  (= (tape-download-y overlay) y)
                  (= (tape-download-z overlay) z)))
           (luvcraft-session-overlays session)))

(defun begin-tape-download (session x y z video-id)
  "Start fetching VIDEO-ID for the tape at X,Y,Z and hang the job on SESSION."
  (check-type video-id string)
  (or (find-tape-download session x y z)
      (let ((download (make-instance 'tape-download
                                     :session session :x x :y y :z z
                                     :video-id video-id)))
        (start-tape-download download)
        (luv:log-event :luvcraft "tape at ~D,~D,~D fetching ~A"
                       x y z video-id)
        (add-luvcraft-overlay session download))))

(defun finish-tape-download (download)
  "The process is over: turn the tape into its film, or leave it a tape."
  (let* ((session (tape-download-session download))
         (world (luvcraft-session-world session))
         (x (tape-download-x download))
         (y (tape-download-y download))
         (z (tape-download-z download)))
    (cond ((tape-download-succeeded-p download)
           (let ((film (apply #'ensure-film (tape-download-video-id download)
                              :pathname (tape-download-file download)
                              (tape-download-metadata download))))
             (luv:log-event :luvcraft "tape at ~D,~D,~D is now the film ~S"
                            x y z (film-label film))
             (pop-tape-orb download)
             ;; The tape may have been dug out while it fetched; then the film
             ;; goes straight into the bag instead of into a hole.
             (if (typep (world-block-at world x y z) 'tape-block-kind)
                 (edit-block-at film world x y z)
                 (add-block-to-inventory
                  (luvcraft-session-inventory session) film 1))
             (request-luvcraft-session-checkpoint session)))
          (t
           (luv:log-event :luvcraft "tape at ~D,~D,~D failed to fetch ~A:~{~%  ~A~}"
                          x y z (tape-download-video-id download)
                          (reverse (tape-download-log download)))))
    (remove-luvcraft-overlay session download)
    download))

(defmethod refresh-luvcraft-overlay ((download tape-download) session)
  (declare (ignore session))
  (let* ((now (get-internal-real-time))
         (last (shiftf (tape-download-last-refresh download) now))
         (seconds (if last
                      (min 0.1 (/ (- now last) internal-time-units-per-second))
                      0.0)))
    (if (tape-download-finished-p download)
        (finish-tape-download download)
        (emit-tape-orb download seconds))))

(defmethod release-luvcraft-overlay ((download tape-download))
  (let ((process (tape-download-process download)))
    (when (and process (sb-ext:process-alive-p process))
      (sb-ext:process-kill process 15)))
  download)

;;; ---------------------------------------------------------------------
;;; Focusing a tape asks the question.

(defgeneric open-tape-prompt (session x y z)
  (:documentation
   "Show SESSION the question a tape at X,Y,Z asks -- which YouTube code? --
and return the focus that is asking it, or NIL when no presentation system
can.  LUVCRAFT/CLIM supplies the panel; the answer comes back through
BEGIN-TAPE-DOWNLOAD.")
  (:method (session x y z)
    (declare (ignore session x y z))
    nil))

(defmethod activate-luvcraft-target
    ((block tape-block-kind) (session luvcraft-session) hit)
  (let* ((coordinate (block-ray-hit-coordinate hit))
         (x (world-coordinate-x coordinate))
         (y (world-coordinate-y coordinate))
         (z (world-coordinate-z coordinate)))
    ;; A tape already fetching has been asked; the orb is its answer.
    (unless (find-tape-download session x y z)
      (open-tape-prompt session x y z))))
