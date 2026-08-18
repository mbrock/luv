;;; A Telegram terminal bolted to a luvcraft wall.
;;;
;;; The wall is the device.  Build a rectangle of terminal blocks, look at it,
;;; press TAB to focus, and switch it to Telegram: the same surface that runs
;;; a shell or plays a film now shows a conversation, drawn by McCLIM into the
;;; texture the block world samples.
;;;
;;; Two threads meet here and only one thing crosses between them.  A console
;;; owns a Telegram connection and a roster on its own thread, and the roster
;;; is never touched from outside it; the game thread posts requests to a
;;; mailbox and reads one slot, which the console fills with a finished,
;;; immutable view.  That is the whole concurrency story: a mailbox one way, a
;;; published snapshot the other, and no lock anywhere.  It is also why the
;;; text is wrapped and the timestamps are formatted on the console's thread
;;; -- by the time a frame repaints, there is nothing left to compute.

(in-package #:mcluv)

;;;; The panel
;;;;
;;;; Sizes are in texture pixels.  The surface's own rectangle decides how big
;;;; the thing is in the world; this decides how much of it is bezel.
;;;;
;;;; Two surfaces carry the panel and they are not the same shape: a wall is
;;;; wide, the phone in the hand is tall.  What differs between them is a
;;;; handful of edges -- the panel's extent, where the well ends and the
;;;; composer begins, how many characters fit on a line -- and those are
;;;; specials, bound from the frame's geometry around every paint and every
;;;; hit-test.  Everything that does not depend on the shape stays a constant.

(defstruct (communicator-geometry (:constructor make-communicator-geometry))
  (width 720 :type fixnum)
  (height 560 :type fixnum)
  (screen-bottom 470 :type fixnum)
  (composer-top 480 :type fixnum)
  (composer-bottom 540 :type fixnum)
  (text-columns 52 :type fixnum))

(defparameter *wall-communicator-geometry* (make-communicator-geometry)
  "The wall's panel: wide, with room for a long line.")

(defparameter *phone-communicator-geometry*
  (make-communicator-geometry :width 440 :height 545
                              :screen-bottom 455
                              :composer-top 465 :composer-bottom 525
                              :text-columns 40)
  "The phone's panel: the proportions of the slab in the hand, so the
texture is not squeezed sideways onto it.")

(defvar *communicator-width* 720)
(defvar *communicator-height* 560)
(defvar *communicator-screen-bottom* 470)
(defvar *communicator-composer-top* 480)
(defvar *communicator-composer-bottom* 540)
(defvar *communicator-text-columns* 52)

(defmacro with-communicator-geometry ((geometry) &body body)
  `(let* ((%geometry ,geometry)
          (*communicator-width* (communicator-geometry-width %geometry))
          (*communicator-height* (communicator-geometry-height %geometry))
          (*communicator-screen-bottom*
            (communicator-geometry-screen-bottom %geometry))
          (*communicator-composer-top*
            (communicator-geometry-composer-top %geometry))
          (*communicator-composer-bottom*
            (communicator-geometry-composer-bottom %geometry))
          (*communicator-text-columns*
            (communicator-geometry-text-columns %geometry)))
     ,@body))

(defconstant +communicator-inset+ 16)
(defconstant +communicator-header-bottom+ 78)
(defconstant +communicator-screen-top+ 86)
(defconstant +communicator-row-height+ 22)
(defconstant +communicator-avatar-size+ 32)
(defconstant +communicator-dialog-row-height+ 38)

(defparameter *communicator-bezel-ink* (make-rgb-color 0.55 0.51 0.43))
(defparameter *communicator-bezel-light* (make-rgb-color 0.73 0.69 0.59))
(defparameter *communicator-bezel-dark* (make-rgb-color 0.26 0.24 0.20))
(defparameter *communicator-screen-ink* (make-rgb-color 0.075 0.075 0.075))
(defparameter *communicator-row-ink* (make-rgb-color 0.125 0.125 0.125))
(defparameter *communicator-text-ink* (make-rgb-color 0.93 0.91 0.84))
(defparameter *communicator-muted-ink* (make-rgb-color 0.54 0.54 0.51))
(defparameter *communicator-accent-ink* (make-rgb-color 0.45 0.72 0.42))

(defparameter *communicator-sender-inks*
  (vector (make-rgb-color 0.42 0.68 0.93)
          (make-rgb-color 0.48 0.80 0.45)
          (make-rgb-color 0.92 0.50 0.44)
          (make-rgb-color 0.90 0.76 0.36)
          (make-rgb-color 0.72 0.58 0.92)
          (make-rgb-color 0.38 0.82 0.78)
          (make-rgb-color 0.94 0.62 0.34)
          (make-rgb-color 0.92 0.56 0.76))
  "Telegram gives every participant a colour and so does this.  The choice is
a hash of the name, so a person keeps their colour between sessions without
anything having to be stored.")

(defun communicator-name-hash (name)
  (let ((hash 5381))
    (loop for character across name
          do (setf hash (logand (+ (* hash 33) (char-code character))
                                most-positive-fixnum)))
    hash))

(defun communicator-sender-ink (name)
  (aref *communicator-sender-inks*
        (mod (communicator-name-hash name)
             (length *communicator-sender-inks*))))

;;;; What a frame paints
;;;;
;;;; One immutable value, built on the console's thread and read on the
;;;; game's.  Publishing it is a single SETF of a slot, which is why no lock
;;;; is needed: a repaint either sees the whole previous view or the whole
;;;; next one.

(defstruct (console-view (:constructor make-console-view))
  (generation 0 :type integer)
  (status "" :type string)
  (failure nil)
  (title "" :type string)
  (subtitle "" :type string)
  (dialogs '() :type list)
  (lines '() :type list)
  ;; While logging in: which answer the console is waiting for, and the
  ;; words to put above the field.  NIL once there is a conversation to show.
  (login nil)
  (prompt '() :type list)
  (secret-p nil))

(defstruct dialog-row
  (key nil)
  (label "" :type string)
  (preview "" :type string)
  (unread 0 :type integer))

;;;; Pictures
;;;;
;;;; FFmpeg opens a still exactly the way it opens a film -- one packet, one
;;;; frame -- so the decode path luv already has for video is also the one for
;;;; a photograph, and no JPEG reader has to exist here.  The word order turns
;;;; around on the way through: libav packs red in the low byte, and a CLIM
;;;; pattern wants it in the third.

(defconstant +communicator-photo-width+ 232)
(defconstant +communicator-photo-height+ 160)

(defun communicator-thumbnail-extent (photo)
  "The size PHOTO should be drawn at, fitted into the transcript's box."
  (let* ((width (max 1 (telegram.chat:chat-photo-width photo)))
         (height (max 1 (telegram.chat:chat-photo-height photo)))
         (scale (min 1.0
                     (/ +communicator-photo-width+ width)
                     (/ +communicator-photo-height+ height))))
    (values (max 1 (round (* width scale)))
            (max 1 (round (* height scale))))))

(defun rgba-words-pattern-array (words)
  (let* ((dimensions (array-dimensions words))
         (result (make-array dimensions :element-type '(unsigned-byte 32))))
    (dotimes (y (first dimensions) result)
      (dotimes (x (second dimensions))
        (let ((word (aref words y x)))
          (setf (aref result y x)
                (logior (ash (ldb (byte 8 24) word) 24)
                        (ash (ldb (byte 8 0) word) 16)
                        (ash (ldb (byte 8 8) word) 8)
                        (ldb (byte 8 16) word))))))))

(defun write-temporary-octets (bytes type)
  (let ((path (merge-pathnames
               (format nil "luvcraft-~36R.~A" (random (expt 2 48)) type)
               (uiop:temporary-directory))))
    (with-open-file (stream path :direction :output
                                 :element-type '(unsigned-byte 8)
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
      (write-sequence bytes stream))
    path))

(defun video-file-type (document)
  "An extension FFmpeg will recognize from the name alone.

The demuxer probes the content anyway, but a plausible suffix costs nothing
and makes a leftover temporary file identifiable."
  (let* ((name (telegram.chat:chat-document-file-name document))
         (dot (and name (position #\. name :from-end t))))
    (if (and dot (< (1+ dot) (length name)))
        (subseq name (1+ dot))
        "mp4")))

(defun decode-photo-pattern (bytes width height)
  "Decode JPEG BYTES into a WIDTH by HEIGHT CLIM pattern, or NIL."
  (let ((path (write-temporary-octets bytes "jpg")))
    (unwind-protect
         (let ((video (luv.libav:open-video path :hardware nil)))
           (unwind-protect
                (when (luv.libav:decode-next-frame video)
                  (make-pattern
                   (rgba-words-pattern-array
                    (luv.libav:frame-rgba-words video width height))
                   nil))
             (luv.libav:close-video video)))
      (ignore-errors (delete-file path)))))

(defstruct transcript-line
  ;; :HEAD carries the avatar, the name, and the time; :BODY is a wrapped
  ;; continuation; :PHOTO is a picture this client can locate but not yet
  ;; draw.
  (kind :body)
  (sender "" :type string)
  (ink nil)
  (time "" :type string)
  (text "" :type string)
  (out-p nil)
  (width 0 :type integer)
  (height 0 :type integer)
  (pattern nil)
  (document nil))

(defun communicator-clock-string (unix-seconds)
  "UNIX-SECONDS as a local HH:MM."
  (multiple-value-bind (second minute hour)
      (decode-universal-time (+ unix-seconds 2208988800))
    (declare (ignore second))
    (format nil "~2,'0D:~2,'0D" hour minute)))

(defun wrap-communicator-text (text columns)
  "TEXT broken into lines of at most COLUMNS characters, on spaces when it
can and mid-word when a word is longer than the whole line."
  (let ((lines '())
        (current (make-string-output-stream))
        (length 0))
    (flet ((flush ()
             (let ((line (get-output-stream-string current)))
               (when (plusp (length line)) (push line lines)))
             (setf length 0)))
      (dolist (paragraph (uiop:split-string text :separator '(#\Newline)))
        (dolist (word (remove "" (uiop:split-string paragraph
                                                    :separator '(#\Space))
                              :test #'string=))
          (loop while (> (length word) columns)
                do (flush)
                   (push (subseq word 0 columns) lines)
                   (setf word (subseq word columns)))
          (when (and (plusp length) (> (+ length 1 (length word)) columns))
            (flush))
          (when (plusp length)
            (write-char #\Space current)
            (incf length))
          (write-string word current)
          (incf length (length word)))
        (flush)))
    (or (nreverse lines) (list ""))))

;;;; The console
;;;;
;;;; A thread, a mailbox, and a slot.  The thread holds the only reference to
;;;; the roster that anything dereferences, and its Telegram connection is
;;;; bound thread-locally so that a console never disturbs whatever the
;;;; listener is doing with TELEGRAM.CLIENT:*CONNECTION*.

(defclass telegram-console ()
  ((roster :initform (telegram.chat:make-roster) :reader console-roster)
   (requests :initform (sb-concurrency:make-mailbox :name "telegram console")
             :reader console-requests)
   (thread :initform nil :accessor console-thread)
   (running-p :initform t :accessor console-running-p)
   (view :initform (make-console-view :status "starting") :accessor console-view)
   (selected :initform nil :accessor console-selected)
   (generation :initform 0 :accessor console-generation)
   ;; Photo id to a decoded pattern, or :UNAVAILABLE for one that will not
   ;; decode -- which has to be remembered too, or a broken picture is
   ;; refetched on every pass forever.
   (photos :initform (make-hash-table :test #'eql) :reader console-photos)
   (wanted-photos :initform '() :accessor console-wanted-photos)
   ;; A film the console has fetched and the render thread has not started
   ;; yet.  Opening it makes GPU resources, so the console only downloads.
   (pending-film :initform nil :accessor console-pending-film)
   ;; The last thing that went wrong, kept until something goes right.  A
   ;; failure passed to one PUBLISH-CONSOLE-VIEW would otherwise be erased by
   ;; the very next publish, which is a good way never to see an error.
   (failure :initform nil :accessor console-failure)
   (transcript-limit :initarg :transcript-limit :initform 40
                     :reader console-transcript-limit)
   (text-columns :initarg :text-columns :initform 52
                 :reader console-text-columns)
   (poll-interval :initarg :poll-interval :initform 2.0
                  :reader console-poll-interval)
   ;; The login, when there is one to do: which answer is wanted next, and
   ;; what has been gathered so far.  NIL means logged in, or trying to be.
   (login-stage :initform nil :accessor console-login-stage)
   (login-note :initform nil :accessor console-login-note)
   (pending-api-id :initform nil :accessor console-pending-api-id))
  (:documentation
   "One Telegram connection and roster, driven on its own thread, publishing
a finished view for a McCLIM frame to paint."))

(defun console-request (console name &optional argument)
  "Post a request to CONSOLE's thread.  Never blocks and never answers."
  (sb-concurrency:send-message (console-requests console)
                               (cons name argument))
  console)

(defun console-selected-peer (console)
  (let ((key (console-selected console)))
    (and key (telegram.chat:roster-peer (console-roster console) key))))

(defun console-peer-subtitle (peer)
  (cond ((null peer) "")
        ((typep peer 'telegram.chat:channel-peer)
         (if (telegram.chat:channel-peer-broadcast-p peer) "channel" "group"))
        ((typep peer 'telegram.chat:chat-peer) "group")
        ((telegram.chat:user-peer-bot-p peer) "bot")
        ((telegram.chat:peer-username peer)
         (format nil "@~A" (telegram.chat:peer-username peer)))
        (t "private chat")))

(defun console-transcript-lines (console peer)
  "PEER's recent history as finished, wrapped transcript lines."
  (let* ((roster (console-roster console))
         (history (telegram.chat:peer-history roster peer))
         (count (fill-pointer history))
         (start (max 0 (- count (console-transcript-limit console))))
         (lines '()))
    (loop for index from start below count
          for message = (aref history index)
          for sender = (telegram.chat:message-sender-label roster message)
          for ink = (communicator-sender-ink sender)
          for photo = (telegram.chat:chat-message-photo message)
          do (push (make-transcript-line
                    :kind :head :sender sender :ink ink
                    :time (communicator-clock-string
                           (telegram.chat:chat-message-date message))
                    :out-p (telegram.chat:chat-message-out-p message))
                   lines)
             (when photo
               (multiple-value-bind (thumb-width thumb-height)
                   (communicator-thumbnail-extent photo)
                 (let ((cached (gethash (telegram.chat:chat-photo-id photo)
                                        (console-photos console))))
                   (unless cached
                     (pushnew photo (console-wanted-photos console)
                              :key #'telegram.chat:chat-photo-id))
                   (push (make-transcript-line
                          :kind :photo :ink ink
                          :width thumb-width :height thumb-height
                          :pattern (and (typep cached 'pattern) cached))
                         lines))))
             (let ((document (telegram.chat:chat-message-document message)))
               (when (and document (telegram.chat:chat-document-video-p document))
                 (push (make-transcript-line
                        :kind :video :ink ink :document document
                        :text (telegram.chat:chat-document-label document))
                       lines)))
             (dolist (text (wrap-communicator-text
                            (telegram.chat:chat-message-text message)
                            (console-text-columns console)))
               (unless (and (or photo
                                (let ((d (telegram.chat:chat-message-document
                                          message)))
                                  (and d (telegram.chat:chat-document-video-p d))))
                            (zerop (length text)))
                 (push (make-transcript-line :kind :body :text text) lines))))
    (nreverse lines)))

(defun console-dialog-rows (console)
  (let ((roster (console-roster console)))
    (loop for peer in (telegram.chat:roster-order roster)
          repeat 24
          collect (make-dialog-row
                   :key (telegram.chat:peer-key peer)
                   :label (telegram.chat:peer-label peer)
                   :preview (console-peer-subtitle peer)
                   :unread (telegram.chat:peer-unread-count peer)))))

(defun publish-console-view (console &key status failure)
  "Build the next view and hand it to whoever repaints."
  ;; What is wanted is whatever this view turns out to reference; a picture
  ;; scrolled out of the transcript stops being worth a round trip.
  (setf (console-wanted-photos console) '())
  (when failure (setf (console-failure console) failure))
  (let ((peer (console-selected-peer console))
        (stage (console-login-stage console)))
    (setf (console-view console)
          (make-console-view
           :generation (incf (console-generation console))
           :status (or status
                       (if stage
                           "not logged in"
                           (let ((user telegram.client:*user*))
                             (if user
                                 (format nil "~A"
                                         (telegram.client:user-label user))
                                 "connected"))))
           :failure (console-failure console)
           :title (cond (stage "Telegram")
                        (peer (telegram.chat:peer-label peer))
                        (t "Conversations"))
           :subtitle (console-peer-subtitle peer)
           :dialogs (unless stage (console-dialog-rows console))
           :lines (and peer (not stage)
                       (console-transcript-lines console peer))
           :login stage
           :prompt (and stage (console-login-prompt console))
           :secret-p (eq stage :password))))
  console)

;;;; Logging in
;;;;
;;;; The client library keeps a login as a file: BEGIN-LOGIN writes the key
;;;; and the pending code hash to ~/.telegram-session, and COMPLETE-LOGIN and
;;;; COMPLETE-PASSWORD pick it up, in this process or another.  So the panel
;;;; needs no state of its own beyond which question it is asking, and a
;;;; player who quits between the code being sent and typed just gets asked
;;;; for the code again next time.  Credentials go the same way: the api_id
;;;; and hash typed here are written to ~/.telegram.env, where the library
;;;; already looks.

(defparameter *communicator-credential-file* "~/.telegram.env")

(defun console-login-prompt (console)
  "The lines shown above the field for the login stage the console is at."
  (let ((note (console-login-note console)))
    (append
     (ecase (console-login-stage console)
       (:api-id
        '("This game needs a Telegram application"
          "identity before it can log anyone in."
          ""
          "Get one at my.telegram.org/apps, then"
          "type its api_id here.  Both values are"
          "written to ~/.telegram.env."
          ""
          "api_id:"))
       (:api-hash '("Now the api_hash:"))
       (:phone
        '("Your phone number, with the country"
          "code, as +46701234567."
          ""
          "Phone:"))
       (:code
        '("Telegram has sent a code to another"
          "device, or by SMS.  Type it here."
          ""
          "Code:"))
       (:password
        '("This account has a password."
          ""
          "Password:")))
     (and note (list "" note)))))

(defun enter-login-stage (console stage &key note failure)
  (setf (console-login-stage console) stage
        (console-login-note console) note)
  (publish-console-view console :failure failure))

(defun write-communicator-credentials (api-id api-hash)
  (with-open-file (stream (merge-pathnames *communicator-credential-file*)
                          :direction :output :if-exists :supersede
                          :external-format :utf-8)
    (format stream "TELEGRAM_API_ID=~D~%TELEGRAM_API_HASH=~A~%"
            api-id api-hash)))

(defun finish-console-login (console)
  "The connection is current and authorized: load and show conversations."
  (setf (console-login-stage console) nil
        (console-login-note console) nil
        (console-failure console) nil)
  (publish-console-view console :status "loading…")
  (telegram.chat:refresh-roster-dialogs (console-roster console) :limit 40)
  (telegram.chat:synchronize-chat-updates (console-roster console))
  (publish-console-view console))

(defgeneric answer-console-login (console stage answer)
  (:documentation
   "Take ANSWER, the player's reply at login STAGE, on the console's thread.
Each stage either moves to the next or, on failure, stays and says why."))

(defmethod answer-console-login (console (stage (eql :api-id)) answer)
  (let ((id (ignore-errors (parse-integer answer))))
    (if id
        (progn (setf (console-pending-api-id console) id)
               (enter-login-stage console :api-hash))
        (enter-login-stage console :api-id
                           :failure "an api_id is a number"))))

(defmethod answer-console-login (console (stage (eql :api-hash)) answer)
  (write-communicator-credentials (console-pending-api-id console) answer)
  ;; A fresh identity from the file, replacing whatever the missing one
  ;; left behind, so BEGIN-LOGIN sees the credentials just written.
  (setf telegram.client:*application* nil)
  (enter-login-stage console :phone))

(defmethod answer-console-login (console (stage (eql :phone)) answer)
  (publish-console-view console :status "sending code…")
  (handler-case
      (let ((sent (telegram.client:begin-login
                   answer :stream (make-broadcast-stream))))
        (enter-login-stage
         console :code
         :note (format nil "sent by ~(~A~) to ~A"
                       (subseq (string (telegram.tl:tl-name
                                        (telegram.tl:tl-value sent :type)))
                               (length "AUTH.SENT-CODE-TYPE-"))
                       answer)))
    (error (condition)
      (let ((text (princ-to-string condition)))
        ;; A rejected identity is not the phone's fault: go back and ask
        ;; for the credentials again rather than leaving the player stuck.
        (enter-login-stage console
                           (if (search "API_ID_INVALID" text) :api-id :phone)
                           :failure text)))))

(defmethod answer-console-login (console (stage (eql :code)) answer)
  (publish-console-view console :status "signing in…")
  (handler-case
      (progn
        (telegram.client:complete-login
         answer :password-reader nil :stream (make-broadcast-stream))
        (finish-console-login console))
    (telegram.client:password-required (condition)
      (enter-login-stage
       console :password
       :note (alexandria:when-let
                 ((hint (telegram.client:password-required-hint condition)))
               (format nil "hint: ~A" hint))))
    (error (condition)
      (enter-login-stage console :code
                         :failure (princ-to-string condition)))))

(defmethod answer-console-login (console (stage (eql :password)) answer)
  (publish-console-view console :status "checking…")
  (handler-case
      (progn
        (telegram.client:complete-password
         answer :stream (make-broadcast-stream))
        (finish-console-login console))
    (error (condition)
      (enter-login-stage console :password
                         :note (console-login-note console)
                         :failure (princ-to-string condition)))))

(defgeneric apply-console-request (console name argument)
  (:documentation
   "Carry out one request on the console's own thread.  Returns true when the
published view should be rebuilt.

Adding a command the panel can ask for is a method here.")
  (:method (console name argument)
    (declare (ignore console name argument))
    nil)
  (:method (console (name (eql :select)) argument)
    (setf (console-selected console) argument)
    (let ((peer (console-selected-peer console)))
      (when peer
        (telegram.chat:refresh-peer-history (console-roster console) peer
                                            :limit (console-transcript-limit console))
        (telegram.chat:mark-peer-read peer)))
    t)
  (:method (console (name (eql :send)) argument)
    (let ((peer (console-selected-peer console)))
      (when (and peer (plusp (length argument)))
        (telegram.chat:send-chat-message (console-roster console)
                                         peer argument)))
    t)
  (:method (console (name (eql :play)) argument)
    ;; Fetch the whole film and leave it where the render thread will find
    ;; it.  Opening it here would make GPU resources on the wrong thread.
    (handler-case
        (setf (console-pending-film console)
              (write-temporary-octets
               (telegram.chat:download-chat-document argument)
               (video-file-type argument)))
      (error (condition)
        (publish-console-view
         console :failure (princ-to-string condition))))
    t)
  (:method (console (name (eql :login)) argument)
    (alexandria:when-let ((stage (console-login-stage console)))
      (setf (console-failure console) nil)
      (answer-console-login console stage argument))
    ;; ANSWER-CONSOLE-LOGIN publishes as it goes.
    nil)
  (:method (console (name (eql :refresh)) argument)
    (declare (ignore argument))
    (telegram.chat:refresh-roster-dialogs (console-roster console) :limit 40)
    (let ((peer (console-selected-peer console)))
      (when peer
        (telegram.chat:refresh-peer-history (console-roster console) peer
                                            :limit (console-transcript-limit console))))
    t))

(defun drain-console-requests (console)
  (let ((dirty nil))
    (dolist (request (sb-concurrency:receive-pending-messages
                      (console-requests console))
                     dirty)
      (when (apply-console-request console (car request) (cdr request))
        (setf dirty t)))))

(defun open-console-connection (console)
  "Resume the stored session and load enough to show something -- or, when
there is nothing to resume, start asking for what a login needs.

The connection is this thread's: TELEGRAM.CLIENT:*CONNECTION* is rebound
around the whole loop, so RESUME makes it current here and nowhere else."
  (publish-console-view console :status "connecting…")
  (let ((stored (telegram.client:load-session)))
    (cond
      ((null (ignore-errors (telegram.client:application-from-environment)))
       (enter-login-stage console :api-id))
      ((null stored)
       (enter-login-stage console :phone))
      ((getf stored :pending-code-hash)
       ;; A code was sent, in this run or an earlier one, and never typed.
       (enter-login-stage console :code
                          :note (format nil "for ~A"
                                        (getf stored :pending-phone))))
      (t
       (handler-case
           (progn (telegram.client:resume)
                  (finish-console-login console))
         (telegram.client:login-failed (condition)
           ;; The key is stale or was logged out elsewhere: start over.
           (enter-login-stage console :phone
                              :failure (princ-to-string condition))))))))

(defun fetch-console-photos (console &key (limit 2))
  "Download and decode a few of the pictures the last view asked for.

A handful per pass rather than all of them: a transcript full of photographs
would otherwise stall the update poll behind a queue of downloads, and the
pictures appearing over a second or two is the better failure."
  (let ((wanted (console-wanted-photos console))
        (fetched 0))
    (dolist (photo wanted (plusp fetched))
      (when (>= fetched limit) (return (plusp fetched)))
      (let ((id (telegram.chat:chat-photo-id photo)))
        (unless (gethash id (console-photos console))
          (setf (gethash id (console-photos console))
                (or (handler-case
                        (multiple-value-bind (width height)
                            (communicator-thumbnail-extent photo)
                          (decode-photo-pattern
                           (telegram.chat:download-chat-photo photo)
                           width height))
                      (error () nil))
                    :unavailable))
          (incf fetched))))))

(defun advance-telegram-console (console)
  (unless (or telegram.client:*connection* (console-login-stage console))
    (open-console-connection console))
  (when (console-login-stage console)
    ;; Nothing to poll while logging in; wait for the player to answer.
    (alexandria:when-let
        ((request (sb-concurrency:receive-message (console-requests console)
                                                  :timeout 0.25)))
      (apply-console-request console (car request) (cdr request)))
    (return-from advance-telegram-console))
  (let ((dirty (drain-console-requests console)))
    (when (telegram.chat:pull-chat-updates (console-roster console))
      (setf dirty t))
    (when dirty (publish-console-view console))
    ;; Pictures are fetched after the view that named them, so the text is on
    ;; the wall before the photographs land on it.
    (when (fetch-console-photos console)
      (publish-console-view console)))
  (sleep (console-poll-interval console)))

(defun run-telegram-console (console)
  "CONSOLE's thread.  Nothing in here touches McCLIM."
  (let ((telegram.client:*connection* nil)
        (telegram.client:*user* nil)
        (telegram.client:*application* nil))
    (unwind-protect
         (loop while (console-running-p console)
               do (handler-case (advance-telegram-console console)
                    (error (condition)
                      ;; A dropped socket is ordinary: Telegram closes an idle
                      ;; connection and the next call notices.  Forget it and
                      ;; the next pass resumes.
                      (publish-console-view
                       console :status "reconnecting…"
                       :failure (princ-to-string condition))
                      (ignore-errors (telegram.client:disconnect))
                      (sleep 3))))
      (ignore-errors (telegram.client:disconnect)))))

(defun start-telegram-console (&rest initargs)
  (let ((console (apply #'make-instance 'telegram-console initargs)))
    (setf (console-thread console)
          (sb-thread:make-thread (lambda () (run-telegram-console console))
                                 :name "luvcraft telegram console"))
    console))

(defun stop-telegram-console (console)
  (setf (console-running-p console) nil)
  (alexandria:when-let ((thread (console-thread console)))
    (when (sb-thread:thread-alive-p thread)
      (ignore-errors (sb-thread:join-thread thread :timeout 5))))
  (setf (console-thread console) nil)
  console)

;;;; Painting

(defclass communicator-pane (application-pane) ())

(define-application-frame luvcraft-communicator ()
  ((console :initarg :console :reader communicator-console)
   (display :initarg :display :initform nil :reader communicator-display)
   (geometry :initarg :geometry :initform *wall-communicator-geometry*
             :reader communicator-geometry)
   ;; Which of the two screens is showing, and what is half-typed.  Both are
   ;; the panel's own business and never leave the game thread.
   (screen :initform :dialogs :accessor communicator-screen)
   (draft :initform "" :accessor communicator-draft)
   ;; How far back the transcript is pushed, in pixels above the bottom.
   ;; Zero means pinned to the newest message, which is where a chat starts.
   (scroll :initform 0 :accessor communicator-scroll)
   (content-height :initform nil :accessor communicator-content-height-cache)
   (painted :initform nil :accessor communicator-painted))
  (:menu-bar nil)
  (:panes
   (communicator
    (make-pane 'communicator-pane
               :default-text-style (make-text-style :fix nil :normal))))
  (:layouts
   (default
    (horizontally (:width *communicator-width* :height *communicator-height*)
      communicator))))

(defun draw-communicator-plate
    (stream left top right bottom
     &key (ink *communicator-bezel-ink*) recessed-p (relief 0.0) (radius 4))
  "One bevelled panel, raised or recessed, optionally standing off the wall."
  (if (plusp relief)
      (draw-analytic-rounded-rectangle*
       stream left top right bottom :radius radius
       :ink (make-relief-design ink relief))
      (draw-rectangle* stream left top right bottom :ink ink))
  (let ((near (if recessed-p *communicator-bezel-dark* *communicator-bezel-light*))
        (far (if recessed-p *communicator-bezel-light* *communicator-bezel-dark*)))
    (draw-line* stream left top right top :ink near :line-thickness 2)
    (draw-line* stream left top left bottom :ink near :line-thickness 2)
    (draw-line* stream left bottom right bottom :ink far :line-thickness 2)
    (draw-line* stream right top right bottom :ink far :line-thickness 2)))

(defun draw-communicator-button (stream left top right bottom glyph)
  (draw-communicator-plate stream left top right bottom :relief 1.6 :radius 5)
  (draw-text* stream glyph
              (/ (+ left right) 2.0) (/ (+ top bottom) 2.0)
              :align-x :center :align-y :center :text-size 20
              :ink (make-rgb-color 0.16 0.15 0.13)))

(defun draw-communicator-avatar (stream name left top size)
  "A little generated head.  Telegram's own avatars are files this client
cannot fetch yet; a hash of the name at least gives every speaker a stable
face rather than a blank square."
  (let* ((hash (communicator-name-hash name))
         (ink (communicator-sender-ink name))
         (cells 8)
         (step (/ size cells)))
    (draw-rectangle* stream left top (+ left size) (+ top size)
                     :ink (make-rgb-color 0.16 0.16 0.16))
    (dotimes (row cells)
      (dotimes (column (ceiling cells 2))
        (when (logbitp (mod (+ (* row 3) column) 30) hash)
          (dolist (mirrored (list column (- cells 1 column)))
            (draw-rectangle*
             stream
             (+ left (* mirrored step)) (+ top (* row step))
             (+ left (* (1+ mirrored) step)) (+ top (* (1+ row) step))
             :ink ink)))))
    (draw-rectangle* stream left top (+ left size) (+ top size)
                     :filled nil :line-thickness 1
                     :ink *communicator-bezel-dark*)))

(defun draw-communicator-header (frame pane view)
  (let ((left +communicator-inset+)
        (right (- *communicator-width* +communicator-inset+)))
    ;; The header is part of the screen, not part of the bezel: cream text on
    ;; a lit stone frame has no contrast, and the device reads as one dark
    ;; pane behind one raised surround.
    (draw-communicator-plate pane left 14 right +communicator-header-bottom+
                             :ink *communicator-screen-ink* :recessed-p t)
    (let* ((chat-p (and (eq :chat (communicator-screen frame))
                        (not (console-view-login view))))
           (text-left (+ left (if chat-p 60 14))))
      (when chat-p
        (draw-communicator-button pane (+ left 6) 22 (+ left 48) 70 "‹"))
      (draw-text* pane (console-view-title view) text-left 34
                  :align-y :center :text-size 19 :ink *communicator-text-ink*)
      (draw-text* pane (if chat-p
                           (console-view-subtitle view)
                           (console-view-status view))
                  text-left 60
                  :align-y :center :text-size 12
                  :ink *communicator-muted-ink*))
    (draw-communicator-button pane (- right 96) 22 (- right 54) 70 "⌕")
    (draw-communicator-button pane (- right 48) 22 (- right 6) 70 "≡")))

(defun draw-communicator-dialogs (pane view)
  (let* ((left (+ +communicator-inset+ 4))
         (right (- *communicator-width* +communicator-inset+ 4))
         (top +communicator-screen-top+))
    (loop for row in (console-view-dialogs view)
          for index from 0
          for row-top = (+ top (* index +communicator-dialog-row-height+))
          while (< (+ row-top +communicator-dialog-row-height+)
                   *communicator-screen-bottom*)
          do (when (oddp index)
               (draw-rectangle* pane left row-top right
                                (+ row-top +communicator-dialog-row-height+)
                                :ink *communicator-row-ink*))
             (draw-communicator-avatar pane (dialog-row-label row)
                                       (+ left 6) (+ row-top 4) 30)
             (draw-text* pane (dialog-row-label row)
                         (+ left 46) (+ row-top 15)
                         :align-y :center :text-size 15
                         :ink (communicator-sender-ink (dialog-row-label row)))
             (draw-text* pane (dialog-row-preview row)
                         (+ left 46) (+ row-top 30)
                         :align-y :center :text-size 11
                         :ink *communicator-muted-ink*)
             (when (plusp (dialog-row-unread row))
               (draw-analytic-rounded-rectangle*
                pane (- right 46) (+ row-top 8) (- right 10) (+ row-top 30)
                :radius 10 :ink *communicator-accent-ink*)
               (draw-text* pane (format nil "~D" (dialog-row-unread row))
                           (- right 28) (+ row-top 19)
                           :align-x :center :align-y :center :text-size 12
                           :ink (make-rgb-color 0.05 0.1 0.05))))))

(defun transcript-line-extent (line)
  (case (transcript-line-kind line)
    (:head 24)
    (:photo (+ 8 (transcript-line-height line)))
    (:video 46)
    (t 18)))

(defun communicator-visible-height ()
  (- *communicator-screen-bottom* +communicator-screen-top+))

(defun communicator-content-height (view)
  (reduce #'+ (console-view-lines view)
          :key #'transcript-line-extent :initial-value 0))

(defun communicator-scroll-limit (view)
  "How far back the transcript can be pushed before it runs out of history."
  (max 0 (- (communicator-content-height view) (communicator-visible-height))))

(defun communicator-transcript-layout (view &optional (scroll 0))
  "VIEW's transcript as (LINE TOP HEIGHT) triples, SCROLL pixels back.

Bottom-anchored like every chat: at scroll zero the newest line sits against
the composer, and scrolling moves the whole column down to uncover older
ones.  Drawing and hit-testing both read this, which is the only way a click
can land on the picture the player is actually looking at.

Lines that fall entirely outside the well are dropped; ones that straddle its
edge are kept and clipped when drawn, so scrolling moves smoothly instead of
a line at a time."
  (let* ((lines (console-view-lines view))
         (heights (mapcar #'transcript-line-extent lines))
         (total (reduce #'+ heights :initial-value 0))
         (available (communicator-visible-height))
         (y (+ +communicator-screen-top+
               (min 0 (- available total))
               (max 0 scroll))))
    (loop for line in lines
          for height in heights
          for top = y
          do (incf y height)
          when (and (< top *communicator-screen-bottom*)
                    (> (+ top height) +communicator-screen-top+))
            collect (list line top height))))

(defun draw-communicator-scrollbar (pane view scroll)
  "A slim mark on the right of the well showing where the transcript is."
  (let ((limit (communicator-scroll-limit view)))
    (when (plusp limit)
      (let* ((track-top (+ +communicator-screen-top+ 2))
             (track-bottom (- *communicator-screen-bottom* 2))
             (track (- track-bottom track-top))
             (total (communicator-content-height view))
             (thumb (max 18 (round (* track (/ (communicator-visible-height)
                                               total)))))
             ;; Scroll counts backwards from the bottom, so a scroll of zero
             ;; puts the thumb at the end of the track.
             (offset (round (* (- track thumb)
                               (- 1.0 (/ (min scroll limit) limit)))))
             (x (- *communicator-width* +communicator-inset+ 7)))
        (draw-rectangle* pane x track-top (+ x 4) track-bottom
                         :ink (make-rgb-color 0.14 0.14 0.14))
        (draw-rectangle* pane x (+ track-top offset) (+ x 4)
                         (+ track-top offset thumb)
                         :ink (make-rgb-color 0.42 0.42 0.40))))))

(defun adjust-communicator-scroll (frame view)
  "Keep a scrolled-back transcript over the same messages as new ones arrive.

Pinned to the bottom it stays pinned, which is what a chat should do; pushed
back, it holds its place instead of being dragged along by every arrival."
  (let ((total (communicator-content-height view))
        (previous (communicator-content-height-cache frame)))
    (when (and previous (> total previous) (plusp (communicator-scroll frame)))
      (incf (communicator-scroll frame) (- total previous)))
    (setf (communicator-content-height-cache frame) total)
    (setf (communicator-scroll frame)
          (max 0 (min (communicator-scroll frame)
                      (communicator-scroll-limit view))))))

(defun draw-communicator-transcript (pane frame view)
  "Paint the visible part of the transcript, clipped to the well."
  (adjust-communicator-scroll frame view)
  (draw-communicator-scrollbar pane view (communicator-scroll frame))
  (let ((left (+ +communicator-inset+ 6))
        (right (- *communicator-width* +communicator-inset+ 6)))
    (with-drawing-options
        (pane :clipping-region
              (make-rectangle* +communicator-inset+ +communicator-screen-top+
                               (- *communicator-width* +communicator-inset+ 10)
                               *communicator-screen-bottom*))
      (loop for (line y height) in (communicator-transcript-layout
                                    view (communicator-scroll frame))
          do (ecase (transcript-line-kind line)
               (:head
                (draw-communicator-avatar
                 pane (transcript-line-sender line) left (- y 2)
                 +communicator-avatar-size+)
                (draw-text* pane (transcript-line-sender line)
                            (+ left 42) (+ y 10)
                            :align-y :center :text-size 15
                            :ink (transcript-line-ink line))
                (draw-text* pane (transcript-line-time line)
                            right (+ y 10)
                            :align-x :right :align-y :center :text-size 11
                            :ink *communicator-muted-ink*))
               (:photo
                (let ((width (transcript-line-width line))
                      (height (transcript-line-height line))
                      (pattern (transcript-line-pattern line)))
                  (if pattern
                      (draw-pattern* pane pattern (+ left 42) y)
                      ;; Still downloading, or a picture that would not
                      ;; decode: keep its exact footprint so the transcript
                      ;; does not jump when it arrives.
                      (draw-rectangle* pane (+ left 42) y
                                       (+ left 42 width) (+ y height)
                                       :ink (make-rgb-color 0.16 0.16 0.16)))
                  (draw-rectangle* pane (+ left 42) y
                                   (+ left 42 width) (+ y height)
                                   :filled nil :line-thickness 1
                                   :ink *communicator-bezel-dark*)))
               (:video
                (let ((document (transcript-line-document line)))
                  (draw-communicator-plate
                   pane (+ left 42) y (+ left 42 260) (+ y 38)
                   :ink (make-rgb-color 0.17 0.17 0.19) :recessed-p t)
                  (draw-communicator-button
                   pane (+ left 48) (+ y 4) (+ left 80) (+ y 34) "▶")
                  (draw-text* pane (transcript-line-text line)
                              (+ left 90) (+ y 13)
                              :align-y :center :text-size 12
                              :ink *communicator-text-ink*)
                  (draw-text* pane
                              (format nil "~,1Fs  ~,1FMB"
                                      (telegram.chat:chat-document-duration
                                       document)
                                      (/ (telegram.chat:chat-document-size
                                          document)
                                         1048576.0))
                              (+ left 90) (+ y 28)
                              :align-y :center :text-size 10
                              :ink *communicator-muted-ink*)))
               (:body
                (draw-text* pane (transcript-line-text line)
                            (+ left 42) (+ y 9)
                            :align-y :center :text-size 14
                            :ink *communicator-text-ink*)))))))

(defun draw-communicator-login (pane view)
  "The login screen: what is being asked, in the well, above the field."
  (let ((left (+ +communicator-inset+ 18))
        (y (+ +communicator-screen-top+ 30)))
    (dolist (line (console-view-prompt view))
      (draw-text* pane line left y
                  :align-y :center :text-size 13
                  :ink (if (and (plusp (length line))
                                (char= #\: (char line (1- (length line)))))
                           *communicator-accent-ink*
                           *communicator-text-ink*))
      (incf y +communicator-row-height+))))

(defun communicator-field-text (frame view)
  "What the composer shows: the draft, or one dot per character of a secret."
  (let ((draft (communicator-draft frame)))
    (if (console-view-secret-p view)
        (make-string (length draft) :initial-element #\•)
        draft)))

(defun draw-communicator-composer (frame pane view)
  (let ((left +communicator-inset+)
        (right (- *communicator-width* +communicator-inset+))
        (draft (communicator-field-text frame view)))
    (draw-communicator-button pane left *communicator-composer-top*
                              (+ left 44) *communicator-composer-bottom* "+")
    (draw-communicator-plate pane (+ left 52) (+ *communicator-composer-top* 4)
                             (- right 60) (- *communicator-composer-bottom* 4)
                             :ink (make-rgb-color 0.11 0.11 0.11)
                             :recessed-p t)
    (if (plusp (length draft))
        (draw-text* pane draft (+ left 64)
                    (/ (+ *communicator-composer-top*
                          *communicator-composer-bottom*)
                       2.0)
                    :align-y :center :text-size 15
                    :ink *communicator-text-ink*)
        (draw-text* pane (if (console-view-login view) "" "Message…") (+ left 64)
                    (/ (+ *communicator-composer-top*
                          *communicator-composer-bottom*)
                       2.0)
                    :align-y :center :text-size 15
                    :ink *communicator-muted-ink*))
    ;; The caret sits after the text rather than inside it, which is all a
    ;; single-line composer with no selection needs.
    (let ((caret-x (+ left 66 (* 8.4 (length draft)))))
      (draw-line* pane caret-x (+ *communicator-composer-top* 14)
                  caret-x (- *communicator-composer-bottom* 14)
                  :ink *communicator-accent-ink* :line-thickness 2))
    (draw-communicator-button pane (- right 52) *communicator-composer-top*
                              (- right 8) *communicator-composer-bottom* "➤")))

(defmethod handle-repaint ((pane communicator-pane) region)
  (declare (ignore region))
  (let* ((frame (pane-frame pane))
         (view (console-view (communicator-console frame))))
    (with-communicator-geometry ((communicator-geometry frame))
     (with-bounding-rectangle* (left top right bottom) pane
      (with-sheet-medium (medium pane)
        (when (typep medium 'luv-raster-medium)
          (clear-raster-medium-reliefs medium))
        ;; The bezel is the body of the device; everything else is inside it.
        (draw-rectangle* medium left top right bottom
                         :ink *communicator-bezel-ink*)
        (draw-rectangle* pane (+ left 3) (+ top 3) (- right 3) (- bottom 3)
                         :filled nil :line-thickness 3
                         :ink *communicator-bezel-light*)
        (draw-rectangle* pane (+ left 6) (+ top 6) (- right 6) (- bottom 6)
                         :filled nil :line-thickness 2
                         :ink *communicator-bezel-dark*)
        (draw-communicator-header frame pane view)
        (draw-communicator-plate
         pane +communicator-inset+ (- +communicator-screen-top+ 4)
         (- *communicator-width* +communicator-inset+)
         (+ *communicator-screen-bottom* 4)
         :ink *communicator-screen-ink* :recessed-p t)
        (cond ((console-view-login view)
               (draw-communicator-login pane view))
              ((eq :chat (communicator-screen frame))
               (draw-communicator-transcript pane frame view))
              (t (draw-communicator-dialogs pane view)))
        (draw-communicator-composer frame pane view)
        (alexandria:when-let ((failure (console-view-failure view)))
          (draw-text* pane (subseq failure 0 (min 70 (length failure)))
                      +communicator-inset+ (- *communicator-height* 8)
                      :align-y :center :text-size 10
                      :ink (make-rgb-color 0.85 0.42 0.36))))))
    (setf (communicator-painted frame)
          (list (console-view-generation view)
                (communicator-screen frame)
                (communicator-draft frame)
                (communicator-scroll frame)))))

(defun repaint-communicator (frame)
  (let ((mirror (sheet-direct-mirror (frame-top-level-sheet frame))))
    (if (typep mirror 'luv-gpu-mirror)
        (repaint-gpu-mirror mirror)
        (progn
          (repaint-sheet (mirror-sheet mirror) +everywhere+)
          (present-mirror mirror))))
  frame)

(defun communicator-paint-state (frame)
  (list (console-view-generation (console-view (communicator-console frame)))
        (communicator-screen frame)
        (communicator-draft frame)
        (communicator-scroll frame)))

;;;; The overlay on the wall

(defclass luvcraft-communicator-overlay (luvcraft-widget-overlay)
  ((display :initarg :display :reader communicator-overlay-display)))

(defmethod luvcraft:encode-luvcraft-overlay
    ((overlay luvcraft-communicator-overlay) session pass surface-texture)
  "Draw the panel flat on its wall, in the scene, with the world's depth."
  (let* ((mirror (widget-overlay-mirror overlay))
         (source (mirror-texture mirror)))
    (when source
      (ensure-spinning-compositor-resources
       overlay (mirror-context mirror) source
       :depth-format :depth32-float
       :target-format
       (luv:gpu-texture-format
        (luvcraft::luvcraft-session-color-texture session)))
      (place-widget-overlay-on-surface
       overlay (communicator-overlay-display overlay) session)
      (let* ((viewport-size
               (luv:canvas-extent (luvcraft::luvcraft-session-context session)))
             (state (world-device-clip-state
                     overlay session (first viewport-size)
                     (second viewport-size)))
             (frame-state
               (ensure-spinning-compositor-frame-state overlay surface-texture)))
        (setf (widget-overlay-render-state overlay) state)
        (luv:write-buffer (spinning-frame-state-buffer frame-state) state)
        (luv:set-pipeline pass (spinning-compositor-pipeline overlay))
        (luv:set-bind-group pass 0
                            (spinning-frame-state-bind-group frame-state))
        (luv:draw pass 4))))
  overlay)

(defmethod luvcraft:refresh-luvcraft-overlay
    ((overlay luvcraft-communicator-overlay) session)
  "Repaint only when the console has published something new, or the player
has typed.  This runs every frame, so it has to be cheap to say no."
  (declare (ignore session))
  (let* ((frame (widget-overlay-frame overlay))
         (console (communicator-console frame)))
    ;; A film the console finished fetching is started here, on the thread
    ;; that owns the device.  The wall stays in :TELEGRAM mode: the film
    ;; suppresses the panel while it runs and gives it back when it stops.
    (alexandria:when-let ((path (console-pending-film console)))
      (setf (console-pending-film console) nil)
      (let ((display (communicator-overlay-display overlay)))
        (handler-case
            (progn (luvcraft:play-terminal-display-film
                    display path :hardware :auto)
                   (setf (luvcraft:terminal-display-mode display) :telegram))
          (error (condition)
            ;; Say so on the panel rather than dropping it: a film that will
            ;; not open is the one thing the player is waiting on.
            (publish-console-view
             console :failure (princ-to-string condition))))))
    (unless (equal (communicator-paint-state frame)
                   (communicator-painted frame))
      (repaint-communicator frame)))
  overlay)

(defun communicator-texture-point (overlay event)
  "Where in the panel's own pixels a pointer event landed, or NIL."
  (alexandria:when-let
      ((uv (luvcraft-widget-texture-coordinate
            overlay
            (luv:canvas-pointer-event-x event)
            (luv:canvas-pointer-event-y event))))
    (with-communicator-geometry
        ((communicator-geometry (widget-overlay-frame overlay)))
      (list (* (first uv) *communicator-width*)
            (* (second uv) *communicator-height*)))))

(defun submit-communicator-draft (frame)
  "Enter, or the send button: an answer while logging in, else a message."
  (let* ((console (communicator-console frame))
         (view (console-view console))
         (draft (communicator-draft frame)))
    (cond ((console-view-login view)
           (when (plusp (length draft))
             (console-request console :login draft)
             (setf (communicator-draft frame) "")))
          ((eq :chat (communicator-screen frame))
           (console-request console :send draft)
           (setf (communicator-draft frame) "")))))

(defun scroll-communicator (frame key)
  "Move the transcript for one of the scrolling keys.

Clamping happens at paint time against the view that will actually be drawn,
so this only has to say which way and how far."
  (with-communicator-geometry ((communicator-geometry frame))
   (let ((view (console-view (communicator-console frame)))
        (page (- (communicator-visible-height) 24)))
    (setf (communicator-scroll frame)
          (max 0
               (min (communicator-scroll-limit view)
                    (case key
                      (:up (+ (communicator-scroll frame) 22))
                      (:down (- (communicator-scroll frame) 22))
                      (:page-up (+ (communicator-scroll frame) page))
                      (:page-down (- (communicator-scroll frame) page))
                      (:home (communicator-scroll-limit view))
                      (:end 0)
                      (t (communicator-scroll frame)))))))))

(defun communicator-video-line-at (view y scroll)
  "The playable video line at texture Y, if the click landed on one."
  (loop for (line top height) in (communicator-transcript-layout view scroll)
        when (and (eq :video (transcript-line-kind line))
                  (<= top y) (< y (+ top height)))
          return line))

(defun communicator-dialog-at (view y)
  (let ((index (floor (- y +communicator-screen-top+)
                      +communicator-dialog-row-height+)))
    (when (and (<= 0 index) (< y *communicator-screen-bottom*))
      (nth index (console-view-dialogs view)))))

(defmethod luvcraft:handle-luvcraft-overlay-event
    ((overlay luvcraft-communicator-overlay) session canvas
     (event luv:canvas-pointer-wheel-event))
  "Scroll the transcript under the pointer.

Only when the pointer is actually on the panel: a wheel turn aimed at the
world should not quietly move a screen on a wall somewhere behind it."
  (declare (ignore session canvas))
  (when (and (eq :chat (communicator-screen (widget-overlay-frame overlay)))
             (communicator-texture-point overlay event))
    (let ((frame (widget-overlay-frame overlay))
          (view (console-view (communicator-console
                               (widget-overlay-frame overlay)))))
      (with-communicator-geometry ((communicator-geometry frame))
        (setf (communicator-scroll frame)
              (max 0 (min (communicator-scroll-limit view)
                          (round (+ (communicator-scroll frame)
                                    (* 48 (luv:canvas-pointer-event-scroll-y
                                           event))))))))
      t)))

(defmethod luvcraft:handle-luvcraft-overlay-event
    ((overlay luvcraft-communicator-overlay) session canvas
     (event luv:canvas-pointer-event))
  (declare (ignore session canvas))
  (alexandria:when-let ((point (communicator-texture-point overlay event)))
    (when (and (typep event 'luv:canvas-pointer-button-press-event)
               (eq :left (luv:canvas-pointer-event-button event)))
      (destructuring-bind (x y) point
        (let* ((frame (widget-overlay-frame overlay))
               (console (communicator-console frame))
               (view (console-view console)))
         (with-communicator-geometry ((communicator-geometry frame))
          (cond
            ((>= y *communicator-composer-top*)
             (when (> x (- *communicator-width* +communicator-inset+ 56))
               (submit-communicator-draft frame)))
            ;; Nothing else on the login screen is a control.
            ((console-view-login view) nil)
            ;; The back button, which only exists on the conversation screen.
            ((and (eq :chat (communicator-screen frame))
                  (< y +communicator-header-bottom+)
                  (< x (+ +communicator-inset+ 54)))
             (setf (communicator-screen frame) :dialogs))
            ((< y +communicator-header-bottom+) nil)
            ((eq :dialogs (communicator-screen frame))
             (alexandria:when-let ((row (communicator-dialog-at view y)))
               (console-request console :select (dialog-row-key row))
               ;; A conversation opens at its newest message, not wherever
               ;; the last one happened to be scrolled to.
               (setf (communicator-screen frame) :chat
                     (communicator-scroll frame) 0
                     (communicator-content-height-cache frame) nil)))
            (t
             ;; A click inside a video's plate plays it on the wall.
             (alexandria:when-let
                 ((line (communicator-video-line-at
                         view y (communicator-scroll frame))))
               (console-request console :play
                                (transcript-line-document line)))))))))
    t))

(defmethod luvcraft:handle-luvcraft-focus-event
    ((overlay luvcraft-communicator-overlay) session canvas
     (event luv:canvas-key-press-event))
  (declare (ignore canvas))
  (let* ((frame (widget-overlay-frame overlay))
         (key (luv:canvas-key-event-key-name event))
         (character (luv:canvas-key-event-character event)))
    (case key
      ;; TAB belongs to the session: it is how the player leaves the wall,
      ;; and a composer that ate it would trap them at the screen.
      (:tab nil)
      ((:up :down :page-up :page-down :home :end)
       (scroll-communicator frame key)
       t)
      (:escape
       ;; One Escape leaves the conversation, the next leaves the wall.
       (if (eq :chat (communicator-screen frame))
           (setf (communicator-screen frame) :dialogs)
           (luvcraft:unfocus-luvcraft-session session))
       t)
      (:return
       (submit-communicator-draft frame)
       t)
      (:backspace
       (let ((draft (communicator-draft frame)))
         (when (plusp (length draft))
           (setf (communicator-draft frame)
                 (subseq draft 0 (1- (length draft))))))
       t)
      (t
       (when (and character (graphic-char-p character))
         (setf (communicator-draft frame)
               (concatenate 'string (communicator-draft frame)
                            (string character))))
       t))))

;;;; The wall mode

(defgeneric communicator-geometry-for (display)
  (:documentation
   "The panel shape that fits DISPLAY's surface.  A wall gets the wide
panel; the phone gets the tall one.  A new kind of surface adds a method.")
  (:method ((display luvcraft:terminal-display)) *wall-communicator-geometry*)
  (:method ((display luvcraft:phone-terminal-display))
    *phone-communicator-geometry*))

(defun open-luvcraft-communicator (display &key console)
  "Mount a Telegram panel on DISPLAY's surface -- a wall, or the phone."
  (let* ((session (luvcraft::terminal-display-session display))
         (port (find-port :server-path '(:luv)))
         (manager (or (first (climi::frame-managers port))
                      (make-instance 'luv-frame-manager :port port)))
         (geometry (communicator-geometry-for display))
         (console (or console
                      (start-telegram-console
                       :text-columns
                       (communicator-geometry-text-columns geometry))))
         (frame
           (let ((*embedded-mirror-target*
                   (luvcraft:luvcraft-session-canvas session))
                 (*embedded-mirror-context*
                   (luvcraft::luvcraft-session-context session))
                 (*embedded-mirror-device*
                   (luvcraft::luvcraft-session-device session)))
             (with-communicator-geometry (geometry)
               (make-application-frame
                'luvcraft-communicator :frame-manager manager :enable t
                :console console :display display :geometry geometry)))))
    (setf (frame-pretty-name frame) "telegram")
    (let* ((mirror (sheet-direct-mirror (frame-top-level-sheet frame)))
           (overlay
             (make-instance
              'luvcraft-communicator-overlay
              :session session :frame frame :mirror mirror :display display
              ;; A little relief, so the buttons and the bezel actually
              ;; stand off the surface instead of being painted on it.
              :height-scale 0.35)))
      (place-widget-overlay-on-surface overlay display session)
      (setf (mirror-compositor mirror) overlay
            (luvcraft:terminal-display-mode-overlay display) overlay)
      (repaint-communicator frame)
      overlay)))

(defmethod luvcraft:release-luvcraft-overlay
    ((overlay luvcraft-communicator-overlay))
  "Stop the console thread and close its Telegram connection.

This is what makes a mode switch safe: the overlay is dropped by whoever is
mounting the next one, and its thread has to go with it."
  (stop-telegram-console (communicator-console (widget-overlay-frame overlay)))
  (call-next-method))

(defun close-luvcraft-communicator (overlay)
  (let ((display (communicator-overlay-display overlay)))
    (when (eq overlay (luvcraft:terminal-display-mode-overlay display))
      (setf (luvcraft:terminal-display-mode-overlay display) nil)))
  (luvcraft:release-luvcraft-overlay overlay)
  nil)

;; Loading this system is what makes the wall offer a third mode, and what
;; makes the phone come out of the pocket as a messenger rather than a shell.
(pushnew :telegram *terminal-display-modes*)
(setf luvcraft:*phone-initial-mode* :telegram)
(setf *terminal-display-modes*
      (sort (copy-list *terminal-display-modes*) #'<
            :key (lambda (mode) (position mode '(:shell :film :telegram)))))

(defmethod luvcraft:change-terminal-display-mode
    ((display luvcraft:terminal-display)
     (session luvcraft:luvcraft-session) (mode (eql :telegram)))
  (luvcraft::stop-terminal-display-film display session)
  (setf (luvcraft:terminal-display-mode display) mode)
  (unless (displace-terminal-mode-overlay
           display 'luvcraft-communicator-overlay)
    (open-luvcraft-communicator display))
  display)
