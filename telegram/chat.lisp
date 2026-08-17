;;;; Conversations.
;;;;
;;;; INVOKE answers a question; it does not hold a conversation.  Telegram's
;;;; message methods speak InputPeer, which carries an access hash you can
;;;; only have been told, and they answer with three parallel vectors --
;;;; messages, chats, users -- that mean nothing until they are joined.  This
;;;; file does the joining and keeps the result: a roster of peers with their
;;;; hashes, a history per peer, and the update cursor that turns "what there
;;;; is" into "what has changed".
;;;;
;;;; The peers are three classes rather than a tag, because a private chat, a
;;;; basic group, and a channel really do differ: they name themselves
;;;; differently, they build different InputPeers, and only one of the three
;;;; has an access hash it can do without.  Everything Telegram owns and we do
;;;; not -- the constructor names, the update kinds, the media kinds -- stays
;;;; a table or an EQL method on a keyword, never a class.
;;;;
;;;; It is still sans-thread and sans-policy: every function here is one call
;;;; the caller chose to make on whatever connection is current.  A client
;;;; that wants a background poller builds one out of PULL-CHAT-UPDATES.

(defpackage #:telegram.chat
  (:use #:cl)
  (:local-nicknames (#:octets #:telegram.octets)
                    (#:tl #:telegram.tl)
                    (#:mt #:telegram)
                    (#:client #:telegram.client))
  (:documentation
   "Peers, histories, and an update cursor over TELEGRAM.CLIENT's INVOKE.")
  (:export #:peer
           #:user-peer
           #:chat-peer
           #:channel-peer
           #:peer-id
           #:peer-kind
           #:peer-key
           #:peer-access-hash
           #:peer-title
           #:peer-username
           #:peer-unread-count
           #:peer-top-message-id
           #:peer-last-date
           #:peer-input-peer
           #:peer-label
           #:channel-peer-broadcast-p
           #:channel-peer-megagroup-p
           #:user-peer-self-p
           #:user-peer-bot-p
           ;; messages
           #:chat-message
           #:chat-message-p
           #:chat-message-id
           #:chat-message-date
           #:chat-message-peer-key
           #:chat-message-from-key
           #:chat-message-out-p
           #:chat-message-text
           #:chat-message-service-p
           #:chat-message-photo
           ;; photos
           #:chat-photo
           #:chat-photo-p
           #:chat-photo-id
           #:chat-photo-access-hash
           #:chat-photo-dc-id
           #:chat-photo-size-type
           #:chat-photo-width
           #:chat-photo-height
           #:decode-chat-photo
           #:download-chat-photo
           ;; the roster
           #:roster
           #:make-roster
           #:roster-peers
           #:roster-order
           #:roster-histories
           #:roster-self
           #:roster-pts
           #:roster-qts
           #:roster-date
           #:roster-seq
           #:roster-peer
           #:find-roster-peer
           #:peer-history
           #:message-sender-label
           ;; calls
           #:refresh-roster-dialogs
           #:refresh-peer-history
           #:send-chat-message
           #:upload-chat-file
           #:send-chat-photo
           #:mark-peer-read
           #:synchronize-chat-updates
           #:pull-chat-updates
           #:apply-chat-update
           #:apply-chat-updates))

(in-package #:telegram.chat)

;;;; Names
;;;;
;;;; Everything arriving from the schema is a TL-RECORD, and which constructor
;;;; it is decides what to do with it.  That name is a keyword, which is what
;;;; makes an EQL method the natural way to extend any of these dispatches.

(defun tl-name (record)
  "The constructor keyword RECORD was decoded as, or NIL.

TL:TL-NAME itself insists on a record; a decoder walking optional fields
wants to ask about something that may not be there at all."
  (when (tl:tl-record-p record)
    (tl:tl-name record)))

(defun trimmed-name (record prefix)
  "RECORD's constructor name with PREFIX removed and spelled for a reader."
  (let* ((name (string-downcase (symbol-name (or (tl-name record) :unknown))))
         (tail (if (and (<= (length prefix) (length name))
                        (string= prefix name :end2 (length prefix)))
                   (subseq name (length prefix))
                   name)))
    (substitute #\Space #\- tail)))

;;;; Peers

(defclass peer ()
  ((id :initarg :id :reader peer-id)
   (access-hash :initarg :access-hash :initform nil :accessor peer-access-hash)
   (title :initarg :title :initform "" :accessor peer-title)
   (username :initarg :username :initform nil :accessor peer-username)
   (unread-count :initform 0 :accessor peer-unread-count)
   (top-message-id :initform 0 :accessor peer-top-message-id)
   (last-date :initform 0 :accessor peer-last-date))
  (:documentation
   "One conversation, and what is needed to name it and address it."))

(defclass user-peer (peer)
  ((first-name :initarg :first-name :initform nil :accessor user-peer-first-name)
   (last-name :initarg :last-name :initform nil :accessor user-peer-last-name)
   (self-p :initarg :self-p :initform nil :accessor user-peer-self-p)
   (bot-p :initarg :bot-p :initform nil :accessor user-peer-bot-p))
  (:documentation "A private chat with one account."))

(defclass chat-peer (peer) ()
  (:documentation
   "A basic group.  Addressed by id alone: membership is the access check."))

(defclass channel-peer (peer)
  ((broadcast-p :initarg :broadcast-p :initform nil
                :accessor channel-peer-broadcast-p)
   (megagroup-p :initarg :megagroup-p :initform nil
                :accessor channel-peer-megagroup-p))
  (:documentation "A channel or supergroup, which keeps its own pts."))

(defgeneric peer-kind (peer)
  (:documentation "The Peer constructor family PEER belongs to.")
  (:method ((peer user-peer)) :user)
  (:method ((peer chat-peer)) :chat)
  (:method ((peer channel-peer)) :channel))

(defgeneric peer-input-peer (peer)
  (:documentation "The InputPeer that addresses PEER in a request.")
  (:method ((peer user-peer))
    (tl:make-tl :input-peer-user
                :user-id (peer-id peer)
                :access-hash (or (peer-access-hash peer) 0)))
  (:method ((peer chat-peer))
    (tl:make-tl :input-peer-chat :chat-id (peer-id peer)))
  (:method ((peer channel-peer))
    (tl:make-tl :input-peer-channel
                :channel-id (peer-id peer)
                :access-hash (or (peer-access-hash peer) 0))))

(defun peer-key (peer)
  "The roster key for PEER.  Ids are only unique within a family."
  (cons (peer-kind peer) (peer-id peer)))

(defun peer-label (peer)
  "PEER's display name, always something."
  (let ((title (peer-title peer)))
    (if (and title (plusp (length title)))
        title
        (format nil "~(~A~) ~D" (peer-kind peer) (peer-id peer)))))

(defmethod print-object ((peer peer) stream)
  (print-unreadable-object (peer stream :type t)
    (format stream "~A~@[ @~A~]~[~:; ~:*~D unread~]"
            (peer-label peer) (peer-username peer) (peer-unread-count peer))))

(defun user-display-name (first last)
  (let ((parts (remove-if (lambda (part) (or (null part) (zerop (length part))))
                          (list first last))))
    (if parts (format nil "~{~A~^ ~}" parts) "")))

;;;; Reading a Peer reference

(defgeneric tl-peer-key (name record)
  (:documentation
   "The roster key a Peer reference names, or NIL when it names nothing.")
  (:method (name record) (declare (ignore name record)) nil)
  (:method ((name (eql :peer-user)) record)
    (cons :user (tl:tl-value record :user-id)))
  (:method ((name (eql :peer-chat)) record)
    (cons :chat (tl:tl-value record :chat-id)))
  (:method ((name (eql :peer-channel)) record)
    (cons :channel (tl:tl-value record :channel-id))))

(defun peer-reference-key (record)
  (when record (tl-peer-key (tl-name record) record)))

;;;; Messages
;;;;
;;;; A message is a struct rather than a class: there are thousands of them,
;;;; nothing dispatches on one, and the whole point is to stop holding the
;;;; forty-field record the wire delivered.

(defstruct (chat-message (:constructor make-chat-message))
  (id 0 :type integer)
  (date 0 :type integer)
  (peer-key nil)
  (from-key nil)
  (out-p nil)
  (service-p nil)
  (photo nil)
  (text "" :type string))

;;;; Photos
;;;;
;;;; A photo arrives as an id, a hash, a file reference, and a menu of sizes
;;;; -- everything needed to ask for the bytes later, and none of the bytes.
;;;; Keeping that separately from the message is what lets a transcript be
;;;; drawn immediately and the pictures arrive when they arrive.

(defstruct (chat-photo (:constructor make-chat-photo))
  (id 0 :type integer)
  (access-hash 0 :type integer)
  (file-reference nil)
  (dc-id 0 :type integer)
  (size-type "x" :type string)
  (width 0 :type integer)
  (height 0 :type integer))

(defun choose-photo-size (sizes limit)
  "The largest concrete size no wider than LIMIT, or the smallest there is.

photoStrippedSize is a blurred thumbnail inline in the record and
photoSizeProgressive is fetched differently; only photoSize is a plain
downloadable rectangle, so those are the ones offered."
  (let ((usable (loop for size across sizes
                      when (and (eq :photo-size (tl-name size))
                                (tl:tl-value size :w :errorp nil))
                        collect size)))
    (when usable
      (let ((sorted (sort (copy-list usable) #'<
                          :key (lambda (size) (tl:tl-value size :w)))))
        (or (car (last (remove-if (lambda (size)
                                    (> (tl:tl-value size :w) limit))
                                  sorted)))
            (first sorted))))))

(defun decode-chat-photo (media &key (width-limit 800))
  "The CHAT-PHOTO a messageMediaPhoto names, or NIL for anything else."
  (when (and media (eq :message-media-photo (tl-name media)))
    (let ((photo (tl:tl-value media :photo :errorp nil)))
      (when (and photo (eq :photo (tl-name photo)))
        (let ((size (choose-photo-size (tl:tl-value photo :sizes) width-limit)))
          (when size
            (make-chat-photo
             :id (tl:tl-value photo :id)
             :access-hash (tl:tl-value photo :access-hash)
             :file-reference (tl:tl-value photo :file-reference)
             :dc-id (or (tl:tl-value photo :dc-id :errorp nil) 0)
             :size-type (tl:tl-value size :type)
             :width (tl:tl-value size :w)
             :height (tl:tl-value size :h))))))))

(defun download-chat-photo (photo &key connection (chunk 524288))
  "Fetch PHOTO's bytes with upload.getFile and return them as one vector.

CHUNK has to divide a megabyte and be a multiple of 4096, which is Telegram's
rule and not ours.  A photo stored on another data centre answers
FILE_MIGRATE_N rather than bytes; that is signalled, since recovering from it
means a second connection with an exported authorization."
  (let ((location (tl:make-tl :input-photo-file-location
                              :id (chat-photo-id photo)
                              :access-hash (chat-photo-access-hash photo)
                              :file-reference (chat-photo-file-reference photo)
                              :thumb-size (chat-photo-size-type photo)))
        (pieces '())
        (total 0))
    (loop for offset = 0 then (+ offset chunk)
          for answer = (client:invoke (client:current-connection connection)
                                      :upload.get-file
                                      :location location
                                      :offset offset
                                      :limit chunk)
          for bytes = (tl:tl-value answer :bytes)
          do (push bytes pieces)
             (incf total (length bytes))
          while (= (length bytes) chunk))
    (let ((result (make-array total :element-type '(unsigned-byte 8)))
          (cursor 0))
      (dolist (piece (nreverse pieces) result)
        (replace result piece :start1 cursor)
        (incf cursor (length piece))))))

(defun media-label (media)
  "A short bracketed word for a message that carries something."
  (when media
    (let ((tail (trimmed-name media "message-media-")))
      (unless (string= tail "empty")
        (format nil "[~A]" tail)))))

(defun message-body-text (record)
  (let ((text (or (tl:tl-value record :message :errorp nil) ""))
        (label (media-label (tl:tl-value record :media :errorp nil))))
    (cond ((and label (plusp (length text)))
           (format nil "~A ~A" label text))
          (label label)
          (t text))))

(defgeneric decode-chat-message (name record)
  (:documentation
   "A CHAT-MESSAGE for one Message constructor, or NIL for messageEmpty.")
  (:method (name record) (declare (ignore name record)) nil)
  (:method ((name (eql :message)) record)
    (let ((photo (decode-chat-photo (tl:tl-value record :media :errorp nil))))
      (make-chat-message
       :id (tl:tl-value record :id)
       :date (tl:tl-value record :date)
       :peer-key (peer-reference-key (tl:tl-value record :peer-id))
       :from-key (peer-reference-key (tl:tl-value record :from-id :errorp nil))
       :out-p (and (tl:tl-value record :out :errorp nil) t)
       :photo photo
       ;; A picture with a caption shows the caption; the "[photo]" stand-in
       ;; is for the media this client cannot draw yet.
       :text (if photo
                 (or (tl:tl-value record :message :errorp nil) "")
                 (message-body-text record)))))
  (:method ((name (eql :message-service)) record)
    (make-chat-message
     :id (tl:tl-value record :id)
     :date (tl:tl-value record :date)
     :peer-key (peer-reference-key (tl:tl-value record :peer-id))
     :from-key (peer-reference-key (tl:tl-value record :from-id :errorp nil))
     :out-p (and (tl:tl-value record :out :errorp nil) t)
     :service-p t
     :text (format nil "[~A]"
                   (trimmed-name (tl:tl-value record :action)
                                 "message-action-")))))

;;;; The roster

(defclass roster ()
  ((peers :initform (make-hash-table :test #'equal) :reader roster-peers
          :documentation "Roster key to PEER, the identity everything shares.")
   (order :initform '() :accessor roster-order
          :documentation "Peers most-recently-active first.")
   (histories :initform (make-hash-table :test #'equal) :reader roster-histories
              :documentation "Roster key to a vector sorted by message id.")
   (self :initform nil :accessor roster-self)
   (pts :initform nil :accessor roster-pts)
   (qts :initform nil :accessor roster-qts)
   (date :initform nil :accessor roster-date)
   (seq :initform nil :accessor roster-seq))
  (:documentation
   "Everything learned about conversations on one account, and how far the
update cursor has got.  A roster is not thread-safe; it belongs to whoever is
making the calls."))

(defun make-roster ()
  (make-instance 'roster))

(defmethod print-object ((roster roster) stream)
  (print-unreadable-object (roster stream :type t)
    (format stream "~D peer~:P~@[, pts ~D~]"
            (hash-table-count (roster-peers roster))
            (roster-pts roster))))

(defun roster-peer (roster key)
  "The peer KEY names, or NIL.  KEY is a (kind . id) cons or a peer."
  (gethash (if (typep key 'peer) (peer-key key) key) (roster-peers roster)))

(defun find-roster-peer (roster text)
  "The first peer whose title or username contains TEXT, case-insensitively."
  (let ((needle (string-downcase text)))
    (find-if (lambda (peer)
               (or (search needle (string-downcase (peer-label peer)))
                   (let ((username (peer-username peer)))
                     (and username
                          (search needle (string-downcase username))))))
             (roster-order roster))))

(defun peer-history (roster peer)
  "PEER's messages, oldest first.  Always a vector, possibly empty."
  (or (gethash (peer-key peer) (roster-histories roster))
      (setf (gethash (peer-key peer) (roster-histories roster))
            (make-array 0 :adjustable t :fill-pointer 0))))

(defun ensure-roster-peer (roster class id)
  "The peer of CLASS with ID in ROSTER, made if it is new."
  (let ((key (cons (ecase class
                     (user-peer :user)
                     (chat-peer :chat)
                     (channel-peer :channel))
                   id)))
    (or (gethash key (roster-peers roster))
        (let ((peer (make-instance class :id id)))
          (setf (gethash key (roster-peers roster)) peer)
          (setf (roster-order roster)
                (append (roster-order roster) (list peer)))
          peer))))

(defun touch-roster-peer (roster peer date)
  "Move PEER to the front of ROSTER's order if DATE is its newest activity."
  (when (> date (peer-last-date peer))
    (setf (peer-last-date peer) date)
    (setf (roster-order roster)
          (cons peer (remove peer (roster-order roster) :test #'eq))))
  peer)

;;;; Absorbing what a call told us
;;;;
;;;; Every messages.* answer carries the chats and users its messages refer
;;;; to.  Absorbing them first is what makes the access hashes accumulate, so
;;;; a peer seen once in a dialog list can be written to an hour later.

(defgeneric absorb-tl-peer (name record roster)
  (:documentation
   "Record what a User or Chat constructor says.  Returns the peer, or NIL
for the empty and unnameable constructors.")
  (:method (name record roster) (declare (ignore name record roster)) nil)
  (:method ((name (eql :user)) record roster)
    (let ((peer (ensure-roster-peer roster 'user-peer (tl:tl-value record :id))))
      (let ((hash (tl:tl-value record :access-hash :errorp nil)))
        (when hash (setf (peer-access-hash peer) hash)))
      (setf (user-peer-first-name peer)
            (tl:tl-value record :first-name :errorp nil)
            (user-peer-last-name peer)
            (tl:tl-value record :last-name :errorp nil)
            (user-peer-self-p peer)
            (and (tl:tl-value record :self :errorp nil) t)
            (user-peer-bot-p peer)
            (and (tl:tl-value record :bot :errorp nil) t)
            (peer-username peer) (tl:tl-value record :username :errorp nil)
            (peer-title peer)
            (user-display-name (user-peer-first-name peer)
                               (user-peer-last-name peer)))
      (when (user-peer-self-p peer)
        (setf (roster-self roster) peer))
      peer))
  (:method ((name (eql :chat)) record roster)
    (let ((peer (ensure-roster-peer roster 'chat-peer (tl:tl-value record :id))))
      (setf (peer-title peer) (or (tl:tl-value record :title) ""))
      peer))
  (:method ((name (eql :chat-forbidden)) record roster)
    (let ((peer (ensure-roster-peer roster 'chat-peer (tl:tl-value record :id))))
      (setf (peer-title peer) (or (tl:tl-value record :title) ""))
      peer))
  (:method ((name (eql :channel)) record roster)
    (let ((peer (ensure-roster-peer roster 'channel-peer
                                    (tl:tl-value record :id))))
      (let ((hash (tl:tl-value record :access-hash :errorp nil)))
        (when hash (setf (peer-access-hash peer) hash)))
      (setf (peer-title peer) (or (tl:tl-value record :title) "")
            (peer-username peer) (tl:tl-value record :username :errorp nil)
            (channel-peer-broadcast-p peer)
            (and (tl:tl-value record :broadcast :errorp nil) t)
            (channel-peer-megagroup-p peer)
            (and (tl:tl-value record :megagroup :errorp nil) t))
      peer))
  (:method ((name (eql :channel-forbidden)) record roster)
    (let ((peer (ensure-roster-peer roster 'channel-peer
                                    (tl:tl-value record :id))))
      (let ((hash (tl:tl-value record :access-hash :errorp nil)))
        (when hash (setf (peer-access-hash peer) hash)))
      (setf (peer-title peer) (or (tl:tl-value record :title) ""))
      peer)))

(defun absorb-peer-vectors (roster answer)
  "Absorb the users and chats vectors an answer carries, if it has them."
  (dolist (field '(:users :chats))
    (let ((vector (tl:tl-value answer field :errorp nil)))
      (when vector
        (loop for record across vector
              do (absorb-tl-peer (tl-name record) record roster)))))
  roster)

(defun insert-chat-message (history message)
  "Put MESSAGE into HISTORY, which stays sorted by id.  An id already there
is replaced, which is how an edit lands."
  (let ((id (chat-message-id message))
        (index (fill-pointer history)))
    (loop while (and (plusp index)
                     (> (chat-message-id (aref history (1- index))) id))
          do (decf index))
    (if (and (plusp index)
             (= (chat-message-id (aref history (1- index))) id))
        (setf (aref history (1- index)) message)
        (progn
          (vector-push-extend message history)
          (loop for position downfrom (1- (fill-pointer history)) above index
                do (rotatef (aref history position)
                            (aref history (1- position))))))
    history))

(defun record-chat-message (roster message)
  "File MESSAGE under its peer and return the peer, or NIL if it names none."
  (let* ((key (chat-message-peer-key message))
         (peer (and key (gethash key (roster-peers roster)))))
    (when peer
      ;; Saved Messages arrives with `out' unset and no from_id -- the server
      ;; sees no direction in a chat with one participant.  Everything in it
      ;; is nonetheless yours, and a transcript that shows it as incoming is
      ;; wrong in the only way a reader would notice.
      (let ((self (roster-self roster)))
        (when (and self (equal key (peer-key self)))
          (setf (chat-message-out-p message) t)
          (unless (chat-message-from-key message)
            (setf (chat-message-from-key message) key))))
      (insert-chat-message (peer-history roster peer) message)
      (touch-roster-peer roster peer (chat-message-date message))
      (when (> (chat-message-id message) (peer-top-message-id peer))
        (setf (peer-top-message-id peer) (chat-message-id message)))
      peer)))

(defun absorb-message-vector (roster answer &optional (field :messages))
  "Decode and file the messages an answer carries.  Returns the peers touched."
  (let ((vector (tl:tl-value answer field :errorp nil))
        (touched '()))
    (when vector
      (loop for record across vector
            for message = (decode-chat-message (tl-name record) record)
            for peer = (and message (record-chat-message roster message))
            when peer do (pushnew peer touched :test #'eq)))
    touched))

(defun message-sender-label (roster message)
  "Who sent MESSAGE, as something to put on a transcript line."
  (cond ((chat-message-out-p message) "you")
        ((let ((from (chat-message-from-key message)))
           (and from (roster-peer roster from)))
         (peer-label (roster-peer roster (chat-message-from-key message))))
        ((let ((peer (roster-peer roster (chat-message-peer-key message))))
           (and peer (typep peer 'user-peer) (peer-label peer))))
        (t "?")))

;;;; Dialogs
;;;;
;;;; messages.getDialogs pages by the date, id, and peer of the last dialog it
;;;; gave back, which is why the messages vector matters: the offset is a
;;;; property of a dialog's top message and not of the dialog.

(defun dialog-top-message (dialog messages)
  (let ((top (tl:tl-value dialog :top-message)))
    (find-if (lambda (record)
               (eql top (tl:tl-value record :id :errorp nil)))
             messages)))

(defun absorb-dialog (roster dialog messages)
  (let ((peer (roster-peer roster (peer-reference-key (tl:tl-value dialog :peer)))))
    (when peer
      (setf (peer-unread-count peer) (or (tl:tl-value dialog :unread-count) 0)
            (peer-top-message-id peer) (or (tl:tl-value dialog :top-message) 0))
      (let ((top (dialog-top-message dialog messages)))
        (when top
          (touch-roster-peer roster peer
                             (or (tl:tl-value top :date :errorp nil) 0)))))
    peer))

(defun refresh-roster-dialogs (roster &key (limit 60) (pages 1) connection)
  "Page messages.getDialogs into ROSTER, newest conversation first.

Returns the peers in dialog order.  PAGES bounds the paging; one page of
LIMIT is plenty for a list you are about to show someone."
  (let ((offset-date 0)
        (offset-id 0)
        (offset-peer (tl:make-tl :input-peer-empty))
        (ordered '()))
    (dotimes (page pages)
      (declare (ignorable page))
      (let* ((answer (client:invoke (client:current-connection connection)
                                    :messages.get-dialogs
                                    :offset-date offset-date
                                    :offset-id offset-id
                                    :offset-peer offset-peer
                                    :limit limit
                                    :hash 0))
             (dialogs (tl:tl-value answer :dialogs :errorp nil))
             (messages (tl:tl-value answer :messages :errorp nil)))
        (unless dialogs (return))
        (absorb-peer-vectors roster answer)
        (absorb-message-vector roster answer)
        (loop for dialog across dialogs
              for peer = (absorb-dialog roster dialog messages)
              when peer do (pushnew peer ordered :test #'eq))
        (when (< (length dialogs) limit) (return))
        (let* ((last (aref dialogs (1- (length dialogs))))
               (top (dialog-top-message last messages))
               (peer (roster-peer roster
                                  (peer-reference-key
                                   (tl:tl-value last :peer)))))
          (unless (and top peer) (return))
          (setf offset-date (or (tl:tl-value top :date :errorp nil) 0)
                offset-id (tl:tl-value last :top-message)
                offset-peer (peer-input-peer peer)))))
    ;; The dialog list is already newest-first; publishing it as the order
    ;; keeps peers learned incidentally from sliding to the top of the list.
    (let ((ordered (nreverse ordered)))
      (setf (roster-order roster)
            (append ordered
                    (remove-if (lambda (peer) (member peer ordered :test #'eq))
                               (roster-order roster))))
      ordered)))

;;;; History

(defun refresh-peer-history (roster peer &key (limit 40) (offset-id 0)
                                              connection)
  "Fetch PEER's most recent messages into ROSTER and return them, oldest
first.  OFFSET-ID pages backwards: pass the oldest id you already have."
  (let ((answer (client:invoke (client:current-connection connection)
                               :messages.get-history
                               :peer (peer-input-peer peer)
                               :offset-id offset-id
                               :offset-date 0
                               :add-offset 0
                               :limit limit
                               :max-id 0
                               :min-id 0
                               :hash 0)))
    (absorb-peer-vectors roster answer)
    (absorb-message-vector roster answer)
    (peer-history roster peer)))

;;;; The update cursor
;;;;
;;;; updates.getState is where a client says "I know everything up to here";
;;;; updates.getDifference then answers with what happened since, and a new
;;;; state to store.  Keeping that cursor is the whole difference between
;;;; polling a conversation and being told about every conversation.

(defun install-update-state (roster state)
  (setf (roster-pts roster) (tl:tl-value state :pts)
        (roster-qts roster) (tl:tl-value state :qts)
        (roster-date roster) (tl:tl-value state :date)
        (roster-seq roster) (tl:tl-value state :seq))
  roster)

(defun advance-update-cursor (roster record)
  "Take the pts and date a short update carried, when it moved us forward."
  (let ((pts (tl:tl-value record :pts :errorp nil))
        (date (tl:tl-value record :date :errorp nil)))
    (when (and pts (or (null (roster-pts roster)) (> pts (roster-pts roster))))
      (setf (roster-pts roster) pts))
    (when (and date (or (null (roster-date roster)) (> date (roster-date roster))))
      (setf (roster-date roster) date)))
  roster)

(defun synchronize-chat-updates (roster &key connection)
  "Set ROSTER's cursor to the present, so PULL-CHAT-UPDATES reports changes
from now rather than replaying the account's backlog."
  (install-update-state
   roster
   (client:invoke (client:current-connection connection) :updates.get-state)))

(defun apply-message-update (update roster)
  (let* ((record (tl:tl-value update :message))
         (message (decode-chat-message (tl-name record) record)))
    (when message
      (let ((peer (record-chat-message roster message)))
        (when (and peer (not (chat-message-out-p message)))
          (incf (peer-unread-count peer)))
        peer))))

(defgeneric apply-chat-update (name update roster)
  (:documentation
   "Apply one Update constructor to ROSTER; return the peer it touched or NIL.

This is the extension point: teaching the roster about another kind of
update is a method here and nothing else.")
  (:method (name update roster) (declare (ignore name update roster)) nil)
  (:method ((name (eql :update-new-message)) update roster)
    (apply-message-update update roster))
  (:method ((name (eql :update-new-channel-message)) update roster)
    (apply-message-update update roster))
  (:method ((name (eql :update-edit-message)) update roster)
    (apply-message-update update roster))
  (:method ((name (eql :update-edit-channel-message)) update roster)
    (apply-message-update update roster))
  (:method ((name (eql :update-read-history-inbox)) update roster)
    (let ((peer (roster-peer roster
                             (peer-reference-key (tl:tl-value update :peer)))))
      (when peer
        (setf (peer-unread-count peer)
              (or (tl:tl-value update :still-unread-count) 0)))
      peer)))

(defun apply-update-container (updates roster)
  (advance-update-cursor roster updates)
  (absorb-peer-vectors roster updates)
  (let ((touched '()))
    (loop for update across (tl:tl-value updates :updates)
          for peer = (apply-chat-update (tl-name update) update roster)
          when peer do (pushnew peer touched :test #'eq))
    touched))

(defun apply-short-message (updates roster peer-key from-key)
  "Apply the compact form Telegram sends for an ordinary text message.

It names its peer by id and carries no users vector, so it is only usable
when the peer is already known -- which, for a client that listed its dialogs
first, it is."
  (advance-update-cursor roster updates)
  (let ((message (make-chat-message
                  :id (tl:tl-value updates :id)
                  :date (tl:tl-value updates :date)
                  :peer-key peer-key
                  :from-key (if (tl:tl-value updates :out :errorp nil)
                                (let ((self (roster-self roster)))
                                  (and self (peer-key self)))
                                from-key)
                  :out-p (and (tl:tl-value updates :out :errorp nil) t)
                  :text (or (tl:tl-value updates :message) ""))))
    (let ((peer (record-chat-message roster message)))
      (when peer
        (unless (chat-message-out-p message)
          (incf (peer-unread-count peer)))
        (list peer)))))

(defgeneric apply-chat-updates (name updates roster)
  (:documentation
   "Apply an Updates container -- what a send or a poll answers with -- and
return the peers it touched.")
  (:method (name updates roster) (declare (ignore name updates roster)) '())
  (:method ((name (eql :updates)) updates roster)
    (apply-update-container updates roster))
  (:method ((name (eql :updates-combined)) updates roster)
    (apply-update-container updates roster))
  (:method ((name (eql :update-short)) updates roster)
    (advance-update-cursor roster updates)
    (let* ((update (tl:tl-value updates :update))
           (peer (apply-chat-update (tl-name update) update roster)))
      (when peer (list peer))))
  (:method ((name (eql :update-short-sent-message)) updates roster)
    (advance-update-cursor roster updates)
    '())
  (:method ((name (eql :update-short-message)) updates roster)
    (apply-short-message updates roster
                         (cons :user (tl:tl-value updates :user-id))
                         (cons :user (tl:tl-value updates :user-id))))
  (:method ((name (eql :update-short-chat-message)) updates roster)
    (apply-short-message updates roster
                         (cons :chat (tl:tl-value updates :chat-id))
                         (cons :user (tl:tl-value updates :from-id)))))

(defgeneric apply-chat-difference (name difference roster)
  (:documentation
   "Apply one updates.Difference and return the peers it touched.")
  (:method (name difference roster)
    (declare (ignore name difference roster))
    '())
  (:method ((name (eql :updates.difference-empty)) difference roster)
    (setf (roster-date roster) (tl:tl-value difference :date)
          (roster-seq roster) (tl:tl-value difference :seq))
    '())
  (:method ((name (eql :updates.difference)) difference roster)
    (prog1 (apply-difference-body difference roster)
      (install-update-state roster (tl:tl-value difference :state))))
  (:method ((name (eql :updates.difference-slice)) difference roster)
    (prog1 (apply-difference-body difference roster)
      (install-update-state roster
                            (tl:tl-value difference :intermediate-state))))
  (:method ((name (eql :updates.difference-too-long)) difference roster)
    ;; The account has moved further than a difference can express.  Taking
    ;; the new pts and refetching is the documented recovery; the dialog list
    ;; is the caller's to refresh.
    (setf (roster-pts roster) (tl:tl-value difference :pts))
    '()))

(defun apply-difference-body (difference roster)
  (absorb-peer-vectors roster difference)
  (let ((touched (absorb-message-vector roster difference :new-messages)))
    (loop for update across (tl:tl-value difference :other-updates)
          for peer = (apply-chat-update (tl-name update) update roster)
          when peer do (pushnew peer touched :test #'eq))
    ;; A message that arrived through the difference is unread by definition
    ;; unless we sent it, and NEW-MESSAGES does not go through the update
    ;; methods that would have counted it.
    (loop for update across (tl:tl-value difference :new-messages)
          for message = (decode-chat-message (tl-name update) update)
          when (and message (not (chat-message-out-p message)))
            do (let ((peer (roster-peer roster (chat-message-peer-key message))))
                 (when peer (incf (peer-unread-count peer)))))
    touched))

(defun pull-chat-updates (roster &key connection)
  "Ask what has happened since ROSTER's cursor and apply it.

Returns the peers touched.  Calls SYNCHRONIZE-CHAT-UPDATES first if there is
no cursor yet, in which case nothing is reported: there is no \"since\" until
there is a state."
  (unless (roster-pts roster)
    (synchronize-chat-updates roster :connection connection)
    (return-from pull-chat-updates '()))
  (let ((difference (client:invoke (client:current-connection connection)
                                   :updates.get-difference
                                   :pts (roster-pts roster)
                                   :date (roster-date roster)
                                   :qts (or (roster-qts roster) 0))))
    (apply-chat-difference (tl-name difference) difference roster)))

;;;; Sending

(defun fresh-random-id ()
  "A 64-bit signed random_id.  Telegram deduplicates sends by it, so it has
to come from real entropy and not from a listener's *RANDOM-STATE*."
  (let ((value (octets:octets-integer
                (octets:random-octets octets:*entropy* 8))))
    (if (>= value (expt 2 63)) (- value (expt 2 64)) value)))

(defun send-chat-message (roster peer text &key connection)
  "Send TEXT to PEER and file the message that results.

Telegram answers a send with whatever updates it produced, which for a
private chat is a short form carrying only the id and date -- the text is
ours already, so that case is completed here rather than refetched."
  (let* ((random-id (fresh-random-id))
         (answer (client:invoke (client:current-connection connection)
                                :messages.send-message
                                :peer (peer-input-peer peer)
                                :message text
                                :random-id random-id)))
    (if (eq :update-short-sent-message (tl-name answer))
        (let ((message
                (make-chat-message
                 :id (tl:tl-value answer :id)
                 :date (tl:tl-value answer :date)
                 :peer-key (peer-key peer)
                 :from-key (let ((self (roster-self roster)))
                             (and self (peer-key self)))
                 :out-p t
                 :text text)))
          (advance-update-cursor roster answer)
          (record-chat-message roster message)
          message)
        (progn (apply-chat-updates (tl-name answer) answer roster)
               ;; The container form already filed the real message; hand back
               ;; the newest one this peer has so a caller can show it.
               (let ((history (peer-history roster peer)))
                 (when (plusp (fill-pointer history))
                   (aref history (1- (fill-pointer history)))))))))

(defun upload-chat-file (bytes &key (name "file.bin") (part-size 524288)
                                    connection)
  "Save BYTES into Telegram's scratch storage and return the InputFile.

PART-SIZE has to divide 512KB and be a multiple of 1024.  The md5_checksum
goes out empty: the server does not verify it for a photo, and carrying an
MD5 that nothing checks would mean a whole digest implementation for the sake
of a field."
  (let* ((file-id (fresh-random-id))
         (total (length bytes))
         (parts (max 1 (ceiling total part-size))))
    (dotimes (index parts)
      (let ((start (* index part-size)))
        (client:invoke (client:current-connection connection)
                       :upload.save-file-part
                       :file-id file-id
                       :file-part index
                       :bytes (subseq bytes start
                                      (min total (+ start part-size))))))
    (tl:make-tl :input-file :id file-id :parts parts
                            :name name :md5-checksum "")))

(defun send-chat-photo (roster peer bytes &key (caption "") (name "photo.png")
                                               connection)
  "Upload BYTES and post them to PEER as a photo with CAPTION."
  (let* ((file (upload-chat-file bytes :name name :connection connection))
         (answer (client:invoke
                  (client:current-connection connection)
                  :messages.send-media
                  :peer (peer-input-peer peer)
                  :media (tl:make-tl :input-media-uploaded-photo :file file)
                  :message caption
                  :random-id (fresh-random-id))))
    (apply-chat-updates (tl-name answer) answer roster)
    answer))

(defun mark-peer-read (peer &key connection)
  "Tell Telegram PEER's history has been read, and forget the unread count."
  (let ((maximum (peer-top-message-id peer)))
    (when (plusp maximum)
      (handler-case
          (etypecase peer
            (channel-peer
             (client:invoke (client:current-connection connection)
                            :channels.read-history
                            :channel (tl:make-tl
                                      :input-channel
                                      :channel-id (peer-id peer)
                                      :access-hash (or (peer-access-hash peer) 0))
                            :max-id maximum))
            (peer
             (client:invoke (client:current-connection connection)
                            :messages.read-history
                            :peer (peer-input-peer peer)
                            :max-id maximum)))
        ;; Marking read is a courtesy.  A channel that will not take it is not
        ;; a reason to abandon the poll that was doing it.
        (mt:remote-rpc-error () nil))
      (setf (peer-unread-count peer) 0)))
  peer)
