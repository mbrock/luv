;;;; Typed fields, records, and messages (#A8LXLK).
;;;;
;;;; A record is a structure whose slots each have a wire type; a message is
;;;; a record with a type code and a channel.  One table of field types
;;;; drives both directions, so what the encoder checks is exactly what the
;;;; decoder refuses.

(in-package #:luvcraft.wire)

;;;; Small values

(defun zero-world-tag ()
  "The tag of a guest that does not yet know which world it joins."
  (make-octets +world-tag-length+))

(defun world-tag (tag)
  "TAG as a world tag: exactly +WORLD-TAG-LENGTH+ octets, or the hexadecimal
string of them that LUFT.TERRAIN:WORLD-SOURCE-TAG returns."
  (let ((octets (if (and (stringp tag) (= (length tag) (* 2 +world-tag-length+))
                         (every (lambda (char) (digit-char-p char 16)) tag))
                    (let ((octets (make-octets +world-tag-length+)))
                      (dotimes (index +world-tag-length+ octets)
                        (setf (aref octets index)
                              (parse-integer tag :start (* 2 index)
                                                 :end (+ 2 (* 2 index))
                                                 :radix 16))))
                    tag)))
    (unless (and (typep octets '(vector octet))
                 (= (length octets) +world-tag-length+))
      (error 'unencodable-value
             :detail (format nil "a world tag is ~D octets or ~D hex digits"
                             +world-tag-length+ (* 2 +world-tag-length+))))
    (coerce octets 'octets)))

(defun wrap-yaw (yaw)
  "YAW, in radians, wrapped into [-pi, pi)."
  (let ((yaw (finite yaw)))
    (- (mod (+ yaw pi) (* 2 pi)) pi)))

(defstruct (site (:constructor make-site (x y z)))
  "An integer cell, or a chunk coordinate."
  (x 0 :type (signed-byte 32))
  (y 0 :type (signed-byte 32))
  (z 0 :type (signed-byte 32)))

(deftype wire-value ()
  "What a WIRE-VALUE field carries: NIL, T, 64-bit integers, finite floats
(as doubles), strings, octet vectors, keywords, and proper lists of these,
nested at most eight deep.  Recipe parameters, entity state, and causes are
wire values."
  't)

(defconstant +max-value-depth+ 8)

(defun write-value (writer value &optional (depth 0))
  (when (> depth +max-value-depth+)
    (refuse "a value nested more than ~D deep" +max-value-depth+))
  (cond ((null value) (write-unsigned writer 1 0))
        ((eq value t) (write-unsigned writer 1 1))
        ((integerp value) (write-unsigned writer 1 2) (write-signed writer 8 value))
        ((floatp value) (write-unsigned writer 1 3) (write-finite writer value))
        ((stringp value) (write-unsigned writer 1 4) (write-string-field writer value))
        ((typep value '(vector octet))
         (write-unsigned writer 1 5)
         (write-unsigned writer 4 (length value))
         (write-octet-vector writer value))
        ((keywordp value) (write-unsigned writer 1 6) (write-keyword writer value))
        ((and (consp value) (null (cdr (last value))))
         (write-unsigned writer 1 7)
         (write-unsigned writer 4 (length value))
         (dolist (element value) (write-value writer element (1+ depth))))
        (t (refuse "~S is not a wire value" value))))

(defun read-value (reader &optional (depth 0))
  (when (> depth +max-value-depth+)
    (refuse "a value nested more than ~D deep" +max-value-depth+))
  (let ((tag (read-unsigned reader 1)))
    (case tag
      (0 nil)
      (1 t)
      (2 (read-signed reader 8))
      (3 (read-finite reader))
      (4 (read-string reader))
      (5 (read-octet-vector reader (read-unsigned reader 4)))
      (6 (read-keyword reader))
      (7 (loop repeat (read-count reader)
               collect (read-value reader (1+ depth))))
      (t (refuse "unknown value tag ~D" tag)))))

(defconstant +max-keyword-length+ 64)

(defun write-keyword (writer keyword)
  (unless (keywordp keyword) (refuse "~S is not a keyword" keyword))
  (let ((name (symbol-name keyword)))
    (when (> (length name) +max-keyword-length+)
      (refuse "keyword ~S is longer than ~D" keyword +max-keyword-length+))
    (write-string-field writer name)))

(defun read-keyword (reader)
  (let ((name (read-string reader)))
    (when (or (zerop (length name)) (> (length name) +max-keyword-length+))
      (refuse "a keyword name of ~D characters" (length name)))
    (intern name '#:keyword)))

(defun read-count (reader)
  "A list length, which cannot exceed the octets left: every element takes
at least one."
  (let ((count (read-unsigned reader 4)))
    (when (> count (reader-remaining reader))
      (refuse "~D elements claimed, ~D octets left" count (reader-remaining reader)))
    count))

;;;; Vectors

(defun vector-of-reals (value length)
  (unless (and (typep value 'sequence) (= (length value) length))
    (refuse "~S is not a vector of ~D reals" value length))
  (map '(simple-array double-float (*)) #'finite value))

(defun read-reals (reader length)
  (let ((vector (make-array length :element-type 'double-float)))
    (dotimes (index length vector)
      (setf (aref vector index) (read-finite reader)))))

(defun check-position (vector)
  (loop for coordinate across vector
        for low in (wire-limits-domain-low *limits*)
        for high in (wire-limits-domain-high *limits*)
        unless (<= low coordinate high)
          do (refuse "position ~S lies outside the domain" vector))
  vector)

(defun check-speed (vector)
  (let ((max (wire-limits-max-speed *limits*)))
    (when (> (reduce #'+ vector :key (lambda (x) (* x x))) (* max max))
      (refuse "velocity ~S is faster than ~A" vector max)))
  vector)

(defun check-unit (vector)
  (let ((norm (reduce #'+ vector :key (lambda (x) (* x x)))))
    (unless (< (abs (- norm 1d0)) 1d-5)
      (refuse "~S is not a unit vector" vector)))
  vector)

(defun check-site (site)
  (loop for coordinate in (list (site-x site) (site-y site) (site-z site))
        for low in (wire-limits-domain-low *limits*)
        for high in (wire-limits-domain-high *limits*)
        unless (and (<= low coordinate) (< coordinate high))
          do (refuse "site ~S lies outside the domain" site))
  site)

;;;; Field types
;;;;
;;;; A type is a keyword, a record name, or one of (:list TYPE),
;;;; (:optional TYPE), and (:enum KEYWORD...).

(defun record-fields (name) (get name 'wire-fields))

(defun write-field (type writer value)
  (if (consp type)
      (ecase (first type)
        (:list
         (unless (listp value) (refuse "~S is not a list" value))
         (write-unsigned writer 4 (length value))
         (dolist (element value) (write-field (second type) writer element)))
        (:optional
         (write-unsigned writer 1 (if value 1 0))
         (when value (write-field (second type) writer value)))
        (:enum
         (let ((index (position value (rest type))))
           (unless index (refuse "~S is not one of ~S" value (rest type)))
           (write-unsigned writer 1 index))))
      (case type
        (:bool (write-unsigned writer 1 (if value 1 0)))
        (:u8 (write-unsigned writer 1 value))
        (:u16 (write-unsigned writer 2 value))
        (:u32 (write-unsigned writer 4 value))
        (:u64 (write-unsigned writer 8 value))
        (:finite (write-finite writer value))
        (:string (write-string-field writer value))
        (:keyword (write-keyword writer value))
        (:value (write-value writer value))
        (:octets
         (unless (typep value '(vector octet)) (refuse "~S is not octets" value))
         (write-unsigned writer 4 (length value))
         (write-octet-vector writer value))
        (:digest
         (unless (and (typep value '(vector octet)) (<= 1 (length value) 64))
           (refuse "~S is not a digest of 1 to 64 octets" value))
         (write-unsigned writer 1 (length value))
         (write-octet-vector writer value))
        (:yaw
         (let ((yaw (finite value)))
           (unless (<= (- pi) yaw pi) (refuse "yaw ~A outside [-pi, pi]" yaw))
           (write-finite writer yaw)))
        (:pitch
         (let ((pitch (finite value)))
           (unless (<= (- (/ pi 2)) pitch (/ pi 2))
             (refuse "pitch ~A outside [-pi/2, pi/2]" pitch))
           (write-finite writer pitch)))
        (:position (map nil (lambda (x) (write-finite writer x))
                        (check-position (vector-of-reals value 3))))
        (:velocity (map nil (lambda (x) (write-finite writer x))
                        (check-speed (vector-of-reals value 3))))
        (:quaternion (map nil (lambda (x) (write-finite writer x))
                          (check-unit (vector-of-reals value 4))))
        ((:site :chunk)
         (unless (site-p value) (refuse "~S is not a site" value))
         (when (eq type :site) (check-site value))
         (write-signed writer 4 (site-x value))
         (write-signed writer 4 (site-y value))
         (write-signed writer 4 (site-z value)))
        (:offset
         (unless (and (typep value '(integer 0))
                      (< value (wire-limits-vocabulary-size *limits*)))
           (refuse "offset ~S outside a vocabulary of ~D" value
                   (wire-limits-vocabulary-size *limits*)))
         (write-unsigned writer 4 value))
        (:vocabulary
         (write-field '(:list :string) writer value)
         (adopt-vocabulary (length value)))
        (t (write-record type writer value)))))

(defun read-field (type reader)
  (if (consp type)
      (ecase (first type)
        (:list (loop repeat (read-count reader)
                     collect (read-field (second type) reader)))
        (:optional (when (read-field :bool reader)
                     (read-field (second type) reader)))
        (:enum (let ((index (read-unsigned reader 1)))
                 (or (nth index (rest type))
                     (refuse "enumeration index ~D of ~S" index (rest type))))))
      (case type
        (:bool (let ((octet (read-unsigned reader 1)))
                 (case octet (0 nil) (1 t)
                   (t (refuse "boolean octet ~D" octet)))))
        (:u8 (read-unsigned reader 1))
        (:u16 (read-unsigned reader 2))
        (:u32 (read-unsigned reader 4))
        (:u64 (read-unsigned reader 8))
        (:finite (read-finite reader))
        (:string (read-string reader))
        (:keyword (read-keyword reader))
        (:value (read-value reader))
        (:octets (read-octet-vector reader (read-unsigned reader 4)))
        (:digest (let ((length (read-unsigned reader 1)))
                   (when (zerop length) (refuse "an empty digest"))
                   (read-octet-vector reader length)))
        (:yaw (let ((yaw (read-finite reader)))
                (unless (<= (- pi) yaw pi) (refuse "yaw ~A outside [-pi, pi]" yaw))
                yaw))
        (:pitch (let ((pitch (read-finite reader)))
                  (unless (<= (- (/ pi 2)) pitch (/ pi 2))
                    (refuse "pitch ~A outside [-pi/2, pi/2]" pitch))
                  pitch))
        (:position (check-position (read-reals reader 3)))
        (:velocity (check-speed (read-reals reader 3)))
        (:quaternion (check-unit (read-reals reader 4)))
        ((:site :chunk)
         (let ((site (make-site (read-signed reader 4) (read-signed reader 4)
                                (read-signed reader 4))))
           (if (eq type :site) (check-site site) site)))
        (:offset (let ((offset (read-unsigned reader 4)))
                   (unless (< offset (wire-limits-vocabulary-size *limits*))
                     (refuse "offset ~D outside a vocabulary of ~D" offset
                             (wire-limits-vocabulary-size *limits*)))
                   offset))
        (:vocabulary (let ((table (read-field '(:list :string) reader)))
                       (adopt-vocabulary (length table))
                       table))
        (t (read-record type reader)))))

(defun adopt-vocabulary (size)
  "Within one encode or decode, offsets after a vocabulary table are checked
against that table."
  (setf *limits* (let ((limits (copy-wire-limits *limits*)))
                   (setf (wire-limits-vocabulary-size limits) size)
                   limits)))

(defun write-record (name writer value)
  (let ((fields (record-fields name)))
    (unless (and fields (typep value (or (get name 'wire-structure) name)))
      (refuse "~S is not a ~(~A~)" value name))
    (loop for (nil type reader) in fields
          do (write-field type writer (funcall reader value)))))

(defun read-record (name reader &rest initargs)
  (let ((fields (or (record-fields name)
                    (error "No wire type named ~S." name))))
    (apply (get name 'wire-constructor)
           (append initargs
                   (loop for (slot type) in fields
                         collect (intern (symbol-name slot) '#:keyword)
                         collect (read-field type reader))))))

;;;; Records

(defmacro define-wire-record (name (&rest options) &body fields)
  "Define structure NAME whose FIELDS, each (SLOT TYPE &optional DEFAULT),
travel in order.  OPTIONS go to DEFSTRUCT."
  (let ((constructor (intern (format nil "MAKE-~A" name)))
        (documentation (when (stringp (first fields)) (list (pop fields)))))
    `(progn
       (defstruct (,name (:constructor ,constructor) ,@options)
         ,@documentation
         ,@(loop for (slot nil default) in fields collect (list slot default)))
       (setf (get ',name 'wire-constructor) ',constructor
             (get ',name 'wire-fields)
             ',(loop for (slot type) in fields
                     collect (list slot type
                                   (intern (format nil "~A-~A" name slot)))))
       ',name)))

(define-wire-record block-identity ()
  "A vocabulary offset and an optional entity (#FBU3TF)."
  (offset :offset 0)
  (entity :value nil))

(define-wire-record recipe ()
  "What raises the terrain baseline: a generator, its version, a seed, and a
plist of parameters as wire values."
  (generator :string "")
  (version :u32 0)
  (seed :u64 0)
  (parameters :value nil))

(define-wire-record principal ()
  "An actor (#3F0SCZ)."
  (kind (:enum :player :inhabitant :import :host) :player)
  (id :string ""))

(define-wire-record edit ()
  "One cell's requested change, checked against BEFORE."
  (site :site (make-site 0 0 0))
  (before :block-identity (make-block-identity))
  (after :block-identity (make-block-identity)))

(define-wire-record placement ()
  "One cell of a compacted edit set."
  (site :site (make-site 0 0 0))
  (block :block-identity (make-block-identity)))

(define-wire-record event ()
  "One sequenced journal event (#3F0SCZ)."
  (sequence :u64 0)
  (tick :u64 0)
  (actor :principal (make-principal))
  (transaction :u64 0)
  (site :site (make-site 0 0 0))
  (before :block-identity (make-block-identity))
  (after :block-identity (make-block-identity))
  (cause :value nil))

(define-wire-record pose ()
  "A player's body at a tick (#8EZK9F).  Yaw is in [-pi, pi], pitch in
[-pi/2, pi/2]; GAIT is the walking phase in radians; RIDE is an entity id."
  (tick :u64 0)
  (position :position (vector 0d0 0d0 0d0))
  (velocity :velocity (vector 0d0 0d0 0d0))
  (yaw :yaw 0d0)
  (pitch :pitch 0d0)
  (gait :finite 0d0)
  (flying :bool nil)
  (held (:optional :block-identity) nil)
  (ride (:optional :u64) nil))

(define-wire-record entity-state ()
  "One host-owned entity.  ORIENTATION is a unit quaternion (x y z w);
STATE is a wire value."
  (id :u64 0)
  (kind :keyword :entity)
  (position :position (vector 0d0 0d0 0d0))
  (velocity :velocity (vector 0d0 0d0 0d0))
  (orientation :quaternion (vector 0d0 0d0 0d0 1d0))
  (state :value nil))

(define-wire-record entity-snapshot ()
  (tick :u64 0)
  (entities (:list :entity-state) nil))

(define-wire-record stack ()
  "One inventory entry: a block and how many, or NIL for unlimited supply."
  (block :block-identity (make-block-identity))
  (count (:optional :u32) nil))

(define-wire-record player-record ()
  "A player in the host's players table (#Y3S82T).  INVENTORY is in number
row order, so its first ten stacks are the hotbar, or KIT is when SIMPLE;
HELD is the exact block in hand.  An empty INVENTORY is a new player."
  (identity :string "")
  (name :string "")
  (pose :pose (make-pose))
  (creative :bool nil)
  (held (:optional :block-identity) nil)
  (simple :bool nil)
  (kit (:list :block-identity) nil)
  (inventory (:list :stack) nil))

(define-wire-record companion-record ()
  "A companion as every peer sees it (#EA88ON): its stable ID, what it was
last asked and said (SAID-SERIAL counts its lines, so a repeated line is
still news), the player it attends to, what it is doing, the foot cell it
walks to, and its body."
  (id :string "")
  (kind :keyword :gnome)
  (presence (:enum :present :left) :present)
  (asked :string "")
  (said :string "")
  (said-serial :u32 0)
  (attending :string "")
  (activity (:enum :idle :opening :thinking :working) :idle)
  (tool :string "")
  (destination (:optional :site) nil)
  (pose :pose (make-pose)))

(define-wire-record chunk-hash ()
  (chunk :chunk (make-site 0 0 0))
  (digest :digest (make-octets 32)))

;; Record names double as field types; field types are keywords.
(dolist (name '(block-identity recipe principal edit placement event pose
                entity-state entity-snapshot stack player-record companion-record chunk-hash))
  (let ((keyword (intern (symbol-name name) '#:keyword)))
    (setf (get keyword 'wire-fields) (get name 'wire-fields)
          (get keyword 'wire-constructor) (get name 'wire-constructor)
          (get keyword 'wire-structure) name)))

;;;; Messages

(defstruct message
  "What every message carries besides its body."
  (world-tag (zero-world-tag) :type octets)
  (session 0 :type (unsigned-byte 64)))

(defvar *message-types* (make-array 256 :initial-element nil)
  "Message structure names by type code.")

(defmacro define-message (name code channel &body fields)
  "Define message NAME with type CODE.  CHANNEL is :RELIABLE (the stream
only), :UNRELIABLE (a datagram, or the stream as fallback), or :DATAGRAM."
  (let ((documentation (when (stringp (first fields)) (list (pop fields)))))
    `(progn
       (define-wire-record ,name ((:include message)) ,@documentation ,@fields)
       (setf (get ',name 'wire-code) ,code
             (get ',name 'wire-channel) ,channel
             (aref *message-types* ,code) ',name)
       ',name)))

(defun message-type-code (message) (get (type-of message) 'wire-code))
(defun message-channel (message)
  "MESSAGE's channel: :RELIABLE, :UNRELIABLE, or :DATAGRAM."
  (get (if (symbolp message) message (type-of message)) 'wire-channel))
(defun message-type-for-code (code) (aref *message-types* code))

(define-message hello-message 1 :reliable
  "A guest's first words (#2SGDU2).  Its session is zero."
  (identity :string "")
  (name :string ""))

(define-message welcome-message 2 :reliable
  "Catch-up: everything a guest needs to raise the world at watermark N."
  (recipe :recipe (make-recipe))
  (vocabulary :vocabulary nil)
  (watermark :u64 0)
  (edits (:list :placement) nil)
  (entities :entity-snapshot (make-entity-snapshot))
  (player :player-record (make-player-record)))

(define-message events-message 3 :reliable
  "Sequenced events, strictly after what the guest has."
  (events (:list :event) nil))

(define-message edit-request-message 4 :reliable
  "One transaction a player asks the sequencer to apply whole."
  (request :u32 0)
  (edits (:list :edit) nil)
  (cause :value nil))

(define-message edit-reply-message 5 :reliable
  "The sequencer's answer: the first sequence number it assigned, or why not."
  (request :u32 0)
  (outcome (:enum :accepted :rejected) :accepted)
  (sequence :u64 0)
  (reason :value nil))

(define-message player-update-message 6 :reliable
  (presence (:enum :joined :present :left) :present)
  (player :player-record (make-player-record)))

(define-message chat-message 7 :reliable
  (speaker :string "")
  (text :string ""))

(define-message chunk-hashes-message 8 :reliable
  "Content hashes of chunks as they stood at journal sequence WATERMARK, so a
peer compares them only once it stands there too (#RELI34)."
  (watermark :u64 0)
  (hashes (:list :chunk-hash) nil))

(define-message chunk-content-message 9 :reliable
  "One chunk's content, in the game's own encoding."
  (chunk :chunk (make-site 0 0 0))
  (digest :digest (make-octets 32))
  (content :octets (make-octets 0)))

(define-message bye-message 10 :reliable
  (reason :keyword :bye)
  (text :string ""))

(define-message companion-message 16 :reliable
  "The host's companion as it now is, sent when its words, attention, or
journey change, and to a guest on joining."
  (companion :companion-record (make-companion-record)))

(define-message companion-tell-message 17 :reliable
  "A guest asking the host's companion COMPANION something."
  (companion :string "")
  (text :string ""))

(define-message pose-message 32 :unreliable
  (player :string "")
  (pose :pose (make-pose)))

(define-message entity-snapshot-message 33 :unreliable
  (snapshot :entity-snapshot (make-entity-snapshot)))

(define-message companion-pose-message 40 :unreliable
  "A host companion's body, as a player's pose."
  (companion :string "")
  (pose :pose (make-pose)))

(define-message probe-message 48 :datagram
  "Keeps the datagram path honest: HEARD says datagrams from the receiver
have arrived lately."
  (heard :bool nil))

(define-message beacon-message 49 :datagram
  "A host announcing itself on the LAN every half second."
  (name :string "")
  (port :u16 +default-port+)
  (world :string ""))

;;;; Frames

(defun stamp-message (message &key world-tag session)
  "A copy of MESSAGE with its WORLD-TAG and SESSION replaced when given."
  (let ((copy (copy-structure message)))
    (when world-tag (setf (message-world-tag copy) (world-tag world-tag)))
    (when session (setf (message-session copy) session))
    copy))

(defun encode-message (message &key (limits *default-wire-limits*))
  "MESSAGE as one frame of octets.  Signals UNENCODABLE-VALUE for a value no
strict decoder would accept."
  (let ((*encoding* t) (*limits* limits))
    (unless (and (message-p message) (message-type-code message))
      (refuse "~S is not a message" message))
    (let ((body (with-wire-writer (writer)
                  (write-record (type-of message) writer message))))
      (when (> (length body) (- (wire-limits-max-frame-length limits)
                                +header-length+))
        (refuse "a ~D-octet body is longer than a frame" (length body)))
      (with-wire-writer (writer)
        (write-octet-vector writer +magic+)
        (write-unsigned writer 2 +protocol-version+)
        (write-unsigned writer 1 (message-type-code message))
        (write-octet-vector writer (world-tag (message-world-tag message)))
        (write-unsigned writer 8 (message-session message))
        (write-unsigned writer 4 (length body))
        (write-octet-vector writer body)))))

(defun read-header (reader channel)
  "The type, world tag, session, and body length of the frame at READER."
  (let ((magic (read-octet-vector reader 4)))
    (unless (equalp magic +magic+)
      (refuse "magic ~S is not LUVW" magic)))
  (let ((version (read-unsigned reader 2)))
    (unless (= version +protocol-version+)
      (error 'unsupported-version :version version)))
  (let* ((code (read-unsigned reader 1))
         (type (or (message-type-for-code code)
                   (refuse "unknown message type ~D" code)))
         (tag (read-octet-vector reader +world-tag-length+))
         (session (read-unsigned reader 8))
         (length (read-unsigned reader 4)))
    (case channel
      (:stream (when (eq (message-channel type) :datagram)
                 (refuse "~(~A~) on the stream" type)))
      (:datagram (when (eq (message-channel type) :reliable)
                   (refuse "~(~A~) in a datagram" type))))
    (when (> length (- (wire-limits-max-frame-length *limits*) +header-length+))
      (refuse "a ~D-octet body is longer than a frame" length))
    (values type tag session length)))

(defun read-message (reader channel)
  (multiple-value-bind (type tag session length) (read-header reader channel)
    (unless (= length (reader-remaining reader))
      (refuse "body of ~D octets, ~D present" length (reader-remaining reader)))
    (prog1 (read-record type reader :world-tag tag :session session)
      (unless (zerop (reader-remaining reader))
        (refuse "~D octets after the message" (reader-remaining reader))))))

(defun decode-message (octets &key (limits *default-wire-limits*) channel)
  "The one message that OCTETS encode exactly.  CHANNEL, when :STREAM or
:DATAGRAM, also refuses types that do not travel there.  Signals
MALFORMED-MESSAGE (or UNSUPPORTED-VERSION) for anything else."
  (let ((*encoding* nil) (*limits* limits)
        (octets (coerce octets 'octets)))
    (read-message (make-wire-reader octets 0 (length octets)) channel)))

;;;; Stream framing

(defstruct (frame-decoder (:constructor make-frame-decoder ()))
  "Turns a stream of octets into whole frames."
  (buffer (make-array 1024 :element-type 'octet :adjustable t :fill-pointer 0)))

(defun frame-decoder-buffered (decoder)
  (fill-pointer (frame-decoder-buffer decoder)))

(defun feed-frame-decoder (decoder octets &key (limits *default-wire-limits*)
                                              (end (length octets)))
  "Append OCTETS (up to END) and return the messages completed, oldest
first.  A bad header is refused as soon as it is complete, so a stream that
is not this protocol fails before its claimed body arrives.  After a
refusal the stream is out of step and should be closed."
  (let ((buffer (frame-decoder-buffer decoder))
        (*encoding* nil) (*limits* limits)
        (messages '()))
    (loop for index below end do (vector-push-extend (aref octets index) buffer))
    (loop
      (let ((available (fill-pointer buffer)))
        (when (mismatch buffer +magic+ :end1 (min available 4)
                                       :end2 (min available 4))
          (refuse "a stream that does not start with LUVW"))
        (when (< available +header-length+) (return))
        (let* ((head (coerce (subseq buffer 0 +header-length+) 'octets))
               (length (nth-value 3 (read-header
                                     (make-wire-reader head 0 +header-length+)
                                     :stream)))
               (total (+ +header-length+ length)))
          (when (< available total) (return))
          (let ((frame (coerce (subseq buffer 0 total) 'octets)))
            (replace buffer buffer :start2 total)
            (setf (fill-pointer buffer) (- available total))
            (push (read-message (make-wire-reader frame 0 total) :stream)
                  messages)))))
    (nreverse messages)))
