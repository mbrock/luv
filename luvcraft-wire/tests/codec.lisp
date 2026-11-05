;;;; Claims about the codec: every message round-trips, and anything an
;;;; encoder would not have written is refused.

(in-package #:luvcraft.wire.tests)

(defun stone () (make-block-identity :offset 1))
(defun film () (make-block-identity :offset 3 :entity '(:film 7 "Glass" 0.25d0)))

(defun sample-pose ()
  (make-pose :tick 123456789012 :position #(10.5d0 -64.25d0 3000.125d0)
             :velocity #(1.5d0 -9.75d0 0.0625d0) :yaw -2.5d0 :pitch 0.625d0
             :gait 17.375d0 :flying t :held (film) :ride 77))

(defun sample-entities ()
  (make-entity-snapshot
   :tick 99
   :entities (list (make-entity-state :id 5 :kind :turtle
                                      :position #(1d0 2d0 3d0)
                                      :velocity #(0d0 -1d0 0d0)
                                      :orientation #(0.6d0 0d0 0d0 0.8d0)
                                      :state '(:resting t :hunger 3))
                   (make-entity-state :id 6 :kind :ball))))

(defun sample-player ()
  (make-player-record :identity "c2f0-player-ida" :name "Daniel"
                      :pose (sample-pose) :creative t :held (film) :simple t
                      :kit (list (stone) (film))
                      :inventory (list (make-stack :block (stone))
                                       (make-stack :block (film) :count 1))))

(defun sample-companion ()
  (make-companion-record :id "gnome-k3x9q" :kind :gnome :asked "Bygg en trappa"
                         :said "Gärna — ett steg i taget." :said-serial 3
                         :attending "c2f0-player-ida" :activity :working
                         :tool "Walk To" :destination (make-site 1030 1031 12)
                         :pose (sample-pose) :owner "c2f0-player-ida"))

(defun sample-messages ()
  "One rich message of every type, with distinct values in every field."
  (let ((world (tag 11)))
    (flet ((in-world (message)
             (stamp-message message :world-tag world :session #xfedcba9876543210)))
      (list
       (make-hello-message :identity "c2f0-player-ida" :name "Ida Åkesson"
                           :resume 4100 :epoch #x0123456789abcdef)
       (in-world
        (make-welcome-message
         :epoch #x0123456789abcdef
         :recipe (make-recipe :generator "luvcraft" :version 3 :seed 1234567
                              :parameters '(:sea-level 64 :roughness 0.75d0
                                            :biomes ("hills" "fjord")))
         :vocabulary '("air" "stone" "glass" "luvcraft:film")
         :watermark 4096
         :edits (list (make-placement :site (make-site 1 -2 3) :block (stone)
                                      :author (make-provenance
                                               :actor (make-principal :kind :player :id "ada")
                                               :sequence 17 :time 4002876413))
                      (make-placement :site (make-site -100 64 7) :block (film)))
         :entities (sample-entities)
         :player (sample-player)))
       (in-world
        (make-events-message
         :events (list (make-event :sequence 4097 :tick 5000
                                   :actor (make-principal :kind :player
                                                          :id "c2f0-player-ida")
                                   :transaction 12 :site (make-site 4 5 -6)
                                   :before (stone) :after (film)
                                   :cause '(:place :hand))
                       (make-event :sequence 4098 :tick 5001
                                   :actor (make-principal :kind :inhabitant
                                                          :id "turtle-5")
                                   :transaction 13 :site (make-site 0 0 0)
                                   :before (film) :after (make-block-identity)
                                   :cause :undo))))
       (in-world
        (make-edit-request-message
         :request 42
         :edits (list (make-edit :site (make-site 9 8 7) :before (make-block-identity)
                                 :after (stone)))
         :cause :place))
       (in-world (make-edit-reply-message :request 42 :outcome :rejected
                                          :reason '(:before-changed 9 8 7)))
       (in-world (make-player-update-message :presence :joined
                                             :player (sample-player)))
       (in-world (make-chat-message :speaker "Ida" :text "Bygg här — ☃"))
       (in-world
        (make-chunk-hashes-message
         :watermark 41
         :hashes (list (make-chunk-hash :chunk (make-site 0 -1 2)
                                        :digest (octets 1 2 3 4 5 6 7 8))
                       (make-chunk-hash :chunk (make-site 3 0 0)
                                        :digest (make-octets 32)))))
       (in-world (make-chunk-content-message :chunk (make-site -5 1 9)
                                             :digest (octets 9 9 9)
                                             :content (octets 0 255 17 34)))
       (in-world (make-bye-message :reason :foreign-world :text "wrong world"))
       (in-world (make-companion-message :companion (sample-companion)))
       (in-world (make-companion-tell-message :companion "gnome-k3x9q"
                                              :text "Kan du hämta en sten?"))
       (in-world (make-companion-pose-message :companion "gnome-k3x9q" :pose (sample-pose)))
       (in-world (make-ride-message :rider "c2f0-player-ida" :entity 5 :action :refused))
       (in-world (make-throw-message :kind :beach-ball :position #(1030.5d0 1040.25d0 21.75d0)
                                     :velocity #(-3.5d0 8d0 2.25d0)))
       (in-world (make-summon-message :kind :cat :position #(1030.5d0 1040.25d0 21d0)))
       (in-world (make-sky-request-message :setting :minutes-per-day :value 7.5d0))
       (in-world (make-pose-message :player "c2f0-player-ida" :pose (sample-pose)))
       (in-world (make-entity-snapshot-message :snapshot (sample-entities)))
       (in-world (make-probe-message :heard t))
       (in-world (make-beacon-message :name "Mikael" :port 47475
                                      :world "Little world"))))))

(defun sample (type)
  (find type (sample-messages) :key #'type-of))

(define-test every-message-round-trips
  (let ((samples (sample-messages)))
    (group (context "there is a sample for every registered message type")
      (true (equal (sort (remove nil (coerce luvcraft.wire::*message-types* 'list))
                         #'string<)
                   (sort (mapcar #'type-of samples) #'string<))))
    (dolist (message samples)
      (let* ((frame (encode-message message))
             (back (decode-message frame)))
        (group (context (princ-to-string (type-of message)))
          (true (eq (type-of message) (type-of back)))
          (true (equalp message back))
          (group (context "decoding loses nothing: the re-encoding is identical")
            (true (equalp frame (encode-message back))))
          (true (equalp +magic+ (subseq frame 0 4))))))
    (group (context "decoded reals are doubles, decoded keywords are keywords")
      (let ((pose (pose-message-pose
                   (decode-message (encode-message (sample 'pose-message))))))
        (true (typep (pose-yaw pose) 'double-float))
        (true (eql 3000.125d0 (aref (pose-position pose) 2)))
        (true (eq :film (first (block-identity-entity (pose-held pose)))))))))

(define-test every-truncation-is-refused
  (dolist (message (sample-messages))
    (let ((frame (encode-message message)))
      (group (context (princ-to-string (type-of message)))
        (true (loop for length below (length frame)
                    always (handler-case
                               (progn (decode-message (subseq frame 0 length)) nil)
                             (malformed-message () t))))
        (group (context "and so is one trailing octet")
          (fail (decode-message (concatenate 'octets frame (octets 0)))
                'malformed-message))))))

(define-test headers-are-checked
  (let ((frame (encode-message (sample 'chat-message))))
    (flet ((with-octet (index value)
             (let ((copy (copy-seq frame))) (setf (aref copy index) value) copy)))
      (group (context "magic")
        (fail (decode-message (with-octet 0 (char-code #\X))) 'malformed-message))
      (group (context "another protocol version is its own condition")
        (fail (decode-message (with-octet 5 (1+ +protocol-version+))) 'unsupported-version))
      (group (context "an unknown type")
        (fail (decode-message (with-octet 6 200)) 'malformed-message))
      (group (context "a body length that disagrees with the octets")
        (fail (decode-message (with-octet (1- +header-length+)
                                          (1+ (aref frame (1- +header-length+)))))
              'malformed-message))))
  (group (context "a world tag is sixteen octets, or the terrain's hex string of them")
    (true (equalp (octets #x01 #x23 #x45 #x67 #x89 #xab #xcd #xef
                          #xfe #xdc #xba #x98 #x76 #x54 #x32 #x10)
                  (world-tag "0123456789abcdeffedcba9876543210")))
    (fail (world-tag "0123456789abcdef") 'unencodable-value)
    (fail (world-tag (make-octets 32)) 'unencodable-value))
  (group (context "channels: reliable types never come by datagram, probes never by stream")
    (fail (decode-message (encode-message (sample 'hello-message)) :channel :datagram)
          'malformed-message)
    (fail (decode-message (encode-message (sample 'probe-message)) :channel :stream)
          'malformed-message)
    (true (pose-message-p (decode-message (encode-message (sample 'pose-message))
                                          :channel :datagram)))
    (true (pose-message-p (decode-message (encode-message (sample 'pose-message))
                                          :channel :stream)))))

(define-test non-finite-and-non-unit-numbers-are-refused
  (let ((frame (encode-message (sample 'pose-message)))
        (nan (octets #x7f #xf8 0 0 0 0 0 0))
        (infinity (octets #x7f #xf0 0 0 0 0 0 0))
        (negative-infinity (octets #xff #xf0 0 0 0 0 0 0)))
    (dolist (field (list 17.375d0 1.5d0 3000.125d0))
      (group (context (princ-to-string field))
        (dolist (bad (list nan infinity negative-infinity))
          (fail (decode-message (patch frame (double-octets field) bad))
                'malformed-message)))))
  (let ((frame (encode-message (sample 'entity-snapshot-message))))
    (group (context "a quaternion of norm 1.2")
      (fail (decode-message (patch frame (double-octets 0.8d0) (double-octets 0.9d0)))
            'malformed-message))
    (group (context "the same frame with a unit quaternion decodes")
      (true (decode-message (patch frame (double-octets 0.8d0)
                                   (double-octets 0.8d0))))))
  (group (context "an encoder refuses what a decoder would")
    (fail (encode-message (make-pose-message
                           :pose (make-pose :gait sb-ext:double-float-positive-infinity)))
          'unencodable-value)
    (fail (encode-message (make-pose-message :pose (make-pose :position #(0 1))))
          'unencodable-value)
    (fail (encode-message (make-entity-snapshot-message
                           :snapshot (make-entity-snapshot
                                      :entities (list (make-entity-state
                                                       :orientation #(1 1 0 0))))))
          'unencodable-value)))

(define-test out-of-range-values-are-refused
  (let ((frame (encode-message (sample 'pose-message))))
    (group (context "pitch beyond straight up")
      (fail (decode-message (patch frame (double-octets 0.625d0) (double-octets 1.6d0)))
            'malformed-message))
    (group (context "yaw beyond pi")
      (fail (decode-message (patch frame (double-octets -2.5d0) (double-octets -3.2d0)))
            'malformed-message))
    (group (context "speed beyond the limit")
      (fail (decode-message (patch frame (double-octets -9.75d0) (double-octets -2000d0)))
            'malformed-message))
    (group (context "a position outside the domain")
      (fail (decode-message frame :limits (make-wire-limits
                                           :domain-low '(-1000 -1000 -1000)
                                           :domain-high '(1000 1000 1000)))
            'malformed-message)))
  (group (context "make-pose takes wrapped yaw; wrap-yaw wraps")
    (true (< (abs (- (wrap-yaw (* 3 pi)) (- pi))) 1d-12))
    (fail (encode-message (make-pose-message :pose (make-pose :yaw 4)))
          'unencodable-value))
  (let ((frame (encode-message (sample 'edit-reply-message))))
    (group (context "an enumeration index past its members")
      ;; The outcome octet follows the four-octet request number.
      (let ((copy (copy-seq frame)))
        (setf (aref copy (+ +header-length+ 4)) 2)
        (fail (decode-message copy) 'malformed-message))))
  (let ((frame (encode-message (sample 'probe-message))))
    (group (context "a boolean octet that is not 0 or 1")
      (let ((copy (copy-seq frame)))
        (setf (aref copy +header-length+) 2)
        (fail (decode-message copy) 'malformed-message))))
  (group (context "sites lie inside the domain")
    (let ((limits (make-wire-limits :domain-low '(-10 -10 -10)
                                    :domain-high '(10 10 10)))
          (message (make-edit-request-message
                    :edits (list (make-edit :site (make-site 9 -10 0))))))
      (true (decode-message (encode-message message :limits limits) :limits limits))
      (fail (decode-message (encode-message message)
                            :limits (make-wire-limits :domain-low '(-10 -10 -10)
                                                      :domain-high '(9 10 10)))
            'malformed-message)
      (fail (encode-message (make-edit-request-message
                             :edits (list (make-edit :site (make-site 10 0 0))))
                            :limits limits)
            'unencodable-value)))
  (group (context "offsets lie inside the vocabulary")
    (let ((message (make-events-message
                    :events (list (make-event :after (make-block-identity :offset 9))))))
      (true (decode-message (encode-message message)
                            :limits (make-wire-limits :vocabulary-size 10)))
      (fail (decode-message (encode-message message)
                            :limits (make-wire-limits :vocabulary-size 9))
            'malformed-message)))
  (group (context "a welcome's offsets are checked against its own table")
    (let ((welcome (sample 'welcome-message)))
      (fail (encode-message
             (let ((copy (copy-structure welcome)))
               (setf (welcome-message-vocabulary copy) '("air" "stone" "glass"))
               copy))
            'unencodable-value)
      (setf welcome (copy-structure welcome)
            (welcome-message-vocabulary welcome) '("a" "b" "c" "d"))
      (group (context "a welcome's own table outranks the decoder's limits")
        (true (decode-message (encode-message welcome)
                              :limits (make-wire-limits :vocabulary-size 2))))
      ;; Fold "c" and "d" into one entry, "c\0\1d": the octets still align,
      ;; but the table now has three entries and offset 3 is outside it.
      (true (search "offset 3 outside a vocabulary of 3"
                    (handler-case
                        (decode-message
                         (patch (patch (encode-message welcome)
                                       (octets 0 0 0 4 0 1 97)
                                       (octets 0 0 0 3 0 1 97))
                                (octets 0 1 99 0 1 100) (octets 0 4 99 0 1 100)))
                      (malformed-message (condition)
                        (malformed-message-detail condition)))))))
  (group (context "malformed text and values")
    (let ((frame (encode-message (make-chat-message :text "ab"))))
      (fail (decode-message (patch frame (octets 0 2 97 98) (octets 0 2 #xc3 #x28)))
            'malformed-message))
    (fail (encode-message (make-chat-message :text (make-string 70000 :initial-element #\a)))
          'unencodable-value)
    (fail (encode-message (make-edit-request-message :cause (make-hash-table)))
          'unencodable-value)
    (fail (encode-message (make-edit-request-message :cause 1/3))
          'unencodable-value)
    (let ((deep '(1)))
      (dotimes (i 9) (setf deep (list deep)))
      (fail (encode-message (make-edit-request-message :cause deep))
            'unencodable-value))))

(define-test the-frame-decoder-reassembles-a-stream
  (let* ((messages (sample-messages))
         (stream (apply #'concatenate 'octets
                        (mapcar #'encode-message
                                (remove :datagram messages :key #'message-channel))))
         (decoder (make-frame-decoder))
         (decoded '()))
    (loop for octet across stream
          do (setf decoded (append decoded (feed-frame-decoder decoder (octets octet)))))
    (true (equalp (remove :datagram messages :key #'message-channel) decoded))
    (true (zerop (frame-decoder-buffered decoder))))
  (group (context "a stream that is not this protocol fails at its header")
    (let ((decoder (make-frame-decoder)))
      (fail (feed-frame-decoder decoder (map 'octets #'char-code
                                             "GET / HTTP/1.1
Host: example.com
Accept: */*
"))
            'malformed-message)))
  (group (context "a claimed body longer than a frame is refused before it arrives")
    (let ((frame (encode-message (sample 'chat-message)))
          (decoder (make-frame-decoder)))
      (replace frame (octets #x7f #xff #xff #xff) :start1 (- +header-length+ 4))
      (fail (feed-frame-decoder decoder (subseq frame 0 +header-length+))
            'malformed-message))))
