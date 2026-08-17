;;;; Transport framing.
;;;;
;;;; Below MTProto's own envelopes there is a framing layer whose only job is
;;;; to mark where one payload ends and the next begins.  Telegram defines
;;;; several; they differ in header shape and in nothing else, and new ones
;;;; keep appearing (obfuscated, padded intermediate).  That is a vocabulary
;;;; we grow, so it is a class and three generic functions.

(in-package #:telegram)

(defclass mtproto-transport ()
  ()
  (:documentation
   "A framing discipline for a byte stream carrying MTProto payloads."))

(defgeneric transport-client-prefix (transport)
  (:documentation
   "The bytes a client writes once, before any frame, to announce which
framing it will use."))

(defgeneric encode-transport-frame (transport payload &key quick-ack)
  (:documentation
   "PAYLOAD wrapped in one frame."))

(defgeneric decode-transport-frame (transport buffer start end)
  (:documentation
   "Try to read one frame from BUFFER between START and END.  Returns the
frame and how many bytes it consumed, or NIL when more bytes are needed."))

(defclass quick-ack ()
  ((token :initarg :token :reader quick-ack-token))
  (:documentation
   "A server acknowledgement that arrives as a bare token rather than a
message.  Frames come back as either one of these or a payload."))

(defmethod print-object ((ack quick-ack) stream)
  (print-unreadable-object (ack stream :type t)
    (format stream "#x~8,'0X" (quick-ack-token ack))))

;;;; The abridged transport
;;;;
;;;; One length byte for payloads under 508 bytes, and a four-byte escape
;;;; above that.  Lengths are counted in 32-bit words, so payloads must be a
;;;; multiple of four -- which every MTProto payload already is.

(defclass abridged-transport (mtproto-transport)
  ()
  (:documentation
   "Telegram's abridged TCP transport: the smallest header of the family."))

(defconstant +abridged-maximum-words+ #x1000000)

(defmethod transport-client-prefix ((transport abridged-transport))
  (octets:to-octets '(#xEF)))

(defmethod encode-transport-frame ((transport abridged-transport) payload
                                   &key quick-ack)
  (assert (zerop (mod (length payload) 4)) (payload)
          "Abridged payloads must be a whole number of words, got ~D bytes."
          (length payload))
  (let ((words (floor (length payload) 4)))
    (assert (< words +abridged-maximum-words+) (payload)
            "Abridged payload of ~D bytes is too large." (length payload))
    (tl:with-tl-writer (writer)
      (if (< words #x7F)
          (tl:write-tl-raw writer (list (if quick-ack (+ words #x80) words)))
          (tl:write-tl-raw writer
                           (list (if quick-ack #xFF #x7F)
                                 (ldb (byte 8 0) words)
                                 (ldb (byte 8 8) words)
                                 (ldb (byte 8 16) words))))
      (tl:write-tl-raw writer payload))))

(defmethod decode-transport-frame ((transport abridged-transport) buffer
                                   start end)
  (let ((available (- end start)))
    (when (zerop available)
      (return-from decode-transport-frame nil))
    (let ((first (aref buffer start)))
      (cond
        ;; A high bit in the first byte marks a quick acknowledgement, which
        ;; the server sends as four bytes in the opposite order.
        ((>= first #x80)
         (when (< available 4)
           (return-from decode-transport-frame nil))
         (values (make-instance
                  'quick-ack
                  :token (octets:octets-integer buffer :start start
                                                       :end (+ start 4)))
                 4))
        ((= first #x7F)
         (when (< available 4)
           (return-from decode-transport-frame nil))
         (let* ((words (octets:octets-integer buffer :start (+ start 1)
                                                     :end (+ start 4)
                                                     :endian :little))
                (length (* 4 words)))
           (when (< available (+ 4 length))
             (return-from decode-transport-frame nil))
           (values (octets:to-octets (subseq buffer (+ start 4)
                                             (+ start 4 length)))
                   (+ 4 length))))
        (t
         (let ((length (* 4 first)))
           (when (< available (+ 1 length))
             (return-from decode-transport-frame nil))
           (values (octets:to-octets (subseq buffer (+ start 1)
                                             (+ start 1 length)))
                   (+ 1 length))))))))

;;;; The intermediate transport
;;;;
;;;; A plain 32-bit little-endian length.  Slightly larger on the wire and
;;;; considerably easier to reason about, which is why most current clients
;;;; use it.

(defclass intermediate-transport (mtproto-transport)
  ()
  (:documentation
   "Telegram's intermediate TCP transport: a four-byte little-endian length
followed by the payload."))

(defmethod transport-client-prefix ((transport intermediate-transport))
  (octets:to-octets '(#xEE #xEE #xEE #xEE)))

(defmethod encode-transport-frame ((transport intermediate-transport) payload
                                   &key quick-ack)
  (declare (ignore quick-ack))
  (tl:with-tl-writer (writer)
    (tl:write-tl-int writer (length payload))
    (tl:write-tl-raw writer payload)))

(defmethod decode-transport-frame ((transport intermediate-transport) buffer
                                   start end)
  (let ((available (- end start)))
    (when (< available 4)
      (return-from decode-transport-frame nil))
    (let ((length (octets:octets-integer buffer :start start :end (+ start 4)
                                                :endian :little)))
      (when (< available (+ 4 length))
        (return-from decode-transport-frame nil))
      (values (octets:to-octets (subseq buffer (+ start 4) (+ start 4 length)))
              (+ 4 length)))))

;;;; Reassembly

(defclass frame-decoder ()
  ((transport :initarg :transport :reader frame-decoder-transport)
   (buffer :initform (make-array 0 :element-type '(unsigned-byte 8)
                                   :adjustable t :fill-pointer 0)
           :reader frame-decoder-buffer))
  (:documentation
   "Holds the bytes a socket has produced but a frame has not yet claimed."))

(defun make-frame-decoder (transport)
  "A reassembly buffer for TRANSPORT."
  (make-instance 'frame-decoder :transport transport))

(defun feed-transport (decoder octets)
  "Add OCTETS to DECODER and return every frame that has become complete, in
order.  A frame is either a payload byte vector or a QUICK-ACK."
  (let ((buffer (frame-decoder-buffer decoder))
        (transport (frame-decoder-transport decoder))
        (frames '()))
    (map nil (lambda (byte) (vector-push-extend byte buffer)) octets)
    (loop with start = 0
          do (multiple-value-bind (frame consumed)
                 (decode-transport-frame transport buffer start
                                         (fill-pointer buffer))
               (cond (consumed (push frame frames) (incf start consumed))
                     (t (replace buffer buffer :start2 start
                                               :end2 (fill-pointer buffer))
                        (decf (fill-pointer buffer) start)
                        (return (nreverse frames))))))))
