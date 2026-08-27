;;;; Claims about reading api.tl and about the records it decodes into.

(in-package #:telegram.tests)

(define-test tl-names-become-lisp-names
  (true (equal "SEND-MESSAGE" (tl:tl-lisp-name-string "sendMessage")))
  (true (equal "MESSAGES.SEND-MESSAGE" (tl:tl-lisp-name-string "messages.sendMessage")))
  (true (equal "INPUT-PEER-USER" (tl:tl-lisp-name-string "inputPeerUser")))
  (true (equal "ACCESS-HASH" (tl:tl-lisp-name-string "access_hash")))
  (group (context "runs of capitals stay together until a word starts")
    (true (equal "DATA-JSON" (tl:tl-lisp-name-string "DataJSON")))
    (true (equal "INPUT-BOT-INLINE-MESSAGE-ID"
                 (tl:tl-lisp-name-string "inputBotInlineMessageID")))
    (true (equal "MESSAGES.GET-DH-CONFIG"
                 (tl:tl-lisp-name-string "messages.getDhConfig"))))
  (group (context "and digits do not split a name that means to keep them")
    (true (equal "INT128" (tl:tl-lisp-name-string "int128")))))

(define-test the-schema-parses
  (group (context "a constructor with flags, optional fields, and a vector")
    (let ((entries (tl:parse-tl-schema
                    "someThing#1a2b3c4d flags:# quiet:flags.0?true
                     peer:InputPeer names:flags.1?Vector<string> n:int
                     = SomeType;")))
      (true (= 1 (length entries)))
      (destructuring-bind (name id function-p result result-name source fields)
          (first entries)
        (declare (ignore source))
        (true (equal "someThing" name))
        (true (= #x1a2b3c4d id))
        (true (not function-p))
        (true (equal "SomeType" result-name))
        (true (equal '(tl::object "SomeType") result))
        (true (= 5 (length fields)))
        (true (equal '("flags" tl::flags nil) (first fields)))
        (true (equal '("quiet" tl::flag ("flags" . 0)) (second fields)))
        (true (equal '("peer" (tl::object "InputPeer") nil) (third fields)))
        (group (context "and a vector of a built-in reuses Lisp's own type names")
          (true (equal '("names" (vector string) ("flags" . 1))
                       (fourth fields)))
          (true (equal '("n" tl::int nil) (fifth fields)))))))
  (group (context "the section marker decides what is callable")
    (let ((entries (tl:parse-tl-schema
                    "a#1 = A;
                     ---functions---
                     b#2 = B;")))
      (true (not (third (first entries))))
      (true (third (second entries)))))
  (group (context "declarations without a constructor id describe the codec itself")
    (true (null (tl:parse-tl-schema "int ? = Int;")))
    (true (null (tl:parse-tl-schema "vector#1cb5c415 {t:Type} # [ t ] = Vector t;"))))
  (group (context "and comments are not part of anything")
    (true (= 1 (length (tl:parse-tl-schema "// a note
                                          a#1 = A;"))))))

(define-test the-bundled-schema-loaded
  (true (< 2000 telegram::+api-schema-size+))
  (group (context "a well-known constructor is where it should be")
    (let ((definition (tl:find-tl-definition :input-peer-self)))
      (true (equal "inputPeerSelf" (tl:tl-definition-name definition)))
      (true (= #x7da07ec9 (tl:tl-definition-id definition)))
      (true (not (tl:tl-definition-function-p definition)))))
  (group (context "and a function knows what it returns, including a bare vector")
    (true (tl:tl-definition-function-p (tl:find-tl-definition :help.get-config)))
    (true (equal '(tl::object "Config")
                 (tl:tl-definition-result-specification
                  (tl:find-tl-definition :help.get-config))))
    (true (equal '(tl::vector (tl::object "ContactStatus"))
                 (tl:tl-definition-result-specification
                  (tl:find-tl-definition :contacts.get-statuses)))))
  (group (context "the four ids the schema repeats resolve to the real definition")
    (true (equal "invokeWithBusinessConnection"
                 (tl:tl-definition-name (tl:find-tl-definition #xdd289f8e)))))
  (group (context "and lookup accepts an id, a keyword, or the TL name")
    (let ((definition (tl:find-tl-definition :help.get-config)))
      (true (eq definition (tl:find-tl-definition #xc4f9186b)))
      (true (eq definition (tl:find-tl-definition "help.getConfig")))))
  (group (context "a name the schema does not have is an error, not a silent NIL")
    (fail (tl:find-tl-definition :no.such-method) 'tl:unknown-tl-name)))

(define-test records-round-trip-through-the-wire
  (let ((query (tl:make-tl :messages.send-message
                           :peer (tl:make-tl :input-peer-self)
                           :message "hello" :random-id 12345
                           :no-webpage t)))
    (group (context "the flags word is computed from what is actually set")
      ;; Spelled out rather than compared to a fixed string: this
      ;; constructor's id moves with the layer, and pinning the whole
      ;; encoding here would make a schema refresh look like a regression.
      (let ((encoded (tl:encode-tl-octets query)))
        (true (= (tl:tl-definition-id
                  (tl:find-tl-definition :messages.send-message))
                 (octets:octets-integer encoded :end 4 :endian :little)))
        (group (context "no_webpage is bit 1, and nothing else was set")
          (true (= 2 (octets:octets-integer encoded :start 4 :end 8
                                                    :endian :little))))
        (group (context "then the peer, the text, and the random id, in order")
          (true (equal "c97ea07d0568656c6c6f00003930000000000000"
                       (unhex (subseq encoded 8)))))))
    (let ((back (tl:decode-tl-octets (tl:encode-tl-octets query))))
      (true (eq :messages.send-message (tl:tl-name back)))
      (true (equal "hello" (tl:tl-value back :message)))
      (true (= 12345 (tl:tl-value back :random-id)))
      (group (context "a set flag comes back true and an unset one nil")
        (true (eq t (tl:tl-value back :no-webpage)))
        (true (null (tl:tl-value back :silent))))
      (group (context "an optional field that was never set stays absent")
        (true (null (tl:tl-value back :entities)))
        (true (null (tl:tl-value back :schedule-date))))
      (group (context "and a nested record is a record")
        (true (eq :input-peer-self (tl:tl-name (tl:tl-value back :peer))))))))

(define-test optional-vectors-keep-empty-apart-from-absent
  (group (context "which is why vectors decode as vectors and not as lists")
    (let* ((present (tl:make-tl :messages.send-message
                                :peer (tl:make-tl :input-peer-self)
                                :message "x" :random-id 1
                                :entities #()))
           (absent (tl:make-tl :messages.send-message
                               :peer (tl:make-tl :input-peer-self)
                               :message "x" :random-id 1))
           (decoded (tl:decode-tl-octets (tl:encode-tl-octets present))))
      (true (not (equalp (tl:encode-tl-octets present)
                         (tl:encode-tl-octets absent))))
      (true (equalp #() (tl:tl-value decoded :entities)))
      (true (null (tl:tl-value
                   (tl:decode-tl-octets (tl:encode-tl-octets absent))
                   :entities))))))

(define-test records-and-classes-share-one-decoder
  (group (context "an MTProto constructor is still a class, because methods want it")
    (true (typep (tl:decode-tl-octets
                  (tl:encode-tl-octets (make-instance 'mt:pong :message-id 1
                                                               :ping-id 2)))
                 'mt:pong)))
  (group (context "and a schema constructor is a record, because nothing does")
    (true (tl:tl-record-p (tl:decode-tl-octets
                           (tl:encode-tl-octets
                            (tl:make-tl :input-peer-chat :chat-id 7))))))
  (group (context "a constructor from neither is named, not swallowed")
    (fail (tl:decode-tl-octets (hex "0badbeef")) 'tl:unknown-tl-constructor)))

(define-test fields-are-checked-by-name
  (let ((record (tl:make-tl :input-peer-chat :chat-id 7)))
    (true (= 7 (tl:tl-value record :chat-id)))
    (setf (tl:tl-value record :chat-id) 9)
    (true (= 9 (tl:tl-value record :chat-id)))
    (group (context "and a field the constructor does not have says so")
      (fail (tl:tl-value record :nonsense) 'tl:unknown-tl-name)
      (true (null (tl:tl-value record :nonsense :errorp nil))))))

(define-test searching-the-schema
  (let ((found (tl:find-tl-definitions "getNearestDc")))
    (true (= 1 (length found)))
    (true (equal "help.getNearestDc" (tl:tl-definition-name (first found)))))
  (group (context "and the search can be narrowed to callable methods")
    (true (every #'tl:tl-definition-function-p
                 (tl:find-tl-definitions "messages.send" :functions t)))))

;;;; Compression

(defparameter +gzipped-hello+
  (hex "1f8b08000000000002ffcb48cdc9c9070086a6103605000000")
  "gzip of \"hello\": short enough that the encoder picked fixed Huffman.")

(defparameter +gzipped-repetitive+
  (hex "1f8b08000000000002ff2b49cd494d2f4acc5530502881310d114c2304d318c134
        41304d114c330473d4b051c3460d1b356cd4b051c3460d1b358c74c300c6c3
        ede998080000")
  "gzip of the repetitive text below: long enough to be worth a dynamic
Huffman table, which is the case with all the machinery in it.")

(defparameter +gzipped-input-peer-chat+
  (hex "1f8b08000000000002ffdb19b3d2949d010200664718db0c000000")
  "gzip of the twelve bytes inputPeerChat#35a95cb9 chat_id:7 encodes to.")

(defun repetitive-text ()
  (with-output-to-string (out)
    (dotimes (index 200)
      (format out "telegram ~D " (mod index 7)))))

(define-test deflate-inflates
  (group (context "a stored block, which is the degenerate case")
    (true (equalp (ascii "hi") (octets:inflate (hex "01 0200 fdff 6869")))))
  (group (context "a fixed-Huffman block under a gzip header")
    (true (equalp (ascii "hello") (octets:decompress +gzipped-hello+))))
  (group (context "a dynamic-Huffman block, tables and back-references and all")
    (true (equal (repetitive-text)
                 (octets:octets-string (octets:decompress +gzipped-repetitive+)))))
  (group (context "and a zlib wrapper is recognized as readily as a gzip one")
    (true (equalp (ascii "hello")
                  (octets:decompress (hex "789ccb48cdc9c90700062c0215")))))
  (group (context "while something that is not compressed at all says so")
    (fail (octets:decompress (hex "00")) 'octets:inflate-error)))

(define-test gzip-packed-is-seen-through
  (let* ((inner (tl:encode-tl-octets (tl:make-tl :input-peer-chat :chat-id 7)))
         (packed (tl:encode-tl-octets
                  (make-instance 'mt:gzip-packed
                                 :packed-data +gzipped-input-peer-chat+))))
    (true (equalp inner (octets:decompress +gzipped-input-peer-chat+)))
    (true (equalp inner (mt:unwrap-gzip packed)))
    (group (context "and something that was never compressed passes through")
      (true (equalp inner (mt:unwrap-gzip inner))))))
