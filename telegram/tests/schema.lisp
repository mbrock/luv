;;;; Claims about reading api.tl and about the records it decodes into.

(in-package #:telegram.tests)

(deftest tl-names-become-lisp-names
  (ok (equal "SEND-MESSAGE" (tl:tl-lisp-name-string "sendMessage")))
  (ok (equal "MESSAGES.SEND-MESSAGE" (tl:tl-lisp-name-string "messages.sendMessage")))
  (ok (equal "INPUT-PEER-USER" (tl:tl-lisp-name-string "inputPeerUser")))
  (ok (equal "ACCESS-HASH" (tl:tl-lisp-name-string "access_hash")))
  (testing "runs of capitals stay together until a word starts"
    (ok (equal "DATA-JSON" (tl:tl-lisp-name-string "DataJSON")))
    (ok (equal "INPUT-BOT-INLINE-MESSAGE-ID"
               (tl:tl-lisp-name-string "inputBotInlineMessageID")))
    (ok (equal "MESSAGES.GET-DH-CONFIG"
               (tl:tl-lisp-name-string "messages.getDhConfig"))))
  (testing "and digits do not split a name that means to keep them"
    (ok (equal "INT128" (tl:tl-lisp-name-string "int128")))))

(deftest the-schema-parses
  (testing "a constructor with flags, optional fields, and a vector"
    (let ((entries (tl:parse-tl-schema
                    "someThing#1a2b3c4d flags:# quiet:flags.0?true
                     peer:InputPeer names:flags.1?Vector<string> n:int
                     = SomeType;")))
      (ok (= 1 (length entries)))
      (destructuring-bind (name id function-p result result-name source fields)
          (first entries)
        (declare (ignore source))
        (ok (equal "someThing" name))
        (ok (= #x1a2b3c4d id))
        (ok (not function-p))
        (ok (equal "SomeType" result-name))
        (ok (equal '(tl::object "SomeType") result))
        (ok (= 5 (length fields)))
        (ok (equal '("flags" tl::flags nil) (first fields)))
        (ok (equal '("quiet" tl::flag ("flags" . 0)) (second fields)))
        (ok (equal '("peer" (tl::object "InputPeer") nil) (third fields)))
        (testing "and a vector of a built-in reuses Lisp's own type names"
          (ok (equal '("names" (vector string) ("flags" . 1))
                     (fourth fields)))
          (ok (equal '("n" tl::int nil) (fifth fields)))))))
  (testing "the section marker decides what is callable"
    (let ((entries (tl:parse-tl-schema
                    "a#1 = A;
                     ---functions---
                     b#2 = B;")))
      (ok (not (third (first entries))))
      (ok (third (second entries)))))
  (testing "declarations without a constructor id describe the codec itself"
    (ok (null (tl:parse-tl-schema "int ? = Int;")))
    (ok (null (tl:parse-tl-schema "vector#1cb5c415 {t:Type} # [ t ] = Vector t;"))))
  (testing "and comments are not part of anything"
    (ok (= 1 (length (tl:parse-tl-schema "// a note
                                          a#1 = A;"))))))

(deftest the-bundled-schema-loaded
  (ok (< 2000 telegram::+api-schema-size+))
  (testing "a well-known constructor is where it should be"
    (let ((definition (tl:find-tl-definition :input-peer-self)))
      (ok (equal "inputPeerSelf" (tl:tl-definition-name definition)))
      (ok (= #x7da07ec9 (tl:tl-definition-id definition)))
      (ok (not (tl:tl-definition-function-p definition)))))
  (testing "and a function knows what it returns, including a bare vector"
    (ok (tl:tl-definition-function-p (tl:find-tl-definition :help.get-config)))
    (ok (equal '(tl::object "Config")
               (tl:tl-definition-result-specification
                (tl:find-tl-definition :help.get-config))))
    (ok (equal '(tl::vector (tl::object "ContactStatus"))
               (tl:tl-definition-result-specification
                (tl:find-tl-definition :contacts.get-statuses)))))
  (testing "the four ids the schema repeats resolve to the real definition"
    (ok (equal "invokeWithBusinessConnection"
               (tl:tl-definition-name (tl:find-tl-definition #xdd289f8e)))))
  (testing "and lookup accepts an id, a keyword, or the TL name"
    (let ((definition (tl:find-tl-definition :help.get-config)))
      (ok (eq definition (tl:find-tl-definition #xc4f9186b)))
      (ok (eq definition (tl:find-tl-definition "help.getConfig")))))
  (testing "a name the schema does not have is an error, not a silent NIL"
    (signals (tl:find-tl-definition :no.such-method) 'tl:unknown-tl-name)))

(deftest records-round-trip-through-the-wire
  (let ((query (tl:make-tl :messages.send-message
                           :peer (tl:make-tl :input-peer-self)
                           :message "hello" :random-id 12345
                           :no-webpage t)))
    (testing "the flags word is computed from what is actually set"
      ;; Spelled out rather than compared to a fixed string: this
      ;; constructor's id moves with the layer, and pinning the whole
      ;; encoding here would make a schema refresh look like a regression.
      (let ((encoded (tl:encode-tl-octets query)))
        (ok (= (tl:tl-definition-id
                (tl:find-tl-definition :messages.send-message))
               (octets:octets-integer encoded :end 4 :endian :little)))
        (testing "no_webpage is bit 1, and nothing else was set"
          (ok (= 2 (octets:octets-integer encoded :start 4 :end 8
                                                  :endian :little))))
        (testing "then the peer, the text, and the random id, in order"
          (ok (equal "c97ea07d0568656c6c6f00003930000000000000"
                     (unhex (subseq encoded 8)))))))
    (let ((back (tl:decode-tl-octets (tl:encode-tl-octets query))))
      (ok (eq :messages.send-message (tl:tl-name back)))
      (ok (equal "hello" (tl:tl-value back :message)))
      (ok (= 12345 (tl:tl-value back :random-id)))
      (testing "a set flag comes back true and an unset one nil"
        (ok (eq t (tl:tl-value back :no-webpage)))
        (ok (null (tl:tl-value back :silent))))
      (testing "an optional field that was never set stays absent"
        (ok (null (tl:tl-value back :entities)))
        (ok (null (tl:tl-value back :schedule-date))))
      (testing "and a nested record is a record"
        (ok (eq :input-peer-self (tl:tl-name (tl:tl-value back :peer))))))))

(deftest optional-vectors-keep-empty-apart-from-absent
  (testing "which is why vectors decode as vectors and not as lists"
    (let* ((present (tl:make-tl :messages.send-message
                                :peer (tl:make-tl :input-peer-self)
                                :message "x" :random-id 1
                                :entities #()))
           (absent (tl:make-tl :messages.send-message
                               :peer (tl:make-tl :input-peer-self)
                               :message "x" :random-id 1))
           (decoded (tl:decode-tl-octets (tl:encode-tl-octets present))))
      (ok (not (equalp (tl:encode-tl-octets present)
                       (tl:encode-tl-octets absent))))
      (ok (equalp #() (tl:tl-value decoded :entities)))
      (ok (null (tl:tl-value
                 (tl:decode-tl-octets (tl:encode-tl-octets absent))
                 :entities))))))

(deftest records-and-classes-share-one-decoder
  (testing "an MTProto constructor is still a class, because methods want it"
    (ok (typep (tl:decode-tl-octets
                (tl:encode-tl-octets (make-instance 'mt:pong :message-id 1
                                                             :ping-id 2)))
               'mt:pong)))
  (testing "and a schema constructor is a record, because nothing does"
    (ok (tl:tl-record-p (tl:decode-tl-octets
                         (tl:encode-tl-octets
                          (tl:make-tl :input-peer-chat :chat-id 7))))))
  (testing "a constructor from neither is named, not swallowed"
    (signals (tl:decode-tl-octets (hex "0badbeef")) 'tl:unknown-tl-constructor)))

(deftest fields-are-checked-by-name
  (let ((record (tl:make-tl :input-peer-chat :chat-id 7)))
    (ok (= 7 (tl:tl-value record :chat-id)))
    (setf (tl:tl-value record :chat-id) 9)
    (ok (= 9 (tl:tl-value record :chat-id)))
    (testing "and a field the constructor does not have says so"
      (signals (tl:tl-value record :nonsense) 'tl:unknown-tl-name)
      (ok (null (tl:tl-value record :nonsense :errorp nil))))))

(deftest searching-the-schema
  (let ((found (tl:find-tl-definitions "getNearestDc")))
    (ok (= 1 (length found)))
    (ok (equal "help.getNearestDc" (tl:tl-definition-name (first found)))))
  (testing "and the search can be narrowed to callable methods"
    (ok (every #'tl:tl-definition-function-p
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

(deftest deflate-inflates
  (testing "a stored block, which is the degenerate case"
    (ok (equalp (ascii "hi") (octets:inflate (hex "01 0200 fdff 6869")))))
  (testing "a fixed-Huffman block under a gzip header"
    (ok (equalp (ascii "hello") (octets:decompress +gzipped-hello+))))
  (testing "a dynamic-Huffman block, tables and back-references and all"
    (ok (equal (repetitive-text)
               (octets:octets-string (octets:decompress +gzipped-repetitive+)))))
  (testing "and a zlib wrapper is recognized as readily as a gzip one"
    (ok (equalp (ascii "hello")
                (octets:decompress (hex "789ccb48cdc9c90700062c0215")))))
  (testing "while something that is not compressed at all says so"
    (signals (octets:decompress (hex "00")) 'octets:inflate-error)))

(deftest gzip-packed-is-seen-through
  (let* ((inner (tl:encode-tl-octets (tl:make-tl :input-peer-chat :chat-id 7)))
         (packed (tl:encode-tl-octets
                  (make-instance 'mt:gzip-packed
                                 :packed-data +gzipped-input-peer-chat+))))
    (ok (equalp inner (octets:decompress +gzipped-input-peer-chat+)))
    (ok (equalp inner (mt:unwrap-gzip packed)))
    (testing "and something that was never compressed passes through"
      (ok (equalp inner (mt:unwrap-gzip inner))))))
