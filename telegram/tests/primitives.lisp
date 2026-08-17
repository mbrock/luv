;;;; Claims about the layers that have nothing to do with Telegram: bytes,
;;;; digests, the block cipher, the TL codec, and transport framing.

(in-package #:telegram.tests)

(deftest octet-conversions
  (testing "hexadecimal round trips and tolerates layout"
    (ok (equal "00ff10" (unhex (hex "00 ff 10"))))
    (ok (equalp (hex "0a0b") (octets:to-octets '(10 11)))))
  (testing "integers convert in both orders and pad on request"
    (ok (= #x0102 (octets:octets-integer (hex "0102"))))
    (ok (= #x0201 (octets:octets-integer (hex "0102") :endian :little)))
    (ok (equal "0000000102" (unhex (octets:integer-octets #x0102 :length 5))))
    (ok (equal "0201000000" (unhex (octets:integer-octets #x0102 :length 5
                                                                 :endian :little)))))
  (testing "base64 decodes PEM bodies, padding and all"
    (ok (equal "" (unhex (octets:base64-octets ""))))
    (ok (equal (unhex (ascii "any carnal pleasure."))
               (unhex (octets:base64-octets "YW55IGNhcm5hbCBwbGVhc3VyZS4=")))))
  (testing "exclusive or is length-checked"
    (ok (equal "0f0f" (unhex (octets:octets-xor (hex "00ff") (hex "0ff0")))))))

(deftest sha-1-agrees-with-fips-180-4
  (ok (equal "da39a3ee5e6b4b0d3255bfef95601890afd80709"
             (unhex (crypto:sha-1 (ascii "")))))
  (ok (equal "a9993e364706816aba3e25717850c26c9cd0d89d"
             (unhex (crypto:sha-1 (ascii "abc")))))
  (ok (equal "84983e441c3bd26ebaae4aa1f95129e5e54670f1"
             (unhex (crypto:sha-1
                     (ascii "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")))))
  (testing "and over a message long enough to need many blocks"
    (ok (equal "34aa973cd4c4daa4f61eeb2bdbad27316534016f"
               (unhex (crypto:sha-1
                       (octets:make-octets 1000000
                                           :initial-element (char-code #\a))))))))

(deftest sha-256-agrees-with-fips-180-4
  (ok (equal "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
             (unhex (crypto:sha-256 (ascii "")))))
  (ok (equal "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
             (unhex (crypto:sha-256 (ascii "abc")))))
  (ok (equal "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
             (unhex (crypto:sha-256
                     (ascii "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"))))))

(deftest aes-256-agrees-with-fips-197
  (let ((schedule (crypto:aes-key-schedule (counting-octets 32)))
        (plaintext (hex "00112233445566778899aabbccddeeff")))
    (testing "appendix C.3's known answer"
      (ok (equal "8ea2b7ca516745bfeafc49904b496089"
                 (unhex (crypto:aes-encrypt-block schedule plaintext)))))
    (testing "and decryption undoes it"
      (ok (equalp plaintext
                  (crypto:aes-decrypt-block
                   schedule (crypto:aes-encrypt-block schedule plaintext)))))))

(deftest ige-round-trips
  (let ((key (counting-octets 32))
        (iv (counting-octets 32 32))
        (data (counting-octets 64 64)))
    (ok (equalp data (crypto:ige-decrypt (crypto:ige-encrypt data key iv)
                                         key iv)))
    (testing "and chains, so a one-byte change alters everything after it"
      (let ((altered (copy-seq data)))
        (setf (aref altered 0) (logxor 1 (aref altered 0)))
        (ok (not (equalp (crypto:ige-encrypt data key iv)
                         (crypto:ige-encrypt altered key iv))))))))

(deftest modular-arithmetic
  (testing "exponentiation matches the naive definition on small inputs"
    (ok (= (mod (expt 7 117) 1009) (crypto:expt-mod 7 117 1009)))
    (ok (= 1 (crypto:expt-mod 12345 0 97))))
  (testing "and factors Telegram's proof of work"
    (multiple-value-bind (p q) (crypto:factor-pq (hex "19546f942a11278d"))
      (ok (= p #x44b2e50d))
      (ok (= q #x5e63ac81))
      (ok (= (* p q) (octets:octets-integer (hex "19546f942a11278d")))))))

;;;; RSA
;;;;
;;;; The modulus below is the key Telegram's published sample handshake uses,
;;;; and the ciphertext is what an independent implementation produces for
;;;; the same payload and the same (all-zero) padding.

(defparameter +sample-modulus+
  (hex "c8c11d635691fac091dd9489aedced2932aa8a0bcefef05fa800892d9b52ed03
        200865c9e97211cb2ee6c7ae96d3fb0e15aeffd66019b44a08a240cfdd2868a8
        5e1f54d6fa5deaa041f6941ddf302690d61dc476385c2fa655142353cb4e4b59
        f6e5b6584db76fe8b1370263246c010c93d011014113ebdf987d093f9d37c2be
        48352d69a1683f8f6e6c2167983c761e3ab169fde5daaa12123fa1beab621e4d
        a5935e9c198f82f35eae583a99386d8110ea6bd1abb0f568759f62694419ea5f
        69847c43462abef858b4cb5edc84e7b9226cd7bd7e183aa974a712c079dde85b
        9dc063b8a5c08e8f859c0ee5dcd824c7807f20153361a7f63cfd2a433a1be7f5"))

(defparameter +production-modulus+
  (hex "e8bb3305c0b52c6cf2afdf7637313489e63e05268e5badb601af417786472e5f
        93b85438968e20e6729a301c0afc121bf7151f834436f7fda680847a66bf64ac
        cec78ee21c0b316f0edafe2f41908da7bd1f4a5107638eeb67040ace472a14f9
        0d9f7c2b7def99688ba3073adb5750bb02964902a359fe745d8170e36876d4fd
        8a5d41b2a76cbff9a13267eb9580b2d06d10357448d20d9da2191cb5d8c93982
        961cdfdeda629e37f1fb09a0722027696032fe61ed663db7a37f6f263d370f69
        db53a0dc0a1748bdaaff6209d5645485e6e001d1953255757e4b8e42813347b1
        1da6ab500fd0ace7e6dfa3736199ccaf9397ed0745a427dcfa6cd67bcb1acff3"))

(deftest public-key-fingerprints
  (let ((key (crypto:make-public-key +sample-modulus+ 65537)))
    (ok (= #xb25898df208d2603 (crypto:public-key-fingerprint key)))
    (testing "and selection prefers the server's own ordering"
      (ok (eq key (crypto:select-public-key
                   (list key) (list 0 #xb25898df208d2603))))
      (signals (crypto:select-public-key (list key) (list 0 1))
               'crypto:unknown-public-key))))

(deftest bundled-keys-parse-from-pem
  (testing "the sample key's PEM and its raw modulus describe one key"
    (let ((from-pem (first (mt:public-keys-from-pem-text
                            mt:+telegram-sample-key-pem+)))
          (from-modulus (crypto:make-public-key +sample-modulus+ 65537)))
      (ok (= (crypto:public-key-modulus from-pem)
             (crypto:public-key-modulus from-modulus)))
      (ok (= (crypto:public-key-fingerprint from-pem)
             (crypto:public-key-fingerprint from-modulus)))))
  (testing "and so do the production key's"
    (let ((from-pem (first (mt:telegram-public-keys))))
      (ok (= (crypto:public-key-modulus from-pem)
             (octets:octets-integer +production-modulus+)))
      (ok (= 65537 (crypto:public-key-exponent from-pem))))))

(deftest padded-rsa-matches-an-independent-implementation
  (let ((key (crypto:make-public-key +production-modulus+ 65537 :mode :padded))
        (plaintext (octets:make-octets 144 :initial-element #x61)))
    (ok (= 224 (crypto:rsa-required-random-length key)))
    (ok (equal
         "bf68719e836806b040cd261ecaf66eb3c4ba19f3bbea3031b2e6cf29167bab647201d101b291dc5b716a42e789a38d947fe59e9bcce8f30ef46a946743ea8b6babbce7fc0afc46b802aa453e83471d82a4dfad83f971f350b4b4fb474cd1c48fdf427e4b5fecce9ec3178ae7dac3985856fdefa21d6fdc5e0e0fd8a57bc4f51580d637d372be8d87c9aa3fde8e6f8287bcb3be846aadcdd59465375479e248f62ed438f9804fbe36d41ca906243a5f740f3937949aa149ba8a8b8e68b3f3e1e3cd3f946387520e21eee55845e1f015a919a22f6a72bfaecd2cae946c91983b41f9ffabe97963bbde8f30eaf5fd3c5b8cecab8711bd269e441b6084f385726ff0"
         (unhex (crypto:rsa-encrypt plaintext key
                                    (octets:make-octets 224)))))))

;;;; The TL codec

(deftest tl-primitives-round-trip
  (testing "bytes carry their padding to a four-byte boundary"
    (ok (equal "01000000" (unhex (tl:with-tl-writer (writer)
                                   (tl:write-tl-bytes writer (hex "00"))))))
    (dolist (length '(0 1 2 3 4 253 254 255 300))
      (let* ((value (counting-octets length))
             (encoded (tl:with-tl-writer (writer)
                        (tl:write-tl-bytes writer value))))
        (ok (zerop (mod (length encoded) 4)))
        (let ((reader (tl:make-tl-reader encoded)))
          (ok (equalp value (tl:read-tl-bytes reader)))
          (ok (tl:tl-reader-exhausted-p reader))))))
  (testing "integers keep their signedness"
    (let ((encoded (tl:with-tl-writer (writer)
                     (tl:write-tl-int writer -1)
                     (tl:write-tl-signed-long writer -1)
                     (tl:write-tl-long writer #xFFFFFFFFFFFFFFFF))))
      (ok (equal "ffffffffffffffffffffffffffffffffffffffff" (unhex encoded)))
      (let ((reader (tl:make-tl-reader encoded)))
        (ok (= -1 (tl:read-tl-int reader)))
        (ok (= -1 (tl:read-tl-signed-long reader)))
        (ok (= #xFFFFFFFFFFFFFFFF (tl:read-tl-long reader))))))
  (testing "vectors carry their constructor"
    (let ((encoded (tl:with-tl-writer (writer)
                     (tl:write-tl-vector writer '(1 2 3) #'tl:write-tl-long))))
      (ok (= tl:+vector-constructor+
             (octets:octets-integer encoded :end 4 :endian :little)))
      (ok (equalp #(1 2 3) (tl:read-tl-vector (tl:make-tl-reader encoded)
                                              #'tl:read-tl-long)))))
  (testing "and a short stream is an error rather than a silent zero"
    (signals (tl:read-tl-long (tl:make-tl-reader (hex "0102")))
             'tl:short-tl-data)))

(deftest tl-objects-encode-as-the-schema-says
  (testing "req_pq_multi"
    (ok (equal "f18e7ebe4e44b426241e8b839153122d44585ac6"
               (unhex (tl:encode-tl-octets
                       (make-instance 'mt:req-pq-multi
                                      :nonce (hex "4e44b426241e8b839153122d44585ac6")))))))
  (testing "req_DH_params, against an independent implementation"
    (ok (equal "bee412d7000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f0101000001020000080706050403020103616263"
               (unhex (tl:encode-tl-octets
                       (make-instance 'mt:req-dh-params
                                      :nonce (counting-octets 16)
                                      :server-nonce (counting-octets 16 16)
                                      :p (hex "01") :q (hex "02")
                                      :public-key-fingerprint #x0102030405060708
                                      :encrypted-data (ascii "abc"))))))))

(deftest res-pq-decodes-into-an-object
  (let ((response (tl:decode-tl-octets
                   (hex "632416054e44b426241e8b839153122d44585ac665ba0b393e1094329eda2c42
                         d62833030819546f942a11278d00000015c4b51c0300000003268d20df9858b2
                         029f4ba16d109296216be86c022bb4c3"))))
    (ok (typep response 'mt:res-pq))
    (ok (equalp (hex "4e44b426241e8b839153122d44585ac6")
                (mt:res-pq-nonce response)))
    (ok (equalp (hex "65ba0b393e1094329eda2c42d6283303")
                (mt:res-pq-server-nonce response)))
    (ok (equalp (hex "19546f942a11278d") (mt:res-pq-pq response)))
    (ok (= 3 (length (mt:res-pq-server-public-key-fingerprints response))))
    (ok (= #xb25898df208d2603
           (elt (mt:res-pq-server-public-key-fingerprints response) 0)))
    (testing "and re-encodes to exactly what arrived"
      (ok (equalp (hex "632416054e44b426241e8b839153122d44585ac665ba0b393e1094329eda2c42
                         d62833030819546f942a11278d00000015c4b51c0300000003268d20df9858b2
                         029f4ba16d109296216be86c022bb4c3")
                  (tl:encode-tl-octets response))))))

(deftest unknown-constructors-name-themselves
  (signals (tl:decode-tl-octets (hex "deadbeef")) 'tl:unknown-tl-constructor))

;;;; Transport framing

(deftest abridged-framing
  (let ((transport (make-instance 'mt:abridged-transport)))
    (ok (equal "ef" (unhex (mt:transport-client-prefix transport))))
    (testing "short payloads take a one-byte header counting words"
      (ok (equal "0100000000" (unhex (mt:encode-transport-frame
                                      transport (hex "00000000"))))))
    (testing "long payloads escape to the four-byte header"
      (let* ((payload (counting-octets 1024))
             (frame (mt:encode-transport-frame transport payload)))
        (ok (= #x7F (aref frame 0)))
        (ok (= (+ 4 1024) (length frame)))))
    (testing "and a decoder reassembles frames split across reads"
      (let* ((payloads (list (counting-octets 8) (counting-octets 1024)
                             (counting-octets 4 9)))
             (stream (apply #'octets:concatenate-octets
                            (mapcar (lambda (payload)
                                      (mt:encode-transport-frame transport
                                                                 payload))
                                    payloads)))
             (decoder (mt:make-frame-decoder transport))
             (frames '()))
        (loop for start from 0 below (length stream) by 7
              do (setf frames
                       (append frames
                               (mt:feed-transport
                                decoder
                                (subseq stream start
                                        (min (length stream) (+ start 7)))))))
        (ok (= (length payloads) (length frames)))
        (ok (every #'equalp payloads frames))))
    (testing "and recognizes a quick acknowledgement"
      (let ((frames (mt:feed-transport (mt:make-frame-decoder transport)
                                       (hex "81020304"))))
        (ok (= 1 (length frames)))
        (ok (typep (first frames) 'mt:quick-ack))
        (ok (= #x81020304 (mt:quick-ack-token (first frames))))))))

(deftest intermediate-framing
  (let ((transport (make-instance 'mt:intermediate-transport)))
    (ok (equal "eeeeeeee" (unhex (mt:transport-client-prefix transport))))
    (let* ((payload (counting-octets 6))
           (frame (mt:encode-transport-frame transport payload)))
      (ok (equal "06000000000102030405" (unhex frame)))
      (ok (equalp (list payload)
                  (mt:feed-transport (mt:make-frame-decoder transport)
                                     frame))))))
