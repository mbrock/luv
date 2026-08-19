;;;; Claims about the crypto a two-factor login needs, and about the pieces
;;;; of the login flow that can be checked without an account.

(in-package #:telegram.tests)

(deftest sha-512-agrees-with-fips-180-4
  (ok (equal (concatenate 'string
                          "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc"
                          "83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f"
                          "63b931bd47417a81a538327af927da3e")
             (unhex (crypto:sha-512 (ascii "")))))
  (ok (equal (concatenate 'string
                          "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea2"
                          "0a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd"
                          "454d4423643ce80e2a9ac94fa54ca49f")
             (unhex (crypto:sha-512 (ascii "abc")))))
  (testing "and over a message long enough to need a second block"
    (ok (equal (concatenate 'string
                            "8e959b75dae313da8cf4f72814fc143f8f7779c6eb9f7fa1"
                            "7299aeadb6889018501d289e4900f7e4331b99dec4b5433a"
                            "c7d329eeb6dd26545e96e55b874be909")
               (unhex (crypto:sha-512
                       (ascii (concatenate
                               'string
                               "abcdefghbcdefghicdefghijdefghijkefghijkl"
                               "fghijklmghijklmnhijklmnoijklmnopjklmnopq"
                               "klmnopqrlmnopqrsmnopqrstnopqrstu"))))))))

(deftest round-constants-come-from-their-definition
  (testing "the primes are the primes"
    (ok (equal '(2 3 5 7 11 13 17 19 23 29) (crypto:small-primes 10))))
  (testing "and the roots are exact, computed on integers"
    (ok (= 4 (crypto::integer-root 64 3)))
    (ok (= 4 (crypto::integer-root 80 3)))
    (ok (= 1000 (crypto::integer-root 1000000 2))))
  (testing "so the first constants match what FIPS 180-4 prints"
    (ok (= #x6a09e667 (aref crypto::+sha-256-initial-state+ 0)))
    (ok (= #x428a2f98 (aref crypto::+sha-256-constants+ 0)))
    (ok (= #xc67178f2 (aref crypto::+sha-256-constants+ 63)))
    (ok (= #x6a09e667f3bcc908 (aref crypto::+sha-512-initial-state+ 0)))
    (ok (= #x428a2f98d728ae22 (aref crypto::+sha-512-constants+ 0)))
    (ok (= #x6c44198c4a475817 (aref crypto::+sha-512-constants+ 79)))))

(deftest hmac-agrees-with-rfc-4231
  (ok (equal (concatenate 'string
                          "87aa7cdea5ef619d4ff0b4241a1d6cb02379f4e2ce4ec278"
                          "7ad0b30545e17cdedaa833b7d6b8a702038b274eaea3f4e4"
                          "be9d914eeb61f1702e696c203a126854")
             (unhex (crypto:hmac (octets:make-octets 20 :initial-element #x0b)
                                 (ascii "Hi There")))))
  (testing "including the case where the key is shorter than the block"
    (ok (equal (concatenate 'string
                            "164b7a7bfcf819e2e395fbe73b56e0a387bd64222e831fd6"
                            "10270cd7ea2505549758bf75c05a994a6d034f65f8f0e6fd"
                            "caeab1a34d4a6b4b636e070a38bce737")
               (unhex (crypto:hmac (ascii "Jefe")
                                   (ascii "what do ya want for nothing?")))))))

(deftest pbkdf2-stretches-as-rfc-2898-says
  (ok (equal (concatenate 'string
                          "867f70cf1ade02cff3752599a3a53dc4af34c7a669815ae5"
                          "d513554e1c8cf252c02d470a285a0501bad999bfe943c08f"
                          "050235d7d68b1da55e63f73b60a57fce")
             (unhex (crypto:pbkdf2 (ascii "password") (ascii "salt") 1 64))))
  (testing "and iterating really does change the answer"
    (ok (equal (concatenate 'string
                            "d197b1b33db0143e018b12f3d1d1479e6cdebdcc97c5c0f8"
                            "7f6902e072f457b5143f30602641b3d55cd335988cb36b84"
                            "376060ecd532e039b742a239434af2d5")
               (unhex (crypto:pbkdf2 (ascii "password") (ascii "salt")
                                     4096 64))))))

;;;; SRP
;;;;
;;;; Telegram's 2048-bit group, which is also the one MTProto's own
;;;; Diffie-Hellman uses.  The test plays the server: it knows the password,
;;;; so it can compute the verifier, choose b, and check that the client's
;;;; proof is the one it would have accepted.

(defparameter +srp-prime+
  (octets:octets-integer
   (hex "C71CAEB9C6B1C9048E6C522F70F13F73980D40238E3E21C14934D037563D930F
         48198A0AA7C14058229493D22530F4DBFA336F6E0AC925139543AED44CCE7C37
         20FD51F69458705AC68CD4FE6B6B13ABDC9746512969328454F18FAF8C595F64
         2477FE96BB2A941D5BCD1D4AC8CC49880708FA9B378E3C4F3A9060BEE67CF9A4
         A4A695811051907E162753B56B0F6B410DBA74D8A84B2A14B3144E0EF1284754
         FD17ED950D5965B4B9DD46582DB1178D169C6BC465B0D6FF9CA3928FEF5B9AE4
         E418FC15E83EBEA0F87FA9FF5EED70050DED2849F47BF959D956850CE929851F
         0D8115F635B105EE2E4E15D04B2454BF6F4FADF034B10403119CD8E3B92FCC5B"))
  "Telegram's published 2048-bit safe prime.")

(defconstant +srp-generator+ 3)

(deftest the-published-group-passes-its-checks
  (ok (crypto:check-srp-group +srp-prime+ +srp-generator+))
  (testing "and a modulus that is merely odd does not"
    (signals (crypto:check-srp-group (1+ +srp-prime+) +srp-generator+)
             'crypto:srp-error))
  (testing "nor a generator outside the permitted set"
    (signals (crypto:check-srp-group +srp-prime+ 11) 'crypto:srp-error)))

(deftest miller-rabin-knows-primes-from-composites
  (ok (crypto:miller-rabin-probably-prime-p 2))
  (ok (crypto:miller-rabin-probably-prime-p 65537))
  (ok (not (crypto:miller-rabin-probably-prime-p 65536)))
  (ok (not (crypto:miller-rabin-probably-prime-p (* 1000003 1000033))))
  (testing "and is not fooled by a Carmichael number"
    (ok (not (crypto:miller-rabin-probably-prime-p 561)))
    (ok (not (crypto:miller-rabin-probably-prime-p 41041)))))

(defun srp-server-check (password salt1 salt2 secret server-secret)
  "Play Telegram's side of an SRP exchange and return what it would send, and
what proof it would then expect.

Knowing the password is exactly what a server does know -- it stores the
verifier v -- so this can compute both halves and check that the client's M1
is the one the server would have accepted."
  (let* ((prime +srp-prime+)
         (generator +srp-generator+)
         (x (octets:octets-integer
             (crypto:srp-password-hash password salt1 salt2 :iterations 16)))
         (v (crypto:expt-mod generator x prime))
         (k (octets:octets-integer
             (crypto:sha-256
              (octets:concatenate-octets (crypto::srp-number prime)
                                         (crypto::srp-number generator)))))
         (server-public (mod (+ (* k v)
                                (crypto:expt-mod generator server-secret prime))
                             prime))
         (client-public (crypto:expt-mod generator
                                         (octets:octets-integer secret) prime))
         (u (octets:octets-integer
             (crypto:sha-256
              (octets:concatenate-octets (crypto::srp-number client-public)
                                         (crypto::srp-number server-public)))))
         ;; The server's own view of the shared secret: (A * v^u)^b.
         (shared (crypto:expt-mod (mod (* client-public
                                          (crypto:expt-mod v u prime))
                                       prime)
                                  server-secret prime))
         (expected
           (crypto:sha-256
            (octets:concatenate-octets
             (octets:octets-xor (crypto:sha-256 (crypto::srp-number prime))
                                (crypto:sha-256 (crypto::srp-number generator)))
             (crypto:sha-256 salt1)
             (crypto:sha-256 salt2)
             (crypto::srp-number client-public)
             (crypto::srp-number server-public)
             (crypto:sha-256 (crypto::srp-number shared))))))
    (values server-public expected)))

(deftest srp-proves-the-password-to-a-server-that-knows-it
  (let* ((salt1 (counting-octets 32))
         (salt2 (counting-octets 16 64))
         (secret (counting-octets 256 7))
         (server-secret (octets:octets-integer (counting-octets 256 99))))
    (multiple-value-bind (server-public expected)
        (srp-server-check "hunter2" salt1 salt2 secret server-secret)
      (multiple-value-bind (public proof)
          (crypto:srp-check-password "hunter2"
                                     :prime +srp-prime+
                                     :generator +srp-generator+
                                     :salt1 salt1 :salt2 salt2
                                     :server-public server-public
                                     :secret-octets secret
                                     :iterations 16)
        (ok (= 256 (length public)))
        (ok (equalp expected proof))
        (testing "and the wrong password produces a proof the server rejects"
          (multiple-value-bind (other-public other-proof)
              (crypto:srp-check-password "hunter3"
                                         :prime +srp-prime+
                                         :generator +srp-generator+
                                         :salt1 salt1 :salt2 salt2
                                         :server-public server-public
                                         :secret-octets secret
                                         :iterations 16)
            (declare (ignore other-public))
            (ok (not (equalp expected other-proof)))))))))

(deftest srp-refuses-a-public-value-it-should-not-trust
  (dolist (bad (list 0 1 +srp-prime+ (1- +srp-prime+)))
    (signals (crypto:srp-check-password "x" :prime +srp-prime+
                                            :generator +srp-generator+
                                            :salt1 (counting-octets 8)
                                            :salt2 (counting-octets 8)
                                            :server-public bad
                                            :secret-octets (counting-octets 256)
                                            :iterations 1)
             'crypto:srp-error)))

;;;; Credentials and stored sessions

(deftest qr-login-uri-is-unpadded-url-safe-base64
  (let ((login (make-instance 'client:qr-login :connection nil :application nil
                                               :session-file "unused" :test nil)))
    (client::qr-login-token-result
     login (tl:make-tl :auth.login-token :expires 123
                                         :token (ascii "any carnal pleasure.")))
    (ok (= 123 (client:qr-login-expires login)))
    (ok (equal "tg://login?token=YW55IGNhcm5hbCBwbGVhc3VyZS4"
               (client:qr-login-uri login)))
    (setf (client:qr-login-token login) (hex "ff ef fe"))
    (ok (equal "tg://login?token=_-_-" (client:qr-login-uri login)))))

(deftest dotenv-files-are-read-the-way-people-write-them
  (let ((path (merge-pathnames "telegram-dotenv-test.env"
                               (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (with-open-file (stream path :direction :output
                                        :if-exists :supersede)
             (write-string "# a comment
export TELEGRAM_API_ID=12345
TELEGRAM_API_HASH=\"deadbeef\"
EMPTY=
QUOTED='single'
" stream))
           (let ((bindings (client:read-dotenv path)))
             (ok (equal "12345" (cdr (assoc "TELEGRAM_API_ID" bindings
                                            :test #'string=))))
             (ok (equal "deadbeef" (cdr (assoc "TELEGRAM_API_HASH" bindings
                                               :test #'string=))))
             (ok (equal "single" (cdr (assoc "QUOTED" bindings
                                             :test #'string=))))
             (ok (equal "" (cdr (assoc "EMPTY" bindings :test #'string=))))))
      (ignore-errors (delete-file path))))
  (testing "and a file that is not there is not an error"
    (ok (null (client:read-dotenv "/nonexistent/.env")))))

(deftest stored-sessions-round-trip
  (let ((path (merge-pathnames "telegram-session-test"
                               (uiop:temporary-directory)))
        (material (test-material :server-salt 4242 :time-offset -3)))
    (unwind-protect
         (let ((session (mt:make-mtproto-session material :session-id 99
                                                          :entropy (constant-entropy 0))))
           ;; SAVE-SESSION wants a connection; the parts it reads are these.
           (with-open-file (stream path :direction :output :if-exists :supersede)
             (prin1 (list :dc-id 2 :test nil
                          :auth-key (unhex (mt:auth-key-data
                                            (mt:session-key session)))
                          :server-salt (mt:session-server-salt session)
                          :time-offset (mt:session-time-offset session)
                          :session-id (mt:session-id session))
                    stream))
           (let* ((stored (client:load-session path))
                  (restored (client:stored-material stored)))
             (ok (= 2 (getf stored :dc-id)))
             (ok (equalp (mt:auth-key-id (mt:session-key session))
                         (mt:auth-key-id
                          (mt:auth-key-material-key restored))))
             (ok (= 4242 (mt:auth-key-material-server-salt restored)))
             (ok (= -3 (mt:auth-key-material-time-offset restored)))))
      (ignore-errors (delete-file path)))))

(deftest migration-errors-name-their-data-centre
  (ok (= 4 (client:migration-data-center "PHONE_MIGRATE_4")))
  (ok (= 2 (client:migration-data-center "USER_MIGRATE_2")))
  (ok (= 5 (client:migration-data-center "NETWORK_MIGRATE_5")))
  (ok (null (client:migration-data-center "AUTH_RESTART")))
  (ok (null (client:migration-data-center "FLOOD_WAIT_30"))))
