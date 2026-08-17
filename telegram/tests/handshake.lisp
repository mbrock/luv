;;;; The recorded authorization-key exchange.
;;;;
;;;; These bytes are one complete handshake against Telegram's published
;;;; sample server key, captured with the client randomness that produced it.
;;;; Because nothing in the core reads a clock or a random device on its own,
;;;; feeding the recording back in has to reproduce every client message
;;;; exactly -- and it is the same recording two other implementations of this
;;;; protocol reproduce, so agreeing with it is a claim about the protocol
;;;; rather than about our own habits.

(in-package #:telegram.tests)

(defparameter +sample-nonce+
  (hex "4e44b426241e8b839153122d44585ac6")
  "The client nonce that opened the recorded exchange.")

(defparameter +sample-req-pq-multi+
  (hex "f18e7ebe4e44b426241e8b839153122d44585ac6")
  "The req_pq_multi it produces.")

(defparameter +sample-res-pq+
  (hex "632416054e44b426241e8b839153122d44585ac665ba0b393e1094329eda2c42
        d62833030819546f942a11278d00000015c4b51c0300000003268d20df9858b2
        029f4ba16d109296216be86c022bb4c3")
  "The server's resPQ.")

(defparameter +sample-random+
  (hex "b9dce68b05ef760fa7edfefeff45aaa8afbac11dc3d333bc3132fd16ab816d63
        ed93c5bef9d0452add8164a2d5df5804277ee5a06fd4523372707ddbd8106d03
        766d76fb8bec672bdcddcd225f7766b83663b32a0fda1055175c5582edd10430
        937666be4fd15510ba5f19aa645973b6e4e9270efac25b58741635fe84dd0af0
        7a4686f750bf34de1073f1e7fa24e9b01a76e537504bd52b8195e5b78c9af2ba
        a982454e1a99eeae0f35944089ad12726d2433a2c18c9698a725364f9c4e939c
        e4f1aee3891e58b85de90c88cc2eaef5db1841a594c0edc13cb4b7480a7e564f
        e892f82282d03ed07eb5ceac6644247bb137241166fe194756dfcffd68c6c345")
  "The client randomness the first step consumed: 32 bytes of new_nonce followed by 224 bytes of RSA padding.")

(defparameter +sample-req-dh-params+
  (hex "bee412d74e44b426241e8b839153122d44585ac665ba0b393e1094329eda2c42
        d62833030444b2e50d000000045e63ac8100000003268d20df9858b2fe000100
        7ec37ca8a84aa1b26d21bc8ac28b261ffa57b44e29f0d6722261e9b436059cc8
        0ae9768a3ae4fbefe46cfbb76b88a1f80a1ebd95ae5d17bf655ed1015755e04c
        483a01cf4094a0830864054a71a0ac8a5ec34d6b24a69bf66c9654b32a8c65b0
        302718351b28f72a9a49610d5259b6edb6da37acc5fedc47d1a09c58df2c7ecc
        bfaf54dfe123ebc253d9069f74e8be128051e5d280b3c9a5e8d3c6da344cb737
        4a6d410d4e088cc0eda3d8b1108ba4f4a85d79fbd2758000723780bc5459f59f
        d1cea1b511b77cc1411781d3feb57b14a97726cf3d2146cf43e648a69ff9cb5d
        48a31f543bd5bc3a023cf382d86d36bbfbbcb5e4a136acee25fd8e3e597e714d")
  "The req_DH_params it must produce.")

(defparameter +sample-server-dh-params+
  (hex "5c07e8d04e44b426241e8b839153122d44585ac665ba0b393e1094329eda2c42
        d6283303fe500200fd064e91012ade621b26a48ac7dc8b2c8670ed67092a00fe
        8c936483e4b02822c3cc655aaffe00542e311df5abdaa645b1da85ca50a6c7b0
        e7cc7cb2b23d42c84e288bb3b5cfe313e1ebafe19833916df4d1f58dba62e0ac
        49cac17a31b8b0d57d43eefda546d67e80e311c4b213adec9635c73f75a18ffb
        26fb71391523bd5ddfcc8be51b36d6b2552394c511ec935d53811a981baca62a
        2b58cbfe96f1b35e118e5e17456994aea931839925c4578f281f3f129d28026e
        c80224617a9ca8c615a12fba9c53e774476567f07b01a59d2e6635e39c16dc0a
        54679f3b54b0482f1cbeac821147d93d7365f4e23fb5794eb5fd4ffdc6456638
        ea32f641f49ee705e7b0da71cb75753e2f4f80d5af07edb017948f332e34a9c5
        886b0c86281e0e7228d5a652a9faaf819f7686c099186169aaa377c136fac57b
        69b7f7b383aaece652f8dcb14e0dfb23e2a65330307a74c31c508cc504450fa2
        08eee14d8bbead1c1f90ccfc183ae1d3345c62424ea3477776204e8fe69efbb6
        a27b168913d3babaca30aa1c9589d6655b2ad4cd59f67e9b3957ab3270d70afa
        b9bd488a6c5f39ca739ca8947def00cdb8812152731710f5108235775a019d3b
        4986d6b720b05167b4ee731a10a29fc1e03c42e99d8ff5cf64f45070c2f5ce48
        5ea5fddc281728b6e4d0dea561c9097e3f8a54b055b0c069a9f8207520f6429e
        b5225c985e3379f2cf6754f56d414fcd00d502e69223b911b915978e0890a9ef
        128715b828bf3fda3fee6c7b9b2621d971a6f7820f89f4c4c2ab29dec00007c3
        ec6cead64f7f5802d5e6a4a16a185cfbfced5351fa68380e")
  "The server's server_DH_params_ok.")

(defparameter +sample-server-random+
  (hex "8fc3605a4604cbb5461fdeff439c761150083cdd502550558e92c730d46c9caf
        0b1b2d64d2c264942c50d98694fff604fdd2bd87f2cafb719bc55e65a1f60b08
        809660a650721c40d56fc9c792df1d463aad1718c6924b7bdffbe395f14633d3
        3fc38ce47c18a1561b83a5c66d29f9e292637127471c3baab0028ae42796b689
        e53a7f9ab5f0ee6d3fb658d847c1abca509fc4ed0d45edbb1c946488910d8d78
        fa0767255b57a7c3898da8d26625bde40c5a0e80b581408ecd95a17d396dc757
        4a8ed3cbc4c085197ffaad29c18e577eb292aa8b98caa92efd6f9536049b5a7d
        efc861e270eca90c55b9585405cb96f3e6ea754850b09e7a59ba5fd92d357982
        915d39752aaa2ec16b6cbde6a6c33971")
  "The client randomness the second step consumed: a 256-byte secret exponent followed by IGE padding.")

(defparameter +sample-set-client-dh-params+
  (hex "1f5f04f54e44b426241e8b839153122d44585ac665ba0b393e1094329eda2c42
        d6283303fe500100def448d48c608480bab65df3f8990be8011f7b415a6f8113
        617bea749b8b0ea6a937987b18cc4dcce8197efdcf8d6ec6af7fc3364b4945df
        77e4a1ae9db7acea4abcd73247edb36bde20fc969c1d55717277afe0bc31a9ee
        99f7d822f91fa2dc69c868a19511b162d55e0814d0292b7708b67d57eb045693
        49d5a20ffe85c0141fc17e9bbbaf207bef56e66decda718c52c45273f868c2ef
        f89bb06355cd515fbfe123d719b244234867d2889c9d0e4436ba644076e5014a
        78af60b2f0e1b30285f4f71539bcf8c506ccafd62cfcd1b040fe5e35bb30e519
        ad56d753100f604e3ea5d02409d74dd3ab0861227410f1e13591cf2a638347e6
        c6d0bcae14e0e8753313b51daee40a67407b5cc8b213856a290a0c7b6cda9ff9
        c58d69faaf6a748cff05512b69f1380f7a36843edecdc764048bc16d9808f353
        a9caf6d49ca8b717c8f6de037518a444931a7da2b80f16d0")
  "The set_client_DH_params it must produce.")

(defparameter +sample-dh-gen-ok+
  (hex "34f7cb3b4e44b426241e8b839153122d44585ac665ba0b393e1094329eda2c42
        d628330313b781a0de4ab6bc7ab414cbe13f9f86")
  "The server's dh_gen_ok.")

(defparameter +sample-auth-key+
  (hex "7582e48ad36cd6eef7944ac9bd7027de9ee3202543b68850ac01e1221350f717
        4e6c3771c9d86b3075f777539c23d053e9da9a1510d49e8fa0ad76a016ce28bf
        e3543dde69959bc682dab762b95a36629a8438e65baa53cc79b551c23d555c76
        75a36f4ece90882ece497d28a903409b780a8a80516cb0f8534fee3a67530beb
        2b1929626e07c2a052c4870b18b0a626606ca05cb13668a65aee3fa32cbebf1b
        3a56532138cb22c017cac44a292021902eea9b9f906c6be19c9203c7bb3ebc5f
        1b2044d0a90cb008f7248c3ae4449e0895b6090abb04c24131c2948bd27d879e
        cb934e50a46671f987653385ab388e4fa1ddd4c95743111e08bf11fef1f8f739")
  "The 2048-bit authorization key the whole exchange arrives at.")

(defconstant +sample-handshake-time+ 1693436740
  "The instant the recording was made.  The server reported the same second,
so the exchange must derive a time offset of zero.")

(defun sample-exchange ()
  "An exchange primed with exactly the inputs the recording was made under."
  (mt:make-auth-exchange
   :public-keys (list (crypto:make-public-key +sample-modulus+ 65537
                                              :mode :padded))
   :entropy (replaying +sample-random+ +sample-server-random+)
   :clock (frozen +sample-handshake-time+)
   ;; The recording predates p_q_inner_data_dc.
   :dc-in-inner-data nil))

(deftest recorded-handshake-reproduces-byte-for-byte
  (let ((exchange (sample-exchange)))
    (testing "the opening request"
      (ok (equalp +sample-req-pq-multi+
                  (mt:begin-auth-exchange exchange :nonce +sample-nonce+)))
      (ok (eq :awaiting-res-pq (mt:auth-exchange-phase exchange))))
    (testing "resPQ is answered with the proof of work under RSA"
      (ok (equalp +sample-req-dh-params+
                  (mt:advance-auth-exchange exchange +sample-res-pq+)))
      (ok (eq :awaiting-server-dh-params (mt:auth-exchange-phase exchange)))
      (ok (equalp (subseq +sample-random+ 0 32)
                  (mt:auth-exchange-new-nonce exchange))))
    (testing "the Diffie-Hellman parameters yield our public value and the key"
      (ok (equalp +sample-set-client-dh-params+
                  (mt:advance-auth-exchange exchange +sample-server-dh-params+)))
      (ok (eq :awaiting-dh-gen (mt:auth-exchange-phase exchange)))
      (ok (= 4459407212920268508 (mt:auth-exchange-server-salt exchange)))
      (ok (= 0 (mt:auth-exchange-time-offset exchange))))
    (testing "and dh_gen_ok completes it"
      (ok (null (mt:advance-auth-exchange exchange +sample-dh-gen-ok+)))
      (ok (mt:auth-exchange-complete-p exchange))
      (ok (equalp +sample-auth-key+
                  (mt:auth-key-data (mt:auth-exchange-key exchange)))))
    (testing "leaving material a session can be built on"
      (let ((material (mt:auth-exchange-result exchange)))
        (ok (= 4459407212920268508 (mt:auth-key-material-server-salt material)))
        (ok (= 0 (mt:auth-key-material-time-offset material)))))))

(deftest a-tampered-recording-is-rejected
  (testing "a resPQ echoing the wrong nonce is not our exchange"
    (let ((exchange (sample-exchange))
          (response (copy-seq +sample-res-pq+)))
      (mt:begin-auth-exchange exchange :nonce +sample-nonce+)
      (setf (aref response 4) (logxor 1 (aref response 4)))
      (signals (mt:advance-auth-exchange exchange response) 'mt:nonce-mismatch)))
  (testing "and a dh_gen_ok whose hash does not match the key we derived"
    (let ((exchange (sample-exchange))
          (answer (copy-seq +sample-dh-gen-ok+)))
      (mt:begin-auth-exchange exchange :nonce +sample-nonce+)
      (mt:advance-auth-exchange exchange +sample-res-pq+)
      (mt:advance-auth-exchange exchange +sample-server-dh-params+)
      (setf (aref answer 40) (logxor 1 (aref answer 40)))
      (signals (mt:advance-auth-exchange exchange answer) 'mt:nonce-mismatch)))
  (testing "and a response arriving in the wrong phase"
    (let ((exchange (sample-exchange)))
      (mt:begin-auth-exchange exchange :nonce +sample-nonce+)
      (signals (mt:advance-auth-exchange exchange +sample-dh-gen-ok+)
               'mt:mtproto-protocol-error))))

;;;; Key derivation
;;;;
;;;; Taken over an authorization key that is simply the bytes 0 through 255,
;;;; which is what makes them comparable across implementations.

(deftest authorization-key-metadata
  (let ((key (mt:make-auth-key (counting-octets 256))))
    (ok (equal "4916d6bdb7f78e68" (unhex (mt:auth-key-aux-hash key))))
    (ok (equal "32d1586ea457dfc8" (unhex (mt:auth-key-id key))))
    (testing "the new_nonce_hash the server proves itself with"
      (ok (equal "c2ced2b33e593a55d27f4a5dabee7c67"
                 (unhex (mt:new-nonce-hash key (counting-octets 32) 1)))))
    (testing "the per-message key"
      (ok (equal "fbfa5fa94e2a70f3ad96dd24f7ad36b5"
                 (unhex (mt:message-key key (counting-octets 32 16) :client)))))
    (testing "and the AES key and IV derived from it"
      (multiple-value-bind (aes-key aes-iv)
          (mt:derive-aes-key-iv key (mt:message-key key (counting-octets 32 16)
                                                    :client)
                                :client)
        (ok (equal (concatenate 'string
                                "36bce969c89677d9cadd87de515f83a5"
                                "265d2f17274cb43de11122996338e5f2")
                   (unhex aes-key)))
        (ok (equal (concatenate 'string
                                "4808d5e2c25ecf23d82aab1b1aae0376"
                                "e073e9f175db200548fb5432d4980271")
                   (unhex aes-iv)))))
    (testing "and the two directions never derive the same key"
      (ok (not (equalp (mt:message-key key (counting-octets 32 16) :client)
                       (mt:message-key key (counting-octets 32 16) :server)))))))

(deftest padded-envelopes-round-trip
  (let ((key (mt:make-auth-key (counting-octets 256)))
        (plaintext (counting-octets 32 16)))
    (let ((sealed (mt:encrypt-padded plaintext key :client)))
      (ok (equalp (mt:auth-key-id key) (subseq sealed 0 8)))
      (ok (equalp plaintext (mt:decrypt-padded sealed key :client)))
      (testing "and opening one as the wrong direction fails its hash"
        (signals (mt:decrypt-padded sealed key :server)
                 'mt:message-key-mismatch))
      (testing "and a flipped bit is caught"
        (let ((tampered (copy-seq sealed)))
          (setf (aref tampered 30) (logxor 1 (aref tampered 30)))
          (signals (mt:decrypt-padded tampered key :client)
                   'mt:message-key-mismatch))))))
