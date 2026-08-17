;;;; SHA-1 and SHA-256.
;;;;
;;;; MTProto uses both, and not interchangeably: SHA-1 fingerprints RSA keys,
;;;; derives the handshake's temporary AES key, and hashes the new nonce;
;;;; SHA-256 derives the per-message key and IV.  Both are here in full
;;;; because the alternative is a dependency for two hundred lines of
;;;; completely specified arithmetic.
;;;;
;;;; These are straightforward transcriptions of FIPS 180-4 with no
;;;; incremental interface: every digest in this protocol is taken over a
;;;; buffer that is already in hand, so the message is padded into one fresh
;;;; vector and consumed in a single pass.

(in-package #:telegram.crypto)

(deftype word32 () '(unsigned-byte 32))

(declaim (inline mask32 rotate-left-32 rotate-right-32))

(defun mask32 (value)
  (ldb (byte 32 0) value))

(defun rotate-left-32 (value count)
  (declare (type word32 value) (type (integer 0 31) count))
  (mask32 (logior (ash value count) (ash value (- count 32)))))

(defun rotate-right-32 (value count)
  (declare (type word32 value) (type (integer 0 31) count))
  (mask32 (logior (ash value (- count)) (ash value (- 32 count)))))

(defun pad-for-sha (data start end)
  "DATA between START and END, followed by the FIPS 180-4 padding: a set bit,
zeroes, and the big-endian message length in bits, filling out whole 64-byte
blocks."
  (let* ((length (- end start))
         (blocks (ceiling (+ length 9) 64))
         (total (* 64 blocks))
         (result (octets:make-octets total))
         (bits (* 8 length)))
    (replace result data :start2 start :end2 end)
    (setf (aref result length) #x80)
    (dotimes (index 8)
      (setf (aref result (- total 1 index)) (ldb (byte 8 (* 8 index)) bits)))
    result))

(declaim (inline block-word))
(defun block-word (block offset index)
  "The big-endian 32-bit word at word INDEX of the block starting at OFFSET."
  (declare (type octets:octets block))
  (let ((base (+ offset (* 4 index))))
    (logior (ash (aref block base) 24)
            (ash (aref block (+ base 1)) 16)
            (ash (aref block (+ base 2)) 8)
            (aref block (+ base 3)))))

(defun digest-octets (words count)
  "COUNT big-endian 32-bit WORDS as a byte vector."
  (let ((result (octets:make-octets (* 4 count))))
    (dotimes (index count result)
      (let ((word (aref words index)))
        (setf (aref result (* 4 index)) (ldb (byte 8 24) word)
              (aref result (+ (* 4 index) 1)) (ldb (byte 8 16) word)
              (aref result (+ (* 4 index) 2)) (ldb (byte 8 8) word)
              (aref result (+ (* 4 index) 3)) (ldb (byte 8 0) word))))))

;;;; SHA-1

(defun sha-1 (data &key (start 0) (end (length data)))
  "The 20-byte SHA-1 digest of DATA between START and END."
  (let ((message (pad-for-sha (octets:to-octets data) start end))
        (state (make-array 5 :element-type 'word32
                             :initial-contents '(#x67452301 #xEFCDAB89
                                                 #x98BADCFE #x10325476
                                                 #xC3D2E1F0)))
        (schedule (make-array 80 :element-type 'word32 :initial-element 0)))
    (declare (type (simple-array word32 (5)) state)
             (type (simple-array word32 (80)) schedule))
    (loop for offset from 0 below (length message) by 64
          do (dotimes (index 16)
               (setf (aref schedule index) (block-word message offset index)))
             (loop for index from 16 below 80
                   do (setf (aref schedule index)
                            (rotate-left-32
                             (logxor (aref schedule (- index 3))
                                     (aref schedule (- index 8))
                                     (aref schedule (- index 14))
                                     (aref schedule (- index 16)))
                             1)))
             (let ((a (aref state 0)) (b (aref state 1)) (c (aref state 2))
                   (d (aref state 3)) (e (aref state 4)))
               (declare (type word32 a b c d e))
               (dotimes (index 80)
                 (multiple-value-bind (f k)
                     (cond ((< index 20)
                            (values (logior (logand b c)
                                            (logand (mask32 (lognot b)) d))
                                    #x5A827999))
                           ((< index 40)
                            (values (logxor b c d) #x6ED9EBA1))
                           ((< index 60)
                            (values (logior (logand b c) (logand b d)
                                            (logand c d))
                                    #x8F1BBCDC))
                           (t (values (logxor b c d) #xCA62C1D6)))
                   (let ((temporary (mask32 (+ (rotate-left-32 a 5) f e k
                                               (aref schedule index)))))
                     (setf e d
                           d c
                           c (rotate-left-32 b 30)
                           b a
                           a temporary))))
               (setf (aref state 0) (mask32 (+ (aref state 0) a))
                     (aref state 1) (mask32 (+ (aref state 1) b))
                     (aref state 2) (mask32 (+ (aref state 2) c))
                     (aref state 3) (mask32 (+ (aref state 3) d))
                     (aref state 4) (mask32 (+ (aref state 4) e)))))
    (digest-octets state 5)))

;;;; SHA-256

(defparameter +sha-256-constants+
  (make-array
   64 :element-type 'word32
   :initial-contents
   '(#x428A2F98 #x71374491 #xB5C0FBCF #xE9B5DBA5 #x3956C25B #x59F111F1
     #x923F82A4 #xAB1C5ED5 #xD807AA98 #x12835B01 #x243185BE #x550C7DC3
     #x72BE5D74 #x80DEB1FE #x9BDC06A7 #xC19BF174 #xE49B69C1 #xEFBE4786
     #x0FC19DC6 #x240CA1CC #x2DE92C6F #x4A7484AA #x5CB0A9DC #x76F988DA
     #x983E5152 #xA831C66D #xB00327C8 #xBF597FC7 #xC6E00BF3 #xD5A79147
     #x06CA6351 #x14292967 #x27B70A85 #x2E1B2138 #x4D2C6DFC #x53380D13
     #x650A7354 #x766A0ABB #x81C2C92E #x92722C85 #xA2BFE8A1 #xA81A664B
     #xC24B8B70 #xC76C51A3 #xD192E819 #xD6990624 #xF40E3585 #x106AA070
     #x19A4C116 #x1E376C08 #x2748774C #x34B0BCB5 #x391C0CB3 #x4ED8AA4A
     #x5B9CCA4F #x682E6FF3 #x748F82EE #x78A5636F #x84C87814 #x8CC70208
     #x90BEFFFA #xA4506CEB #xBEF9A3F7 #xC67178F2))
  "The first thirty-two bits of the fractional parts of the cube roots of the
first sixty-four primes, per FIPS 180-4.")

(defun sha-256 (data &key (start 0) (end (length data)))
  "The 32-byte SHA-256 digest of DATA between START and END."
  (let ((message (pad-for-sha (octets:to-octets data) start end))
        (state (make-array 8 :element-type 'word32
                             :initial-contents '(#x6A09E667 #xBB67AE85
                                                 #x3C6EF372 #xA54FF53A
                                                 #x510E527F #x9B05688C
                                                 #x1F83D9AB #x5BE0CD19)))
        (schedule (make-array 64 :element-type 'word32 :initial-element 0)))
    (declare (type (simple-array word32 (8)) state)
             (type (simple-array word32 (64)) schedule))
    (loop for offset from 0 below (length message) by 64
          do (dotimes (index 16)
               (setf (aref schedule index) (block-word message offset index)))
             (loop for index from 16 below 64
                   for previous = (aref schedule (- index 15))
                   for recent = (aref schedule (- index 2))
                   do (setf (aref schedule index)
                            (mask32
                             (+ (aref schedule (- index 16))
                                (logxor (rotate-right-32 previous 7)
                                        (rotate-right-32 previous 18)
                                        (ash previous -3))
                                (aref schedule (- index 7))
                                (logxor (rotate-right-32 recent 17)
                                        (rotate-right-32 recent 19)
                                        (ash recent -10))))))
             (let ((a (aref state 0)) (b (aref state 1)) (c (aref state 2))
                   (d (aref state 3)) (e (aref state 4)) (f (aref state 5))
                   (g (aref state 6)) (h (aref state 7)))
               (declare (type word32 a b c d e f g h))
               (dotimes (index 64)
                 (let* ((sigma1 (logxor (rotate-right-32 e 6)
                                        (rotate-right-32 e 11)
                                        (rotate-right-32 e 25)))
                        (choice (logxor (logand e f)
                                        (logand (mask32 (lognot e)) g)))
                        (temporary1 (mask32 (+ h sigma1 choice
                                               (aref +sha-256-constants+ index)
                                               (aref schedule index))))
                        (sigma0 (logxor (rotate-right-32 a 2)
                                        (rotate-right-32 a 13)
                                        (rotate-right-32 a 22)))
                        (majority (logxor (logand a b) (logand a c)
                                          (logand b c)))
                        (temporary2 (mask32 (+ sigma0 majority))))
                   (setf h g
                         g f
                         f e
                         e (mask32 (+ d temporary1))
                         d c
                         c b
                         b a
                         a (mask32 (+ temporary1 temporary2)))))
               (setf (aref state 0) (mask32 (+ (aref state 0) a))
                     (aref state 1) (mask32 (+ (aref state 1) b))
                     (aref state 2) (mask32 (+ (aref state 2) c))
                     (aref state 3) (mask32 (+ (aref state 3) d))
                     (aref state 4) (mask32 (+ (aref state 4) e))
                     (aref state 5) (mask32 (+ (aref state 5) f))
                     (aref state 6) (mask32 (+ (aref state 6) g))
                     (aref state 7) (mask32 (+ (aref state 7) h)))))
    (digest-octets state 8)))
