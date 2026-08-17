;;;; SHA-1, SHA-256, SHA-512, and the constructions built on them.
;;;;
;;;; MTProto uses all three, and not interchangeably: SHA-1 fingerprints RSA
;;;; keys, derives the handshake's temporary AES key, and hashes the new
;;;; nonce; SHA-256 derives the per-message key and IV and does the SRP
;;;; hashing; SHA-512 exists only underneath PBKDF2, which is how a
;;;; two-factor password becomes an SRP secret.  All of them are here because
;;;; the alternative is a dependency for a few hundred lines of completely
;;;; specified arithmetic.
;;;;
;;;; The round constants are computed rather than transcribed.  FIPS 180-4
;;;; defines them as the leading fractional bits of the square and cube roots
;;;; of the small primes, and saying that in eight lines is both shorter and
;;;; more obviously right than a hundred and fifty copied hexadecimal words --
;;;; the same reason the AES S-box next door is derived from its definition.

(in-package #:telegram.crypto)

(deftype word32 () '(unsigned-byte 32))
(deftype word64 () '(unsigned-byte 64))

(declaim (inline mask32 rotate-left-32 rotate-right-32
                 mask64 rotate-right-64))

(defun mask32 (value)
  (ldb (byte 32 0) value))

(defun mask64 (value)
  (ldb (byte 64 0) value))

(defun rotate-left-32 (value count)
  (declare (type word32 value) (type (integer 0 31) count))
  (mask32 (logior (ash value count) (ash value (- count 32)))))

(defun rotate-right-32 (value count)
  (declare (type word32 value) (type (integer 0 31) count))
  (mask32 (logior (ash value (- count)) (ash value (- 32 count)))))

(defun rotate-right-64 (value count)
  (declare (type word64 value) (type (integer 0 63) count))
  (mask64 (logior (ash value (- count)) (ash value (- 64 count)))))

;;;; The constants, from their definition

(defun small-primes (count)
  "The first COUNT primes."
  (loop with primes = '()
        for candidate from 2
        while (< (length primes) count)
        do (when (loop for prime in primes
                       never (zerop (mod candidate prime)))
             (push candidate primes))
        finally (return (nreverse primes))))

(defun integer-root (value degree)
  "The floor of the DEGREE-th root of VALUE, by Newton's method on integers."
  (if (zerop value)
      0
      (loop with estimate = (ash 1 (ceiling (integer-length value) degree))
            for next = (floor (+ (* (1- degree) estimate)
                                 (floor value (expt estimate (1- degree))))
                              degree)
            while (< next estimate)
            do (setf estimate next)
            finally (return estimate))))

(defun fractional-root-bits (value degree bits)
  "The leading BITS bits of the fractional part of VALUE's DEGREE-th root.

Exactly, and without a float in sight: scaling under the root by 2^(degree *
bits) moves those bits above the point, where integer arithmetic can have
them."
  (mod (integer-root (* value (ash 1 (* degree bits))) degree) (ash 1 bits)))

(defun root-constants (count degree bits)
  (map 'vector (lambda (prime) (fractional-root-bits prime degree bits))
       (small-primes count)))

(defparameter +sha-256-initial-state+ (root-constants 8 2 32)
  "The square roots of the first eight primes.")

(defparameter +sha-256-constants+ (root-constants 64 3 32)
  "The cube roots of the first sixty-four primes.")

(defparameter +sha-512-initial-state+ (root-constants 8 2 64))

(defparameter +sha-512-constants+ (root-constants 80 3 64))

;;;; Padding and block access

(defun pad-for-sha (data start end &key (block-length 64) (length-bytes 8))
  "DATA between START and END, followed by the FIPS 180-4 padding: a set bit,
zeroes, and the big-endian message length in bits, filling out whole blocks."
  (let* ((length (- end start))
         (blocks (ceiling (+ length 1 length-bytes) block-length))
         (total (* block-length blocks))
         (result (octets:make-octets total))
         (bits (* 8 length)))
    (replace result data :start2 start :end2 end)
    (setf (aref result length) #x80)
    (dotimes (index length-bytes)
      (setf (aref result (- total 1 index)) (ldb (byte 8 (* 8 index)) bits)))
    result))

(declaim (inline block-word))
(defun block-word (block offset index size)
  "The big-endian SIZE-byte word at word INDEX of the block starting at OFFSET."
  (declare (type octets:octets block))
  (loop with base = (+ offset (* size index))
        with value = 0
        for byte from 0 below size
        do (setf value (logior (ash value 8) (aref block (+ base byte))))
        finally (return value)))

(defun digest-octets (words count size)
  "COUNT big-endian words of SIZE bytes each, as a byte vector."
  (let ((result (octets:make-octets (* size count))))
    (dotimes (index count result)
      (loop with word = (aref words index)
            for byte from 0 below size
            do (setf (aref result (+ (* size index) byte))
                     (ldb (byte 8 (* 8 (- size 1 byte))) word))))))

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
               (setf (aref schedule index) (block-word message offset index 4)))
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
    (digest-octets state 5 4)))

;;;; SHA-256

(defun sha-256 (data &key (start 0) (end (length data)))
  "The 32-byte SHA-256 digest of DATA between START and END."
  (let ((message (pad-for-sha (octets:to-octets data) start end))
        (state (make-array 8 :element-type 'word32
                             :initial-contents (coerce +sha-256-initial-state+
                                                       'list)))
        (schedule (make-array 64 :element-type 'word32 :initial-element 0)))
    (declare (type (simple-array word32 (8)) state)
             (type (simple-array word32 (64)) schedule))
    (loop for offset from 0 below (length message) by 64
          do (dotimes (index 16)
               (setf (aref schedule index) (block-word message offset index 4)))
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
    (digest-octets state 8 4)))

;;;; SHA-512
;;;;
;;;; The same shape as SHA-256 in sixty-four-bit words: twice the block, twice
;;;; the state, eighty rounds, and different rotation amounts.

(defun sha-512 (data &key (start 0) (end (length data)))
  "The 64-byte SHA-512 digest of DATA between START and END."
  (let ((message (pad-for-sha (octets:to-octets data) start end
                              :block-length 128 :length-bytes 16))
        (state (make-array 8 :element-type 'word64
                             :initial-contents (coerce +sha-512-initial-state+
                                                       'list)))
        (schedule (make-array 80 :element-type 'word64 :initial-element 0)))
    (declare (type (simple-array word64 (8)) state)
             (type (simple-array word64 (80)) schedule))
    (loop for offset from 0 below (length message) by 128
          do (dotimes (index 16)
               (setf (aref schedule index) (block-word message offset index 8)))
             (loop for index from 16 below 80
                   for previous = (aref schedule (- index 15))
                   for recent = (aref schedule (- index 2))
                   do (setf (aref schedule index)
                            (mask64
                             (+ (aref schedule (- index 16))
                                (logxor (rotate-right-64 previous 1)
                                        (rotate-right-64 previous 8)
                                        (ash previous -7))
                                (aref schedule (- index 7))
                                (logxor (rotate-right-64 recent 19)
                                        (rotate-right-64 recent 61)
                                        (ash recent -6))))))
             (let ((a (aref state 0)) (b (aref state 1)) (c (aref state 2))
                   (d (aref state 3)) (e (aref state 4)) (f (aref state 5))
                   (g (aref state 6)) (h (aref state 7)))
               (declare (type word64 a b c d e f g h))
               (dotimes (index 80)
                 (let* ((sigma1 (logxor (rotate-right-64 e 14)
                                        (rotate-right-64 e 18)
                                        (rotate-right-64 e 41)))
                        (choice (logxor (logand e f)
                                        (logand (mask64 (lognot e)) g)))
                        (temporary1 (mask64 (+ h sigma1 choice
                                               (aref +sha-512-constants+ index)
                                               (aref schedule index))))
                        (sigma0 (logxor (rotate-right-64 a 28)
                                        (rotate-right-64 a 34)
                                        (rotate-right-64 a 39)))
                        (majority (logxor (logand a b) (logand a c)
                                          (logand b c)))
                        (temporary2 (mask64 (+ sigma0 majority))))
                   (setf h g
                         g f
                         f e
                         e (mask64 (+ d temporary1))
                         d c
                         c b
                         b a
                         a (mask64 (+ temporary1 temporary2)))))
               (setf (aref state 0) (mask64 (+ (aref state 0) a))
                     (aref state 1) (mask64 (+ (aref state 1) b))
                     (aref state 2) (mask64 (+ (aref state 2) c))
                     (aref state 3) (mask64 (+ (aref state 3) d))
                     (aref state 4) (mask64 (+ (aref state 4) e))
                     (aref state 5) (mask64 (+ (aref state 5) f))
                     (aref state 6) (mask64 (+ (aref state 6) g))
                     (aref state 7) (mask64 (+ (aref state 7) h)))))
    (digest-octets state 8 8)))

;;;; HMAC and PBKDF2
;;;;
;;;; Both are defined over any hash, so both take one: a digest function and
;;;; the block length it works in.

(defun hmac (key message &key (digest #'sha-512) (block-length 128))
  "RFC 2104 HMAC of MESSAGE under KEY."
  (let ((key (octets:to-octets
              (if (> (length key) block-length) (funcall digest key) key))))
    (let ((inner (octets:make-octets block-length :initial-element #x36))
          (outer (octets:make-octets block-length :initial-element #x5C)))
      (dotimes (index (length key))
        (setf (aref inner index) (logxor (aref inner index) (aref key index))
              (aref outer index) (logxor (aref outer index) (aref key index))))
      (funcall digest
               (octets:concatenate-octets
                outer (funcall digest (octets:concatenate-octets inner
                                                                 message)))))))

(defun pbkdf2 (password salt iterations length
               &key (digest #'sha-512) (block-length 128))
  "RFC 2898 PBKDF2, which Telegram uses with HMAC-SHA-512 and a hundred
thousand iterations to turn a two-factor password into an SRP secret."
  (let ((output (octets:make-octets length))
        (written 0))
    (loop for block from 1
          while (< written length)
          do (let* ((seed (octets:concatenate-octets
                           salt (octets:integer-octets block :length 4)))
                    (previous (hmac password seed :digest digest
                                                  :block-length block-length))
                    (accumulator (copy-seq previous)))
               (loop repeat (1- iterations)
                     do (setf previous (hmac password previous :digest digest
                                                               :block-length
                                                               block-length))
                        (dotimes (index (length accumulator))
                          (setf (aref accumulator index)
                                (logxor (aref accumulator index)
                                        (aref previous index)))))
               (let ((take (min (length accumulator) (- length written))))
                 (replace output accumulator :start1 written :end2 take)
                 (incf written take))))
    output))
