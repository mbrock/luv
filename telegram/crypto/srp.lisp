;;;; Telegram's SRP, for two-factor passwords.
;;;;
;;;; When an account has a password, auth.signIn stops at
;;;; SESSION_PASSWORD_NEEDED and the client has to prove it knows the password
;;;; without sending it.  Telegram's scheme is SRP-6a over a 2048-bit group,
;;;; with the password first stretched by PBKDF2-HMAC-SHA-512 across a hundred
;;;; thousand iterations and salted on both sides of every hash.
;;;;
;;;; The double salting is the unusual part and the reason for SALTED-HASH
;;;; below: Telegram hashes salt|data|salt rather than the usual salt|data.
;;;; Everything else follows the specification at
;;;; core.telegram.org/api/srp, including its insistence that every number
;;;; going into a hash be padded to a full 256 bytes -- get that wrong and M1
;;;; is simply refused, with nothing to say why.

(in-package #:telegram.crypto)

(define-condition srp-error (crypto-error)
  ((detail :initarg :detail :reader srp-error-detail))
  (:report (lambda (condition stream)
             (format stream "SRP refused these parameters: ~A."
                     (srp-error-detail condition))))
  (:documentation
   "The server's SRP parameters did not survive checking.  Refusing to
continue is the point: a bad group is how a password gets stolen."))

(defconstant +srp-number-length+ 256
  "Every number in an SRP hash is padded to the length of the modulus.")

(defun srp-number (value)
  "VALUE as the 256 big-endian bytes SRP hashes it as."
  (octets:integer-octets value :length +srp-number-length+))

(defun salted-hash (data salt)
  "SHA-256 of SALT | DATA | SALT, which is how Telegram salts."
  (sha-256 (octets:concatenate-octets salt data salt)))

(defun srp-password-hash (password salt1 salt2 &key (iterations 100000))
  "The SRP secret x, derived from PASSWORD.

Two salted hashes, then PBKDF2 across a hundred thousand iterations of
HMAC-SHA-512, then a third salted hash.  This is the only slow part of a
login, and deliberately so."
  (let* ((password (if (stringp password) (octets:string-octets password)
                       (octets:to-octets password)))
         (first-hash (salted-hash (salted-hash password salt1) salt2))
         (stretched (pbkdf2 first-hash salt1 iterations 64)))
    (salted-hash stretched salt2)))

(defun miller-rabin-probably-prime-p (value &key (rounds 24))
  "Is VALUE probably prime?  Miller-Rabin with fixed small bases, which is
what a client can afford on a 2048-bit modulus."
  (cond ((< value 2) nil)
        ((< value 4) t)
        ((evenp value) nil)
        (t (let ((exponent (1- value))
                 (twos 0))
             (loop while (evenp exponent)
                   do (setf exponent (ash exponent -1))
                      (incf twos))
             (loop for base in (subseq (small-primes rounds) 0 rounds)
                   always (or (zerop (mod value base))
                              (let ((witness (expt-mod base exponent value)))
                                (or (= witness 1) (= witness (1- value))
                                    (loop repeat (1- twos)
                                          do (setf witness
                                                   (expt-mod witness 2 value))
                                          thereis (= witness (1- value)))))))))))

(defun check-srp-group (prime generator)
  "Check that the server's group is one a client may safely use.

The specification asks for a 2048-bit safe prime and one of four permitted
generators, with a quadratic-residue condition on each.  A client is supposed
to do this once and remember it; nothing here caches, because a login happens
seldom enough not to care."
  (unless (= 2048 (integer-length prime))
    (error 'srp-error :detail "the modulus is not 2048 bits"))
  (unless (member generator '(2 3 4 5 6 7))
    (error 'srp-error :detail "the generator is outside the permitted set"))
  (unless (miller-rabin-probably-prime-p prime)
    (error 'srp-error :detail "the modulus is not prime"))
  (unless (miller-rabin-probably-prime-p (floor (1- prime) 2))
    (error 'srp-error :detail "the modulus is not a safe prime"))
  ;; g must generate a large enough subgroup; for each permitted generator the
  ;; specification gives the congruence that guarantees it.
  (let ((condition (case generator
                     (2 (= 7 (mod prime 8)))
                     (3 (= 2 (mod prime 3)))
                     (4 t)
                     (5 (member (mod prime 5) '(1 4)))
                     (6 (member (mod prime 24) '(19 23)))
                     (7 (member (mod prime 7) '(3 5 6))))))
    (unless condition
      (error 'srp-error :detail "the generator does not fit the modulus")))
  t)

(defun check-srp-value (value prime what)
  "A public value has to sit strictly inside the group, and far enough from
its edges that a small exponent cannot be searched for."
  (let ((margin (ash 1 (- 2048 64))))
    (unless (and (< 1 value (1- prime)) (< margin value (- prime margin)))
      (error 'srp-error :detail (format nil "~A is outside its safe range"
                                        what)))
    t))

(defun srp-check-password (password &key prime generator salt1 salt2 server-public
                                         (secret-octets nil) (iterations 100000))
  "Prove knowledge of PASSWORD against the server's challenge.

Returns the client's public value A and the proof M1, as the byte strings
inputCheckPasswordSRP carries.  SECRET-OCTETS supplies the random exponent so
that the computation is reproducible in a test; otherwise pass fresh
randomness."
  (let* ((prime (if (integerp prime) prime (octets:octets-integer prime)))
         (salt1 (octets:to-octets salt1))
         (salt2 (octets:to-octets salt2))
         (server-public (if (integerp server-public)
                            server-public
                            (octets:octets-integer server-public)))
         (secret (octets:octets-integer secret-octets)))
    (check-srp-group prime generator)
    (check-srp-value server-public prime "the server's public value")
    (let* ((x (octets:octets-integer
               (srp-password-hash password salt1 salt2 :iterations iterations)))
           (client-public (expt-mod generator secret prime))
           (k (octets:octets-integer
               (sha-256 (octets:concatenate-octets (srp-number prime)
                                                   (srp-number generator)))))
           (v (expt-mod generator x prime))
           (u (octets:octets-integer
               (sha-256 (octets:concatenate-octets (srp-number client-public)
                                                   (srp-number server-public)))))
           (base (mod (- server-public (mod (* k v) prime)) prime))
           (shared (expt-mod base (+ secret (* u x)) prime))
           (session-key (sha-256 (srp-number shared))))
      (check-srp-value client-public prime "our public value")
      (when (zerop u)
        (error 'srp-error :detail "the scrambling parameter came out zero"))
      (values
       (srp-number client-public)
       (sha-256
        (octets:concatenate-octets
         (octets:octets-xor (sha-256 (srp-number prime))
                            (sha-256 (srp-number generator)))
         (sha-256 salt1)
         (sha-256 salt2)
         (srp-number client-public)
         (srp-number server-public)
         session-key))))))
