;;;; The number theory MTProto needs.
;;;;
;;;; Two operations, both on Lisp bignums, which is the one place where a
;;;; Common Lisp client has less to write than a C++ one: modular
;;;; exponentiation for RSA and Diffie-Hellman, and the factorization of the
;;;; server's proof-of-work semiprime.

(in-package #:telegram.crypto)

(defun expt-mod (base exponent modulus)
  "BASE raised to EXPONENT modulo MODULUS, by right-to-left square and
multiply.  The 2048-bit Diffie-Hellman exponentiations in the handshake go
through here."
  (check-type exponent (integer 0))
  (check-type modulus (integer 1))
  (loop with result = (mod 1 modulus)
        with square = (mod base modulus)
        with remaining = exponent
        until (zerop remaining)
        do (when (logbitp 0 remaining)
             (setf result (mod (* result square) modulus)))
           (setf square (mod (* square square) modulus)
                 remaining (ash remaining -1))
        finally (return result)))

(defconstant +pollard-attempts+ 32
  "How many polynomials to try before admitting defeat.")

(defconstant +pollard-steps+ 250000
  "How far to walk one polynomial.  Telegram's pq is a product of two primes
of about thirty-one bits each, which Pollard's rho finds in well under this.")

(defun pollard-rho (value polynomial-offset)
  "One Pollard rho attempt on VALUE with x^2+POLYNOMIAL-OFFSET.  Returns a
nontrivial factor, or NIL if this polynomial does not find one."
  (flet ((step-value (x) (mod (+ (* x x) polynomial-offset) value)))
    (loop with slow = 2
          with fast = 2
          repeat +pollard-steps+
          do (setf slow (step-value slow)
                   fast (step-value (step-value fast)))
             (let ((divisor (gcd (abs (- slow fast)) value)))
               (cond ((= divisor 1))
                     ((= divisor value) (return nil))
                     (t (return divisor)))))))

(defun factor-pq (value)
  "Factor VALUE -- given as an integer or as the big-endian bytes the server
sends -- into its two prime factors, smaller first.

This is the client's half of MTProto's proof of work: the server picks a
semiprime and will not continue the handshake until we hand back its
factors."
  (let ((number (if (integerp value) value (octets:octets-integer value))))
    (assert (> number 3) (value) "~D is too small to be a Telegram pq." number)
    (when (evenp number)
      (return-from factor-pq (values 2 (floor number 2))))
    (let ((factor (loop for offset from 1 to +pollard-attempts+
                        thereis (pollard-rho number offset))))
      (unless factor
        (error "Could not factor ~D." number))
      (let ((other (floor number factor)))
        (if (<= factor other)
            (values factor other)
            (values other factor))))))
