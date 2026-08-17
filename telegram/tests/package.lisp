(defpackage #:telegram.tests
  (:use #:cl #:rove)
  (:local-nicknames (#:octets #:telegram.octets)
                    (#:tl #:telegram.tl)
                    (#:crypto #:telegram.crypto)
                    (#:mt #:telegram)
                    (#:net #:telegram.net))
  (:documentation
   "Executable claims for the MTProto core.

Most of the interesting vectors are cross-implementation: the digests come
from FIPS 180-4, the block cipher from FIPS 197, and the whole authorization
handshake from a recorded exchange that two other implementations of this
protocol -- one in Elixir, one in C++ -- reproduce byte for byte.  Agreeing
with them is a much stronger claim than agreeing with ourselves."))

(in-package #:telegram.tests)

(defun hex (string)
  "Bytes from a hexadecimal literal, whitespace ignored."
  (octets:hex-octets string))

(defun unhex (sequence)
  "Bytes as a hexadecimal string, for readable failure reports."
  (octets:octets-hex sequence))

(defun counting-octets (length &optional (first 0))
  "LENGTH bytes counting up from FIRST, wrapping at 256.  Several published
vectors are taken over exactly this."
  (let ((result (octets:make-octets length)))
    (dotimes (index length result)
      (setf (aref result index) (mod (+ first index) 256)))))

(defun ascii (string)
  "STRING as bytes, for the odd `pong' or `abc' in a test."
  (octets:string-octets string))

(defun replaying (&rest sequences)
  "A deterministic entropy source that hands out SEQUENCES in order."
  (make-instance 'octets:replaying-entropy
                 :source (apply #'octets:concatenate-octets sequences)))

(defun constant-entropy (byte &optional (length 4096))
  "An entropy source of one repeated byte, for padding that must not vary."
  (make-instance 'octets:replaying-entropy
                 :source (octets:make-octets length :initial-element byte)))

(defun frozen (unix-seconds)
  "A clock stopped at UNIX-SECONDS."
  (make-instance 'octets:frozen-clock
                 :nanoseconds (* unix-seconds 1000000000)))
