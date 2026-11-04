(defpackage #:luvcraft.wire.tests
  (:use #:cl #:luvcraft.wire #:luvcraft.wire.net)
  (:import-from #:parachute #:define-test #:true #:false #:fail #:is #:group)
  (:documentation
   "Executable claims for the Luvcraft wire.  The codec claims need no
sockets; the endpoint claims run two endpoints in this image, over a
loopback network and over real TCP and UDP sockets."))

(in-package #:luvcraft.wire.tests)

(defun tag (octet)
  "A world tag of distinct octets starting at OCTET."
  (world-tag (coerce (loop for index below +world-tag-length+
                           collect (mod (+ octet (* 7 index)) 256))
                     'octets)))

(defun octets (&rest values) (coerce values 'octets))

(defun double-octets (value)
  "VALUE's eight big-endian octets as the codec writes them."
  (luvcraft.wire::with-wire-writer (writer)
    (luvcraft.wire::write-finite writer value)))

(defun patch (frame old new)
  "FRAME with the one occurrence of the octets OLD replaced by NEW."
  (let ((start (search old frame)))
    (assert start () "~S does not occur in the frame" old)
    (assert (not (search old frame :start2 (1+ start))) ()
            "~S occurs more than once in the frame" old)
    (let ((copy (copy-seq frame)))
      (replace copy new :start1 start)
      copy)))
