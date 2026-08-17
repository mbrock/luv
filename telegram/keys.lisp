;;;; The RSA public keys Telegram's servers identify themselves with.
;;;;
;;;; A resPQ names the keys the server will accept by fingerprint, and the
;;;; client must already hold one of them; there is no in-band way to learn a
;;;; key.  So they are bundled, as PEM text, in the source.

(in-package #:telegram)

(defparameter +telegram-production-key-pem+
  "-----BEGIN RSA PUBLIC KEY-----
MIIBCgKCAQEA6LszBcC1LGzyr992NzE0ieY+BSaOW622Aa9Bd4ZHLl+TuFQ4lo4g
5nKaMBwK/BIb9xUfg0Q29/2mgIR6Zr9krM7HjuIcCzFvDtr+L0GQjae9H0pRB2OO
62cECs5HKhT5DZ98K33vmWiLowc621dQuwKWSQKjWf50XYFw42h21P2KXUGyp2y/
+aEyZ+uVgLLQbRA1dEjSDZ2iGRy12Mk5gpYc397aYp438fsJoHIgJ2lgMv5h7WY9
t6N/byY9Nw9p21Og3AoXSL2q/2IJ1WRUhebgAdGVMlV1fkuOQoEzR7EdpqtQD9Cs
5+bfo3Nhmcyvk5ftB0WkJ9z6bNZ7yxrP8wIDAQAB
-----END RSA PUBLIC KEY-----
"
  "The public key current Telegram production servers offer.  Used with
RSA_PAD, the padded scheme.")

(defparameter +telegram-sample-key-pem+
  "-----BEGIN RSA PUBLIC KEY-----
MIIBCgKCAQEAyMEdY1aR+sCR3ZSJrtztKTKqigvO/vBfqACJLZtS7QMgCGXJ6XIR
yy7mx66W0/sOFa7/1mAZtEoIokDP3ShoqF4fVNb6XeqgQfaUHd8wJpDWHcR2OFwv
plUUI1PLTktZ9uW2WE23b+ixNwJjJGwBDJPQEQFBE+vfmH0JP503wr5INS1poWg/
j25sIWeYPHYeOrFp/eXaqhISP6G+q2IeTaWTXpwZj4LzXq5YOpk4bYEQ6mvRq7D1
aHWfYmlEGepfaYR8Q0YqvvhYtMte3ITnuSJs171+GDqpdKcSwHnd6FudwGO4pcCO
j4WcDuXc2CTHgH8gFTNhp/Y8/SpDOhvn9QIDAQAB
-----END RSA PUBLIC KEY-----
"
  "The key used throughout Telegram's published sample handshake -- the one
the recorded exchange in the test suite runs against -- and, as it happens,
the one Telegram's test data centres still offer today.")

(defun public-keys-from-pem-text (pem &key (mode :padded))
  "Parse PEM into public keys.  A thin forwarder so callers in this package
need not reach into the crypto package for one function."
  (crypto:public-keys-from-pem pem :mode mode))

(defvar *bundled-keys* nil)

(defun telegram-public-keys ()
  "Every bundled key, parsed once.

Holding more than one costs nothing: a resPQ names the keys its server will
accept and the client picks whichever of them it has, so bundling the
production and test keys together is what lets one client reach both."
  (or *bundled-keys*
      (setf *bundled-keys*
            (append (public-keys-from-pem-text +telegram-production-key-pem+)
                    (public-keys-from-pem-text +telegram-sample-key-pem+)))))
