(defpackage #:mqtt.tests
  (:use #:cl #:rove)
  (:documentation
   "Executable claims for the MQTT core.  The wire vectors are the
specification's own examples where it gives them; the session claims are
about what goes out and what is reported when known packets come in."))

(in-package #:mqtt.tests)

(defun hex (string)
  "Bytes from a hexadecimal literal, whitespace ignored."
  (let* ((digits (remove-if (lambda (char) (member char '(#\Space #\Newline #\Tab)))
                            string))
         (octets (mqtt:make-octets (/ (length digits) 2))))
    (dotimes (index (length octets) octets)
      (setf (aref octets index)
            (parse-integer digits :start (* 2 index) :end (+ 2 (* 2 index))
                                  :radix 16)))))

(defun unhex (octets)
  "Bytes as a hexadecimal string, for readable failure reports."
  (format nil "~{~(~2,'0x~)~}" (coerce octets 'list)))

(defun encoded (packet)
  (unhex (mqtt:encode-packet packet)))

(defun decoded (hex-string)
  (mqtt:decode-packet (hex hex-string)))

(defun round-trip (packet)
  "PACKET decoded from its own encoding."
  (mqtt:decode-packet (mqtt:encode-packet packet)))
