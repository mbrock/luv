;;;; The D-Bus wire format: signatures, values, and messages.

(in-package #:luv.dbus)

(defstruct (variant (:constructor make-variant (signature value)))
  "A value sent with its own signature, as the V type carries one."
  signature
  value)

(define-condition dbus-error (error)
  ((name :initarg :name :reader dbus-error-name)
   (message :initarg :message :initform nil :reader dbus-error-message))
  (:report (lambda (condition stream)
             (format stream "~A~@[: ~A~]" (dbus-error-name condition)
                     (dbus-error-message condition)))))

;;; Signatures parse to trees: a basic type is its character; containers are
;;; (:ARRAY element), (:STRUCT . fields), and (:DICT-ENTRY key value).

(defun parse-signature (signature)
  "SIGNATURE, a string such as \"a{sv}\", as a list of type trees."
  (let ((index 0))
    (labels ((next ()
               (when (>= index (length signature))
                 (error "Signature ~S ends inside a container." signature))
               (let ((char (char signature index)))
                 (incf index)
                 (case char
                   (#\a (list :array (next)))
                   (#\( (cons :struct (loop until (char= (char signature index) #\))
                                            collect (next)
                                            finally (incf index))))
                   (#\{ (prog1 (list :dict-entry (next) (next))
                          (unless (char= (char signature index) #\})
                            (error "Malformed dictionary entry in ~S." signature))
                          (incf index)))
                   ((#\y #\b #\n #\q #\i #\u #\x #\t #\d #\s #\o #\g #\v #\h) char)
                   (t (error "Unknown type ~S in signature ~S." char signature))))))
      (loop while (< index (length signature)) collect (next)))))

(defun type-alignment (type)
  (if (consp type)
      (ecase (first type)
        (:array 4)
        ((:struct :dict-entry) 8))
      (ecase type
        ((#\y #\g #\v) 1)
        ((#\n #\q) 2)
        ((#\b #\i #\u #\s #\o #\h) 4)
        ((#\x #\t #\d) 8))))

;;; Marshalling into an adjustable octet vector, little-endian.  Alignment is
;;; relative to the start of the vector, which starts at an 8-aligned offset
;;; of the message: the header and the body each begin on one.

(defun make-octets ()
  (make-array 64 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))

(defun pad-to (octets alignment)
  (loop until (zerop (mod (fill-pointer octets) alignment))
        do (vector-push-extend 0 octets)))

(defun put-unsigned (octets value bytes)
  (pad-to octets bytes)
  (dotimes (i bytes)
    (vector-push-extend (ldb (byte 8 (* 8 i)) value) octets)))

(defun put-signed (octets value bytes)
  (put-unsigned octets (ldb (byte (* 8 bytes) 0) value) bytes))

(defun utf-8 (string)
  (sb-ext:string-to-octets string :external-format :utf-8))

(defun put-string-octets (octets bytes length-bytes)
  (if (= length-bytes 1)
      (vector-push-extend (length bytes) octets)
      (put-unsigned octets (length bytes) 4))
  (loop for byte across bytes do (vector-push-extend byte octets))
  (vector-push-extend 0 octets))

(defun octet-vector-p (value)
  (and (vectorp value) (not (stringp value))
       (every (lambda (x) (typep x '(unsigned-byte 8))) value)))

(defun guess-signature (value)
  (typecase value
    (variant (variant-signature value))
    (string "s")
    ((signed-byte 32) "i")
    (integer "x")
    (real "d")
    ((eql t) "b")
    (vector (if (octet-vector-p value) "ay" (error "Cannot guess a signature for ~S." value)))
    (t (error "Cannot guess a signature for ~S; wrap it with MAKE-VARIANT." value))))

(defun type-signature (type)
  (if (consp type)
      (ecase (first type)
        (:array (format nil "a~A" (type-signature (second type))))
        (:struct (format nil "(~{~A~})" (mapcar #'type-signature (rest type))))
        (:dict-entry (format nil "{~A~A}" (type-signature (second type))
                             (type-signature (third type)))))
      (string type)))

(defun put-value (octets type value)
  (if (consp type)
      (ecase (first type)
        (:array
         (let ((element (second type)))
           (pad-to octets 4)
           (let ((length-at (fill-pointer octets)))
             (put-unsigned octets 0 4)
             ;; The length excludes the padding before the first element.
             (pad-to octets (type-alignment element))
             (let ((start (fill-pointer octets)))
               (map nil (lambda (item) (put-value octets element item)) value)
               (let ((length (- (fill-pointer octets) start)))
                 (dotimes (i 4)
                   (setf (aref octets (+ length-at i)) (ldb (byte 8 (* 8 i)) length))))))))
        (:struct
         (pad-to octets 8)
         (loop for field in (rest type)
               for item in value
               do (put-value octets field item)))
        (:dict-entry
         (pad-to octets 8)
         (put-value octets (second type) (car value))
         (put-value octets (third type) (cdr value))))
      (ecase type
        (#\y (put-unsigned octets value 1))
        (#\b (put-unsigned octets (if value 1 0) 4))
        (#\n (put-signed octets value 2))
        (#\q (put-unsigned octets value 2))
        (#\i (put-signed octets value 4))
        ((#\u #\h) (put-unsigned octets value 4))
        (#\x (put-signed octets value 8))
        (#\t (put-unsigned octets value 8))
        (#\d (let ((value (coerce value 'double-float)))
               (put-unsigned octets (logior (ash (ldb (byte 32 0) (sb-kernel:double-float-high-bits value)) 32)
                                            (sb-kernel:double-float-low-bits value))
                             8)))
        ((#\s #\o) (pad-to octets 4) (put-string-octets octets (utf-8 value) 4))
        (#\g (put-string-octets octets (utf-8 value) 1))
        (#\v
         (let* ((signature (guess-signature value))
                (inner (if (variant-p value) (variant-value value) value))
                (types (parse-signature signature)))
           (unless (= 1 (length types))
             (error "A variant holds one complete type, not ~S." signature))
           (put-string-octets octets (utf-8 signature) 1)
           (put-value octets (first types) inner))))))

(defun marshal-values (signature values &optional (octets (make-octets)))
  "Append VALUES, of SIGNATURE, to OCTETS, an adjustable octet vector that
begins at an 8-aligned offset of its message."
  (loop for type in (parse-signature signature)
        for value in values
        do (put-value octets type value))
  octets)

;;; Unmarshalling from a simple octet vector.

(defstruct (reader (:constructor make-reader (octets &key (position 0) (big-endian-p nil))))
  octets
  position
  big-endian-p)

(defun skip-to (reader alignment)
  (setf (reader-position reader)
        (* alignment (ceiling (reader-position reader) alignment))))

(defun get-unsigned (reader bytes)
  (skip-to reader bytes)
  (let ((octets (reader-octets reader))
        (start (reader-position reader))
        (value 0))
    (when (> (+ start bytes) (length octets))
      (error "D-Bus message ends early."))
    (dotimes (i bytes)
      (setf value (dpb (aref octets (+ start i))
                       (byte 8 (* 8 (if (reader-big-endian-p reader) (- bytes i 1) i)))
                       value)))
    (incf (reader-position reader) bytes)
    value))

(defun get-signed (reader bytes)
  (let ((value (get-unsigned reader bytes))
        (bits (* 8 bytes)))
    (if (logbitp (1- bits) value) (- value (ash 1 bits)) value)))

(defun get-string (reader length-bytes)
  (let* ((length (if (= length-bytes 1)
                     (prog1 (aref (reader-octets reader) (reader-position reader))
                       (incf (reader-position reader)))
                     (get-unsigned reader 4)))
         (start (reader-position reader)))
    (setf (reader-position reader) (+ start length 1))
    (sb-ext:octets-to-string (reader-octets reader) :external-format :utf-8
                                                    :start start :end (+ start length))))

(defun get-double (reader)
  (let ((bits (get-unsigned reader 8)))
    (sb-kernel:make-double-float (let ((high (ldb (byte 32 32) bits)))
                                   (if (logbitp 31 high) (- high (ash 1 32)) high))
                                 (ldb (byte 32 0) bits))))

(defun get-value (reader type)
  (if (consp type)
      (ecase (first type)
        (:array
         (let* ((length (get-unsigned reader 4))
                (element (second type)))
           (skip-to reader (type-alignment element))
           (let ((end (+ (reader-position reader) length)))
             (if (eql element #\y)
                 (prog1 (subseq (reader-octets reader) (reader-position reader) end)
                   (setf (reader-position reader) end))
                 (loop while (< (reader-position reader) end)
                       collect (get-value reader element))))))
        (:struct
         (skip-to reader 8)
         (loop for field in (rest type) collect (get-value reader field)))
        (:dict-entry
         (skip-to reader 8)
         (cons (get-value reader (second type)) (get-value reader (third type)))))
      (ecase type
        (#\y (get-unsigned reader 1))
        (#\b (/= 0 (get-unsigned reader 4)))
        (#\n (get-signed reader 2))
        (#\q (get-unsigned reader 2))
        (#\i (get-signed reader 4))
        ((#\u #\h) (get-unsigned reader 4))
        (#\x (get-signed reader 8))
        (#\t (get-unsigned reader 8))
        (#\d (get-double reader))
        ((#\s #\o) (skip-to reader 4) (get-string reader 4))
        (#\g (get-string reader 1))
        (#\v (let ((types (parse-signature (get-string reader 1))))
               (get-value reader (first types)))))))

(defun unmarshal-values (signature octets &key (start 0) big-endian-p)
  "The values of SIGNATURE in OCTETS from START, an 8-aligned offset."
  (let ((reader (make-reader octets :position start :big-endian-p big-endian-p)))
    (loop for type in (parse-signature signature)
          collect (get-value reader type))))

;;; Messages.

(defparameter +message-types+ '(nil :method-call :method-return :error :signal))
(defparameter +header-fields+
  '((1 :path #\o) (2 :interface #\s) (3 :member #\s) (4 :error-name #\s)
    (5 :reply-serial #\u) (6 :destination #\s) (7 :sender #\s) (8 :signature #\g)
    (9 :unix-fds #\u)))

(defstruct message
  type
  (flags 0)
  (serial 0)
  path interface member error-name reply-serial destination sender
  (signature "")
  (body '()))

(defmethod print-object ((message message) stream)
  (print-unreadable-object (message stream :type t)
    (format stream "~(~A~) ~D~@[ ~A~]~@[ ~A~]~@[.~A~]~@[ reply to ~D~]~@[ ~A~]"
            (message-type message) (message-serial message)
            (message-path message) (message-interface message) (message-member message)
            (message-reply-serial message) (message-error-name message))))

(defun header-value (message key)
  (ecase key
    (:path (message-path message)) (:interface (message-interface message))
    (:member (message-member message)) (:error-name (message-error-name message))
    (:reply-serial (message-reply-serial message))
    (:destination (message-destination message)) (:sender (message-sender message))
    (:signature (let ((signature (message-signature message)))
                  (and (plusp (length signature)) signature)))
    (:unix-fds nil)))

(defun encode-message (message)
  "MESSAGE's bytes, little-endian, ready for the socket."
  (let ((body (marshal-values (message-signature message) (message-body message)))
        (octets (make-octets)))
    (vector-push-extend (char-code #\l) octets)
    (vector-push-extend (position (message-type message) +message-types+) octets)
    (vector-push-extend (message-flags message) octets)
    (vector-push-extend 1 octets)
    (put-unsigned octets (length body) 4)
    (put-unsigned octets (message-serial message) 4)
    (put-value octets '(:array (:struct #\y #\v))
               (loop for (code key type) in +header-fields+
                     for value = (header-value message key)
                     when value
                       collect (list code (make-variant (string type) value))))
    (pad-to octets 8)
    (loop for byte across body do (vector-push-extend byte octets))
    (coerce octets '(simple-array (unsigned-byte 8) (*)))))

(defun message-length (prefix)
  "The whole length of the message whose first 16 bytes are PREFIX."
  (let* ((reader (make-reader prefix :big-endian-p (= (aref prefix 0) (char-code #\B))))
         (body-length (progn (setf (reader-position reader) 4) (get-unsigned reader 4)))
         (fields-length (progn (setf (reader-position reader) 12) (get-unsigned reader 4))))
    (+ (* 8 (ceiling (+ 16 fields-length) 8)) body-length)))

(defun decode-message (octets)
  "The message in OCTETS, the whole of one message as read from the socket."
  (let* ((big-endian-p (= (aref octets 0) (char-code #\B)))
         (reader (make-reader octets :position 4 :big-endian-p big-endian-p))
         (body-length (get-unsigned reader 4))
         (message (make-message :type (nth (aref octets 1) +message-types+)
                                :flags (aref octets 2)
                                :serial (get-unsigned reader 4))))
    (declare (ignore body-length))
    (dolist (field (get-value reader '(:array (:struct #\y #\v))))
      (destructuring-bind (code value) field
        (let ((key (second (assoc code +header-fields+))))
          (case key
            (:path (setf (message-path message) value))
            (:interface (setf (message-interface message) value))
            (:member (setf (message-member message) value))
            (:error-name (setf (message-error-name message) value))
            (:reply-serial (setf (message-reply-serial message) value))
            (:destination (setf (message-destination message) value))
            (:sender (setf (message-sender message) value))
            (:signature (setf (message-signature message) value))))))
    (skip-to reader 8)
    (setf (message-body message)
          (unmarshal-values (message-signature message) octets
                            :start (reader-position reader)
                            :big-endian-p big-endian-p))
    message))
