;;;; DEFLATE decompression.
;;;;
;;;; Telegram wraps any answer it thinks is worth compressing in
;;;; gzip_packed, so a client that cannot inflate cannot read a contact list.
;;;; This is RFC 1951 with the RFC 1950 and RFC 1952 wrappers sniffed off the
;;;; front -- decompression only, which is the half a client needs and about a
;;;; fifth of the work.
;;;;
;;;; The canonical-Huffman decoder is the one from zlib's `puff': keep a count
;;;; of codes per bit length and the symbols in canonical order, then walk one
;;;; bit at a time comparing against the first code of each length.  No
;;;; lookup tables to build, and the whole decoder is a dozen lines.

(in-package #:telegram.octets)

(define-condition inflate-error (error)
  ((detail :initarg :detail :reader inflate-error-detail))
  (:report (lambda (condition stream)
             (format stream "Malformed compressed data: ~A."
                     (inflate-error-detail condition))))
  (:documentation "The compressed stream was not valid DEFLATE."))

(defstruct (bit-reader (:constructor make-bit-reader (octets position)))
  "A cursor that reads bits least-significant first within each byte, which
is the order DEFLATE packs them."
  (octets (make-octets 0) :type octets)
  (position 0 :type fixnum)
  (bits 0 :type (unsigned-byte 32))
  (count 0 :type fixnum))

(defun read-bits (reader count)
  "COUNT bits, least significant first."
  (loop while (< (bit-reader-count reader) count)
        do (when (>= (bit-reader-position reader)
                     (length (bit-reader-octets reader)))
             (error 'inflate-error :detail "ran out of input"))
           (setf (bit-reader-bits reader)
                 (logior (bit-reader-bits reader)
                         (ash (aref (bit-reader-octets reader)
                                    (bit-reader-position reader))
                              (bit-reader-count reader))))
           (incf (bit-reader-position reader))
           (incf (bit-reader-count reader) 8))
  (prog1 (ldb (byte count 0) (bit-reader-bits reader))
    (setf (bit-reader-bits reader) (ash (bit-reader-bits reader) (- count)))
    (decf (bit-reader-count reader) count)))

(defun align-to-byte (reader)
  (setf (bit-reader-bits reader) 0
        (bit-reader-count reader) 0))

(defstruct (huffman (:constructor %make-huffman (counts symbols)))
  "A canonical Huffman code: how many codes there are of each bit length, and
the symbols in canonical order."
  (counts #() :type simple-vector)
  (symbols #() :type simple-vector))

(defconstant +maximum-code-length+ 15)

(defun make-huffman (lengths)
  "The canonical code described by LENGTHS, a symbol-indexed vector of code
lengths in bits, where zero means the symbol is unused."
  (let ((counts (make-array (1+ +maximum-code-length+) :initial-element 0))
        (offsets (make-array (1+ +maximum-code-length+) :initial-element 0))
        (symbols (make-array (count-if #'plusp lengths))))
    (map nil (lambda (length) (incf (aref counts length))) lengths)
    (setf (aref counts 0) 0)
    (loop for length from 1 to +maximum-code-length+
          do (setf (aref offsets length)
                   (+ (aref offsets (1- length)) (aref counts (1- length)))))
    (loop for symbol from 0 below (length lengths)
          for length = (aref lengths symbol)
          unless (zerop length)
            do (setf (aref symbols (aref offsets length)) symbol)
               (incf (aref offsets length)))
    (%make-huffman counts symbols)))

(defun decode-symbol (reader code)
  "One symbol from READER under CODE."
  (loop with value = 0
        with first = 0
        with index = 0
        for length from 1 to +maximum-code-length+
        do (setf value (logior value (read-bits reader 1)))
           (let ((count (aref (huffman-counts code) length)))
             (when (< (- value first) count)
               (return (aref (huffman-symbols code) (+ index (- value first)))))
             (incf index count)
             (setf first (ash (+ first count) 1)
                   value (ash value 1)))
        finally (error 'inflate-error :detail "invalid Huffman code")))

(defparameter +length-bases+
  #(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59 67 83 99 115 131 163
    195 227 258))

(defparameter +length-extra-bits+
  #(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0))

(defparameter +distance-bases+
  #(1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769 1025 1537 2049
    3073 4097 6145 8193 12289 16385 24577))

(defparameter +distance-extra-bits+
  #(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13))

(defparameter +code-length-order+
  #(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15)
  "The order the code-length code's own lengths arrive in, chosen so that the
rarely used ones fall at the end and can be omitted.")

(defparameter +fixed-literal-code+
  (make-huffman (let ((lengths (make-array 288)))
                  (loop for symbol from 0 below 288
                        do (setf (aref lengths symbol)
                                 (cond ((< symbol 144) 8)
                                       ((< symbol 256) 9)
                                       ((< symbol 280) 7)
                                       (t 8))))
                  lengths))
  "The literal/length code every fixed-Huffman block uses.")

(defparameter +fixed-distance-code+
  (make-huffman (make-array 30 :initial-element 5)))

(defun inflate-block (reader output literal-code distance-code)
  "One compressed block's worth of symbols, appended to OUTPUT."
  (loop for symbol = (decode-symbol reader literal-code)
        until (= symbol 256)
        do (cond
             ((< symbol 256) (vector-push-extend symbol output))
             (t
              (let ((index (- symbol 257)))
                (unless (< index (length +length-bases+))
                  (error 'inflate-error :detail "invalid length code"))
                (let* ((length (+ (aref +length-bases+ index)
                                  (read-bits reader
                                             (aref +length-extra-bits+ index))))
                       (code (decode-symbol reader distance-code)))
                  (unless (< code (length +distance-bases+))
                    (error 'inflate-error :detail "invalid distance code"))
                  (let ((distance (+ (aref +distance-bases+ code)
                                     (read-bits reader
                                                (aref +distance-extra-bits+
                                                      code)))))
                    (when (> distance (fill-pointer output))
                      (error 'inflate-error :detail "distance before output"))
                    ;; Copied byte by byte on purpose: a run may overlap
                    ;; itself, which is how DEFLATE spells a repeat.
                    (loop with start = (- (fill-pointer output) distance)
                          for offset from 0 below length
                          do (vector-push-extend (aref output (+ start offset))
                                                 output)))))))))

(defun read-dynamic-codes (reader)
  "The literal/length and distance codes a dynamic block carries in its own
header."
  (let* ((literal-count (+ 257 (read-bits reader 5)))
         (distance-count (+ 1 (read-bits reader 5)))
         (code-length-count (+ 4 (read-bits reader 4)))
         (code-lengths (make-array 19 :initial-element 0)))
    (dotimes (index code-length-count)
      (setf (aref code-lengths (aref +code-length-order+ index))
            (read-bits reader 3)))
    (let ((code-length-code (make-huffman code-lengths))
          (lengths (make-array (+ literal-count distance-count)
                               :initial-element 0))
          (index 0))
      (loop while (< index (length lengths))
            for symbol = (decode-symbol reader code-length-code)
            do (cond
                 ((< symbol 16) (setf (aref lengths index) symbol) (incf index))
                 ((= symbol 16)
                  (when (zerop index)
                    (error 'inflate-error :detail "no length to repeat"))
                  (let ((previous (aref lengths (1- index))))
                    (dotimes (repeat (+ 3 (read-bits reader 2)))
                      (setf (aref lengths index) previous)
                      (incf index))))
                 ((= symbol 17)
                  (incf index (+ 3 (read-bits reader 3))))
                 (t (incf index (+ 11 (read-bits reader 7))))))
      (when (> index (length lengths))
        (error 'inflate-error :detail "code lengths overran their table"))
      (values (make-huffman (subseq lengths 0 literal-count))
              (make-huffman (subseq lengths literal-count))))))

(defun inflate (data &key (start 0))
  "Decompress raw DEFLATE DATA."
  (let ((reader (make-bit-reader (to-octets data) start))
        (output (make-array 4096 :element-type '(unsigned-byte 8)
                                 :adjustable t :fill-pointer 0)))
    (loop for final = (read-bits reader 1)
          for kind = (read-bits reader 2)
          do (ecase kind
               (0 (align-to-byte reader)
                  (let* ((octets (bit-reader-octets reader))
                         (position (bit-reader-position reader))
                         (length (octets-integer octets :start position
                                                        :end (+ position 2)
                                                        :endian :little)))
                    (setf (bit-reader-position reader) (+ position 4))
                    (loop for index from 0 below length
                          do (vector-push-extend
                              (aref octets (+ (bit-reader-position reader)
                                              index))
                              output))
                    (incf (bit-reader-position reader) length)))
               (1 (inflate-block reader output +fixed-literal-code+
                                 +fixed-distance-code+))
               (2 (multiple-value-bind (literal-code distance-code)
                      (read-dynamic-codes reader)
                    (inflate-block reader output literal-code distance-code))))
          until (= 1 final))
    (to-octets output)))

(defun decompress (data)
  "Decompress DATA, recognizing the gzip and zlib wrappers as well as bare
DEFLATE.  Telegram sends gzip; other MTProto implementations have been known
to send zlib, and sniffing costs two bytes of comparison."
  (let ((data (to-octets data)))
    (when (< (length data) 2)
      (error 'inflate-error :detail "too short to be compressed data"))
    (cond ((and (= #x1F (aref data 0)) (= #x8B (aref data 1)))
           (inflate data :start (gzip-payload-start data)))
          ;; A zlib header is a byte whose low nibble is 8 followed by a check
          ;; byte making the pair a multiple of 31.
          ((and (= 8 (logand #x0F (aref data 0)))
                (zerop (mod (+ (* 256 (aref data 0)) (aref data 1)) 31)))
           (inflate data :start 2))
          (t (inflate data)))))

(defun gzip-payload-start (data)
  "Where the DEFLATE stream begins inside a gzip member."
  (unless (>= (length data) 10)
    (error 'inflate-error :detail "truncated gzip header"))
  (let ((flags (aref data 3))
        (position 10))
    (when (logbitp 2 flags)                 ; FEXTRA
      (let ((length (octets-integer data :start position :end (+ position 2)
                                         :endian :little)))
        (incf position (+ 2 length))))
    (when (logbitp 3 flags)                 ; FNAME
      (setf position (1+ (position 0 data :start position))))
    (when (logbitp 4 flags)                 ; FCOMMENT
      (setf position (1+ (position 0 data :start position))))
    (when (logbitp 1 flags)                 ; FHCRC
      (incf position 2))
    position))
