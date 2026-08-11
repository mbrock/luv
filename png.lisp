;;; Tiny dependency-free PNG output for renderer readbacks.

(in-package #:luv)

(defparameter *png-crc-table*
  (make-array
   256 :element-type '(unsigned-byte 32)
   :initial-contents
   (loop for index below 256
         collect
         (let ((value index))
           (dotimes (bit 8)
             (setf value
                   (if (oddp value)
                       (logxor #xedb88320 (ash value -1))
                       (ash value -1))))
           (ldb (byte 32 0) value)))))

(defun png-crc32 (type data)
  (let ((crc #xffffffff))
    (flet ((consume (byte)
             (setf crc
                   (logxor (aref *png-crc-table*
                                 (logand #xff (logxor crc byte)))
                           (ash crc -8)))))
      (loop for character across type
            do (consume (char-code character)))
      (loop for byte across data do (consume byte)))
    (ldb (byte 32 0) (logxor crc #xffffffff))))

(defun write-png-u32 (stream value)
  (loop for shift in '(24 16 8 0)
        do (write-byte (ldb (byte 8 shift) value) stream)))

(defun write-png-chunk (stream type data)
  (write-png-u32 stream (length data))
  (loop for character across type do (write-byte (char-code character) stream))
  (write-sequence data stream)
  (write-png-u32 stream (png-crc32 type data)))

(defun png-adler32 (data)
  (let ((a 1) (b 0))
    (loop for byte across data
          do (setf a (mod (+ a byte) 65521)
                   b (mod (+ b a) 65521)))
    (logior (ash b 16) a)))

(defun png-uncompressed-zlib (data)
  (let ((output (make-array 0 :element-type '(unsigned-byte 8)
                              :adjustable t :fill-pointer 0)))
    ;; CMF/FLG for DEFLATE, a 32K window, and the fastest compression level.
    (vector-push-extend #x78 output)
    (vector-push-extend #x01 output)
    (loop with position = 0
          while (< position (length data))
          for count = (min 65535 (- (length data) position))
          for final-p = (= (+ position count) (length data))
          for complement = (logxor count #xffff)
          do (vector-push-extend (if final-p 1 0) output)
             (vector-push-extend (ldb (byte 8 0) count) output)
             (vector-push-extend (ldb (byte 8 8) count) output)
             (vector-push-extend (ldb (byte 8 0) complement) output)
             (vector-push-extend (ldb (byte 8 8) complement) output)
             (loop repeat count
                   do (vector-push-extend (aref data position) output)
                      (incf position)))
    (let ((checksum (png-adler32 data)))
      (loop for shift in '(24 16 8 0)
            do (vector-push-extend (ldb (byte 8 shift) checksum) output)))
    output))

(defun rgba-png-scanlines (pixels width height format)
  (unless (= (length pixels) (* width height 4))
    (error "Expected ~D pixels bytes, got ~D."
           (* width height 4) (length pixels)))
  (unless (member format '(:rgba8-unorm :rgba8-unorm-srgb
                           :bgra8-unorm :bgra8-unorm-srgb))
    (error "Cannot write PNG pixels in format ~S." format))
  (let ((scanlines
          (make-array (* height (1+ (* width 4)))
                      :element-type '(unsigned-byte 8)))
        (source 0)
        (destination 0)
        (bgra-p (member format '(:bgra8-unorm :bgra8-unorm-srgb))))
    (dotimes (y height)
      (setf (aref scanlines destination) 0)
      (incf destination)
      (dotimes (x width)
        (if bgra-p
            (setf (aref scanlines destination) (aref pixels (+ source 2))
                  (aref scanlines (+ destination 1))
                  (aref pixels (+ source 1))
                  (aref scanlines (+ destination 2)) (aref pixels source)
                  (aref scanlines (+ destination 3))
                  (aref pixels (+ source 3)))
            (replace scanlines pixels :start1 destination :end1 (+ destination 4)
                                      :start2 source :end2 (+ source 4)))
        (incf source 4)
        (incf destination 4)))
    scanlines))

(defun write-rgba-png (pathname pixels width height format)
  "Write tightly packed RGBA or BGRA byte PIXELS to PATHNAME as a PNG."
  (let ((header (make-array 13 :element-type '(unsigned-byte 8)
                              :initial-element 0))
        (scanlines (rgba-png-scanlines pixels width height format)))
    (labels ((set-header-u32 (offset value)
               (loop for shift in '(24 16 8 0)
                     for index from offset
                     do (setf (aref header index)
                              (ldb (byte 8 shift) value)))))
      (set-header-u32 0 width)
      (set-header-u32 4 height))
    (setf (aref header 8) 8       ; bit depth
          (aref header 9) 6       ; RGBA
          (aref header 10) 0      ; DEFLATE
          (aref header 11) 0      ; adaptive filtering
          (aref header 12) 0)     ; no interlace
    (with-open-file (stream pathname :direction :output
                                     :if-exists :supersede
                                     :element-type '(unsigned-byte 8))
      (write-sequence #(137 80 78 71 13 10 26 10) stream)
      (write-png-chunk stream "IHDR" header)
      (write-png-chunk stream "IDAT" (png-uncompressed-zlib scanlines))
      (write-png-chunk stream "IEND" #()))
    pathname))
