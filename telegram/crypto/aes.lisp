;;;; AES-256 and the IGE mode MTProto wraps around it.
;;;;
;;;; IGE -- infinite garble extension -- is a block mode almost nothing else
;;;; uses, which is exactly why it has to be here: no library ships it, and
;;;; MTProto uses it for every encrypted message and for both halves of the
;;;; authorization handshake.  Since IGE needs the raw block cipher rather
;;;; than a streaming ECB interface, AES itself is here too.
;;;;
;;;; The S-box is computed rather than transcribed.  It is defined as the
;;;; multiplicative inverse in GF(2^8) followed by a fixed affine map, and
;;;; writing that down is both shorter and more obviously right than two
;;;; hundred and fifty-six copied hexadecimal constants.

(in-package #:telegram.crypto)

(defconstant +aes-block-length+ 16)
(defconstant +aes-256-rounds+ 14)
(defconstant +aes-256-round-key-length+ (* 16 (1+ +aes-256-rounds+)))

(defun gf-multiply (left right)
  "Multiplication in GF(2^8) modulo the AES polynomial x^8+x^4+x^3+x+1."
  (declare (type (unsigned-byte 8) left right))
  (let ((accumulator left)
        (remaining right)
        (result 0))
    (declare (type (unsigned-byte 9) accumulator)
             (type (unsigned-byte 8) remaining result))
    (loop repeat 8
          do (when (logbitp 0 remaining) (setf result (logxor result accumulator)))
             (setf remaining (ash remaining -1)
                   accumulator (logand #x1FF (ash accumulator 1)))
             (when (logbitp 8 accumulator)
               (setf accumulator (logxor accumulator #x11B))))
    result))

(defun gf-inverse (value)
  "The multiplicative inverse of VALUE in GF(2^8); zero inverts to zero, as
the AES specification defines it."
  (if (zerop value)
      0
      (loop for candidate from 1 below 256
            when (= 1 (gf-multiply value candidate))
              return candidate)))

(defun rotate-left-8 (value count)
  (logand #xFF (logior (ash value count) (ash value (- count 8)))))

(defun affine-transform (value)
  "The AES S-box's affine map over GF(2)."
  (logxor value
          (rotate-left-8 value 1)
          (rotate-left-8 value 2)
          (rotate-left-8 value 3)
          (rotate-left-8 value 4)
          #x63))

(defparameter +aes-substitution+
  (let ((table (octets:make-octets 256)))
    (dotimes (value 256 table)
      (setf (aref table value) (affine-transform (gf-inverse value)))))
  "The AES S-box.")

(defparameter +aes-inverse-substitution+
  (let ((table (octets:make-octets 256)))
    (dotimes (value 256 table)
      (setf (aref table (aref +aes-substitution+ value)) value)))
  "The AES inverse S-box, derived by inverting the permutation above.")

(defparameter +aes-round-constants+
  (octets:to-octets '(#x00 #x01 #x02 #x04 #x08 #x10 #x20 #x40))
  "Rcon, indexed by round-key group.  AES-256 never needs more than seven.")

(defun aes-key-schedule (key)
  "Expand a 32-byte KEY into the 240 bytes of AES-256 round keys."
  (assert (= 32 (length key)) (key) "AES-256 needs a 32-byte key, got ~D."
          (length key))
  (let ((schedule (octets:make-octets +aes-256-round-key-length+))
        (temporary (octets:make-octets 4)))
    (replace schedule key)
    (loop for word from 8 below 60
          for base = (* 4 word)
          do (replace temporary schedule :start2 (- base 4) :end2 base)
             (cond ((zerop (mod word 8))
                    (let ((first (aref temporary 0)))
                      (replace temporary temporary :start1 0 :start2 1 :end2 4)
                      (setf (aref temporary 3) first))
                    (dotimes (index 4)
                      (setf (aref temporary index)
                            (aref +aes-substitution+ (aref temporary index))))
                    (setf (aref temporary 0)
                          (logxor (aref temporary 0)
                                  (aref +aes-round-constants+ (floor word 8)))))
                   ((= 4 (mod word 8))
                    (dotimes (index 4)
                      (setf (aref temporary index)
                            (aref +aes-substitution+ (aref temporary index))))))
             (dotimes (index 4)
               (setf (aref schedule (+ base index))
                     (logxor (aref schedule (+ base index -32))
                             (aref temporary index)))))
    schedule))

(defun add-round-key (state schedule round)
  (let ((base (* 16 round)))
    (dotimes (index 16 state)
      (setf (aref state index)
            (logxor (aref state index) (aref schedule (+ base index)))))))

(defun substitute-bytes (state table)
  (dotimes (index 16 state)
    (setf (aref state index) (aref table (aref state index)))))

(defmacro rotating-row (state row &rest indices)
  "Rewrite ROW of STATE from the state cells named by INDICES, in order.
The AES state is column-major, so row R lives at R, R+4, R+8, R+12."
  (let ((values (gensym "VALUES")))
    `(let ((,values (list ,@(loop for index in indices
                                  collect `(aref ,state ,index)))))
       (setf ,@(loop for offset from 0 below 4
                     append `((aref ,state ,(+ row (* 4 offset)))
                              (pop ,values)))))))

(defun shift-rows (state)
  (rotating-row state 1 5 9 13 1)
  (rotating-row state 2 10 14 2 6)
  (rotating-row state 3 15 3 7 11)
  state)

(defun inverse-shift-rows (state)
  (rotating-row state 1 13 1 5 9)
  (rotating-row state 2 10 14 2 6)
  (rotating-row state 3 7 11 15 3)
  state)

(defun mix-columns (state)
  (dotimes (column 4 state)
    (let* ((base (* 4 column))
           (a0 (aref state base)) (a1 (aref state (+ base 1)))
           (a2 (aref state (+ base 2))) (a3 (aref state (+ base 3))))
      (setf (aref state base)
            (logxor (gf-multiply a0 2) (gf-multiply a1 3) a2 a3)
            (aref state (+ base 1))
            (logxor a0 (gf-multiply a1 2) (gf-multiply a2 3) a3)
            (aref state (+ base 2))
            (logxor a0 a1 (gf-multiply a2 2) (gf-multiply a3 3))
            (aref state (+ base 3))
            (logxor (gf-multiply a0 3) a1 a2 (gf-multiply a3 2))))))

(defun inverse-mix-columns (state)
  (dotimes (column 4 state)
    (let* ((base (* 4 column))
           (a0 (aref state base)) (a1 (aref state (+ base 1)))
           (a2 (aref state (+ base 2))) (a3 (aref state (+ base 3))))
      (setf (aref state base)
            (logxor (gf-multiply a0 14) (gf-multiply a1 11)
                    (gf-multiply a2 13) (gf-multiply a3 9))
            (aref state (+ base 1))
            (logxor (gf-multiply a0 9) (gf-multiply a1 14)
                    (gf-multiply a2 11) (gf-multiply a3 13))
            (aref state (+ base 2))
            (logxor (gf-multiply a0 13) (gf-multiply a1 9)
                    (gf-multiply a2 14) (gf-multiply a3 11))
            (aref state (+ base 3))
            (logxor (gf-multiply a0 11) (gf-multiply a1 13)
                    (gf-multiply a2 9) (gf-multiply a3 14))))))

(defun aes-encrypt-block (schedule block &key (start 0))
  "Encrypt the sixteen bytes of BLOCK at START under SCHEDULE."
  (let ((state (octets:to-octets (subseq block start (+ start 16)))))
    (add-round-key state schedule 0)
    (loop for round from 1 below +aes-256-rounds+
          do (substitute-bytes state +aes-substitution+)
             (shift-rows state)
             (mix-columns state)
             (add-round-key state schedule round))
    (substitute-bytes state +aes-substitution+)
    (shift-rows state)
    (add-round-key state schedule +aes-256-rounds+)
    state))

(defun aes-decrypt-block (schedule block &key (start 0))
  "Decrypt the sixteen bytes of BLOCK at START under SCHEDULE."
  (let ((state (octets:to-octets (subseq block start (+ start 16)))))
    (add-round-key state schedule +aes-256-rounds+)
    (loop for round from (1- +aes-256-rounds+) above 0
          do (inverse-shift-rows state)
             (substitute-bytes state +aes-inverse-substitution+)
             (add-round-key state schedule round)
             (inverse-mix-columns state))
    (inverse-shift-rows state)
    (substitute-bytes state +aes-inverse-substitution+)
    (add-round-key state schedule 0)
    state))

;;;; IGE
;;;;
;;;; Each block is chained through both neighbours: the ciphertext depends on
;;;; the previous ciphertext going in and the previous plaintext coming out.
;;;; The 32-byte IV is those two priming values, ciphertext first.

(defun check-ige-arguments (data key iv)
  (assert (zerop (mod (length data) +aes-block-length+)) (data)
          "IGE data must be a whole number of 16-byte blocks, got ~D."
          (length data))
  (assert (= 32 (length key)) (key) "IGE needs a 32-byte key, got ~D."
          (length key))
  (assert (= 32 (length iv)) (iv) "IGE needs a 32-byte IV, got ~D."
          (length iv)))

(defun xor-into (target source &key (target-start 0) (source-start 0)
                                    (count +aes-block-length+))
  (dotimes (index count target)
    (setf (aref target (+ target-start index))
          (logxor (aref target (+ target-start index))
                  (aref source (+ source-start index))))))

(defun ige-encrypt (data key iv)
  "AES-256-IGE encryption of DATA under KEY with the 32-byte IV."
  (check-ige-arguments data key iv)
  (let* ((data (octets:to-octets data))
         (iv (octets:to-octets iv))
         (schedule (aes-key-schedule key))
         (result (octets:make-octets (length data)))
         (previous-cipher (octets:to-octets (subseq iv 0 16)))
         (previous-plain (octets:to-octets (subseq iv 16 32))))
    (loop for offset from 0 below (length data) by +aes-block-length+
          for plain = (octets:to-octets
                       (subseq data offset (+ offset +aes-block-length+)))
          for masked = (let ((copy (copy-seq plain)))
                         (xor-into copy previous-cipher))
          for cipher = (aes-encrypt-block schedule masked)
          do (xor-into cipher previous-plain)
             (replace result cipher :start1 offset)
             (setf previous-cipher cipher
                   previous-plain plain))
    result))

(defun ige-decrypt (data key iv)
  "AES-256-IGE decryption of DATA under KEY with the 32-byte IV."
  (check-ige-arguments data key iv)
  (let* ((data (octets:to-octets data))
         (iv (octets:to-octets iv))
         (schedule (aes-key-schedule key))
         (result (octets:make-octets (length data)))
         (previous-cipher (octets:to-octets (subseq iv 0 16)))
         (previous-plain (octets:to-octets (subseq iv 16 32))))
    (loop for offset from 0 below (length data) by +aes-block-length+
          for cipher = (octets:to-octets
                        (subseq data offset (+ offset +aes-block-length+)))
          for masked = (let ((copy (copy-seq cipher)))
                         (xor-into copy previous-plain))
          for plain = (aes-decrypt-block schedule masked)
          do (xor-into plain previous-cipher)
             (replace result plain :start1 offset)
             (setf previous-cipher cipher
                   previous-plain plain))
    result))
