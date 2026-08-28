(in-package #:luft.avx512)

;;; SBCL 2.6.7 has native 512-bit register classes and AVX-512 instruction
;;; encoders, but SB-SIMD does not yet expose an AVX-512 instruction set.  One
;;; small whole-kernel VOP keeps that unfinished interface out of the selector.
;;;
;;; A ZMM covers two adjacent four-word Y fibers.  The caller consequently
;;; supplies an eight-word destination and consumes its two halves as two
;;; stars.  All offsets and the destination start are ordinary tagged fixnums.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-c:defknown %active-words
      ((simple-array (unsigned-byte 64) (*))
       (simple-array (unsigned-byte 64) (*))
       fixnum fixnum fixnum fixnum
       (simple-array (unsigned-byte 64) (*)) fixnum)
      (simple-array (unsigned-byte 64) (*))
      (sb-c:always-translatable)
    :overwrite-fndb-silently t)

  (sb-c:define-vop (%active-words-vop)
    (:translate %active-words)
    (:policy :fast-safe)
    (:args (below :scs (sb-vm::descriptor-reg))
           (above :scs (sb-vm::descriptor-reg))
           (southwest :scs (sb-vm::any-reg sb-vm::signed-reg
                             sb-vm::unsigned-reg))
           (southeast :scs (sb-vm::any-reg sb-vm::signed-reg
                             sb-vm::unsigned-reg))
           (northwest :scs (sb-vm::any-reg sb-vm::signed-reg
                             sb-vm::unsigned-reg))
           (northeast :scs (sb-vm::any-reg sb-vm::signed-reg
                             sb-vm::unsigned-reg))
           (active :scs (sb-vm::descriptor-reg) :target result)
           (active-start :scs (sb-vm::any-reg sb-vm::signed-reg
                                sb-vm::unsigned-reg)))
    (:arg-types sb-vm::simple-array-unsigned-byte-64
                sb-vm::simple-array-unsigned-byte-64
                sb-vm::tagged-num sb-vm::tagged-num
                sb-vm::tagged-num sb-vm::tagged-num
                sb-vm::simple-array-unsigned-byte-64 sb-vm::tagged-num)
    (:temporary (:sc sb-vm::int-avx512-reg) union intersection occupancy)
    (:results (result :scs (sb-vm::descriptor-reg)))
    (:result-types sb-vm::simple-array-unsigned-byte-64)
    (:generator
     8
     (macrolet
         ((address (vector index)
            `(sb-vm::ea
              (+ (* sb-vm::vector-data-offset sb-vm::n-word-bytes)
                 (- sb-vm::other-pointer-lowtag))
              ,vector ,index (sb-vm::index-scale 8 ,index)))
          (load-first (vector index)
            `(progn
               (sb-vm::inst sb-x86-64-asm::vmovdqu64 union
                            (address ,vector ,index))
               (sb-vm::move intersection union)))
          (include (vector index)
            `(progn
               (sb-vm::inst sb-x86-64-asm::vmovdqu64 occupancy
                            (address ,vector ,index))
               (sb-vm::inst sb-x86-64-asm::vporq union union occupancy)
               (sb-vm::inst sb-x86-64-asm::vpandq
                            intersection intersection occupancy))))
       ;; mixed = union(occupancy) AND NOT intersection(occupancy)
       (load-first below southwest)
       (include below southeast)
       (include below northwest)
       (include below northeast)
       (include above southwest)
       (include above southeast)
       (include above northwest)
       (include above northeast)
       (sb-vm::inst sb-x86-64-asm::vpandnq
                    union intersection union)
       (sb-vm::inst sb-x86-64-asm::vmovdqu64
                    (address active active-start) union)
       (sb-vm::move result active)))))

(defun active-words
    (below above southwest southeast northwest northeast active active-start)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array (unsigned-byte 64) (*)) below above active)
           (type fixnum southwest southeast northwest northeast active-start))
  (%active-words below above southwest southeast northwest northeast
                 active active-start))

(defun available-p ()
  "Whether the CPU advertises the AVX-512 foundation used by this backend."
  (and (>= (sb-simd-internals::cpuid 0) 7)
       (logbitp 16 (nth-value 1 (sb-simd-internals::cpuid 7)))))
