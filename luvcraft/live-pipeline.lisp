;;; Live shader pipelines moved to HAL/LIVE-ARTIFACT.LISP.
;;;
;;; This former Luvcraft implementation path remains only as a source-history
;;; marker.  LUV owns the shared artifact protocol and its concrete
;;; LIVE-SHADER-PIPELINE; both Luvcraft and LUFT can attach application-level
;;; artifacts without copying the transactional build/install lifecycle.

(in-package #:luvcraft)
