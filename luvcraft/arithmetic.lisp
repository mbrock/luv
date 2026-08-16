;;; Arithmetic laws shared by luvcraft's CPU and GPU realizations.

(in-package #:luvcraft.arithmetic)

;;; Fog has explicit near/far semantics: no attenuation before near, full fog
;;; at far, and quadratic shaping between.  This is a world law rather than a
;;; shader helper, so both production targets realize this one definition.
(lang:define-arithmetic-function fog-amount-at-view-distance
    ((view-distance :quantity :view-distance :unit :cell)
     (fog-near :quantity :view-distance :unit :cell)
     (fog-far :quantity :view-distance :unit :cell))
  (let* ((fog-span (- fog-far fog-near))
         (fog-progress
           (math:clamp (/ (- view-distance fog-near) fog-span)
                       (lang:quantity 0.0 :unit :one)
                       (lang:quantity 1.0 :unit :one))))
    (lang:interpret (* fog-progress fog-progress)
                    :quantity :fog-amount :unit :one)))

;;; Voxel light loses one attenuation step per cell entered plus the entered
;;; cell's own opacity, except that a direct transmission (sky light continuing
;;; straight down) pays only the opacity.  This is the one local law shared by
;;; the legacy solver, the seeds, and the compiled frontier kernel, which
;;; inlines the checked definition rather than calling this function. #53Q1II
(lisp:define-lisp-arithmetic-function light-propagation-loss
    ((opacity :quantity :block-light-attenuation-step :unit :one)
     (direct-transmission-p))
  (if direct-transmission-p
      opacity
      (+ opacity
         (lang:quantity 1 :quantity :block-light-attenuation-step :unit :one))))
