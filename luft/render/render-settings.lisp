(in-package #:luft.render)

;;; Presentation choices shared by the component factories and live atelier.

(defparameter *wireframe* 0.0
  "Global construction-edge strength.  The atelier toggles it between 0 and 1.")

(defparameter *render-scale* 0.75
  "Linear internal resolution of the LUFT scene before temporal upscaling.")

(defparameter *scene-sample-count* 4
  "Raster samples used by Luft's geometry, motion, and depth scene pass.")

(defparameter *temporal-upscaling-p* t
  "Whether LUFT uses temporal reconstruction on supported GPU devices.")

(defparameter *vulkan-temporal-history-weight* 0.97f0
  "Baseline retained history for Luft's inspectable Vulkan temporal resolve.")

(defparameter *flame-time* nil
  "Optional deterministic torch-flame time in seconds.

NIL lets the live viewer pass its monotonic presentation clock.  Captures may
dynamically bind a real value to reproduce the exact same flame field.")
