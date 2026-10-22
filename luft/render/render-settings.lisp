(in-package #:luft.render)

;;; Presentation choices shared by the component factories and live atelier.

(defparameter *wireframe* 0.0
  "Global construction-edge strength.  The atelier toggles it between 0 and 1.")

(defparameter *render-scale* 0.75
  "Linear internal resolution of the LUFT scene before temporal upscaling.

This is the ordinary size; dynamic resolution draws a share of it.")

(defparameter *dynamic-resolution-frame-rate* 60
  "The frame rate LUFT holds by drawing its scene smaller, or NIL for never.

Frames are due at this rate or the display's, whichever is slower; the
scene's share of *RENDER-SCALE* falls to a half while the GPU cannot keep up.")

(defparameter *dynamic-resolution-sample-interval* 0.1d0
  "Seconds between the GPU time samples dynamic resolution observes.")

(defparameter *scene-sample-count* 4
  "Raster samples for Luft's scene pass when temporal reconstruction is off.

A temporal scene rasterizes one sample per pixel: the jitter sequence
supplies the antialiasing, and multisampled depth and motion would need
resolves whose meaning at a silhouette is ambiguous.")

(defparameter *temporal-upscaling-p* t
  "Whether LUFT uses temporal reconstruction on supported GPU devices.")

(defparameter *temporal-reconstruction* :auto
  "Which temporal implementation LUFT uses: :AUTO, :SHADER, or :METALFX.

:AUTO selects MetalFX on Metal and Luft's own shader resolve elsewhere;
:SHADER selects the portable resolve on every device.")

(defparameter *temporal-history-weight* 0.9f0
  "Retained history share of Luft's shader resolve for a still pixel.")

(defparameter *flame-time* nil
  "Optional deterministic torch-flame time in seconds.

NIL lets the live viewer pass its monotonic presentation clock.  Captures may
dynamically bind a real value to reproduce the exact same flame field.")
