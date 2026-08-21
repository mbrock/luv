(in-package #:luv.showcase)

;;; Short upright cuts of LUFT's authored atelier flights.  These recipes keep
;;; scene, camera, motion, and cleanup in LUFT's existing film owners; they only
;;; name the compositions for the showcase gazetteer. #SY26PO #2TQEBB

(luv:define-capture luft-holm-portrait
    (:figure SY26PO :kind :video :extension "mp4" :layout :portrait
     :description
     "An upright aerial and close pass over the atelier's island architecture.")
    (pathname)
  (luft.render:film-atelier-flight
   pathname
   :pieces '(:holm)
   :seconds-per-shot 5
   :frame-rate 30
   :width 720
   :height 1280
   :field-scale 1.25
   :style :stock
   :light :evening
   :aperture 0.85))

(luv:define-capture luft-vale-portrait
    (:figure SY26PO :kind :video :extension "mp4" :layout :portrait
     :description
     "An upright flight through the vale with its tree crowns modeled in clay.")
    (pathname)
  (let ((luft.render:*clay-radius* 0.5)
        (luft.render:*clay-melt* 0.45))
    (luft.render:film-atelier-flight
     pathname
     :pieces '(:vale)
     :seconds-per-shot 6
     :frame-rate 30
     :width 720
     :height 1280
     :field-scale 1.25
     :style :stock
     :light :evening
     :aperture 0.85
     :clay-stocks '(:conifer :leaf))))

(luv:define-capture luft-clay-holm-breath
    (:figure 2TQEBB :kind :video :extension "mp4" :layout :portrait
     :description
     "The holm's masonry breathing between quilted cells, pearls, and clay melt.")
    (pathname)
  (luft.render:film-clay-breath
   pathname
   :pieces '(:holm)
   :seconds-per-shot 5
   :frame-rate 30
   :width 720
   :height 1280
   :field-scale 1.25
   :light :golden
   :aperture 0.85))
