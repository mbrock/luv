;;; One adapter for all games using engine knobs.
(in-package #:mcluv)

(defmethod metabar-groups-for (host)
  (luv.knobs:knob-groups (luv.knobs:knobs-for-host host)))

(defmethod metabar-controls-for (host group)
  (luv.knobs:knobs-in-group group (luv.knobs:knobs-for-host host)))

(defmethod metabar-actions-for (host)
  (luv.knobs:actions-for-host host))

(defmethod metabar-control-kind ((knob luv.knobs:scalar-knob) host)
  (declare (ignore knob host))
  :scalar)

(defmethod metabar-control-kind ((knob luv.knobs:switch-knob) host)
  (declare (ignore knob host))
  :switch)

(defmethod metabar-control-label ((knob luv.knobs:knob) host)
  (declare (ignore host))
  (luv.knobs:knob-label knob))

(defmethod metabar-control-value ((knob luv.knobs:knob) host)
  (luv.knobs:knob-value knob host))

(defmethod metabar-control-value-label ((knob luv.knobs:knob) host value)
  (declare (ignore value))
  (luv.knobs:format-knob-value knob host))

(defmethod metabar-control-fraction ((knob luv.knobs:knob) host value)
  (declare (ignore value))
  (luv.knobs:knob-fraction knob host))

(defmethod metabar-control-change-kind ((knob luv.knobs:knob) host)
  (and (luv.knobs:knob-rebuild-p knob host) :rebuild))

(defmethod metabar-control-update-policy ((knob luv.knobs:scalar-knob) host)
  (if (luv.knobs:knob-rebuild-p knob host) :commit-on-release :continuous))

(defmethod perform-metabar-control-step
    ((knob luv.knobs:knob) host direction multiplier)
  (luv.knobs:step-knob knob host direction multiplier))

(defmethod perform-metabar-control-set-fraction
    ((knob luv.knobs:scalar-knob) host fraction)
  (let* ((minimum (luv.knobs:knob-minimum knob))
         (maximum (luv.knobs:knob-maximum knob))
         (step (luv.knobs:knob-step knob))
         (raw (+ minimum (* fraction (- maximum minimum)))))
    (luv.knobs:set-knob-value knob (* step (round raw step)) host)))

(defmethod perform-metabar-control-toggle ((knob luv.knobs:switch-knob) host)
  (luv.knobs:toggle-knob knob host))

(defmethod metabar-action-label ((action luv.knobs:action) host)
  (declare (ignore host))
  (luv.knobs:action-label action))

(defmethod perform-metabar-action ((action luv.knobs:action) host)
  (luv.knobs:run-action action host))
