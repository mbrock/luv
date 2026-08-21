(in-package #:luvcraft.tests)

(deftest native-luvcraft-world-is-authored-as-packed-luft-sites
  (let* ((native (make-native-luvcraft-world :horizontal-bits 6))
         (domain (native-luvcraft-world-domain native))
         (site (native-luvcraft-cell-site native 8 18 8)))
    (ok (typep site 'fixnum))
    (ok (typep site 'luft:site))
    (ok (= 8 (luft:site-x site)))
    (ok (= 18 (luft:site-y site)))
    (ok (= 8 (luft:site-z site)))
    (ok (eq domain
            (luft.render:scene-domain
             (native-luvcraft-world-scene native))))))

(deftest native-luvcraft-spawn-is-owned-by-the-native-world
  (let* ((camera (luvcraft::make-native-luvcraft-spawn-camera))
         (adapter (make-luft-frame-adapter))
         (target (update-luft-frame-adapter-camera adapter camera 0))
         (position (luft.render:camera-position target)))
    (ok (= 8.0 (vec3-x position)))
    (ok (= 4.0 (vec3-y position)))
    (ok (= 11.0 (vec3-z position)))
    (ok (luft-frame-close-p
         (/ pi 2) (luft.render:camera-yaw target)))
    (ok (luft-frame-close-p
         -0.18 (luft.render:camera-pitch target)))))

(deftest native-luvcraft-world-has-a-useful-not-seven-stock-palette
  (let* ((native (make-native-luvcraft-world :horizontal-bits 6))
         (materials (native-luvcraft-world-materials native)))
    (ok (> (length materials) 7))
    (ok (<= (length materials) luft.render.shaders:+stock-slots+))
    (loop for stock across luvcraft::+native-luvcraft-initial-materials+
          do (ok (find stock materials)))))

(deftest native-luvcraft-cell-edits-publish-one-direct-boundary-revision
  (let* ((native (make-native-luvcraft-world :horizontal-bits 6))
         (scene (native-luvcraft-world-scene native))
         (site (native-luvcraft-cell-site native 40 8 30))
         (revision (luft.render:scene-revision scene)))
    (ng (native-luvcraft-cell-p native site))
    (set-native-luvcraft-cell native site t :stock :crystal)
    (ok (native-luvcraft-cell-p native site))
    (ok (= (1+ revision) (luft.render:scene-revision scene)))
    (set-native-luvcraft-cell native site nil)
    (ng (native-luvcraft-cell-p native site))
    (ok (= (+ 2 revision) (luft.render:scene-revision scene)))))
