(defpackage #:mcluv.surveyor-tests
  (:use #:cl #:rove))

(in-package #:mcluv.surveyor-tests)

(deftest surveyor-captures-one-dense-terrain-product
  (let* ((world (luvcraft:make-empty-little-block-world :seed 121))
         (camera
           (make-instance 'luvcraft:fly-camera
                          :position (luvcraft::make-vec3 12d0 10d0 34d0)))
         (player (luvcraft:make-player-for-camera camera))
         (session
           (make-instance 'luvcraft:luvcraft-session
                          :world world :player player))
         (snapshot
           (mcluv::capture-surveyor-map-snapshot
            session :width 8 :depth 6)))
    (ok (= 48 (length (mcluv::surveyor-snapshot-heights snapshot))))
    (ok (= 48 (length (mcluv::surveyor-snapshot-materials snapshot))))
    (ok (= 48 (length (mcluv::surveyor-snapshot-lights snapshot))))
    (ok (= 12 (mcluv::surveyor-snapshot-center-x snapshot)))
    (ok (= 34 (mcluv::surveyor-snapshot-center-z snapshot)))
    (ok (<= (mcluv::surveyor-snapshot-minimum-height snapshot)
            (mcluv::surveyor-snapshot-maximum-height snapshot)))
    (ok (every (lambda (material) (typep material 'luvcraft:block-kind))
               (coerce (mcluv::surveyor-snapshot-materials snapshot) 'list)))))

(deftest unavailable-world-light-is-explicit
  (let ((world (luvcraft:make-empty-little-block-world :seed 121)))
    (multiple-value-bind (sky block state)
        (luvcraft:world-light-levels-at world 200 8 200)
      (ok (zerop sky))
      (ok (zerop block))
      (ok (eq state :unavailable)))))

(deftest hotbar-palette-covers-every-placeable-block
  (ok (= (length mcluv::*hotbar-material-colors*)
         (length (luvcraft:placeable-block-kinds))))
  (loop for number from 1 to 9
        do (ok (typep (mcluv::hotbar-material-color number) 'clim:color))
           (ok (typep (mcluv::hotbar-material-ink number 0 80)
                      'mcluv:linear-gradient))))

(deftest hotbar-is-composited-after-scene-postprocessing
  (ok (eq :hud
          (luvcraft:luvcraft-overlay-stage
           (allocate-instance
            (find-class 'mcluv:luvcraft-hotbar-overlay))))))

(deftest terminal-film-browser-shows-directories-and-playable-files
  (let* ((root (asdf:system-source-directory :luv))
         (directory (merge-pathnames "libav/" root))
         (entries (mcluv::terminal-film-browser-entries directory)))
    (ok (mcluv::terminal-film-pathname-p
         (merge-pathnames "test-pattern.mp4" directory)))
    (ok (not (mcluv::terminal-film-pathname-p
              (merge-pathnames "README.org" directory))))
    (ok (find "test-pattern.mp4" entries :test #'string=
              :key (lambda (entry)
                     (and (eq :film (mcluv::terminal-film-entry-kind entry))
                          (file-namestring
                           (mcluv::terminal-film-entry-pathname entry))))))))

(deftest terminal-film-browser-shortens-long-directory-headings
  (let ((label
          (mcluv::terminal-film-browser-path-label
           #P"/a/directory/name/that/is/long/enough/to/collide/with/the/browser/title/")))
    (ok (<= (length label) mcluv::+terminal-film-browser-path-limit+))
    (ok (string= "..." label :end2 3))))
