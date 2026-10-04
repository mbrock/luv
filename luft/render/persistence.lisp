(in-package #:luft.render)

;;; Saving the large world: TODO #DLH49J's first, smallest form.
;;;
;;; The authored world is a pure function of its seed, so a save is only what
;;; a player changed: the sparse edits, where they stand, whether they fly,
;;; where they look, and the time of day.  Chunks, meshes, and light are
;;; derived again on load.  The file is one readable s-expression, written
;;; whole to a temporary file and renamed over the old one, like Luvcraft's
;;; checkpoints, so a crash leaves the previous save intact.

(defparameter *luft-autosave-seconds* 30.0
  "How often a playing viewer writes its world when something changed.")

(defun default-luft-world-pathname ()
  "The ordinary persistent LUFT world, beside Luvcraft's under XDG data."
  (merge-pathnames
   #P"luft/worlds/default.sexp"
   (let ((data-home (uiop:getenv "XDG_DATA_HOME")))
     (if (and data-home (plusp (length data-home)))
         (uiop:ensure-directory-pathname (pathname data-home))
         (merge-pathnames #P".local/share/" (user-homedir-pathname))))))

(defstruct (viewer-world-save (:constructor make-viewer-world-save (pathname)))
  "Where a viewer's world is kept, and what was last written there."
  pathname
  (written nil)
  (next-time 0.0d0))

(defvar *viewer-world-saves* (make-hash-table :test #'eq :weakness :key)
  "Each persistent viewer's save record.")

(defun persistent-world-source (scene)
  (and (typep scene 'streaming-scene) (streaming-scene-source scene)))

(defun placement-named (name)
  (find name (domains:identity-vocabulary-members
              (make-scene-material-vocabulary))
        :key #'material-placement-name))

(defun luft-world-description (viewer)
  "Return VIEWER's world as a save description, or NIL without one."
  (let ((source (persistent-world-source (viewer-source viewer))))
    (when source
      (let* ((domain (authored-world-source-domain source))
             (player (viewer-player viewer))
             (camera (viewer-camera viewer))
             (edits nil))
        (maphash (lambda (cell placement)
                   (push (list (luft:site-x cell) (luft:site-y cell)
                               (luft:site-z cell)
                               (and placement (material-placement-name placement)))
                         edits))
                 (authored-world-source-edits source))
        (append
         (list :luft-world 1
               :seed (authored-world-source-seed source)
               :x-bits (luft:world-domain-x-bits domain)
               :y-bits (luft:world-domain-y-bits domain)
               :edits (sort edits #'< :key (lambda (edit)
                                            (+ (* 1000000 (first edit))
                                               (* 1000 (second edit))
                                               (third edit))))
               :yaw (camera-yaw camera)
               :pitch (camera-pitch camera)
               :sky-hour *sky-hour*)
         (when player
           (let ((position (body-position (character-body player))))
             (list :player (list (vec3-x position) (vec3-y position)
                                 (vec3-z position))
                   :flying-p (character-flying-p player)))))))))

(defun write-luft-world-description (description pathname)
  "Replace PATHNAME with DESCRIPTION, never leaving a partial file."
  (ensure-directories-exist pathname)
  (let ((temporary (make-pathname :type "tmp" :defaults pathname)))
    (with-open-file (stream temporary :direction :output :if-exists :supersede)
      (with-standard-io-syntax
        (let ((*print-pretty* t) (*print-right-margin* 100))
          (write description :stream stream)
          (terpri stream))))
    (rename-file temporary pathname))
  pathname)

(defun read-luft-world-description (pathname)
  "Return the save description at PATHNAME, or NIL when there is none."
  (when (probe-file pathname)
    (handler-case
        (with-open-file (stream pathname)
          (with-standard-io-syntax
            (let ((*read-eval* nil))
              (let ((description (read stream nil nil)))
                (and (listp description)
                     (eql 1 (getf description :luft-world))
                     description)))))
      (error (condition)
        (warn "Ignoring unreadable LUFT world ~A: ~A" pathname condition)
        nil))))

(defun load-luft-world-description (scene pathname)
  "Replay PATHNAME's edits into SCENE's source; return its description.

Runs before any chunk is materialized, so every chunk is built with the
edits already in place.  A save from another seed or world size is left
alone rather than applied to the wrong landscape."
  (let ((source (persistent-world-source scene))
        (description (and pathname (read-luft-world-description pathname))))
    (when (and source description)
      (let ((domain (authored-world-source-domain source)))
        (if (and (eql (getf description :seed) (authored-world-source-seed source))
                 (eql (getf description :x-bits) (luft:world-domain-x-bits domain))
                 (eql (getf description :y-bits) (luft:world-domain-y-bits domain)))
            (let ((edits (authored-world-source-edits source)))
              (dolist (edit (getf description :edits))
                (destructuring-bind (x y z name) edit
                  (let ((placement (and name (placement-named name))))
                    (when (or (null name) placement)
                      (setf (gethash (luft:make-site domain x y z
                                                     luft:+cell-extent+ 1)
                                     edits)
                            placement)))))
              (when (realp (getf description :sky-hour))
                (setf *sky-hour* (getf description :sky-hour)))
              description)
            (progn
              (warn "LUFT world ~A belongs to another seed or size; not loading it."
                    pathname)
              nil))))))

(defun restore-luft-world-player (viewer description)
  "Put VIEWER's player and camera where DESCRIPTION left them."
  (let ((player (viewer-player viewer))
        (camera (viewer-camera viewer))
        (saved (getf description :player)))
    (when (and player saved (= 3 (length saved)) (every #'realp saved))
      (let ((body (character-body player)))
        (dolist (position (list (body-position body) (body-previous-position body)))
          (setf (vec3-x position) (coerce (first saved) 'single-float)
                (vec3-y position) (coerce (second saved) 'single-float)
                (vec3-z position) (coerce (third saved) 'single-float)))
        (setf (vec3-x (body-velocity body)) 0.0
              (vec3-y (body-velocity body)) 0.0
              (vec3-z (body-velocity body)) 0.0)
        (set-character-flying player (getf description :flying-p))))
    (when (realp (getf description :yaw))
      (setf (camera-yaw camera) (coerce (getf description :yaw) 'single-float)))
    (when (realp (getf description :pitch))
      (setf (camera-pitch camera) (coerce (getf description :pitch) 'single-float))))
  viewer)

(defun note-viewer-world-pathname (viewer pathname)
  "Make VIEWER save its world to PATHNAME as it plays and when it stops."
  (if pathname
      (setf (gethash viewer *viewer-world-saves*)
            (make-viewer-world-save pathname))
      (remhash viewer *viewer-world-saves*))
  viewer)

(defun save-viewer-world (viewer &key force-p)
  "Write VIEWER's world if it changed since the last write; return its path."
  (let ((save (gethash viewer *viewer-world-saves*)))
    (when save
      (let ((description (luft-world-description viewer)))
        (when (and description
                   (or force-p
                       (not (equalp description (viewer-world-save-written save)))))
          (write-luft-world-description description
                                        (viewer-world-save-pathname save))
          (setf (viewer-world-save-written save) description)
          (viewer-world-save-pathname save))))))

(defun autosave-viewer-world (viewer)
  "Every *LUFT-AUTOSAVE-SECONDS*, write VIEWER's world if it changed."
  (let ((save (gethash viewer *viewer-world-saves*))
        (now (/ (get-internal-real-time)
                (float internal-time-units-per-second 1d0))))
    (when (and save (>= now (viewer-world-save-next-time save)))
      (setf (viewer-world-save-next-time save) (+ now *luft-autosave-seconds*))
      (handler-case (save-viewer-world viewer)
        (error (condition)
          (warn "Could not save the LUFT world: ~A" condition))))))
