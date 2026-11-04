(in-package #:luvcraft.luft)

;;; A Luvcraft world's tag and chunk hashes (#RELI34), and a fingerprint two
;;; machines can diff: the recipe, the tag, the spawn, and the hashes of the
;;; Luft chunks around Luvcraft's origin.

(defconstant +luvcraft-generator-version+ 1
  "The version of what Luvcraft's generators raise for a recipe.

Bump it whenever luvcraft/terrain.lisp, a little-world class, or its load
request changes any generated cell, so worlds raised by the old and new
generators no longer share a tag.  The pinned chunk hash in the tests
notices such a change.")

(defmethod terrain:world-source-recipe ((source luvcraft-world-source))
  (let ((domain (terrain:world-source-domain source)))
    (list :luvcraft-little-world +luvcraft-generator-version+
          :seed (luvcraft-world-source-seed source)
          :relief (luvcraft-world-source-relief source)
          :height (luvcraft-world-source-height source)
          :little-class (luvcraft-world-source-little-class source)
          :offset (luvcraft-world-source-offset source)
          :domain (list (luft:world-domain-x-bits domain)
                        (luft:world-domain-y-bits domain)))))

(defun luvcraft-fingerprint-chunk-keys (source radius)
  "The Luft chunk keys within RADIUS chunks of the one holding Luvcraft's
origin, inside SOURCE's domain, rows of ascending Y and then X."
  (let* ((domain (terrain:world-source-domain source))
         (offset (luvcraft-world-source-offset source))
         (center (floor offset luft:+chunk-size+)))
    (loop for cy from (- center radius) to (+ center radius)
          nconc (loop for cx from (- center radius) to (+ center radius)
                      for x = (* cx luft:+chunk-size+)
                      for y = (* cy luft:+chunk-size+)
                      when (and (< -1 x (luft:world-domain-x-limit domain))
                                (< -1 y (luft:world-domain-y-limit domain)))
                        collect (luft:chunk-key-at x y)))))

(defun write-luvcraft-world-fingerprint
    (source &key (radius 1) (stream *standard-output*))
  "Write SOURCE's recipe, vocabulary digest, tag, spawn, and the content hash
of each chunk within RADIUS of Luvcraft's origin, one fact per line."
  (format stream "luvcraft world fingerprint 1~%")
  (format stream "recipe ~A~%"
          (terrain:canonical-recipe-text (terrain:world-source-recipe source)))
  (format stream "vocabulary ~A ~D~%"
          (terrain:material-vocabulary-digest
           (terrain:world-source-material-vocabulary source))
          (length (domains:identity-vocabulary-members
                   (terrain:world-source-material-vocabulary source))))
  (format stream "tag ~A~%" (terrain:world-source-tag source))
  ;; The scenic spawn's yaw comes through ATAN, the one libm call on the way.
  (multiple-value-bind (x y z yaw) (terrain:world-source-spawn source)
    (format stream "spawn ~D ~D ~D ~A~%" x y z
            (with-standard-io-syntax (prin1-to-string yaw))))
  (dolist (key (luvcraft-fingerprint-chunk-keys source radius))
    (multiple-value-bind (hash resident) (terrain:world-source-chunk-hash source key)
      (format stream "chunk ~D ~D cells ~D ~A~%"
              (luft:chunk-key-x key) (luft:chunk-key-y key)
              (hash-table-count (terrain:resident-cell-chunk-material-cells resident))
              hash)))
  (finish-output stream)
  (values))
