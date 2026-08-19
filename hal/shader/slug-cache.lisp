;;; Device-owned Slug shaping, outline, and atlas resources.
;;;
;;; SLUG-GLYPH-CACHE is the semantic device boundary.  It retains durable
;;; HarfBuzz results and normalized outlines, then packs the exact glyph set a
;;; run needs into shared RG16U/RGBA16F atlases.  Screen and world renderers
;;; supply their own placement geometry around these reusable records.

(in-package #:luv.slug)

(defstruct slug-device-glyph
  key glyph-id serialized)

(defstruct slug-glyph-placement
  glyph-id resource origin-x origin-y
  outline-min-x outline-min-y outline-max-x outline-max-y)

(defstruct slug-glyph-atlas
  key locations band-texel-count curve-texel-count
  band-texture band-view curve-texture curve-view)

(defclass slug-glyph-cache ()
  ((device :initarg :device :reader slug-glyph-cache-device)
   (entries :initform (make-hash-table :test #'equal)
            :reader slug-glyph-cache-entries)
   (atlases :initform (make-hash-table :test #'equal)
            :reader slug-glyph-cache-atlases))
  (:documentation
   "Reusable HarfBuzz results and exact Slug GPU outlines owned by one device."))

(defun make-slug-glyph-cache (device)
  (make-instance 'slug-glyph-cache :device device))

(defvar *slug-font-keys* (make-hash-table :test #'equal :synchronized t)
  "Resolved font keys by the namestring asked for: TRUENAME is a few
system calls and some consing, and is asked for once per glyph.")

(defun slug-font-key (font-pathname)
  (let ((name (namestring font-pathname)))
    (or (gethash name *slug-font-keys*)
        (setf (gethash name *slug-font-keys*)
              (namestring (truename font-pathname))))))

;;; One parsed font per file for the whole process.  Opening a TTF with
;;; ZPB-TTF reads and parses its tables, which is far too much to do per
;;; string per frame; the loader keeps its stream and is never closed.

(defvar *slug-font-loaders* (make-hash-table :test #'equal)
  "Open ZPB-TTF font loaders, keyed by the font file's true name.")

(defvar *slug-font-loaders-lock*
  (sb-thread:make-mutex :name "slug font loaders"))

(defun slug-font-loader (font-pathname)
  "The process-wide open ZPB-TTF loader for FONT-PATHNAME."
  (let ((key (slug-font-key font-pathname)))
    (sb-thread:with-mutex (*slug-font-loaders-lock*)
      (or (gethash key *slug-font-loaders*)
          (setf (gethash key *slug-font-loaders*)
                (zpb-ttf:open-font-loader key))))))

(defvar *slug-shaped-texts* (make-hash-table :test #'equal :synchronized t)
  "HarfBuzz results for the whole process, by font key, string, and
direction.  Shaping does not depend on the device, and measuring text
(TEXT-SIZE) happens with no device cache at hand, so one table serves all.")

(defun slug-shaped-text-for (font-pathname string &key direction)
  "Return one durable HarfBuzz result for an exact font, string, and direction."
  (let ((key (list (slug-font-key font-pathname) string direction)))
    (or (gethash key *slug-shaped-texts*)
        (setf (gethash key *slug-shaped-texts*)
              (shape-slug-text string font-pathname :direction direction)))))

(defun cached-slug-shaped-text (cache font-pathname string &key direction)
  "Return one durable HarfBuzz result for an exact font, string, and direction."
  (declare (ignore cache))
  (slug-shaped-text-for font-pathname string :direction direction))

(defun create-slug-device-glyph (key glyph-id font-loader)
  (let* ((glyph (load-slug-glyph-index glyph-id font-loader))
         (outline (normalize-slug-glyph-outline glyph)))
    (when (slug-outline-contours outline)
      (make-slug-device-glyph
       :key key :glyph-id glyph-id
       :serialized (serialize-slug-outline outline)))))

(defun slug-device-glyph-for (cache font-pathname glyph-id font-loader)
  (let* ((key (list (slug-font-key font-pathname) glyph-id))
         (entries (slug-glyph-cache-entries cache)))
    (multiple-value-bind (value present-p) (gethash key entries)
      (if present-p
          (unless (eq value :empty) value)
          (let ((resource
                  (create-slug-device-glyph key glyph-id font-loader)))
            (setf (gethash key entries) (or resource :empty))
            resource)))))

(defun slug-glyph-cache-resource-count (cache)
  (loop for resource being the hash-values of
          (slug-glyph-cache-entries cache)
        count (not (eq resource :empty))))

(defun slug-atlas-upload-data (resources count-reader data-reader element-type)
  (let* ((width 4096)
         (texel-count
           (loop for resource in resources
                 sum (funcall count-reader
                              (slug-device-glyph-serialized resource))))
         (height (max 1 (ceiling texel-count width)))
         (data (make-array (list height width)
                           :element-type element-type :initial-element 0))
         (offset 0))
    (dolist (resource resources)
      (let* ((serialized (slug-device-glyph-serialized resource))
             (count (funcall count-reader serialized))
             (source (funcall data-reader serialized)))
        (dotimes (index count)
          (setf (row-major-aref data (+ offset index))
                (row-major-aref source index)))
        (incf offset count)))
    (values data texel-count)))

(defun create-slug-glyph-atlas (cache key resources)
  "Pack RESOURCES into one RG16U band and one RGBA16F curve texture."
  (let ((locations (make-hash-table :test #'eq))
        (band-offset 0)
        (curve-offset 0))
    (dolist (resource resources)
      (setf (gethash resource locations) (list band-offset curve-offset))
      (incf band-offset
            (slug-serialized-outline-band-texel-count
             (slug-device-glyph-serialized resource)))
      (incf curve-offset
            (slug-serialized-outline-curve-texel-count
             (slug-device-glyph-serialized resource))))
    (multiple-value-bind (band-data band-count)
        (slug-atlas-upload-data
         resources #'slug-serialized-outline-band-texel-count
         #'slug-serialized-outline-band-upload-data '(unsigned-byte 32))
      (multiple-value-bind (curve-data curve-count)
          (slug-atlas-upload-data
           resources #'slug-serialized-outline-curve-texel-count
           #'slug-serialized-outline-curve-upload-data '(unsigned-byte 64))
        (let* ((device (slug-glyph-cache-device cache))
               (band-size (list 4096 (array-dimension band-data 0)))
               (curve-size (list 4096 (array-dimension curve-data 0)))
               (band-texture nil) (curve-texture nil)
               (band-view nil) (curve-view nil) (completed-p nil))
          (unwind-protect
               (progn
                 (setf band-texture
                       (luv:create
                        device
                        (luv:make-texture-descriptor
                         :label "Slug RG16U band atlas" :size band-size
                         :dimensions :2d :format :rg16-uint
                         :usage '(:texture-binding :copy-dst)))
                       curve-texture
                       (luv:create
                        device
                        (luv:make-texture-descriptor
                         :label "Slug RGBA16F curve atlas" :size curve-size
                         :dimensions :2d :format :rgba16-float
                         :usage '(:texture-binding :copy-dst)))
                       band-view
                       (luv:create
                        device
                        (luv:make-texture-view-descriptor
                         :texture band-texture))
                       curve-view
                       (luv:create
                        device
                        (luv:make-texture-view-descriptor
                         :texture curve-texture)))
                 (luv:write-texture
                  (luv:device-queue device)
                  (luv:make-texture-copy :texture band-texture)
                  band-data
                  (luv:make-texture-data-layout
                   :bytes-per-row (* 4 4096)
                   :rows-per-image (second band-size))
                  band-size)
                 (luv:write-texture
                  (luv:device-queue device)
                  (luv:make-texture-copy :texture curve-texture)
                  curve-data
                  (luv:make-texture-data-layout
                   :bytes-per-row (* 8 4096)
                   :rows-per-image (second curve-size))
                  curve-size)
                 (prog1
                     (make-slug-glyph-atlas
                      :key key :locations locations
                      :band-texel-count band-count
                      :curve-texel-count curve-count
                      :band-texture band-texture :band-view band-view
                      :curve-texture curve-texture :curve-view curve-view)
                   (setf completed-p t)))
            (unless completed-p
              (dolist (resource
                        (remove nil (list curve-view band-view
                                          curve-texture band-texture)))
                (luv:destroy resource)))))))))

(defun slug-glyph-atlas-for (cache glyphs)
  (let* ((resources
           (remove-duplicates
            (mapcar #'slug-glyph-placement-resource glyphs) :test #'eq))
         (key (mapcar #'slug-device-glyph-key resources))
         (atlases (slug-glyph-cache-atlases cache)))
    (or (gethash key atlases)
        (setf (gethash key atlases)
              (create-slug-glyph-atlas cache key resources)))))

(defun release-slug-glyph-atlas (atlas)
  (luv:destroy (slug-glyph-atlas-curve-view atlas))
  (luv:destroy (slug-glyph-atlas-band-view atlas))
  (luv:destroy (slug-glyph-atlas-curve-texture atlas))
  (luv:destroy (slug-glyph-atlas-band-texture atlas)))

(defun release-slug-glyph-cache (cache)
  (maphash (lambda (key atlas)
             (declare (ignore key))
             (release-slug-glyph-atlas atlas))
           (slug-glyph-cache-atlases cache))
  (clrhash (slug-glyph-cache-atlases cache))
  (clrhash (slug-glyph-cache-entries cache))
  (values))

(defun make-slug-glyph-placements
    (shaped font-loader cache font-pathname)
  "Join HarfBuzz placements to normalized Slug outlines by selected glyph ID."
  (let* ((unit (/ 1 (slug-shaped-text-units-per-em shaped)))
         (pen-x 0)
         (pen-y 0)
         glyphs)
    (loop for placement across (slug-shaped-text-glyphs shaped)
          for glyph-id = (slug-shaped-glyph-glyph-id placement)
          for resource = (slug-device-glyph-for
                          cache font-pathname glyph-id font-loader)
          do (when resource
               (let* ((serialized (slug-device-glyph-serialized resource))
                      (packed
                        (slug-serialized-outline-packed-outline serialized)))
                 (push
                  (make-slug-glyph-placement
                   :glyph-id glyph-id :resource resource
                   :origin-x (* (+ pen-x (slug-shaped-glyph-x-offset placement))
                                unit)
                   :origin-y (* (+ pen-y (slug-shaped-glyph-y-offset placement))
                                unit)
                   :outline-min-x (slug-packed-outline-min-x packed)
                   :outline-min-y (slug-packed-outline-min-y packed)
                   :outline-max-x (slug-packed-outline-max-x packed)
                   :outline-max-y (slug-packed-outline-max-y packed))
                  glyphs)))
             (incf pen-x (slug-shaped-glyph-x-advance placement))
             (incf pen-y (slug-shaped-glyph-y-advance placement)))
    (nreverse glyphs)))

(defun slug-text-extents (glyphs shaped font-loader)
  "Return normalized left, bottom, right, and top extents for GLYPHS."
  (let* ((unit (/ 1 (slug-shaped-text-units-per-em shaped)))
         (advance-x (* unit (slug-shaped-text-x-advance shaped)))
         (advance-y (* unit (slug-shaped-text-y-advance shaped))))
    (values
     (min 0.0
          (loop for glyph in glyphs
                minimize (+ (slug-glyph-placement-origin-x glyph)
                            (slug-glyph-placement-outline-min-x glyph))))
     (min 0.0 (* unit (zpb-ttf:descender font-loader))
          (loop for glyph in glyphs
                minimize (+ (slug-glyph-placement-origin-y glyph)
                            (slug-glyph-placement-outline-min-y glyph))))
     (max advance-x
          (loop for glyph in glyphs
                maximize (+ (slug-glyph-placement-origin-x glyph)
                            (slug-glyph-placement-outline-max-x glyph))))
     (max advance-y (* unit (zpb-ttf:ascender font-loader))
          (loop for glyph in glyphs
                maximize (+ (slug-glyph-placement-origin-y glyph)
                            (slug-glyph-placement-outline-max-y glyph)))))))
