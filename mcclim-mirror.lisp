(in-package #:luv.mcclim)

(defclass luv-mirror (mcclim-render:image-mirror-mixin)
  ((sheet
    :initarg :sheet
    :reader mirror-sheet)
   (target
    :initarg :target
    :reader mirror-target)
   (context
    :initarg :context
    :initform nil
    :accessor mirror-context))
  (:documentation
   "A McCLIM sheet's relationship with a luv presentation target.

TARGET is initially a native canvas.  It is intentionally not part of the
mirror's identity: a later target may be a texture presented on a 3D quad."))

(defmethod print-object ((mirror luv-mirror) stream)
  (print-unreadable-object (mirror stream :type t :identity t)
    (format stream "~S on ~S" (mirror-sheet mirror) (mirror-target mirror))))

(defun sheet-title (sheet)
  (or (and (typep sheet 'top-level-sheet-mixin)
           (sheet-pretty-name sheet))
      "McCLIM on luv"))

(defmethod realize-mirror ((port luv-port) (sheet mirrored-sheet-mixin))
  (with-bounding-rectangle* (:width width :height height) sheet
    (let* ((canvas (luv:make-sdl-canvas
                    :title (sheet-title sheet)
                    :width (max 1 (ceiling width))
                    :height (max 1 (ceiling height))))
           (mirror (make-instance 'luv-mirror
                                  :sheet sheet
                                  :target canvas)))
      (handler-case
          (progn
            (mcclim-render::%set-image-region
             mirror
             (make-rectangle* 0 0
                              (max 1 (ceiling width))
                              (max 1 (ceiling height))))
            (luv:open-canvas canvas)
            (push mirror (port-mirrors port))
            mirror)
        (error (condition)
          (when (member (luv:canvas-state canvas) '(:opening :open))
            (ignore-errors (luv:close-canvas canvas)))
          (error condition))))))

(defmethod destroy-mirror ((port luv-port) (sheet mirrored-sheet-mixin))
  (let ((mirror (sheet-direct-mirror sheet)))
    (when mirror
      (let ((target (mirror-target mirror)))
        (when (member (luv:canvas-state target) '(:opening :open))
          (luv:close-canvas target)))
      (setf (port-mirrors port)
            (delete mirror (port-mirrors port))))))

(defmethod enable-mirror ((port luv-port) (sheet mirrored-sheet-mixin))
  (declare (ignore port sheet))
  ;; OPEN-CANVAS currently realizes and shows its native target atomically.
  nil)

(defmethod disable-mirror ((port luv-port) (sheet mirrored-sheet-mixin))
  (declare (ignore port sheet))
  ;; Native show/hide will become part of the canvas host protocol.
  nil)

(defmethod set-mirror-geometry
    ((port luv-port) (sheet mirrored-sheet-mixin) region)
  (declare (ignore port sheet))
  ;; Report the geometry McCLIM requested.  Native resizing belongs in the
  ;; forthcoming canvas host protocol rather than in SDL calls here.
  (bounding-rectangle* region))

(defmethod set-mirror-name
    ((port luv-port) (sheet top-level-sheet-mixin) name)
  (declare (ignore port sheet name))
  nil)

(defmethod set-mirror-icon
    ((port luv-port) (sheet top-level-sheet-mixin) icon)
  (declare (ignore port sheet icon))
  nil)

(defmethod raise-mirror ((port luv-port) (sheet top-level-sheet-mixin))
  (declare (ignore port sheet))
  nil)

(defmethod bury-mirror ((port luv-port) (sheet top-level-sheet-mixin))
  (declare (ignore port sheet))
  nil)

(defmethod shrink-mirror ((port luv-port) (sheet top-level-sheet-mixin))
  (declare (ignore port sheet))
  nil)

(defmethod unshrink-mirror ((port luv-port) (sheet top-level-sheet-mixin))
  (declare (ignore port sheet))
  nil)
