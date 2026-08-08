(in-package #:luv.mcclim)

;;; A deliberately low-level backend laboratory, following McCLIM SDL2's
;;; PLAIN-SHEET example.  It exercises sheets, mirrors, media, repainting, and
;;; native lifetime without pulling in the examples system or an application
;;; frame.  The same sheet should remain useful as new renderers appear.

(defclass lab-sheet (immediate-repainting-mixin
                     immediate-sheet-input-mixin
                     permanent-medium-sheet-output-mixin
                     sheet-identity-transformation-mixin
                     sheet-parent-mixin
                     sheet-leaf-mixin
                     top-level-sheet-mixin
                     mirrored-sheet-mixin
                     basic-sheet)
  ()
  (:default-initargs
   :pretty-name "McCLIM on luv"
   :region (make-rectangle* 0 0 480 320)))

(defmethod handle-event ((sheet lab-sheet) event)
  (declare (ignore sheet event))
  nil)

(defmethod handle-event ((sheet lab-sheet) (event window-manager-delete-event))
  (declare (ignore event))
  (close-lab-sheet sheet))

(defmethod handle-event ((sheet lab-sheet) (event window-repaint-event))
  (dispatch-repaint sheet (window-event-region event)))

(defmethod handle-repaint ((sheet lab-sheet) region)
  (declare (ignore region))
  (with-bounding-rectangle* (left top right bottom) sheet
    (medium-clear-area sheet left top right bottom)
    (draw-rectangle* sheet
                     (+ left 24) (+ top 24)
                     (- right 24) (- bottom 24)
                     :ink +light-blue+)
    (draw-rectangle* sheet
                     (+ left 24) (+ top 24)
                     (- right 24) (- bottom 24)
                     :ink +dark-blue+ :filled nil)
    (draw-line* sheet
                (+ left 48) (- bottom 64)
                (- right 48) (+ top 64)
                :ink +dark-red+ :line-thickness 3)
    (draw-circle* sheet
                  (/ (+ left right) 2)
                  (/ (+ top bottom) 2)
                  52
                  :ink +goldenrod+)
    (draw-text* sheet "hello from McCLIM on luv"
                (+ left 36) (+ top 42)
                :ink +black+ :align-y :top))
  (medium-finish-output sheet))

(defun lab-sheet-image (sheet)
  "Return SHEET's inspectable CPU raster image, or NIL before realization."
  (let ((mirror (sheet-direct-mirror sheet)))
    (when (typep mirror 'luv-raster-mirror)
      (mcclim-render:image-mirror-image mirror))))

(defun open-lab-sheet (&key (server-path '(:luv))
                            (title "McCLIM on luv")
                            (width 480) (height 320))
  "Realize and repaint a small backend-test sheet on SERVER-PATH."
  (let* ((port (find-port :server-path server-path))
         (graft (find-graft :port port))
         (sheet (make-instance 'lab-sheet
                               :pretty-name title
                               :region (make-rectangle* 0 0 width height))))
    (sheet-adopt-child graft sheet)
    (enable-mirror port sheet)
    (repaint-sheet sheet +everywhere+)
    sheet))

(defun close-lab-sheet (sheet)
  "Disown SHEET, destroying its mirror and native target."
  (check-type sheet lab-sheet)
  (when (sheet-parent sheet)
    (sheet-disown-child (sheet-parent sheet) sheet))
  nil)
