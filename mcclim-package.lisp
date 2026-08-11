;;; McCLIM names the logical display connection a port and the relationship
;;; between a sheet and a presentation target a mirror.  Luv's canvas is one
;;; possible presentation target; a texture displayed on a 3D surface will be
;;; another.  Keep those concepts separate even in this first native-window
;;; implementation.

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Keep source reloads honest when experimental public names are replaced.
  (let ((package (find-package '#:luv.mcclim)))
    (when package
      (let ((symbol (find-symbol "LUV-MEDIUM" package)))
        (when symbol
          (unexport symbol package))))))

(defpackage #:luv.mcclim
  (:use #:clim-lisp #:clim #:clime #:climb)
  (:local-nicknames (#:luv #:luv))
  (:export #:luv-port
           #:luv-raster-port
           #:luv-graft
           #:luv-mirror
           #:luv-raster-mirror
           #:luv-raster-medium
           #:luv-pointer
           #:mirror-sheet
           #:mirror-target
           #:mirror-context
           #:mirror-texture
           #:mirror-compositor
           #:port-mirrors
           #:present-mirror
           #:lab-sheet
           #:lab-sheet-image
           #:open-lab-sheet
           #:close-lab-sheet
           #:widget-lab
           #:widget-lab-click-count
           #:widget-lab-toggle-value
           #:open-widget-lab
           #:close-widget-lab
           #:shader-lab
           #:shader-lab-lowering
           #:shader-lab-selection
           #:open-shader-lab
           #:close-shader-lab
           #:spinning-texture-compositor
           #:enable-spinning-mirror
           #:disable-spinning-mirror
           #:open-spinning-widget-lab
           #:open-listener))

(in-package #:luv.mcclim)
