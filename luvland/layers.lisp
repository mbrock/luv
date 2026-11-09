;;;; Layer surfaces: other programs' bars, launchers, wallpapers, overlays.
;;;;
;;;; A wlr-layer-shell client (waybar, fuzzel, swaybg, mako) asks for a place
;;;; on the screen rather than in the world, so Luvland draws its layer
;;;; surfaces flat over the view: background and bottom beneath the windows,
;;;; top and overlay above them, and its own bar above all.  They share the
;;;; space below that bar, and an exclusive zone moves the windows aside, as
;;;; the bar itself does.  Each is a WINDOW record whose toplevel is the
;;;; layer surface, so it uploads and imports as a window does.

(in-package #:luvland)

(defun layer-p (object)
  (typep object 'wl:layer-surface))

(defun window-surface (window)
  "The Wayland surface whose contents WINDOW shows."
  (let ((owner (window-toplevel window)))
    (if (layer-p owner)
        (wl:layer-surface-surface owner)
        (wl:toplevel-surface owner))))

(defun layer-area (luvland)
  "The (x y width height) in drawable pixels offered to layer surfaces: the
view below the bar."
  (destructuring-bind (width height) (canvas-extent luvland)
    (let ((bar (if (luvland-workbench luvland)
                   (round (* *bar-height* (display-scale luvland)))
                   0)))
      (list 0 bar width (max 1 (- height bar))))))

(defun exclusive-zones (luvland)
  "Drawable pixels layer surfaces keep windows away from, as a plist
(:top :right :bottom :left)."
  (let ((zones (list :top 0 :right 0 :bottom 0 :left 0)))
    (dolist (window (luvland-layers luvland) zones)
      (multiple-value-bind (edge pixels) (wl:layer-surface-exclusive-edge (window-toplevel window))
        (when edge
          (setf (getf zones edge) (max (getf zones edge) pixels)))))))

(defun sync-layers (luvland)
  "Follow the server's mapped layer surfaces, as SYNC-WINDOWS follows its
toplevels, and offer them the space below the bar.  Canvas thread."
  (let* ((server (luvland-server luvland))
         (mapped (remove-if-not #'wl:layer-surface-mapped-p
                                (reverse (wl:server-layer-surfaces server))))
         (kept (remove-if-not (lambda (window) (member (window-toplevel window) mapped))
                              (luvland-layers luvland)))
         (area (layer-area luvland)))
    (dolist (window (set-difference (luvland-layers luvland) kept))
      (retire luvland (window-shm-view window))
      (retire luvland (window-shm-texture window))
      (release-later luvland (window-held-frame window))
      (when (eq (luvland-keyboard-layer luvland) (window-toplevel window))
        (setf (luvland-keyboard-layer luvland) nil)
        (focus-keyboard-later luvland (luvland-focus luvland))))
    (dolist (layer mapped)
      (unless (find layer kept :key #'window-toplevel)
        (setf kept (append kept (list (make-window layer))))
        ;; A launcher that wants the keyboard gets it while it shows.
        (when (and (eq (wl:layer-surface-keyboard-interactivity layer) :exclusive)
                   (member (wl:layer-surface-layer layer) '(:top :overlay)))
          (setf (luvland-keyboard-layer luvland) layer)
          (let ((surface (wl:layer-surface-surface layer)))
            (send-to-server luvland (lambda () (wl:focus-keyboard surface)))))))
    (setf (luvland-layers luvland) kept)
    (unless (equal area (luvland-layer-area luvland))
      (setf (luvland-layer-area luvland) area)
      (send-to-server luvland (lambda () (wl:reconfigure-layer-surfaces area))))
    (dolist (window kept)
      (or (take-dmabuf-frame luvland window)
          (upload-snapshot luvland window)))))

(defun layer-placement (luvland window)
  "WINDOW's layer surface's (x y width height) in drawable pixels."
  (wl:layer-surface-geometry (window-toplevel window) (layer-area luvland)))

(defun layers-in (luvland layers)
  (remove-if-not (lambda (window)
                   (and (window-view window)
                        (member (wl:layer-surface-layer (window-toplevel window)) layers)))
                 (luvland-layers luvland)))

(defun encode-layers (luvland pass state extent layers)
  "Draw the layer surfaces in LAYERS, such as '(:TOP :OVERLAY), flat at
their places, one client pixel to one drawable pixel."
  (destructuring-bind (width height) extent
    (dolist (window (layers-in luvland layers))
      (destructuring-bind (x y w h) (layer-placement luvland window)
        (declare (ignore w h))
        (let* ((pixel-width (window-width window))
               (pixel-height (window-height window))
               (matrix (mat4 (/ (* 2 pixel-width) width) 0 0 0
                             0 (/ (* 2 pixel-height) height) 0 0
                             0 0 1 0
                             (- (/ (* 2 x) width) 1) (- (/ (* 2 y) height) 1) 0 1))
               (entry (window-bindings luvland state window)))
          (luv:write-buffer (getf entry :window-buffer)
                            (placement-uniform matrix window :tint '(1 1 1 1) :extend 0 :radius 0
                                                             :border-color '(0 0 0)))
          (luv:set-pipeline pass (if (window-alpha-p window)
                                     (luvland-icon-pipeline luvland)
                                     (luvland-pipeline luvland)))
          (luv:set-bind-group pass 0 (getf entry :window-group))
          (luv:draw pass 4))))))

(defun pick-layer (luvland x y layers)
  "The layer surface in LAYERS under logical point X, Y, topmost first, and
the surface-local point there."
  (let ((scale (display-scale luvland)))
    (dolist (window (reverse (layers-in luvland layers)))
      (destructuring-bind (left top w h) (layer-placement luvland window)
        (declare (ignore w h))
        (let ((u (- (* x scale) left))
              (v (- (* y scale) top)))
          (when (and (<= 0 u (window-width window)) (<= 0 v (window-height window)))
            (return (values window u v))))))))

(defun pick-anything (luvland x y)
  "What the pointer at logical X, Y is over, as PICK answers: top and overlay
layers first, then windows, then the layers beneath them."
  (multiple-value-bind (window u v) (pick-layer luvland x y '(:top :overlay))
    (if window
        (values window u v)
        (multiple-value-bind (window u v) (pick luvland x y)
          (if window
              (values window u v)
              (pick-layer luvland x y '(:background :bottom)))))))

(defun pointer-surface (window)
  "The surface the pointer enters over WINDOW, or NIL for a terminal."
  (if (layer-p (window-toplevel window))
      (window-surface window)
      (toplevel-surface (window-toplevel window))))

(defun click-layer (luvland layer)
  "A click on LAYER gives it the keyboard if it takes the keyboard on demand."
  (when (eq (wl:layer-surface-keyboard-interactivity layer) :on-demand)
    (setf (luvland-keyboard-layer luvland) layer)
    (let ((surface (wl:layer-surface-surface layer)))
      (send-to-server luvland (lambda () (wl:focus-keyboard surface))))))
