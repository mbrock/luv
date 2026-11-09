;;;; wlr-layer-shell: bars, launchers, wallpapers, and overlays.
;;;;
;;;; A layer surface is not a window.  It asks to be anchored to edges of the
;;;; output, in one of four layers (background, bottom, top, overlay), with
;;;; margins and perhaps an exclusive zone that windows keep clear of.  The
;;;; server keeps that state and proposes sizes; the host places and draws
;;;; the surfaces on the screen, outside its world, from LAYER-SURFACE-GEOMETRY.

(in-package #:luv.wayland)

(defclass layer-shell (resource) ())

(defstruct (layer-state (:copier copy-layer-state))
  (size '(0 0))
  (anchor 0)
  (exclusive-zone 0)
  (margin '(0 0 0 0))                   ; top right bottom left
  (keyboard-interactivity 0)
  (layer :top))

(defclass layer-surface (resource)
  ((surface :initarg :surface :reader layer-surface-surface)
   (namespace :initarg :namespace :reader layer-surface-namespace)
   (pending :initarg :pending :accessor layer-surface-pending)
   (current :initarg :current :accessor layer-surface-current)
   (configured-size :initform nil :accessor layer-surface-configured-size
                    :documentation "The size last proposed by configure.")
   (mapped-p :initform nil :accessor layer-surface-mapped-p)))

(defmethod print-object ((layer layer-surface) stream)
  (print-unreadable-object (layer stream :type t)
    (format stream "~S ~(~A~)~:[~; mapped~]" (layer-surface-namespace layer)
            (layer-surface-layer layer) (layer-surface-mapped-p layer))))

(defparameter +layers+ '(:background :bottom :top :overlay))

(defun layer-surface-layer (layer)
  (layer-state-layer (layer-surface-current layer)))

(defun layer-surface-anchor (layer)
  "The edges LAYER is anchored to, as a list of :TOP, :BOTTOM, :LEFT, :RIGHT."
  (anchor-edges (layer-state-anchor (layer-surface-current layer))))

(defun layer-surface-exclusive-zone (layer)
  (layer-state-exclusive-zone (layer-surface-current layer)))

(defun layer-surface-keyboard-interactivity (layer)
  (nth (layer-state-keyboard-interactivity (layer-surface-current layer))
       '(:none :exclusive :on-demand)))

(defun anchor-edges (bits)
  (loop for (bit edge) in '((1 :top) (2 :bottom) (4 :left) (8 :right))
        when (logtest bit bits) collect edge))

(define-globals layer-shell-global ()
  (add-global "zwlr_layer_shell_v1" 4
              (lambda (client version id)
                (make-resource 'layer-shell client "zwlr_layer_shell_v1" version id))))

(define-request (shell layer-shell :get-layer-surface) (id surface output layer namespace)
  (declare (ignore output))
  (let* ((state (make-layer-state :layer (or (nth layer +layers+) :top)))
         (layer-surface (make-child-resource 'layer-surface shell "zwlr_layer_surface_v1" id
                                             :surface surface :namespace (or namespace "")
                                             :pending state
                                             :current (copy-layer-state state))))
    (setf (surface-role surface) layer-surface)
    (push layer-surface (server-layer-surfaces *server*))
    layer-surface))

(define-request (shell layer-shell :destroy) ())

(defmacro define-layer-setter (request lambda-list &body body)
  `(define-request (layer layer-surface ,request) ,lambda-list
     (let ((pending (layer-surface-pending layer)))
       ,@body)))

(define-layer-setter :set-size (width height)
  (setf (layer-state-size pending) (list width height)))
(define-layer-setter :set-anchor (anchor)
  (setf (layer-state-anchor pending) anchor))
(define-layer-setter :set-exclusive-zone (zone)
  (setf (layer-state-exclusive-zone pending) zone))
(define-layer-setter :set-margin (top right bottom left)
  (setf (layer-state-margin pending) (list top right bottom left)))
(define-layer-setter :set-keyboard-interactivity (interactivity)
  (setf (layer-state-keyboard-interactivity pending) interactivity))
(define-layer-setter :set-layer (layer-index)
  (setf (layer-state-layer pending) (or (nth layer-index +layers+) :top)))

(define-request (layer layer-surface :get-popup) (popup)
  (declare (ignore popup)))
(define-request (layer layer-surface :ack-configure) (serial)
  (declare (ignore serial)))
(define-request (layer layer-surface :destroy) ())

(defmethod resource-destroyed ((layer layer-surface))
  (setf (server-layer-surfaces *server*) (remove layer (server-layer-surfaces *server*)))
  (let ((surface (layer-surface-surface layer)))
    (when (eq (surface-role surface) layer)
      (setf (surface-role surface) nil))))

;;; Placement.

(defun layer-geometry (area state)
  "Where a layer surface in STATE goes within AREA, a list (x y width
height) of the space the host offers: its (x y width height).  A surface
anchored to two opposite edges stretches between them when it asked for
size 0; one anchored to neither is centred on that axis."
  (destructuring-bind (ax ay aw ah) area
    (destructuring-bind (top right bottom left) (layer-state-margin state)
      (let ((edges (anchor-edges (layer-state-anchor state))))
        (flet ((axis (start length size near far near-margin far-margin)
                 (let* ((both (and (member near edges) (member far edges)))
                        (size (if (and both (zerop size))
                                  (max 0 (- length near-margin far-margin))
                                  size)))
                   (values (cond ((and (member near edges) (not both)) (+ start near-margin))
                                 ((and (member far edges) (not both))
                                  (- (+ start length) size far-margin))
                                 (both (+ start near-margin
                                          (floor (- length near-margin far-margin size) 2)))
                                 (t (+ start (floor (- length size) 2))))
                           size))))
          (destructuring-bind (width height) (layer-state-size state)
            (multiple-value-bind (x w) (axis ax aw width :left :right left right)
              (multiple-value-bind (y h) (axis ay ah height :top :bottom top bottom)
                (list x y w h)))))))))

(defun layer-exclusive-edge (state)
  "The edge whose space STATE's exclusive zone claims, or NIL: the edge it is
anchored to, alone or with both edges perpendicular to it."
  (when (plusp (layer-state-exclusive-zone state))
    (let ((edges (anchor-edges (layer-state-anchor state))))
      (flet ((only (edge across)
               (and (member edge edges)
                    (let ((others (remove edge edges)))
                      (or (null others)
                          (and (subsetp across others) (subsetp others across)))))))
        (cond ((only :top '(:left :right)) :top)
              ((only :bottom '(:left :right)) :bottom)
              ((only :left '(:top :bottom)) :left)
              ((only :right '(:top :bottom)) :right))))))

(defun layer-surface-geometry (layer area)
  (layer-geometry area (layer-surface-current layer)))

(defun layer-surface-exclusive-edge (layer)
  "The edge LAYER keeps windows away from, and by how many pixels."
  (let* ((state (layer-surface-current layer))
         (edge (layer-exclusive-edge state)))
    (when edge
      (values edge (+ (layer-state-exclusive-zone state)
                      (destructuring-bind (top right bottom left) (layer-state-margin state)
                        (ecase edge (:top top) (:right right) (:bottom bottom) (:left left))))))))

(defun configure-layer-surface (layer &optional (area (layer-area *server*)))
  "Propose LAYER's size within AREA, if it changed.  Call on the server thread."
  (let ((size (subseq (layer-surface-geometry layer area) 2)))
    (unless (equal size (layer-surface-configured-size layer))
      (setf (layer-surface-configured-size layer) size)
      (post-event layer :configure (next-serial) (first size) (second size)))))

(defun layer-area (server)
  (or (server-layer-area server)
      (destructuring-bind (width height) (server-output-size server)
        (list 0 0 width height))))

(defmethod surface-committed ((layer layer-surface) surface)
  (setf (layer-surface-current layer) (copy-layer-state (layer-surface-pending layer)))
  (cond
    ((null (layer-surface-configured-size layer))
     ;; The initial commit carries no buffer: answer it with a size.
     (configure-layer-surface layer))
    (t
     (let ((mapped (and (surface-has-contents-p surface) t)))
       (unless (eq mapped (layer-surface-mapped-p layer))
         (setf (layer-surface-mapped-p layer) mapped)
         (server-log "~A ~:[unmapped~;mapped~]" layer mapped))
       (if mapped
           (configure-layer-surface layer)
           ;; Unmapped by a null buffer: the next commit starts afresh.
           (setf (layer-surface-configured-size layer) nil))))))

(defun reconfigure-layer-surfaces (area &optional (server *server*))
  "Offer AREA, (x y width height), to every layer surface and propose new
sizes where that changes them.  Call on the server thread."
  (setf (server-layer-area server) area)
  (dolist (layer (server-layer-surfaces server))
    (when (layer-surface-configured-size layer)
      (configure-layer-surface layer area))))
