;;;; xdg-shell: windows, and the decorations Luvland draws itself.

(in-package #:luv.wayland)

(defclass wm-base (resource) ())
(defclass positioner (resource)
  ((size :initform '(0 0) :accessor positioner-size)
   (anchor-rect :initform '(0 0 0 0) :accessor positioner-anchor-rect)
   (offset :initform '(0 0) :accessor positioner-offset)))

(defclass xdg-surface (resource)
  ((surface :initarg :surface :reader xdg-surface-surface)
   (role :initform nil :accessor xdg-surface-role)
   (geometry :initform nil :accessor xdg-surface-geometry)
   (pending-geometry :initform nil :accessor xdg-surface-pending-geometry)
   (configured-p :initform nil :accessor xdg-surface-configured-p
                 :documentation "Whether the initial configure has been sent.")
   (configure-queued-p :initform nil :accessor xdg-surface-configure-queued-p)
   (last-serial :initform nil :accessor xdg-surface-last-serial)
   (acked-serial :initform nil :accessor xdg-surface-acked-serial)))

(defclass toplevel (resource)
  ((xdg-surface :initarg :xdg-surface :reader toplevel-xdg-surface)
   (title :initform nil :accessor toplevel-title)
   (app-id :initform nil :accessor toplevel-app-id)
   (size :initform nil :accessor toplevel-size
         :documentation "The size last proposed by configure, or NIL for the client's choice.")
   (states :initform '(:activated) :accessor toplevel-states)
   (mapped-p :initform nil :accessor toplevel-mapped-p)
   (decoration :initform nil :accessor toplevel-decoration)))

(defclass popup (resource)
  ((xdg-surface :initarg :xdg-surface :reader popup-xdg-surface)
   (parent :initarg :parent :reader popup-parent)
   (geometry :initarg :geometry :accessor popup-geometry)))

(defmethod print-object ((toplevel toplevel) stream)
  (print-unreadable-object (toplevel stream :type t)
    (format stream "~S ~S~:[~; mapped~]" (toplevel-app-id toplevel)
            (toplevel-title toplevel) (toplevel-mapped-p toplevel))))

(defun toplevel-surface (toplevel)
  (xdg-surface-surface (toplevel-xdg-surface toplevel)))

(define-globals xdg-shell-global ()
  (add-global "xdg_wm_base" 3
              (lambda (client version id)
                (make-resource 'wm-base client "xdg_wm_base" version id))))

(define-request (base wm-base :pong) (serial)
  (declare (ignore serial)))

(define-request (base wm-base :create-positioner) (id)
  (make-child-resource 'positioner base "xdg_positioner" id))

(define-request (base wm-base :get-xdg-surface) (id surface)
  (let ((xdg-surface (make-child-resource 'xdg-surface base "xdg_surface" id
                                          :surface surface)))
    (setf (surface-role surface) xdg-surface)))

;;; Positioners hold just enough for a first popup placement.

(define-request (positioner positioner :set-size) (width height)
  (setf (positioner-size positioner) (list width height)))
(define-request (positioner positioner :set-anchor-rect) (x y width height)
  (setf (positioner-anchor-rect positioner) (list x y width height)))
(define-request (positioner positioner :set-offset) (x y)
  (setf (positioner-offset positioner) (list x y)))
(define-request (positioner positioner :set-anchor) (anchor) (declare (ignore anchor)))
(define-request (positioner positioner :set-gravity) (gravity) (declare (ignore gravity)))
(define-request (positioner positioner :set-constraint-adjustment) (adjustment)
  (declare (ignore adjustment)))
(define-request (positioner positioner :set-reactive) ())
(define-request (positioner positioner :set-parent-size) (width height)
  (declare (ignore width height)))
(define-request (positioner positioner :set-parent-configure) (serial)
  (declare (ignore serial)))

(defun positioner-geometry (positioner)
  "Below the anchor rectangle's corner, ignoring anchor and gravity for now."
  (destructuring-bind (ax ay aw ah) (positioner-anchor-rect positioner)
    (declare (ignore aw))
    (destructuring-bind (dx dy) (positioner-offset positioner)
      (destructuring-bind (width height) (positioner-size positioner)
        (list (+ ax dx) (+ ay ah dy) width height)))))

;;; xdg_surface.

(define-request (xdg-surface xdg-surface :get-toplevel) (id)
  (let ((toplevel (make-child-resource 'toplevel xdg-surface "xdg_toplevel" id
                                       :xdg-surface xdg-surface)))
    (setf (xdg-surface-role xdg-surface) toplevel
          (toplevel-size toplevel) (server-initial-toplevel-size *server*))
    (push toplevel (server-toplevels *server*))
    toplevel))

(define-request (xdg-surface xdg-surface :get-popup) (id parent positioner)
  (let ((popup (make-child-resource 'popup xdg-surface "xdg_popup" id
                                    :xdg-surface xdg-surface :parent parent
                                    :geometry (positioner-geometry positioner))))
    (setf (xdg-surface-role xdg-surface) popup)))

(define-request (xdg-surface xdg-surface :set-window-geometry) (x y width height)
  (setf (xdg-surface-pending-geometry xdg-surface) (list x y width height)))

(define-request (xdg-surface xdg-surface :ack-configure) (serial)
  (setf (xdg-surface-acked-serial xdg-surface) serial))

(defmethod surface-committed ((xdg-surface xdg-surface) surface)
  (when (xdg-surface-pending-geometry xdg-surface)
    (setf (xdg-surface-geometry xdg-surface)
          (shiftf (xdg-surface-pending-geometry xdg-surface) nil)))
  (cond
    ((not (xdg-surface-configured-p xdg-surface))
     ;; The initial commit carries no buffer.  Configure after the rest of
     ;; the client's batch, so its title and app id are already known.
     (unless (xdg-surface-configure-queued-p xdg-surface)
       (setf (xdg-surface-configure-queued-p xdg-surface) t)
       (defer (lambda ()
                (when (resource-live-p xdg-surface)
                  (send-configure xdg-surface))))))
    (t
     (role-committed (xdg-surface-role xdg-surface) surface))))

(defgeneric role-committed (role surface)
  (:method (role surface) (declare (ignore role surface)) nil))

(defgeneric send-role-configure (role)
  (:method (role) (declare (ignore role)) nil))

(defun send-configure (xdg-surface)
  (send-role-configure (xdg-surface-role xdg-surface))
  (let ((serial (next-serial)))
    (setf (xdg-surface-last-serial xdg-surface) serial
          (xdg-surface-configured-p xdg-surface) t)
    (post-event xdg-surface :configure serial)
    serial))

(defmethod resource-destroyed ((xdg-surface xdg-surface))
  (let ((surface (xdg-surface-surface xdg-surface)))
    (when (eq (surface-role surface) xdg-surface)
      (setf (surface-role surface) nil))))

;;; xdg_toplevel.

(defun toplevel-state-codes (toplevel)
  (loop for state in (toplevel-states toplevel)
        collect (enum-value (resource-interface toplevel) "state" state)))

(defmethod send-role-configure ((toplevel toplevel))
  (destructuring-bind (&optional (width 0) (height 0)) (toplevel-size toplevel)
    (post-event toplevel :configure width height
                (uint32-array (toplevel-state-codes toplevel))))
  (let ((decoration (toplevel-decoration toplevel)))
    (when decoration
      (post-event decoration :configure :server-side))))

(defun configure-toplevel (toplevel &key (size nil size-p) (states nil states-p))
  "Propose SIZE, a list (width height), and STATES to TOPLEVEL's client.
Call on the server thread."
  (when size-p (setf (toplevel-size toplevel) size))
  (when states-p (setf (toplevel-states toplevel) states))
  (when (xdg-surface-configured-p (toplevel-xdg-surface toplevel))
    (send-configure (toplevel-xdg-surface toplevel))))

(defun close-toplevel (toplevel)
  "Ask TOPLEVEL's client to close it.  Call on the server thread."
  (post-event toplevel :close))

(defmethod role-committed ((toplevel toplevel) surface)
  (let ((mapped (and (surface-has-contents-p surface) t)))
    (unless (eq mapped (toplevel-mapped-p toplevel))
      (setf (toplevel-mapped-p toplevel) mapped)
      (server-log "~A ~:[unmapped~;mapped~]" toplevel mapped))
    (unless mapped
      ;; An unmapped toplevel must perform the initial configure afresh.
      (let ((xdg-surface (toplevel-xdg-surface toplevel)))
        (setf (xdg-surface-configured-p xdg-surface) nil
              (xdg-surface-configure-queued-p xdg-surface) nil)))))

(defmethod resource-destroyed ((toplevel toplevel))
  (setf (server-toplevels *server*) (remove toplevel (server-toplevels *server*))))

(define-request (toplevel toplevel :set-title) (title)
  (setf (toplevel-title toplevel) title))
(define-request (toplevel toplevel :set-app-id) (app-id)
  (setf (toplevel-app-id toplevel) app-id))
(define-request (toplevel toplevel :set-parent) (parent) (declare (ignore parent)))
(define-request (toplevel toplevel :set-min-size) (width height)
  (declare (ignore width height)))
(define-request (toplevel toplevel :set-max-size) (width height)
  (declare (ignore width height)))
(define-request (toplevel toplevel :set-maximized) ())
(define-request (toplevel toplevel :unset-maximized) ())
(define-request (toplevel toplevel :set-fullscreen) (output) (declare (ignore output)))
(define-request (toplevel toplevel :unset-fullscreen) ())
(define-request (toplevel toplevel :set-minimized) ())
(define-request (toplevel toplevel :move) (seat serial) (declare (ignore seat serial)))
(define-request (toplevel toplevel :resize) (seat serial edges)
  (declare (ignore seat serial edges)))
(define-request (toplevel toplevel :show-window-menu) (seat serial x y)
  (declare (ignore seat serial x y)))

;;; xdg_popup.

(defmethod send-role-configure ((popup popup))
  (apply #'post-event popup :configure (popup-geometry popup)))

(define-request (popup popup :grab) (seat serial) (declare (ignore seat serial)))

(define-request (popup popup :reposition) (positioner token)
  (setf (popup-geometry popup) (positioner-geometry positioner))
  (post-event popup :repositioned token)
  (send-configure (popup-xdg-surface popup)))

;;; Server-side decorations: in a world of quads, the compositor decorates.

(defclass decoration-manager (resource) ())
(defclass toplevel-decoration (resource)
  ((toplevel :initarg :toplevel :reader decoration-toplevel)))

(define-globals decoration-global ()
  (add-global "zxdg_decoration_manager_v1" 1
              (lambda (client version id)
                (make-resource 'decoration-manager client
                               "zxdg_decoration_manager_v1" version id))))

(define-request (manager decoration-manager :get-toplevel-decoration) (id toplevel)
  (let ((decoration (make-child-resource 'toplevel-decoration manager
                                         "zxdg_toplevel_decoration_v1" id
                                         :toplevel toplevel)))
    (setf (toplevel-decoration toplevel) decoration)))

(defun reconfigure-decoration (decoration)
  (let ((xdg-surface (toplevel-xdg-surface (decoration-toplevel decoration))))
    (if (xdg-surface-configured-p xdg-surface)
        (send-configure xdg-surface)
        (post-event decoration :configure :server-side))))

(define-request (decoration toplevel-decoration :set-mode) (mode)
  (declare (ignore mode))
  (reconfigure-decoration decoration))

(define-request (decoration toplevel-decoration :unset-mode) ()
  (reconfigure-decoration decoration))

(defmethod resource-destroyed ((decoration toplevel-decoration))
  (let ((toplevel (decoration-toplevel decoration)))
    (when (eq (toplevel-decoration toplevel) decoration)
      (setf (toplevel-decoration toplevel) nil))))
