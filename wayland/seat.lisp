;;;; The seat: one keyboard and one pointer, fed by the host.
;;;;
;;;; libxkbcommon compiles the keymap clients receive and tracks modifier
;;;; state, so the compositor and its clients agree on what a key means.
;;;; Keys are Linux evdev codes, as wl_keyboard.key carries them.

(in-package #:luv.wayland)

(cffi:define-foreign-library libxkbcommon
  (:unix (:or "libxkbcommon.so.0" "libxkbcommon.so")))

(defvar *libxkbcommon* nil)

(defun load-libxkbcommon ()
  (or *libxkbcommon*
      (setf *libxkbcommon* (cffi:use-foreign-library libxkbcommon))))

(cffi:defcstruct xkb-rule-names
  (rules :pointer)
  (model :pointer)
  (layout :pointer)
  (variant :pointer)
  (options :pointer))

(cffi:defcfun ("xkb_context_new" %xkb-context-new) :pointer (flags :int))
(cffi:defcfun ("xkb_context_unref" %xkb-context-unref) :void (context :pointer))
(cffi:defcfun ("xkb_keymap_new_from_names" %xkb-keymap-new-from-names) :pointer
  (context :pointer) (names :pointer) (flags :int))
(cffi:defcfun ("xkb_keymap_get_as_string" %xkb-keymap-get-as-string) :pointer
  (keymap :pointer) (format :int))
(cffi:defcfun ("xkb_keymap_unref" %xkb-keymap-unref) :void (keymap :pointer))
(cffi:defcfun ("xkb_state_new" %xkb-state-new) :pointer (keymap :pointer))
(cffi:defcfun ("xkb_state_update_key" %xkb-state-update-key) :int
  (state :pointer) (keycode :uint32) (direction :int))
(cffi:defcfun ("xkb_state_serialize_mods" %xkb-state-serialize-mods) :uint32
  (state :pointer) (components :int))
(cffi:defcfun ("xkb_state_serialize_layout" %xkb-state-serialize-layout) :uint32
  (state :pointer) (components :int))

(defconstant +xkb-keymap-format-text-v1+ 1)
(defconstant +xkb-state-mods-depressed+ 1)
(defconstant +xkb-state-mods-latched+ 2)
(defconstant +xkb-state-mods-locked+ 4)
(defconstant +xkb-state-layout-effective+ 128)

(defparameter *default-keymap*
  '(:layout "us" :variant "dvorak" :options "ctrl:nocaps,compose:ralt")
  "The XKB names clients are given; the author's niri configuration.")

(defparameter *repeat-rate* 18)
(defparameter *repeat-delay* 200)

(defclass seat ()
  ((keymap-text :accessor seat-keymap-text)
   (xkb-state :accessor seat-xkb-state)
   (keyboards :initform '() :accessor seat-keyboards)
   (pointers :initform '() :accessor seat-pointers)
   (keyboard-focus :initform nil :accessor keyboard-focus)
   (pointer-focus :initform nil :accessor pointer-focus)
   (pressed-keys :initform '() :accessor seat-pressed-keys)
   (modifiers :initform '(0 0 0 0) :accessor seat-modifiers)))

(defun compile-keymap (names)
  "The keymap text and a fresh xkb_state for NAMES, a plist of XKB names."
  (load-libxkbcommon)
  (let ((context (%xkb-context-new 0))
        (strings '()))
    (when (cffi:null-pointer-p context)
      (error "xkb_context_new failed."))
    (unwind-protect
         (cffi:with-foreign-object (rule-names '(:struct xkb-rule-names))
           (flet ((field (key)
                    (let ((value (getf names key)))
                      (if value
                          (let ((pointer (cffi:foreign-string-alloc value)))
                            (push pointer strings)
                            pointer)
                          (cffi:null-pointer)))))
             (setf (cffi:foreign-slot-value rule-names '(:struct xkb-rule-names) 'rules)
                   (field :rules)
                   (cffi:foreign-slot-value rule-names '(:struct xkb-rule-names) 'model)
                   (field :model)
                   (cffi:foreign-slot-value rule-names '(:struct xkb-rule-names) 'layout)
                   (field :layout)
                   (cffi:foreign-slot-value rule-names '(:struct xkb-rule-names) 'variant)
                   (field :variant)
                   (cffi:foreign-slot-value rule-names '(:struct xkb-rule-names) 'options)
                   (field :options)))
           (let ((keymap (%xkb-keymap-new-from-names context rule-names 0)))
             (when (cffi:null-pointer-p keymap)
               (error "XKB could not compile the keymap ~S." names))
             (let* ((text-pointer (%xkb-keymap-get-as-string
                                   keymap +xkb-keymap-format-text-v1+))
                    (text (cffi:foreign-string-to-lisp text-pointer)))
               (cffi:foreign-free text-pointer)
               ;; The state holds its own reference to the keymap.
               (values text (prog1 (%xkb-state-new keymap)
                              (%xkb-keymap-unref keymap))))))
      (mapc #'cffi:foreign-free strings)
      (%xkb-context-unref context))))

(defun keymap-fd (text)
  "A sealed-enough memfd holding TEXT and its terminating NUL, and its size."
  (let* ((octets (sb-ext:string-to-octets text :external-format :utf-8
                                               :null-terminate t))
         (fd (%memfd-create "luv-keymap" +mfd-cloexec+)))
    (when (minusp fd)
      (error "memfd_create failed for the keymap."))
    (sb-sys:with-pinned-objects (octets)
      (%write fd (sb-sys:vector-sap octets) (length octets)))
    (values fd (length octets))))

(define-globals seat-global ()
  (let ((seat (make-instance 'seat)))
    (multiple-value-bind (text state)
        (compile-keymap (or (server-keymap-names *server*) *default-keymap*))
      (setf (seat-keymap-text seat) text
            (seat-xkb-state seat) state))
    (setf (server-seat *server*) seat)
    (add-global "wl_seat" 5
                (lambda (client version id)
                  (let ((resource (make-resource 'seat-resource client "wl_seat"
                                                 version id)))
                    (post-event resource :capabilities 3)
                    (post-event resource :name "luvland"))))))

(defclass seat-resource (resource) ())
(defclass keyboard (resource) ())
(defclass pointer (resource) ())

(defun seat () (server-seat *server*))

(define-request (resource seat-resource :get-keyboard) (id)
  (let ((keyboard (make-child-resource 'keyboard resource "wl_keyboard" id))
        (seat (seat)))
    (multiple-value-bind (fd size) (keymap-fd (seat-keymap-text seat))
      (unwind-protect (post-event keyboard :keymap 1 fd size)
        (%close fd)))
    (post-event keyboard :repeat-info *repeat-rate* *repeat-delay*)
    (push keyboard (seat-keyboards seat))
    ;; A keyboard created while its client's surface has focus must hear so.
    (let ((focus (keyboard-focus seat)))
      (when (and focus (eq (resource-client focus) (resource-client keyboard)))
        (send-keyboard-enter keyboard focus)))
    keyboard))

(define-request (resource seat-resource :get-pointer) (id)
  (let ((pointer (make-child-resource 'pointer resource "wl_pointer" id)))
    (push pointer (seat-pointers (seat)))
    pointer))

(define-request (resource seat-resource :get-touch) (id)
  (make-child-resource 'resource resource "wl_touch" id))

(define-request (pointer pointer :set-cursor) (serial surface x y)
  (declare (ignore serial surface x y)))

(defmethod resource-destroyed ((keyboard keyboard))
  (setf (seat-keyboards (seat)) (remove keyboard (seat-keyboards (seat)))))

(defmethod resource-destroyed ((pointer pointer))
  (setf (seat-pointers (seat)) (remove pointer (seat-pointers (seat)))))

(defmethod resource-destroyed :after ((surface surface))
  (let ((seat (seat)))
    (when seat
      (when (eq (keyboard-focus seat) surface)
        (setf (keyboard-focus seat) nil))
      (when (eq (pointer-focus seat) surface)
        (setf (pointer-focus seat) nil)))))

(defun client-resources (resources surface)
  "Those of RESOURCES that belong to SURFACE's client."
  (when surface
    (let ((client (resource-client surface)))
      (remove-if-not (lambda (resource) (eq (resource-client resource) client))
                     resources))))

;;; Keyboard.

(defun send-keyboard-enter (keyboard surface)
  (let ((seat (seat)))
    (post-event keyboard :enter (next-serial) surface
                (uint32-array (seat-pressed-keys seat)))
    (apply #'post-event keyboard :modifiers (next-serial) (seat-modifiers seat))))

(defun focus-keyboard (surface)
  "Move keyboard focus to SURFACE, or to nothing.  Call on the server thread."
  (let* ((seat (seat))
         (old (keyboard-focus seat)))
    (unless (eq old surface)
      (when (and old (resource-live-p old))
        (dolist (keyboard (client-resources (seat-keyboards seat) old))
          (post-event keyboard :leave (next-serial) old)))
      (setf (keyboard-focus seat) surface)
      (when surface
        (dolist (keyboard (client-resources (seat-keyboards seat) surface))
          (send-keyboard-enter keyboard surface))))
    surface))

(defun update-modifiers (seat keycode pressed-p)
  "Advance XKB state; return true when the serialized modifiers changed."
  (let ((state (seat-xkb-state seat)))
    (%xkb-state-update-key state (+ keycode 8) (if pressed-p 1 0))
    (let ((modifiers (list (%xkb-state-serialize-mods state +xkb-state-mods-depressed+)
                           (%xkb-state-serialize-mods state +xkb-state-mods-latched+)
                           (%xkb-state-serialize-mods state +xkb-state-mods-locked+)
                           (%xkb-state-serialize-layout state +xkb-state-layout-effective+))))
      (unless (equal modifiers (seat-modifiers seat))
        (setf (seat-modifiers seat) modifiers)
        t))))

(defun event-time-ms ()
  (ldb (byte 32 0) (floor (* 1000 (get-internal-real-time))
                          internal-time-units-per-second)))

(defun send-key (keycode pressed-p)
  "Deliver evdev KEYCODE to the focused surface.  Call on the server thread."
  (let ((seat (seat)))
    (if pressed-p
        (pushnew keycode (seat-pressed-keys seat))
        (setf (seat-pressed-keys seat) (remove keycode (seat-pressed-keys seat))))
    (let ((changed (update-modifiers seat keycode pressed-p))
          (keyboards (client-resources (seat-keyboards seat) (keyboard-focus seat))))
      (dolist (keyboard keyboards)
        (post-event keyboard :key (next-serial) (event-time-ms) keycode
                    (if pressed-p 1 0))
        (when changed
          (apply #'post-event keyboard :modifiers (next-serial)
                 (seat-modifiers seat)))))))

;;; Pointer.

(defun pointer-frame (pointers)
  (dolist (pointer pointers)
    (post-event pointer :frame)))

(defun focus-pointer (surface x y)
  "Move pointer focus to SURFACE at surface-local X, Y, or to nothing."
  (let* ((seat (seat))
         (old (pointer-focus seat)))
    (unless (eq old surface)
      (when (and old (resource-live-p old))
        (let ((pointers (client-resources (seat-pointers seat) old)))
          (dolist (pointer pointers)
            (post-event pointer :leave (next-serial) old))
          (pointer-frame pointers)))
      (setf (pointer-focus seat) surface)
      (when surface
        (let ((pointers (client-resources (seat-pointers seat) surface)))
          (dolist (pointer pointers)
            (post-event pointer :enter (next-serial) surface x y))
          (pointer-frame pointers))))
    surface))

(defun send-pointer-motion (x y)
  (let ((pointers (client-resources (seat-pointers (seat)) (pointer-focus (seat)))))
    (dolist (pointer pointers)
      (post-event pointer :motion (event-time-ms) x y))
    (pointer-frame pointers)))

(defun send-pointer-button (button pressed-p)
  "BUTTON is an evdev code: #x110 left, #x111 right, #x112 middle."
  (let ((pointers (client-resources (seat-pointers (seat)) (pointer-focus (seat)))))
    (dolist (pointer pointers)
      (post-event pointer :button (next-serial) (event-time-ms) button
                  (if pressed-p 1 0)))
    (pointer-frame pointers)))

(defun send-pointer-axis (dx dy)
  "Scroll by DX, DY in surface-local units."
  (let ((pointers (client-resources (seat-pointers (seat)) (pointer-focus (seat)))))
    (dolist (pointer pointers)
      (unless (zerop dy) (post-event pointer :axis (event-time-ms) 0 dy))
      (unless (zerop dx) (post-event pointer :axis (event-time-ms) 1 dx)))
    (pointer-frame pointers)))
