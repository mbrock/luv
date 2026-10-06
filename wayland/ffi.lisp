;;;; The slice of libwayland-server's C ABI that the Lisp server uses.

(in-package #:luv.wayland)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:define-foreign-library libwayland-server
    (:unix (:or "libwayland-server.so.0" "libwayland-server.so"))))

(defvar *libwayland-server* nil)

(defun load-libwayland-server ()
  "Load libwayland-server once, by soname from the luv native library path."
  (or *libwayland-server*
      (setf *libwayland-server*
            (cffi:use-foreign-library libwayland-server))))

;;; Structures shared with libwayland.

(cffi:defcstruct wl-message
  (name :pointer)
  (signature :pointer)
  (types :pointer))

(cffi:defcstruct wl-interface
  (name :pointer)
  (version :int)
  (method-count :int)
  (methods :pointer)
  (event-count :int)
  (events :pointer))

(cffi:defcstruct wl-array
  (size :size)
  (alloc :size)
  (data :pointer))

(cffi:defcstruct wl-list
  (prev :pointer)
  (next :pointer))

(cffi:defcstruct wl-listener
  (link (:struct wl-list))
  (notify :pointer))

;;; union wl_argument is eight bytes on every platform libwayland supports.
(defconstant +argument-size+ 8)

;;; Display and event loop.

(cffi:defcfun ("wl_display_create" %display-create) :pointer)
(cffi:defcfun ("wl_display_destroy" %display-destroy) :void (display :pointer))
(cffi:defcfun ("wl_display_destroy_clients" %display-destroy-clients) :void
  (display :pointer))
(cffi:defcfun ("wl_display_get_event_loop" %display-get-event-loop) :pointer
  (display :pointer))
(cffi:defcfun ("wl_display_add_socket" %display-add-socket) :int
  (display :pointer) (name :string))
(cffi:defcfun ("wl_display_add_socket_auto" %display-add-socket-auto) :pointer
  (display :pointer))
(cffi:defcfun ("wl_display_flush_clients" %display-flush-clients) :void
  (display :pointer))
(cffi:defcfun ("wl_display_next_serial" %display-next-serial) :uint32
  (display :pointer))
(cffi:defcfun ("wl_display_init_shm" %display-init-shm) :int (display :pointer))
(cffi:defcfun ("wl_display_add_shm_format" %display-add-shm-format) :pointer
  (display :pointer) (format :uint32))
(cffi:defcfun ("wl_display_add_client_created_listener"
               %display-add-client-created-listener)
    :void
  (display :pointer) (listener :pointer))

(cffi:defcfun ("wl_event_loop_get_fd" %event-loop-get-fd) :int (loop :pointer))
(cffi:defcfun ("wl_event_loop_dispatch" %event-loop-dispatch) :int
  (loop :pointer) (timeout :int))
(cffi:defcfun ("wl_event_loop_dispatch_idle" %event-loop-dispatch-idle) :void
  (loop :pointer))
(cffi:defcfun ("wl_event_loop_add_fd" %event-loop-add-fd) :pointer
  (loop :pointer) (fd :int) (mask :uint32) (func :pointer) (data :pointer))
(cffi:defcfun ("wl_event_loop_add_idle" %event-loop-add-idle) :pointer
  (loop :pointer) (func :pointer) (data :pointer))
(cffi:defcfun ("wl_event_source_remove" %event-source-remove) :int
  (source :pointer))

(defconstant +event-readable+ #x01)

;;; Globals, clients, and resources.

(cffi:defcfun ("wl_global_create" %global-create) :pointer
  (display :pointer) (interface :pointer) (version :int) (data :pointer)
  (bind :pointer))
(cffi:defcfun ("wl_global_destroy" %global-destroy) :void (global :pointer))

(cffi:defcfun ("wl_client_get_credentials" %client-get-credentials) :void
  (client :pointer) (pid :pointer) (uid :pointer) (gid :pointer))
(cffi:defcfun ("wl_client_add_destroy_listener" %client-add-destroy-listener)
    :void
  (client :pointer) (listener :pointer))
(cffi:defcfun ("wl_client_destroy" %client-destroy) :void (client :pointer))

(cffi:defcfun ("wl_resource_create" %resource-create) :pointer
  (client :pointer) (interface :pointer) (version :int) (id :uint32))
(cffi:defcfun ("wl_resource_set_dispatcher" %resource-set-dispatcher) :void
  (resource :pointer) (dispatcher :pointer) (implementation :pointer)
  (data :pointer) (destroy :pointer))
(cffi:defcfun ("wl_resource_post_event_array" %resource-post-event-array) :void
  (resource :pointer) (opcode :uint32) (arguments :pointer))
(cffi:defcfun ("wl_resource_destroy" %resource-destroy) :void
  (resource :pointer))
(cffi:defcfun ("wl_resource_get_id" %resource-get-id) :uint32
  (resource :pointer))
(cffi:defcfun ("wl_resource_get_client" %resource-get-client) :pointer
  (resource :pointer))
(cffi:defcfun ("wl_resource_get_version" %resource-get-version) :int
  (resource :pointer))
(cffi:defcfun ("wl_resource_get_class" %resource-get-class) :string
  (resource :pointer))
(cffi:defcfun ("wl_resource_add_destroy_listener"
               %resource-add-destroy-listener)
    :void
  (resource :pointer) (listener :pointer))

(defun %resource-post-error (resource code message)
  (cffi:foreign-funcall "wl_resource_post_error"
                        :pointer resource :uint32 code
                        :string "%s" :string message :void))

;;; Shared-memory buffers, implemented by libwayland itself.

(cffi:defcfun ("wl_shm_buffer_get" %shm-buffer-get) :pointer
  (resource :pointer))
(cffi:defcfun ("wl_shm_buffer_begin_access" %shm-buffer-begin-access) :void
  (buffer :pointer))
(cffi:defcfun ("wl_shm_buffer_end_access" %shm-buffer-end-access) :void
  (buffer :pointer))
(cffi:defcfun ("wl_shm_buffer_get_data" %shm-buffer-get-data) :pointer
  (buffer :pointer))
(cffi:defcfun ("wl_shm_buffer_get_stride" %shm-buffer-get-stride) :int32
  (buffer :pointer))
(cffi:defcfun ("wl_shm_buffer_get_format" %shm-buffer-get-format) :uint32
  (buffer :pointer))
(cffi:defcfun ("wl_shm_buffer_get_width" %shm-buffer-get-width) :int32
  (buffer :pointer))
(cffi:defcfun ("wl_shm_buffer_get_height" %shm-buffer-get-height) :int32
  (buffer :pointer))

;;; Small pieces of libc.

(cffi:defcfun ("eventfd" %eventfd) :int (initial :uint) (flags :int))
(cffi:defcfun ("memfd_create" %memfd-create) :int (name :string) (flags :uint))
(cffi:defcfun ("close" %close) :int (fd :int))
(cffi:defcfun ("read" %read) :long (fd :int) (buffer :pointer) (count :size))
(cffi:defcfun ("write" %write) :long (fd :int) (buffer :pointer) (count :size))
(cffi:defcfun ("memcpy" %memcpy) :pointer
  (destination :pointer) (source :pointer) (count :size))

(defconstant +efd-cloexec+ #o2000000)
(defconstant +efd-nonblock+ #o4000)
(defconstant +mfd-cloexec+ #x0001)
