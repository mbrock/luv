;;;; The server: one libwayland display on one Lisp thread.
;;;;
;;;; Every resource is created with the same dispatcher.  It decodes the
;;;; request's arguments from the Lisp protocol description and calls
;;;; HANDLE-REQUEST on the resource's object, so an interface is implemented by
;;;; defining methods rather than by filling C vtables.

(in-package #:luv.wayland)

(defvar *server* nil
  "The server whose thread is running, bound on that thread.")

(defclass server ()
  ((display :initform nil :accessor server-display)
   (event-loop :initform nil :accessor server-event-loop)
   (thread :initform nil :accessor server-thread)
   (socket-name :initform nil :accessor server-socket-name)
   (running-p :initform nil :accessor server-running-p)
   (resources :initform (make-hash-table) :reader server-resources
              :documentation "Lisp resources by the address of their wl_resource.")
   (globals :initform (make-hash-table) :reader server-globals)
   (clients :initform (make-hash-table) :reader server-clients-table)
   (listeners :initform (make-hash-table) :reader server-listeners
              :documentation "Lisp closures by the address of their wl_listener.")
   (next-key :initform 0 :accessor server-next-key)
   (tasks :initform '() :accessor server-tasks)
   (task-lock :initform (sb-thread:make-mutex :name "wayland server tasks")
              :reader server-task-lock)
   (wakeup-fd :initform -1 :accessor server-wakeup-fd)
   (surfaces :initform '() :accessor server-surfaces)
   (toplevels :initform '() :accessor server-toplevels)
   (seat :initform nil :accessor server-seat)
   (log :initform '() :accessor server-log-entries)
   (failures :initform '() :accessor server-failures)
   (keymap :initarg :keymap :initform nil :reader server-keymap-names)
   (output-size :initarg :output-size :initform '(1920 1080)
                :accessor server-output-size)
   (initial-toplevel-size :initarg :initial-toplevel-size :initform nil
                          :accessor server-initial-toplevel-size
                          :documentation "The (width height) proposed to new windows,
or NIL to let each client choose.")))

(defmethod print-object ((server server) stream)
  (print-unreadable-object (server stream :type t)
    (format stream "~A ~:[stopped~;running~] ~D client~:P"
            (server-socket-name server) (server-running-p server)
            (hash-table-count (server-clients-table server)))))

(defun server-clients (&optional (server *server*))
  (loop for client being the hash-values of (server-clients-table server)
        collect client))

(defun server-log (format-control &rest arguments)
  "Remember a line of server activity; the newest 256 are kept."
  (let ((server *server*)
        (line (apply #'format nil format-control arguments)))
    (when server
      (let ((entries (cons line (server-log-entries server))))
        (setf (server-log-entries server)
              (if (> (length entries) 256) (subseq entries 0 256) entries))))
    line))

(defun next-serial ()
  (%display-next-serial (server-display *server*)))

(defun allocate-key (server)
  (incf (server-next-key server)))

;;; Failures inside callbacks.  A Lisp error must not unwind through
;;; libwayland's C frames; the condition and its backtrace are kept, and the
;;; offending client is told it met an implementation error.

(defun record-failure (where condition)
  (let ((backtrace (with-output-to-string (out)
                     (sb-debug:print-backtrace :stream out :count 40))))
    (push (list :where where :condition condition :backtrace backtrace
                :time (get-universal-time))
          (server-failures *server*))
    (server-log "failure in ~A: ~A" where condition)))

(defmacro guarding-callback ((where &key on-failure) &body body)
  (let ((block (gensym "GUARD")))
    `(block ,block
       (handler-bind ((serious-condition
                        (lambda (condition)
                          (record-failure ,where condition)
                          ,@(when on-failure `((funcall ,on-failure condition)))
                          (return-from ,block nil))))
         ,@body))))

;;; Clients.

(defclass client ()
  ((pointer :initarg :pointer :reader client-pointer)
   (pid :initarg :pid :reader client-pid)
   (listener :initarg :listener :accessor client-listener)))

(defmethod print-object ((client client) stream)
  (print-unreadable-object (client stream :type t)
    (format stream "pid ~D" (client-pid client))))

(defun pointer-key (pointer)
  (cffi:pointer-address pointer))

(defun find-client (pointer)
  (gethash (pointer-key pointer) (server-clients-table *server*)))

(defun make-listener (function)
  "A wl_listener whose notification calls FUNCTION with the signal's data."
  (let ((listener (cffi:foreign-alloc '(:struct wl-listener))))
    (setf (cffi:foreign-slot-value listener '(:struct wl-listener) 'notify)
          (cffi:callback listener-notified))
    (setf (gethash (pointer-key listener) (server-listeners *server*)) function)
    listener))

(defun free-listener (listener)
  (remhash (pointer-key listener) (server-listeners *server*))
  (cffi:foreign-free listener))

(cffi:defcallback listener-notified :void ((listener :pointer) (data :pointer))
  (let ((function (gethash (pointer-key listener) (server-listeners *server*))))
    (when function
      (guarding-callback ("listener")
        (funcall function data)))))

(defun client-created (pointer)
  (let ((pid (cffi:with-foreign-objects ((pid :int) (uid :uint) (gid :uint))
               (%client-get-credentials pointer pid uid gid)
               (cffi:mem-ref pid :int)))
        (client nil))
    (setf client (make-instance 'client :pointer pointer :pid pid))
    (let ((listener nil))
      (setf listener (make-listener
                      (lambda (data)
                        (declare (ignore data))
                        (server-log "client ~D disconnected" pid)
                        (remhash (pointer-key pointer) (server-clients-table *server*))
                        (free-listener listener))))
      (setf (client-listener client) listener)
      (%client-add-destroy-listener pointer listener))
    (setf (gethash (pointer-key pointer) (server-clients-table *server*)) client)
    (server-log "client ~D connected" pid)
    client))

;;; Resources.

(defclass resource ()
  ((pointer :initarg :pointer :reader resource-pointer)
   (interface :initarg :interface :reader resource-interface)
   (client :initarg :client :reader resource-client)
   (version :initarg :version :reader resource-version)
   (id :initarg :id :reader resource-id)
   (live-p :initform t :accessor resource-live-p)))

(defmethod print-object ((resource resource) stream)
  (print-unreadable-object (resource stream :type t)
    (format stream "~A@~D v~D~:[ dead~;~]"
            (interface-name (resource-interface resource))
            (resource-id resource) (resource-version resource)
            (resource-live-p resource))))

(defclass foreign-resource ()
  ((pointer :initarg :pointer :reader resource-pointer)
   (class-name :initarg :class-name :reader foreign-resource-class))
  (:documentation "A resource implemented inside libwayland, such as an shm wl_buffer."))

(defmethod print-object ((resource foreign-resource) stream)
  (print-unreadable-object (resource stream :type t)
    (format stream "~A" (foreign-resource-class resource))))

(defun make-resource (class client interface-name version id &rest initargs)
  "Create a wl_resource for CLIENT whose requests reach an instance of CLASS."
  (let* ((interface (find-interface interface-name))
         (pointer (%resource-create (client-pointer client)
                                    (interface-pointer interface)
                                    version id)))
    (when (cffi:null-pointer-p pointer)
      (error "wl_resource_create failed for ~A@~D." interface-name id))
    (let ((resource (apply #'make-instance class
                           :pointer pointer :interface interface
                           :client client :version version :id id
                           initargs)))
      (%resource-set-dispatcher pointer
                                (cffi:callback dispatch-request)
                                ;; libwayland only needs a non-NULL
                                ;; implementation; the dispatcher ignores it.
                                (cffi:callback dispatch-request)
                                (cffi:null-pointer)
                                (cffi:callback resource-destroyed))
      (setf (gethash (pointer-key pointer) (server-resources *server*)) resource)
      resource)))

(defun make-child-resource (class parent interface-name id &rest initargs)
  "A resource created by a request on PARENT, inheriting its version."
  (apply #'make-resource class (resource-client parent) interface-name
         (resource-version parent) id initargs))

(defun destroy-resource (resource)
  (when (resource-live-p resource)
    (%resource-destroy (resource-pointer resource))))

(defgeneric resource-destroyed (resource)
  (:documentation "Called once when RESOURCE's wl_resource is destroyed,
whether by a destructor request, by the server, or by client disconnection.")
  (:method ((resource resource)) nil))

(cffi:defcallback resource-destroyed :void ((pointer :pointer))
  (let ((resource (gethash (pointer-key pointer) (server-resources *server*))))
    (remhash (pointer-key pointer) (server-resources *server*))
    (when resource
      (setf (resource-live-p resource) nil)
      (guarding-callback ("resource destruction")
        (resource-destroyed resource)))))

(defun find-resource (pointer)
  (unless (cffi:null-pointer-p pointer)
    (or (gethash (pointer-key pointer) (server-resources *server*))
        (make-instance 'foreign-resource
                       :pointer pointer
                       :class-name (%resource-get-class pointer)))))

(defun post-error (resource code message)
  (server-log "protocol error on ~A: ~A" resource message)
  (%resource-post-error (resource-pointer resource) code message))

;;; Requests.

(defgeneric handle-request (resource request &rest arguments)
  (:documentation "Handle REQUEST, a keyword such as :ATTACH, sent to RESOURCE.

Arguments arrive decoded: integers, rationals for fixed-point numbers, Lisp
strings, Lisp resources (or NIL) for objects, plain integer ids for new_id,
octet vectors for arrays, and file descriptors the method now owns.")
  (:method ((resource resource) request &rest arguments)
    (declare (ignore arguments))
    (server-log "unhandled ~A.~(~A~)"
                (interface-name (resource-interface resource)) request)))

(defmacro define-request ((variable class request) lambda-list &body body)
  "Define how instances of CLASS handle REQUEST, binding the resource to
VARIABLE and the decoded arguments by LAMBDA-LIST."
  (let ((arguments (gensym "ARGUMENTS"))
        (name (gensym "REQUEST")))
    `(defmethod handle-request ((,variable ,class) (,name (eql ,request))
                                &rest ,arguments)
       (declare (ignorable ,variable))
       (destructuring-bind ,lambda-list ,arguments
         ,@body))))

(defun decode-argument (argument slot)
  (ecase (argument-type argument)
    (:int (cffi:mem-ref slot :int32))
    (:uint (cffi:mem-ref slot :uint32))
    (:fixed (/ (cffi:mem-ref slot :int32) 256))
    (:string (let ((pointer (cffi:mem-ref slot :pointer)))
               (unless (cffi:null-pointer-p pointer)
                 (cffi:foreign-string-to-lisp pointer))))
    (:object (find-resource (cffi:mem-ref slot :pointer)))
    (:new-id (cffi:mem-ref slot :uint32))
    (:array (let ((array (cffi:mem-ref slot :pointer)))
              (if (cffi:null-pointer-p array)
                  (make-array 0 :element-type '(unsigned-byte 8))
                  (cffi:with-foreign-slots ((size data) array (:struct wl-array))
                    (let ((octets (make-array size :element-type '(unsigned-byte 8))))
                      (dotimes (index size octets)
                        (setf (aref octets index)
                              (cffi:mem-aref data :uint8 index))))))))
    (:fd (cffi:mem-ref slot :int32))))

(defun decode-arguments (message arguments)
  (loop for argument across (message-arguments message)
        for index from 0
        collect (decode-argument argument
                                 (cffi:inc-pointer arguments
                                                   (* index +argument-size+)))))

(cffi:defcallback dispatch-request :int
    ((implementation :pointer) (target :pointer) (opcode :uint32)
     (wl-message :pointer) (arguments :pointer))
  (declare (ignore implementation wl-message))
  (let ((resource (gethash (pointer-key target) (server-resources *server*))))
    (when resource
      (let ((message (aref (interface-requests (resource-interface resource))
                           opcode)))
        (guarding-callback ((format nil "~A.~(~A~)"
                                    (interface-name (resource-interface resource))
                                    (message-name message))
                            :on-failure
                            (lambda (condition)
                              (when (resource-live-p resource)
                                (cffi:foreign-funcall
                                 "wl_client_post_implementation_error"
                                 :pointer (client-pointer (resource-client resource))
                                 :string "%s"
                                 :string (princ-to-string condition)
                                 :void))))
          (apply #'handle-request resource (message-name message)
                 (decode-arguments message arguments))
          (when (and (message-destructor-p message) (resource-live-p resource))
            (destroy-resource resource))))))
  0)

;;; Events.

(defun encode-argument (resource argument value slot cleanups)
  "Store VALUE for ARGUMENT in the union at SLOT; return updated CLEANUPS."
  (ecase (argument-type argument)
    ((:int :uint)
     (let ((integer (if (and (keywordp value) (argument-enum argument))
                        (enum-value (resource-interface resource)
                                    (argument-enum argument) value)
                        value)))
       (if (eq (argument-type argument) :int)
           (setf (cffi:mem-ref slot :int32) integer)
           (setf (cffi:mem-ref slot :uint32) integer))))
    (:fixed (setf (cffi:mem-ref slot :int32) (round (* value 256))))
    (:string (let ((pointer (if value
                                (cffi:foreign-string-alloc value)
                                (cffi:null-pointer))))
               (setf (cffi:mem-ref slot :pointer) pointer)
               (unless (cffi:null-pointer-p pointer)
                 (push pointer cleanups))))
    ((:object :new-id)
     (setf (cffi:mem-ref slot :pointer)
           (if value (resource-pointer value) (cffi:null-pointer))))
    (:array
     (let* ((octets (coerce value '(vector (unsigned-byte 8))))
            (array (cffi:foreign-alloc '(:struct wl-array)))
            (data (cffi:foreign-alloc :uint8 :count (max 1 (length octets)))))
       (dotimes (index (length octets))
         (setf (cffi:mem-aref data :uint8 index) (aref octets index)))
       (setf (cffi:foreign-slot-value array '(:struct wl-array) 'size) (length octets)
             (cffi:foreign-slot-value array '(:struct wl-array) 'alloc) (length octets)
             (cffi:foreign-slot-value array '(:struct wl-array) 'data) data)
       (setf (cffi:mem-ref slot :pointer) array)
       (push data cleanups)
       (push array cleanups)))
    (:fd (setf (cffi:mem-ref slot :int32) value)))
  cleanups)

(defun post-event (resource event &rest values)
  "Send EVENT, a keyword such as :CONFIGURE, on RESOURCE with VALUES.

Events newer than the version the client bound are not sent, and nothing is
sent to a destroyed resource.  Returns true when the event was posted."
  (when (resource-live-p resource)
    (let* ((interface (resource-interface resource))
           (message (find-event interface event))
           (arguments (message-arguments message)))
      (unless (= (length values) (length arguments))
        (error "~A.~(~A~) takes ~D arguments, not ~D."
               (interface-name interface) event (length arguments) (length values)))
      (when (<= (message-since message) (resource-version resource))
        (let ((cleanups '())
              (buffer (cffi:foreign-alloc :uint8
                                          :count (* (max 1 (length arguments))
                                                    +argument-size+))))
          (unwind-protect
               (progn
                 (loop for argument across arguments
                       for value in values
                       for index from 0
                       do (setf cleanups
                                (encode-argument resource argument value
                                                 (cffi:inc-pointer
                                                  buffer (* index +argument-size+))
                                                 cleanups)))
                 (%resource-post-event-array (resource-pointer resource)
                                             (message-opcode message) buffer)
                 t)
            (mapc #'cffi:foreign-free cleanups)
            (cffi:foreign-free buffer)))))))

(defun uint32-array (values)
  "Octets of VALUES as native 32-bit words, for a wl_array argument."
  (let ((octets (make-array (* 4 (length values)) :element-type '(unsigned-byte 8))))
    (loop for value in values
          for offset from 0 by 4
          do (setf (aref octets offset) (ldb (byte 8 0) value)
                   (aref octets (+ offset 1)) (ldb (byte 8 8) value)
                   (aref octets (+ offset 2)) (ldb (byte 8 16) value)
                   (aref octets (+ offset 3)) (ldb (byte 8 24) value)))
    octets))

;;; Globals.

(defstruct (global (:constructor make-global (interface-name version bind)))
  interface-name version bind pointer)

(defun add-global (interface-name version bind)
  "Advertise INTERFACE-NAME at VERSION.  BIND is called with the client, the
version it asked for, and the new object's id, and must create the resource."
  (let* ((server *server*)
         (key (allocate-key server))
         (global (make-global interface-name version bind)))
    (setf (gethash key (server-globals server)) global
          (global-pointer global)
          (%global-create (server-display server)
                          (interface-pointer (find-interface interface-name))
                          version (cffi:make-pointer key)
                          (cffi:callback global-bound)))
    global))

(cffi:defcallback global-bound :void
    ((client :pointer) (data :pointer) (version :uint32) (id :uint32))
  (let ((global (gethash (cffi:pointer-address data) (server-globals *server*))))
    (guarding-callback ((format nil "bind ~A" (global-interface-name global)))
      (funcall (global-bind global)
               (or (find-client client) (client-created client))
               version id))))

;;; Tasks from other threads.

(defun wake-server (server)
  (cffi:with-foreign-object (one :uint64)
    (setf (cffi:mem-ref one :uint64) 1)
    (%write (server-wakeup-fd server) one 8)))

(cffi:defcallback wakeup-readable :int ((fd :int) (mask :uint32) (data :pointer))
  (declare (ignore mask data))
  (cffi:with-foreign-object (count :uint64)
    (%read fd count 8))
  (run-server-tasks *server*)
  0)

(defun run-server-tasks (server)
  (let ((tasks (sb-thread:with-mutex ((server-task-lock server))
                 (shiftf (server-tasks server) '()))))
    (dolist (task (reverse tasks))
      (guarding-callback ("server task")
        (funcall task)))))

(defun call-in-server (function &key (server *server*) (wait t) (timeout 5))
  "Run FUNCTION on SERVER's thread.  With WAIT, return its values, or
re-signal its error here; otherwise return at once."
  (cond
    ((eq sb-thread:*current-thread* (server-thread server))
     (funcall function))
    ((not wait)
     (sb-thread:with-mutex ((server-task-lock server))
       (push function (server-tasks server)))
     (wake-server server)
     nil)
    (t
     (let ((semaphore (sb-thread:make-semaphore))
           (values nil)
           (failure nil))
       (sb-thread:with-mutex ((server-task-lock server))
         (push (lambda ()
                 (handler-case (setf values (multiple-value-list (funcall function)))
                   (serious-condition (condition) (setf failure condition)))
                 (sb-thread:signal-semaphore semaphore))
               (server-tasks server)))
       (wake-server server)
       (unless (sb-thread:wait-on-semaphore semaphore :timeout timeout)
         (error "The Wayland server did not run a task within ~D s." timeout))
       (when failure (error failure))
       (values-list values)))))

(defun defer (function)
  "Run FUNCTION on this server's thread after the current batch of requests,
as niri queues an initial configure behind the rest of a client's commit."
  (call-in-server function :wait nil))

;;; Starting and stopping.

(defvar *global-installers* '()
  "Names of functions that advertise globals, in the order they were defined.")

(defmacro define-globals (name () &body body)
  "Define NAME to advertise some globals on the current server's thread, and
have every server started from now on call it."
  `(progn
     (defun ,name () ,@body)
     (unless (member ',name *global-installers*)
       (setf *global-installers* (append *global-installers* (list ',name))))
     ',name))

(defun install-globals (server)
  (declare (ignore server))
  (mapc #'funcall *global-installers*))

(defun open-display (server socket-name)
  (load-libwayland-server)
  (let ((display (%display-create)))
    (when (cffi:null-pointer-p display)
      (error "wl_display_create failed."))
    (setf (server-display server) display
          (server-event-loop server) (%display-get-event-loop display))
    (let ((name (if socket-name
                    (if (zerop (%display-add-socket display socket-name))
                        socket-name
                        (error "Could not listen on Wayland socket ~S." socket-name))
                    (let ((pointer (%display-add-socket-auto display)))
                      (when (cffi:null-pointer-p pointer)
                        (error "Could not create a Wayland socket."))
                      (cffi:foreign-string-to-lisp pointer)))))
      (setf (server-socket-name server) name))
    (setf (server-wakeup-fd server) (%eventfd 0 (logior +efd-cloexec+ +efd-nonblock+)))
    (%event-loop-add-fd (server-event-loop server) (server-wakeup-fd server)
                        +event-readable+ (cffi:callback wakeup-readable)
                        (cffi:null-pointer))
    (%display-add-client-created-listener
     display (make-listener (lambda (client)
                              (unless (find-client client)
                                (client-created client)))))
    (unless (zerop (%display-init-shm display))
      (error "wl_display_init_shm failed."))
    (install-globals server)))

(defun close-display (server)
  (let ((display (server-display server)))
    (when display
      (%display-destroy-clients display)
      (%display-destroy display)
      (setf (server-display server) nil)))
  (when (>= (server-wakeup-fd server) 0)
    (%close (server-wakeup-fd server))
    (setf (server-wakeup-fd server) -1))
  (loop for listener being the hash-keys of (server-listeners server)
        do (cffi:foreign-free (cffi:make-pointer listener)))
  (clrhash (server-listeners server)))

(defun serve (server ready)
  (let ((*server* server))
    (handler-case (open-display server (getf ready :socket-name))
      (serious-condition (condition)
        (setf (getf ready :failure) condition)
        (sb-thread:signal-semaphore (getf ready :semaphore))
        (close-display server)
        (return-from serve)))
    (setf (server-running-p server) t)
    (sb-thread:signal-semaphore (getf ready :semaphore))
    (unwind-protect
         (let ((fd (%event-loop-get-fd (server-event-loop server))))
           (loop while (server-running-p server)
                 do (sb-sys:wait-until-fd-usable fd :input 1)
                    (%event-loop-dispatch (server-event-loop server) 0)
                    (%display-flush-clients (server-display server))))
      (setf (server-running-p server) nil)
      (close-display server))))

(defun start-server (&key (class 'server) socket-name initargs)
  "Start a Wayland server on its own thread and return it once it listens."
  (let* ((server (apply #'make-instance class initargs))
         (ready (list :socket-name socket-name
                      :semaphore (sb-thread:make-semaphore)
                      :failure nil)))
    (setf (server-thread server)
          (sb-thread:make-thread #'serve :name "luv Wayland server"
                                         :arguments (list server ready)))
    (sb-thread:wait-on-semaphore (getf ready :semaphore))
    (when (getf ready :failure)
      (sb-thread:join-thread (server-thread server) :default nil)
      (error (getf ready :failure)))
    server))

(defun stop-server (server)
  (when (server-running-p server)
    (call-in-server (lambda () (setf (server-running-p server) nil))
                    :server server :wait nil)
    (sb-thread:join-thread (server-thread server) :default nil :timeout 5))
  server)
