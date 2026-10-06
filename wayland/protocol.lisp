;;;; Wayland protocol descriptions, read from the vendored XML.
;;;;
;;;; The Lisp description is made at load time and survives a saved image.  The
;;;; C tables libwayland reads (struct wl_interface and wl_message) are made on
;;;; first use in each process; the core interfaces are libwayland's own
;;;; exported structures.

(in-package #:luv.wayland)

(defstruct (interface (:constructor %make-interface))
  (name "" :type string)
  (version 1 :type fixnum)
  (requests #() :type simple-vector)
  (events #() :type simple-vector)
  (enums '() :type list)
  (%pointer nil))

(defstruct (message (:constructor %make-message))
  (name nil :type keyword)
  (opcode 0 :type fixnum)
  (since 1 :type fixnum)
  (destructor-p nil)
  (arguments #() :type simple-vector))

(defstruct (argument (:constructor %make-argument))
  (name nil :type keyword)
  (type nil :type keyword)
  (interface-name nil)
  (nullable nil)
  (enum nil))

(defmethod print-object ((interface interface) stream)
  (print-unreadable-object (interface stream :type t)
    (format stream "~A v~D" (interface-name interface)
            (interface-version interface))))

(defmethod print-object ((message message) stream)
  (print-unreadable-object (message stream :type t)
    (format stream "~A #~D" (message-name message) (message-opcode message))))

(defvar *interfaces* (make-hash-table :test 'equal)
  "Every known interface, by its protocol name such as \"wl_surface\".")

(defparameter *protocol-files*
  '("wayland.xml" "xdg-shell.xml" "xdg-decoration-unstable-v1.xml"
    "viewporter.xml" "presentation-time.xml" "linux-dmabuf-v1.xml"))

(defun protocol-directory ()
  (asdf:system-relative-pathname "luv/wayland" "wayland/protocols/"))

(defun find-interface (name &optional (errorp t))
  (or (gethash name *interfaces*)
      (when errorp
        (error "Unknown Wayland interface ~S." name))))

(defun protocol-keyword (name)
  (intern (substitute #\- #\_ (string-upcase name)) :keyword))

;;; Reading XML.

(defun element-children (element tag)
  (loop for node across (dom:child-nodes element)
        when (and (dom:element-p node) (string= (dom:tag-name node) tag))
          collect node))

(defun attribute (element name)
  (let ((value (dom:get-attribute element name)))
    (if (and value (plusp (length value))) value nil)))

(defun read-argument (element)
  (%make-argument
   :name (protocol-keyword (attribute element "name"))
   :type (protocol-keyword (attribute element "type"))
   :interface-name (attribute element "interface")
   :nullable (equal (attribute element "allow-null") "true")
   :enum (attribute element "enum")))

(defun read-message (element opcode)
  (%make-message
   :name (protocol-keyword (attribute element "name"))
   :opcode opcode
   :since (parse-integer (or (attribute element "since") "1"))
   :destructor-p (equal (attribute element "type") "destructor")
   :arguments (coerce (mapcar #'read-argument (element-children element "arg"))
                      'simple-vector)))

(defun parse-enum-value (text)
  (if (and (> (length text) 2) (string-equal "0x" text :end2 2))
      (parse-integer text :start 2 :radix 16)
      (parse-integer text)))

(defun read-enum (element)
  (cons (attribute element "name")
        (loop for entry in (element-children element "entry")
              collect (cons (protocol-keyword (attribute entry "name"))
                            (parse-enum-value (attribute entry "value"))))))

(defun read-interface (element)
  (flet ((messages (tag)
           (coerce (loop for child in (element-children element tag)
                         for opcode from 0
                         collect (read-message child opcode))
                   'simple-vector)))
    (%make-interface
     :name (attribute element "name")
     :version (parse-integer (attribute element "version"))
     :requests (messages "request")
     :events (messages "event")
     :enums (mapcar #'read-enum (element-children element "enum")))))

(defun read-protocol-file (pathname)
  (let ((document (cxml:parse-file pathname (cxml-dom:make-dom-builder))))
    (mapcar #'read-interface
            (element-children (dom:document-element document) "interface"))))

(defun load-protocols ()
  (clrhash *interfaces*)
  (dolist (file *protocol-files*)
    (dolist (interface (read-protocol-file (merge-pathnames file (protocol-directory))))
      (setf (gethash (interface-name interface) *interfaces*) interface)))
  (hash-table-count *interfaces*))

(load-protocols)

;;; Looking up messages and enum values.

(defun find-message (interface name messages)
  (or (find name messages :key #'message-name)
      (error "~A has no message ~S." (interface-name interface) name)))

(defun find-request (interface name)
  (find-message interface name (interface-requests interface)))

(defun find-event (interface name)
  (find-message interface name (interface-events interface)))

(defun enum-value (interface enum-reference entry)
  "The integer named by ENTRY in ENUM-REFERENCE, written as in the XML:
either \"name\" within INTERFACE or \"other_interface.name\"."
  (let* ((dot (position #\. enum-reference))
         (owner (if dot
                    (find-interface (subseq enum-reference 0 dot))
                    interface))
         (enum-name (if dot (subseq enum-reference (1+ dot)) enum-reference))
         (entries (cdr (assoc enum-name (interface-enums owner) :test #'string=))))
    (or (cdr (assoc entry entries))
        (error "~A.~A has no entry ~S." (interface-name owner) enum-name entry))))

;;; The C tables.

(defun signature-string (message)
  (with-output-to-string (out)
    (when (> (message-since message) 1)
      (format out "~D" (message-since message)))
    (loop for argument across (message-arguments message)
          do (when (argument-nullable argument)
               (write-char #\? out))
             (if (and (eq (argument-type argument) :new-id)
                      (null (argument-interface-name argument)))
                 (write-string "sun" out)
                 (write-char (ecase (argument-type argument)
                               (:int #\i) (:uint #\u) (:fixed #\f)
                               (:string #\s) (:object #\o) (:new-id #\n)
                               (:array #\a) (:fd #\h))
                             out)))))

(defun signature-types (message)
  "One interface name, or NIL, for each argument position in the signature."
  (loop for argument across (message-arguments message)
        if (and (eq (argument-type argument) :new-id)
                (null (argument-interface-name argument)))
          append (list nil nil nil)
        else
          collect (and (member (argument-type argument) '(:object :new-id))
                       (argument-interface-name argument))))

(defun library-interface-pointer (name)
  (let ((pointer (cffi:foreign-symbol-pointer
                  (format nil "~A_interface" name))))
    (and pointer (not (cffi:null-pointer-p pointer)) pointer)))

(defun build-messages (messages)
  (let ((table (cffi:foreign-alloc '(:struct wl-message)
                                   :count (max 1 (length messages)))))
    (loop for message across messages
          for index from 0
          for entry = (cffi:mem-aptr table '(:struct wl-message) index)
          for types = (signature-types message)
          for type-table = (cffi:foreign-alloc :pointer :count (max 1 (length types)))
          do (loop for name in types
                   for slot from 0
                   do (setf (cffi:mem-aref type-table :pointer slot)
                            (if name
                                (interface-pointer (find-interface name))
                                (cffi:null-pointer))))
             (setf (cffi:foreign-slot-value entry '(:struct wl-message) 'name)
                   (cffi:foreign-string-alloc
                    (substitute #\_ #\- (string-downcase (message-name message))))
                   (cffi:foreign-slot-value entry '(:struct wl-message) 'signature)
                   (cffi:foreign-string-alloc (signature-string message))
                   (cffi:foreign-slot-value entry '(:struct wl-message) 'types)
                   type-table))
    table))

(defun interface-pointer (interface)
  "The struct wl_interface libwayland knows INTERFACE by, made on first use."
  (or (interface-pointer-cached interface)
      (let ((library (library-interface-pointer (interface-name interface))))
        (if library
            (setf (interface-pointer-cached interface) library)
            (let ((pointer (cffi:foreign-alloc '(:struct wl-interface))))
              ;; Publish first: message tables may refer back to this
              ;; interface, as wl_surface's do to wl_surface.
              (setf (interface-pointer-cached interface) pointer)
              (cffi:with-foreign-slots ((name version method-count methods
                                         event-count events)
                                        pointer (:struct wl-interface))
                (setf name (cffi:foreign-string-alloc (interface-name interface))
                      version (interface-version interface)
                      method-count (length (interface-requests interface))
                      event-count (length (interface-events interface))
                      methods (build-messages (interface-requests interface))
                      events (build-messages (interface-events interface))))
              pointer)))))

(defun interface-pointer-cached (interface)
  (interface-%pointer interface))

(defun (setf interface-pointer-cached) (pointer interface)
  (setf (interface-%pointer interface) pointer))

(defun forget-interface-pointers ()
  "Foreign tables do not survive a saved image; rebuild them after restart."
  (loop for interface being the hash-values of *interfaces*
        do (setf (interface-%pointer interface) nil)))

(pushnew 'forget-interface-pointers sb-ext:*save-hooks*)
