#|
 Copyright(c) 2020 - Lukas Herzberger <herzberger.lukas@gmail.com>
 SPDX-License-Identifier: MIT

 Copyright(c) 2015-2019 - NVIDIA CORPORATION
 SPDX-License-Identifier: Apache-2.0
|#

(in-package #:vulkan-spec)

;;; CLASSES WITH HEAVILY REUSED SLOTS

(defclass has-name ()
  ((name
    :initarg :name
    :type string
    :initform (error "name not supplied")
    :accessor name))
  (:documentation "Provides a required NAME slot.

Slots:
See NAME    the name of the instance in the Vulkan API registry."))

(defclass has-alias ()
  ((alias
    :initarg :alias
    :type string
    :initform nil
    :accessor alias))
  (:documentation "Provides an optional ALIAS slot.

Slots:
See ALIAS   the alias for this instance."))

(defclass has-type-name ()
  ((type-name
    :initarg :type-name
    :type string
    :initform (error "type-name not supplied")
    :accessor type-name))
  (:documentation "Provides a requiered TYPE-NAME slot.

Slots:
See TYPE-NAME    the name of a type in the Vulkan API registry or a primitive C type."))

(defclass has-array-sizes ()
  ((array-sizes
    :initarg :array-sizes
    :type list ;; string
    :initform nil
    :accessor array-sizes))
  (:documentation "TODO"))

(defclass has-bit-count ()
  ((bit-count
    :initarg :bit-count
    :type string
    :initform nil
    :accessor bit-count)))

(defclass has-referenced-in ()
  ((referenced-in
    :initarg :referenced-in
    :type string
    :initform nil
    :accessor referenced-in))
  (:documentation "Has a slot REFERENCED-IN holding a string which names a feature or extension referencing the instance."))

;; todo: maybe this is also a vk-element
(defclass name-data (has-name has-array-sizes has-bit-count)
  ()
  (:documentation "TODO"))

(defclass vk-element (has-name)
  ((comment
    :initarg :comment
    :type string
    :initform ""
    :accessor comment)
   (id
    :type integer
    :initform 0
    :reader id)
   (count
    :allocation :class
    :type integer
    :initform 0))
  (:documentation "The base class for elements parsed from a vk.xml.

Instances of VK-ELEMENT are initialized with the index of their occurrence ID
which is determined using the shared slot COUNT.
This can be used to sort elements based on their occurrence in the XML API Registry.

Slots:
See ID
See NAME       the name of the element in the Vulkan API registry.
See COMMENT    a comment describing the element.
"))

(defmethod initialize-instance :after ((obj vk-element) &rest args)
  (setf (slot-value obj 'id) (slot-value obj 'count))
  (incf (slot-value obj 'count)))

(defmethod print-object ((obj name-data) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((name name)
                     (array-sizes array-sizes)
                     (bit-count bit-count))
        obj
      (format stream "name: ~a, array-sizes: ~a, bit-count: ~a" name array-sizes bit-count))))

(defmethod print-object ((obj vk-element) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((name name))
        obj
      (format stream "name: ~a" name))))

(defun sorted-elements (vk-elements)
  "Returns a sorted list of VK-ELEMENT instances, sorted by their ID in increasing order."
  (sort vk-elements #'< :key #'id))

(defun sorted-names (vk-elements)
  "Returns a sorted list of names of VK-ELEMENT instances, sorted by their ID in increasing order.

See SORTED-ELEMENTS
"
  (map 'list #'name (sorted-elements vk-elements)))


(defclass base-type (vk-element has-type-name)
  ()
  (:documentation "A base type from the Vulkan API Registry.
Base types are aliases for other types named by TYPE. These types are either
types from the Vulkan API registry or C base types.
After the whole vk.xml has been parsed TYPE-NAME must name either a type in
*VK-PLATFORM* or ???. TODO: find out where this is stored in the end.

Slots:
See TYPE   the name of the type of this base type. 

See VK-ELEMENT
See *VK-PLATFORM* 
"))

(defclass bitmask (vk-element has-type-name has-alias)
  ((requires
   :initarg :requires
   :type string
   :initform nil
   :accessor requires))
  (:documentation "TODO"))

;; TODO: check what compose should do
;; TODO: needs const-pointer-p
;; TODO: needs non-const-pointer-p
;; TODO: needs value-p
(defclass type-info (has-type-name)
  ((prefix
    :initarg :prefix
    :type string
    :initform nil
    :accessor prefix)
   (postfix
    :initarg :postfix
    :type string
    :initform nil
    :accessor postfix))
  (:documentation "TODO"))

(defmethod print-object ((obj type-info) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots ((prefix prefix)
                 (type-name type-name)
                 (postfix postfix))
        obj
      (format stream "~@[~a ~]~a~@[ ~a~]" prefix type-name postfix))))

(defun const-pointer-p (type-info)
  "Checks whether or not a TYPE-INFO describes a const pointer."
  (when (and (search "const" (prefix type-info))
             (search "*" (postfix type-info)))
    t))

(defun non-const-pointer-p (type-info)
  "Checks whether or not a TYPE-INFO describes a non-const pointer."
  (when (and (not (search "const" (prefix type-info)))
             (search "*" (postfix type-info)))
    t))

(defun value-p (type-info)
  "Checks whether or not a TYPE-INFO describes a value rather than a pointer."
  (when (and (not (search "*" (prefix type-info)))
             (not (search "*" (postfix type-info))))
    t))

(defun pointer-p (type-info)
  "Checks whether a TYPE-INFO describes a pointer."
  (not (value-p type-info)))

(defun pointer-to-non-char-pointer-p (type-info)
  "Checks whether a TYPE-INFO describes an array of pointers which is not an array of strings."
  (and (search "* const*" (postfix type-info))
       (not (string= "char" (type-name type-info)))))

(defclass has-type-info ()
  ((type-info
    :initarg :type-info
    :type type-info
    :accessor type-info))
  (:documentation "The superclass for all VK-ELEMENTs that have a TYPE-INFO slot."))

(defun get-type-name (h)
  "Returns a HAS-TYPE-INFO's type name."
  (type-name (type-info h)))

(defclass param (vk-element has-type-info has-array-sizes)
  ((len
    :initarg :len
    :type string
    :initform nil
    :accessor len)
   (optional-p
    :initarg :optional-p
    :type boolean
    :initform nil
    :accessor optional-p))
  (:documentation "TODO"))

(defclass has-needs-explicit-loading-p ()
  ((needs-explicit-loading-p
    :initarg :needs-explicit-loading-p
    :type boolean
    :initform nil
    :accessor needs-explicit-loading-p))
  (:documentation "Provides an optional NEEDS-EXPLICIT-LOADING-P slot.

Slots:
See NEEDS-EXPLICIT-LOADING-P   If true, the command is an extension function which needs to be loaded explicitly.
                               Note that *KHR-functions are extension functions, but reside in the core shared library (libvulkan.so/vulkan-1.dll/libvulkan.1.dylib),
                               so their function pointers don't have to be loaded dynamically using vkGet*ProcAddr.
"))

(defclass command-alias (vk-element has-referenced-in has-needs-explicit-loading-p)
  ()
  (:documentation "TODO"))

(defclass command (vk-element has-referenced-in has-needs-explicit-loading-p)
  ((alias
    :initarg :alias
    :type hash-table ;; string - command-alias
    :initform (make-hash-table :test 'equal)
    :accessor alias)
   (error-codes
    :initarg :error-codes
    :type list ;; string
    :initform nil
    :accessor error-codes)
   (handle
    :initarg :handle
    :type string
    :initform nil
    :accessor handle)
   (params
    :initarg :params
    :type list ;; param
    :initform nil
    :accessor params)
   (return-type
    :initarg :return-type
    :type string
    :initform nil
    :accessor return-type)
   (success-codes
    :initarg :success-codes
    :type list ;; string
    :initform nil
    :accessor success-codes))
  (:documentation "TODO

See VK-ELEMENT
See HAS-REFERENCED-IN
See HAS-EXTENSION-P
"))

(defun make-aliased-command (command command-alias)
  "Creates an aliased command from a COMMAND and a COMMAND-ALIAS instance."
  (make-instance 'command
                 :name (name command-alias)
                 :referenced-in (referenced-in command-alias)
                 :comment (comment command-alias)
                 :error-codes (error-codes command)
                 :handle (handle command)
                 :params (params command)
                 :return-type (return-type command)
                 :success-codes (success-codes command)))

(defclass enum-value (vk-element)
  ((number-value
    :initarg :number-value
    :type integer
    :initform nil
    :accessor number-value)
   (string-value
    :initarg :string-value
    :type string
    :initform nil
    :accessor string-value)
   ;; how should offsets be handled?
   (vk-hpp-name ;; I'll keep this, in case I wanna name lisp enum values according to this rather than their C names
    :initarg :vk-hpp-name
    :type string
    :initform nil
    :accessor vk-hpp-name)
   (single-bit-p
    :initarg :single-bit-p
    :type boolean
    :initform nil
    :accessor single-bit-p)
   (protect
    :initarg :protect
    :type string
    :initform nil
    :accessor protect)
   (extension
    :initarg :extension
    :type string
    :initform nil
    :accessor extension))
  (:documentation "TODO"))

(defclass api-constant (enum-value has-alias)
  ()
  (:documentation "TODO"))

(defclass enum (vk-element has-alias)
  ((aliases
    :initarg :aliases
    :type hash-table ;; string to (string string)
    :initform (make-hash-table :test 'equal)
    :accessor aliases)
   (is-bitmask-p
    :initarg :is-bitmask-p
    :type boolean
    :initform nil
    :accessor is-bitmask-p)
   (bitwdith
    :initarg :bitwidth
    :type string
    :initform nil
    :accessor bitwidth)
   (enum-values
    :initarg :enum-values
    :type list ;; enum-value
    :initform nil
    :accessor enum-values))
  (:documentation "TODO"))

(defclass require-data (vk-element)
  ((name
    :initarg :name
    :type string
    :initform ""
    :accessor name)
   (title
    :initarg :title
    :type string
    :initform nil
    :accessor title)
   (commands
    :initarg :commands
    :type list ;; of strings
    :initform nil
    :accessor commands)
   (types
    :initarg :types
    :type list ;; of strings
    :initform nil
    :accessor types)))

(defclass feature (vk-element)
  ((feature-number
    :initarg :feature-number
    :type string
    :initform nil
    :accessor feature-number)
   (require-data
    :initarg :require-data
    :type list ;; of require-data
    :initform nil
    :accessor require-data)))

(defclass extension (vk-element)
  ((deprecated-by
    :initarg :deprecated-by
    :type string
    :accessor deprecated-by)
   (obsoleted-by
    :initarg :obsoleted-by
    :type string
    :accessor obsoleted-by)
   (platform
    :initarg :platform
    :type string
    :accessor platform)
   (promoted-to
    :initarg :promoted-to
    :type string
    :accessor promoted-to)
   (requires-attribute
    :initarg :requires-attribute
    :type list ;; a set of strings
    :initform nil
    :accessor requires-attribute)
   (require-data
    :initarg :require-data
    :type list ;; of require-data
    :initform nil
    :accessor require-data))
  (:documentation "TODO"))

(defclass func-pointer (vk-element)
  ((requirements
    :initarg :requirements
    :type string
    :accessor requirements))
  (:documentation "TODO"))

;; todo: there is new stuff here as well (e.g. objtypeenum, parent is singular now?, secondLevelCommands
(defclass handle (vk-element has-alias)
  ((children
    :initarg :children
    :type list ;; string
    :initform nil
    :accessor children)
   (parents
    :initarg :parents
    :type list ;; string
    :initform nil
    :accessor parents)
   (non-dispatch-handle-p
    :initarg :non-dispatch-handle-p
    :type boolean
    :initform nil
    :accessor non-dispatch-handle-p)
   (commands
    :initarg :commands
    :type list ;; string
    :initform nil
    :accessor commands)
   (delete-command
    :initarg :delete-command
    :type string
    :initform nil
    :accessor delete-command)
   (delete-pool
    :initarg :delete-pool
    :type string
    :initform nil
    :accessor delete-pool)
   (create-commands
    :initarg :create-commands
    :type list ;; string
    :initform nil
    :accessor create-commands))
  (:documentation "TODO

See CHILDREN         The names of the HANDLE types that belong to this type.
See PARENTS          The names of the HANDLE types this type belongs to.
See COMMANDS         The names of the COMMAND types that are performed using
                     this type.
See DELETE-COMMAND   The name of the command that deletes this handle.
See DELETE-POOL
"))

(defun handlep (type-name vk-spec)
  (or (gethash type-name (handles vk-spec))
      (find-if (lambda (h)
                 (string= type-name
                          (alias h)))
               (alexandria:hash-table-values (handles vk-spec)))))

(defun get-handle (type-name vk-spec)
  (or (gethash type-name (handles vk-spec))
      (find-if (lambda (h)
                 (string= type-name
                          (alias h)))
               (alexandria:hash-table-values (handles vk-spec)))))

(defclass member-data (vk-element name-data has-type-info)
  ((len
    :initarg :len
    :type list ;; string
    :initform nil
    :accessor len)
   (no-autovalidity-p
    :initarg :no-autovalidity-p
    :type boolean
    :initform nil
    :accessor no-autovalidity-p)
   (optional-p
    :initarg :optional-p
    :type boolean
    :initform nil
    :accessor optional-p)
   (selection
    :initarg :selection
    :type string
    :initform nil
    :accessor selection)
   (selector
    :initarg :selector
    :type string
    :initform nil
    :accessor selector)
   (allowed-values
    :initarg :allowed-values
    :type list ;; string
    :initform nil
    :accessor allowed-values)
   (used-constant
    :initarg :used-constant
    :type string
    :initform nil
    :accessor used-constant))
  (:documentation "TODO

Slots:
See ALLOWED-VALUES    a list of allowed values for this MEMBER-DATA instance."))

(defclass platform (vk-element)
  ((protect
    :initarg :protect
    :type string
    :initform nil
    :accessor protect))
  (:documentation "TODO"))

(defclass struct (vk-element)
  ((allow-duplicate-p
    :initarg :allow-duplicate-p
    :type boolean
    :initform nil
    :accessor allow-duplicate-p)
   (is-union-p
    :initarg :is-union-p
    :type boolean
    :initform nil
    :accessor is-union-p)
   (returned-only-p
    :initarg :returned-only-p
    :type boolean
    :initform nil
    :accessor returned-only-p)
   (members
    :initarg :members
    :type list ;; member-data
    :initform nil
    :accessor members)
   (struct-extends
    :initarg :struct-extends
    :type list ;; string
    :initform nil
    :accessor struct-extends)
   (aliases
    :initarg :aliases
    :type list ;; string
    :initform nil
    :accessor aliases)
   (sub-struct
    :initarg :sub-struct
    :type string
    :initform nil
    :accessor sub-struct))
  (:documentation "TODO

Slots:
See MEMBERS    an ordered list of MEMBER-DATA instances describing members of this STRUCT instance."))

(defun structure-type-p (type-name vk-spec &optional (include-opaque-struct-types t))
  (or (gethash type-name (structures vk-spec))
      (and include-opaque-struct-types
           (member type-name *opaque-struct-types* :test #'string=))
      (member-if (lambda (m)
                   (member type-name (aliases m) :test #'string=))
                 (alexandria:hash-table-values (structures vk-spec)))))

(defun get-structure-type (type-name vk-spec)
  (or (gethash type-name (structures vk-spec))
      (find-if (lambda (m)
                 (member type-name (aliases m) :test #'string=))
               (alexandria:hash-table-values (structures vk-spec)))))

(defun has-type-and-next-p (type-name vk-spec)
  (let ((struct (or (gethash type-name (structures vk-spec))
                    (find-if (lambda (s)
                               (member type-name (aliases s) :test #'string=))
                             (alexandria:hash-table-values (structures vk-spec))))))
    (and struct
         (and (member-if (lambda (m)
                           (string= "sType" (name m)))
                         (members struct))
              (member-if (lambda (m)
                           (string= "pNext" (name m)))
                         (members struct))))))

(deftype type-category ()
  "TODO: documentation"
  '(member
    :bitmask
    :basetype
    :define
    :enum
    :funcpointer
    :handle
    :requires
    :struct
    :union
    :unknown))

(defclass vk-type (vk-element has-referenced-in)
  ((category
    :initarg :category
    :type type-category
    :initform :unknown
    :accessor category))
  (:documentation "TODO"))

(defclass define (vk-element)
  ((is-value-p
    :initarg :is-value-p
    :type boolean
    :initform nil
    :accessor is-value-p)
   (is-struct-p
    :initarg :is-struct-p
    :type boolean
    :initform nil
    :accessor is-struct-p)
   (requires
    :initarg :requires
    :type string
    :initform nil
    :accessor requires)
   (calls
    :initarg :calls
    :type string
    :initform nil
    :accessor calls)
   (args
    :initarg :args
    :type string
    :initform nil
    :accessor args))
  (:documentation "Describes a #define in the Khronos Vulkan XML API Registry.

DEFINE instances should be parsed to lisp functions / values.

If IS-VALUE-P is truthy ARGS is a value (e.g.: '#define VK_HEADER_VERSION 29').

If CALLS is not NIL ARGS are the arguments for a function from another #define (e.g.: '#define VK_API_VERSION_1_0 VK_MAKE_VERSION(1, 0, 0)').

If IS-VALUE-P and CALLS are both NIL, the #define defines a function (e.g. '#define VK_MAKE_VERSION(major, minor, patch) ((((uint32_t)(major)) << 22) | (((uint32_t)(minor)) << 12) | ((uint32_t)(patch)))').

If REQUIRES is not NIL the #define uses another #define as an argument for a function (e.g. '#define VK_MAKE_VERSON(1, 2, VK_HEADER_VERSION)').

If IS-STRUCT-P the #define actually names a generic external type (e.g.: 'struct ANativeWindow'). Since version 1.2.140 these types are no longer in the category :DEFINE, but in :BASETYPE.
"))

(defclass vulkan-spec ()
  ((base-types
    :initarg :base-types
    :type hash-table ;; string to base-type
    :initform (make-hash-table :test 'equal)
    :accessor base-types)
   (bitmasks
    :initarg :bitmasks
    :type hash-table ;; string to bitmask
    :initform (make-hash-table :test 'equal)
    :accessor bitmasks)
   (commands
    :initarg :commands
    :type hash-table ;; string to command
    :initform (make-hash-table :test 'equal)
    :accessor commands)
   (constants
    :initarg :constants
    :type hash-table ;; string to api-constant
    :initform (make-hash-table :test 'equal)
    :accessor constants)
   (defines
    :initarg :defines
    :type hash-table ;; string to define
    :initform (make-hash-table :test 'equal)
    :accessor defines)
   (enums
    :initarg :enums
    :type hash-table ;; string to enum
    :initform (make-hash-table :test 'equal)
    :accessor enums)
   (extended-structs
    :initarg :extended-structs
    :type list ;; string
    :initform nil
    :accessor extended-structs)
   (extensions
    :initarg :extensions
    :type hash-table ;; string to extension
    :initform (make-hash-table :test 'equal)
    :accessor extensions)
   (extension-names
    :initarg :extension-names
    :type hash-table ;; string to string (name of constant to extension name)
    :initform (make-hash-table :test 'equal)
    :accessor extension-names)
   (features
    :initarg :features
    :type hash-table ;; string to feature
    :initform (make-hash-table :test 'equal)
    :accessor features)
   (func-pointers
    :initarg :func-pointers
    :type hash-table ;; string to func-pointer
    :initform (make-hash-table :test 'equal)
    :accessor func-pointers)
   (handles
    :initarg :handles
    :type hash-table ;; string to handle
    :initform (make-hash-table :test 'equal)
    :accessor handles)
   (includes
    :initarg :includes
    :type list ;; string
    :initform nil
    :accessor includes)
   (platforms
    :initarg :platforms
    :type hash-table ;; string to platform
    :initform (make-hash-table :test 'equal)
    :accessor platforms)
   (structure-aliases
    :initarg :structure-aliases
    :type hash-table ;; string to string
    :initform (make-hash-table :test 'equal)
    :accessor structure-aliases)
   (structures
    :initarg :structures
    :type hash-table ;; string to structure
    :initform (make-hash-table :test 'equal)
    :accessor structures)
   (tags
    :initarg :tags
    :type list ;; string
    :initform nil
    :accessor tags)
   (types
    :initarg :types
    :type hash-table ;; string to vk-type
    :initform (make-hash-table :test 'equal)
    :accessor types)
   (typesafe-check ;; todo: this is not needed for CL bindings
    :initarg :typesafe-check
    :type string
    :initform nil
    :accessor typesafe-check)
   (version
    :initarg :version
    :type string
    :initform nil
    :accessor version)
   (vulkan-license-header
    :initarg :vulkan-license-header
    :type string
    :initform nil
    :accessor vulkan-license-header))
  (:documentation "Vulkan specification class based on VulkanHppGenerator"))

