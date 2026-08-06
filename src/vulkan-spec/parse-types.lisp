#|
 Copyright(c) 2020 - Lukas Herzberger <herzberger.lukas@gmail.com>
 SPDX-License-Identifier: MIT

 Copyright(c) 2015-2020 - NVIDIA CORPORATION
 SPDX-License-Identifier: Apache-2.0
|#

(in-package #:vulkan-spec)

(defun parse-type-info (node)
  "TODO"
  ;; todo: check attributes
  (let ((type-name (xps (xpath:evaluate "type" node)))
        (prefix (xps (xpath:evaluate "type/preceding-sibling::text()" node)))
        (postfix (xps (xpath:evaluate "type/following-sibling::text()" node))))
    (make-instance 'type-info
                   :type-name (or type-name "")
                   :prefix prefix
                   :postfix postfix)))

;; See VulkanHppGenerator::readBaseType
(defun parse-basetype (node vk-spec)
  "TODO"
  ;; todo: check attributes
  (let* ((attributes (attrib-names node))
         (name-data (parse-name-data node))
         (type-info (parse-type-info node)))
    (assert (not (array-sizes name-data))
            () "name <~a> with unsupported array-sizes" (name name-data))
    (assert (not (bit-count name-data))
            () "name <~a> with unsupported bit-count <~a>" (name name-data) (bit-count name-data))
    (assert (or (= (length (type-name type-info)) 0)
                (string= (prefix type-info) "typedef"))
            () "unexpected type prefix <~a>" (prefix type-info))
    (assert (or (= (length (prefix type-info)) 0)
                (string= (prefix type-info) "typedef"))
            () "unexpected type prefix <~a>" (prefix type-info))
    (assert (or (= (length (postfix type-info)) 0)
                (string= (postfix type-info) "*"))
            () "unexpected type postfix <~a>" (postfix type-info))
    (when (> (length (type-name type-info)) 0)
      (assert (not (gethash (name name-data) (base-types vk-spec)))
              () "basetype <~a> already specified" (name name-data))
      (setf (gethash (name name-data) (base-types vk-spec))
            (make-instance 'base-type
                           :name (name name-data)
                           :type-name (type-name type-info))))
    (assert (not (gethash (name name-data) (types vk-spec)))
            () "basetype <~a> already specified as a type" (name name-data))
    (setf (gethash (name name-data) (types vk-spec))
          (make-instance 'vk-type
                         :name (name name-data)
                         :category :basetype))))

;; see VulkanHppGenerator::readBitmask && VulkanHppGenerator::readBitmaskAlias
(defun parse-bitmask (node vk-spec)
  "Parses a "
  (let ((alias (xps (xpath:evaluate "@alias" node))))
    (if alias
        (let* ((alias (xps (xpath:evaluate "@alias" node)))
               (name (xps (xpath:evaluate "@name" node)))
               (bitmask (gethash alias (bitmasks vk-spec))))
          (assert bitmask
                  () "missing alias <~a>" alias)
          (assert (= (length (alias bitmask)))
                  () "alias for bitmask <~a> already specified as <~a>" (name bitmask) (alias bitmask))
          (setf (alias bitmask) name)
          (assert (not (gethash name (types vk-spec)))
                  () "aliased bitmask <~a> already specified as a type" name)
          (setf (gethash name (types vk-spec))
                (make-instance 'vk-type
                               :name name
                               :category :bitmask)))
        (let ((name-data (parse-name-data node))
              (type-info (parse-type-info node))
              (requires (xps (xpath:evaluate "@requires" node))))
          (assert (alexandria:starts-with-subseq "Vk" (name name-data))
                  () "name <~a> does not begin with <VK>" (name name-data))
          (assert (= (length (array-sizes name-data)) 0)
                  () "name <~a> with unsupported array-sizes" (array-sizes name-data))
          (unless (find (type-name type-info) '("VkFlags" "VkFlags64") :test #'string=)
            (warn "unexpected bitmask type <~a>" (type-name type-info)))
          (assert (string= (prefix type-info) "typedef")
                  () "unexpected type prefix <~a>" (prefix type-info))
          (assert (= (length (postfix type-info)) 0)
                  () "unexpected type postfix <~a>" (postfix type-info))
          (assert (not (gethash (name name-data) (commands vk-spec)))
                  () "command <~a> already specified" (name name-data))
          (setf (gethash (name name-data) (bitmasks vk-spec))
                (make-instance 'bitmask
                               :name (name name-data)
                               :type-name (type-name type-info)
                               :requires requires))
          (assert (not (gethash (name name-data) (types vk-spec)))
                  () "bitmask <~a> already specified as a type" (name name-data))
          (setf (gethash (name name-data) (types vk-spec))
                (make-instance 'vk-type
                               :name (name name-data)
                               :category :bitmask))))))

;; see VulkanHppGenerator::readDefine
(defun parse-define (node vk-spec)
  "TODO"
  (let* ((name (xps (xpath:evaluate "name" node)))
         (@name (xps (xpath:evaluate "@name" node)))
         (type (xps (xpath:evaluate "type" node)))
         (requires (xps (xpath:evaluate "@requires" node)))
         (args (xps (xpath:evaluate (cond
                                      (type "type/following-sibling::text()")
                                      (name "name/following-sibling::text()")
                                      (@name "text()")
                                      (t (error "unknown define args path for define")))
                                    node)))
         (is-value-p (alexandria:starts-with-subseq "(" args))
         (is-struct-p (search "struct" (xps node))))
    (when is-struct-p
        (assert (not (gethash name (types vk-spec)))
                () "type <~a> has already been specified" name)
        (setf (gethash (or name @name) (types vk-spec))
              (make-instance 'vk-type
                             :name (or name @name)
                             :category :define)))
    (when @name
      (assert (member @name
                      '("VK_DEFINE_NON_DISPATCHABLE_HANDLE"
                        "VK_USE_64_BIT_PTR_DEFINES" ;; new since v1.2.174
                        "VK_NULL_HANDLE")           ;; new since v1.2.174
                      :test #'string=)
              () "unknown category=define name <~a>" @name)
      (setf name @name)
      (setf is-value-p nil)
      (setf args (xps node)))    
    (assert (not (gethash name (defines vk-spec)))
            () "define <~a> has already been specified" name)
    (setf (gethash name (defines vk-spec))
          (make-instance 'define
                         :name name
                         :is-value-p is-value-p
                         :is-struct-p is-struct-p
                         :requires requires
                         :calls type
                         :args args))))

(defun parse-handle (node vk-spec)
  "TODO"
  (let ((alias (xps (xpath:evaluate "@alias" node))))
    (if alias
        (let ((handle (gethash alias (handles vk-spec)))
              (name (xps (xpath:evaluate "@name" node))))
          (assert handle
                  () "using unspecified alias <~a>" alias)
          (assert (not (alias handle))
                  () "handle <~a> already has an alias <~a>" (name handle) (alias name))
          (setf (alias handle) name)
          (assert (not (gethash name (types vk-spec)))
                  () "handle alias <~a> already specified as a type" name)
          (setf (gethash name (types vk-spec))
                (make-instance 'vk-type
                               :name name
                               :category :handle)))
        (let ((parent (xps (xpath:evaluate "@parent" node)))
              (name-data (parse-name-data node))
              (type-info (parse-type-info node)))
          (assert (alexandria:starts-with-subseq "Vk" (name name-data))
                  () "name <~a> does not begin with <Vk>" (name name-data))
          (assert (= (length (array-sizes name-data)) 0)
                  () "name <~a> with unsupported array-sizes" (name name-data))
          (assert (= (length (bit-count name-data)) 0)
                  () "name <~a> with unsupported bit-count <~a>" (name name-data) (bit-count name-data))
          (assert (or (string= (type-name type-info) "VK_DEFINE_HANDLE")
                      (string= (type-name type-info) "VK_DEFINE_NON_DISPATCHABLE_HANDLE"))
                  () "handle with invalid type <~a>" (type-name type-info))
          (assert (= (length (prefix type-info)) 0)
                  () "unexpected type prefix <~a>" (prefix type-info))
          (assert (string= (postfix type-info) "(")
                  () "unexpected type postfix <~a>" (postfix type-info))
          (assert (not (gethash (name name-data) (handles vk-spec)))
                  () "handle <~a> already specified" (name name-data))
          (setf (gethash (name name-data) (handles vk-spec))
                (make-instance 'handle
                               :name (name name-data)
                               :parents (tokenize parent)
                               :non-dispatch-handle-p (string= (type-name type-info) "VK_DEFINE_NON_DISPATCHABLE_HANDLE")))
          (assert (not (gethash (name name-data) (types vk-spec)))
                  () "handle <~a> already specified as a type" (name name-data))
          (setf (gethash (name name-data) (types vk-spec))
                (make-instance 'vk-type
                               :name (name name-data)
                               :category :handle))))))

(defun parse-funcpointer (node vk-spec)
  "TODO"
  (let ((requirements (xps (xpath:evaluate "@requires" node)))
        (name (xps (xpath:evaluate "name" node))))
    (assert (str-not-empty-p name)
            () "funcpointer with empty name")
    (assert (not (gethash name (func-pointers vk-spec)))
            () "funcpointer <~a> already specified" name)
    (setf (gethash name (func-pointers vk-spec))
          (make-instance 'func-pointer
                         :name name
                         :requirements requirements))
    (assert (not (gethash name (types vk-spec)))
            () "funcpointer <~a> already specified as a type" name)
    (setf (gethash name (types vk-spec))
          (make-instance 'vk-type
                         :name name
                         :category :funcpointer))
    (let* ((types (mapcar 'xps (xpath:all-nodes (xpath:evaluate "type" node)))))
      (loop for type in types
            do (progn
                 (assert (str-not-empty-p type)
                         () "funcpointer argument with empty type")
                 (assert (or (gethash type (types vk-spec))
                             (string= type requirements))
                         () "funcpointer argument of unknown type <~a>" type))))))

;; see VulkanHppGenerator::readTypeEnum
(defun parse-enum-type (node vk-spec)
  "TODO"
  (let ((alias (xps (xpath:evaluate "@alias" node)))
        (name (xps (xpath:evaluate "@name" node))))
    (if alias
        (progn
          (assert (> (length alias) 0)
                  () "enum with empty alias")
          (let ((enum (gethash alias (enums vk-spec))))
            (assert enum
                    () "enum with unknown alias <~a>" alias)
            (assert (= (length (alias enum)) 0)
                    () "enum <~a> already has an alias <~a>" (name enum) (alias enum))
            (setf (alias enum) name)))
        (progn
          (assert (not (gethash name (enums vk-spec)))
                  () "enum <~a> already specified" name)
          (setf (gethash name (enums vk-spec))
                (make-instance 'enum
                               :name name
                               :alias alias))))
    (assert (not (gethash name (types vk-spec)))
            () "enum <~a> already specified as a type" name)
    (setf (gethash name (types vk-spec))
          (make-instance 'vk-type
                         :name name
                         :category :enum))))

(defun parse-type-include (node vk-spec)
  "Parse a \"type\" tag belonging to the \"include\" category from the given NODE and stores its name in the list of include types in the list bound to the INCLUDES slot of the given VULKAN-SPEC instance.

See INCLUDES
See VULKAN-SPEC
"
  (let ((name (xps (xpath:evaluate "@name" node))))
    (assert (not (find name (includes vk-spec)))
            () "include named <~a> already specified" name)
    (push name (includes vk-spec))))

(defun parse-requires (node vk-spec)
  "Parses a \"type\" tag with a \"requires\" attribute but no \"category\" from the given NODE into a VK-TYPE and stores them in the hash map bound to the TYPES slot of the given VULKAN-SPEC instance.

See VK-TYPE
See VULKAN-SPEC
"
  (let ((name (xps (xpath:evaluate "@name" node)))
        (requires (xps (xpath:evaluate "@requires" node))))
    (assert (not (gethash name (types vk-spec)))
            () "type <~a> already specified as a type" name)
    (if requires
        (progn
          (assert (find requires (includes vk-spec) :test #'string=)
                  () "type requires unknown include <~a>" requires)
          (setf (gethash name (types vk-spec))
                (make-instance 'vk-type
                               :name name
                               :category :requires)))
        (progn
          (assert (string= name "int")
                  () "unknown type")
          (setf (gethash name (types vk-spec))
                (make-instance 'vk-type
                               :name name
                               :category :unknown))))))

(defun parse-types (vk.xml vk-spec)
  "Parses all \"type\" tags in the given vk.xml to their representations in VULKAN-SPEC and stores them in the given VULKAN-SPEC instance.

See PARSE-TYPE-INCLUDE
See PARSE-REQUIRES
See PARSE-BASETYPE
See PARSE-BITMASK
See PARSE-DEFINE
See PARSE-ENUM-TYPE
See PARSE-HANDLE
See PARSE-STRUCT
See PARSE-FUNCPOINTER
See VULKAN-SPEC
"
  (xpath:do-node-set (node (xpath:evaluate "/registry/types/type[(@category=\"include\")]" vk.xml))
    (parse-type-include node vk-spec))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types/type[not(@category)]" vk.xml))
    (parse-requires node vk-spec))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types/type[@category=\"basetype\"]" vk.xml))
    (parse-basetype node vk-spec))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types/type[@category=\"bitmask\"]" vk.xml))
    (parse-bitmask node vk-spec))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types/type[@category=\"define\"]" vk.xml))
    (parse-define node vk-spec))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types/type[@category=\"enum\"]" vk.xml))
    (parse-enum-type node vk-spec))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types/type[@category=\"handle\"]" vk.xml))
    (parse-handle node vk-spec))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types/type[@category=\"struct\" or @category=\"union\"]" vk.xml))
    (parse-struct node vk-spec))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types/type[@category=\"funcpointer\"]" vk.xml))
    (parse-funcpointer node vk-spec)))
