#|
 Copyright(c) 2020 - Lukas Herzberger <herzberger.lukas@gmail.com>
 SPDX-License-Identifier: MIT

 Copyright(c) 2015-2020 - NVIDIA CORPORATION
 SPDX-License-Identifier: Apache-2.0
|#

(in-package #:vulkan-spec)

(defun parse-require-enum (node tag vk-spec)
  "TODO"
  (let ((alias (xps (xpath:evaluate "@alias" node)))
        (name (xps (xpath:evaluate "@name" node)))
        (extends (xps (xpath:evaluate "@extends" node))))
    (if alias
        ;; readRequireEnumAlias
        (when extends
          (let ((enum (gethash extends (enums vk-spec))))
            (assert enum
                    () "feature extends unknown enum <~a>" extends)
            (multiple-value-bind (prefix postfix) (get-enum-pre-and-postfix extends (is-bitmask-p enum) (tags vk-spec))
              (let ((vk-hpp-name (create-enum-vk-hpp-name name prefix postfix (is-bitmask-p enum) tag)))
                (when (alias enum)
                  (multiple-value-bind (alias-prefix alias-postfix) (get-enum-pre-and-postfix (alias enum) (is-bitmask-p enum) (tags vk-spec))
                    (when (alexandria:ends-with-subseq postfix name)
                      (setf vk-hpp-name (create-enum-vk-hpp-name name alias-prefix alias-postfix (is-bitmask-p enum) tag)))))
                (add-enum-alias enum name alias vk-hpp-name)))))
        (let ((value-string (xps (xpath:evaluate "@value" node))))
          (if extends
              (let* ((enum (gethash extends (enums vk-spec)))
                     (dir-string (xps (xpath:evaluate "@dir" node)))
                     (dir (if (and dir-string (string= dir-string "-"))
                              -1
                              1))
                     (bitpos-string (xps (xpath:evaluate "@bitpos" node)))
                     (bitpos (numeric-value bitpos-string))
                     (value (numeric-value value-string))
                     (offset-string (xps (xpath:evaluate "@offset" node)))
                     (offset (numeric-value offset-string))
                     (extension-number (numeric-value (and offset
                                                           (or (xps (xpath:evaluate "@extnumber" node))
                                                               (xps (xpath:evaluate "../../@number" node)))))))
                (assert enum
                        () "feature extends unknown enum <~a>" extends)
                (multiple-value-bind (prefix postfix) (get-enum-pre-and-postfix extends (is-bitmask-p enum) (tags vk-spec))
                  (assert (or (and bitpos-string (not value-string) (not offset-string))
                              (and (not bitpos-string) value-string (not offset-string))
                              (and (not bitpos-string) (not value-string) offset-string))
                          () "exactly one of bitpos = <~a>, offset = <~a>, and value = <~a> is supposed to be set" bitpos-string offset-string value-string)
                  (unless (find-if (lambda (v) (string= (name v) name)) (enum-values enum))
                    (push (make-instance 'enum-value
                                         :name name
                                         :number-value (* dir
                                                          (or (and offset (+ +ext-base+
                                                                             (* +ext-block-size+ (1- extension-number))
                                                                             offset))
                                                              value
                                                              (ash 1 bitpos)))
                                         :string-value (or value-string bitpos-string offset-string)
                                         :vk-hpp-name (create-enum-vk-hpp-name name prefix postfix (is-bitmask-p enum) tag)
                                         :single-bit-p (not value))
                          (enum-values enum))
                  (setf (enum-values enum)
                        (sort (enum-values enum)
                              (lambda (a b) (< (number-value a) (number-value b))))))))
              (if value-string
                  (when (search "EXTENSION_NAME" name)
                    (assert (not (gethash name (extension-names vk-spec)))
                            () "name for extension <~a> already registered" name)
                    (setf (gethash name (extension-names vk-spec)) (subseq value-string 1 (- (length value-string) 1))))
                (assert (gethash name (constants vk-spec))
                        () "unknown required enum <~a>" name)))))))

(defun parse-extension-require-command (node extension-name require-data vk-spec)
  "TODO"
  (let* ((name (xps (xpath:evaluate "@name" node)))
         (aliasp (not (gethash name (commands vk-spec))))
         (command (or (gethash name (commands vk-spec))
                      (find-if (lambda (c)
                                 (gethash name (alias c)))
                               (alexandria:hash-table-values (commands vk-spec))))))
    (assert command
            () "extension <~a> requires unknown command <~a>" extension-name name)
    (when aliasp
      (setf command (gethash name (alias command))))
    (if (not (referenced-in command))
        (setf (referenced-in command) extension-name)
        (assert (string= (get-platform (referenced-in command) vk-spec)
                         (get-platform extension-name vk-spec))
                () "command <~a> is referenced in extensions <~a> and <~a> and thus protected by different platforms <~a> and <~a>!"
                name
                (referenced-in command)
                extension-name
                (get-platform (referenced-in command) vk-spec)
                (get-platform extension-name vk-spec)))
    (when (and (not (needs-explicit-loading-p command))
               (not (member extension-name *directly-exposed-extensions* :test #'string=)))
      (setf (needs-explicit-loading-p command) t))
    (assert (not (member name (commands require-data) :test #'string=)) ()
            "command <~a> already listed in require-data of extension <~a>" name extension-name)
    (push name (commands require-data))))

(defun get-platform (title vk-spec)
  (unless (gethash title (features vk-spec))
    (let ((extension (gethash title (extensions vk-spec))))
      (assert extension ()
              "extension <~a> not found" title)
      (platform extension))))

(defun get-platforms (extension-names vk-spec)
  "Returns a list of unique platform names for a given sequence of extension names."
  (remove-duplicates
   (loop for e in extension-names
         collect (platform (gethash e (extensions vk-spec))))))

(defun parse-extension-require-type (node extension-name require-data vk-spec)
  "TODO"
  (let* ((name (xps (xpath:evaluate "@name" node)))
         (type (gethash name (types vk-spec))))
    (assert type
            () "failed to find required type <~a>" name)
    (if (not (referenced-in type))
        (progn
          (setf (referenced-in type) extension-name)
          (assert (not (member name (types require-data) :test #'string=)) ()
                  "type <~a> already listed in require-data of extension <~a>" name extension-name)
          (push name (types require-data)))
        (assert (string= (get-platform (referenced-in type) vk-spec)
                         (get-platform extension-name vk-spec))
                () "type <~a> is referenced in extensions <~a> and <~a> and thus protected by different platforms <~a> and <~a>!"
                name
                (referenced-in type)
                extension-name
                (get-platform (referenced-in type) vk-spec)
                (get-platform extension-name vk-spec)))))

(defun parse-extensions (vk.xml vk-spec)
  "TODO"
  (xpath:do-node-set (node (xpath:evaluate "/registry/extensions/extension" vk.xml))
    (let ((name (xps (xpath:evaluate "@name" node)))
          (number (xps (xpath:evaluate "@number" node)))
          (platform (xps (xpath:evaluate "@platform" node)))
          (deprecated-by (xps (xpath:evaluate "@deprecatedby" node)))
          (obsoleted-by (xps (xpath:evaluate "@obsoletedby" node)))
          (promoted-to (xps (xpath:evaluate "@promotedto" node)))
          (provisional (xps (xpath:evaluate "@provisional" node)))
          (requirements
            (remove-duplicates
             (tokenize (xps (xpath:evaluate "@requires" node)))
             :test 'string=))
          (requires-core (xps (xpath:evaluate "@requiresCore" node)))
          (supported (xps (xpath:evaluate "@supported" node))))
      (assert (or (not platform)
                  (gethash platform (platforms vk-spec)))
              () "unknown platform <~a>" platform)
      (assert (or (not requires-core)
                  (find-if (lambda (f)
                             (string= (feature-number f) requires-core))
                           (alexandria:hash-table-values (features vk-spec))))
              () "unknown feature number <~a>" requires-core)
      (if (string= supported "disabled")
          ;; see VulkanHppGenerator::readExtensionDisabledRequire
          (xpath:do-node-set (require-node (xpath:evaluate "require" node))
            ;; todo: remove disabled stuff
            (xpath:do-node-set (disable-node (xpath:evaluate "command" require-node))
              (let* ((command-name (xps (xpath:evaluate "@name" disable-node)))
                     (command (gethash command-name (commands vk-spec))))
                (if (not command)
                    (warn "trying to remove unknown command <~a>" command-name)
                    (progn
                      (remhash command-name (commands vk-spec))
                      (let ((handle (gethash (handle command) (handles vk-spec))))
                        (assert handle
                                () "cannot find handle corresponding to command <~a>" command-name)
                        (remove command-name (commands handle) :test 'string=))))))
            ;; disabled enums are skipped also by VulkanHppGenerator
            (xpath:do-node-set (disable-node (xpath:evaluate "type" require-node))
              (let* ((type-name (xps (xpath:evaluate "@name" disable-node)))
                     (type (gethash type-name (types vk-spec))))
                (if (not type)
                    (warn "trying to remove unknown type <~a>" type-name)
                    (progn
                      (cond
                        ((eq (category type) :bitmask)
                         (let ((bitmask (gethash type-name (bitmasks vk-spec))))
                           (assert bitmask
                                   () "trying to remove unknown bitmask <~a>" type-name)
                           (assert (not (alias bitmask))
                                   () "trying to remove disabled bitmask <~a> which has alias <~a>" type-name (alias bitmask))
                           (remhash type-name (bitmasks vk-spec))))
                        ((eq (category type) :enum)
                         (let ((enum (gethash type-name (enums vk-spec))))
                           (assert enum
                                   () "trying to remove unknown enum <~a>" type-name)
                           (assert (not (alias enum))
                                   () "trying to remove disabled enum <~a> which has alias <~a>" type-name (alias enum))
                           (remhash type-name (enums vk-spec))))
                        ((eq (category type) :struct)
                         (let ((struct (gethash type-name (structures vk-spec))))
                           (assert struct
                                   () "trying to remove unknown structure <~a>" type-name)
                           (assert (= (length (aliases struct)) 0)
                                   () "trying to remove disabled structure <~a> which has ~a aliases" type-name (length (aliases struct)))
                           (remhash type-name (structures vk-spec))))
                        (t (error "trying to remove <~a> of unhandled type <~a>" type-name (category type)))))))))
          (let ((extension (make-instance 'extension
                                          :name name
                                          :platform platform
                                          :deprecated-by deprecated-by
                                          :obsoleted-by obsoleted-by
                                          :promoted-to promoted-to
                                          :requires-attribute requirements)))
            (assert (not (gethash name (extensions vk-spec)))
                    () "already encountered extension <~a>" name)
            (setf (gethash name (extensions vk-spec))
                  extension)
            ;; check if extension tag is known: VK_<tag>_<other>
            (let ((tag (second (split-sequence:split-sequence #\_ name))))
              (assert (find tag (tags vk-spec) :test 'string=)
                        () "name <~a> is using an unknown tag <~a>" name tag)
              ;; see VulkanHppGenerator::readExtensionRequire
              (xpath:do-node-set (require-node (xpath:evaluate "require" node))
                (let* ((@extension (xps (xpath:evaluate "@extension" require-node)))
                       (@feature (xps (xpath:evaluate "@feature" require-node)))
                       (require-title (or @extension @feature)))
                  (assert (not (and @extension @feature)) ()
                          "require node is both a feature and an extension")
                  (when @extension
                    (assert (not (find-if (lambda (req)
                                            (string= (title req) @extension))
                                          (require-data extension)))
                            () "require extension <~a> already listed" @extension))
                  (when @feature
                    (assert (gethash @feature (features vk-spec))
                            () "unknown feature <~a>" @feature))
                  (let ((require-data (make-instance 'require-data
                                                     :name require-title
                                                     :title require-title))
                        (require-data-empty-p t)
                        (extension-name (or require-title name)))
                    (xpath:do-node-set (command-node (xpath:evaluate "command" require-node))
                      (parse-extension-require-command command-node name require-data vk-spec)
                      (setf require-data-empty-p nil))
                    (xpath:do-node-set (enum-node (xpath:evaluate "enum" require-node))
                      (parse-require-enum enum-node tag vk-spec))
                    (xpath:do-node-set (type-node (xpath:evaluate "type" require-node))
                      (parse-extension-require-type type-node name require-data vk-spec)
                      (setf require-data-empty-p nil))
                    (unless require-data-empty-p
                      (push require-data
                            (require-data (gethash name (extensions vk-spec))))))))))))))


;; see VulkanHppGenerator::readFeature
(defun parse-features (vk.xml vk-spec)
  "TODO"
  (xpath:do-node-set (node (xpath:evaluate "/registry/feature" vk.xml))
    (let* ((name (xps (xpath:evaluate "@name" node)))
           (feature-number (xps (xpath:evaluate "@number" node))))
      (assert (string= name
                       (concatenate 'string "VK_VERSION_" (substitute #\_ #\. feature-number)))
              () "unexpected formatting of name <~a>" name)
      (assert (not (gethash name (features vk-spec)))
              () "already specified feature <~a>" name)
      (setf (gethash name (features vk-spec))
            (make-instance 'feature
                           :name name
                           :feature-number feature-number))
      ;; see VulkanHppGenerator::readFeatureRequire
      (xpath:do-node-set (require-node (xpath:evaluate "require" node))
        (let ((require-data (make-instance 'require-data))
              (require-data-empty-p t))
          ;; see VulkanHppGenerator::readFeatureRequireCommand
          (xpath:do-node-set (command-node (xpath:evaluate "command" require-node))
            (let* ((command-name (xps (xpath:evaluate "@name" command-node)))
                   (command (gethash command-name (commands vk-spec))))
              (assert command
                      () "feature requires unknown command <~a>" command-name)
              (assert (not (referenced-in command))
                      () "command <~a> already listed with feature <~a>" command-name (referenced-in command))
              (setf (referenced-in command) name)
              (assert (not (find command-name (commands require-data) :test #'string=)) ()
                      "command <~a> already listed in require of feature <~a>" command-name name)
              (push command-name (commands require-data))
              (setf require-data-empty-p nil)))
          ;; see VulkanHppGenerator::readFeatureRequireEnum
          (xpath:do-node-set (enum-node (xpath:evaluate "enum" require-node))
            (parse-require-enum enum-node "" vk-spec))
          ;; see VulkanHppGenerator::readFeatureRequireType
          (xpath:do-node-set (type-node (xpath:evaluate "type" require-node))
            (let ((type-name (xps (xpath:evaluate "@name" type-node))))
              (when (and (not (gethash type-name (defines vk-spec)))
                         (not (find type-name (includes vk-spec) :test 'string=)))
                (let ((type (gethash type-name (types vk-spec))))
                  (assert type
                          () "feature requires unknown type <~a>" type-name)
                  (assert (or (not (referenced-in type))
                              (string= (referenced-in type) name))
                          () "type <~a> already listed on feature <~a>" type-name (referenced-in type))
                  (setf (referenced-in type)
                        name)
                  (push type-name (types require-data))
                  (setf require-data-empty-p nil)))))
          (unless require-data-empty-p
            (push require-data (require-data (gethash name (features vk-spec))))))))))

