#|
 Copyright(c) 2020 - Lukas Herzberger <herzberger.lukas@gmail.com>
 SPDX-License-Identifier: MIT

 Copyright(c) 2015-2020 - NVIDIA CORPORATION
 SPDX-License-Identifier: Apache-2.0
|#

(in-package #:vulkan-spec)

(defun parse-command-name (node vk-spec)
  "TODO"
  (let ((name-data (parse-name-data (xpath:first-node (xpath:evaluate "proto" node)))))
    (assert (alexandria:starts-with-subseq "vk" (name name-data))
            () "name <~a> does not begin with <vk>" (name name-data))
    (assert (not (array-sizes name-data))
            () "name <~a> with unsupported array-sizes <~a>" (name name-data) (array-sizes name-data))
    (assert (not (bit-count name-data))
            () "name <~a> with unsupported bit-count <~a>" (name name-data) (bit-count name-data))
    (assert (not (gethash (name name-data) (commands vk-spec)))
            () "command <~a> already specified" (name name-data))
    (name name-data)))

(defun parse-command-return-type (node vk-spec)
  "TODO"
  (let ((type-info (parse-type-info (xpath:first-node (xpath:evaluate "proto" node)))))
    (assert (gethash (type-name type-info) (types vk-spec))
            () "unknown type <~a>" (type-name type-info))
    (assert (not (prefix type-info))
            () "unexpected type prefix <~a>" (prefix type-info))
    (assert (not (postfix type-info))
            () "unexpected type postfix <~a>" (postfix type-info))
    (type-name type-info)))

(defun is-param-indirect (name params vk-spec)
  (let ((delimiter-pos (or (search "->" name) (search "::" name))))
    (when (and delimiter-pos (< delimiter-pos (- (length name) 2)))
      (let* ((param-name (subseq name 0 delimiter-pos))
             (param-member (subseq name (+ delimiter-pos 2)))
             (param (find-if (lambda (p)
                               (string= (name p) param-name))
                             params)))
        (when param
          (let ((struct (gethash (get-type-name param) (structures vk-spec))))
            (when struct
              (find-if (lambda (m)
                         (string= (name m) param-member))
                       (members struct)))))))))

(defun parse-command-param (node params vk-spec)
  "TODO"
  (let ((len (xps (xpath:evaluate "@len" node)))
        (optional-p (parse-boolean (xpath:evaluate "@optional" node)))
        (name-data (parse-name-data node))
        (type-info (parse-type-info node)))
    (assert (or (not len)
                (string= len "null-terminated")
                (find-if (lambda (p)
                           (string= (name p) len))
                         params)
                (is-param-indirect len params vk-spec))
            () "command param len <~a> is not recognized as a valid len value" len)
    (assert (not (bit-count name-data))
            () "name <~a> with unsupported bit-count <~a>" (name name-data) (bit-count name-data))
    (assert (gethash (type-name type-info) (types vk-spec))
            () "unknown type <~a>" (type-name type-info))
    (assert (or (not (prefix type-info))
                (string= (prefix type-info) "const")
                (string= (prefix type-info) "const struct")
                (string= (prefix type-info) "struct"))
            () "unexpected type prefix <~a>" (prefix type-info))
    (assert (or (not (postfix type-info))
                (string= (postfix type-info) "*")
                (string= (postfix type-info) "**")
                (string= (postfix type-info) "* const*"))
            () "unexpected type postfix <~a>" (postfix type-info))
    (assert (not (find-if (lambda (p)
                            (string= (name name-data) (name p)))
                          params))
            () "command param <~a> already used" (name name-data))
    (make-instance 'param
                   :name (name name-data)
                   :array-sizes (array-sizes name-data)
                   :type-info type-info
                   :len len
                   :optional-p optional-p)))

(defun register-creator (command vk-spec)
  "TODO"
  (let* ((params (params command))
         (num-params (length params))
         (key nil)
         (value-index nil)
         (handle-arg (find-if (lambda (p)
                                (and (handlep (get-type-name p) vk-spec)
                                     (non-const-pointer-p (type-info p))))
                              params)))
    (assert handle-arg
            () "creator has no handle parameter <~a>" command)
    (assert (or (not (string= (subseq (name command) 2 8) "Create"))
                (string= "VkAllocationCallbacks"
                         (get-type-name (elt params (- num-params 2)))))
            () "unexpected second to last parameter for creator <~a>" (get-type-name (elt params (- num-params 2))))
    (push (name command)
          (create-commands (get-handle (get-type-name handle-arg) vk-spec)))))

(defun register-deleter (command vk-spec)
  "TODO"
  (let* ((params (params command))
         (num-params (length params))
         (key nil)
         (value-index nil))
    (cond
      ((or (= 2 num-params)
           (= 3 num-params))
       (assert (string= (get-type-name (alexandria:lastcar params))
                        "VkAllocationCallbacks")
               () "unexpected last paramer for deleter <~a>" (get-type-name (alexandria:lastcar params)))
       (if (= num-params 2)
           (setf key "")
           (setf key (get-type-name (first params))))
       (setf value-index (- num-params 2)))
      ((= num-params 4)
       (setf key (get-type-name (first params)))
       (setf value-index 3)
       (let ((handle (gethash (get-type-name (nth value-index params)) (handles vk-spec))))
         (assert (gethash (get-type-name (nth value-index params)) (handles vk-spec))
                 () "missing handle <~a>" (get-type-name (nth value-index params)))
         (setf (delete-pool handle)
               (get-type-name (first params)))))
      (t (error "illegal number of params for deleter <~a>" num-params)))
    (let* ((value-name (get-type-name (nth value-index params)))
           (key-handle (gethash key (handles vk-spec)))
           (handle (gethash value-name (handles vk-spec))))
      (assert (and key-handle
                   (not (find value-name (children handle))))
              () "handle <~a> already specified as a child of <~a>" value-name key)
      (push value-name (children key-handle))
      (assert handle
              () "missing handle <~a>" value-name)
      (setf (delete-command handle) (name command)))))

(defun parse-commands (vk.xml vk-spec)
  "TODO"
  (xpath:do-node-set (node (xpath:evaluate "/registry/commands/command" vk.xml))
    (let ((alias (xps (xpath:evaluate "@alias" node))))
      (if alias
          (let ((name (xps (xpath:evaluate "@name" node)))
                (command (gethash alias (commands vk-spec))))
            (assert (alexandria:starts-with-subseq "vk" name)
                    () "name <~a> should begin with <vk>" name)
            (assert command
                    () "missing command <~a>" alias)
            (assert (not (gethash name (alias command)))
                    () "command <~a> already listed as alias to <~a>" name alias)
            (setf (gethash name (alias command))
                  (make-instance 'command-alias
                                 :name name)))
          (let* ((error-codes (tokenize (xps (xpath:evaluate "@errorcodes" node))))
                 (success-codes (tokenize (xps (xpath:evaluate "@successcodes" node))))
                 (name (parse-command-name node vk-spec))
                 (return-type (parse-command-return-type node vk-spec))
                 (command (make-instance 'command
                                         :name name
                                         :return-type return-type
                                         :success-codes success-codes
                                         :error-codes error-codes)))
            (xpath:do-node-set (param-node (xpath:evaluate "param" node))
              (push (parse-command-param param-node (params command) vk-spec)
                    (params command)))
            (setf (params command) (reverse (params command)))
            (when (or (string= (subseq name 2 9) "Destroy")
                      (string= (subseq name 2 6) "Free"))
              (register-deleter command vk-spec))
            (when (or (and (>= (length name) 8)
                           (string= (subseq name 2 8) "Create"))
                      (and (>= (length name) 10)
                           (string= (subseq name 2 10) "Allocate")))
              (register-creator command vk-spec))

            (assert (> (length (params command)) 0)
                    () "command <~a> with no params" name)
            (let ((handle (gethash (name (first (params command))) (handles vk-spec))))
              (unless handle
                (setf handle (gethash "" (handles vk-spec))))
              (assert handle
                      () "could not find a handle to hold command <~a>" name)
              (setf (handle command) (name handle))
              (assert (not (gethash name (commands vk-spec)))
                      () "already encountered command <~a>" name)
              (setf (gethash name (commands vk-spec))
                    command)
              (assert (not (find name (commands handle) :test 'string=))
                      () "command list of handle <~a> already holds a command <~a>" (name handle) name)
              (push name (commands handle))))))))
