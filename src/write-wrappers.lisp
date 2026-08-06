#|
 Copyright(c) 2021 - Lukas Herzberger <herzberger.lukas@gmail.com>
 SPDX-License-Identifier: MIT

 Copyright(c) 2015-2020 - NVIDIA CORPORATION
 SPDX-License-Identifier: Apache-2.0
|#

(in-package :vk-generator)

(defun make-command-docstring (command required-params optional-params output-params vector-params vk-spec)
  ;; todo: reference VkStructs, list arguments, document return type, if command starts with vkGet -> gets foo, if command is registered as a create-func or destroy-func -> Creates/Destroys a foo handle from bla belonging to a bar, document success codes, document error codes (= signalled conditions)
  (flet ((format-arg-doc (param &optional (optional-p nil) (default-arg nil))
           (let ((sequence-p (member param vector-params)))
             (format nil "~% - ~a~@[~a~]: a ~@[~a~]~a~@[, defaults to: ~a~]"
                     (fix-slot-name (name param) (get-type-name param) vk-spec)
                     (when optional-p
                       " (optional)")
                     (when sequence-p
                       "(OR LIST VECTOR) of ")
                     (cond
                       ((gethash (get-type-name param) (structures vk-spec))
                        (format nil "(OR ~a CFFI:FOREIGN-POINTER)~@[~a~]"
                                (fix-type-name (get-type-name param) (tags vk-spec))
                                (when sequence-p " instances")))
                       ((gethash (get-type-name param) (handles vk-spec))
                        (format nil "~a~@[~a~]"
                                (fix-type-name (get-type-name param) (tags vk-spec))
                                (when sequence-p " handles")))
                       ((not (gethash (get-type-name param) *vk-platform*))
                        (format nil "~a~@[~a~]"
                                (fix-type-name (get-type-name param) (tags vk-spec))
                                (when sequence-p "s")))
                       (t (format nil "~:@(~a~)"
                                  (get-type-to-declare (get-type-name param) vk-spec param vector-params))))
                     default-arg))))
    (let ((referenced-types (sort
                             (remove-duplicates
                              (loop for param in (concatenate 'list
                                                              required-params
                                                              optional-params
                                                              output-params)
                                    for type-name = (get-type-name param)
                                    unless (gethash type-name *vk-platform*)
                                    collect (fix-type-name type-name (tags vk-spec))))
                             #'string<))
          (formatted-required-args (loop for param in required-params
                                         collect (format-arg-doc param)))
          (formatted-optional-args (loop for param in optional-params
                                         collect (format-arg-doc
                                                  param
                                                  t
                                                  (cond
                                                    ((string= "pAllocator" (name param))
                                                     "*DEFAULT-ALLOCATOR*")
                                                    (t "NIL")))))
          (formatted-output-args (loop for param in (remove-if (lambda (p)
                                                                 (member (name p)
                                                                         (map 'list #'len vector-params)
                                                                         :test #'string=))
                                                               output-params)
                                       collect (let ((array-arg-p (member param vector-params)))
                                                 (format nil "~a~a"
                                                         (if (gethash (get-type-name param) *vk-platform*)
                                                             (cond
                                                               ((or (string= "size_t" (get-type-name param))
                                                                    (alexandria:starts-with-subseq "uint" (get-type-name param)))
                                                                "UNSIGNED-BYTE")
                                                               ((alexandria:starts-with-subseq "int" (get-type-name param))
                                                                "INTEGER")
                                                               (t (error "unhandled output arg in doc generation: ~a" (get-type-name param))))
                                                             (fix-type-name (get-type-name param) (tags vk-spec)))
                                                         (if array-arg-p "s" ""))))))
      (when (needs-explicit-loading-p command)
        (setf formatted-optional-args
              (concatenate 'list
                           formatted-optional-args
                           (list (format nil "~% - EXTENSION-LOADER (optional): an EXTENSION-LOADER, defaults to: *DEFAULT-EXTENSION-LOADER*"))))
        (setf referenced-types
              (sort
               (concatenate 'list
                            referenced-types
                            '("EXTENSION-LOADER"))
               #'string<)))
      (unless (string= (return-type command) "void")
        (setf formatted-output-args
              (concatenate 'list
                           formatted-output-args
                           (list (cond
                                   ((or (string= "size_t" (return-type command))
                                        (alexandria:starts-with-subseq "uint" (return-type command)))
                                    "UNSIGNED-BYTE")
                                   ((alexandria:starts-with-subseq "int" (return-type command))
                                    "INTEGER")
                                   ((string= "PFN_vkVoidFunction" (return-type command))
                                    "CFFI:FOREIGN-POINTER")
                                   ((string= "VkResult" (return-type command))
                                    "RESULT")
                                   ((string= "VkBool32" (return-type command))
                                    "BOOLEAN")
                                   ((string= "VkDeviceSize" (return-type command))
                                    "DEVICE-SIZE")
                                   ((string= "VkDeviceAddress" (return-type command))
                                    "DEVICE-ADDRESS")
                                   (t (error "unhandled return type in doc generation: ~a" (return-type command)))))))
        (when (and (alexandria:starts-with-subseq "Vk" (return-type command))
                   (not (string= "VkBool32" (return-type command))))
          (setf referenced-types
                (sort
                 (remove-duplicates
                  (concatenate 'list
                               referenced-types
                               (list (fix-type-name (return-type command) (tags vk-spec)))))
                 #'string<))))
      (format nil "Represents [~a](https://www.khronos.org/registry/vulkan/specs/1.2-extensions/man/html/~a.html).

Args:~{~a~}~{~a~}~@[

Returns:
  (CL:VALUES~{~%    ~a~})~]~@[

Success codes:~{~% - ~a~}~]~@[

Errors signalled on codes:~{~% - ~a~}~]
~{~%See ~a~}~@[
See ~a~]~@[
See ~a~]
"
              (name command)
              (name command)
              formatted-required-args
              formatted-optional-args
              formatted-output-args
              (loop for c in (success-codes command)
                    collect (fix-bit-name c (tags vk-spec)))
              (loop for c in (error-codes command)
                    collect (fix-bit-name c (tags vk-spec)))
              referenced-types
              (when (member-if (lambda (p)
                                 (string= "pAllocator" (name p)))
                               (params command))
                "*DEFAULT-ALLOCATOR*")
              (when (needs-explicit-loading-p command)
                "*DEFAULT-EXTENSION-LOADER*")))))

(defun get-type-to-declare (type-name vk-spec &optional param vector-params)
  (cond
    ;; the types that can't be translated and must not come in a list
    ((and (gethash type-name *misc-os-types*)
          (not (consp (gethash type-name *misc-os-types*))))
     (cond
       ((member (gethash type-name *misc-os-types*) '(:uint32 :ulong))
        "unsigned-byte")
       (t (error ":Unknown MISC-OS-TYPE: ~a" type-name))))
    ((or (string= "void" type-name)
         (find type-name *special-base-types* :test #'string=)
         (and (gethash type-name (types vk-spec))
              (eq :requires (category (gethash type-name (types vk-spec))))
              (not (gethash type-name *vk-platform*))))
     "cffi:foreign-pointer")
    ((and param
          vector-params
          (find param vector-params))
     "(or list vector)")
    ;; handles must come after "list" because a list of handles should be declared as type list
    ((handlep type-name vk-spec)
     (format nil "~(~a~)"
             (fix-type-name type-name (tags vk-spec))))
    ((string= "VkBool32" type-name)
     "boolean")
    ((or (alexandria:starts-with-subseq "float" type-name)
         (string= "double" type-name))
     "real")
    ((or (alexandria:starts-with-subseq "uint" type-name)
         (string= "size_t" type-name))
     "unsigned-byte")
    ((alexandria:starts-with-subseq "int" type-name)
     "integer")
    ((string= "char" type-name)
     "string")
    ((get-structure-type type-name vk-spec)
     (format nil "(or vk:~(~a~) cffi:foreign-pointer)"
             (fix-type-name (name (get-structure-type type-name vk-spec)) (tags vk-spec))))
    ((gethash type-name (bitmasks vk-spec))
     "(or unsigned-byte list)")
    ((or (and (gethash type-name (enums vk-spec))
              (not (is-bitmask-p (gethash type-name (enums vk-spec)))))
         (search "FlagBits" type-name))
     "keyword")
    ((gethash type-name (base-types vk-spec))
     (get-type-to-declare (type-name (gethash type-name (base-types vk-spec))) vk-spec))
    (t (error "No type declaration for: ~a" type-name))))

(defun format-type-to-declare (param vector-params vk-spec)
  (format nil "~(~a~)"
          (let ((type-name (get-type-name param)))
            (get-type-to-declare type-name vk-spec param vector-params))))


(defun format-required-args (required-args vector-params vk-spec)
  "Maps a list of PARAM-DATA instances into a list of argument names for the lambda list of a function in package VK."
  (loop for arg in required-args
        for i from 1
        collect (format nil "~((~a ~a)~)~@[ ~]"
                        (fix-slot-name (name arg) (get-type-name arg) vk-spec)
                        (format-type-to-declare arg vector-params vk-spec)
                        (< i (length required-args)))))

(defun format-optional-args (optional-args vector-params vk-spec &optional unused-args)
  "Maps a list of PARAM-DATA instances into a list of default argument lists for the lambda list of a function in package VK."
  (loop for arg in optional-args
        for i from 1
        collect (format nil "((~(~a ~a) ~a~@[ t~]~))~@[ ~]"
                        (fix-slot-name (name arg) (get-type-name arg) vk-spec)
                        (determine-param-default-value-string arg vk-spec)
                        (format-type-to-declare arg vector-params vk-spec)
                        (member arg unused-args)
                        (< i (length optional-args)))))

(defun format-type-name (type-name vk-spec)
  (cond
    ((find type-name *special-base-types* :test #'string=)
     "'(:pointer :void)")
    ((structure-type-p type-name vk-spec)
     (format nil "'(:~a %vk:~(~a~))"
             (if (and (gethash type-name (structures vk-spec))
                      (is-union-p (gethash type-name (structures vk-spec))))
                 "union"
                 "struct")
             (fix-type-name type-name (tags vk-spec))))
    ((gethash type-name (handles vk-spec))
     (format nil "'%vk:~(~a~)" (fix-type-name type-name (tags vk-spec))))
    ((gethash type-name (enums vk-spec))
     (format nil "'%vk:~(~a~)" (fix-type-name type-name (tags vk-spec))))
    ((string= "void" type-name)
     "'(:pointer :void)")
    ((string= "char" type-name)
     ":string")
    ((string= "size_t" type-name)
     ":size")
    ((gethash type-name *vk-platform*)
     (format nil "~(~s~)" (gethash type-name *vk-platform*)))
    ((gethash type-name *misc-os-types*)
     (format nil "~:[~;'~]~(~s~)"
             (consp (gethash type-name *misc-os-types*))
             (gethash type-name *misc-os-types*)))
    ((and (gethash type-name (types vk-spec))
          (eq :requires (category (gethash type-name (types vk-spec)))))
     "'(:pointer :void)")
    (t
     (format nil "'%vk:~(~a~)" (fix-type-name type-name (tags vk-spec))))))

(defun format-arg-type (arg vk-spec)
  ""
  (let ((formatted (format-type-name (get-type-name arg) vk-spec)))
    (if (pointer-to-non-char-pointer-p (type-info arg))
        (format nil "'(:pointer (:pointer ~a))"
                (if (alexandria:starts-with #\' formatted)
                    (subseq formatted 1)
                    formatted))
        formatted)))

(defun make-arg-qualifier-list (arg output-params optional-params vector-params vk-spec &optional unused-params)
  (let ((qualifiers nil))
    (when (find arg optional-params)
      (push :optional qualifiers))
    (when (member arg vector-params)
      (push :list qualifiers))
    ;; todo: maybe just split all of these up into :raw and :translate - this would be way less confusing...
    ;; todo: void pointers should also be treaded as :raw instead of :handle to get rid of the ambiguity
    (when (or (and (gethash (get-type-name arg) *vk-platform*)
                   (value-p (type-info arg)))
              (and (string= "char" (get-type-name arg))
                   (not (string= "**" (postfix (type-info arg)))))
              (gethash (get-type-name arg) (enums vk-spec))
              (gethash (get-type-name arg) (bitmasks vk-spec))
              (and (gethash (get-type-name arg) (base-types vk-spec))
                   (value-p (type-info arg))))
      (push :raw qualifiers))
    (when (or (gethash (get-type-name arg) (handles vk-spec)) ;; it's a handle
              (string= "void" (get-type-name arg)) ;; it's a void pointer
              (and (gethash (get-type-name arg) (types vk-spec)) ;; it's a vk-defined type
                   (eq :requires (category (gethash (get-type-name arg) (types vk-spec))))
                   (not (gethash (get-type-name arg) *vk-platform*))))
      (push :handle qualifiers))
    (when (handlep (get-type-name arg) vk-spec)
      (push (if (non-dispatch-handle-p (get-handle (get-type-name arg) vk-spec))
                :non-dispatchable
                :dispatchable)
            qualifiers))
    (push (if (find arg output-params)
              :out
              :in)
          qualifiers)
    ;; structs which can be used to query via their next-slot are also :in args even though they are present in the output-params
    (when (and (member arg output-params)
               (member arg optional-params))
      (push :in qualifiers))
    (when (member arg unused-params)
      (push :ignore-in qualifiers))
    qualifiers))

(defun format-vk-args (vk-args count-to-vector-param-indices output-params optional-params vector-params vk-spec &optional singular-in-out-params unused-params)
  ""
  (loop for arg in vk-args
        for i from 0
        for arg-name = (fix-slot-name (name arg) (get-type-name arg) vk-spec)
        collect (format nil "(~(~a ~a ~a~{ ~s~}~))~@[~%~]"
                        arg-name
                        (format-arg-type arg vk-spec)
                        (cond
                          ((member arg singular-in-out-params)
                           (format nil "~((or ~a (vk:make-~a))~)"
                                   arg-name
                                   (fix-type-name (name (get-structure-type (get-type-name arg) vk-spec))
                                                  (tags vk-spec))))
                          ((and (gethash i count-to-vector-param-indices)
                                (not (gethash (get-type-name arg)
                                              (structures vk-spec)))
                                (not (find arg output-params)))
                           (let* ((array-args (loop for j in (gethash i count-to-vector-param-indices)
                                                    for array-arg = (nth j vk-args)
                                                    unless (find array-arg output-params)
                                                    collect array-arg))
                                  (array-arg (first array-args)))
                             (format nil "(length ~(~a~))"
                                     (fix-slot-name (name array-arg) (get-type-name array-arg) vk-spec))))
                          (t (fix-slot-name (name arg) (get-type-name arg) vk-spec)))
                        (make-arg-qualifier-list arg output-params optional-params vector-params vk-spec unused-params)
                        (< (+ i 1) (length vk-args)))))

(defun write-defvkfun (out
                       fixed-function-name
                       required-params
                       optional-params
                       kw-args
                       docstring
                       vk-args)
  "Writes a DEFVKFUN form. All arguments to this function are already formatted."
  (format out "
(defvkfun (~(~a~)
           ~(%vk:~a~)
           (~(~{~a~}~))
           (~(~{~a~}~))~{
           ~a~})
  ~s
~{  ~(~a~)~})~%"
          fixed-function-name
          fixed-function-name
          required-params
          optional-params
          kw-args
          docstring
          vk-args))

;; todo: check if vkGetShaderInfoAMD is written correctly (what is the info parameter exactly? - check spec)
(defun write-command (out command vk-spec)
  (let* ((fixed-function-name (fix-function-name (name command) (tags vk-spec)))
         ;; maps from array-param to array-count-param
         (vector-param-indices (determine-vector-param-indices (params command) vk-spec))
         (count-to-vector-param-indices (reverse-hash-table vector-param-indices))
         (vector-params (loop for i in (alexandria:hash-table-keys vector-param-indices)
                              collect (nth i (params command))))
         (vector-count-params (loop for i in (alexandria:hash-table-values vector-param-indices)
                                    collect (nth i (params command))))
         (non-const-pointer-param-indices (determine-non-const-pointer-param-indices (params command)))
         (output-params (loop for i in non-const-pointer-param-indices
                              collect (nth i (params command))))
         (handle-params (remove-if-not (lambda (p)
                                         (and (not (find p output-params))
                                              (gethash (get-type-name p) (handles vk-spec))))
                                       (params command)))
         (non-struct-params (sorted-elements
                             (remove-if (lambda (p)
                                          (or (gethash (get-type-name p) (structures vk-spec))
                                              (gethash (get-type-name p) (handles vk-spec))
                                              (find p vector-count-params)
                                              (find p output-params)))
                                        (params command))))
         (struct-params (sorted-elements
                         (remove-if-not (lambda (p)
                                          (and (gethash (get-type-name p) (structures vk-spec))
                                               (not (find p output-params))))
                                        (params command))))
         (required-handle-params (remove-if (lambda (p)
                                              (optional-p p))
                                            handle-params))
         (required-non-struct-params (remove-if (lambda (p)
                                                  (optional-p p))
                                                non-struct-params))
         (required-struct-params (remove-if (lambda (p)
                                              (optional-p p))
                                            struct-params))
         (optional-handle-params (remove-if-not (lambda (p)
                                                  (optional-p p))
                                                handle-params))
         (optional-non-struct-params (remove-if-not (lambda (p)
                                                      (optional-p p))
                                                    non-struct-params))
         (optional-struct-params (remove-if-not (lambda (p)
                                                  (optional-p p))
                                                struct-params))
         (required-params (sorted-elements (concatenate 'list required-handle-params required-non-struct-params required-struct-params)))
         (optional-params (sorted-elements (concatenate 'list optional-handle-params optional-non-struct-params optional-struct-params)))
         (command-type (first (determine-command-type-2 command vk-spec)))
         (kw-args (when (needs-explicit-loading-p command)
                    '(":extension-p t")))
         (singular-out-params nil)
         (unused-params nil))
    ;; todo: check create-debug-utils-messenger example!

    ;; set kw args and other special args
    (assert (member command-type
                    '(:no-output-param
                      :fill-void-pointer
                      :get-or-create-handle
                      :create-handles
                      :allocate-handles
                      :get-value
                      :get-struct
                      :get-struct-chain
                      :get-structs
                      :get-struct-chains
                      :enumerate-values
                      :enumerate-handles
                      :enumerate-structs
                      :enumerate-struct-chains
                      :get-value-array-and-value
                      :enumerate-two-struct-chains))
            () "Never encountered a function like <~a>!" (name command))
    (when (member command-type
                  '(:no-output-param
                    :fill-void-pointer))
      (pushnew
        (format nil ":trivial-return-type ~(~a~)"
                (if (not (or (member (return-type command)
                                     '("VkBool32"
                                       "VkResult"
                                       "VkDeviceAddress"
                                       "VkDeviceSize")
                                     :test #'string=)
                             (gethash (return-type command) *vk-platform*)))
                    (format-type-name (return-type command) vk-spec)
                    ":trivial"))
        kw-args))
    (when (or (and (member command-type
                        '(:get-or-create-handle
                          :create-handles
                          :allocate-handles
                          :get-value
                          :get-structs
                          :get-struct-chains))
                   (string= "void" (return-type command)))
              (member command-type
                      '(:get-struct
                        :get-struct-chain)))
      (pushnew ":no-vk-result-p t"
               kw-args))
    (when (member command-type
                  '(:get-struct-chain
                    :get-struct-chains
                    :enumerate-struct-chains
                    :enumerate-two-struct-chains))
      (pushnew ":returns-struct-chain-p t"
               kw-args))
    (when (member command-type
                  '(:enumerate-values
                    :enumerate-handles
                    :enumerate-structs
                    :enumerate-struct-chains
                    :enumerate-two-struct-chains))
      (pushnew ":enumerate-p t"
                kw-args))
    (when (member command-type
                  '(:create-handles
                    :allocate-handles))
      (pushnew
       (format nil ":len-provider ~(~a~)"
               (if (= (length vector-params) 2)
                   ;; the length of the output array is the same size as an input array
                   (format nil "(length ~(~a~))"
                           (let ((array-arg (find-if-not (lambda (arg)
                                                           (find arg output-params))
                                                         vector-params)))
                             (fix-slot-name (name array-arg) (get-type-name array-arg) vk-spec)))
                   ;; the length of the output array depends on the slot of an input parameter
                   ;; now we need to find out which slot this is and - more importantly - if it is one
                   ;; of the omitted slots because it redundantly describes the length of yet another
                   ;; slot of the parameter
                   (let* ((split-len-data (split-len-by-struct-member (len (first output-params))))
                          (count-arg (find-if (lambda (p)
                                                (string= (first split-len-data) (name p)))
                                              (params command)))
                          (count-struct (gethash (get-type-name count-arg) (structures vk-spec)))
                          (struct-count-member-names (get-count-member-names count-struct))
                          (count-slot-name (second split-len-data)))
                     (if (member count-slot-name struct-count-member-names :test #'string=)
                         (let ((count-slot (find-if (lambda (m)
                                                      (string= count-slot-name (car (len m))))
                                                    (members count-struct))))
                           (format nil "(length (vk:~(~a ~a~)))"
                                   (fix-slot-name (name count-slot) (get-type-name count-slot) vk-spec t)
                                   (fix-slot-name (name count-arg) (get-type-name count-arg) vk-spec)))
                         (let ((count-slot (find-if (lambda (m)
                                                      (string= count-slot-name (name m)))
                                                    (members count-struct))))
                           (format nil "(vk:~(~a ~a~))"
                                   (fix-slot-name (name count-slot) (get-type-name count-slot) vk-spec t)
                                   (fix-slot-name (name count-arg) (get-type-name count-arg) vk-spec)))))))
       kw-args))
    (when (eq command-type :get-struct-chain)
      (setf optional-params (sorted-elements (concatenate 'list optional-params (list (first output-params)))))
      (setf singular-out-params (list (first output-params))))
    (when (member command-type
                  '(:get-structs
                    :get-struct-chains
                    :enumerate-values
                    :enumerate-handles
                    :enumerate-structs
                    :enumerate-struct-chains
                    :get-value-array-and-value))
      (let ((array-arg (find-if #'len output-params))
            (count-arg (find-if-not #'len output-params)))
        (pushnew
         (format nil ":first-array-arg-name ~(~a~)"
                 (fix-slot-name (name array-arg) (get-type-name array-arg) vk-spec))
         kw-args)
        (if (eq command-type :get-value-array-and-value)
            (pushnew
             (format nil ":len-provider ~((length ~a)~)"
                     (let ((array-arg (find-if-not (lambda (arg) ;; len-provider is the length of the input array
                                                     (find arg output-params))
                                                   vector-params)))
                       (fix-slot-name (name array-arg) (get-type-name array-arg) vk-spec)))
             kw-args)
            (pushnew
             (format nil ":count-arg-name ~(~a~)"
                     (fix-slot-name (name count-arg) (get-type-name count-arg) vk-spec))
             kw-args))
        (when (member command-type
                      '(:get-struct-chains
                        :enumerate-struct-chains))
          (setf optional-params (sorted-elements (concatenate 'list optional-params (list array-arg)))))
        (when (eq command-type :get-struct-chains)
          (pushnew
           (format nil ":vk-constructor vk:make-~(~a~)"
                   (fix-type-name (get-type-name array-arg) (tags vk-spec)))
           kw-args))))
    (when (eq command-type :enumerate-two-struct-chains)
      (let* ((array-args (remove-if-not #'len output-params))
             (first-arg (first array-args))
             (second-arg (second array-args))
             (count-arg (find-if-not #'len output-params)))
        (setf optional-params (sorted-elements (concatenate 'list optional-params array-args)))
        (setf unused-params array-args)
        (pushnew
         (format nil ":first-array-arg-name ~(~a~)"
                 (fix-slot-name (name first-arg) (get-type-name first-arg) vk-spec))
         kw-args)
        (pushnew
         (format nil ":second-array-arg-name ~(~a~)"
                 (fix-slot-name (name second-arg) (get-type-name second-arg) vk-spec))
         kw-args)
        (pushnew
         (format nil ":count-arg-name ~(~a~)"
                 (fix-slot-name (name count-arg) (get-type-name count-arg) vk-spec))
         kw-args)))
    (when (and (member command-type '(:get-or-create-handle
                                      :create-handles
                                      :allocate-handles
                                      :enumerate-handles))
               ;; note: I only added this check for safety
               (some (lambda (p)
                       (handlep (get-type-name p) vk-spec))
                     output-params))
      (pushnew
       (format nil ":handle-constructor make-~(~a~)-wrapper"
               (fix-type-name
                (find-if (lambda (p)
                           (handlep p vk-spec))
                         (map 'list (lambda (p)
                                      (get-type-name p))
                              output-params))
                (tags vk-spec)))
       kw-args))
    ;; write def
    (write-defvkfun out
                    (fix-function-name (name command) (tags vk-spec))
                    (format-required-args required-params vector-params vk-spec)
                    (format-optional-args optional-params vector-params vk-spec
                                          unused-params)
                    kw-args
                    (make-command-docstring command required-params optional-params output-params vector-params vk-spec)
                    (format-vk-args (params command) count-to-vector-param-indices output-params optional-params vector-params vk-spec
                                    singular-out-params
                                    unused-params))))

(defun write-vk-functions (vk-functions-file vk-spec &optional dry-run)
  (flet ((write-commands (stream)
           (format stream ";;; this file is automatically generated, do not edit~%")
           (format stream "#||~%~a~%||#~%~%" (vulkan-license-header vk-spec))
           (format stream "(in-package :vk)~%~%")

           (loop for command in (sorted-elements (alexandria:hash-table-values (commands vk-spec)))
                 do (write-command stream command vk-spec)
                 when (alias command)
                 do (loop for alias in (alexandria:hash-table-values (alias command))
                          do (write-command stream (make-aliased-command command alias) vk-spec)))))
    (if dry-run
        (write-commands t)
        (with-open-file (out vk-functions-file :direction :output :if-exists :supersede)
          (write-commands out)))))

