#|
 Copyright(c) 2021 - Lukas Herzberger <herzberger.lukas@gmail.com>
 SPDX-License-Identifier: MIT

 Copyright(c) 2015-2020 - NVIDIA CORPORATION
 SPDX-License-Identifier: Apache-2.0
|#

(in-package #:vulkan-spec)

(defun reverse-hash-table (hash-table)
  (let ((result (make-hash-table)))
    (loop for k being each hash-key of hash-table using (hash-value v)
          do (push k (gethash v result)))
    result))

(defun split-len-by-struct-member (name)
  "Splits a given len NAME into two parts: the name of the referenced parameter and the slot name containing the length within the referenced parameter.
Returns NIL if the given NAME does not reference a slot of another parameter."
  (let ((name-parts (cl-ppcre:split "->" name)))
    (when (= (length name-parts) 1)
      ;; older version of the Vulkan API registry used the notation parameter::member instead of parameter->member
      (setf name-parts (cl-ppcre:split "::" name)))
    (when (and (= (length name-parts) 2))
      name-parts)))

(defun len-by-struct-member-p (name param vk-spec)
  "Checks if a given NAME references a MEMBER-DATA instance of a STRUCT instance and this STRUCT instances is the type of the given PARAM instance.
 
E.g.: In \"vkAllocateDescriptorSets\" the \"len\" \"pAllocateInfo->descriptorSetCount\" of parameter \"pDescriptorSets\" references the parameter \"pAllocateInfo\" of type \"VkDescriptorSetAllocateInfo\" which has a member named \"descriptorSetCount\".
"
  (let ((name-parts (cl-ppcre:split "->" name)))
    (when (= (length name-parts) 1)
      ;; older version of the Vulkan API registry used the notation parameter::member instead of parameter->member
      (setf name-parts (cl-ppcre:split "::" name)))
    (when (and (= (length name-parts) 2)
               (string= (first name-parts) (name param)))
      (let ((struct (gethash (get-type-name param) (structures vk-spec))))
        (assert struct
                () "Undefined structure <~a>" (get-type-name param))
        (assert (find-if (lambda (m)
                           (string= (second name-parts) (name m)))
                         (members struct))
                () "Structure <~a> has no member named <~a>" (name struct) (second name-parts))
        t))))

(defun structure-chain-anchor-p (struct-name vk-spec)
  "Determines if STRUCT-NAME names a struct type in the Vulkan API that is a structure chain anchor."
  (when (alexandria:starts-with-subseq "Vk" struct-name)
    (let ((struct (or (gethash struct-name (structures vk-spec))
                      (find-if (lambda (s)
                                 (find struct-name (aliases s) :test #'string=))
                               (alexandria:hash-table-values (structures vk-spec))))))
      (and struct
           (find (name struct) (extended-structs vk-spec) :test #'string=)))))

(defun determine-vector-param-indices (params vk-spec &optional (ignore-void t))
  "Creates a mapping of indices of array arguments to indices of the arguments specifying the number of elements within the array in a given sequence of PARAM instances.

E.g.: In \"vkQueueSubmit\" the parameter \"submitCount\" specifies the number of \"VkSubmitInfo\" instances in \"pSubmits\".

Note: For arbitrary data sizes (i.e. the vector parameter is a void pointer) the vector parameter and its size parameter are ignored by default.
Both are treated as unrelated input parameters of the resulting function.
E.g.: \"pData\" and \"dataSize\" in \"vkGetQueryPoolResults\".
To disable this behaviour pass NIL as IGNORE-VOID parameter.
"
  (let ((vector-param-indices (make-hash-table :test 'equal)))
    (loop for param in params and param-index from 0
          for len = (len param)
          when (and len
                    (or (not ignore-void)
                        (not (string= "void" (get-type-name param)))))
          do (let ((len-param-index
                     (position-if
                      (lambda (p)
                        (or (string= len (name p))
                            (len-by-struct-member-p len p vk-spec)))
                      params)))
               (when len-param-index
                 (setf (gethash param-index vector-param-indices) len-param-index))))
    vector-param-indices))

(defun determine-count-to-vector-param-indices (params vk-spec)
  (reverse-hash-table (determine-vector-param-indices params vk-spec)))

(defun determine-non-const-pointer-param-indices (params &optional (ignore-void t))
  "Find all indices of PARAM instances describing output arguments in a given sequence of PARAM instances.

Note: By default void* are ignored and treated as input arguments. The same goes for meta data of void*
(e.g. size_t* pDataSize for a void* data).
To disable this behaviour pass NIL as the IGNORE-VOID parameter.
"
  (loop for param in params and param-index from 0
        when (and (non-const-pointer-p (type-info param))
                  (not (find (get-type-name param) *special-pointer-types* :test #'string=))
                  (or (not ignore-void)
                      (and (not (string= "void" (get-type-name param)))
                           ;; filter out counter params for void pointers (e.g. size_t* pDataSize for void* data)
                           (not (and (string= "size_t" (get-type-name param))
                                     (member-if (lambda (p)
                                                  (and (len p)
                                                       (string= "void" (get-type-name p))
                                                       (string= (len p) (name param))))
                                                params))))))
        collect param-index))

(defun determine-const-pointer-param-indices (params)
  "Find all indices of PARAM instances describing const pointer arguments in a given sequence of PARAM instances."
  (loop for param in params and param-index from 0
        when (or (const-pointer-p (type-info param))
                 (and (not (non-const-pointer-p (type-info param)))
                      (member (get-type-name param) *special-pointer-types* :test #'string=)))
        collect param-index))

(defun get-vector-params (command vk-spec)
  (with-slots (params) command
    (loop for i in (alexandria:hash-table-keys
                    (determine-vector-param-indices params vk-spec))
          collect (nth i params))))

(defun get-vector-count-params (command vk-spec)
  (with-slots (params) command
    (loop for i in (alexandria:hash-table-values
                    (determine-vector-param-indices params vk-spec))
          collect (nth i params))))

(defun get-output-params (command)
  (with-slots (params) command
    (loop for i in (determine-non-const-pointer-param-indices params)
          collect (nth i params))))

(defun get-handle-params (command vk-spec)
  (let ((output-params (get-output-params command)))
    (remove-if-not (lambda (p)
                     (and (not (find p output-params))
                          (handlep (get-type-name p) vk-spec)))
                   (params command))))

(defun get-non-struct-params (command vk-spec)
  (sorted-elements
   (let ((vector-count-params (get-vector-count-params command vk-spec))
         (output-params (get-output-params command)))
     (with-slots (structures handles) vk-spec
       (remove-if (lambda (p)
                    (or (gethash (get-type-name p) structures)
                        (gethash (get-type-name p) handles)
                        (find p vector-count-params)
                        (find p output-params)))
                  (params command))))))

(defun get-struct-params (command vk-spec)
  (sorted-elements
   (let ((output-params (get-output-params command)))
     (with-slots (structures) vk-spec
       (remove-if-not (lambda (p)
                        (and (gethash (get-type-name p) structures)
                             (not (find p output-params))))
                      (params command))))))

(defun get-required-params (command vk-spec)
  (sorted-elements (remove-if #'optional-p
                              (concatenate 'list
                                           (get-handle-params command vk-spec)
                                           (get-non-struct-params command vk-spec)
                                           (get-struct-params command vk-spec)))))

(defun get-optional-params (command vk-spec)
  (sorted-elements (remove-if-not #'optional-p
                                  (concatenate 'list
                                               (get-handle-params command vk-spec)
                                               (get-non-struct-params command vk-spec)
                                               (get-struct-params command vk-spec)))))

(defun get-skipped-input-params (command vk-spec)
  (with-slots (params) command
    (let ((vector-count-params (get-vector-count-params command vk-spec))
          (count-to-vector-param-indices (determine-count-to-vector-param-indices params vk-spec))
          (non-const-pointer-param-indices (determine-non-const-pointer-param-indices params))
          (required-params (get-required-params command vk-spec))
          (optional-params (get-optional-params command vk-spec)))
      (remove-if-not (lambda (p)
                       (and (find p vector-count-params)
                            (not (intersection (gethash (position p params) count-to-vector-param-indices)
                                               non-const-pointer-param-indices))))
                     (concatenate 'list
                                  required-params
                                  optional-params)))))

(defun determine-command-type (command vk-spec)
  (with-slots (name
               params
               return-type)
      command
    (with-slots (handles
                 base-types)
        vk-spec
      (let ((output-params (get-output-params command))
            (non-const-pointer-param-indices (determine-non-const-pointer-param-indices params))
            (vector-params (get-vector-params command vk-spec))
            (vector-param-indices (determine-vector-param-indices params vk-spec)))
        (cond
          ((not output-params)
           ;; case 0a: no or implicit return value - e.g. vkDestroyInstance
           ;; case 0b: non-trivial return value - e.g. vkGetInstanceProcAddr
           :simple)
          ((= (length non-const-pointer-param-indices) 1)
           (let ((ret (first output-params)))
             (if (or (handlep (get-type-name ret) vk-spec)
                     ;; added the next two conditions, since function return e.g. a uint64_t* were
                     ;; falsely classified as get-struct-funs
                     (gethash (get-type-name ret) *vk-platform*)
                     (gethash (get-type-name ret) base-types))
                 ;; 1) handle type
                 (if (not (find ret vector-params))
                     ;; case 1a-1: create a handle - e.g. vkCreateInstance
                     ;; case 1a-2: get an existing handle - e.g. vkGetDeviceQueue
                     :create-single-handle
                     ;; 1b-1) vector of handles, where the output vector uses the same len as an input vector - e.g. vkCreateGraphicsPipelines
                     ;; 1b-2) vector of handles using len-by-struct-member - e.g. vkAllocateCommandBuffers
                     :create-multiple-handles)
                 ;; 2) structure chain anchor
                 (if (or (structure-chain-anchor-p (get-type-name (nth (first non-const-pointer-param-indices) params))
                                                   vk-spec)
                         (not (gethash (first non-const-pointer-param-indices) vector-param-indices)))
                     ;; case 1c-1: get a struct extended by its NEXT slot - e.g. vkGetPhysicalDeviceFeatures2
                     ;; case 1c-2: get a struct without NEXT slot - e.g. vkGetPhysicalDeviceProperties
                     #|(if (structure-chain-anchor-p (get-type-name (nth (first non-const-pointer-param-indices) params)) vk-spec)
                         (progn
                           (format t "structure chain anchor: ~a~%" (get-type-name (nth (first non-const-pointer-param-indices) params)))
                           :get-single-struct)
                         (progn
                           (if (gethash (get-type-name (nth (first non-const-pointer-param-indices) params)) (structures vk-spec))
                               (format t "has ~:[no~;a~] next slot: ~a~%"
                                       (find-if (lambda (m)
                                                  (string= "pNext" (name m)))
                                                (members (gethash (get-type-name (nth (first non-const-pointer-param-indices) params)) (structures vk-spec))))
                                       (get-type-name (nth (first non-const-pointer-param-indices) params)))
                               (format t "not a struct type: ~a~%"
                                       (get-type-name (nth (first non-const-pointer-param-indices) params))))
                           :get-single-struct))|#
                     :get-single-struct
                     ;; TODO: with the current implementation such functions are actually simple-funs which don't return anything -> breaks consistency!
                     ;; case 1d: arbitrary data as output param - e.g. vkGetQueryPoolResults
                     :fill-arbitrary-buffer))))
          ((= (length non-const-pointer-param-indices) 2)
           (if (or (structure-chain-anchor-p (get-type-name (nth (second non-const-pointer-param-indices) params))
                                             vk-spec)
                   (and (= (hash-table-count vector-param-indices) 1)
                        (string= "void" return-type)))
               ;; case 2a: get list of structs - e.g. vkGetPhysicalDeviceQueueFamilyProperties2
               :get-multiple-structs
               (cond
                 ;; todo: find out which function falls (or should fall) into that category
                 ;; case 2b: two returns and a non-trivial success code, no array - e.g. ???
                 ((= (hash-table-count vector-param-indices) 0)
                  :get-two-non-array-values)
                 ;; case 2c: enumerate - e.g. vkEnumeratePhysicalDevices
                 ((= (hash-table-count vector-param-indices) 1)
                  :enumerate-single-array)
                 ;; case 2d: return multiple values. one array of the same size as an input array and one additional non-array value - e.g. vkGetCalibratedTimestampsEXT
                 ((= (hash-table-count vector-param-indices) 2)
                  :get-array-and-non-array-value))))
          ((= (length non-const-pointer-param-indices) 3)
           ;; case 3: return two arrays using the same counter which is also an output argument - e.g vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR
           :enumerate-two-arrays)
          (t
           (warn "Never encountered a function like <~a>!" name)
           :unknown))))))

(defmacro %with-cmd-data ((params
                           non-const-pointer-param-indices
                           const-pointer-param-indices
                           vector-param-indices
                           command
                           vk-spec
                           &key (ignore-void t))
                          &body body)
  `(with-slots ((,params params))
       ,command
     (let ((,non-const-pointer-param-indices (determine-non-const-pointer-param-indices ,params ,ignore-void))
           (,const-pointer-param-indices (determine-const-pointer-param-indices ,params))
           (,vector-param-indices (determine-vector-param-indices ,params ,vk-spec ,ignore-void)))
       (progn ,@body))))

(define-condition command-classification-error (error)
  ((reason :initarg :reason
           :initform ""
           :reader reason)
   (command :initarg :command
            :initform ""
            :reader command)
   (vk-spec :initarg :vk-spec
            :initform (error "VK-SPEC must be provided")
            :reader vk-spec))
  (:report
   (lambda (condition stream)
     (with-slots (command
                  vk-spec
                  reason)
         condition
       (%with-cmd-data (params
                        return-param-indices
                        const-pointer-param-indices
                        vector-param-indices
                        command
                        vk-spec
                        :ignore-void nil)
         (format stream "Never encountered a command like <~a>~%~t~a~%~tReturn parameters: ~a~%~tVector parameters: ~a~%~tConst pointer parameters: ~a~&"
                 command
                 reason
                 (loop for idx in (sort return-param-indices #'<)
                       collect (nth idx params))
                 (loop for idx in (sort (alexandria:hash-table-keys vector-param-indices) #'<)
                       for vec-param = (nth idx params)
                       for counter-param = (nth idx params)
                       collect (format nil "~a counted by ~a" vec-param counter-param))
                 (loop for idx in (sort const-pointer-param-indices #'<)
                       collect (nth idx params))))))))

(defmacro with-cmd-data ((params
                          non-const-pointer-param-indices
                          const-pointer-param-indices
                          vector-param-indices
                          check-cmd
                          command
                          vk-spec
                          &key (ignore-void t))
                         &body body)
  (let ((validp (gensym))
        (reason (gensym)))
    `(%with-cmd-data (,params
                      ,non-const-pointer-param-indices
                      ,const-pointer-param-indices
                      ,vector-param-indices
                      ,command
                      ,vk-spec
                      :ignore-void ,ignore-void)
       (flet ((,check-cmd (,validp ,reason)
                (unless ,validp
                  (error 'command-classification-error
                         :reason ,reason
                         :command ,command
                         :vk-spec ,vk-spec))))
         (progn ,@body)))))

;; generateCommandResultSingleSuccessNoErrors
(defun classify-result-single-success-no-errors (command vk-spec)
  (list
   :no-output-param
   (with-cmd-data (params return-param-indices const-pointer-param-indices vector-param-indices check-cmd command vk-spec :ignore-void nil)
     (check-cmd (not return-param-indices)
                "Expected no return params")
     (if (= 0 (hash-table-count vector-param-indices))
         (progn
           (check-cmd (not const-pointer-param-indices)
                      "Expected no const pointer params")
           '(:result :single-success :no-errors :no-output :no-const))
         (progn
           (check-cmd (= 1 (hash-table-count vector-param-indices))
                      "Expected exactly one vector param")
           (let ((vector-param-index (first (alexandria:hash-table-keys vector-param-indices))))
             (check-cmd (value-p (type-info (nth (gethash vector-param-index vector-param-indices) params)))
                        "Expected counter param to be of a value type")
             (check-cmd (handlep (get-type-name (nth vector-param-index params)) vk-spec)
                        "Expected vector param to be of handle type"))
           '(:result :single-success :no-errors :no-output (:vector (:handle :value))))))))

;; generateCommandResultSingleSuccessWithErrors0Return
(defun classify-result-single-success-with-errors-0-return (command vk-spec)
  (list
   :no-output-param
   (with-cmd-data (params return-param-indices const-pointer-param-indices vector-param-indices check-cmd command vk-spec :ignore-void nil)
     (cond
       ((= 0 (hash-table-count vector-param-indices))
        (cond
          ((= 0 (length const-pointer-param-indices))
           '(:result :single-success :with-errors :no-output :no-const))
          ((= 1 (length const-pointer-param-indices))
           (check-cmd (not (string= "void" (get-type-name (nth (first const-pointer-param-indices) params))))
                      "Expected const pointer param not to be of type void")
           '(:result :single-success :with-errors :no-output :1-non-void-const))
          (t
           (check-cmd nil "Expected less than 2 const pointer params"))))
       ((= 1 (hash-table-count vector-param-indices))
        (let ((vector-param-index (first (alexandria:hash-table-keys vector-param-indices))))
          (check-cmd (value-p (type-info (nth (gethash vector-param-index vector-param-indices) params)))
                     "Expected counter param to be of a value type")
          (check-cmd (not (string= "void" (get-type-name (nth vector-param-index params))))
                     "Expected vector param to not be void"))
        '(:result :single-success :with-errors :np-output (:vector (:non-void :value))))
       (t
        (check-cmd nil "Expected less than 2 vector param indices"))))))

;; generateCommandResultSingleSuccessWithErrors1ReturnHandle
(defun classify-result-single-success-with-errors-1-return-handle (command return-param-index return-param vk-spec)
  (with-cmd-data (params return-param-indices const-pointer-param-indices vector-param-indices check-cmd command vk-spec :ignore-void nil)
    (cond
      ;; generateCommandResultSingleSuccessWithErrors1ReturnHandle0Vector
      ((= 0 (hash-table-count vector-param-indices))
       '(:get-or-create-handle (:result :single-success :with-errors (:output :handle))))
      ;; generateCommandResultSingleSuccessWithErrors1ReturnHandle1Vector
      ((= 1 (hash-table-count vector-param-indices))
       (let ((vector-param-index (first (alexandria:hash-table-keys vector-param-indices))))
         (check-cmd (eq return-param-index vector-param-index)
                    "Expected return param to be the first vector param")
         (check-cmd (len-by-struct-member-p (len (nth vector-param-index params)) (nth (gethash vector-param-index vector-param-indices) params) vk-spec)
                    "Expected vector to be counted by a struct member"))
       '(:allocate-handles (:result :single-success :with-errors (:output :handle-vector) (:vector (:handle :struct-member)))))
      ;; generateCommandResultSingleSuccessWithErrors1ReturnHandle2Vector
      ((= 2 (hash-table-count vector-param-indices))
       (let* ((vector-param-indices-keys (sort (alexandria:hash-table-keys vector-param-indices) #'<))
              (first-vector-param-index (first vector-param-indices-keys))
              (second-vector-param-index (second vector-param-indices-keys)))
         (check-cmd (= return-param-index second-vector-param-index)
                    "Expected return param to be the second vector param")
         (check-cmd (= (gethash first-vector-param-index vector-param-indices)
                       (gethash second-vector-param-index vector-param-indices))
                    "Expected both vector params to be counted by same param")
         (check-cmd (value-p (type-info (nth (gethash first-vector-param-index vector-param-indices) params)))
                    "Expected counter to be a value")
         (check-cmd (structure-chain-anchor-p (get-type-name (nth first-vector-param-index params)) vk-spec)
                    "Expected first vector param to be a structure chain anchor"))
       '(:create-handles (:result :single-success :with-errors (:output :handle-vector) (:vector (:stucture-chain-anchor (:value (:same-as 1))) (:handle (:value :same-as 0) :output)))))
      (t
       (check-cmd nil "Expected less than 3 vector param indices")))))

;; generateCommandResultSingleSuccessWithErrors1ReturnChain
(defun classify-result-single-success-with-errors-1-return-chain (command return-param-index return-param vk-spec)
  (with-cmd-data (params return-param-indices const-pointer-param-indices vector-param-indices check-cmd command vk-spec :ignore-void nil)
    (check-cmd (= 0 (hash-table-count vector-param-indices))
               "Expected no vector params")
    (list
     (if (has-type-and-next-p (get-type-name return-param) vk-spec)
         :get-struct-chain
         :get-struct)
     '(:result :single-success :with-errors
       (:output :structure-chain-anchor)))))

;; generateCommandResultSingleSuccessWithErrors1ReturnVoid
(defun classify-result-single-success-with-errors-1-return-void (command return-param-index return-param vk-spec)
  (with-cmd-data (params return-param-indices const-pointer-param-indices vector-param-indices check-cmd command vk-spec :ignore-void nil)
    (cond
      ((= 0 (hash-table-count vector-param-indices))
       (check-cmd (string= "vkMapMemory" (name command))
                  "Expected command to be vkMapMemory")
       '(:no-output-param (:result :single-success :with-errors :no-output)))
      ((= 1 (hash-table-count vector-param-indices))
       (let ((vector-param-index (first (alexandria:hash-table-keys vector-param-indices))))
         (check-cmd (= return-param-index vector-param-index)
                    "Expected return param be the vector param")
         (check-cmd (value-p (type-info (nth (gethash vector-param-index vector-param-indices) params)))
                    "Expected counter for vector param to be a value"))
       '(:fill-void-pointer (:result :single-success :with-errors (:output :void-vector) (:vector (:void :value :output)))))
      ((= 2 (hash-table-count vector-param-indices))
       (let* ((vector-param-indices-keys (sort (alexandria:hash-table-keys vector-param-indices) #'<))
              (first-vector-param-index (first vector-param-indices-keys))
              (second-vector-param-index (second vector-param-indices-keys)))
         (check-cmd (= return-param-index second-vector-param-index)
                    "Expected return param to be the second vector param")
         (check-cmd (not (= (gethash first-vector-param-index vector-param-indices)
                            (gethash second-vector-param-index vector-param-indices)))
                    "Expected vector params not to be counted by same param")
         (check-cmd (value-p (type-info (nth (gethash first-vector-param-index vector-param-indices) params)))
                    "Expected counter for first vector param to be of a value type")
         (check-cmd(handlep (get-type-name (nth first-vector-param-index params)) vk-spec)
                   "Expected first vector param to be a handle")
         (check-cmd (value-p (type-info (nth (gethash second-vector-param-index vector-param-indices) params)))
                    "Expected counter for second vector param to be of a value type"))
       '(:fill-void-pointer (:result :single-success :with-errors (:output :void-vector) (:vector (:handle :value) (:void :value :output)))))
      (t
       (check-cmd nil "Expected less than 3 vector params")))))

;; generateCommandResultSingleSuccessWithErrors1ReturnValue
(defun classify-result-single-success-with-errors-1-return-value (command return-param-index return-param vk-spec)
  (with-cmd-data (params return-param-indices const-pointer-param-indices vector-param-indices check-cmd command vk-spec :ignore-void nil)
    (check-cmd (= 0 (hash-table-count vector-param-indices))
               "Expected no vector params")
    (let ((return-param-type-name (get-type-name return-param)))
      (cond
        ((handlep return-param-type-name vk-spec)
         '(:get-or-create-handle (:result :single-success :with-errors (:output :handle))))
        ((structure-type-p return-param-type-name vk-spec)
         (list
          (if (has-type-and-next-p return-param-type-name vk-spec)
              :get-struct-chain
              :get-struct)
          '(:result :single-success :with-errors
            (:output :struct))))
        (t
         '(:get-value (:result :single-success :with-errors (:output :value))))))))

;; generateCommandResultSingleSuccessWithErrors1Return
(defun classify-result-single-success-with-errors-1-return (command vk-spec)
  (let ((return-param-indices (determine-non-const-pointer-param-indices (params command) nil)))
    (let* ((return-param-index (first return-param-indices))
           (return-param (nth return-param-index (params command))))
      (cond
        ((handlep (get-type-name return-param) vk-spec)
         (classify-result-single-success-with-errors-1-return-handle command return-param-index return-param vk-spec))
        ((structure-chain-anchor-p (get-type-name return-param) vk-spec)
         (classify-result-single-success-with-errors-1-return-chain command return-param-index return-param vk-spec))
        ((string= "void" (get-type-name return-param))
         (classify-result-single-success-with-errors-1-return-void command return-param-index return-param vk-spec))
        (t
         (classify-result-single-success-with-errors-1-return-value command return-param-index return-param vk-spec))))))

;; generateCommandResultSingleSuccessWithErrors2Return
(defun classify-result-single-success-with-errors-2-return (command vk-spec)
  (with-cmd-data (params return-param-indices const-pointer-param-indices vector-param-indices check-cmd command vk-spec :ignore-void nil)
    (let ((first-return-param (get-type-name (nth (first return-param-indices) params))))
      (check-cmd (and (not (string= "void" first-return-param))
                      (not (handlep first-return-param vk-spec))
                      (not (structure-chain-anchor-p first-return-param vk-spec)))
                 "Expected first return param neither void, nor a handle nor a structure chain anchor"))
    (let ((second-return-param (get-type-name (nth (second return-param-indices) params))))
      (check-cmd (and (not (string= "void" second-return-param))
                      (not (handlep second-return-param vk-spec))
                      (not (structure-chain-anchor-p second-return-param vk-spec)))
                 "Expected second return param neither void, nor a handle nor a structure chain anchor"))
    (check-cmd (= 2 (hash-table-count vector-param-indices))
               "Expected exactly two vector param indices")
    (let* ((vector-param-indices-keys (sort (alexandria:hash-table-keys vector-param-indices) #'<))
           (first-vector-param-index (first vector-param-indices-keys))
           (second-vector-param-index (second vector-param-indices-keys)))
      (check-cmd (= (first return-param-indices) second-vector-param-index)
                 "Expected first return param to be the second vector param")
      (check-cmd (not (member (second return-param-indices) vector-param-indices-keys))
                 "Expected second return param not to be a vector param")
      (check-cmd (and (not (= (first return-param-indices) (gethash first-vector-param-index vector-param-indices)))
                      (not (= (first return-param-indices) (gethash second-vector-param-index vector-param-indices))))
                 "Expected second return param not to be the counter for any of the vector params")
      (check-cmd (= (gethash first-vector-param-index vector-param-indices)
                    (gethash second-vector-param-index vector-param-indices))
                 "Expected vector params to be counted by same param")
      (check-cmd (value-p (type-info (nth (gethash first-vector-param-index vector-param-indices) params)))
                 "Expected counter for first vector param to be of a value type")
      (let ((first-vector-param (get-type-name (nth first-vector-param-index params))))
        (check-cmd (and (not (string= "void" first-vector-param))
                        (not (handlep first-vector-param vk-spec))
                        (not (structure-chain-anchor-p first-vector-param vk-spec)))
                   "Expected first vector param neither void, nor a handle nor a structure chain anchor")))
    '(:get-value-array-and-value (:result :single-success :with-errors (:output :value-vector :value) (:vector (:struct (:value (:same-as 1))) (:value (:value (:same-as 0)) :output))))))

;; generateCommandResultSingleSuccessWithErrors
(defun classify-result-single-success-with-errors (command vk-spec)
  (let ((return-param-indices (determine-non-const-pointer-param-indices (params command) nil)))
    (cond
      ((= 0 (length return-param-indices))
       (classify-result-single-success-with-errors-0-return command vk-spec))
      ((= 1 (length return-param-indices))
       (classify-result-single-success-with-errors-1-return command vk-spec))
      ((= 2 (length return-param-indices))
       (classify-result-single-success-with-errors-2-return command vk-spec))
      (t
       (error 'command-classification-error
              :reason "Expected less than 3 return params for void command"
              :command command
              :vk-spec vk-spec)))))

;; generateCommandResultMultiSuccessNoErrors
(defun classify-result-multi-success-no-errors (command vk-spec)
  (with-cmd-data (params return-param-indices const-pointer-param-indices vector-param-indices check-cmd command vk-spec :ignore-void nil)
    (check-cmd (not return-param-indices)
               "Expected no return params")
    (check-cmd (= 0 (hash-table-count vector-param-indices))
               "Expected no vector param indices")
    (check-cmd (not const-pointer-param-indices)
               "Expected no const pointer param indices")
    '(:no-output-param (:result :multi-success :no-errors :no-output))))

;; generateCommandResultMultiSuccessWithErrors0Return
(defun classify-result-multi-success-with-errors-0-return (command vk-spec)
  (with-cmd-data (params return-param-indices const-pointer-param-indices vector-param-indices check-cmd command vk-spec :ignore-void nil)
    (cond
      ((= 0 (hash-table-count vector-param-indices))
       (check-cmd (< (length const-pointer-param-indices) 2)
                  "Expected less than 2 const pointer params")
       (cond
         ((= 0 (length const-pointer-param-indices))
          '(:no-output-param (:result :multi-success :with-errors :no-output :no-const)))
         ((= 1 (length const-pointer-param-indices))
          (check-cmd (not (string= "void" (get-type-name (nth (first const-pointer-param-indices) params))))
                     "Expected first cost pointer param not to be of type void")
          '(:no-output-param (:result :multi-success :with-errors :no-output)))))
      ((= 1 (hash-table-count vector-param-indices))
       (let ((vector-param-index (first (alexandria:hash-table-keys vector-param-indices))))
         (check-cmd (value-p (type-info (nth (gethash vector-param-index vector-param-indices) params)))
                    "Expected counter for vector param to be of a value type")
         (check-cmd (handlep (get-type-name (nth vector-param-index params)) vk-spec)
                    "Expected vector param to be of a handle type"))
       '(:no-output-param (:result :multi-success :with-errors :no-output (:vector (:handle :value)))))
      ((= 2 (hash-table-count vector-param-indices))
       (let* ((vector-param-indices-keys (sort (alexandria:hash-table-keys vector-param-indices) #'<))
              (first-vector-param-index (first vector-param-indices-keys))
              (second-vector-param-index (second vector-param-indices-keys)))
         (check-cmd (= (gethash first-vector-param-index vector-param-indices)
                       (gethash second-vector-param-index vector-param-indices))
                    "Expected vector params to be counted by same param")
         (check-cmd (string= "uint32_t" (get-type-name (nth (gethash first-vector-param-index vector-param-indices) params)))
                    "Expected vector params to be counted by a uint32_t")
         (let ((first-vector-param (get-type-name (nth first-vector-param-index params))))
           (check-cmd (and (not (string= "void" first-vector-param))
                           (not (handlep first-vector-param vk-spec))
                           (not (structure-chain-anchor-p first-vector-param vk-spec)))
                      "Expected first vector param neither void, nor a handle nor a structure chain anchor"))
         (let ((second-vector-param (get-type-name (nth second-vector-param-index params))))
           (check-cmd (and (not (string= "void" second-vector-param))
                           (not (handlep second-vector-param vk-spec))
                           (not (structure-chain-anchor-p second-vector-param vk-spec)))
                      "Expected second vector param neither void, nor a handle nor a structure chain anchor")))
       '(:no-output-param (:result :multi-success :with-errors :no-output-param (:vector (:struct ((:value :uint32) (:same-as 1))) (:struct ((:value :uint32) (:same-as 0)))))))
      (t
       (check-cmd nil "Expected less than 3 vector param indices")))))

;; generateCommandResultMultiSuccessWithErrors1Return
(defun classify-result-multi-success-with-errors-1-return (command vk-spec)
  (with-cmd-data (params return-param-indices const-pointer-param-indices vector-param-indices check-cmd command vk-spec :ignore-void nil)
    (let* ((return-param-index (first return-param-indices))
           (return-param (nth return-param-index params))
           (return-param-type-name (get-type-name return-param)))
      (cond
        ((string= "void" (get-type-name return-param))
         (check-cmd (= 1 (hash-table-count vector-param-indices))
                    "Expected exactly one vector param")
         (let ((vector-param-index (first (alexandria:hash-table-keys vector-param-indices))))
           (check-cmd (= return-param-index vector-param-index)
                      "Expected return param to be the vector param")
           (check-cmd (value-p (type-info (nth (gethash vector-param-index vector-param-indices) params)))
                      "Expected counter for vector param to be of a value type"))
         '(:fill-void-pointer (:result :multi-success :with-errors (:output :void-vector) (:vector (:void :value :output)))))
        ((handlep (get-type-name return-param) vk-spec)
         (check-cmd (= 2 (hash-table-count vector-param-indices))
                    "Expected exactly 2 vector params")
         (let* ((vector-param-indices-keys (sort (alexandria:hash-table-keys vector-param-indices) #'<))
                (first-vector-param-index (first vector-param-indices-keys))
                (second-vector-param-index (second vector-param-indices-keys)))
           (check-cmd (= return-param-index second-vector-param-index)
                      "Expected return param to be the second vector param")
           (check-cmd (= (gethash first-vector-param-index vector-param-indices)
                         (gethash second-vector-param-index vector-param-indices))
                      "Expected vector params to be counted by same param")
           (check-cmd (string= "uint32_t" (get-type-name (nth (gethash first-vector-param-index vector-param-indices) params)))
                      "Expected vector params to be counted by a uint32_t")
           (check-cmd (structure-chain-anchor-p (get-type-name (nth first-vector-param-index params)) vk-spec)
                      "Expected first vector param to be a structure chain anchor"))
         '(:create-handles (:result :multi-success :with-errors (:output :handle-vector) (:vector (:structure-chain-anchor ((:value :uint32) (:same-as 1))) (:handle ((:value :uint32) (:same-as 0)) :output)))))
        ((not (structure-chain-anchor-p (get-type-name return-param) vk-spec))
         (check-cmd (= 0 (hash-table-count vector-param-indices))
                    "Expected no vector params")
         (cond
           ((string= "uint32_t" return-param-type-name)
            '(:get-value
              (:result :multi-success :with-errors
               (:output :value))))
           ((structure-type-p return-param-type-name vk-spec)
            (list
             (if (has-type-and-next-p return-param-type-name vk-spec)
                 :get-struct-chain
                 :get-struct)
             '(:result :multi-success :with-errors
               (:output :struct))))
           (t
            (check-cmd nil "Expected return param to either be a uint32_t or a struct"))))
        (t
         (check-cmd nil "Expected return param to be either void, a handle or not to be a structure chain anchor"))))))

;; generateCommandResultMultiSuccessWithErrors2Return
(defun classify-result-multi-success-with-errors-2-return (command vk-spec)
  (with-cmd-data (params return-param-indices const-pointer-param-indices vector-param-indices check-cmd command vk-spec :ignore-void nil)
    (let* ((first-return-param-index (first return-param-indices))
           (second-return-param-index (second return-param-indices))
           (first-return-param (get-type-name (nth first-return-param-index params)))
           (second-return-param (get-type-name (nth second-return-param-index params))))
      (check-cmd (or (string= "size_t" first-return-param)
                     (string= "uint32_t" first-return-param))
                 "Expected first return param to be either size_t or uint32_t")
      (check-cmd (not (structure-chain-anchor-p second-return-param vk-spec))
                 "Expected second return param not to be a structure chain anchor")
      (check-cmd (= 1 (hash-table-count vector-param-indices))
                 "Expected exactly one vector param")
      (let ((vector-param-index (first (alexandria:hash-table-keys vector-param-indices))))
        (check-cmd (= first-return-param-index (gethash vector-param-index vector-param-indices))
                   "Expected first return param to be the counter of the vector param")
        (check-cmd (= second-return-param-index vector-param-index)
                   "Expected second return param to be the vector param"))
      (cond
        ((string= "void" second-return-param)
         '(:fill-void-pointer
           (:result :multi-success :with-errors
            (:output :void-vector :value)
            (:vector
             (:void :value :output)))))
        ((handlep second-return-param vk-spec)
         '(:enumerate-handles
           (:result :multi-success :with-errors
            (:output :handle-vector :value)
            (:vector
             (:handle :value :output)))))
        ((structure-type-p second-return-param vk-spec)
         (list
          (if (has-type-and-next-p second-return-param vk-spec)
              :enumerate-struct-chains
              :enumerate-structs)
          '(:result :multi-success :with-errors
            (:output :struct-vector :value)
            (:vector
             (:struct :value :output)))))
        (t
         '(:enumerate-values
           (:result :multi-success :with-errors
            (:output :value-vector :value)
            (:vector
             (:value :value :output)))))))))

;; generateCommandResultMultiSuccessWithErrors3Return
(defun classify-result-multi-success-with-errors-3-return (command vk-spec)
  (with-cmd-data (params return-param-indices const-pointer-param-indices vector-param-indices check-cmd command vk-spec :ignore-void nil)
    (let* ((first-return-param-index (first return-param-indices))
           (second-return-param-index (second return-param-indices))
           (third-return-param-index (third return-param-indices))
           (first-return-param (get-type-name (nth first-return-param-index params)))
           (second-return-param (get-type-name (nth second-return-param-index params)))
           (third-return-param (get-type-name (nth third-return-param-index params))))
      (check-cmd (string= "uint32_t" first-return-param)
                 "Expected first return param to be a uint32_t")
      (check-cmd (and (not (string= "void" second-return-param))
                      (not (handlep second-return-param vk-spec))
                      (not (structure-chain-anchor-p second-return-param vk-spec)))
                 "Expected second return param neither void, nor a handle nor a structure chain anchor")
      (check-cmd (and (not (string= "void" third-return-param))
                      (not (handlep third-return-param vk-spec))
                      (not (structure-chain-anchor-p third-return-param vk-spec)))
                 "Expected third return param neither void, nor a handle nor a structure chain anchor")
      (check-cmd (= 2 (hash-table-count vector-param-indices))
                 "Expected exactly two vector params")
      (let* ((vector-param-indices-keys (sort (alexandria:hash-table-keys vector-param-indices) #'<))
             (first-vector-param-index (first vector-param-indices-keys))
             (second-vector-param-index (second vector-param-indices-keys)))
        (check-cmd (= (gethash first-vector-param-index vector-param-indices)
                      (gethash second-vector-param-index vector-param-indices))
                   "Expected vector params to be counted by same param")
        (check-cmd (= first-return-param-index (gethash first-vector-param-index vector-param-indices))
                   "Expected first return param to be the counter of the vector params")
        (check-cmd (= second-return-param-index first-vector-param-index)
                   "Expected second return param to be the first vector param")
        (check-cmd (= third-return-param-index second-vector-param-index)
                   "Expected third return param to be the second vector param"))
      (list
       (cond
         ((and (has-type-and-next-p second-return-param vk-spec)
               (has-type-and-next-p third-return-param vk-spec))
          :enumerate-two-struct-chains)
         ((has-type-and-next-p second-return-param vk-spec)
          :enumerate-struct-chain-and-struct)
         (t
          :enumerate-struct-and-struct-chain))
       '(:result :multi-success :with-errors
         (:output :value :struct-vector :struct-vector)
         (:vector
          (:struct ((:value :uint32) (:same-as 1)) :output)
          (:struct ((:value :uint32) (:same-as 0)) :output)))))))

;; generateCommandResultMultiSuccessWithErrors
(defun classify-result-multi-success-with-errors (command vk-spec)
  (let ((return-param-indices (determine-non-const-pointer-param-indices (params command) nil)))
    (cond
      ((= 0 (length return-param-indices))
       (classify-result-multi-success-with-errors-0-return command vk-spec))
      ((= 1 (length return-param-indices))
       (classify-result-multi-success-with-errors-1-return command vk-spec))
      ((= 2 (length return-param-indices))
       (classify-result-multi-success-with-errors-2-return command vk-spec))
      ((= 3 (length return-param-indices))
       (classify-result-multi-success-with-errors-3-return command vk-spec))
      (t
       (error 'command-classification-error
              :reason "Expected less than 4 return params"
              :command command
              :vk-spec vk-spec)))))

;; generateCommandVoid0Return
(defun classify-void-0-return (command vk-spec)
  (list
   :no-output-param
   (with-cmd-data (params return-param-indices const-pointer-param-indices vector-param-indices check-cmd command vk-spec :ignore-void nil)
     (cond
       ((= 0 (hash-table-count vector-param-indices))
        (if (and const-pointer-param-indices
                 (not (find-if (lambda (idx)
                                 (not (string= "void" (get-type-name (nth idx params)))))
                               const-pointer-param-indices)))
            '(:void :no-output)
            '(:void :no-output :no-const-void)))
       ((= 1 (hash-table-count vector-param-indices))
        (let ((counter-type (type-info (nth (first (alexandria:hash-table-values vector-param-indices)) params))))
          (check-cmd (value-p counter-type) "Expected counter param to be of a value type")
          (check-cmd (or (string= "uint32_t" (type-name counter-type))
                         (string= "VkDeviceSize" (type-name counter-type)))
                     "Expected counter type to be either uint32_t or VkDeviceSize"))
        '(:void :no-output (:vector (:any :value))))
       ((not (find-if (lambda (p)
                        (let ((counter-param (nth (gethash p vector-param-indices) params))
                              (vector-param (nth p params)))
                          (or (not (value-p (type-info counter-param)))
                              (not (string= "uint32_t" (get-type-name counter-param)))
                              (string= "void" (get-type-name vector-param)))))
                      (alexandria:hash-table-keys vector-param-indices)))
        '(:void :no-output :may-have-vector-params))
       (t
        (check-cmd nil
                   "Expected less than 2 vector params or one counter param being either not of a value type or not a uint32_t or one vector param of void type"))))))

;; generateCommandVoid1Return
(defun classify-void-1-return (command vk-spec)
  (with-cmd-data (params return-param-indices const-pointer-param-indices vector-param-indices check-cmd command vk-spec :ignore-void nil)
    (let* ((return-param-index (first return-param-indices))
           (return-param-type-name (get-type-name (nth return-param-index params))))
      (cond
        ((handlep return-param-type-name vk-spec)
         (check-cmd (= 0 (hash-table-count vector-param-indices))
                    "Expected no vector params")
         '(:get-or-create-handle (:void (:output :handle))))
        ((structure-chain-anchor-p return-param-type-name vk-spec)
         (check-cmd (= 0 (hash-table-count vector-param-indices))
                    "Expected no vector params")
         (list
          (if (has-type-and-next-p return-param-type-name vk-spec)
              :get-struct-chain
              :get-struct)
          '(:void (:output :structure-chain-anchor))))
        ((not (string= "void" return-param-type-name))
         (cond
           ((= 0 (hash-table-count vector-param-indices))
            (cond
              ((structure-type-p return-param-type-name vk-spec)
               (list
                (if (has-type-and-next-p return-param-type-name vk-spec)
                    :get-struct-chain
                    :get-struct)
                '(:void (:output :struct))))
              ((handlep return-param-type-name vk-spec)
               '(:get-or-create-handle (:void (:output :handle))))
              (t
               '(:get-value (:void (:output :value))))))
           ((= 1 (hash-table-count vector-param-indices))
            (let* ((vector-param-index (first (alexandria:hash-table-keys vector-param-indices)))
                   (vector-param (nth vector-param-index params))
                   (vector-param-type-name (get-type-name vector-param))
                   (counter-param (nth (gethash vector-param-index vector-param-indices) params)))
              (check-cmd (not (= return-param-index vector-param-index))
                         "Expected return param not to be the vector param")
              (check-cmd (and (not (handlep vector-param-type-name vk-spec))
                              (not (structure-chain-anchor-p vector-param-type-name vk-spec))
                              (not (string= "void" vector-param-type-name)))
                         "Expected vector param not to be a handle or void type or not to be a structure chain anchor")
              (check-cmd (len-by-struct-member-p (len vector-param) counter-param vk-spec)
                         "Expected vector param to be counted by a struct member"))
            (list
             (if (has-type-and-next-p return-param-type-name vk-spec)
                 :get-struct-chain
                 :get-struct)
             '(:void
               (:output :struct)
               (:vector
                (:struct :value)))))
           (t
            (check-cmd nil "Expected less than 2 vector params"))))
        (t
         (check-cmd nil "Expected return param to be either a handle type or a structure chain anchor or not to be of type void"))))))

;; generateCommandVoid2Return
(defun classify-void-2-return (command vk-spec)
  (with-cmd-data (params return-param-indices const-pointer-param-indices vector-param-indices check-cmd command vk-spec :ignore-void nil)
    (let* ((first-return-param-index (first return-param-indices))
           (second-return-param-index (second return-param-indices))
           (first-return-param-type-name (get-type-name (nth first-return-param-index params)))
           (second-return-param-type-name (get-type-name (nth second-return-param-index params))))
      (check-cmd (string= "uint32_t" first-return-param-type-name)
                 "Expected first return param to be a uint32_t")
      (cond
        ((structure-chain-anchor-p first-return-param-type-name vk-spec)
         (check-cmd (= 1 (hash-table-count vector-param-indices))
                    "Expected exactly one vector param")
         (let ((vector-param-index (first (alexandria:hash-table-keys vector-param-indices))))
           (check-cmd (= first-return-param-index (gethash vector-param-index vector-param-indices))
                      "Expected first return param to be the counter of the vector param")
           (check-cmd (= second-return-param-index vector-param-index)
                      "Expected second return param to be the vector param"))
         '(:unknown-need-example (:void :2-return-chain-vector :1-vector)))
        ((and (not (string= "void" second-return-param-type-name))
              (not (handlep second-return-param-type-name vk-spec)))
         (check-cmd (= 1 (hash-table-count vector-param-indices))
                    "Expected exactly one vector param")
         (let ((vector-param-index (first (alexandria:hash-table-keys vector-param-indices))))
           (check-cmd (= first-return-param-index (gethash vector-param-index vector-param-indices))
                      "Expected first return param to be the counter of the vector param")
           (check-cmd (= second-return-param-index vector-param-index)
                      "Expected second return param to be the vector param"))
         (check-cmd (structure-type-p second-return-param-type-name vk-spec)
                    "Expected return param to be of a struct type")
         (list
          (if (has-type-and-next-p second-return-param-type-name vk-spec)
              :get-struct-chains
              :get-structs)
          '(:void
            (:output :struct-vector :value)
            (:vector
             (:struct :value :output)))))
        (t
         (check-cmd nil "Expected first return param to be a structure chain anchor or second return param not to be void or a handle type"))))))

(defun classify-void (command vk-spec)
  (let ((return-param-indices (determine-non-const-pointer-param-indices (params command) nil)))
    (cond
      ((= 0 (length return-param-indices))
       (classify-void-0-return command vk-spec))
      ((= 1 (length return-param-indices))
       (classify-void-1-return command vk-spec))
      ((= 2 (length return-param-indices))
       (classify-void-2-return command vk-spec))
      (t
       (error 'command-classification-error
              :reason "Expected less than 3 return params for void command"
              :command command
              :vk-spec vk-spec)))))

;; generateCommandValue
(defun classify-value (command vk-spec)
  (with-cmd-data (params return-param-indices const-pointer-param-indices vector-param-indices check-cmd command vk-spec :ignore-void nil)
    (check-cmd (= 0 (length return-param-indices)) "Expected no return params")
    (check-cmd (= 0 (hash-table-count vector-param-indices)) "Expected no vector params")
    (cond
      ((= 0 (length const-pointer-param-indices))
       '(:no-output-param (:value :no-const)))
      ((= 1 (length const-pointer-param-indices))
       (check-cmd (not (string= "void" (get-type-name (nth (first const-pointer-param-indices) params))))
                  "Expected const pointer param not to be of void type")
       '(:no-output-param (:value (:const :non-void) :no-const-void)))
      (t
       (check-cmd nil "Expected less than 2 const pointer params")))))

(defparameter *command-types*
  '((:no-output-param "A function without any output parameters.")
    (:get-or-create-handle "A function returning a single handle or some other pointer.")
    (:allocate-handles "A function allocating multiple handles (possibly equivalent for wrapper: :CREATE-HANDLES)")
    (:create-handles "A function creating multiple handles (possibly equivalent for wrapper: :ALLOCATE-HANDLES)")
    (:get-struct "A function returning a single struct without an sType or a pNext member.")
    (:get-struct-chain "A function returning a single struct which might be extended by a pNext member.")
    (:get-structs "A function returning multiple structs and no VkResult")
    (:get.struct-chains "A function returning multiple structs which might be extended by a pNext member and no VkResult.")
    (:get-value "A function returning a single primitive type or a flag")
    (:enumerate-structs "A function returning multiple structs and a VkResult which might be VK_INCOMPLETE")
    (:enumerate-struct-chains "A function returning multiple structs which might be extended by a pNext member and a VkResult which might be VK_INCOMPLETE")
    (:enumerate-handles "A function returning multiple handles and a VkResult which might be VK_INCOMPLETE")
    (:enumerate-values "A function returning multiple values and a VkResult which might be VK_INCOMPLETE")
    (:enumerate-two-struct-chains "A function returning two lists of structs which each might be extended by a pNext member an a VkResult which might be VK_INCOMPLETE")
    (:get-value-array-and-non-array-value "A function returning a list of values and a value which is no meta parameter of the list parameter.")
    (:fill-void-pointer "A function taking a void pointer - must be handled by user (equivalent for wrapper: :NO-OUTPUT-PARAM"))
  "A list of command types.")

;; todo: map new cases to old ones
(defun determine-command-type-2 (command vk-spec)
  (with-slots (return-type
               success-codes
               error-codes)
      command
    (if (string= "VkResult" return-type)
        (if (= 1 (length success-codes))
            (if (not error-codes)
                (classify-result-single-success-no-errors command vk-spec)
                (classify-result-single-success-with-errors command vk-spec))
            (if (not error-codes)
                (classify-result-multi-success-no-errors command vk-spec)
                (classify-result-multi-success-with-errors command vk-spec)))
        (if (string= "void" return-type)
            (classify-void command vk-spec)
            (classify-value command vk-spec)))))
