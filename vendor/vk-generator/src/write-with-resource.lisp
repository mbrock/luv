;;;; write-with-resource.lisp

(in-package #:vk-generator)

(defun make-def-with-name (name vk-spec)
  (format nil "with-~(~a~)"
          (fix-function-name (if (alexandria:starts-with-subseq "vkCreate" name)
                                 (subseq name 8)
                                 (subseq name 10))
                             (tags vk-spec))))

(defun write-with-resource (out handle create-command-name delete-command-name vk-spec)
  (let* ((create-command (gethash create-command-name (commands vk-spec)))
         (delete-command (gethash delete-command-name (commands vk-spec)))
         (required-create-params (get-required-params create-command vk-spec))
         (optional-create-params (get-optional-params create-command vk-spec))
         (required-delete-params (get-required-params delete-command vk-spec))
         (optional-delete-params (get-optional-params delete-command vk-spec))
         (extensionp (needs-explicit-loading-p create-command))
         (command-type (determine-command-type create-command vk-spec))
         (multiplep (and (eq command-type :create-multiple-handles)
                         (not (or (string= delete-command-name "vkFreeCommandBuffers")
                                  (string= delete-command-name "vkFreeDescriptorSets")))))
         (resource-arg-string (cond
                                ((eq command-type :create-multiple-handles)
                                 "resources")
                                ((eq command-type :create-single-handle)
                                 "resource")
                                (t (error "Command does not create a handle: ~a command-type: ~a"
                                          create-command-name
                                          command-type))))
         (destructor-string (format nil "(~(vk:~a~{ ~a~}~:[~; (or ,extension-loader vk:*default-extension-loader*)~]~))"
                                    (fix-function-name delete-command-name (tags vk-spec))
                                    (loop for p in (concatenate 'list required-delete-params optional-delete-params)
                                          collect (cond
                                                    ((string= (name handle)
                                                              (get-type-name p))
                                                     (if (or (string= delete-command-name "vkFreeCommandBuffers")
                                                             (string= delete-command-name "vkFreeDescriptorSets"))
                                                         (format nil ",~(~a~)" resource-arg-string)
                                                         ",resource"))
                                                    ((and (string= delete-command-name "vkFreeCommandBuffers")
                                                          (string= (name p) "commandPool"))
                                                     "(vk:command-pool ,allocate-info)")
                                                    ((and (string= delete-command-name "vkFreeDescriptorSets")
                                                          (string= (name p) "descriptorPool"))
                                                     "(vk:descriptor-pool ,allocate-info)")
                                                    ((string= (name p) "pAllocator")
                                                     "(or ,allocator vk:*default-allocator*)")
                                                    (t (format nil ",~(~a~)"
                                                               (fix-slot-name (name p) (get-type-name p) vk-spec)))))
                                    extensionp))
         (let-string (format nil "(,~a (~a))"
                             resource-arg-string
                             (format nil "~(vk:~a~{ ~a~}~:[~; (or ,extension-loader vk:*default-extension-loader*)~]~)"
                                     (fix-function-name create-command-name (tags vk-spec))
                                     (concatenate 'list
                                                  (loop for p in required-create-params
                                                        collect (format nil ",~(~a~)"
                                                                        (fix-slot-name (name p) (get-type-name p) vk-spec)))
                                                  (loop for p in optional-create-params
                                                        collect (format nil "(or ,~(~a~) ~a)"
                                                                        (fix-slot-name (name p) (get-type-name p) vk-spec)
                                                                        (determine-param-default-value-string p vk-spec))))
                                     extensionp)))
         (body-string (format nil (if multiplep
                                      "
  (let ((resource (gensym \"RESOURCE\")))
    `(let (~a)
       (unwind-protect
           (progn ,@body)
         (loop for ,resource in ,resources do
               ~a))))"
                                      "
  `(let (~a)
     (unwind-protect
         (progn ,@body)
       ~a))")
                              let-string
                              destructor-string))
         (doc-string (format nil "~%  \"Binds ~:@(~a~) to the result of a VK:~:@(~a~) call.~%See VK:~:@(~a~)~%See VK:~:@(~a~)\""
                             resource-arg-string
                             (fix-function-name create-command-name (tags vk-spec))
                             (fix-function-name create-command-name (tags vk-spec))
                             (fix-function-name delete-command-name (tags vk-spec)))))
       (format out "(defmacro ~a ((~a~(~{ ~a~}~) &key~(~{ ~a~}~)~:[~; extension-loader~]) &body body)~a~a)~%~%"
               (make-def-with-name create-command-name vk-spec)
               resource-arg-string
               (loop for p in required-create-params collect (fix-slot-name (name p) (get-type-name p) vk-spec))
               (loop for p in optional-create-params
                     collect (fix-slot-name (name p) (get-type-name p) vk-spec))
               extensionp
               doc-string
               body-string)))

(defun write-with-resource-macros (defwith-resource-file vk-spec &optional dry-run)
  (flet ((generate-with-resource-macros (stream)
           (format stream ";;; this file is automatically generated, do not edit~%~%")
           (format stream "(in-package :vk-utils)~%~%")
           (loop for handle in (sorted-elements (alexandria:hash-table-values (handles vk-spec)))
                 when (and (create-commands handle)
                           (delete-command handle))
                 do (loop for c in (create-commands handle)
                            do (write-with-resource stream handle c (delete-command handle) vk-spec)))))
    (if dry-run
        (generate-with-resource-macros t)
        (with-open-file (out defwith-resource-file :direction :output :if-exists :supersede)
          (generate-with-resource-macros out)))))

