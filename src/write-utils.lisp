(in-package :vk-generator)

(defun determine-param-default-value-string (param vk-spec)
  (with-slots (name type-info) param
    (with-slots (type-name postfix) type-info
      (if (string= "pAllocator" name)
          "vk:*default-allocator*"
          (cond
            ((handlep type-name vk-spec)
             (let ((h (get-handle type-name vk-spec)))
               (format nil "(vk:make-~a-wrapper (cffi:null-pointer))"
                       (fix-type-name type-name (tags vk-spec)))))
            ((and (string= type-name "char")
                  (not (string= postfix "**")))
             "\"\"")
            (t nil))))))

(defun get-vk-class-slots (struct vk-spec)
  (let ((count-member-names (get-count-member-names struct)))
    (loop for m in (members struct)
          unless (or (member (name m) count-member-names :test #'string=)
                     (= (length (allowed-values m)) 1))
          collect m)))

(defun get-fixed-vk-class-slot-names (struct vk-spec)
  (map
   'list
   (lambda (m)
     (fix-slot-name (name m) (get-type-name m) vk-spec))
   (get-vk-class-slots struct vk-spec)))

