;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; generate.lisp --- generate CFFI bindings from vk.xml file.
;;;
;;; Copyright (c) 2016, Bart Botta  <00003b@gmail.com>
;;;   All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;

(in-package :vk-generator)

(defun make-arg-type (arg-name type-info vk-spec)
  "TODO"
  (let* ((type-name (type-name type-info))
         (primitive-type (gethash type-name *vk-platform*))
         (vk-type (gethash type-name (types vk-spec)))
         (fixed-type-name (let ((fixed-type (fix-type-name type-name (tags vk-spec))))
                            (cond
                              ((member (category vk-type) '(:struct :union))
                               (list (category vk-type) fixed-type))
                              ((member type-name *opaque-struct-types* :test #'string=)
                               (list :struct fixed-type))
                              (t fixed-type))))
         (pointer-type  (if primitive-type
                            primitive-type
                            fixed-type-name)))
    (cond
      ;; not a pointer
      ((and (not (string= (postfix type-info) "*"))
            (not (string= (postfix type-info) "**"))
            (not (string= (postfix type-info) "* const*"))
            (not (string= (postfix type-info) "* const *")))
       (if primitive-type
           primitive-type
           fixed-type-name))
      ;; char* should map to a string
      ((and primitive-type
            (eq primitive-type :char)
            (string= (postfix type-info) "*"))
       :string)
      ;; pointer types
      ((string= (postfix type-info) "*") (list :pointer pointer-type))
      ((or (string= (postfix type-info) "**")
           (string= (postfix type-info) "* const*")
           (string= (postfix type-info) "* const *"))
       (list :pointer (list :pointer pointer-type))))))

(defun fixed-size-array-sizes-p (array-sizes)
  (or (> (length array-sizes) 1)
      (not (alexandria:starts-with-subseq "VK_" (first array-sizes)))))

(defun prepare-array-sizes (array-sizes vk-spec)
  (cond
    ((= (length array-sizes) 1)
     (if (alexandria:starts-with-subseq "VK_" (first array-sizes))
         (number-value (gethash (first array-sizes) (constants vk-spec)))
         (parse-integer (first array-sizes))))
    ;; multidimensional arrays
    ((> (length array-sizes) 0)
     (reduce #'* (mapcar (lambda (array-size) (parse-integer array-size)) array-sizes)))
    (t array-sizes)))

(defun write-extension-names (out vk-spec)
  (format out ";;; extension names~%")
  (loop for name in (sort (alexandria:hash-table-keys (extension-names vk-spec)) #'string<)
        do (format out "(alexandria:define-constant +~(~a~)+ ~s
  :test #'string=
  :documentation ~s)~%"
                   (ppcre:regex-replace-all
                    "^VK-" (substitute #\- #\_ name) "")
                   (gethash name (extension-names vk-spec))
                   (format nil "The name of the extension [~a](https://www.khronos.org/registry/vulkan/specs/1.2-extensions/man/html/~a.html)."
                           (string (gethash name (extension-names vk-spec)))
                           (gethash name (extension-names vk-spec)))))
  (format out "~%"))

(defun write-base-types (out vk-spec)
  (loop for base-type in (sorted-elements (alexandria:hash-table-values (base-types vk-spec)))
        for cffi-type = (gethash (type-name base-type) *vk-platform*)
        do (format out "~((defctype ~a ~s)~)~%~%"
                   (fix-type-name (name base-type) (tags vk-spec))
                   (if (eq cffi-type :void) :pointer cffi-type)))
  (format out "(defctype handle :pointer)~%")
  (format out "#.(if (= 8 (foreign-type-size :pointer))~%  '(defctype non-dispatch-handle :pointer)~%  '(defctype non-dispatch-handle :uint64))~%~%")

  ;; todo: get these from VK-SPEC (e.g. (find-if (lambda (t) (and (= (category t) :basetype) (not (gethash (name t) (base-types vk-spec))))) (alexandria:hash-table-values (types vk-spec))) ... )
  ;; misc OS types that are just passed around as pointers
  (loop for name in *opaque-types*
        ;; fixme: these used to be :void, but :void is only valid as a return type
        do (format out "~((defctype ~a :pointer)~)~%~%"
                   (fix-type-name name (tags vk-spec))))
  (loop for name being each hash-key of *misc-os-types*
        using (hash-value type)
        do (format out "~((defctype ~a ~s)~)~%~%"
                   (fix-type-name name (tags vk-spec)) type))
  (loop for name in *opaque-struct-types*
        do (format out "~((defcstruct ~a)~)~%~%"
                   (fix-type-name name (tags vk-spec)))))

(defun write-handles (out vk-spec)
  (loop for handle in (sorted-elements (alexandria:hash-table-values (handles vk-spec)))
        ;; TODO: after 6 months I have no idea if what I'm doing here is correct... thanks past-me
        ;; there are some functions without a handle, belonging to a handle with an empty string as its name
        when (not (string= (name handle) ""))
        ;; handles are pointers to foo_T struct
        ;; on 32bit platform, 'non-dispatch' handles are 64bit int,
        ;; otherwise pointer to foo_T struct
        do (labels ((write-handle (handle-name)
                      (format out "(~(defctype ~a ~a~))~%~%"
                              (fix-type-name handle-name (tags vk-spec))
                              (if (non-dispatch-handle-p handle)
                                  "non-dispatch-handle"
                                  "handle"))))
             (write-handle (name handle))
             (when (alias handle) (write-handle (alias handle))))))

(defun write-bitfield (out bitmask-name bitmask vk-spec)
  (let* ((base-type (type-name bitmask))
         (requires (requires bitmask))
         (enum (gethash requires (enums vk-spec)))
         (bits  (if enum (enum-values (gethash requires (enums vk-spec))) '()))
         (last-bit (alexandria:lastcar bits))
         (prefix "VK_")
         (fixed-name (string (fix-type-name bitmask-name (tags vk-spec)))))
    (format out "(defbitfield (~(~a~@[ ~a~]~))"
            fixed-name
            (when (stringp base-type) (fix-type-name base-type (tags vk-spec))))
    ;; possibly shouldn't strip prefix from things like
    ;; VK_QUERY_RESULT_64_BIT or VK_SAMPLE_COUNT_1_BIT where
    ;; only :64 or :1 is left?
    (let ((p (search "-FLAG" fixed-name)))
      (when p
        (setf prefix (format nil "VK_~a"
                             (substitute #\_ #\- (subseq fixed-name 0 (1+ p)))))
        (when (alexandria:ends-with-subseq "Flags2KHR" bitmask-name)
          (setf prefix (format nil "~a2_" prefix)))))
    (loop for enum-value in bits
          for comment = (comment enum-value)
          do (format out "~%  (:~(~a~) #x~x)"
                     (fix-bit-name (name enum-value) (tags vk-spec) :prefix prefix)
                     (number-value enum-value))
          when (string= (name last-bit) (name enum-value))
          do (format out ")")
          when comment
          do (format out " ;; ~a" comment))
    (format out "~:[)~;~]~%~%" bits)))

(defun write-bitfields (out vk-spec)
  ;; NOTE: in the original generator bitfields where created for the flags (a type) and the flagbits (an enum)
  ;;       now we only write the flags using the flagbits as values and later write the flagbits as a separate enum
  ;;       I guess comparisons between the two should still work after this, but I'll have to check
  (loop for bitmask in (sorted-elements (alexandria:hash-table-values (bitmasks vk-spec)))
        for name = (name bitmask)
        for alias = (alias bitmask)
        do (write-bitfield out name bitmask vk-spec)
           (when alias (write-bitfield out alias bitmask vk-spec))))

(defun write-enum (out enum-name enum vk-spec)
  ;; TODO: before 1.07 there was an "expand" attribute for enum tags
  (let* ((values (enum-values enum))
         (last-value (alexandria:lastcar values))
         (prefix "VK_")
         (fixed-name (string (fix-type-name enum-name (tags vk-spec)))))
    ;; originally bitmasks were omitted
    ;; originally flagbits without values were omitted
    (if (string-equal fixed-name "RESULT")
        ;; work around cffi bug: cffi always uses unsigned
        ;; type for enums, and VkResult has negative values
        (format out "(defcenum (~(~a :int~))" fixed-name)
        (format out "(defcenum (~(~a~))" fixed-name))
    (when values
      (setf prefix (find-enum-prefix fixed-name values (tags vk-spec))))
    ;; needs special handling since there are some bits with a different prefix...
    (when (string= enum-name "VkPipelineCreateFlagBits")
      (setf prefix "VK_PIPELINE_CREATE_"))
    (loop for enum-value in values
          for comment = (comment enum-value)
          for fixed-enum-value-name = (fix-bit-name (name enum-value)
                                                    (tags vk-spec)
                                                    :prefix (if (and (string= enum-name "VkPipelineCreateFlagBits")
                                                                     (not (alexandria:starts-with-subseq prefix (name enum-value))))
                                                                "VK_PIPELINE_"
                                                                prefix))
          do (format out "~%  (:~(~a~) ~:[#x~x~;~d~])"
                     fixed-enum-value-name
                     (minusp (number-value enum-value)) (number-value enum-value))
          when (string= (name last-value) (name enum-value))
          do (format out ")")
          when comment
          do (format out " ;; ~a" comment))
    (format out "~:[)~;~]~%~%" values)
    (when (string-equal fixed-name "RESULT")
      ;; write out error->comment, since they seem useful
      ;; enough to print out to users in errors
      (format out "(defparameter *result-comments*~%  (alexandria:plist-hash-table~%    '(~{~(:~a~) ~s~^~%     ~})))~%~%"
              (loop for enum-value in values
                    collect (fix-bit-name (name enum-value) (tags vk-spec) :prefix prefix)
                    collect (comment enum-value))))))

(defun write-enums (out vk-spec)
  (loop for enum in (sorted-elements (alexandria:hash-table-values (enums vk-spec)))
        for name = (name enum)
        for alias = (alias enum)
        do (write-enum out name enum vk-spec)
           (when alias (write-enum out alias enum vk-spec))))

(defun write-function-pointer-types (out vk-spec)
  ;; TODO: return type and argument types should be documented somewhere (need to be stored in func-pointer first...)
  (loop for func-pointer in (sorted-elements (alexandria:hash-table-values (func-pointers vk-spec)))
        do (format out "~((defctype ~a :pointer)~)~%~%"
                   (fix-type-name (name func-pointer) (tags vk-spec)))))

(defun write-structs (out vk-spec)
  (loop with dumped = (make-hash-table :test 'equal)
        for structure in (sorted-elements (alexandria:hash-table-values (structures vk-spec)))
        do (labels
               ((dump (struct struct-name)
                  (unless (gethash struct-name dumped)
                    (setf (gethash struct-name dumped) t)
                    (loop for member-value in (members struct)
                          for member-type = (get-type-name member-value)
                          when (and (gethash member-type (structures vk-spec))
                                    (not (gethash (get-type-name member-value) dumped)))
                          do (dump (gethash member-type (structures vk-spec)) member-type))
                    (let ((fixed-type-name (fix-type-name struct-name (tags vk-spec))))
                      (if (is-union-p struct)
                          (format out "(defcunion ~(~a~)" fixed-type-name)
                          (format out "(defcstruct (~(~a :class c-~a~))" fixed-type-name fixed-type-name)))
                    (loop for member-value in (get-cstruct-members struct vk-spec)
                          for name = (fix-type-name (name member-value) (tags vk-spec))
                          for member-type = (make-arg-type name (type-info member-value) vk-spec)
                          for array-count = (prepare-array-sizes (array-sizes member-value) vk-spec)
                          ;; TODO: what exactly should bit-count do? see VkAccelerationStructureInstanceKHR
                          do ;; todo: CFFI-bug :count is not respected if member type is a struct and not a primitive
                          (format out "~%  ~1{(~(~a ~s~@[ :count ~a~])~)~}"
                                  (list name member-type array-count)))
                    (format out "~:[)~;~]~%~%" nil)
                    (loop for struct-alias in (aliases structure)
                          when (not (gethash struct-alias dumped))
                          do (dump structure struct-alias)))))
             (dump structure (name structure)))))

(defun write-types-file (types-file vk-spec &optional dry-run)
  (flet ((write-to-stream (out)
           (format out ";;; this file is automatically generated, do not edit~%")
           (format out "#||~%~a~%||#~%~%" (vulkan-license-header vk-spec))
           (format out "(in-package :~a)~%~%" *in-package-name*)
    
           (write-extension-names out vk-spec)
           (write-base-types out vk-spec)
           (write-handles out vk-spec)
           (write-bitfields out vk-spec)
           (write-enums out vk-spec)
           (write-function-pointer-types out vk-spec)
           (write-structs out vk-spec)))
    (if dry-run
        (write-to-stream t)
        (with-open-file (out types-file :direction :output :if-exists :supersede)
          (write-to-stream out)))))
