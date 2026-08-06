#|
 Copyright(c) 2020 - Lukas Herzberger <herzberger.lukas@gmail.com>
 SPDX-License-Identifier: MIT
|#

(in-package #:vk-generator)

(defclass compound-member (member-data)
  ((members
    :initarg :members
    :accessor members)))

(defun get-cstruct-members (struct vk-spec)
  (flet ((resolve-size (type-name)
           (cond
             ((string= type-name "uint32_t") 32)
             ((string= type-name "uint64_t") 64)
             (t (error "Unexpected TYPE-NAME for compound member: ~a" type-name))))
         (make-compound (compound)
           (when compound
             (let ((first-member (first compound)))
               (assert (member (get-type-name first-member) '("uint32_t" "uint64_t") :test #'string=)
                       () "Unexpected type name for compound member: ~a" (get-type-namefirst-member))
               (make-instance 'compound-member
                              :name (format nil "~(~{~a~^-and-~}~)"
                                            (map 'list
                                                 (lambda (m)
                                                   (fix-type-name (name m) (tags vk-spec)))
                                                 compound))
                              :comment (comment first-member)
                              :array-sizes (array-sizes first-member)
                              :bit-count (bit-count first-member)
                              :type-info (type-info first-member)
                              :len (len first-member)
                              :no-autovalidity-p (no-autovalidity-p first-member)
                              :optional-p (optional-p first-member)
                              :selection (selection first-member)
                              :selector (selector first-member)
                              :allowed-values (allowed-values first-member)
                              :used-constant (used-constant first-member)
                              :members compound))))
         (copy-member-data (m)
           (make-instance 'member-data
                          :name (name m)
                          :comment (comment m)
                          :array-sizes (array-sizes m)
                          :bit-count (bit-count m)
                          :type-info (type-info m)
                          :len (len m)
                          :no-autovalidity-p (no-autovalidity-p m)
                          :optional-p (optional-p m)
                          :selection (selection m)
                          :selector (selector m)
                          :allowed-values (allowed-values m)
                          :used-constant (used-constant m))))
    (loop with remaining-bits = 0
          with compound = nil
          for m in (members struct)
          for type-info = (type-info m)
          for bits = (when (bit-count m)
                       (parse-integer (bit-count m)))
          when (= remaining-bits 0) do (setf compound nil)
          when (and bits (= remaining-bits 0)) do (setf remaining-bits (resolve-size (type-name type-info)))
          when bits do (progn
                         (setf remaining-bits (- remaining-bits bits))
                         (push m compound))
          when (= remaining-bits 0) collect (or (make-compound (reverse compound))
                                                (copy-member-data m)))))
