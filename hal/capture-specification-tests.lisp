(in-package #:luv.tests)

(deftest capture-specifications-have-stable-wiki-paths-and-live-redefinition
  (let* ((name "capture-protocol-test")
         (first
           (make-instance 'luv:capture-specification
                          :name name :figure-id "ABC12G" :kind :image
                          :description "first" :extension "png"
                          :renderer #'identity))
         (second
           (make-instance 'luv:capture-specification
                          :name name :figure-id "ABC12G" :kind :image
                          :description "second" :extension "webp"
                          :renderer #'identity)))
    (unwind-protect
         (progn
           (luv:register-capture-specification first)
           (ok (string= "ABC12G-capture-protocol-test.png"
                        (file-namestring
                         (luv:capture-output-pathname first #P"build/"))))
           (luv:register-capture-specification second)
           (ok (eq second (luv:find-capture-specification name)))
           (ok (= 1 (count name (luv:capture-specifications)
                           :key #'luv:capture-specification-name
                           :test #'string=))))
      (remhash name luv::*capture-specifications*)
      (setf luv::*capture-specification-order*
            (remove name luv::*capture-specification-order* :test #'string=)))))
