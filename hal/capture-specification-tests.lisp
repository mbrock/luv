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
                          :layout :portrait
                          :renderer #'identity)))
    (unwind-protect
         (progn
           (luv:register-capture-specification first)
           (ok (string= "ABC12G-capture-protocol-test.png"
                        (file-namestring
                         (luv:capture-output-pathname first #P"build/"))))
           (luv:register-capture-specification second)
           (ok (eq second (luv:find-capture-specification name)))
           (ok (eq :portrait (luv:capture-specification-layout second)))
           (ok (= 1 (count name (luv:capture-specifications)
                           :key #'luv:capture-specification-name
                           :test #'string=))))
      (remhash name luv::*capture-specifications*)
      (setf luv::*capture-specification-order*
            (remove name luv::*capture-specification-order* :test #'string=)))))

(deftest capture-web-derivative-is-smaller-and-keeps-the-original
  (let* ((directory
           (uiop:ensure-directory-pathname
            (merge-pathnames
             (format nil "luv-capture-web-test-~36R/"
                     (random most-positive-fixnum))
             (uiop:temporary-directory))))
         (specification
           (make-instance 'luv:capture-specification
                          :name "web-derivative-test"
                          :figure-id "WEB12G"
                          :kind :image
                          :description "responsive image test"
                          :extension "png"
                          :renderer #'identity))
         (original (luv:capture-output-pathname specification directory))
         (responsive (luv::capture-responsive-image-pathname original)))
    (unwind-protect
         (progn
           (ensure-directories-exist original)
           (luv:write-rgba-png
            original
            (make-array (* 800 400 4)
                        :element-type '(unsigned-byte 8)
                        :initial-element 127)
            800 400 :rgba8-unorm)
           (luv::prepare-capture-web-media specification original)
           (ok (probe-file original))
           (ok (probe-file responsive))
           (multiple-value-bind (width height)
               (luv::capture-media-dimensions responsive)
             (ok (= 768 width))
             (ok (= 384 height)))
           (let* ((entry
                    (luv::capture-manifest-entry specification directory))
                  (variants (getf entry :variants))
                  (variant (find 768 variants :key (lambda (item)
                                                     (getf item :width)))))
             (ok (string= "WEB12G-web-derivative-test.png"
                          (getf entry :file)))
             (ok (eq :landscape (getf entry :layout)))
             (ok (equal '(480 768) (mapcar (lambda (item)
                                             (getf item :width))
                                           variants)))
             (ok (string= "WEB12G-web-derivative-test-768w.webp"
                          (getf variant :file)))
             (ok (= 768 (getf variant :width)))))
      (uiop:delete-directory-tree directory :validate t
                                            :if-does-not-exist :ignore))))
