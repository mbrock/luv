(in-package #:luv.tests)

(define-test capture-specifications-have-stable-wiki-paths-and-live-redefinition
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
           (true (string= "ABC12G-capture-protocol-test.png"
                          (file-namestring
                           (luv:capture-output-pathname first #P"build/"))))
           (luv:register-capture-specification second)
           (true (eq second (luv:find-capture-specification name)))
           (true (eq :portrait (luv:capture-specification-layout second)))
           (true (= 1 (count name (luv:capture-specifications)
                             :key #'luv:capture-specification-name
                             :test #'string=))))
      (remhash name luv::*capture-specifications*)
      (setf luv::*capture-specification-order*
            (remove name luv::*capture-specification-order* :test #'string=)))))

(define-test capture-web-derivative-is-smaller-and-keeps-the-original
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
           (true (probe-file original))
           (true (probe-file responsive))
           (multiple-value-bind (width height)
               (luv::capture-media-dimensions responsive)
             (true (= 768 width))
             (true (= 384 height)))
           (let* ((entry
                    (luv::capture-manifest-entry specification directory))
                  (variants (getf entry :variants))
                  (variant (find 768 variants :key (lambda (item)
                                                     (getf item :width)))))
             (true (string= "WEB12G-web-derivative-test.png"
                            (getf entry :file)))
             (true (eq :landscape (getf entry :layout)))
             (true (equal '(480 768) (mapcar (lambda (item)
                                               (getf item :width))
                                             variants)))
             (true (string= "WEB12G-web-derivative-test-768w.webp"
                            (getf variant :file)))
             (true (= 768 (getf variant :width)))))
      (uiop:delete-directory-tree directory :validate t
                                            :if-does-not-exist :ignore))))
