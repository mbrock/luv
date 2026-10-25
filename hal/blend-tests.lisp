(in-package #:luv.tests)

(defun check-native-blend-equation (blend factors)
  "Exercise the factors read back from native descriptors, without drawing."
  (destructuring-bind (source-rgb destination-rgb source-alpha destination-alpha)
      factors
    (labels ((factor (kind destination alpha)
               (ecase kind
                 (:zero 0)
                 (:one 1)
                 (:dst-color destination)
                 (:one-minus-src-alpha (- 1 alpha))))
             (combine (source destination alpha source-factor destination-factor)
               (+ (* source (factor source-factor destination alpha))
                  (* destination (factor destination-factor destination alpha)))))
      (dolist (alpha '(0 1/4 1))
        (loop for source in '(0 1/4 1)
              for destination in '(3/4 1/2 1/4)
              do (true
                  (= (if blend
                         (combine source destination alpha source-rgb destination-rgb)
                         source)
                     (ecase blend
                       ((nil) source)
                       (:premultiplied-alpha (+ source (* destination (- 1 alpha))))
                       (:multiply (* source destination))))))
        (true
         (= (if blend
                (combine alpha 3/8 alpha source-alpha destination-alpha)
                alpha)
            (ecase blend
              ((nil) alpha)
              (:premultiplied-alpha (+ alpha (* 3/8 (- 1 alpha))))
              (:multiply 3/8))))))))

(define-test vulkan-native-blend-descriptors-preserve-rgb-and-alpha-contracts
  (true (= 4 (cffi:foreign-enum-value 'lvk::blend-factor :dst-color)))
  (lvk::call-with-color-blend-attachments
   nil (lambda (count attachments)
         (true (zerop count))
         (true (cffi:null-pointer-p attachments))))
  (lvk::call-with-color-blend-attachments
   '(nil :premultiplied-alpha :multiply)
   (lambda (count attachments)
     (true (= 3 count))
     (loop for blend in '(nil :premultiplied-alpha :multiply)
           for expected in '((:one :zero :one :zero)
                             (:one :one-minus-src-alpha :one :one-minus-src-alpha)
                             (:dst-color :zero :zero :one))
           for index from 0
           for attachment =
             (cffi:mem-aptr attachments '(:struct lvk::pipeline-color-blend-attachment-state)
                            index)
           do (flet ((slot (name)
                       (cffi:foreign-slot-value
                        attachment '(:struct lvk::pipeline-color-blend-attachment-state)
                        name)))
                (true (= (if blend 1 0) (slot 'lvk::blend-enable)))
                (true (eq :add (slot 'lvk::color-blend-op)))
                (true (eq :add (slot 'lvk::alpha-blend-op)))
                (true (= 15 (cffi:foreign-bitfield-value
                             'lvk::color-component-flags (slot 'lvk::color-write-mask))))
                (let ((factors (mapcar #'slot '(lvk::src-color-blend-factor
                                                lvk::dst-color-blend-factor
                                                lvk::src-alpha-blend-factor
                                                lvk::dst-alpha-blend-factor))))
                  (true (equal expected factors))
                  (check-native-blend-equation blend factors)))))))

#+darwin
(progn
  (objc:define-objective-c-message blend-test-state ("blendingState" :uint64))
  (objc:define-objective-c-message blend-test-source-rgb ("sourceRGBBlendFactor" :uint64))
  (objc:define-objective-c-message blend-test-destination-rgb ("destinationRGBBlendFactor" :uint64))
  (objc:define-objective-c-message blend-test-source-alpha ("sourceAlphaBlendFactor" :uint64))
  (objc:define-objective-c-message blend-test-destination-alpha ("destinationAlphaBlendFactor" :uint64))
  (objc:define-objective-c-message blend-test-rgb-operation ("rgbBlendOperation" :uint64))
  (objc:define-objective-c-message blend-test-alpha-operation ("alphaBlendOperation" :uint64))
  (objc:define-objective-c-message blend-test-write-mask ("writeMask" :uint64)))

#+darwin
(define-test metal-native-blend-descriptors-preserve-rgb-and-alpha-contracts
  ;; These are CPU-side Metal descriptors: no device, compiler, or queue.
  (objc:with-autorelease-pool ()
    (loop for class in '("MTL4RenderPipelineDescriptor" "MTL4MeshRenderPipelineDescriptor")
          for constructor in '(metal::%new-metal-4-render-pipeline-descriptor
                               metal::%new-metal-4-mesh-render-pipeline-descriptor)
          do (objc:with-owned-objective-c-object
                 (descriptor (funcall constructor (objc:find-objective-c-class class)))
               (loop with attachments = (metal::%render-pipeline-color-attachments descriptor)
                     for blend in '(nil :premultiplied-alpha :multiply)
                     for expected in '((1 0 1 0) (1 5 1 5) (6 0 0 1))
                     for index from 0
                     for attachment =
                       (metal::%render-pipeline-color-attachment-at attachments index)
                     do (metal::configure-metal-color-attachment-blend attachment blend)
                        (true (= (if blend 1 0) (blend-test-state attachment)))
                        (true (zerop (blend-test-rgb-operation attachment)))
                        (true (zerop (blend-test-alpha-operation attachment)))
                        (true (= 15 (blend-test-write-mask attachment)))
                        (let ((factors (list (blend-test-source-rgb attachment)
                                             (blend-test-destination-rgb attachment)
                                             (blend-test-source-alpha attachment)
                                             (blend-test-destination-alpha attachment))))
                          (true (equal expected factors))
                          (check-native-blend-equation
                           blend (mapcar (lambda (value)
                                           (ecase value
                                             (0 :zero)
                                             (1 :one)
                                             (5 :one-minus-src-alpha)
                                             (6 :dst-color)))
                                         factors))))))))

(defun check-pipeline-blend-validation (device layout vertex mesh fragment)
  ;; Deliberately foreign-owned modules stop after descriptor validation,
  ;; before native pipeline creation.  Rejection must retain the source.
  (dolist (mesh-p '(nil t))
    (dolist (blend '(nil :premultiplied-alpha :multiply :unknown-blend))
      (let* ((targets (list (list :format :rgba16-float :blend blend)
                            (list :format :rgba8-unorm)))
             (snapshot (copy-tree targets))
             (fragment-description (list :module fragment :targets targets))
             (descriptor
               (if mesh-p
                   (luv:make-mesh-render-pipeline-descriptor
                    :layout layout :mesh (list :module mesh) :fragment fragment-description)
                   (luv:make-render-pipeline-descriptor
                    :layout layout :vertex (list :module vertex) :fragment fragment-description))))
        (if (eq blend :unknown-blend)
            (let ((condition (gpu-request-condition (lambda () (luv:create device descriptor)))))
              (true (typep condition 'luv:gpu-request-error))
              (true (eq descriptor (luv:gpu-request-error-descriptor condition))))
            (fail (luv:create device descriptor) 'luv:gpu-device-mismatch-error))
        (true (equal snapshot targets))
        (true (eq fragment-description
                  (if mesh-p
                      (luv::mesh-render-pipeline-descriptor-fragment descriptor)
                      (luv::render-pipeline-descriptor-fragment descriptor))))))))

(define-test vulkan-vertex-and-mesh-validate-blends-without-changing-source
  (let* ((device (make-instance 'luv::vulkan-gpu-device :handle :fake-device
                                :physical-device :fake-physical-device))
         (other-device (make-instance 'luv::vulkan-gpu-device :handle :other-device))
         (layout (make-instance 'luv::vulkan-gpu-bind-group-layout
                                :handle :fake-layout :device device))
         (module (make-instance 'luv::vulkan-gpu-shader-module
                                :handle :fake-module :device other-device))
         (symbol 'lvk:physical-device-mesh-shader-p)
         (original (symbol-function symbol)))
    ;; Fake handles own no native resources, including during finalization.
    #+sbcl (mapc #'sb-ext:cancel-finalization (list layout module))
    (unwind-protect
         (progn
           (setf (symbol-function symbol) (lambda (physical-device)
                                           (declare (ignore physical-device)) t))
           (check-pipeline-blend-validation device layout module module module))
      (setf (symbol-function symbol) original))))

#+darwin
(define-test metal-vertex-and-mesh-validate-blends-without-changing-source
  (let* ((device (make-instance 'luv::metal-gpu-device :native-object :fake-device))
         (other-device (make-instance 'luv::metal-gpu-device :native-object :other-device))
         (modules (loop for type in (list metal:+function-type-vertex+
                                          metal:+function-type-mesh+
                                          metal:+function-type-fragment+)
                        collect (make-instance 'luv::metal-gpu-shader-module
                                               :native-object :fake-module
                                               :device other-device
                                               :entry-point "main" :function-type type)))
         (symbol 'metal:metal-device-supports-texture-sample-count-p)
         (original (symbol-function symbol)))
    (unwind-protect
         (progn
           (setf (symbol-function symbol) (lambda (native-device count)
                                           (declare (ignore native-device count)) 1))
           (apply #'check-pipeline-blend-validation device nil modules))
      (setf (symbol-function symbol) original))))
