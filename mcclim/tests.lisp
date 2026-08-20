(defpackage #:mcluv.tests
  (:use #:cl #:rove)
  (:local-nicknames (#:shader #:luv.shader)))

(in-package #:mcluv.tests)

(defun fresh-gpu-medium ()
  (make-instance 'mcluv:luv-gpu-medium))

(defstruct protocol-test-gpu-command clip)

(defmethod mcluv::rebase-gpu-command
    ((command protocol-test-gpu-command) offsets)
  (declare (ignore offsets))
  command)

(defmethod mcluv::prepare-gpu-command
    ((command protocol-test-gpu-command) mirror frame-build)
  (declare (ignore mirror frame-build))
  command)

(defmethod mcluv::gpu-command-clip ((command protocol-test-gpu-command))
  (protocol-test-gpu-command-clip command))

(defmethod mcluv::encode-gpu-command
    ((command protocol-test-gpu-command) pass frame-state)
  (list command pass frame-state))

(defclass gpu-command-spy-encoder (luv:gpu-command-encoder)
  ((commands :initform nil :accessor gpu-command-spy-commands)))

(defmethod luv:encode ((encoder gpu-command-spy-encoder) command)
  (push command (gpu-command-spy-commands encoder))
  encoder)

(deftest gpu-command-phases-are-an-open-command-grain-protocol
  (dolist (name '(mcluv::rebase-gpu-command
                  mcluv::prepare-gpu-command
                  mcluv::gpu-command-clip
                  mcluv::encode-gpu-command))
    (ok (typep (fdefinition name) 'generic-function)))
  (let ((command (make-protocol-test-gpu-command :clip '(1 2 3 4))))
    (ok (eq command (mcluv::rebase-gpu-command command nil)))
    (ok (eq command (mcluv::prepare-gpu-command command nil nil)))
    (ok (equal '(1 2 3 4) (mcluv::gpu-command-clip command)))
    (ok (equal (list command :pass :frame-state)
               (mcluv::encode-gpu-command command :pass :frame-state)))))

(deftest built-in-gpu-commands-rebase-their-own-dense-stream
  (let* ((offsets
           (mcluv::make-gpu-command-offsets
            :vertex 10 :analytic 20 :relief 30 :gradient 40 :image 50))
         (solid
           (mcluv::rebase-gpu-command
            (mcluv::make-gpu-solid-command
             :first-vertex 1 :vertex-count 6 :clip '(1 2 3 4))
            offsets))
         (analytic
           (mcluv::rebase-gpu-command
            (mcluv::make-gpu-analytic-command :first-vertex 2)
            offsets))
         (lattice
           (mcluv::rebase-gpu-command
            (mcluv::make-gpu-lattice-command
             :modules :modules :first-vertex 3)
            offsets))
         (relief
           (mcluv::rebase-gpu-command
            (mcluv::make-gpu-relief-analytic-command :first-vertex 4)
            offsets))
         (gradient
           (mcluv::rebase-gpu-command
            (mcluv::make-gpu-gradient-analytic-command :first-vertex 5)
            offsets))
         (design (list :design))
         (image
           (mcluv::rebase-gpu-command
            (mcluv::make-gpu-image-command
             :design design :first-vertex 6)
            offsets))
         (text (mcluv::make-gpu-text-command)))
    (ok (= 11 (mcluv::gpu-solid-command-first-vertex solid)))
    (ok (= 6 (mcluv::gpu-solid-command-vertex-count solid)))
    (ok (equal '(1 2 3 4) (mcluv::gpu-command-clip solid)))
    (ok (= 22 (mcluv::gpu-analytic-command-first-vertex analytic)))
    (ok (= 23 (mcluv::gpu-lattice-command-first-vertex lattice)))
    (ok (eq :modules (mcluv::gpu-lattice-command-modules lattice)))
    (ok (= 34 (mcluv::gpu-relief-analytic-command-first-vertex relief)))
    (ok (= 45 (mcluv::gpu-gradient-analytic-command-first-vertex gradient)))
    (ok (= 56 (mcluv::gpu-image-command-first-vertex image)))
    (ok (eq design (mcluv::gpu-image-command-design image)))
    (ok (eq text (mcluv::rebase-gpu-command text offsets)))))

(deftest every-built-in-command-has-its-required-phase-methods
  (let ((prepare (fdefinition 'mcluv::prepare-gpu-command)))
    (dolist (command
              (list (mcluv::make-gpu-solid-command)
                    (mcluv::make-gpu-analytic-command)
                    (mcluv::make-gpu-relief-analytic-command)
                    (mcluv::make-gpu-gradient-analytic-command)
                    (mcluv::make-gpu-lattice-command)
                    (mcluv::make-gpu-image-command)
                    (mcluv::make-gpu-text-command)))
      (ok (compute-applicable-methods prepare (list command nil nil)))))
  (let ((clip (fdefinition 'mcluv::gpu-command-clip))
        (encode (fdefinition 'mcluv::encode-gpu-command))
        (state (make-instance 'mcluv::gpu-mirror-frame-state :mirror nil)))
    (dolist (command
              (list (mcluv::make-gpu-solid-command)
                    (mcluv::make-gpu-analytic-command)
                    (mcluv::make-gpu-relief-analytic-command)
                    (mcluv::make-gpu-gradient-analytic-command)
                    (mcluv::make-gpu-prepared-lattice-command)
                    (mcluv::make-gpu-prepared-image-command)
                    (mcluv::make-gpu-prepared-text-command)))
      (ok (compute-applicable-methods clip (list command)))
      (ok (compute-applicable-methods encode (list command nil state))))))

(deftest native-command-encoders-map-to-their-dense-gpu-resources
  (let* ((mirror
           (make-instance 'mcluv:luv-gpu-mirror :sheet nil :target nil))
         (state
           (make-instance
            'mcluv::gpu-mirror-frame-state
            :mirror mirror
            :vertex-buffer :solid-buffer
            :analytic-buffer :analytic-buffer
            :relief-buffer :relief-buffer
            :gradient-buffer :gradient-buffer
            :image-buffer :image-buffer
            :text-buffer :text-buffer))
         (lattice-paint
           (mcluv::make-gpu-lattice-paint
            :texture :lattice-texture
            :view :lattice-view
            :bind-group :lattice-bind-group))
         (image-paint
           (make-instance
            'mcluv::gpu-cached-image-paint
            :texture :image-texture
            :view :image-view
            :bind-group :image-bind-group
            :width 16
            :height 8))
         (atlas :text-atlas))
    (setf (mcluv::gpu-mirror-pipeline mirror) :solid-pipeline
          (mcluv::gpu-mirror-analytic-pipeline mirror) :analytic-pipeline
          (mcluv::gpu-mirror-relief-pipeline mirror) :relief-pipeline
          (mcluv::gpu-mirror-gradient-analytic-pipeline mirror)
          :gradient-pipeline
          (mcluv::gpu-mirror-lattice-pipeline mirror) :lattice-pipeline
          (mcluv::gpu-mirror-image-pipeline mirror) :image-pipeline
          (mcluv::gpu-mirror-text-pipeline mirror) :text-pipeline
          (mcluv::gpu-mirror-bind-group mirror) :shape-bind-group
          (gethash atlas (mcluv::gpu-mirror-text-bind-groups mirror))
          :text-bind-group)
    (dolist (specification
              (list
               (list (mcluv::make-gpu-solid-command
                      :first-vertex 11 :vertex-count 3)
                     :solid-pipeline :shape-bind-group :solid-buffer 11 3)
               (list (mcluv::make-gpu-analytic-command
                      :first-vertex 22 :vertex-count 6)
                     :analytic-pipeline :shape-bind-group
                     :analytic-buffer 22 6)
               (list (mcluv::make-gpu-relief-analytic-command
                      :first-vertex 33 :vertex-count 9)
                     :relief-pipeline :shape-bind-group :relief-buffer 33 9)
               (list (mcluv::make-gpu-gradient-analytic-command
                      :first-vertex 44 :vertex-count 12)
                     :gradient-pipeline :shape-bind-group
                     :gradient-buffer 44 12)
               (list (mcluv::make-gpu-prepared-lattice-command
                      :paint lattice-paint :first-vertex 55 :vertex-count 15)
                     :lattice-pipeline :lattice-bind-group
                     :analytic-buffer 55 15)
               (list (mcluv::make-gpu-prepared-image-command
                      :paint image-paint :first-vertex 66 :vertex-count 18)
                     :image-pipeline :image-bind-group :image-buffer 66 18)
               (list (mcluv::make-gpu-prepared-text-command
                      :atlas atlas :first-vertex 77 :vertex-count 21)
                     :text-pipeline :text-bind-group :text-buffer 77 21)))
      (destructuring-bind
          (command expected-pipeline expected-bind-group expected-buffer
           expected-first-vertex expected-vertex-count)
          specification
        (let ((encoder (make-instance 'gpu-command-spy-encoder)))
          (mcluv::encode-gpu-command command encoder state)
          (let ((commands (reverse (gpu-command-spy-commands encoder))))
            (ok (= 4 (length commands)))
            (destructuring-bind
                (pipeline-command bind-group-command vertex-buffer-command
                 draw-command)
                commands
              (ok (typep pipeline-command 'luv:gpu-set-pipeline-command))
              (ok (eq expected-pipeline
                      (luv::gpu-set-pipeline-command-pipeline
                       pipeline-command)))
              (ok (typep bind-group-command
                         'luv:gpu-set-bind-group-command))
              (ok (zerop
                   (luv::gpu-set-bind-group-command-index
                    bind-group-command)))
              (ok (eq expected-bind-group
                      (luv::gpu-set-bind-group-command-bind-group
                       bind-group-command)))
              (ok (typep vertex-buffer-command
                         'luv:gpu-set-vertex-buffer-command))
              (ok (zerop
                   (luv::gpu-set-vertex-buffer-command-slot
                    vertex-buffer-command)))
              (ok (eq expected-buffer
                      (luv::gpu-set-vertex-buffer-command-buffer
                       vertex-buffer-command)))
              (ok (zerop
                   (luv::gpu-set-vertex-buffer-command-offset
                    vertex-buffer-command)))
              (ok (typep draw-command 'luv:gpu-draw-command))
              (ok (= expected-vertex-count
                     (luv::gpu-draw-command-vertex-count draw-command)))
              (ok (= 1 (luv::gpu-draw-command-instance-count draw-command)))
              (ok (= expected-first-vertex
                     (luv::gpu-draw-command-first-vertex draw-command)))
              (ok (zerop
                   (luv::gpu-draw-command-first-instance draw-command))))))))))

(deftest compositor-shaders-are-shared-mathematical-specifications
  (dolist (specification
            (list (mcluv::spinning-texture-vertex-specification)
                  (mcluv::spinning-texture-fragment-specification)
                  (mcluv::lisp-machine-chassis-vertex-specification)
                  (mcluv::lisp-machine-chassis-fragment-specification)))
    (ok (typep specification 'shader:shader-specification))
    (ok (> (length (spv:assemble-shader-specification specification)) 5))
    (ok (search "using namespace metal"
                (luv.msl:msl-document-source
                 (luv.msl:compile-msl specification))))))

(deftest filled-rectangles-become-one-analytic-command
  (let ((medium (fresh-gpu-medium)))
    (clim:medium-draw-rectangle* medium 10 20 110 60 t)
    (ok (= 1 (length (mcluv::gpu-medium-commands medium))))
    (ok (typep (aref (mcluv::gpu-medium-commands medium) 0)
               'mcluv::gpu-analytic-command))
    (ok (= 72 (length (mcluv::gpu-medium-analytic-vertices medium))))
    (ok (null (mcluv:gpu-medium-fallback-report medium)))))

(deftest full-ellipses-are-analytic-while-arcs-retain-the-fallback
  (let ((medium (fresh-gpu-medium)))
    (clim:medium-draw-ellipse*
     medium 80 60 35 8 -5 20 0 (* 2 pi) t)
    (ok (typep (aref (mcluv::gpu-medium-commands medium) 0)
               'mcluv::gpu-analytic-command))
    (ok (null (mcluv:gpu-medium-fallback-report medium)))
    (clim:medium-draw-ellipse*
     medium 80 60 35 8 -5 20 0 pi t)
    (ok (find :ellipse (mcluv:gpu-medium-fallback-report medium)
              :key (lambda (entry) (getf entry :primitive))))))

(deftest roundrect-command-carries-the-semantic-radius
  (let ((medium (fresh-gpu-medium)))
    (mcluv::medium-draw-analytic-rounded-rectangle*
     medium 10 20 110 60 12 t)
    (let ((vertices (mcluv::gpu-medium-analytic-vertices medium)))
      ;; Each vertex is position, local coordinate, half-size/radius, color.
      (ok (= 50.0 (aref vertices 6)))
      (ok (= 20.0 (aref vertices 7)))
      (ok (= 12.0 (aref vertices 8))))
    (ok (= 1 (length (mcluv::gpu-medium-commands medium))))
    (ok (null (mcluv:gpu-medium-fallback-report medium)))))

(deftest roundrect-has-a-native-mcclim-output-record
  (ok (find-class 'mcluv::draw-analytic-rounded-rectangle-output-record nil))
  (ok (typep (fdefinition 'mcluv::medium-draw-analytic-rounded-rectangle*)
             'generic-function)))

(deftest linear-gradient-coordinates-and-colors
  (let ((gradient
          (mcluv:make-linear-gradient
           10 20 110 20 clim:+black+ clim:+white+)))
    (ok (= 0.0 (mcluv::gradient-coordinate gradient 10 20)))
    (ok (= 0.5 (mcluv::gradient-coordinate gradient 60 20)))
    (ok (= 1.0 (mcluv::gradient-coordinate gradient 110 20)))
    (multiple-value-bind (red green blue alpha)
        (mcluv::color-rgba (mcluv::design-ink gradient 60 20))
      (ok (= 0.5 red))
      (ok (= 0.5 green))
      (ok (= 0.5 blue))
      (ok (= 1.0 alpha)))))

(deftest radial-gradient-coordinates
  (let ((gradient
          (mcluv:make-radial-gradient
           50 60 25 clim:+white+ clim:+black+)))
    (ok (= 0.0 (mcluv::gradient-coordinate gradient 50 60)))
    (ok (= 0.5 (mcluv::gradient-coordinate gradient 62.5 60)))
    (ok (= 1.0 (mcluv::gradient-coordinate gradient 50 85)))))

(deftest relief-design-is-a-semantic-height-bearing-ink
  (let* ((albedo (clim:make-rgb-color 0.2 0.4 0.7))
         (relief (mcluv:make-relief-design albedo 6.5))
         (transformed
           (clim:transform-region
            (clim:make-translation-transformation 10 20) relief)))
    (ok (eq albedo (mcluv::design-ink relief 12 14)))
    (ok (= 6.5 (mcluv:design-height relief)))
    (ok (= 0 (mcluv:design-height albedo)))
    (ok (= 6.5 (mcluv:design-height transformed)))
    (ok (eq albedo (mcluv:relief-albedo transformed)))))

(deftest relief-roundrect-is-one-dense-analytic-command
  (let ((medium (fresh-gpu-medium)))
    (setf (clim:medium-ink medium)
          (mcluv:make-relief-design
           (clim:make-rgb-color 0.2 0.4 0.7) 7.0))
    (mcluv::medium-draw-analytic-rounded-rectangle*
     medium 10 20 110 60 12 t)
    (ok (= 1 (length (mcluv::gpu-medium-commands medium))))
    (ok (typep (aref (mcluv::gpu-medium-commands medium) 0)
               'mcluv::gpu-relief-analytic-command))
    ;; Six vertices, each containing five packed float32x3 attributes.
    (ok (= 90 (length (mcluv::gpu-medium-relief-vertices medium))))
    (ok (= 7.0 (aref (mcluv::gpu-medium-relief-vertices medium) 12)))
    (ok (zerop (length (mcluv::gpu-medium-vertices medium))))
    (ok (zerop (length (mcluv::gpu-medium-analytic-vertices medium))))
    (ok (null (mcluv:gpu-medium-fallback-report medium)))))

(deftest gradient-roundrect-is-one-dense-analytic-command
  (let* ((medium (fresh-gpu-medium))
         (gradient
           (mcluv:make-linear-gradient
            10 20 110 20 clim:+black+ clim:+white+)))
    (setf (clim:medium-ink medium) gradient)
    (mcluv::medium-draw-analytic-rounded-rectangle*
     medium 10 20 110 60 12 t)
    (ok (= 1 (length (mcluv::gpu-medium-commands medium))))
    (ok (typep (aref (mcluv::gpu-medium-commands medium) 0)
               'mcluv::gpu-gradient-analytic-command))
    ;; Six vertices, each containing seven packed float32x3 attributes.
    (ok (= 126 (length (mcluv::gpu-medium-gradient-vertices medium))))
    (ok (zerop (length (mcluv::gpu-medium-vertices medium))))
    (ok (zerop (length (mcluv::gpu-medium-analytic-vertices medium))))
    (ok (null (mcluv:gpu-medium-fallback-report medium)))))

(defun tiny-image-pattern ()
  (clim:make-pattern
   (make-array '(4 4) :element-type '(unsigned-byte 32)
                       :initial-element #xff3366cc)
   nil))

(deftest draw-pattern-is-one-cached-image-command
  (let ((medium (fresh-gpu-medium))
        (pattern (tiny-image-pattern)))
    (climi::medium-draw-pattern* medium pattern 10 20)
    (ok (= 1 (length (mcluv::gpu-medium-commands medium))))
    (let ((command (aref (mcluv::gpu-medium-commands medium) 0)))
      (ok (typep command 'mcluv::gpu-image-command))
      (ok (eq pattern
              (mcluv::gpu-image-paint-source
               (mcluv::gpu-image-command-design command)))))
    (ok (= 72 (length (mcluv::gpu-medium-image-vertices medium))))
    (ok (null (mcluv:gpu-medium-fallback-report medium)))))

(deftest transformed-image-paint-keeps-source-and-affine-coordinates
  (let* ((pattern (tiny-image-pattern))
         (transformation (clim:make-transformation 2 0.5 -0.25 3 40 60))
         (paint (clim:transform-region transformation pattern)))
    (ok (eq pattern (mcluv::gpu-image-paint-source paint)))
    (multiple-value-bind (x y)
        (clim:transform-position transformation 2 3)
      (multiple-value-bind (u v)
          (mcluv::gpu-image-paint-coordinate paint x y)
        (ok (< (abs (- u 0.5)) 1.0e-6))
        (ok (< (abs (- v 0.75)) 1.0e-6))))))

(deftest polygons-use-native-gradient-and-image-paints
  (dolist (paint
            (list (mcluv:make-linear-gradient
                   0 0 100 0 clim:+black+ clim:+white+)
                  (tiny-image-pattern)))
    (let ((medium (fresh-gpu-medium)))
      (setf (clim:medium-ink medium) paint)
      (clim:medium-draw-polygon*
       medium #(10 10 90 20 70 80 20 70) t t)
      (ok (= 1 (length (mcluv::gpu-medium-commands medium))))
      (ok (typep (aref (mcluv::gpu-medium-commands medium) 0)
                 (if (typep paint 'mcluv:linear-gradient)
                     'mcluv::gpu-gradient-analytic-command
                     'mcluv::gpu-image-command))))))

(deftest commands-retain-mcclim-clipping-for-native-scissors
  (let ((medium (fresh-gpu-medium)))
    (setf (clim:medium-clipping-region medium)
          (clim:make-rectangle* 0.1 0.2 0.8 0.9))
    (clim:medium-draw-rectangle* medium 0 0 1 1 t)
    (let ((clip
            (mcluv::gpu-analytic-command-clip
             (aref (mcluv::gpu-medium-commands medium) 0))))
      (ok clip)
      (ok (equal '(0.1 0.2 0.8 0.9) clip)))))
