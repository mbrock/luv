;;;; Compare current indexed production to the preceding vertex-pulled visibility.
(require :asdf)
(let ((*package* (find-package :cl-user)))
  (with-open-file (stream "wiki/measurements/yawl-visibility-c8f1d28/run.lisp")
    (loop for form = (read stream nil :eof)
          until (eq form :eof)
          unless (and (consp form) (= (length form) 1)
                      (symbolp (first form)) (string= (symbol-name (first form)) "MAIN"))
            do (eval form))))

(in-package #:gpu-fps)
(defvar *indexed-build-p* nil)
(defvar *indexed-drawings* (make-hash-table :test #'eq))

;; Only these four definitions differ in the control. All other production
;; paths, resident publications, buffers, cameras and finishing are shared.
(let* ((names '(luft.render.shaders:terrain-visibility-vertex-specification
                luft.render.shaders:terrain-visibility-fragment-specification
                r::make-terrain-visibility-binding r::encode-terrain-visibility))
       (current (mapcar (lambda (name) (cons name (symbol-function name))) names)))
  (dolist (path '("luft/render/terrain-visibility-shaders.lisp" "luft/render/terrain-drawing.lisp"))
    (let ((*package* (find-package :cl-user)))
      (with-input-from-string
          (stream (uiop:run-program
                   (list "git" "show"
                         (concatenate 'string "294b7238896c8242162f29ad04d3f2af5b230e6f:" path))
                   :output :string))
        (loop for form = (read stream nil :eof) until (eq form :eof) do
          (cond ((eq (first form) 'in-package) (eval form))
                ((member (second form) names)
                 (eval (subst 196608 'luft.render.shaders::+terrain-visibility-resident-stride+ form))))))))
  (dolist (entry current)
    (let* ((name (car entry)) (indexed (cdr entry)) (reference (symbol-function name))
           (shader-p (eq (symbol-package name) (find-package :luft.render.shaders))))
      (setf (symbol-function name)
            (lambda (&rest args)
              (apply (if (if shader-p *indexed-build-p* (gethash (first args) *indexed-drawings*))
                         indexed reference)
                     args))))))

(let ((original (symbol-function 'r::make-terrain-drawing)))
  (setf (symbol-function 'r::make-terrain-drawing)
        (lambda (&rest args)
          (let ((drawing (apply original args)))
            (setf (gethash drawing *indexed-drawings*) *indexed-build-p*)
            drawing))))

(let ((original (symbol-function 'measure)))
  (setf (symbol-function 'measure)
        (lambda (&rest args)
          (setf *indexed-build-p* t)
          (unwind-protect (apply original args) (setf *indexed-build-p* nil)))))

;; Read back both identity attachments as well as the harness's depth/HDR/
;; motion/presentation. These copies are outside every timed interval.
(defmethod luv:create :around ((device luv::metal-gpu-device) (descriptor luv::texture-descriptor))
  (let ((copy (luv::canonical-texture-descriptor descriptor)))
    (when (and (= 1 (luv::texture-descriptor-sample-count copy))
               (member (luv::texture-descriptor-format copy)
                       '(:rgba8-unorm :rgba16-float :rg16-float :depth32-float))
               (member :render-attachment (luv::texture-descriptor-usage copy)))
      (pushnew :copy-src (luv::texture-descriptor-usage copy)))
    (call-next-method device copy)))

(defmethod luv:create :around ((device luv::vulkan-gpu-device) (descriptor luv::texture-descriptor))
  (let ((copy (luv::canonical-texture-descriptor descriptor)))
    (when (and (= 1 (luv::texture-descriptor-sample-count copy))
               (member (luv::texture-descriptor-format copy)
                       '(:rgba8-unorm :rgba16-float :rg16-float :depth32-float))
               (member :render-attachment (luv::texture-descriptor-usage copy)))
      (pushnew :copy-src (luv::texture-descriptor-usage copy)))
    (call-next-method device copy)))

(defun capture-identities (renderer prefix)
  (let* ((device (r::renderer-device renderer))
         (queue (luv:device-queue device))
         (generation (r::renderer-target-generation renderer)))
    (dolist (entry (list (list "sites" (r::renderer-target-generation-visibility-sites-texture generation))
                        (list "primitives" (r::renderer-target-generation-visibility-primitives-texture generation))))
      (let* ((texture (second entry))
             (size (luv::gpu-texture-size texture))
             (buffer (luv:create device (luv:make-buffer-descriptor
                                        :size (* 4 (first size) (second size)) :usage '(:copy-dst))))
             (encoder (luv:create device (luv:make-command-encoder-descriptor)))
             (commands nil))
        (unwind-protect
             (progn
               (luv:encode encoder (luv::make-gpu-copy-texture-to-buffer-command
                                    :source texture :destination buffer))
               (setf commands (luv:finish encoder))
               (luv:submit queue commands)
               (luv:submitted-work-done queue)
               (with-open-file (out (concatenate 'string prefix "." (first entry))
                                    :direction :output :if-exists :supersede
                                    :element-type '(unsigned-byte 8))
                 (write-sequence (luv:read-buffer buffer) out)))
          (when commands (luv:destroy commands))
          (luv:destroy encoder)
          (luv:destroy buffer))))))

(let ((original (symbol-function 'production-frame)))
  (setf (symbol-function 'production-frame)
        (lambda (host renderer target width height enabled &optional capture)
          (declare (ignore enabled))
          (prog1 (funcall original host renderer target width height t capture)
            (when capture (capture-identities renderer capture))))))

(main)
