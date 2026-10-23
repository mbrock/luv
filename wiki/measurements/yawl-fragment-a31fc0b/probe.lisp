;; Disposable-process ablations. Never load into the managed game image.
(in-package #:luft.render.shaders)

(define-shader-function probe-screen-relief-normal (normal world strength height)
  ;; Recover the tangent-plane gradient from the quad's existing height
  ;; samples rather than evaluating the entire material two more times.
  (let* ((dx (derivative-x world))
         (dy (derivative-y world))
         (hx (derivative-x height))
         (hy (derivative-y height))
         (rx (cross-product dy normal))
         (ry (cross-product normal dx))
         (det (dot dx rx)))
    (if (> (abs det) 0.000000000001)
        (normalize (- normal (* (+ (* rx hx) (* ry hy))
                                (/ (* strength 0.09) det))))
        normal)))

(defvar *probe-original-fragment* (symbol-function 'star-fragment-specification))

(defun probe-fragment-specification ()
  (let* ((original (funcall *probe-original-fragment*))
         (source (luv.shader:shader-object-source-form original))
         (body (copy-tree (cdddr source)))
         (mode (uiop:getenv "FRAGMENT_PROBE")))
    (assert (eq (caar body) 'let*))
    (flet ((replace-binding (name expression)
             (let ((binding (assoc name (second (first body)))))
               (assert binding)
               (setf (second binding) expression))))
      (cond
        ((equal mode "normal-off")
         (replace-binding 'bent 'normal))
        ((equal mode "relief-off")
         (replace-binding 'bent 'normal)
         (replace-binding 'height 0.5))
        ((equal mode "screen-gradient")
         (replace-binding 'bent '(probe-screen-relief-normal normal world strength height)))
        ((equal mode "control"))
        (t (error "Unknown fragment probe ~S" mode))))
    (luv.shader:parse-shader-specification (second source) (third source) body)))

(setf (symbol-function 'star-fragment-specification) #'probe-fragment-specification)
(in-package #:cl-user)
