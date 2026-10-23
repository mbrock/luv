;;; Luvcraft's domain adapter for the engine frontier language.
(in-package #:luvcraft.frontier)

(defmethod luv.frontier:frontier-declaration-maximum-value
    ((declaration luvcraft.world.fields:voxel-field-definition))
  (let ((legal (luvcraft.world.fields:voxel-field-definition-legal-value-type
                declaration)))
    (and (consp legal) (eq (first legal) 'integer)
         (integerp (third legal)) (third legal))))

(defun compile-frontier-program (program &rest arguments)
  (apply #'luv.frontier:compile-frontier-program program
         :neighbor-operator 'do-chunk-site-neighbors
         :face-directions '*voxel-face-directions* arguments))

(defmacro do-voxel-frontier-relations
    ((source source-offset priority
      target target-offset direction destination crossing availability
      frontier window domain directions
      &key execution result)
     &body body)
  "Luvcraft's chunk-window realization of spatial frontier relations."
  (let ((present-p (gensym "PRESENT-P"))
        (local (gensym "LOCAL"))
        (resolved (gensym "RESOLVED"))
        (execution-value (gensym "EXECUTION")))
    `(let ((,execution-value ,execution))
       (loop until (bucket-frontier-empty-p ,frontier)
             do (multiple-value-bind
                    (,source ,source-offset ,priority ,present-p)
                    (bucket-frontier-pop ,frontier)
                  (declare (ignore ,present-p))
                  (let ((,local
                          (chunk-domain-local-coordinate
                           ,domain ,source-offset)))
                    (declare (dynamic-extent ,local))
                    (when ,execution-value
                      (incf (frontier-execution-visits ,execution-value)))
                    (do-chunk-window-neighbors
                        (,target-offset ,destination ,crossing ,direction
                         ,resolved ,availability
                         ,window ,domain ,local ,directions)
                      (when ,execution-value
                        (incf (frontier-execution-relations ,execution-value))
                        (when ,crossing
                          (incf (frontier-execution-crossings ,execution-value)))
                        (when (eq ,availability :unavailable)
                          (incf
                           (frontier-execution-unavailable ,execution-value))))
                      (let ((,target
                              (ecase ,availability
                                (:local ,source)
                                (:available ,resolved)
                                (:unavailable nil))))
                        ,@body))))
             finally (return ,result)))))
