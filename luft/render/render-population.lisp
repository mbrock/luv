(in-package #:luft.render)

;;; The CPU form of a terrain population: active star sites and their parallel
;;; packed appearance.  A streaming scene given +RENDER-POPULATION-PREPARER+
;;; builds it on the worker beside each mesh, so the owner thread only stages
;;; buffers; GPU residency is renderer-residency.lisp.

(defun pack-terrain-appearance-codes (codes)
  "Pack eight u8 sample codes per star into the GPU's parallel uvec2 lane."
  (declare (type (simple-array (unsigned-byte 8) (*)) codes)
           (optimize (speed 3) (safety 1)))
  (unless (zerop (mod (length codes) 8))
    (error "Terrain appearance has ~D bytes, not eight per active star."
           (length codes)))
  (let ((words (make-array (/ (length codes) 4)
                           :element-type '(unsigned-byte 32))))
    (loop for offset fixnum from 0 below (length codes) by 4
          for word fixnum from 0
          do (setf (aref words word)
                   (logior (aref codes offset)
                           (ash (aref codes (+ offset 1)) 8)
                           (ash (aref codes (+ offset 2)) 16)
                           (ash (aref codes (+ offset 3)) 24))))
    words))

(defstruct (render-population
             (:constructor %make-render-population
                 (instance-words appearance-words descriptor-words
                  site-count))
             (:copier nil))
  "Geometry sites plus a one-for-one, independently replaceable appearance."
  (instance-words #() :type (simple-array (unsigned-byte 32) (*)) :read-only t)
  (appearance-words #() :type (simple-array (unsigned-byte 32) (*))
                    :read-only t)
  (descriptor-words #() :type (simple-array (unsigned-byte 32) (*))
                    :read-only t)
  (site-count 0 :type (integer 0 *) :read-only t))

(defstruct (prepared-render-mesh
             (:constructor %make-prepared-render-mesh (mesh population))
             (:copier nil))
  "Worker-transferable CPU realization of one semantic surface mesh."
  (mesh nil :type luft:surface-mesh :read-only t)
  (population nil :type render-population :read-only t))

(zdefun (prepare-render-mesh :zone :luft/prepare-population) (mesh)
  "Canonicalize one MESH before it crosses to the renderer owner."
  (check-type mesh luft:surface-mesh)
  (%make-prepared-render-mesh mesh (make-render-population (list mesh))))

(defun %make-star-render-population (meshes)
  "Flatten geometry and its parallel active-star appearance independently."
  (let* ((site-words
           (apply #'concatenate '(simple-array (unsigned-byte 32) (*))
                  (mapcar #'luft:surface-mesh-star-site-words meshes)))
         (appearance-codes
           (apply #'concatenate '(simple-array (unsigned-byte 8) (*))
                  (mapcar #'luft:surface-mesh-appearance-codes meshes)))
         (descriptor-words
           (or (loop for mesh in meshes
                     for words =
                       (luft:surface-mesh-appearance-descriptor-words mesh)
                     when (plusp (length words)) return words)
               (compile-terrain-material-descriptors
                (make-scene-material-vocabulary)))))
    (unless (= (* 2 (length site-words)) (length appearance-codes))
      (error "~D site words do not have a one-for-one eight-byte appearance (~D bytes)."
             (length site-words) (length appearance-codes)))
    (dolist (mesh meshes)
      (let ((words (luft:surface-mesh-appearance-descriptor-words mesh)))
        (unless (or (zerop (length words)) (equalp words descriptor-words))
          (error "One terrain population contains incompatible material palettes."))))
    (%make-render-population
     site-words (pack-terrain-appearance-codes appearance-codes)
     descriptor-words (/ (length site-words) 4))))

(zdefun (make-render-population :zone :luft/build-render-population
                                :value (length meshes))
    (meshes)
  "Prepare the sole terrain ABI: active sites indexing the fixed star atlas."
  (%make-star-render-population meshes))

(defclass render-population-preparer () ()
  (:documentation "The renderer's realization of streamed meshes."))

(defvar +render-population-preparer+ (make-instance 'render-population-preparer)
  "Pass as a streaming scene's :MESH-PREPARER to build populations on the worker.")

(defmethod prepare-streamed-mesh
    ((preparer render-population-preparer) mesh previous)
  (if previous
      (%make-prepared-render-mesh mesh (prepared-render-mesh-population previous))
      (prepare-render-mesh mesh)))

(defmethod streamed-mesh-preparation-signature
    ((preparer render-population-preparer))
  (list (symbol-function 'make-render-population)
        (symbol-function 'pack-terrain-appearance-codes)))
