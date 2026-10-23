;;;; A second game on the terrain alone: its own palette, its own world
;;;; source, and its own mesh preparer, with no renderer and no Luft world.

(defpackage #:luft.terrain.tests
  (:use #:cl)
  (:import-from #:parachute #:define-test #:true #:is)
  (:local-nicknames (#:terrain #:luft.terrain)
                    (#:domains #:luv.domains)
                    (#:production #:luv.production)))

(in-package #:luft.terrain.tests)

(defparameter *stone*
  (make-instance 'terrain:material-placement
                 :name :stone
                 :kind (make-instance 'terrain:material-kind
                                      :name :stone :base-tone '(0.4 0.4 0.4))))

(defparameter *glow*
  (make-instance 'terrain:material-placement
                 :name :glow
                 :kind (make-instance 'terrain:material-kind
                                      :name :glow :base-tone '(0.9 0.8 0.2)
                                      :light-opacity 1
                                      :light-emission '(15 12 4))))

(defun make-palette ()
  (domains:make-identity-vocabulary-domain :members (list *stone* *glow*)
                                           :limit #xff))

(defun make-builder ()
  (terrain:make-scene-builder :material-vocabulary (make-palette)
                              :ground-material *stone*
                              :architecture-material *glow*))

(define-test a-game-palette-paints-a-built-scene
  (let ((builder (make-builder)))
    (terrain:scene-builder-cell builder 3 3 0)
    (terrain:scene-builder-cell builder 4 3 0 :architecture-p t)
    (terrain:scene-builder-cell builder 5 3 0 :material *glow*)
    (let* ((scene (terrain:finish-scene-builder builder))
           (domain (terrain:scene-domain scene)))
      (flet ((at (x)
               (terrain::scene-material-placement-at
                scene (luft:make-site domain x 3 0 luft:+cell-extent+ 1))))
        (is eq *stone* (at 3))
        (is eq *glow* (at 4))
        (is eq *glow* (at 5))
        (is eq nil (at 6)))
      ;; Both glowing cells, and only they, seed authored light.
      (is = 2 (length (terrain:scene-authored-light-sources scene))))))

(defclass flat-source (terrain:world-source) ()
  (:documentation "Two layers of stone under the whole domain."))

(defmethod terrain:generate-world-source-chunk ((source flat-source) key place)
  (loop for x from (luft:chunk-origin-x key)
          below (+ (luft:chunk-origin-x key) luft:+chunk-size+)
        do (loop for y from (luft:chunk-origin-y key)
                   below (+ (luft:chunk-origin-y key) luft:+chunk-size+)
                 do (funcall place x y 0 *stone*)
                    (funcall place x y 1 *stone*))))

(define-test a-world-source-replays-its-edits-over-generated-cells
  (let* ((domain (luft:make-world-domain :x-bits 7 :y-bits 7))
         (source (make-instance 'flat-source :domain domain
                                             :material-vocabulary (make-palette)))
         (key (luft:chunk-key-at 64 0))
         (removed (luft:make-site domain 70 5 1 luft:+cell-extent+ 1))
         (added (luft:make-site domain 70 5 2 luft:+cell-extent+ 1))
         (elsewhere (luft:make-site domain 5 5 2 luft:+cell-extent+ 1)))
    (setf (gethash removed (terrain:world-source-edits source)) nil
          (gethash added (terrain:world-source-edits source)) *glow*
          (gethash elsewhere (terrain:world-source-edits source)) *glow*)
    (let* ((edits (terrain:capture-world-source-chunk-edits source key))
           (chunk (terrain:materialize-world-source-chunk
                   source key 1 :edits edits))
           (fibers (terrain::resident-cell-chunk-fibers chunk))
           (materials (terrain::resident-cell-chunk-material-cells chunk)))
      ;; The edit in another chunk is not captured for this one.
      (is = 2 (length edits))
      (is = 1 (luft:fibers-cell-bit fibers 70 6 1))
      (is = 0 (luft:fibers-cell-bit fibers 70 5 1))
      (is = 1 (luft:fibers-cell-bit fibers 70 5 2))
      (is = 0 (luft:fibers-cell-bit fibers 70 6 2))
      (is = 1 (gethash added materials))
      (is eq nil (nth-value 1 (gethash removed materials)))
      ;; 64 x 64 columns of two cells, one removed and one added.
      (is = (* 64 64 2) (hash-table-count materials)))))

(defclass counting-preparer ()
  ((calls :initform nil :accessor preparer-calls))
  (:documentation "Records each realization and returns a tagged product."))

(defmethod terrain:prepare-streamed-mesh
    ((preparer counting-preparer) mesh previous)
  (let ((product (list :prepared mesh)))
    (push (list product previous) (preparer-calls preparer))
    product))

(defmethod terrain:streamed-mesh-preparation-signature
    ((preparer counting-preparer))
  (list :counting-preparer))

(defun stream-once (scene system)
  "Mesh chunk 0 on SYSTEM's worker and install the result as a renderer would."
  (terrain:schedule-streaming-scene-cohort scene system '(0) 1 0)
  (loop for (result present-p)
          = (multiple-value-list
             (production:receive-production-result-no-hang system))
        until present-p do (sleep 0.005)
        finally (terrain::receive-streaming-mesh-result scene result))
  (multiple-value-bind (meshes generation ready-p)
      (terrain::ready-streaming-scene-meshes scene)
    (declare (ignore generation))
    (setf (terrain:streaming-scene-mesh-cache scene)
          (terrain:streaming-mesh-result-mesh-cache
           (terrain::streaming-replacement-result
            (terrain:streaming-scene-replacement scene)))
          (terrain:streaming-scene-replacement scene) nil)
    (values meshes ready-p)))

(defun call-with-streamed-scene (function &optional mesh-preparer)
  (let* ((builder (make-builder)))
    (terrain:scene-builder-cell builder 10 10 2)
    (terrain:scene-builder-cell builder 11 10 2)
    (let ((scene (terrain:make-streaming-scene
                  (terrain:finish-scene-builder builder)
                  :mesh-preparer mesh-preparer))
          (system (production:make-single-worker-production-system
                   :name "terrain test")))
      (setf (gethash 0 (terrain:streaming-scene-loaded scene)) 1)
      (unwind-protect (funcall function scene system)
        (production:stop-production-system system)))))

(define-test streaming-without-a-preparer-yields-meshes
  (call-with-streamed-scene
   (lambda (scene system)
     (multiple-value-bind (meshes ready-p) (stream-once scene system)
       (true ready-p)
       (is equal '(0) (mapcar #'car meshes))
       (true (typep (cdr (first meshes)) 'luft:surface-mesh))
       ;; Two cells: a closed surface with active star sites.
       (true (plusp (length (luft:surface-mesh-star-site-words
                             (cdr (first meshes))))))))))

(define-test a-preparer-reuses-its-previous-realization
  (let ((preparer (make-instance 'counting-preparer)))
    (call-with-streamed-scene
     (lambda (scene system)
       (let* ((first (cdr (first (stream-once scene system))))
              (second (cdr (first (stream-once scene system))))
              (calls (reverse (preparer-calls preparer))))
         (is = 2 (length calls))
         ;; The worker publishes exactly what the preparer returned.
         (is eq (first (first calls)) first)
         (is eq (first (second calls)) second)
         ;; Cold, there is nothing to reuse; warm, the cached realization
         ;; of the unchanged chunk is offered back with its star sites.
         (is eq nil (second (first calls)))
         (is eq first (second (second calls)))
         (is eq (luft:surface-mesh-star-site-words (second first))
             (luft:surface-mesh-star-site-words (second second)))))
     preparer)))
