(in-package #:luft.world)

;;; The atelier's palette: the material kinds its worlds are made of and the
;;; placements an author chooses among.  The terrain defines what a material
;;; means; this file decides which ones exist.

(defvar *earth-material* nil)
(defvar *limestone-material* nil)
(defvar *highland-rock-material* nil)
(defvar *crystal-material* nil)
(defvar *terrain-material-placement* nil)
(defvar *highland-rock-material-placement* nil)
(defvar *sanctuary-material-placement* nil)
(defvar *beacon-material-placement* nil)
(defvar *crystal-material-placement* nil)
(defvar *snow-material* nil)
(defvar *slate-material* nil)
(defvar *sand-material* nil)
(defvar *snow-material-placement* nil)
(defvar *slate-material-placement* nil)
(defvar *sand-material-placement* nil)
(defvar *terminal-material* nil)
(defvar *terminal-material-placement* nil)

(setf *earth-material*
      (ensure-semantic-instance
       *earth-material* 'material-kind
       :name :earth :base-tone '(0.42 0.32 0.21) :relief :grass :relief-strength 0.8 :edge-wear 0.5
       :top-tone '(0.18 0.31 0.105)
       :side-tone '(0.42 0.32 0.21)
       :bottom-tone '(0.24 0.18 0.13))
      *limestone-material*
      (ensure-semantic-instance
       *limestone-material* 'material-kind
       :name :limestone :base-tone '(0.53 0.49 0.39) :relief :stone :relief-strength 0.6 :edge-wear 0.7)
      *highland-rock-material*
      (ensure-semantic-instance
       *highland-rock-material* 'material-kind
       :name :highland-rock :base-tone '(0.29 0.30 0.27) :relief :stone :relief-strength 0.9 :edge-wear 0.7)
      *crystal-material*
      (ensure-semantic-instance
       *crystal-material* 'material-kind
       :name :aether-crystal :base-tone '(0.16 0.68 0.94) :relief :gloss :relief-strength 0.15 :roughness 0.15
       :light-opacity 1 :light-emission '(3 11 15)
       :surface-emission 0.30)
      *terrain-material-placement*
      (ensure-semantic-instance
       *terrain-material-placement* 'material-placement
       :name :terrain :kind *earth-material*)
      *highland-rock-material-placement*
      (ensure-semantic-instance
       *highland-rock-material-placement* 'material-placement
       :name :highland-rock :kind *highland-rock-material*)
      *sanctuary-material-placement*
      (ensure-semantic-instance
       *sanctuary-material-placement* 'material-placement
       :name :sanctuary-limestone :kind *limestone-material*)
      *beacon-material-placement*
      (ensure-semantic-instance
       *beacon-material-placement* 'material-placement
       :name :ridge-beacon-limestone :kind *limestone-material*)
      *crystal-material-placement*
      (ensure-semantic-instance
       *crystal-material-placement* 'material-placement
       :name :aether-crystal :kind *crystal-material*)
      ;; The alpine relief's surfaces: snow lies on gentle ground above the
      ;; snow line over rock, slate faces the cliffs, sand the lowest shores.
      *snow-material*
      (ensure-semantic-instance
       *snow-material* 'material-kind
       :name :snow :base-tone '(0.80 0.83 0.86) :relief :snow
       :top-tone '(0.86 0.89 0.92)
       :side-tone '(0.62 0.65 0.68))
      *slate-material*
      (ensure-semantic-instance
       *slate-material* 'material-kind
       :name :slate :base-tone '(0.17 0.19 0.21) :relief :stone :edge-wear 0.7)
      *sand-material*
      (ensure-semantic-instance
       *sand-material* 'material-kind
       :name :sand :base-tone '(0.66 0.57 0.36) :relief :grain)
      *snow-material-placement*
      (ensure-semantic-instance
       *snow-material-placement* 'material-placement
       :name :snow :kind *snow-material*)
      *slate-material-placement*
      (ensure-semantic-instance
       *slate-material-placement* 'material-placement
       :name :slate :kind *slate-material*)
      *sand-material-placement*
      (ensure-semantic-instance
       *sand-material-placement* 'material-placement
       :name :sand :kind *sand-material*)
      ;; A graphite slab: adjacent terminal cells with one exposed face
      ;; become a single shell screen (see terminal.lisp).
      *terminal-material*
      (ensure-semantic-instance
       *terminal-material* 'material-kind
       :name :terminal :base-tone '(0.045 0.055 0.06) :relief :gloss :roughness 0.3)
      *terminal-material-placement*
      (ensure-semantic-instance
       *terminal-material-placement* 'material-placement
       :name :terminal :kind *terminal-material*))

(defun make-scene-material-vocabulary ()
  "Return the authored placement vocabulary shared by one scene's cells."
  (domains:make-identity-vocabulary-domain
   :members (list *terrain-material-placement*
                  *highland-rock-material-placement*
                  *sanctuary-material-placement*
                  *crystal-material-placement*
                  ;; Appended, so the four older placements keep their codes.
                  *snow-material-placement*
                  *slate-material-placement*
                  *sand-material-placement*
                  *terminal-material-placement*)
   :limit #xff))

(defvar *torch-flame-material* nil)

;;; A torch is an attachment made of flame.
(setf *torch-flame-material*
      (ensure-semantic-instance
       *torch-flame-material* 'material-kind
       :name :torch-flame :base-tone '(1.0 0.36 0.055)
       :light-emission '(15 9 3) :surface-emission 1.8))

(defun scene-builder-torch (builder x y z axis side &key (u 0.0) (v 0.0))
  "Attach a torch to support cell X/Y/Z's outward AXIS/SIDE face."
  (scene-builder-attach builder x y z axis side *torch-flame-material*
                        :u u :v v))

(defun make-world-scene-builder (&rest initargs &key &allow-other-keys)
  "Make a scene builder over this palette: earth by default, limestone for
architecture."
  (apply #'make-scene-builder
         :material-vocabulary (make-scene-material-vocabulary)
         :ground-material *terrain-material-placement*
         :architecture-material *sanctuary-material-placement*
         initargs))
