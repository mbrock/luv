(in-package #:mcluv.tests)

(clim:define-command-table command-menu-test-verbs)
(clim:define-command-table command-menu-test-more-verbs
  :inherit-from (command-menu-test-verbs))

(defvar *command-menu-test-execution* nil)

(clim:define-application-frame command-menu-test-owner ()
  ()
  (:command-table
   (command-menu-test-owner
    :inherit-from (command-menu-test-verbs command-menu-test-more-verbs))))

(clim:define-command (com-command-menu-alpha
                      :command-table command-menu-test-verbs
                      :name "Alpha Command")
    ()
  (push :alpha *command-menu-test-execution*))

(clim:define-command (com-command-menu-beta
                      :command-table command-menu-test-more-verbs
                      :name "Beta Operation")
    ()
  (push :beta *command-menu-test-execution*))

(clim:define-command (com-command-menu-needs-value
                      :command-table command-menu-test-verbs
                      :name "Needs Value")
    ((value 'integer :prompt "value"))
  (push value *command-menu-test-execution*))

(defun make-command-menu-test-owner ()
  (clim:make-application-frame 'command-menu-test-owner))

(defun make-command-menu-test-frame (&optional (owner (make-command-menu-test-owner)))
  (clim:make-application-frame
   'mcluv:command-menu
   :owner-frame owner
   :command-tables
   '(command-menu-test-verbs command-menu-test-more-verbs)))

(defun command-menu-test-key (key-name &key character modifiers repeat-p)
  (make-instance 'luv:canvas-key-press-event
                 :timestamp 0
                 :key-name key-name
                 :character character
                 :unshifted-character character
                 :modifiers modifiers
                 :repeat-p repeat-p))

(define-test m-x-reads-one-executable-application-vocabulary
  (let* ((owner (make-command-menu-test-owner))
         (entries
           (mcluv:command-menu-entries-for-tables
            '(command-menu-test-verbs command-menu-test-more-verbs)
            :owner-frame owner)))
    (true (equal '("Alpha Command" "Beta Operation")
                 (mapcar #'mcluv:command-menu-entry-label entries)))
    ;; The inherited Alpha command is encountered twice but remains one verb.
    (true (= 1 (count 'com-command-menu-alpha entries
                      :key #'mcluv:command-menu-entry-command-name)))
    ;; A row which cannot yet prompt for its required argument is not a dead
    ;; promise in the visible command vocabulary.
    (true (null (find "Needs Value" entries :test #'string=
                      :key #'mcluv:command-menu-entry-label)))
    (true (equal '("Beta Operation")
                 (mapcar
                  #'mcluv:command-menu-entry-label
                  (mcluv:matching-command-menu-entries entries "operation be"))))
    (setf (clim:command-enabled 'com-command-menu-beta owner) nil)
    (true (equal '("Alpha Command")
                 (mapcar
                  #'mcluv:command-menu-entry-label
                  (mcluv:command-menu-entries-for-tables
                   '(command-menu-test-verbs command-menu-test-more-verbs)
                   :owner-frame owner))))))

(define-test m-x-filters-repaints-and-then-executes-outside-paint
  (let ((frame (make-command-menu-test-frame))
        (*command-menu-test-execution* nil))
    (setf (mcluv::command-menu-dirty-p frame) nil)
    (mcluv:refresh-command-menu-entries frame)
    (true (mcluv::command-menu-dirty-p frame))
    (dolist (character '(#\b #\e))
      (multiple-value-bind (action command)
          (mcluv:handle-command-menu-key-event
           frame (command-menu-test-key :text :character character))
        (true (eq :continue action))
        (true (null command))))
    (true (string= "be" (mcluv:command-menu-query frame)))
    (true (equal '("Beta Operation")
                 (mapcar
                  #'mcluv:command-menu-entry-label
                  (mcluv:command-menu-results frame))))
    ;; Modified characters are application gestures, not finder text.
    (mcluv:handle-command-menu-key-event
     frame (command-menu-test-key :x :character #\x :modifiers '(:meta)))
    (true (string= "be" (mcluv:command-menu-query frame)))
    (multiple-value-bind (action command)
        (mcluv:handle-command-menu-key-event
         frame (command-menu-test-key :return))
      (true (eq :execute action))
      (true (equal '(com-command-menu-beta) command))
      (mcluv:execute-command-menu-command
       frame command
       :before-execute
       (lambda () (push :dismissed *command-menu-test-execution*))))
    (true (equal '(:beta :dismissed) *command-menu-test-execution*))
    (multiple-value-bind (action command)
        (mcluv:handle-command-menu-key-event
         frame (command-menu-test-key :escape))
      (true (eq :dismiss action))
      (true (null command)))))

(define-test static-m-x-prepares-live-shader-revisions
  (let ((frame (make-command-menu-test-frame)))
    (setf (mcluv::command-menu-dirty-p frame) nil)
    (multiple-value-bind (probe revision)
        (mount-static-direct-preparation-probe frame)
      (mcluv:prepare-command-menu frame)
      (true (equal (list revision)
                   (current-revision-preparation-probe-revisions probe))))))

(define-test m-x-layout-is-one-logical-pixel-at-the-native-destination
  (let* ((logical '(1344 840))
         (drawable '(2688 1680))
         (state (mcluv:command-menu-screen-state nil logical))
         (half-width (aref state 4))
         (half-height (aref state 9)))
    ;; Clip-space half extents times the logical viewport recover the authored
    ;; dimensions exactly: no pane raster is resampled to get there.
    (true (< (abs (- 620.0 (* half-width (first logical)))) 1.0e-4))
    (true (< (abs (- 420.0 (* half-height (second logical)))) 1.0e-4))
    ;; A 2x drawable supplies two native samples for every logical coordinate.
    (true (< (abs (- 1240.0 (* half-width (first drawable)))) 1.0e-4))
    (true (< (abs (- 840.0 (* half-height (second drawable)))) 1.0e-4))
    (multiple-value-bind (x y)
        (mcluv:command-menu-local-coordinate
         nil (/ (first logical) 2) (/ (second logical) 2) logical)
      (true (< (abs (- 310.0 x)) 1.0e-4))
      (true (< (abs (- 210.0 y)) 1.0e-4)))))

(define-test m-x-analytic-ink-is-translucent-and-prepares-no-image-command
  (let ((medium (fresh-gpu-medium)))
    (setf (clim:medium-ink medium) mcluv::*command-menu-panel-ink*)
    (mcluv::medium-draw-analytic-rounded-rectangle*
     medium 0 0 620 420 18 t)
    (let ((vertices (mcluv::gpu-medium-analytic-vertices medium))
          (semantic-commands (mcluv::gpu-medium-commands medium)))
      (true (< (abs (- 0.92 (aref vertices 2))) 1.0e-5))
      (true (= 1 (length semantic-commands)))
      (true (typep (aref semantic-commands 0) 'mcluv::gpu-analytic-command))
      (true (null (mcluv:gpu-medium-fallback-report medium)))
      (let* ((sheet
               (make-instance 'mcluv::mx-command-menu-pane
                              :region
                              (clim:make-bounding-rectangle 0 0 620 420)))
             (mirror
               (make-instance 'mcluv:luv-gpu-mirror
                              :sheet sheet :target nil :embedded-p t)))
        (multiple-value-bind (prepared text-data)
            (mcluv::prepare-gpu-frame-commands mirror semantic-commands)
          (declare (ignore text-data))
          (true (= 1 (length prepared)))
          (true (typep (first prepared) 'mcluv::gpu-analytic-command))
          (true (null (find-if
                       (lambda (command)
                         (typep command 'mcluv::gpu-prepared-image-command))
                       prepared))))))))
