(in-package #:luvcraft)

(defvar *session* nil
  "The live game session in this Lisp, or NIL.")

(defvar *checkpoint-writer* nil
  "The checkpoint writer for the world PLAY opened, or NIL.")

(defun default-luvcraft-world-pathname ()
  "Return the pathname of the ordinary persistent luvcraft world."
  (merge-pathnames
   #P"luvcraft/worlds/default.sexp"
   (let ((data-home (uiop:getenv "XDG_DATA_HOME")))
     (if data-home
         (uiop:ensure-directory-pathname (pathname data-home))
         (merge-pathnames #P".local/share/" (user-homedir-pathname))))))

(defun load-or-make-luvcraft-world (pathname)
  "Read the persistent world at PATHNAME, or make a new little world."
  (if (probe-file pathname)
      (progn
        (format t "Loading luvcraft world from ~A~%" pathname)
        (read-luvcraft-save pathname))
      (progn
        (format t "Creating luvcraft world at ~A~%" pathname)
        (values (make-empty-little-block-world) nil))))

(defvar *video-pathname* nil
  "A video file to play on a screen hanging in front of the player, or NIL.

PLAY reads this so a live image can hang a film in the world without changing
any call site: set it, STOP-PLAYING, and PLAY again.")

(defun play (&key (provider luv:*gpu-provider*) tracy-p fullscreen-p
                  (video-pathname *video-pathname*)
                  (world-pathname (default-luvcraft-world-pathname)))
  "Open the ordinary persistent game in this Lisp and return the session.

Load or create WORLD-PATHNAME, restore its saved camera and player, start
checkpointing, and open the visible game window, on the whole display when
FULLSCREEN-P.  *SESSION* names the live
game for later SLY evaluations.  STOP-PLAYING checkpoints and closes it."
  (when *session*
    (error "A game is already playing; call STOP-PLAYING first."))
  (when tracy-p
    (start-luvcraft-tracy))
  (multiple-value-bind (world resume-description)
      (load-or-make-luvcraft-world world-pathname)
    (multiple-value-bind (camera player selected-block carried)
        (restore-luvcraft-resume-save-description resume-description)
      (let ((writer (make-world-checkpoint-writer world-pathname))
            ;; A first visit stands on the generated ground rather than at
            ;; the camera's fixed height, which alpine terrain may bury.
            (player (or player (little-world-spawn-player world camera)))
            (session nil))
        (unwind-protect
             (setf session
                   (start-luvcraft
                    :provider provider
                    :title "luvcraft — walk, jump, mine, and build"
                    :world world :camera camera :player player
                    :selected-block selected-block
                    :fullscreen-p fullscreen-p
                    :video-pathname video-pathname
                    :quit-function
                    (lambda (playing-session)
                      ;; REQUEST-LUVCRAFT-QUIT already granted this worker sole
                      ;; ownership; do not recursively enter STOP-LUVCRAFT.
                      (let ((playing-writer writer))
                        (unwind-protect-releasing
                            (with-release-report
                              (releasing :checkpoint
                                (request-luvcraft-session-checkpoint
                                 playing-session))
                              (releasing :session
                                (perform-luvcraft-stop playing-session)))
                          (releasing :playing-session-publication
                            (when (eq playing-session *session*)
                              (setf *session* nil
                                    *checkpoint-writer* nil)))
                          (releasing :checkpoint-writer
                            (when playing-writer
                              (stop-world-checkpoint-writer
                               playing-writer))))))
                    :checkpoint-writer writer))
          (unless session
            (stop-world-checkpoint-writer writer)))
        ;; The films the player was carrying come back into the bag, and the
        ;; ones standing beside walls light them again.
        (dolist (block carried)
          (add-block-to-inventory (luvcraft-session-inventory session) block 1))
        (relight-world-films session)
        (setf *checkpoint-writer* writer
              *session* session)))))

(defun stop-playing ()
  "Checkpoint and close the game PLAY opened, releasing its resources.

*SESSION* is cleared whether or not the release succeeds, so a failed teardown
cannot leave the image believing a game is still playing.  The failure itself
is not swallowed: STOP-LUVCRAFT closes the window first and then signals what
went wrong, and that condition comes out of here."
  (let ((session *session*)
        (writer *checkpoint-writer*))
    (unwind-protect-releasing
         (when session
           (with-release-report
             (releasing :checkpoint
               (request-luvcraft-session-checkpoint session))
             (releasing :session (stop-luvcraft session))))
      (releasing :playing-session-publication
        (setf *session* nil
              *checkpoint-writer* nil))
      (releasing :checkpoint-writer
        (when writer
          (stop-world-checkpoint-writer writer))))
    (values)))

(defun shader-knob-p (knob)
  "Whether KNOB has been folded into a live pipeline's source, so that
turning it rebuilds one.  Answered for the live session."
  (and (boundp '*session*) *session*
       (some (lambda (pipeline)
               (assoc (knob-name knob)
                      (live-shader-pipeline-attempted-source-values pipeline)))
             (luvcraft-session-live-shader-pipelines *session*))))

;;; ---------------------------------------------------------------------
;;; The verbs the metabar offers.

(define-action focus-target (:label "focus what the crosshair is on")
  ;; Whatever gadget offered this button is itself the focus at the moment
  ;; it is pressed; it must stand down before the world can be looked at.
  (unfocus-luvcraft-session session)
  (toggle-luvcraft-session-focus session))

(define-action quit (:label "quit the game")
  (unless (request-luvcraft-quit session)
    (close-canvas (luvcraft-session-canvas session))))
