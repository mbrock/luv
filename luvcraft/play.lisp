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

(defun play (&key (provider luv:*gpu-provider*) tracy-p
                  (world-pathname (default-luvcraft-world-pathname)))
  "Open the ordinary persistent game in this Lisp and return the session.

Load or create WORLD-PATHNAME, restore its saved camera and player, start
checkpointing, and open the visible game window.  *SESSION* names the live
game for later SLY evaluations.  STOP-PLAYING checkpoints and closes it."
  (when *session*
    (error "A game is already playing; call STOP-PLAYING first."))
  (when tracy-p
    (start-luvcraft-tracy))
  (multiple-value-bind (world resume-description)
      (load-or-make-luvcraft-world world-pathname)
    (multiple-value-bind (camera player selected-block)
        (restore-luvcraft-resume-save-description resume-description)
      (let ((writer (make-world-checkpoint-writer world-pathname))
            (session nil))
        (unwind-protect
             (setf session
                   (start-luvcraft
                    :provider provider
                    :title "luvcraft — walk, jump, mine, and build"
                    :world world :camera camera :player player
                    :selected-block selected-block
                    :checkpoint-writer writer))
          (unless session
            (stop-world-checkpoint-writer writer)))
        (setf *checkpoint-writer* writer
              *session* session)))))

(defun stop-playing ()
  "Checkpoint and close the game PLAY opened, releasing its resources."
  (let ((session *session*)
        (writer *checkpoint-writer*))
    (setf *session* nil
          *checkpoint-writer* nil)
    (unwind-protect
         (when session
           (request-luvcraft-session-checkpoint session)
           (stop-luvcraft session))
      (when writer
        (stop-world-checkpoint-writer writer)))
    (values)))
