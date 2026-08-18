;;; A real urbit on a wall.
;;;
;;; The urbit material is not a picture of a computer.  Activating a wall of
;;; it attaches the actual vere runtime to the wall's PTY: the first
;;; activation boots a comet into a pier under build/urbit/ -- vere finds a
;;; sponsor over the network and takes its minutes, all of it visible on the
;;; wall -- and every later activation resumes the same pier, so the ship is
;;; as durable as the checkout.  The dojo is the wall.
;;;
;;; A comet is the ship that needs no keys.  Named ships -- a star like
;;; ~nodfur under the galaxies ~fur and ~lev -- boot from a keyfile instead,
;;; which is a later adventure; URBIT-BOOT-ARGUMENTS is where that choice
;;; will land when it arrives.

(in-package #:luvcraft)

(defparameter *urbit-pier-root* #P"build/urbit/"
  "Where piers live, relative to the checkout the game runs in.")

(defparameter *urbit-default-pier* "comet"
  "The pier a bare urbit wall runs.")

(defun find-program-in-path (name)
  "Return the full name of executable NAME on PATH, or NIL."
  (loop for directory in (uiop:split-string (or (uiop:getenv "PATH") "")
                                            :separator ":")
        for candidate = (unless (string= directory "")
                          (merge-pathnames
                           name (uiop:ensure-directory-pathname directory)))
        when (and candidate (uiop:file-exists-p candidate))
          return (namestring candidate)))

(defun urbit-executable ()
  "The vere runtime this environment provides, or NIL.

The nix dev shell names its own copy as LUV_URBIT; anywhere else, whatever
urbit or vere the PATH offers is the one that runs."
  (or (uiop:getenv "LUV_URBIT")
      (find-program-in-path "urbit")
      (find-program-in-path "vere")))

(defun urbit-pier-pathname (&optional (name *urbit-default-pier*))
  "The directory of the pier called NAME, under the checkout's pier root."
  (merge-pathnames
   (make-pathname :directory (list :relative name))
   (merge-pathnames *urbit-pier-root* (uiop:getcwd))))

(defun urbit-pier-exists-p (pier)
  "Whether PIER has been booted: a pier is a directory vere made an .urb in."
  (and (uiop:directory-exists-p (merge-pathnames #P".urb/" pier)) t))

(defun urbit-boot-arguments (pier)
  "The vere argument list that resumes PIER, or boots it as a comet first."
  (let ((namestring (namestring pier)))
    (if (urbit-pier-exists-p pier)
        (list namestring)
        (list "-c" namestring))))

(defun attach-terminal-display-urbit
    (display &key (pier-name *urbit-default-pier*))
  "Attach the real urbit runtime to DISPLAY: the dojo on the wall.

Booting and resuming both happen in the PTY where the player can watch;
vere checkpoints the pier when the display goes away and takes its process
with it."
  (let ((program (urbit-executable))
        (pier (urbit-pier-pathname pier-name)))
    (unless program
      (error "No urbit runtime: set LUV_URBIT or put urbit on PATH."))
    (ensure-directories-exist
     (merge-pathnames *urbit-pier-root* (uiop:getcwd)))
    (attach-terminal-display-pty
     display
     :program program
     :arguments (urbit-boot-arguments pier)
     :directory (uiop:getcwd)
     :environment (copy-list (sb-ext:posix-environ)))))

(defmethod activate-wall-material
    ((name (eql :urbit)) (session luvcraft-session) hit)
  (open-activated-wall-display
   session hit *urbit-block* #'attach-terminal-display-urbit))
