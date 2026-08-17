;;;; The portal server: a Unix socket where child luvcrafts come knocking.
;;;;
;;;; A playing game listens on a socket and tells every shell it opens on a
;;;; terminal wall where that socket is (LUVCRAFT_PARENT_SOCKET) and which wall
;;;; the shell is on (LUVCRAFT_PARENT_SCREEN).  Any luvcraft started in such a
;;;; shell notices the variables, connects instead of opening a window, says
;;;; hello naming its wall, and the game here answers with a ring of surfaces
;;;; and puts the child's picture on that very wall.  Since the child is a whole
;;;; game with walls of its own, it serves a socket too, and the shells on its
;;;; walls can start grandchildren.

(in-package #:luvcraft)

(defclass luvcraft-portal-server ()
  ((session :initarg :session :reader portal-server-session)
   (path :initarg :path :reader portal-server-path)
   (socket :initarg :socket :accessor portal-server-socket)
   (acceptor :initform nil :accessor portal-server-acceptor)
   (running-p :initform t :accessor portal-server-running-p)
   ;; Screen name -> function of a ready mirror which places it.
   (screens :initform (make-hash-table :test #'equal)
            :reader portal-server-screens)
   (portals :initform nil :accessor portal-server-portals))
  (:documentation "A listening Unix socket and the walls children may ask for."))

(defvar *portal-servers* (make-hash-table :test #'eq)
  "Session -> its portal server, for the game that is playing here.")

(defun luvcraft-portal-server (session)
  (gethash session *portal-servers*))

(defun luvcraft-portal-socket-path ()
  (namestring
   (merge-pathnames (format nil "luvcraft-~D.sock" (sb-posix:getpid))
                    (uiop:temporary-directory))))

(defun ensure-luvcraft-portal-server (session)
  "SESSION's portal server, started on first request."
  (or (luvcraft-portal-server session)
      (let* ((path (luvcraft-portal-socket-path))
             (socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
        (when (probe-file path) (delete-file path))
        (sb-bsd-sockets:socket-bind socket path)
        (sb-bsd-sockets:socket-listen socket 8)
        (let ((server (make-instance 'luvcraft-portal-server
                                     :session session :path path :socket socket)))
          (setf (portal-server-acceptor server)
                (sb-thread:make-thread
                 (lambda () (accept-luvcraft-portal-children server))
                 :name "luvcraft portal server"))
          (setf (gethash session *portal-servers*) server)))))

(defun stop-luvcraft-portal-server (session)
  (alexandria:when-let ((server (luvcraft-portal-server session)))
    (setf (portal-server-running-p server) nil)
    (ignore-errors (sb-bsd-sockets:socket-close (portal-server-socket server)))
    (ignore-errors (delete-file (portal-server-path server)))
    (dolist (portal (portal-server-portals server))
      (ignore-errors (close-luvcraft-portal session portal)))
    (setf (portal-server-portals server) nil)
    (remhash session *portal-servers*)
    (values)))

(defun luvcraft-portal-environment (session screen environment)
  "ENVIRONMENT with the variables that make a luvcraft started under it a
child of SESSION appearing on SCREEN."
  (let ((server (ensure-luvcraft-portal-server session)))
    (list* (format nil "LUVCRAFT_PARENT_SOCKET=~A" (portal-server-path server))
           (format nil "LUVCRAFT_PARENT_SCREEN=~A" screen)
           (remove-if (lambda (entry)
                        (or (uiop:string-prefix-p "LUVCRAFT_PARENT_SOCKET=" entry)
                            (uiop:string-prefix-p "LUVCRAFT_PARENT_SCREEN=" entry)))
                      environment))))

(defun register-luvcraft-portal-screen (session screen placer)
  "When a child asks for SCREEN, call PLACER with its ready mirror.  PLACER
returns the portal it opened, or NIL to decline."
  (setf (gethash screen (portal-server-screens (ensure-luvcraft-portal-server session)))
        placer))

(defun unregister-luvcraft-portal-screen (session screen)
  (alexandria:when-let ((server (luvcraft-portal-server session)))
    (remhash screen (portal-server-screens server))))

(defun accept-luvcraft-portal-children (server)
  (loop while (portal-server-running-p server)
        do (let ((client (handler-case
                             (sb-bsd-sockets:socket-accept (portal-server-socket server))
                           (error () nil))))
             (cond
               ((not (portal-server-running-p server))
                (when client (ignore-errors (sb-bsd-sockets:socket-close client))))
               (client
                (sb-thread:make-thread
                 (lambda () (welcome-luvcraft-portal-child server client))
                 :name "luvcraft portal child"))))))

(defun welcome-luvcraft-portal-child (server client)
  "Negotiate with one connected child and put it where it asked to be."
  (let* ((stream (sb-bsd-sockets:socket-make-stream
                  client :input t :output t :element-type 'character
                  :buffering :line :external-format :utf-8))
         (mirror (make-instance 'luvcraft-mirror :stream stream)))
    (handler-case
        (progn
          (negotiate-luvcraft-mirror mirror)
          (let* ((screen (luvcraft-mirror-screen mirror))
                 (placer (or (gethash screen (portal-server-screens server))
                             (gethash "-" (portal-server-screens server))))
                 (portal (and placer (funcall placer mirror))))
            (if portal
                (push portal (portal-server-portals server))
                (progn
                  (warn "No wall named ~S for a child luvcraft; sending it away." screen)
                  (stop-luvcraft-mirror mirror)))))
      (error (condition)
        (warn "A child luvcraft did not make it onto a wall: ~A" condition)
        (ignore-errors (stop-luvcraft-mirror mirror))))))

(defun forget-luvcraft-portal (session portal)
  (alexandria:when-let ((server (luvcraft-portal-server session)))
    (setf (portal-server-portals server)
          (delete portal (portal-server-portals server)))))

;;; The child end: LUVCRAFT_PARENT_SOCKET in the environment.

(defun luvcraft-parent-socket-path ()
  (let ((path (uiop:getenv "LUVCRAFT_PARENT_SOCKET")))
    (and path (plusp (length path)) path)))

(defun serve-luvcraft-parent (&key (path (luvcraft-parent-socket-path))
                                   (screen (or (uiop:getenv "LUVCRAFT_PARENT_SCREEN") "-")))
  "Connect to the parent game at PATH and be a mirror child on SCREEN."
  (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (sb-bsd-sockets:socket-connect socket path)
    (let ((stream (sb-bsd-sockets:socket-make-stream
                   socket :input t :output t :element-type 'character
                   :buffering :line :external-format :utf-8)))
      (unwind-protect
           (serve-luvcraft-mirror :input stream :output stream :screen screen)
        (ignore-errors (close stream))
        (ignore-errors (sb-bsd-sockets:socket-close socket))))))
