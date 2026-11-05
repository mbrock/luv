;;;; Finding a game (#3JTHM3).
;;;;
;;;; A host keeps a retained entry in the tailnet lobby's store,
;;;; luv/store/luvcraft/sessions/ID, for as long as it hosts: the address it
;;;; reaches the lobby from, its port, the world's name and tag, and how many
;;;; play.  ID is its radio's client id, so an entry counts only while that
;;;; radio is present: a host that died without clearing its entry is not
;;;; listed.  The workbench lobby pane's Join list is these entries together
;;;; with the hosts heard on the LAN, and a click joins one.
;;;;
;;;; An entry is a few KEY=VALUE lines, read without the Lisp reader.
(in-package #:luvcraft.luft.game)

(defparameter *session-advert-prefix* "luvcraft/sessions/")

(defparameter *advert-period* 1
  "Seconds between a host's checks that its lobby entry is current.")

(defparameter *join-list-period* 1/2
  "Seconds a view's join list is kept before it is gathered again.")

(defparameter *advert-fields* '(:address :port :world :tag :players :host))

;;; Entries.

(defun session-world-name (session)
  "The name SESSION's world is announced by, or its host's world."
  (or (ignore-errors
       (net::socket-transport-announce (net::endpoint-transport (session-endpoint session))))
      (format nil "~A's world" (player-name (session-local-player session)))))

(defun session-advert (session address)
  "SESSION's lobby entry, reached at ADDRESS."
  (format nil "~{~(~A~)=~A~%~}"
          (list :address address
                :port (net:endpoint-port (session-endpoint session))
                :world (remove #\Newline (session-world-name session))
                :tag (tag-hex (net:endpoint-world-tag (session-endpoint session)))
                :players (length (world-players (session-world session)))
                :host (remove #\Newline (player-name (session-local-player session))))))

(defun parse-session-advert (text)
  "The plist a lobby entry TEXT describes, or NIL unless it names an address
and a port."
  (let ((fields nil))
    (with-input-from-string (stream text)
      (loop for line = (read-line stream nil)
            while line
            do (let* ((equals (position #\= line))
                      (field (and equals (find (subseq line 0 equals) *advert-fields*
                                               :test #'string-equal))))
                 (when (and field (not (getf fields field)))
                   (setf (getf fields field) (subseq line (1+ equals)))))))
    (let ((port (parse-integer (or (getf fields :port) "") :junk-allowed t))
          (players (parse-integer (or (getf fields :players) "") :junk-allowed t)))
      (and (plusp (length (getf fields :address)))
           port (< 0 port 65536)
           (list :address (getf fields :address) :port port
                 :world (or (getf fields :world) "a world")
                 :tag (or (getf fields :tag) "")
                 :players (or players 0)
                 :host (or (getf fields :host) ""))))))

;;; The host's entry.

(defvar *view-chores* (make-hash-table :test #'eq :weakness :key :synchronized t)
  "View -> (ADVERT-DUE JOIN-LIST-DUE JOIN-LIST): when each chore of a view
is next due, and its join list.")

(defun view-chores (viewer)
  (or (gethash viewer *view-chores*)
      (setf (gethash viewer *view-chores*) (list 0d0 0d0 nil))))

(defun advertise-viewer-session (viewer)
  "Keep VIEWER's world's lobby entry current: there while it hosts on the
network, and gone once it does not."
  (let ((client (render:viewer-lobby-client viewer))
        (chores (view-chores viewer))
        (now (wall-seconds)))
    (when (and client (>= now (first chores)))
      (setf (first chores) (+ now *advert-period*))
      (let ((session (world-session (viewer-world viewer)))
            (address (luv.lobby:lobby-client-local-address client)))
        (setf (luv.lobby:lobby-client-own-value
               client (concatenate 'string *session-advert-prefix*
                                   (luv.lobby:lobby-client-id client)))
              (and address (typep session 'host-session) (eq (session-state session) :open)
                   (net:endpoint-port (session-endpoint session))
                   (session-advert session address)))))))

;;; The join list.

(defvar *lan-discovery* nil
  "The listener for LAN beacons, started by the first join list; :UNAVAILABLE
when the beacon port cannot be had.")

(defun lan-hosts ()
  "The hosts heard on the LAN lately."
  (when (null *lan-discovery*)
    (setf *lan-discovery*
          (or (and (fboundp 'net:start-discovery) (ignore-errors (net:start-discovery)))
              :unavailable)))
  (unless (eq *lan-discovery* :unavailable)
    (ignore-errors (net:discovered-hosts *lan-discovery*))))

(defun stop-lan-discovery ()
  (let ((discovery *lan-discovery*))
    (setf *lan-discovery* nil)
    (when (and discovery (not (eq discovery :unavailable)))
      (ignore-errors (net:stop-discovery discovery)))))

(defun own-session-p (world port tag)
  "Whether PORT and TAG are the session WORLD hosts."
  (let ((session (world-session world)))
    (and (typep session 'host-session)
         (eql port (net:endpoint-port (session-endpoint session)))
         (string= tag (tag-hex (net:endpoint-world-tag (session-endpoint session)))))))

(defun players-detail (players where)
  (format nil "~D player~:P · ~A" players where))

(defun gather-join-entries (client world lan-hosts)
  "The sessions a player in WORLD can join: the entries in CLIENT's lobby
whose hosts are present, then LAN-HOSTS not already listed.  WORLD's own is
not one."
  (let* ((present (and client (mapcar #'luv.lobby:lobby-peer-id
                                      (luv.lobby:lobby-snapshot-peers
                                       (luv.lobby:lobby-client-snapshot client)))))
         (adverts
           (loop for (key . text) in (luv.lobby:lobby-client-values-under
                                      client *session-advert-prefix*)
                 for owner = (subseq key (length *session-advert-prefix*))
                 for advert = (and (member owner present :test #'string=)
                                   (parse-session-advert text))
                 when (and advert
                           (not (own-session-p world (getf advert :port) (getf advert :tag))))
                   collect advert))
         (lan (loop for host in lan-hosts
                    for tag = (tag-hex (net:discovered-host-world-tag host))
                    unless (own-session-p world (net:discovered-host-port host) tag)
                      collect host)))
    (flet ((same-p (advert host)
             (and (eql (getf advert :port) (net:discovered-host-port host))
                  (string= (getf advert :tag) (tag-hex (net:discovered-host-world-tag host))))))
      (append
       ;; A host heard on the LAN too is dialled there, closest.
       (loop for advert in adverts
             for near = (find-if (lambda (host) (same-p advert host)) lan)
             collect (luv.lobby.mcclim:make-lobby-join-entry
                      :title (getf advert :world)
                      :detail (players-detail (getf advert :players) (if near "LAN" "tailnet"))
                      :target (if near
                                  (net:discovered-host-target near)
                                  (format nil "~A:~D" (getf advert :address) (getf advert :port)))))
       (loop for host in lan
             unless (find-if (lambda (advert) (same-p advert host)) adverts)
               collect (luv.lobby.mcclim:make-lobby-join-entry
                        :title (net:discovered-host-world host)
                        :detail "LAN"
                        :target (net:discovered-host-target host)))))))

(defmethod luv.lobby.mcclim:lobby-join-entries ((viewer luvcraft-viewer))
  (let ((chores (view-chores viewer))
        (now (wall-seconds)))
    (when (>= now (second chores))
      (setf (second chores) (+ now *join-list-period*)
            (third chores) (gather-join-entries (render:viewer-lobby-client viewer)
                                                (viewer-world viewer) (lan-hosts))))
    (values (third chores) t)))

(defmethod luv.lobby.mcclim:lobby-join ((viewer luvcraft-viewer) entry)
  ;; This view closes, leaving or closing its world, and a guest's opens.
  ;; Both cross frame boundaries, so neither can wait on this frame.
  (let ((target (luv.lobby.mcclim:lobby-join-entry-target entry))
        (player (viewer-local-player viewer)))
    (sb-thread:make-thread
     (lambda ()
       (render:stop-viewer viewer)
       (play :join target :player-id (player-id player) :name (player-name player)))
     :name "luvcraft joining from the lobby")
    target))

(defun forget-view-chores (viewer)
  "VIEWER is closing: its chores end, and the LAN is no longer listened to."
  (remhash viewer *view-chores*)
  (stop-lan-discovery))
