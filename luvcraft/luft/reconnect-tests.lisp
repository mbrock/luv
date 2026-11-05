;;;; Company comes and goes (#3JTHM3): a guest whose link drops reconnects and
;;;; resumes, its host keeping its place; a host lists itself in the lobby.
(in-package #:luvcraft.luft.game.tests)

(defun guest-host-peer (session)
  "The host's peer for its only guest."
  (car (first (host::session-guests session))))

(defun drop-link (session)
  "Cut the link to the host's only guest, as a pulled cable would: no bye."
  (luvcraft.wire.net:close-peer (host:session-endpoint session) (guest-host-peer session)))

(defun events-message-p (item)
  (and (luvcraft.wire.net:arrival-p item)
       (typep (luvcraft.wire.net:arrival-message item) 'luvcraft.wire:events-message)))

(defun receive-first-events (guest)
  "Act on what GUEST has received up to its first events message, and lose
the rest in transit."
  (let ((items (loop with deadline = (+ (get-internal-real-time) internal-time-units-per-second)
                     for items = (luvcraft.wire.net:drain-mailbox (host:session-endpoint guest))
                       then (append items (luvcraft.wire.net:drain-mailbox
                                           (host:session-endpoint guest)))
                     until (or (> (count-if #'events-message-p items) 1)
                               (> (get-internal-real-time) deadline))
                     do (sleep 0.01)
                     finally (return items))))
    (loop for item in items
          do (host::handle-session-message guest (luvcraft.wire.net:arrival-peer item)
                                           (luvcraft.wire.net:arrival-message item))
          until (events-message-p item))))

(defun caught-up-p (guest host-world)
  "Whether GUEST is linked and stands at HOST-WORLD's head.  A reply lost
with its link is never answered, so replies are not counted."
  (lambda ()
    (and (eq :open (host::session-state guest))
         (= (host:session-applied guest)
            (terrain:world-journal-head (terrain:world-source-journal (host:world-source host-world)))))))

(defun film-count (player film)
  (let ((entry (luvcraft:block-inventory-entry-for (luvcraft:player-inventory player) film)))
    (if entry (luvcraft:block-inventory-entry-quantity entry) 0)))

(define-test a-guest-whose-link-drops-mid-catch-up-resumes-where-it-was
  (with-host (world session network)
    (multiple-value-bind (guest-world bea guest) (join-resident session network "bea" "Bea")
      (let* ((ada (host:find-world-player world "ada"))
             (bea-there (host:find-world-player world "bea"))
             (from (host:session-applied guest))
             (stone (game:block-placement luvcraft::*stone-block*))
             (film (luvcraft::ensure-film "reconnect-mid-catch-up"))
             (hers (air-cell world 1034 1050))
             (cells (loop for x from 1030 below 1033 collect (air-cell world x 1050))))
        (luvcraft:add-block-to-inventory (luvcraft:player-inventory bea) film 2)
        (setf (host:player-held-block bea) film)
        ;; Ada builds three stones, one at a time, and the host sends each
        ;; on its own; then Bea places a film.  Bea's link drops after the
        ;; first stone reached her: the other two, her own film's event,
        ;; and its reply are lost in transit.
        (dolist (cell cells)
          (is eq :edited (nth-value 1 (host:player-edit-world-cell world ada cell stone)))
          (pump (list session) :seconds 0.05))
        (is eq :requested (nth-value 1 (host:player-edit-world-cell guest-world bea hers
                                                                    (game:block-placement film))))
        (true (pump (list session)
                    :until (lambda ()
                             (= (+ from 4) (terrain:world-journal-head
                                            (terrain:world-source-journal (host:world-source world)))))))
        (receive-first-events guest)
        (let ((partway (host:session-applied guest))
              (head (terrain:world-journal-head (terrain:world-source-journal (host:world-source world)))))
          (true (< from partway head))
          (drop-link session)
          ;; The host keeps her place while she is away; she dials again and
          ;; resumes from her own journal point.
          (true (pump (list session guest)
                      :until (lambda () (eq :reconnecting (host::session-state guest)))))
          (is eq bea-there (host:find-world-player world "bea"))
          (true (pump (list session guest) :until (caught-up-p guest world)))
          (is = head (host:session-applied guest)))
        ;; Nothing was lost and nothing recorded twice: the journals agree
        ;; event for event, and so do the cells.
        (true (worlds-agree-p world guest-world from))
        (is = 4 (length (journal-tail guest-world from)))
        (dolist (cell cells)
          (is eq stone (terrain:scene-material-placement-at (host:world-scene guest-world) cell)))
        ;; Her film was spent once, though she never heard it confirmed
        ;; before the drop, and the host still has her, the same player.
        (is = 1 (film-count bea film))
        (is eq bea-there (host:find-world-player world "bea"))
        ;; She plays on: her next edit is sequenced after them all.
        (let ((next (air-cell world 1035 1050)))
          (is eq :requested (nth-value 1 (host:player-edit-world-cell guest-world bea next stone)))
          (true (pump (list session guest)
                      :until (lambda ()
                               (and (funcall (caught-up-p guest world))
                                    (terrain:scene-material-placement-at
                                     (host:world-scene guest-world) next))))))
        (true (worlds-agree-p world guest-world from))
        (host:close-world-session guest)))))

(define-test the-host-keeps-a-lost-guests-place-for-a-while
  (let ((host::*reconnect-grace* 1/2))
    (with-host (world session network)
      (multiple-value-bind (guest-world bea guest) (join-resident session network "bea" "Bea")
        (declare (ignore guest-world bea))
        (drop-link session)
        ;; Away, Bea is still in the host's world ...
        (pump (list session) :seconds 1/4)
        (true (host:find-world-player world "bea"))
        (is equal '("bea") (getf (host:session-status session) :away))
        ;; ... until she has been away too long.
        (true (pump (list session) :until (lambda () (null (host:find-world-player world "bea")))))
        (true (host::absent-player-record world "bea"))
        (host:close-world-session guest)))))

(define-test a-restarted-host-does-not-resume-a-guest
  (with-host (world session network)
    (multiple-value-bind (guest-world bea guest) (join-resident session network "bea" "Bea")
      (declare (ignore guest-world bea))
      ;; The host goes away without a word and comes back: another session
      ;; of the same world, which has not kept Bea's journal point.
      (drop-link session)
      (host:close-world-session session)
      (let ((again (host:host-luvcraft-world world (host:find-world-player world "ada")
                                             :loopback network :name "host")))
        (unwind-protect
             (progn
               (true (pump (list again guest)
                           :until (lambda () (eq :closed (host::session-state guest)))))
               (is eq nil (host:find-world-player world "bea")))
          (host:close-world-session again)))
      (host:close-world-session guest))))

;;; Finding a game: a host's lobby entry, and the join list built from it.

(defun lobby-test-client (&rest publications)
  "A lobby client that has heard PUBLICATIONS, (TOPIC PAYLOAD) pairs."
  (let ((client (luv.lobby:make-lobby-client
                 :client-id-prefix "test" :transport (make-instance 'luv.lobby:lobby-transport))))
    (loop for (topic payload) on publications by #'cddr
          do (luv.lobby:receive-lobby-publication client topic payload))
    client))

(define-test a-hosts-lobby-entry-lists-it-to-join
  (with-host (world session network)
    ;; As on sockets: the loopback has no port of its own.
    (setf (luvcraft.wire.net::endpoint-port (host:session-endpoint session)) 47475)
    (let* ((advert (host::session-advert session "100.64.0.7"))
           (parsed (host::parse-session-advert advert))
           (tag (getf parsed :tag)))
      (is equal "100.64.0.7" (getf parsed :address))
      (is = 1 (getf parsed :players))
      (is = 32 (length tag))
      (is string= (host::tag-hex (luvcraft.wire.net:endpoint-world-tag
                                  (host:session-endpoint session)))
          tag)
      ;; Junk is not an entry.
      (is eq nil (host::parse-session-advert "address=x
port=nope"))
      ;; Another machine's world -- as this test's world with another port --
      ;; is listed, dialled at its address, while its host's radio is
      ;; present.  An entry left by a radio that is gone is not.
      (let* ((theirs (format nil "address=100.64.0.9~%port=47476~%world=Bea's world~%tag=~A~%players=2~%" tag))
             (client (lobby-test-client "luv/presence/bea-radio" "Bea"
                                        "luv/store/luvcraft/sessions/bea-radio" theirs
                                        "luv/store/luvcraft/sessions/dead-radio" theirs
                                        "luv/store/luvcraft/sessions/ada-radio" advert
                                        "luv/presence/ada-radio" "Ada"))
             (entries (host::gather-join-entries client world nil)))
        ;; Ada's own session is not one she can join.
        (is = 1 (length entries))
        (is string= "Bea's world" (luv.lobby.mcclim:lobby-join-entry-title (first entries)))
        (is string= "100.64.0.9:47476" (luv.lobby.mcclim:lobby-join-entry-target (first entries)))
        (is string= "2 players · tailnet" (luv.lobby.mcclim:lobby-join-entry-detail (first entries)))
        ;; Heard on the LAN too, the same session is dialled there, once;
        ;; a LAN host without an entry follows.
        (let* ((near (luvcraft.wire.net::make-discovered-host
                      :address "192.168.1.9" :port 47476 :name "Bea" :world "Bea's world"
                      :world-tag (luvcraft.wire.net:endpoint-world-tag (host:session-endpoint session))))
               (other (luvcraft.wire.net::make-discovered-host
                       :address "192.168.1.11" :port 47475 :name "Cy" :world "Cy's world"
                       :world-tag (luvcraft.wire:zero-world-tag)))
               (entries (host::gather-join-entries client world (list near other))))
          (is equal '("192.168.1.9:47476" "192.168.1.11:47475")
              (mapcar #'luv.lobby.mcclim:lobby-join-entry-target entries))
          (is string= "2 players · LAN" (luv.lobby.mcclim:lobby-join-entry-detail (first entries))))))))
