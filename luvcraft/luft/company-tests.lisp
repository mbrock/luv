;;;; Company made comfortable (#LV16J7), over the loopback network.  These
;;;; use the session tests' worlds and pumps.
(in-package #:luvcraft.luft.game.tests)

(define-test both-players-read-each-line-of-chat-under-its-speakers-name
  (with-host (world session network)
    (multiple-value-bind (guest-world bea guest) (join-resident session network "bea" "Bea")
      (declare (ignore bea))
      (flet ((heard (session line)
               (lambda () (member line (host::session-chat-lines session) :test #'string=))))
        ;; Bea's line reaches the host and comes back to her once it has.
        (host::say-in-world guest-world (format nil "  hej Ada,~%kom hit  "))
        (is eq nil (host::session-chat-lines guest))
        (true (pump (list session guest) :until (heard guest "Bea: hej Ada, kom hit")))
        (true (funcall (heard session "Bea: hej Ada, kom hit")))
        ;; Ada's goes the other way, under her own name.
        (host::say-in-world world "I'm on the hill")
        (true (pump (list session guest) :until (heard guest "Ada: I'm on the hill")))
        (is equal '("Ada: I'm on the hill" "Bea: hej Ada, kom hit")
            (host::session-chat-lines guest))
        (is equal (host::session-chat-lines session) (host::session-chat-lines guest))
        ;; A guest speaks only as itself: the host names the speaker.
        (luvcraft.wire.net:send-message (host:session-endpoint guest)
                                        (luvcraft.wire:make-chat-message :speaker "Ada"
                                                                         :text "it's me, Ada")
                                        (host::session-peer guest))
        (true (pump (list session guest) :until (heard guest "Bea: it's me, Ada")))
        (is eq nil (funcall (heard session "Ada: it's me, Ada")))
        (host:close-world-session guest)))))
