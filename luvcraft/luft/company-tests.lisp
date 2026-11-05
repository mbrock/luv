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

(define-test a-guests-day-knobs-ask-the-host-and-hold-meanwhile
  (with-host (world session network)
    (let ((clock (host::world-sky-clock world)))
      (setf (luvcraft::sky-clock-day-fraction clock) 0.3d0)
      (multiple-value-bind (guest-world bea guest) (join-resident session network "bea" "Bea")
        (let* ((guest-clock (host::world-sky-clock guest-world))
               ;; Bea's window, whose knobs she turns.
               (viewer (make-instance 'host:luvcraft-viewer :world guest-world :local-player bea
                                                            :source (host:world-scene guest-world))))
          (flet ((set-knob (name value)
                   (luv.knobs:set-knob-value (luv.knobs:find-knob name) value viewer))
                 (hour (clock) (luvcraft::sky-clock-hour clock)))
            (set-knob 'render::time-of-day 18.0)
            (set-knob 'render::day-length 3.0)
            (set-knob 'render::sun-orbit-tilt 0.6)
            ;; Her own sky turns at once ...
            (true (< (abs (- 18 (hour guest-clock))) 0.01))
            ;; ... and a snapshot from before the host heard her does not turn it back.
            (host::take-entity-snapshot
             guest-world (luvcraft.wire:make-entity-snapshot
                          :tick (1+ (host::living-newest-tick (host::living guest-world)))
                          :entities (list (host::sky-entity-state world))))
            (true (< (abs (- 18 (hour guest-clock))) 0.01))
            (is = 0.6 (host::world-sun-orbit-tilt guest-world))
            ;; The host's day is what she asked for, and stays hers in both windows.
            (live (list world guest-world) 0.5)
            (true (< (abs (- 18 (hour clock))) 0.15)) ; a 3-minute day runs on 0.07 h in 0.5 s
            (true (< (abs (- 3 (luvcraft::sky-clock-minutes-per-day clock))) 1d-6))
            (is = 0.6 (host::world-sun-orbit-tilt world))
            (set-knob 'render::freeze-time t)
            (live (list world guest-world) 1.5)
            (true (luvcraft::sky-clock-paused-p clock))
            (true (luvcraft::sky-clock-paused-p guest-clock))
            (true (< (abs (- (hour clock) (hour guest-clock))) 0.01))
            (true (< (abs (- 3 (luvcraft::sky-clock-minutes-per-day guest-clock))) 1d-6))
            (is = 0.6 (host::world-sun-orbit-tilt guest-world))
            (host:close-world-session guest)))))))
