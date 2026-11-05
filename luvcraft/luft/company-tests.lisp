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

(define-test a-turtle-is-handed-back-without-a-step-back
  ;; Bea rides the host's turtle and gets off.  Her window shows it where
  ;; her reins left it, then as the host's snapshots show it, a little
  ;; behind time: the hand-back neither steps back nor jumps.  The host's
  ;; turtle, drawn where her track showed it, goes on to where she got off.
  (with-host (world session network)
    (let ((turtle (first (host-turtles world))))
      (multiple-value-bind (guest-world bea guest) (join-resident session network "bea" "Bea")
        (let* ((mount (replica-of guest-world world turtle))
               (character (terrain:make-walking-character
                           :position (host::animal-luft-position mount))))
          (setf (host:player-character bea) character)
          (terrain:add-simulation-character (host:world-simulation guest-world) character)
          (host::begin-world-ride guest-world bea mount)
          (flet ((rein ()
                   (luvcraft::urge-critter mount 1d0 0d0 1/60)
                   (setf (terrain:body-position (terrain:character-body character))
                         (host::animal-luft-position mount))))
            (true (live-until (list world guest-world)
                              (lambda () (equal "bea" (host::turtle-rider-id world turtle)))
                              :each #'rein))
            (live (list world guest-world) 1.0 :each #'rein)
            (let* ((v (luvcraft::critter-velocity mount))
                   (speed (sqrt (+ (expt (v:vec3-x v) 2) (expt (v:vec3-z v) 2))))
                   (ahead (list (/ (v:vec3-x v) speed) 0 (/ (v:vec3-z v) speed)))
                   (left (turtle-at mount))
                   (track nil)
                   (hosts (list (list (turtle-at turtle) t))))
              (true (> speed 0.5))
              (host::end-world-ride guest-world bea mount)
              (live (list world guest-world) 1.0
                    :each (lambda () (push (turtle-at mount) track)
                                  (push (list (turtle-at turtle) (host::turtle-rider-id world turtle)) hosts)))
              (setf track (cons left (reverse track)) hosts (reverse hosts))
              (flet ((along (at) (reduce #'+ (mapcar (lambda (a b c) (* (- a b) c)) at left ahead))))
                ;; It never goes back behind where she got off, but for the rider's
                ;; motion after her last pose the host heard (0.01 cells) ...
                (true (> (reduce #'min track :key #'along) -0.03))
                ;; ... nor leaps from one frame to the next ...
                (true (< (loop for (a b) on track while b maximize (distance a b)) 0.05))
                ;; ... while the host's, let go, closes on where she got off
                ;; no faster than she rode it, a 60th of a second a frame ...
                (true (< (loop for ((a) (b ridden)) on hosts while b
                               unless ridden maximize (distance a b))
                         (* 1.6 (/ speed 60))))
                ;; ... and they come to rest together.
                (true (< (distance (car (last track)) (turtle-at turtle)) 0.1)))))
          (host:close-world-session guest))))))
