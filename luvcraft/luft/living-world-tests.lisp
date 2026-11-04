;;;; The host's living world, followed by a guest over loopback (#U4TBMP).
(in-package #:luvcraft.luft.game.tests)

(defun place-turtle (world x y seed)
  "A turtle standing on WORLD's ground at Luft cell X, Y."
  (make-instance 'luvcraft::turtle
                 :seed seed :yaw (* 0.7d0 seed) :heading (* 0.7d0 seed)
                 :position (v:make-vec3 (- x 1024d0) (float (ground world x y) 1d0) (- y 1024d0))))

(defun turtle-at (turtle)
  (let ((p (luvcraft::critter-position turtle)))
    (list (v:vec3-x p) (v:vec3-y p) (v:vec3-z p))))

(defun distance (a b)
  (sqrt (reduce #'+ (mapcar (lambda (a b) (expt (- a b) 2)) a b))))

(defun live (worlds seconds &key (step 1/60) each)
  "Advance WORLDS together for SECONDS of wall time, as views would, calling
EACH after every step."
  (let ((deadline (+ (get-internal-real-time) (* seconds internal-time-units-per-second))))
    (loop while (< (get-internal-real-time) deadline)
          do (dolist (world worlds) (host:advance-luvcraft-world world step))
             (when each (funcall each))
             (sleep step))))

(defun live-until (worlds until &key (seconds 5) each)
  (let ((deadline (+ (get-internal-real-time) (* seconds internal-time-units-per-second))))
    (loop (live worlds 1/30 :each each)
          (when (funcall until) (return t))
          (when (> (get-internal-real-time) deadline) (return nil)))))

(defun host-turtles (world)
  (setf (host::world-animals world)
        (list (place-turtle world 1040 1040 1) (place-turtle world 1048 1036 2)
              (place-turtle world 1036 1050 3))
        (host::world-animals-initialized-p world) t)
  (host::world-animals world))

(defun replica-of (guest-world host-world turtle)
  "GUEST-WORLD's replica of HOST-WORLD's TURTLE, found by entity id."
  (host::find-world-turtle guest-world (host::turtle-entity-id host-world turtle)))

(define-test a-guests-turtles-balls-and-clock-follow-the-hosts
  (with-host (world session network)
    (let* ((turtles (host-turtles world))
           (clock (host::world-sky-clock world))
           (floor (ground world 1044 1046))
           (ball (luvcraft::spawn-body-of-kind (host::ensure-world-physics world) :marble
                                               20.5 (+ floor 1.5) 22.5)))
      (setf (luvcraft::sky-clock-day-fraction clock) 0.8d0
            (luvcraft::sky-clock-minutes-per-day clock) 7
            (host::world-sun-orbit-tilt world) 0.3)
      (multiple-value-bind (guest-world bea guest) (join-resident session network "bea" "Bea")
        (declare (ignore bea))
        (let ((guest-clock (host::world-sky-clock guest-world)))
          ;; The welcome brings the turtles and the hour at once.
          (is = 3 (length (host::world-animals guest-world)))
          (true (< (abs (- 0.8d0 (luvcraft::sky-clock-day-fraction guest-clock))) 1d-3))
          (live (list world guest-world) 1.5)
          (progn ; each turtle follows its own, a little behind time
            (dolist (turtle turtles)
              (let ((replica (replica-of guest-world world turtle)))
                (true replica)
                (true (< (distance (turtle-at turtle) (turtle-at replica)) 0.4d0))
                ;; ... and is nearer it than any other host turtle.
                (is eq turtle (first (sort (copy-list turtles) #'<
                                           :key (lambda (other)
                                                  (distance (turtle-at other)
                                                            (turtle-at replica)))))))))
          (progn ; the guest simulates no balls of its own
            (is eq nil (host::world-physics guest-world))
            (let ((balls (host::world-ball-list guest-world))
                  (centre (multiple-value-list
                           (luvcraft::physics-body-position (host::world-physics world) ball))))
              (is = 1 (length balls))
              (is = ball (first (first balls)))
              (is eq :marble (luvcraft::body-kind-name (fourth (first balls))))
              ;; The marble has fallen and come to rest, and the guest shows it there.
              (true (< (second centre) (+ floor 0.5)))
              (true (< (distance centre (second (first balls))) 0.05))))
          (progn ; the sky is the host's
            (is = 7 (round (luvcraft::sky-clock-minutes-per-day guest-clock)))
            (true (< (abs (- (luvcraft::sky-clock-day-fraction clock)
                             (luvcraft::sky-clock-day-fraction guest-clock)))
                     1d-3))
            (true (> (luvcraft::sky-clock-day-fraction guest-clock) 0.8d0))
            (is = 0.3 (host::world-sun-orbit-tilt guest-world))
            (setf (luvcraft::sky-clock-paused-p clock) t)
            (live (list world guest-world) 0.3)
            (true (luvcraft::sky-clock-paused-p guest-clock)))
          (progn ; a turtle the host forgets is forgotten
            (setf (host::world-animals world) (rest turtles))
            (live (list world guest-world) 0.3)
            (is = 2 (length (host::world-animals guest-world)))
            (is eq nil (replica-of guest-world world (first turtles))))
          (host:close-world-session guest))))))

(define-test a-guests-throw-happens-once-on-the-host
  (with-host (world session network)
    (multiple-value-bind (guest-world bea guest) (join-resident session network "bea" "Bea")
      (declare (ignore bea))
      (flet ((host-bodies ()
               (let ((physics (host::world-physics world)) (count 0))
                 (when physics (luvcraft::do-physics-bodies (handle physics) (incf count)))
                 count)))
        (is = 0 (host-bodies))
        ;; Bea throws: her world only asks.
        (is eq nil (host::throw-world-body guest-world :smiley-ball 16d0 30d0 20d0 2d0 3d0 0d0))
        (is eq nil (host::world-physics guest-world))
        (true (live-until (list world guest-world)
                          (lambda () (host::world-ball-list guest-world))))
        (live (list world guest-world) 0.5)
        (is = 1 (host-bodies))
        (is = 1 (length (host::world-ball-list guest-world)))
        (is eq :smiley-ball (luvcraft::body-kind-name (fourth (first (host::world-ball-list world)))))
        (is eq nil (host::world-physics guest-world))
        (host:close-world-session guest)))))

(define-test a-guest-rides-the-hosts-turtle
  (with-host (world session network)
    (let* ((turtle (first (host-turtles world)))
           (ada (host:find-world-player world "ada")))
      (multiple-value-bind (guest-world bea guest) (join-resident session network "bea" "Bea")
        (let* ((mount (replica-of guest-world world turtle))
               (start (turtle-at mount))
               (character (terrain:make-walking-character
                           :position (host::animal-luft-position mount))))
          (setf (host:player-character bea) character)
          (terrain:add-simulation-character (host:world-simulation guest-world) character)
          (host::begin-world-ride guest-world bea mount)
          (flet ((rein ()
                   ;; As a view rides: the reins urge the mount, and the
                   ;; rider is carried on its back.
                   (luvcraft::urge-critter mount 1d0 0.4d0 1/60)
                   (setf (terrain:body-position (terrain:character-body character))
                         (host::animal-luft-position mount))))
            (true (live-until (list world guest-world)
                              (lambda () (equal "bea" (host::turtle-rider-id world turtle)))
                              :each #'rein))
            (progn ; the host's turtle goes where the rider's pose carries it
              (live (list world guest-world) 1.5 :each #'rein)
              (true (> (distance start (turtle-at mount)) 0.5d0))
              (true (< (distance (turtle-at turtle) (turtle-at mount)) 0.4d0))
              (is eq nil (luvcraft::turtle-resting-p turtle)))
            (progn ; nobody else may mount it meanwhile
              (true (host::ridden-by-another-p world ada turtle)))
            (host::end-world-ride guest-world bea mount)
            (true (live-until (list world guest-world)
                              (lambda () (null (host::turtle-rider-id world turtle)))))
            (progn ; afterwards the turtle has its own mind back
              (true (luvcraft::turtle-resting-p turtle))
              (true (< (distance (turtle-at turtle) (turtle-at mount)) 0.5d0))))
          ;; The host mounts it; Bea, not yet told, asks too, and is refused.
          (host::begin-world-ride world ada turtle)
          (host::begin-world-ride guest-world bea mount)
          (true (live-until (list world guest-world)
                            (lambda () (equal "ada" (host::turtle-rider-id guest-world mount)))))
          (is equal "ada" (host::turtle-rider-id world turtle))
          (host:close-world-session guest))))))
