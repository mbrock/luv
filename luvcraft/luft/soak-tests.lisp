;;;; A short soak (#3E8LY0); scripts/luvcraft-soak runs the ten-minute one.
(in-package #:luvcraft.luft.game.tests)

(define-test a-soaked-host-and-guest-agree
  ;; Bots dig and build in both worlds by replaying tapes read back from
  ;; their files; afterwards every event and every resident chunk agree.
  (let ((directory (uiop:ensure-directory-pathname
                    (merge-pathnames (format nil "luvcraft-soak-~D" (random 1000000 (make-random-state t)))
                                     (uiop:temporary-directory)))))
    (unwind-protect
         (let ((result (let ((*standard-output* (make-broadcast-stream)))
                         (host::run-soak :seconds 15 :transport :loopback :paced nil
                                         :tape-directory directory :progress-every nil))))
           (true (getf result :settled))
           (is equal nil (getf result :divergences))
           (is equal nil (getf result :hashes-differed-live))
           (true (getf result :agree))
           ;; Both bots changed the world, the guest through the host.
           (true (< 10 (getf result :events)))
           (true (getf (getf result :host-actions) :edited))
           (true (getf (getf result :guest-actions) :requested))
           ;; Their flag cell sets the guest's stale view against the host.
           (true (getf result :guest-rejected)))
      (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore))))
