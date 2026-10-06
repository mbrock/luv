(defpackage #:luv.wayland.tests
  (:use #:cl)
  (:import-from #:parachute #:define-test #:true #:false #:is #:skip)
  (:local-nicknames (#:wl #:luv.wayland)))

(in-package #:luv.wayland.tests)

(define-test protocol-signatures-match-libwayland
  ;; The same strings wayland-scanner writes into its generated tables.
  (flet ((signature (interface request)
           (wl::signature-string
            (wl::find-request (wl:find-interface interface) request))))
    (is string= "?oii" (signature "wl_surface" :attach))
    (is string= "5ii" (signature "wl_surface" :offset))
    (is string= "no" (signature "xdg_wm_base" :get-xdg-surface))
    (is string= "n?oo" (signature "xdg_surface" :get-popup))
    ;; An untyped new_id expands to interface name, version, and id.
    (is string= "usun" (signature "wl_registry" :bind))))

(define-test protocol-enums-resolve-across-interfaces
  (let ((toplevel (wl:find-interface "xdg_toplevel")))
    (is = 4 (wl::enum-value toplevel "state" :activated))
    (is = 2 (wl::enum-value (wl:find-interface "zxdg_toplevel_decoration_v1")
                            "mode" :server-side))
    (is = 1 (wl::enum-value toplevel "wl_output.transform" :90))))

(defun foot-program ()
  (let ((path (uiop:getenv "PATH")))
    (loop for directory in (uiop:split-string path :separator ":")
          for candidate = (format nil "~A/foot" directory)
          when (and (plusp (length directory)) (probe-file candidate))
            return candidate)))

(defun wait-for (predicate &key (timeout 10))
  (loop with deadline = (+ (get-internal-real-time)
                           (* timeout internal-time-units-per-second))
        for value = (funcall predicate)
        until (or value (> (get-internal-real-time) deadline))
        do (sleep 0.02)
        finally (return value)))

(defun distinct-pixels (pixels)
  (let ((seen (make-hash-table)))
    (dotimes (index (array-total-size pixels))
      (setf (gethash (row-major-aref pixels index) seen) t))
    (hash-table-count seen)))

(defun host-frame (server toplevel function)
  "Do what a host does each frame: claim TOPLEVEL's newest snapshot for
FUNCTION, then send frame callbacks.  Return FUNCTION's value, or NIL when
there was nothing new to claim."
  (prog1 (wl:call-with-surface-snapshot (wl:toplevel-surface toplevel) function)
    (wl:call-in-server #'wl:send-frame-callbacks :server server)))

(define-test foot-draws-a-window-of-the-configured-size
  (let ((foot (foot-program)))
    (if (null foot)
        (skip "foot is not on PATH" (true nil))
        (let* ((server (wl:start-server
                        :initargs '(:initial-toplevel-size (640 400))))
               (process nil))
          (unwind-protect
               (progn
                 (setf process
                       (sb-ext:run-program
                        foot (list "--override=main.resize-by-cells=no"
                                   "sh" "-c" "echo luvland; sleep 30")
                        :wait nil :output nil :error nil
                        :environment
                        (append (list (format nil "WAYLAND_DISPLAY=~A"
                                              (wl:server-socket-name server)))
                                (remove-if (lambda (entry)
                                             (or (uiop:string-prefix-p "WAYLAND_DISPLAY=" entry)
                                                 (uiop:string-prefix-p "DISPLAY=" entry)))
                                           (sb-ext:posix-environ)))))
                 (let ((toplevel
                         (wait-for
                          (lambda ()
                            (find-if (lambda (toplevel)
                                       (and (wl:toplevel-mapped-p toplevel)
                                            (wl:surface-snapshot
                                             (wl:toplevel-surface toplevel))))
                                     (wl:server-toplevels server))))))
                   (true toplevel)
                   (when toplevel
                     (let ((snapshot (wl:surface-snapshot (wl:toplevel-surface toplevel))))
                       (is string= "foot" (wl:toplevel-app-id toplevel))
                       (is = 640 (wl:snapshot-width snapshot))
                       (is = 400 (wl:snapshot-height snapshot)))
                     ;; The first commit is bare background.  Play the host:
                     ;; send frame callbacks and claim each new snapshot
                     ;; until the shell's text is drawn.
                     (true (wait-for
                            (lambda ()
                              (host-frame server toplevel
                                          (lambda (snapshot)
                                            (> (distinct-pixels (wl:snapshot-pixels snapshot))
                                               2))))))
                     ;; Typing echoes through the tty, so each key is a commit.
                     ;; However many commits arrive, the surface reuses a
                     ;; handful of arrays instead of allocating per commit.
                     (let ((surface (wl:toplevel-surface toplevel))
                           (serials '()))
                       (wl:call-in-server (lambda () (wl:focus-keyboard surface))
                                          :server server)
                       (dotimes (index 12)
                         (wl:call-in-server (lambda ()
                                              (wl:send-key 30 t)
                                              (wl:send-key 30 nil))
                                            :server server)
                         (wait-for (lambda ()
                                     (host-frame server toplevel
                                                 (lambda (snapshot)
                                                   (pushnew (wl:snapshot-serial snapshot)
                                                            serials))))
                                   :timeout 2))
                       (true (>= (length serials) 8))
                       (true (<= (wl:snapshot-pool-allocated
                                  (wl:surface-snapshot-pool surface))
                                 3)))
                     (wl:call-in-server (lambda () (wl:close-toplevel toplevel))
                                        :server server)
                     (true (wait-for (lambda () (not (sb-ext:process-alive-p process)))
                                     :timeout 5)))))
            (when (and process (sb-ext:process-alive-p process))
              (sb-ext:process-kill process 15))
            (wl:stop-server server)
            (false (wl:server-running-p server)))))))
