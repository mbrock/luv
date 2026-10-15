(defpackage #:luvland.tests
  (:use #:cl)
  (:import-from #:parachute #:define-test #:true #:false #:is #:skip)
  (:local-nicknames (#:wl #:luv.wayland)))

(in-package #:luvland.tests)

(defun program-on-path (name)
  (loop for directory in (uiop:split-string (or (uiop:getenv "PATH") "") :separator ":")
        for candidate = (format nil "~A/~A" directory name)
        when (and (plusp (length directory)) (probe-file candidate))
          return candidate))

(defun client-environment (server)
  (cons (format nil "WAYLAND_DISPLAY=~A" (wl:server-socket-name server))
        (remove-if (lambda (entry)
                     (or (uiop:string-prefix-p "WAYLAND_DISPLAY=" entry)
                         (uiop:string-prefix-p "DISPLAY=" entry)))
                   (sb-ext:posix-environ))))

(defun call-with-device (function)
  (let ((device (ignore-errors
                 (luv:request-gpu-device luv:*gpu-provider*
                                         (luv:make-device-descriptor
                                          :label "Luvland dmabuf test")))))
    (if (null device)
        (skip "no GPU device" (true nil))
        (unwind-protect (funcall function device)
          (luv:destroy device)))))

(define-test vkcube-dmabufs-arrive-and-import
  ;; The whole zero-copy chain without a window: the device's import table
  ;; is offered, a real Vulkan client renders into its own buffers, a commit
  ;; is published once its fences signal, and the device imports it.
  (cond
    ((null (program-on-path "vkcube"))
     (skip "vkcube is not on PATH" (true nil)))
    (t
     (call-with-device
      (lambda (device)
        (let ((description (luvland::dmabuf-description device :bgra8-unorm-srgb)))
          (if (null description)
              (skip "the device cannot import dmabufs" (true nil))
              (let* ((server (wl:start-server
                              :initargs (list :initial-toplevel-size '(640 480)
                                              :dmabuf description)))
                     (process (sb-ext:run-program
                               "vkcube" '("--wsi" "wayland")
                               :search t :wait nil :output nil :error nil
                               :environment (client-environment server)))
                     (frames '()))
                (unwind-protect
                     (progn
                       (loop repeat 200
                             until (>= (length frames) 3)
                             do (sleep 0.02)
                                (wl:call-in-server #'wl:send-frame-callbacks :server server)
                                (dolist (toplevel (wl:server-toplevels server))
                                  (let ((frame (wl:claim-dmabuf-frame
                                                (wl:toplevel-surface toplevel))))
                                    (when frame
                                      (push frame frames)
                                      ;; Hand frames back as a host would, or
                                      ;; the client runs out of buffers.
                                      (when (rest frames)
                                        (let ((old (second frames)))
                                          (wl:call-in-server
                                           (lambda () (wl:release-dmabuf-frame old))
                                           :server server)))))))
                       (true (>= (length frames) 3))
                       (let* ((frame (first frames))
                              (buffer (and frame (wl:dmabuf-frame-buffer frame))))
                         (when buffer
                           (is = 640 (wl:dmabuf-width buffer))
                           (is = 480 (wl:dmabuf-height buffer))
                           (true (find (cons (wl:dmabuf-format buffer)
                                             (wl:dmabuf-modifier buffer))
                                       (getf description :formats) :test #'equal))
                           (let ((texture (luv:import-dmabuf-texture
                                           device
                                           (luv:make-texture-descriptor
                                            :size '(640 480) :dimensions :2d
                                            :format :bgra8-unorm-srgb
                                            :usage '(:texture-binding))
                                           :modifier (wl:dmabuf-modifier buffer)
                                           :planes (wl:dmabuf-frame-planes frame))))
                             (true texture)
                             (luv:destroy texture)))))
                  (when (sb-ext:process-alive-p process)
                    (sb-ext:process-kill process 15))
                  (wl:stop-server server))))))))))

;;; The ratpoison layer, without a GPU: keys, keymaps, sequences, and
;;; command lines on a headless CLIM frame.

(defun key-event (class key-name character &key modifiers (unshifted character))
  (make-instance class :timestamp 0 :key-name key-name :modifiers modifiers
                       :character character :unshifted-character unshifted
                       :repeat-p nil))

(define-test kbd-reads-and-prints-stumpwm-key-names
  (dolist (name '("C-t" "M-Left" "C-M-RET" "SPC" "!" "K" "C-g" "F5" "s-x"))
    (is string= name (luvland::print-key (luvland:kbd name)))))

(define-test keys-are-named-by-layout-character
  ;; On Dvorak the T key is QWERTY's K: the binding follows the character.
  (is equal (luvland:kbd "C-t")
      (luvland::event-key (key-event 'luv:canvas-key-press-event :k (code-char 20)
                                     :modifiers '(:control) :unshifted #\t)))
  (is equal (luvland:kbd "!")
      (luvland::event-key (key-event 'luv:canvas-key-press-event :1 #\!
                                     :modifiers '(:shift) :unshifted #\1)))
  (is equal (luvland:kbd "RET")
      (luvland::event-key (key-event 'luv:canvas-key-press-event :return nil))))

(define-test key-sequences-are-read-one-event-at-a-time
  (let* ((luvland:*top-map* (luvland:make-sparse-keymap))
         (luvland:*root-map* (luvland:make-sparse-keymap))
         (ran '())
         (luvland (make-instance 'luvland::luvland)))
    (luvland:define-key luvland:*top-map* (luvland:kbd "C-t") 'luvland:*root-map*)
    (luvland:define-key luvland:*root-map* (luvland:kbd "c")
                        (lambda (luvland) (declare (ignore luvland)) (push :c ran)))
    (flet ((press (key-name character &rest options)
             (luvland::handle-command-key
              luvland (apply #'key-event 'luv:canvas-key-press-event
                             key-name character options))))
      ;; An unbound key at top level belongs to the client.
      (false (press :a #\a))
      ;; The prefix is taken and the sequence waits for its next key.
      (true (press :k (code-char 20) :modifiers '(:control) :unshifted #\t))
      (true (luvland::luvland-pending-keymaps luvland))
      ;; A modifier mid-sequence still reaches the client and changes nothing.
      (false (press :shift-left nil))
      (true (luvland::luvland-pending-keymaps luvland))
      (true (press :c #\c))
      (is equal '(:c) ran)
      (false (luvland::luvland-pending-keymaps luvland))
      ;; An unbound key after the prefix is consumed and ends the sequence.
      (press :k (code-char 20) :modifiers '(:control) :unshifted #\t)
      (true (press :z #\z))
      (false (luvland::luvland-pending-keymaps luvland))
      (is equal '(:c) ran))))

(define-test command-lines-parse-against-the-command-table
  (let* ((luvland (make-instance 'luvland::luvland))
         (*error-output* (make-broadcast-stream)))
    (setf (luvland::luvland-command-frame luvland)
          (clim:make-application-frame 'luvland::luvland-frame :luvland luvland))
    (is string= "No window 3." (luvland::run-command-line luvland "select 3"))
    (is string= "No window 7." (luvland::run-command-line luvland "  SELECT   7 "))
    (is string= "No command \"frobnicate\"." (luvland::run-command-line luvland "frobnicate"))
    (true (search "is not a integer"
                  (luvland::run-command-line luvland "select seven")))))
