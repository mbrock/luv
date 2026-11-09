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

(define-test app-ids-name-windows-unless-made-up
  (is string= "Nautilus" (luvland::app-id-name "org.gnome.Nautilus"))
  (is string= "foot" (luvland::app-id-name "foot"))
  ;; Chromium's ids for web apps: the dotted host is not a reverse-DNS name.
  (false (luvland::app-id-name "chrome-127.0.0.1__-Default"))
  (false (luvland::app-id-name "chrome-fmgjjmmmlfnkbppncabfkddbjimcfncm-Default"))
  (false (luvland::app-id-name ""))
  (false (luvland::app-id-name nil)))

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

;;; Luvland's own terminal, without a GPU: keys reach its shell past the
;;; prefix, and it closes with the shell.

(defun terminal-text (terminal)
  (luv.terminal:call-with-pty-device-terminal (luvland::terminal-device terminal)
                                              #'luv.ghostty:terminal-text))

(defun wait-until (predicate &key (timeout 3.0))
  (loop repeat (round timeout 0.01)
        thereis (funcall predicate)
        do (sleep 0.01)))

(define-test terminals-read-keys-past-the-prefix-and-close-with-their-shell
  (if (null (ignore-errors (luv.ghostty:load-libghostty-vt)))
      (skip "libghostty-vt is not loadable" (true nil))
      (let* ((luvland (make-instance 'luvland::luvland))
             (terminal (luvland::start-terminal :columns 40 :rows 4 :program '("cat"))))
        (setf (luvland::luvland-command-frame luvland)
              (clim:make-application-frame 'luvland::luvland-frame :luvland luvland)
              (luvland::luvland-terminals luvland) (list terminal)
              (luvland::luvland-focus luvland) terminal)
        (unwind-protect
             (flet ((press (key-name character &rest options)
                      (luv:handle-canvas-event
                       luvland nil (apply #'key-event 'luv:canvas-key-press-event
                                          key-name character options)))
                    (control (key-name character)
                      (list key-name (code-char (- (char-code character) 96))
                            :modifiers '(:control) :unshifted character)))
               (press :h #\h)
               (press :i #\i)
               ;; C-t n is Luvland's; C-t t sends C-t itself, which the tty
               ;; echoes as ^T.
               (apply #'press (control :t #\t))
               (press :n #\n)
               (apply #'press (control :t #\t))
               (press :t #\t)
               (press :return nil)
               (true (wait-until (lambda () (search "hi^T" (terminal-text terminal)))))
               (false (search "n" (terminal-text terminal)))
               ;; End of file ends cat, and the next sync drops its window.
               (apply #'press (control :d #\d))
               (true (wait-until (lambda () (null (luvland::live-terminals luvland))))))
          (luvland::close-terminal luvland terminal)))))

;;; The bar's sources, against real D-Bus daemons Luvland does not own: a
;;; private bus, a StatusNotifierItem as nm-applet would publish one, and a
;;; NetworkManager with one Wi-Fi connection.

(defun call-with-private-bus (function)
  "Call FUNCTION with the address of a fresh dbus-daemon, which is also the
session bus meanwhile."
  (if (null (program-on-path "dbus-daemon"))
      (skip "dbus-daemon is not on PATH" (true nil))
      (let* ((process (sb-ext:run-program "dbus-daemon" '("--session" "--nofork" "--print-address=1")
                                          :search t :wait nil :output :stream :error nil))
             (address (string-trim '(#\Space #\Newline)
                                   (read-line (sb-ext:process-output process))))
             (previous (uiop:getenv "DBUS_SESSION_BUS_ADDRESS")))
        (sb-posix:setenv "DBUS_SESSION_BUS_ADDRESS" address 1)
        (unwind-protect (funcall function address)
          (if previous
              (sb-posix:setenv "DBUS_SESSION_BUS_ADDRESS" previous 1)
              (sb-posix:unsetenv "DBUS_SESSION_BUS_ADDRESS"))
          (sb-ext:process-kill process 15)))))

(defun variant-plist (&rest pairs)
  "PAIRS, name signature value ..., as an a{sv} dictionary."
  (loop for (name signature value) on pairs by #'cdddr
        collect (cons name (luv.dbus:make-variant signature value))))

(defun menu-node (id properties &rest children)
  (list id properties
        (mapcar (lambda (child) (luv.dbus:make-variant "(ia{sv}av)" child)) children)))

(defun solid-pixmap (size argb)
  (let ((octets (make-array (* 4 size size) :element-type '(unsigned-byte 8))))
    (dotimes (i (* size size))
      (dotimes (byte 4)
        (setf (aref octets (+ (* 4 i) byte)) (ldb (byte 8 (* 8 (- 3 byte))) argb))))
    (list size size octets)))

(defclass fake-tray-item ()
  ((bus :initarg :bus :reader fake-bus)
   (path :initarg :path :initform "/StatusNotifierItem" :reader fake-path)
   (title :initarg :title :accessor fake-title)
   (pixmaps :initarg :pixmaps :initform '() :reader fake-pixmaps)
   (calls :initform '() :accessor fake-calls)))

(defun fake-menu-layout ()
  (menu-node 0 '()
             (menu-node 1 (variant-plist "label" "s" "_Enable Wi-Fi"
                                         "toggle-type" "s" "checkmark"
                                         "toggle-state" "i" 1))
             (menu-node 2 (variant-plist "type" "s" "separator"))
             (menu-node 3 (variant-plist "label" "s" "Hidden" "visible" "b" nil))
             (menu-node 5 (variant-plist "label" "s" "Disabled" "enabled" "b" nil))
             (menu-node 6 (variant-plist "label" "s" "VPN" "children-display" "s" "submenu")
                        (menu-node 7 (variant-plist "label" "s" "Connect __home")))))

(defun serve-fake-tray-item (fake)
  "Answer as an item at FAKE's path: properties, Activate, and a dbusmenu."
  (luv.dbus:add-handler
   (fake-bus fake)
   (lambda (bus message)
     (when (eq (luv.dbus:message-type message) :method-call)
       (let ((member (luv.dbus:message-member message))
             (path (luv.dbus:message-path message)))
         (cond
           ((and (string= path (fake-path fake)) (string= member "GetAll"))
            (luv.dbus:reply bus message :signature "a{sv}"
                                        :arguments (list (variant-plist
                                                          "Id" "s" "nm-applet"
                                                          "Title" "s" (fake-title fake)
                                                          "Status" "s" "Active"
                                                          "IconName" "s" "nm-signal-75"
                                                          "IconPixmap" "a(iiay)" (fake-pixmaps fake)
                                                          "ItemIsMenu" "b" nil
                                                          "Menu" "o" "/MenuBar")))
            t)
           ((string= member "Activate")
            (push (list :activate) (fake-calls fake))
            (luv.dbus:reply bus message)
            t)
           ((and (string= path "/MenuBar") (string= member "AboutToShow"))
            (luv.dbus:reply bus message :signature "b" :arguments '(nil))
            t)
           ((and (string= path "/MenuBar") (string= member "GetLayout"))
            (luv.dbus:reply bus message :signature "u(ia{sv}av)"
                                        :arguments (list 1 (fake-menu-layout)))
            t)
           ((and (string= path "/MenuBar") (string= member "Event"))
            (push (list :event (first (luv.dbus:message-body message))
                        (second (luv.dbus:message-body message)))
                  (fake-calls fake))
            (luv.dbus:reply bus message)
            t)))))))

(defun open-fake-tray-item (&key (name "org.example.FakeApplet") (title "Network")
                                 (pixmaps (list (solid-pixmap 16 #xff00ff00)
                                                (solid-pixmap 48 #xc0336699)
                                                (solid-pixmap 64 #xff000000)))
                                 path)
  "Publish a fake item and register it as nm-applet would, by NAME, or as
libappindicator would, by PATH alone."
  (let* ((bus (luv.dbus:open-bus :session))
         (fake (make-instance 'fake-tray-item :bus bus :title title :pixmaps pixmaps
                                              :path (or path "/StatusNotifierItem"))))
    (serve-fake-tray-item fake)
    (unless path (luv.dbus:request-name bus name))
    (luv.dbus:call bus "org.kde.StatusNotifierWatcher" "/StatusNotifierWatcher"
                   "org.kde.StatusNotifierWatcher" "RegisterStatusNotifierItem"
                   :signature "s" :arguments (list (or path name)))
    fake))

(define-test the-tray-watches-items-their-menus-and-their-exits
  (call-with-private-bus
   (lambda (address)
     (declare (ignore address))
     (let ((tray (luvland::start-tray)))
       (unwind-protect
            (let ((fake (open-fake-tray-item)))
              (true (luvland::tray-watcher-p tray))
              (true (wait-until (lambda () (luvland::tray-items tray))))
              (let ((item (first (luvland::tray-items tray))))
                (is string= "Network" (luvland::tray-item-name item))
                (is string= "org.example.FakeApplet/StatusNotifierItem"
                    (luvland::tray-item-key item))
                ;; The smallest pixmap at least 48 wide, not the first or largest.
                (is = 48 (first (luvland::tray-item-pixmap item)))
                ;; Separators, hidden, and disabled entries are not offered;
                ;; submenus flatten, mnemonics go, and toggles show their state.
                (is equal '(("● Enable Wi-Fi" . 1) ("VPN › Connect _home" . 7))
                    (luvland::tray-item-menu item))
                (luvland::click-tray-menu-entry tray item 7)
                (luvland::activate-tray-item tray item)
                (true (wait-until (lambda () (and (find :activate (fake-calls fake) :key #'first)
                                                  (find :event (fake-calls fake) :key #'first)))))
                (is equal '(:event 7 "clicked") (find :event (fake-calls fake) :key #'first)))
              ;; A changed title is read again when the item says so.
              (setf (fake-title fake) "Wi-Fi: plank-net")
              (luv.dbus:emit-signal (fake-bus fake) "/StatusNotifierItem"
                                    "org.kde.StatusNotifierItem" "NewTitle")
              (true (wait-until (lambda () (equal "Wi-Fi: plank-net"
                                                  (luvland::tray-item-name
                                                   (first (luvland::tray-items tray)))))))
              ;; libappindicator registers an object path its connection serves.
              (let ((path-fake (open-fake-tray-item :path "/org/ayatana/NotificationItem/x"
                                                    :title "Indicator" :pixmaps '())))
                (true (wait-until (lambda () (= 2 (length (luvland::tray-items tray))))))
                (let ((item (second (luvland::tray-items tray))))
                  (is string= (luv.dbus:bus-unique-name (fake-bus path-fake))
                      (luvland::tray-item-service item))
                  (false (luvland::tray-item-pixmap item)))
                ;; An item leaves with its connection.
                (luv.dbus:close-bus (fake-bus path-fake))
                (true (wait-until (lambda () (= 1 (length (luvland::tray-items tray)))))))
              (luv.dbus:close-bus (fake-bus fake))
              (true (wait-until (lambda () (null (luvland::tray-items tray))))))
         (luvland::stop-tray tray))))))

(defun serve-fake-network-manager (bus primary)
  "Serve NetworkManager's properties on BUS, its primary connection the
CAR of PRIMARY, a Wi-Fi connection or \"/\" for none."
  (let ((properties
          `(("/org/freedesktop/NetworkManager/ActiveConnection/1"
             ("Type" . ,(luv.dbus:make-variant "s" "802-11-wireless"))
             ("Id" . ,(luv.dbus:make-variant "s" "plank-net"))
             ("Devices" . ,(luv.dbus:make-variant "ao" '("/org/freedesktop/NetworkManager/Devices/3"))))
            ("/org/freedesktop/NetworkManager/Devices/3"
             ("ActiveAccessPoint" . ,(luv.dbus:make-variant "o" "/org/freedesktop/NetworkManager/AccessPoint/9")))
            ("/org/freedesktop/NetworkManager/AccessPoint/9"
             ("Strength" . ,(luv.dbus:make-variant "y" 68))))))
    (luv.dbus:request-name bus "org.freedesktop.NetworkManager")
    (luv.dbus:add-handler
     bus (lambda (bus message)
           (when (equal (luv.dbus:message-member message) "Get")
             (let* ((path (luv.dbus:message-path message))
                    (name (second (luv.dbus:message-body message)))
                    (value (if (and (string= path "/org/freedesktop/NetworkManager")
                                    (string= name "PrimaryConnection"))
                               (luv.dbus:make-variant "o" (car primary))
                               (cdr (assoc name (cdr (assoc path properties :test #'string=))
                                           :test #'string=)))))
               (if value
                   (luv.dbus:reply bus message :signature "v" :arguments (list value))
                   (luv.dbus:reply-error bus message "org.freedesktop.DBus.Error.UnknownProperty"))
               t))))))

(define-test the-network-indicator-reads-networkmanager
  (call-with-private-bus
   (lambda (address)
     (let ((manager (luv.dbus:open-bus address))
           (reader (luv.dbus:open-bus address))
           (primary (list "/org/freedesktop/NetworkManager/ActiveConnection/1")))
       (unwind-protect
            (progn
              ;; Without NetworkManager there is nothing to say.
              (false (luvland::sample-network reader))
              (serve-fake-network-manager manager primary)
              (is equal '(:kind :wifi :name "plank-net" :strength 68)
                  (luvland::sample-network reader))
              (setf (car primary) "/")
              (is equal '(:kind :offline) (luvland::sample-network reader)))
         (luv.dbus:close-bus manager)
         (luv.dbus:close-bus reader))))))

(define-test indicators-read-wpctl-and-sysfs
  (is equal '(:level 45 :muted-p nil) (luvland::parse-wpctl-volume "Volume: 0.45"))
  (is equal '(:level 30 :muted-p t) (luvland::parse-wpctl-volume "Volume: 0.30 [MUTED]"))
  (is equal '(:level 120 :muted-p nil) (luvland::parse-wpctl-volume "Volume: 1.20"))
  (false (luvland::parse-wpctl-volume "Could not connect to PipeWire"))
  (let ((root (uiop:ensure-directory-pathname
               (format nil "/tmp/luvland-power-~D/" (random 1000000)))))
    (flet ((supply (name &rest files)
             (loop for (file contents) on files by #'cddr
                   do (let ((path (merge-pathnames file (merge-pathnames (format nil "~A/" name) root))))
                        (ensure-directories-exist path)
                        (with-open-file (stream path :direction :output :if-exists :supersede)
                          (write-line contents stream))))))
      (unwind-protect
           (progn
             (supply "AC" "type" "Mains" "online" "1")
             (supply "BAT0" "type" "Battery" "capacity" "76" "status" "Charging")
             (supply "ps-controller-battery-aa" "type" "Battery" "capacity" "40"
                     "status" "Discharging" "scope" "Device")
             (let* ((luvland::*power-supply-directory* root)
                    (batteries (sort (luvland::sample-batteries) #'string<
                                     :key (lambda (battery) (getf battery :name)))))
               (is = 2 (length batteries))
               (is equal '("BAT0" 76 t nil)
                   (let ((battery (first batteries)))
                     (list (getf battery :name) (getf battery :level)
                           (and (getf battery :charging-p) t) (getf battery :device-p))))
               (true (getf (second batteries) :device-p))
               (false (getf (second batteries) :charging-p))))
        (uiop:delete-directory-tree root :validate t)))))
