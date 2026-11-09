;;;; What the machine is doing, for the bar: network, sound, and batteries.
;;;;
;;;; A sampler thread reads them every few seconds from where Linux keeps
;;;; them -- NetworkManager on the system bus, PipeWire through wpctl, and
;;;; power supplies in sysfs -- and publishes one immutable plist.  The bar
;;;; only reads that plist, so a slow daemon never holds a frame.  Anything
;;;; the machine lacks is simply NIL and leaves no gap in the bar.

(in-package #:luvland)

(defparameter *indicator-seconds* 2
  "Seconds between samples of the machine's state.")

;;; Batteries, from /sys/class/power_supply.  A gamepad's battery has scope
;;; Device; the machine's own has scope System or none.

(defparameter *power-supply-directory* #P"/sys/class/power_supply/")

(defun read-sysfs (directory name)
  (let ((line (ignore-errors
               (with-open-file (stream (merge-pathnames name directory)
                                       :if-does-not-exist nil)
                 (and stream (read-line stream nil nil))))))
    (and line (string-trim '(#\Space #\Newline) line))))

(defun sample-batteries ()
  "Each battery as a plist with :NAME, :LEVEL (percent), :CHARGING-P, and
:DEVICE-P for a peripheral's, such as a gamepad's."
  (loop for directory in (ignore-errors
                          (uiop:subdirectories *power-supply-directory*))
        for type = (read-sysfs directory "type")
        for capacity = (read-sysfs directory "capacity")
        when (and (equal type "Battery") capacity (every #'digit-char-p capacity))
          collect (list :name (car (last (pathname-directory directory)))
                        :level (parse-integer capacity)
                        :charging-p (member (read-sysfs directory "status")
                                            '("Charging" "Full") :test #'equal)
                        :device-p (equal (read-sysfs directory "scope") "Device"))))

;;; Sound, from PipeWire's default sink through wpctl: "Volume: 0.45 [MUTED]".

(defun parse-wpctl-volume (line)
  "WPCTL's volume LINE as (:LEVEL percent :MUTED-P boolean), or NIL."
  (let ((start (and line (search "Volume:" line))))
    (when start
      (multiple-value-bind (value end)
          (let ((*read-default-float-format* 'double-float))
            (ignore-errors (read-from-string line t nil :start (+ start 7))))
        (declare (ignore end))
        (when (realp value)
          (list :level (round (* 100 value))
                :muted-p (and (search "[MUTED]" line) t)))))))

(defun run-for-line (program &rest arguments)
  "The first line PROGRAM prints, or NIL if it is missing or fails."
  (ignore-errors
   (let ((output (with-output-to-string (stream)
                   (let ((process (sb-ext:run-program program arguments
                                                      :search t :output stream
                                                      :error nil :input nil)))
                     (unless (zerop (sb-ext:process-exit-code process))
                       (return-from run-for-line nil))))))
     (first (uiop:split-string output :separator '(#\Newline))))))

(defun sample-volume ()
  (parse-wpctl-volume (run-for-line "wpctl" "get-volume" "@DEFAULT_AUDIO_SINK@")))

(defun change-volume (luvland how)
  "Change the default sink's volume: :MUTE toggles it, :UP and :DOWN step it."
  (let ((arguments (ecase how
                     (:mute '("set-mute" "@DEFAULT_AUDIO_SINK@" "toggle"))
                     (:up '("set-volume" "-l" "1.0" "@DEFAULT_AUDIO_SINK@" "5%+"))
                     (:down '("set-volume" "@DEFAULT_AUDIO_SINK@" "5%-")))))
    (sb-thread:make-thread
     (lambda ()
       (unless (apply #'run-for-line "wpctl" arguments)
         ;; wpctl prints nothing on success; only a missing program matters.
         (unless (run-for-line "wpctl" "status")
           (message luvland "No wpctl: volume needs PipeWire's WirePlumber.")))
       (sample-now luvland))
     :name "Luvland volume")))

;;; The network, from NetworkManager on the system bus.

(defparameter +nm+ "org.freedesktop.NetworkManager")

(defun nm-property (bus path interface name)
  (luv.dbus:get-property bus +nm+ path (format nil "~A~@[.~A~]" +nm+ interface) name))

(defun sample-network (bus)
  "The primary connection as (:KIND :WIFI|:WIRED|:OTHER :NAME name
:STRENGTH percent), (:KIND :OFFLINE), or NIL when NetworkManager is absent."
  (when bus
    (handler-case
        (let ((primary (nm-property bus "/org/freedesktop/NetworkManager" nil
                                    "PrimaryConnection")))
          (if (or (null primary) (string= primary "/"))
              (list :kind :offline)
              (let ((type (nm-property bus primary "Connection.Active" "Type"))
                    (name (nm-property bus primary "Connection.Active" "Id")))
                (cond
                  ((equal type "802-11-wireless")
                   (let* ((device (first (nm-property bus primary "Connection.Active" "Devices")))
                          (access-point (and device (nm-property bus device "Device.Wireless"
                                                                 "ActiveAccessPoint"))))
                     (list :kind :wifi :name name
                           :strength (and access-point (string/= access-point "/")
                                          (nm-property bus access-point "AccessPoint"
                                                       "Strength")))))
                  ((equal type "802-3-ethernet") (list :kind :wired :name name))
                  (t (list :kind :other :name name))))))
      (error () nil))))

(defun wireless-link-quality ()
  "Without NetworkManager: the first wireless interface's link quality in
/proc/net/wireless as a percentage of 70, the usual maximum, or NIL."
  (ignore-errors
   (with-open-file (stream "/proc/net/wireless" :if-does-not-exist nil)
     (when stream
       (read-line stream nil) (read-line stream nil)
       (let ((line (read-line stream nil)))
         (when line
           (let ((fields (remove "" (uiop:split-string line :separator '(#\Space #\Tab))
                                 :test #'string=)))
             (list :kind :wifi :name (string-right-trim ":" (first fields))
                   :strength (min 100 (round (* 100 (parse-integer (third fields)
                                                                   :junk-allowed t))
                                             70))))))))))

;;; The sampler.

(defun open-system-bus ()
  (ignore-errors (luv.dbus:open-bus :system)))

(defun sample-indicators (luvland)
  (let ((bus (luvland-system-bus luvland)))
    (when (and bus (not (luv.dbus:bus-open-p bus)))
      (setf bus nil))
    (let ((network (or (sample-network bus) (wireless-link-quality))))
      (setf (luvland-indicators luvland)
            (list :network network
                  :volume (sample-volume)
                  :batteries (sample-batteries))))))

(defun sample-now (luvland)
  "Sample the machine's state again soon rather than at the next tick."
  (setf (luvland-sample-requested-p luvland) t))

(defun start-indicators (luvland)
  (setf (luvland-indicator-thread luvland)
        (sb-thread:make-thread
         (lambda ()
           (let ((retry-at 0))
             (loop while (luvland-running-p luvland)
                   do (when (and (null (luvland-system-bus luvland))
                                 (>= (get-universal-time) retry-at))
                        ;; NetworkManager may start after Luvland does.
                        (setf (luvland-system-bus luvland) (open-system-bus)
                              retry-at (+ (get-universal-time) 30)))
                      (handler-case (sample-indicators luvland)
                        (error (condition)
                          (push (list :indicators condition) (luvland-failures luvland))))
                      (loop repeat (* 10 *indicator-seconds*)
                            until (shiftf (luvland-sample-requested-p luvland) nil)
                            while (luvland-running-p luvland)
                            do (sleep 0.1)))))
         :name "Luvland indicators")))

(defun stop-indicators (luvland)
  (let ((bus (shiftf (luvland-system-bus luvland) nil)))
    (when bus (ignore-errors (luv.dbus:close-bus bus)))))
