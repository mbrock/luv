;;;; plank's Luvland: the games the play menu offers.

;;; plank's screen runs at 1280x720 (video= in /boot/firmware/cmdline.txt),
;;; and Starfall draws a little smaller than its window, scaled up: at 5/4 it
;;; holds 58-60 fps on the Pi's GPU, against 48-60 at full size.
(setf *client-scale* 5/4)

(define-game "Starfall"
  (format nil "/usr/bin/chromium --app=http://127.0.0.1:8765/ --ozone-platform=wayland ~
               --no-first-run --password-store=basic --autoplay-policy=no-user-gesture-required ~
               --user-data-dir=~A"
          (uiop:native-namestring (merge-pathnames ".local/share/starfall/chromium/"
                                                   (user-homedir-pathname)))))

;;; Moppe is far too heavy for the Pi's GPU, so it is not offered here.

;;; Greet the sofa with the play menu.
(run-command "play")
