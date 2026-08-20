;;;; Render the project's films: headless MP4s from luft and luvcraft.
;;;;
;;;; Usage: sbcl --script scripts/film.lisp [TARGET-DIRECTORY] [FILM...]
;;;; FILM names limit the run.  The incremental construction films are
;;;; explicit-only: luft-rise, luft-spiral, and luft-carve.

(require :asdf)

(defun film-project-root ()
  (merge-pathnames #P"../"
                   (uiop:pathname-directory-pathname *load-truename*)))

(dolist (system '("luv.asd" "luvcraft.asd" "telegram.asd" "mqtt.asd"
                  "openai.asd" "luft.asd"))
  (asdf:load-asd (merge-pathnames system (film-project-root))))
(asdf:load-system :luvcraft)
(asdf:load-system :luvcraft/birthday)
(asdf:load-system :luft/render)

(defun film-target-and-names ()
  (let* ((arguments (uiop:command-line-arguments))
         (directory (uiop:ensure-directory-pathname
                     (or (first arguments)
                         (merge-pathnames #P"build/films/"
                                          (film-project-root)))))
         (names (mapcar (lambda (name) (intern (string-upcase name) :keyword))
                        (rest arguments))))
    (values directory names)))

(defun film-wanted-p (name names)
  (or (null names) (member name names)))

(luv:call-with-sdl-main-thread
 (lambda ()
   (multiple-value-bind (directory names) (film-target-and-names)
     (ensure-directories-exist directory)
     (when (film-wanted-p :luft-orbit names)
       (format t "~&Filming the luft studio orbit (stock, TAA)...~%")
       (luft.render:film-studio-orbit
        (merge-pathnames #P"luft-orbit.mp4" directory)
        :seconds 12 :style :stock))
     (when (film-wanted-p :luft-styles names)
       (format t "~&Filming the luft style tour...~%")
       (luft.render:film-studio-orbit
        (merge-pathnames #P"luft-styles.mp4" directory)
        :seconds 16
        :styles '(:flat :bevel :chamfer :paper :stock :field :soft :ink)))
     (when (film-wanted-p :luft-flight names)
       (format t "~&Filming the atelier drone flight...~%")
       (luft.render:film-atelier-flight
        (merge-pathnames #P"luft-flight.mp4" directory)))
     (when (film-wanted-p :luft-flight-vertical names)
       (format t "~&Filming the atelier drone flight, portrait...~%")
       (luft.render:film-atelier-flight
        (merge-pathnames #P"luft-flight-vertical.mp4" directory)
        :width 720 :height 1280 :field-scale 1.25))
     (when (film-wanted-p :luft-clay-breath names)
       (format t "~&Filming the clay world breathing...~%")
       (luft.render:film-clay-breath
        (merge-pathnames #P"luft-clay-breath.mp4" directory)))
     (when (member :luft-rise names)
       (format t "~&Filming the Luft holm rising from its cell chain...~%")
       (luft.render:film-atelier-construction
        (merge-pathnames #P"luft-holm-rise.mp4" directory)
        :piece :holm :mode :rise))
     (when (member :luft-spiral names)
       (format t "~&Filming the Luft holm winding into place...~%")
       (luft.render:film-atelier-construction
        (merge-pathnames #P"luft-holm-spiral.mp4" directory)
        :piece :holm :mode :spiral))
     (when (member :luft-carve names)
       (format t "~&Filming the Luft holm carved from a granite chunk...~%")
       (luft.render:film-atelier-construction
        (merge-pathnames #P"luft-holm-carve.mp4" directory)
        :piece :holm :mode :carve))
     (when (film-wanted-p :birthday names)
       (format t "~&Filming the birthday party cutscenes...~%")
       (luvcraft.birthday:film-birthday-cutscenes
        (merge-pathnames #P"birthday.mp4" directory)))
     (when (film-wanted-p :birthday-daniel names)
       (format t "~&Filming Daniel's birthday party cutscenes...~%")
       (luvcraft.birthday:film-birthday-cutscenes
        (merge-pathnames #P"birthday-daniel.mp4" directory)
        :name "DANIEL"
        :age (luvcraft.birthday::birthday-age 1985 8 15)))
     (format t "~&Films written under ~A~%" directory))))
