(in-package #:cl-user)

(defun ridge-source ()
  (luvcraft.luft:make-luvcraft-world-source
   :seed 121 :height 128 :relief :biomes
   :homeland '(:meadow-height 16 :blend 120
               :boxes ((-1059 644 -770 1053) (-769 210 -498 459) (-78 -791 445 218)))))

(defun carve-ridge (source)
  (let ((domain (luft.terrain:world-source-domain source))
        (changes nil))
    (loop for x from 780 below 864 do
      (loop for y from 1048 below 1064 do
        (loop for z from 76 below 84 do
          (push (list (luft:make-site domain x y z luft:+cell-extent+ 1) nil nil) changes))))
    (luft.terrain:record-world-events (luft.terrain:world-source-journal source) changes)))
