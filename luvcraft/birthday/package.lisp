;;; A birthday party in the little world.
;;;
;;; This system decorates luvcraft for a celebration: a meadow world of its
;;; own, a gazebo built from blocks, balloons and dancing gnomes drawn as
;;; signed distance fields, and a fireworks show at dusk.  Each file owns one
;;; aspect and exposes a small constructor; PARTY.LISP is the only file that
;;; knows how they compose.
;;;
;;; Shader code for these scene objects lives in sections of the same files
;;; under (in-package #:luvcraft.shaders), beside the gnome's SDF vocabulary.

(defpackage #:luvcraft.birthday
  (:use #:cl)
  (:local-nicknames (#:luv #:luv)
                    (#:luvcraft #:luvcraft))
  (:export
   ;; world.lisp
   #:birthday-world-source
   #:make-birthday-world
   ;; gazebo.lisp
   #:build-birthday-gazebo
   ;; balloons.lisp
   #:add-birthday-balloons
   #:remove-birthday-balloons
   ;; gnomes.lisp
   #:add-dancing-gnomes
   #:remove-dancing-gnomes
   ;; fireworks.lisp
   #:add-birthday-fireworks
   #:stop-birthday-fireworks
   ;; marquee.lisp
   #:add-birthday-marquee
   #:remove-birthday-marquee
   ;; party.lisp
   #:celebrate-birthday))
