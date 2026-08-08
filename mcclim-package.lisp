;;; McCLIM names the logical display connection a port and the relationship
;;; between a sheet and a presentation target a mirror.  Luv's canvas is one
;;; possible presentation target; a texture displayed on a 3D surface will be
;;; another.  Keep those concepts separate even in this first native-window
;;; implementation.

(defpackage #:luv.mcclim
  (:use #:clim-lisp #:clim #:clime #:climb)
  (:local-nicknames (#:luv #:luv))
  (:export #:luv-port
           #:luv-graft
           #:luv-mirror
           #:luv-medium
           #:mirror-sheet
           #:mirror-target
           #:port-mirrors))

(in-package #:luv.mcclim)
