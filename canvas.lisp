(in-package #:luv)

(defclass canvas () ())

(defclass canvas-context () 
  (canvas))

(defgeneric get-current-texture (canvas-context)
  (:documentation "Get the handle to a GPU texture that this context will
present on its canvas as its next frame."))

(defclass sdl-canvas (canvas)
  ())
