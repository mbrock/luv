;;; TrueType outlines enter Slug before McCLIM's software rasterizer.

(in-package #:luv.slug)

(defstruct slug-glyph
  character outline advance-width left-side-bearing units-per-em)

(defun slug-point-from-zpb (point)
  (make-slug-point :x (zpb-ttf:x point) :y (zpb-ttf:y point)))

(defun slug-outline-from-zpb-glyph (glyph)
  "Convert ZPB-TTF's resolved line/quadratic segments to a Slug outline."
  (let (contours)
    (zpb-ttf:do-contours (contour glyph)
      (let (curves)
        (zpb-ttf:do-contour-segments (start control end) contour
          (let ((slug-start (slug-point-from-zpb start))
                (slug-end (slug-point-from-zpb end)))
            (push (if control
                      (make-slug-quadratic
                       :start slug-start
                       :control (slug-point-from-zpb control)
                       :end slug-end)
                      (make-slug-line slug-start slug-end))
                  curves)))
        (push (nreverse curves) contours)))
    (make-slug-outline :contours (nreverse contours))))

(defun make-slug-glyph-from-zpb-glyph
    (glyph font-loader &key character)
  "Capture one ZPB-TTF GLYPH's outline and placement metrics for Slug."
  (make-slug-glyph
   :character character
   :outline (slug-outline-from-zpb-glyph glyph)
   :advance-width (zpb-ttf:advance-width glyph)
   :left-side-bearing (zpb-ttf:left-side-bearing glyph)
   :units-per-em (zpb-ttf:units/em font-loader)))

(defun load-slug-glyph (character font-loader)
  "Find CHARACTER in FONT-LOADER and capture its Slug outline and metrics."
  (make-slug-glyph-from-zpb-glyph
   (zpb-ttf:find-glyph character font-loader)
   font-loader
   :character character))

(defun map-slug-outline-points (function outline)
  "Return an OUTLINE whose points are the result of calling FUNCTION on x/y."
  (labels ((map-point (point)
             (multiple-value-bind (x y)
                 (funcall function (slug-point-x point) (slug-point-y point))
               (make-slug-point :x x :y y)))
           (map-curve (curve)
             (make-slug-quadratic
              :start (map-point (slug-quadratic-start curve))
              :control (map-point (slug-quadratic-control curve))
              :end (map-point (slug-quadratic-end curve)))))
    (make-slug-outline
     :contours
     (loop for contour in (slug-outline-contours outline)
           collect (mapcar #'map-curve contour)))))

(defun normalize-slug-glyph-outline (glyph)
  "Return GLYPH's outline in em units, preserving its TrueType origin."
  (let ((units-per-em (slug-glyph-units-per-em glyph)))
    (unless (plusp units-per-em)
      (error 'slug-outline-error
             :reason :invalid-units-per-em :details units-per-em))
    (map-slug-outline-points
     (lambda (x y)
       (values (/ x units-per-em) (/ y units-per-em)))
     (slug-glyph-outline glyph))))
