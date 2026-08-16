;;; PNG output for renderer readbacks.

(in-package #:luv)

(defun rgba-png-pixels (pixels width height format)
  (unless (= (length pixels) (* width height 4))
    (error "Expected ~D pixel bytes, got ~D."
           (* width height 4) (length pixels)))
  (ecase format
    ((:rgba8-unorm :rgba8-unorm-srgb)
     pixels)
    ((:bgra8-unorm :bgra8-unorm-srgb)
     (let ((rgba (copy-seq pixels)))
       (loop for offset below (length rgba) by 4
             do (rotatef (aref rgba offset)
                         (aref rgba (+ offset 2))))
       rgba))))

(defun write-rgba-png (pathname pixels width height format)
  "Write tightly packed RGBA or BGRA byte PIXELS to PATHNAME as a PNG."
  (zpng:write-png
   (make-instance 'zpng:png
                  :width width
                  :height height
                  :color-type :truecolor-alpha
                  :image-data (rgba-png-pixels pixels width height format))
   pathname
   :if-exists :supersede)
  pathname)
