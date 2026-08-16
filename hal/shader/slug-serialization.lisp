;;; Dense single-glyph texture data matching the open Slug shader contract.

(in-package #:luv.slug)

(ieee-floats:make-float-converters
    encode-slug-half decode-slug-half 5 10 t)

(defconstant +slug-texture-width+ 4096)

(defstruct slug-serialized-outline
  packed-outline
  (curve-width +slug-texture-width+ :type (eql 4096))
  (band-width +slug-texture-width+ :type (eql 4096))
  curve-half-words
  curve-texel-count
  curve-upload-data
  band-uint16-words
  band-texel-count
  band-upload-data
  horizontal-band-count
  vertical-band-count)

(defun slug-half-bits (value)
  (encode-slug-half (coerce value 'single-float)))

(defun slug-half-value (value)
  (let ((decoded (decode-slug-half (slug-half-bits value))))
    (unless (realp decoded)
      (error 'slug-outline-error
             :reason :non-finite-curve-coordinate :details value))
    decoded))

(defun quantize-slug-point (point)
  (make-slug-point :x (slug-half-value (slug-point-x point))
                   :y (slug-half-value (slug-point-y point))))

(defun quantize-slug-outline (outline)
  (make-slug-outline
   :contours
   (loop for contour in (slug-outline-contours outline)
         collect
         (loop for curve in contour
               collect
               (make-slug-quadratic
                :start (quantize-slug-point (slug-quadratic-start curve))
                :control (quantize-slug-point (slug-quadratic-control curve))
                :end (quantize-slug-point (slug-quadratic-end curve)))))))

(defun make-slug-word-vector ()
  (make-array 0 :element-type '(unsigned-byte 16)
                :adjustable t :fill-pointer 0))

(defun push-slug-half-point (point words)
  (vector-push-extend (slug-half-bits (slug-point-x point)) words)
  (vector-push-extend (slug-half-bits (slug-point-y point)) words))

(defun push-slug-curve-texel (curve words)
  (push-slug-half-point (slug-quadratic-start curve) words)
  (push-slug-half-point (slug-quadratic-control curve) words))

(defun push-slug-endpoint-texel (point words)
  (push-slug-half-point point words)
  (vector-push-extend 0 words)
  (vector-push-extend 0 words))

(defun slug-texel-location (offset width)
  (values (mod offset width) (floor offset width)))

(defun ensure-slug-uint16 (value role)
  (unless (typep value '(unsigned-byte 16))
    (error 'slug-outline-error
           :reason :texture-address-overflow :details (list role value)))
  value)

(defun push-slug-band-word-pair (left right words)
  (vector-push-extend (ensure-slug-uint16 left :band-left) words)
  (vector-push-extend (ensure-slug-uint16 right :band-right) words))

(defun append-slug-band-curve-locations
    (band curve-offsets curve-width band-words)
  (dolist (curve-index (slug-band-curve-indices band))
    (multiple-value-bind (x y)
        (slug-texel-location (aref curve-offsets curve-index) curve-width)
      (push-slug-band-word-pair x y band-words))))

(defun pack-slug-uint16-words (words start count)
  (loop for index below count
        for word-index from start
        sum (ash (if (< word-index (length words))
                     (aref words word-index)
                     0)
                 (* index 16))))

(defun make-slug-upload-data (words words-per-texel width element-type)
  (let* ((texel-count (ceiling (length words) words-per-texel))
         (height (max 1 (ceiling texel-count width)))
         (data (make-array (list height width)
                           :element-type element-type
                           :initial-element 0)))
    (dotimes (texel texel-count data)
      (setf (row-major-aref data texel)
            (pack-slug-uint16-words
             words (* texel words-per-texel) words-per-texel)))))

(defun slug-serialized-outline-curve-texture-size (serialized)
  (list (slug-serialized-outline-curve-width serialized)
        (array-dimension
         (slug-serialized-outline-curve-upload-data serialized) 0)))

(defun slug-serialized-outline-band-texture-size (serialized)
  (list (slug-serialized-outline-band-width serialized)
        (array-dimension
         (slug-serialized-outline-band-upload-data serialized) 0)))

(defun serialize-slug-outline
    (outline &key horizontal-band-count vertical-band-count)
  "Quantize OUTLINE to RGBA16F, repack bands, and emit Slug's RG16U index data."
  (let* ((curve-width +slug-texture-width+)
         (band-width +slug-texture-width+)
         (quantized (quantize-slug-outline outline))
         (packed (pack-slug-outline
                  quantized
                  :horizontal-band-count horizontal-band-count
                  :vertical-band-count vertical-band-count))
         (curves (slug-packed-outline-curves packed))
         (curve-offsets (make-array (length curves)))
         (curve-words (make-slug-word-vector))
         (curve-index 0)
         (curve-texel 0))
    (dolist (contour (slug-outline-contours quantized))
      (dolist (curve contour)
        (setf (aref curve-offsets curve-index) curve-texel)
        (incf curve-index)
        (incf curve-texel)
        (push-slug-curve-texel curve curve-words))
      (push-slug-endpoint-texel
       (slug-quadratic-end (car (last contour))) curve-words)
      (incf curve-texel))
    (multiple-value-bind (curve-x curve-y)
        (slug-texel-location (max 0 (1- curve-texel)) curve-width)
      (declare (ignore curve-x))
      (ensure-slug-uint16 curve-y :curve-row))
    (let* ((horizontal (slug-packed-outline-horizontal-bands packed))
           (vertical (slug-packed-outline-vertical-bands packed))
           (bands (append horizontal vertical))
           (band-words (make-slug-word-vector))
           (header-count (length bands)))
      ;; Reserve every header so its offset can point past the complete header block.
      (loop repeat header-count
            do (push-slug-band-word-pair 0 0 band-words))
      (loop for band in bands
            for header from 0
            for offset = (/ (length band-words) 2)
            do (setf (aref band-words (* header 2))
                     (ensure-slug-uint16
                      (length (slug-band-curve-indices band)) :curve-count)
                     (aref band-words (1+ (* header 2)))
                     (ensure-slug-uint16 offset :curve-list-offset))
               (append-slug-band-curve-locations
                band curve-offsets curve-width band-words))
      (multiple-value-bind (band-x band-y)
          (slug-texel-location
           (max 0 (1- (/ (length band-words) 2))) band-width)
        (declare (ignore band-x))
        (ensure-slug-uint16 band-y :band-row))
      (make-slug-serialized-outline
       :packed-outline packed
       :curve-width curve-width :band-width band-width
       :curve-half-words curve-words :curve-texel-count curve-texel
       :curve-upload-data
       (make-slug-upload-data
        curve-words 4 curve-width '(unsigned-byte 64))
       :band-uint16-words band-words
       :band-texel-count (/ (length band-words) 2)
       :band-upload-data
       (make-slug-upload-data
        band-words 2 band-width '(unsigned-byte 32))
       :horizontal-band-count (length horizontal)
       :vertical-band-count (length vertical)))))
