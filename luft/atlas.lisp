(in-package #:luft.atlas)

(defun triangle-counts (mask)
  (let ((geometry (luft:star-triangles mask)))
    (list (length (getf geometry :faces))
          (length (getf geometry :bands))
          (length (getf geometry :junctions)))))

(defun render-occupancy-layer (samples label)
  (spinneret:with-html
    (:div.occupancy-layer :aria-label label
      (dolist (sample samples)
        (:span.occupancy-cell :data-sample sample)))))

(defun render-star-card (mask)
  (destructuring-bind (faces bands junctions) (triangle-counts mask)
    (spinneret:with-html
      (:button.star-card :type "button" :data-mask mask
                         :aria-label (format nil "Star #x~2,'0X" mask)
        (:canvas)
        (:span.card-caption
          (:span.card-mask (format nil "#x~2,'0X" mask))
          (:span.card-counts
            (format nil "~D · ~D · ~D" faces bands junctions)))))))

(defun render-star-atlas (site)
  "Render the atlas as a built-in wiki view, with only its body being special."
  (let ((luv.wiki::*site* site)
        (luv.wiki::*rendering-document* nil)
        (luv.wiki::*page-prefix* "")
        (luv.wiki::*page-kind* "page"))
    (luv.wiki::render-page-frame
     "The 256 stars"
     (lambda ()
       (spinneret:with-html
        (:h1 "The 256 stars")
        (:p.lede
          "Every arrangement of the eight cells around a lattice point, resolved by the production mesher into face, band, and junction triangles.")
        (:div#luft-star-atlas.atlas-layout
          (:section.detail :aria-label "Selected star"
            (:div.detail-heading
              (:p.mask (:span#selected-mask "#x08"))
              (:span#selected-bits.bits "00001000"))
            (:canvas#selected-canvas
              :aria-label "Rotatable triangle geometry of the selected star")
            (:div.detail-controls
              (:div.layers
                (:label (:input#show-faces :type "checkbox" :checked t)
                        (:span.swatch.face-swatch) "Faces")
                (:label (:input#show-bands :type "checkbox" :checked t)
                        (:span.swatch.band-swatch) "Bands")
                (:label (:input#show-junctions :type "checkbox" :checked t)
                        (:span.swatch.junction-swatch) "Junctions"))
              (:div.facts
                (:div.occupancy
                  (render-occupancy-layer '(0 1 2 3) "Low Z cells")
                  (render-occupancy-layer '(4 5 6 7) "High Z cells"))
                (:div)
                (:span#face-count.fact "0" (:small "faces"))
                (:span#band-count.fact "0" (:small "bands"))
                (:span#junction-count.fact "0" (:small "junctions")))
              (:div.stepper
                (:button#previous-star :type "button" "Previous")
                (:button#next-star :type "button" "Next"))))
          (:section.atlas-grid :aria-label "All 256 occupancy stars"
            (dotimes (mask 256)
              (render-star-card mask))))
        (:p.atlas-note
          "Drag the large view to orbit; scroll over it to approach. Triangle coordinates come directly from "
          (:code "LUFT:STAR-TRIANGLES") ".")
        (:script :src "luft-star-atlas.js" :defer t)))
     :body-class "wide atlas-page"
     :kind "page"
     :crumbs '(("Pages" . "pages.html") ("The 256 stars"))
     :right "star atlas")))

(defun write-star-atlas (site directory)
  "Write the wiki-framed atlas and its ParenScript program into DIRECTORY."
  (let* ((directory (uiop:ensure-directory-pathname directory))
         (html (merge-pathnames "luft-star-atlas.html" directory))
         (javascript (merge-pathnames "luft-star-atlas.js" directory)))
    (luv.wiki::write-html-file html (lambda () (render-star-atlas site)))
    (with-open-file (stream javascript :direction :output :if-exists :supersede
                                       :external-format :utf-8)
      (write-string (star-atlas-javascript) stream))
    (list html javascript)))
