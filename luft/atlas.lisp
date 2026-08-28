(in-package #:luft.atlas)

(defun star-symmetry-classes ()
  "Order full-symmetry orbits by population, with complements kept together."
  (mapcan #'copy-list
          (sort (%complement-paired-classes (%all-star-symmetry-classes))
                #'family-pair<)))

(defun %all-star-symmetry-classes ()
  (sort
   (loop with unseen = (loop for mask below 256 collect mask)
         while unseen
         for class = (gray-star-orbit (reduce #'min unseen))
         do (setf unseen (set-difference unseen class))
         collect class)
   #'<
   :key #'first))

(defun %complement-paired-classes (classes)
  (loop with remaining = classes
        while remaining
        for class = (first remaining)
        for complement = (%complementary-symmetry-class class remaining)
        do (setf remaining
                 (remove complement (rest remaining) :test #'eq))
        collect (if (eq class complement)
                    (list class)
                    (list class complement))))

(defun family-pair< (left right)
  (let ((left-count (logcount (first (first left))))
        (right-count (logcount (first (first right)))))
    (if (= left-count right-count)
        (< (first (first left)) (first (first right)))
        (< left-count right-count))))

(defun %complementary-symmetry-class (class classes)
  (let ((representative
          (luft:star-canonical-form (logxor #xff (first class))
                                    :reflections t)))
    (find representative classes :key #'first)))

(defun render-occupancy-layer (samples label)
  (spinneret:with-html
    (:div.occupancy-layer :aria-label label
      (dolist (sample samples)
        (:span.occupancy-cell :data-sample sample)))))

(defun render-star-choice (mask representative)
  (spinneret:with-html
    (:button.star-choice.star-member :type "button"
                                     :data-mask mask
                                     :data-class representative
                                     :data-view "owned"
                                     :aria-label (format nil "Star #x~2,'0X" mask)
      (format nil "~2,'0X" mask))))

(defun render-star-family (class)
  (let ((representative (first class)))
    (spinneret:with-html
      (:article.star-family :data-representative representative
                            :aria-label
                            (format nil "Symmetry family #x~2,'0X"
                                    representative)
        (:button.star-choice.star-card :type "button"
                                       :data-mask representative
                                       :data-class representative
                                       :data-view "surface"
                                       :aria-label
                                       (format nil "Star #x~2,'0X"
                                               representative)
          (:canvas)
          (:span.card-caption
            (:span.card-mask (format nil "#x~2,'0X" representative))
            (:span.card-orbit
              (format nil "~D orientation~:P" (length class)))))
        (:details.family-orbit
          (:summary "Choose orientation")
          (:div.family-members
            (dolist (mask class)
              (render-star-choice mask representative))))))))

(defun render-star-atlas (site)
  "Render the atlas as a built-in wiki view, with only its body being special."
  (let ((luv.wiki::*site* site)
        (luv.wiki::*rendering-document* nil)
        (luv.wiki::*page-prefix* "")
        (luv.wiki::*page-kind* "page"))
    (luv.wiki::render-page-frame
     "The stars, by symmetry"
     (lambda ()
       (spinneret:with-html
        (:h1 "The stars, by symmetry")
        (:p.lede
          "The 256 arrangements of eight cells become 22 families under the full cubical symmetry group, reflections included. Each family shows the complete local surface; choose an orientation to see which triangles that lattice point actually owns.")
        (:div#luft-star-atlas.atlas-layout
          (:section.detail :aria-label "Selected star"
            (:div.detail-heading
              (:div
                (:p.mask (:span#selected-mask "#x08"))
                (:span#selected-view.view-label "Whole local patch"))
              (:span#selected-bits.bits "00001000"))
            (:canvas#selected-canvas
              :aria-label "Rotatable fixed-frame geometry and orientation axes of the selected star")
            (:div.detail-controls
              (:fieldset.view-modes
                (:legend "Mesh view")
                (:label
                  (:input#view-surface :type "radio" :name "mesh-view"
                                       :value "surface" :checked t)
                  (:span "Whole patch"))
                (:label
                  (:input#view-owned :type "radio" :name "mesh-view"
                                     :value "owned")
                  (:span "Owned orientation")))
              (:p#view-explanation.view-explanation
                "Every face and band touching the center, with ownership forgotten.")
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
                (:button#previous-star :type "button" "Previous orientation")
                (:button#next-star :type "button" "Next orientation"))))
          (:section.atlas-grid :aria-label "22 symmetry families"
            (dolist (class (star-symmetry-classes))
              (render-star-family class))))
        (:p.atlas-note
          "Drag the large view to orbit; scroll over it to approach. Previous and next step through the selected symmetry family while its occupancy stays in a fixed frame; the XYZ triad carries the changing orientation. Family meshes are the symmetry ownership closure from "
          (:code "LUFT:STAR-LOCAL-SURFACE-TRIANGLES")
          "; orientation buttons reveal the corresponding production packet from "
          (:code "LUFT:STAR-TRIANGLES") ".")
        (:script :src "luft-star-atlas.js" :defer t)))
     :body-class "wide atlas-page"
     :kind "page"
     :crumbs '(("Pages" . "pages.html") ("The stars, by symmetry"))
     :right "star atlas")))

(defun star-atlas-resources (site)
  "The atlas page and browser program in the wiki's common resource model."
  (list
   (luv.wiki:make-generated-resource
    "/luft-star-atlas.html" "luft-star-atlas.html" "text/html; charset=utf-8"
    (lambda ()
      (with-output-to-string (stream)
        (luv.wiki::call-with-html-output
         stream (lambda () (render-star-atlas site)))))
    :label "Stars"
    :description "22 symmetry families of 256 stars"
    :kind "atlas")
   (luv.wiki:make-generated-resource
    "/luft-star-atlas.js" "luft-star-atlas.js"
    "text/javascript; charset=utf-8" #'star-atlas-javascript)))

(luv.wiki:register-resource-provider 'star-atlas #'star-atlas-resources)
