(defpackage #:luft.atlas.tests
  (:use #:cl)
  (:import-from #:parachute #:define-test #:true #:false)
  (:local-nicknames (#:atlas #:luft.atlas)))

(in-package #:luft.atlas.tests)

(define-test atlas-contains-every-production-star
  (let* ((form (atlas::star-atlas-data-form))
         (stars (rest form))
         (empty-star (rest (first stars)))
         (full-star (rest (car (last stars))))
         (empty-owned (rest (getf empty-star :owned)))
         (empty-surface (rest (getf empty-star :surface))))
    (true (eq 'parenscript:array (first form)))
    (true (= 256 (length stars)))
    (true (zerop (getf empty-star :mask)))
    (true (= 255 (getf full-star :mask)))
    ;; Empty families are still ParenScript array literals rather than NIL.
    (true (equal '(parenscript:array) (getf empty-owned :faces)))
    (true (equal '(parenscript:array) (getf empty-owned :bands)))
    (true (equal '(parenscript:array) (getf empty-owned :junctions)))
    (true (equal '(parenscript:array) (getf empty-surface :faces)))
    (true (equal '(parenscript:array) (getf empty-surface :bands)))
    (true (equal '(parenscript:array) (getf empty-surface :junctions)))
    (dolist (star stars)
      (dolist (view (list (rest (getf (rest star) :owned))
                          (rest (getf (rest star) :surface))))
        (true (every (lambda (polygon) (= 5 (length polygon)))
                     (append (rest (getf view :faces))
                             (rest (getf view :bands)))))
        (true (every (lambda (polygon) (= 4 (length polygon)))
                     (rest (getf view :junctions))))))))

(define-test atlas-condenses-stars-into-symmetry-families
  (let* ((classes (atlas::star-symmetry-classes))
         (stars (loop for class in classes append class)))
    (true (= 22 (length classes)))
    (true (= 256 (length stars)))
    (true (= 256 (length (remove-duplicates stars))))
    (true (every (lambda (class)
                   (equal class (atlas::gray-star-orbit (first class))))
                 classes))
    (true (equal '(#x00 #xff #x01 #x7f #x03 #x3f #x06 #x6f #x18 #x7e
                   #x07 #x1f #x16 #x6b #x19 #x3d
                   #x0f #x17 #x1b #x1e #x3c #x69)
                 (mapcar #'first classes)))
    ;; Reflections fold the sole chiral pair into one family.
    (true (find #x1b classes :key #'first))
    (false (find #x1d classes :key #'first))
    (true (member #x1d (find #x1b classes :key #'first)))))

(define-test atlas-orientations-take-simple-group-steps
  (let ((mirror '((-1 0 0) (0 1 0) (0 0 1))))
    (dolist (class (atlas::star-symmetry-classes))
      (let ((mirror-count 0))
        (loop for current on (atlas::star-orientation-walk (first class))
              while (rest current)
              for current-transformation = (rest (first current))
              for next-transformation = (rest (second current))
              for relative =
                (atlas::matrix-product
                 next-transformation
                 (apply #'mapcar #'list current-transformation))
              do (if (equal relative mirror)
                     (incf mirror-count)
                     (true (member relative (atlas::quarter-turns)
                                   :test #'equal))))
        (true (<= mirror-count 1))))))

(define-test atlas-normalizes-each-orientation-into-its-family-frame
  (loop for mask below 256
        do (multiple-value-bind (representative display-transformation)
               (atlas::star-display-frame mask)
             (true (= representative
                      (luft:transform-star display-transformation mask)))))
  (multiple-value-bind (representative display-transformation)
      (atlas::star-display-frame #x01)
    (true (= #x01 representative))
    (true (equal '((1 0 0) (0 1 0) (0 0 1)) display-transformation))))

(define-test atlas-browser-program-is-parenscript
  (let ((javascript (atlas:star-atlas-javascript)))
    (true (plusp (length javascript)))
    (true (search "document.getElementById" javascript))
    (true (search "star.surface" javascript))
    (true (search "star.owned" javascript))
    (true (search "star.representative" javascript))
    (true (search "drawOrientationAxes" javascript))
    (true (search "polygon.backface" javascript))
    (true (search "shadeColor" javascript))
    (true (search "pointOccludedP" javascript))
    (false (search "drawStarFrame" javascript))
    (true (search "var families" javascript))
    (true (search "stepOrientation(-1)" javascript))
    (true (search "stepOrientation(1)" javascript))
    (false (search "JSON.parse" javascript))
    (false (search "luft-star-data" javascript))))

(define-test typst-star-data-comes-from-production-geometry
  (let ((source (atlas::typst-star-data-source #x1b)))
    (true (search "#let star-x1b" source))
    (true (search "bits: \"00011011\"" source))
    (true (search "occupied: (0, 1, 3, 4," source))
    (true (= 18 (length (atlas::occupancy-boundary-quads #x1b))))
    (true (= 6 (length (atlas::triangle-pairs-as-quads
                         (getf (luft:star-triangles #x1b) :faces)))))
    (true (= 1 (length (atlas::triangle-pairs-as-quads
                         (getf (atlas::star-owned-geometry #x1b) :faces)))))
    (true (= 8 (length (getf (atlas::star-owned-geometry #x1b) :junctions))))))

(define-test star-plate-is-a-renderable-production-figure
  (let ((figure (atlas:star-plate #x1b)))
    (true (typep figure 'luv.wiki:visual-figure))
    (true (search "#let star-x1b"
                  (with-output-to-string (stream)
                    (luv.wiki:write-figure-typst figure stream))))
    (true (search "Star #x1B" (luv.wiki:figure-alt-text figure)))))

(define-test region-plate-composes-the-production-owned-mesh
  (let* ((cells '((0 0 0) (1 0 0) (0 1 0)))
         (canonical (atlas::canonical-region-cells cells))
         (records (atlas::region-site-records canonical))
         (triangles (loop for record in records append (getf record :triangles)))
         (production (luft:star-surface-triangles (atlas::coordinate-set cells)))
         (source (atlas::typst-region-data-source canonical)))
    (true (= 16 (length records)))
    (true (= 136 (length triangles)))
    (true (null (set-difference triangles production :test #'equal)))
    (true (null (set-difference production triangles :test #'equal)))
    (true (string= source
                   (atlas::typst-region-data-source
                    (atlas::canonical-region-cells (reverse cells)))))
    (true (search "triangle-count: 136" source))
    (true (typep (atlas:region-plate cells) 'luv.wiki:visual-figure))))

(define-test atlas-stylesheet-is-a-luv-css-tree
  (let ((stylesheet
          (luv.css:css-text (luv.css:find-style 'atlas::star-atlas))))
    (true (search ".atlas-layout {" stylesheet))
    (true (search "grid-template-columns: minmax(20rem, 31rem) minmax(0, 1fr);"
                  stylesheet))
    (true (search ".star-family {" stylesheet))
    (true (search ".family-members {" stylesheet))
    (true (search ".view-modes {" stylesheet))
    (true (search ".cell-key {" stylesheet))
    (true (search "@media screen and (max-width: 52rem)" stylesheet))))

(define-test atlas-is-a-wiki-view
  (let* ((site (luv.wiki:make-site nil))
         (page (luv.wiki:find-resource "/luft-star-atlas.html" site))
         (program (luv.wiki:find-resource "/luft-star-atlas.js" site))
         (html
           (first (third (luv.wiki:resource-response page)))))
    (true page)
    (true program)
    (true (string= "luft-star-atlas.html"
                   (luv.wiki:resource-output-path page)))
    (true (search "<header class=library>" html))
    (true (search "<div class=status>" html))
    (true (search "<link rel=stylesheet href=style.css>" html))
    (true (search "<div class=atlas-layout id=luft-star-atlas>" html))
    (true (search "aria-label=\"22 symmetry families\"" html))
    (true (search "<article class=star-family" html))
    (true (search "<details class=family-orbit>" html))
    (true (search "name=mesh-view" html))
    (true (search "data-view=owned" html))
    (true (search "<i class=solid-cell></i>solid" html))
    (true (search "<i class=air-cell></i>air" html))
    (true (search ">Previous orientation</button>" html))
    (true (search ">Next orientation</button>" html))
    (true (search "<footer class=site-footer>" html))
    (false (search "<style" html))
    (false (search "application/json" html))))
