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
    (true (equal '(parenscript:array) (getf empty-surface :junctions)))))

(define-test atlas-condenses-stars-into-symmetry-families
  (let* ((classes (atlas::star-symmetry-classes))
         (stars (loop for class in classes append class)))
    (true (= 22 (length classes)))
    (true (= 256 (length stars)))
    (true (= 256 (length (remove-duplicates stars))))
    (true (every (lambda (class) (equal class (sort (copy-list class) #'<)))
                 classes))
    (true (equal '(#x00 #xff #x01 #x7f #x03 #x3f #x06 #x6f
                   #x07 #x1f #x0f #x16 #x6b #x17 #x18 #x7e
                   #x19 #x3d #x1b #x1e #x3c #x69)
                 (mapcar #'first classes)))
    ;; Reflections fold the sole chiral pair into one family.
    (true (find #x1b classes :key #'first))
    (false (find #x1d classes :key #'first))
    (true (member #x1d (find #x1b classes :key #'first)))))

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
    (true (search "var families" javascript))
    (true (search "stepOrientation(-1)" javascript))
    (true (search "stepOrientation(1)" javascript))
    (false (search "JSON.parse" javascript))
    (false (search "luft-star-data" javascript))))

(define-test atlas-stylesheet-is-a-luv-css-tree
  (let ((stylesheet
          (luv.css:css-text (luv.css:find-style 'luft.atlas::star-atlas))))
    (true (search ".atlas-layout {" stylesheet))
    (true (search "grid-template-columns: minmax(20rem, 31rem) minmax(0, 1fr);"
                  stylesheet))
    (true (search ".star-family {" stylesheet))
    (true (search ".family-members {" stylesheet))
    (true (search ".view-modes {" stylesheet))
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
    (true (search ">Previous orientation</button>" html))
    (true (search ">Next orientation</button>" html))
    (true (search "<footer class=site-footer>" html))
    (false (search "<style" html))
    (false (search "application/json" html))))
