(defpackage #:luft.atlas.tests
  (:use #:cl)
  (:import-from #:parachute #:define-test #:true #:false)
  (:local-nicknames (#:atlas #:luft.atlas)))

(in-package #:luft.atlas.tests)

(define-test atlas-contains-every-production-star
  (let* ((form (atlas::star-atlas-data-form))
         (stars (rest form))
         (empty-star (rest (first stars)))
         (full-star (rest (car (last stars)))))
    (true (eq 'parenscript:array (first form)))
    (true (= 256 (length stars)))
    (true (zerop (getf empty-star :mask)))
    (true (= 255 (getf full-star :mask)))
    ;; Empty families are still ParenScript array literals rather than NIL.
    (true (equal '(parenscript:array) (getf empty-star :faces)))
    (true (equal '(parenscript:array) (getf empty-star :bands)))
    (true (equal '(parenscript:array) (getf empty-star :junctions)))))

(define-test atlas-condenses-stars-into-rotation-families
  (let* ((classes (atlas::star-rotation-classes))
         (stars (loop for class in classes append class)))
    (true (= 23 (length classes)))
    (true (= 256 (length stars)))
    (true (= 256 (length (remove-duplicates stars))))
    (true (every (lambda (class) (equal class (sort (copy-list class) #'<)))
                 classes))
    (true (equal '(#x00 #xff #x01 #x7f #x03 #x3f #x06 #x6f
                   #x07 #x1f #x0f #x16 #x6b #x17 #x18 #x7e
                   #x19 #x3d #x1b #x1d #x1e #x3c #x69)
                 (mapcar #'first classes)))
    ;; Reflections are intentionally not folded into the atlas: these are the
    ;; two representatives of the sole chiral pair.
    (true (find #x1b classes :key #'first))
    (true (find #x1d classes :key #'first))))

(define-test atlas-browser-program-is-parenscript
  (let ((javascript (atlas:star-atlas-javascript)))
    (true (plusp (length javascript)))
    (true (search "document.getElementById" javascript))
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
    (true (search "@media screen and (max-width: 52rem)" stylesheet))))

(define-test atlas-is-a-wiki-view
  (let* ((site (luv.wiki:make-site nil))
         (html
           (with-output-to-string (stream)
             (luv.wiki::call-with-html-output
              stream (lambda () (atlas::render-star-atlas site))))))
    (true (search "<header class=library>" html))
    (true (search "<div class=status>" html))
    (true (search "<link rel=stylesheet href=style.css>" html))
    (true (search "<div class=atlas-layout id=luft-star-atlas>" html))
    (true (search "aria-label=\"23 proper-rotation families\"" html))
    (true (search "<article class=star-family" html))
    (true (search "<details class=family-orbit>" html))
    (true (search "<footer class=site-footer>" html))
    (false (search "<style" html))
    (false (search "application/json" html))))
