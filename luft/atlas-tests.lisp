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
    (true (search "<footer class=site-footer>" html))
    (false (search "<style" html))
    (false (search "application/json" html))))
