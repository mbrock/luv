(defpackage #:luvcraft.web
  (:use #:cl #:parenscript)
  (:local-nicknames (#:shader #:luv.shader)
                    (#:wgsl #:luv.wgsl)
                    (#:ps #:parenscript)
                    (#:css #:luv.css))
  (:export #:web-body-type
           #:web-body-id
           #:web-body-label
           #:web-body-role
           #:web-body-knob-group
           #:web-body-center-height
           #:web-body-radius
           #:web-body-types
           #:body-gallery-bundle
           #:body-gallery-bundle-body
           #:body-gallery-bundle-vertex
           #:body-gallery-bundle-fragment
           #:compile-body-gallery
           #:body-gallery-json
           #:body-gallery-javascript
           #:showcase-page
           #:showcase-page-directory
           #:make-showcase-page))
