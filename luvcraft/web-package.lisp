(defpackage #:luvcraft.web
  (:use #:cl)
  (:local-nicknames (#:shader #:luv.shader)
                    (#:wgsl #:luv.wgsl))
  (:export #:web-response
           #:web-response-status
           #:web-response-content-type
           #:web-response-body
           #:web-page
           #:web-page-path
           #:web-page-label
           #:web-page-description
           #:web-application
           #:web-application-pages
           #:make-web-application
           #:respond-to-web-request
           #:serve-web-application
           #:web-body-type
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
           #:showcase-page
           #:showcase-page-directory
           #:make-showcase-page
           #:make-luvcraft-web-application
           #:serve-luvcraft-web))
