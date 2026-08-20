(defpackage #:luvcraft.body-gallery
  (:use #:cl)
  (:local-nicknames (#:shader #:luv.shader)
                    (#:wgsl #:luv.wgsl))
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
           #:serve-body-gallery))
