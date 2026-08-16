(in-package #:asdf-user)

(defsystem "luv-wiki-site"
  :description "The Org corpus and rendered static site for the luv workshop wiki."
  :version "0.0.1"
  :author "Mikael Brockman"
  :defsystem-depends-on ("luv-wiki")
  :depends-on ("luv-wiki")
  :build-operation "luv.wiki:render-op"
  :components
  ((:module "wiki"
    :default-component-class "luv.wiki:org-file"
    :components ((:file "index")
                 (:file "block-world")
                 (:file "world-terminal")
                 (:file "box3d-architecture")
                 (:file "domains-and-bundles")
                 (:file "field-notes-measures")
                 (:file "field-notes-mp-units")
                 (:file "gpu-architecture")
                 (:file "frame-performance")
                 (:file "mathematical-shaders")
                 (:file "slug-bezier")
                 (:file "metal-backend")
                 (:file "moppe-legacy")
                 (:file "objective-c-and-metal")
                 (:file "physics-and-simd")
                 (:file "quantities-and-measurement")
                 (:file "sb-simd")
                 (:file "sky-and-light")
                 (:file "voxel-fields-and-windows")
                 (:file "wiki-site")
                 (:static-file "site.js")
                 (:static-file "images/dexp.png")
                 (:static-file "images/slug-bezier-proof.png")
                 (:static-file "images/slug-text-proof.png")
                 (:static-file "images/slug-world-text.png")))))
