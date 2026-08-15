(asdf:defsystem #:luv-wiki
  :description "An Org-subset reader, Spinneret site renderer, and the ASDF
component and operation that make the wiki a buildable system."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:spinneret
               #:eclector)
  :serial t
  :components ((:file "wiki-package")
               (:file "wiki-org")
               (:file "wiki-html")
               (:file "wiki-lisp")
               (:file "wiki-dexp")
               (:file "wiki-source")
               (:file "wiki-asdf"))
  :in-order-to ((asdf:test-op (asdf:test-op #:luv-wiki/tests))))

(asdf:defsystem #:luv-wiki/tests
  :description "Executable claims about the wiki reader and renderer."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv-wiki
               #:luv-wiki/cli
               #:luv/wiki
               #:rove)
  :components ((:module "tests"
                :components ((:file "wiki"))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call
                      '#:rove '#:run-suite
                      (uiop:symbol-call
                       '#:rove '#:find-suite '#:luv-wiki/tests))
               (error "luv wiki tests failed"))))

(asdf:defsystem #:luv-wiki/cli
  :description "The ./wiki command: table of contents, work marks, figures,
mentions, definitions, and the site build from a shell."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv-wiki
               #:luv-wiki/introspect)
  :components ((:file "wiki-cli"))
  :build-operation "program-op"
  :build-pathname "build/wiki-cli"
  :entry-point "luv.wiki.cli:main")

(asdf:defsystem #:luv-wiki/introspect
  :description "Gathers real operator lambda lists from a loaded image for the
dexp renderer's derived layouts."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv-wiki
               (:require #:sb-introspect))
  :components ((:file "wiki-introspect")))
