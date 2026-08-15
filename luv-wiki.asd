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
               (:file "wiki-asdf"))
  :in-order-to ((asdf:test-op (asdf:test-op #:luv-wiki/tests))))

(asdf:defsystem #:luv-wiki/tests
  :description "Executable claims about the wiki reader and renderer."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv-wiki
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
