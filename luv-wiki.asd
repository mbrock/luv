(in-package #:asdf-user)

(defsystem "luv-wiki"
  :description "The opinionated luv web framework: Org and Lisp source pages,
Spinneret, S-expression CSS and JavaScript, dynamic hosting, and publication."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:spinneret
               #:eclector
               #:named-readtables
               #:parenscript
               #:clack
               #:clack-handler-woo)
  :serial t
  :components ((:module "wiki"
                :serial t
                :components ((:file "package")
                             (:file "css")
                             (:file "parenscript")
                             (:file "deploy")
                             (:file "org")
                             (:file "html")
                             (:file "lisp")
                             (:file "dexp")
                             (:file "style")
                             (:file "history")
                             (:file "source")
                             (:file "web")
                             (:file "typst")
                             (:file "asdf"))))
  :in-order-to ((test-op (test-op "luv-wiki/test"))))

(defsystem "luv-wiki/test"
  :description "Executable claims about the wiki reader and renderer."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luv-wiki"
               "luv-wiki/cli"
               "luv/test-support")
  :components ((:module "wiki"
                :components ((:file "tests"))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:luv.test-support '#:test-package
                               '#:luv.wiki.tests)))

(defsystem "luv-wiki/cli"
  :description "The ./wiki command: table of contents, work marks, figures,
mentions, definitions, and the site build from a shell."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv-wiki
               #:luv-wiki/introspect)
  :components ((:module "wiki"
                :components ((:file "cli"))))
  :build-operation "program-op"
  :build-pathname "build/wiki-cli"
  :entry-point "luv.wiki.cli:main")

(defsystem "luv-wiki/introspect"
  :description "Gathers real operator lambda lists from a loaded image for the
dexp renderer's derived layouts."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv-wiki
               (:require #:sb-introspect))
  :components ((:module "wiki"
                :components ((:file "introspect")))))
