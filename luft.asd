(in-package #:asdf-user)

(defsystem "luft"
  :description "Packed sites in a small cubical world."
  :version "0.0.1"
  :author "Mikael Brockman"
  :serial t
  :components ((:file "luft/package")
               (:file "luft/luft"))
  :in-order-to ((test-op (test-op "luft/test"))))

(defsystem "luft/test"
  :description "Executable incidence laws for packed LUFT sites."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luft" "rove")
  :components ((:file "luft/tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call '#:rove '#:run-suite
                                       (uiop:symbol-call '#:rove '#:find-suite
                                                         '#:luft.tests))
               (error "LUFT tests failed"))))
