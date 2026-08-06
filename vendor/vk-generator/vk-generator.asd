;;;; vk-generator.asd

(defsystem vk-generator
  :version "0.0.0"
  :licence "MIT"
  :author "Lukas Herzberger"
  :maintainer "Lukas Herzberger"
  :homepage "https://github.com/lHerzberger/vk-generator"
  :bug-tracker "https://github.com/lHerzberger/vk-generator/issues"
  :source-control (:git "https://github.com/lHerzberger/vk-generator.git")
  :description "A tool to generate Vulkan bindings for Common Lisp from the XML API Registry."
  :depends-on (#:alexandria
               #:cffi
               #:cl-fad
               #:cl-ppcre
               #:cxml
               #:cxml-stp
               #:kebab
               #:split-sequence
               #:trivial-download
               #:xpath)
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "package")
     (:file "versions")
     (:file "ensure-vk-xml")
     (:module "vulkan-spec"
      :serial t
      :components
      ((:file "constants")
       (:file "vulkan-spec")
       (:file "parse-utils-3b")
       (:file "parse-utils")
       (:file "parse-name-data")
       (:file "parse-struct")
       (:file "parse-types")
       (:file "parse-enum")
       (:file "parse-command")
       (:file "parse-extensions-and-features")
       (:file "parse")
       (:file "command-utils")))
     (:file "structure-utils")
     (:file "write-utils-3b")
     (:file "write-utils")
     (:file "write-types-file")
     (:file "write-vk-types")
     (:file "write-wrappers")
     (:file "write-with-resource")
     (:file "write-constructors")
     (:file "write-printers")
     (:file "write-vk-package")
     (:file "generate")))))

