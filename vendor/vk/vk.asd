(defsystem vk
  :version "3.1.1"
  :license "MIT"
  :description "Common Lisp bindings for the Vulkan API."
  :author "Lukas Herzberger <herzberger.lukas at gmail.com>"
  :maintainer "Lukas Herzberger <herzberger.lukas at gmail.com>"
  :homepage "https://jolifantobambla.github.io/vk/"
  :bug-tracker "https://github.com/JolifantoBambla/vk/issues"
  :source-control (:git "https://github.com/JolifantoBambla/vk.git")
  #+sbcl
  :around-compile
  #+sbcl
  (lambda (thunk)
    ;; The generated binding consists of a few very large, heavily expanded
    ;; files.  Keep its compiler metadata lean even when VK is compiled while
    ;; loading a DEBUG 3 client such as LUV/GPU.
    (with-compilation-unit (:override t
                            :policy '(optimize (debug 1)
                                               (speed 2)
                                               (safety 1)))
      (funcall thunk)))
  :depends-on (cffi alexandria)
  :components
  ((:module "src"
    :serial t
    :components ((:file "package")
                 (:file "vk-alloc")
                 (:file "vulkan-bindings")
                 (:file "vulkan-api-constants")
                 (:file "vulkan-types")
                 (:file "vk-handle")
                 (:file "vk-types")
                 (:file "vk-constructors")
                 (:file "vulkan-define-conditions")
                 (:file "vulkan-errors")
                 (:file "vulkan-extra-types")
                 (:file "vk-translate-to-foreign")
                 (:file "vk-expand-to-foreign")
                 (:file "vk-translate-from-foreign")
                 (:file "vk-expand-from-foreign")
                 (:file "vulkan-commands")
                 (:file "vk-base")
                 (:file "vk-bindings")
                 (:file "vk-commands")
                 (:file "vk-pretty-printers")
                 (:file "vk-utils-common")
                 (:file "vk-utils-with-resource"))))
  :in-order-to ((test-op (test-op vk/tests))))

(defsystem vk/tests
  :depends-on (:vk
               :rove)
  :defsystem-depends-on (:rove)
  :components ((:module "test"
                :components ((:file "translators"))))
  :perform (test-op :after (op c) (uiop:symbol-call :rove :run c)))
