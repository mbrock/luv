(defpackage #:vulkan-spec.constants
  (:use :cl)
  (:export
   ;; constants
   :*special-words*
   :*special-pointer-types*
   :*special-base-types*
   :*vk-platform*
   :*opaque-types*
   :*opaque-struct-types*
   :*fix-must-be*
   :*misc-os-types*
   :+ext-base+
   :+ext-block-size+
   :*directly-exposed-extensions*))

(defpackage #:vulkan-spec
  (:documentation "A CLOS-based representation of the Vulkan specifcation parsed from a vk.xml.")
  (:use :cl
        :vulkan-spec.constants)
  (:export
   ;; classes
   :api-constant
   :base-type
   :bitmask
   :command
   :command-alias
   :define
   :enum
   :enum-value
   :extension
   :feature
   :func-pointer
   :handle
   :member-data
   :name-data
   :param
   :platform
   :require-data
   :struct
   :type-category
   :type-info
   :vk-type
   
   ;; accessors
   :name
   :alias
   :type-info
   :type-name
   :feature
   :array-sizes
   :bit-count
   :comment
   :id
   :count
   :requires
   :prefix
   :postfix
   :type-info
   :len
   :optional-p
   :error-codes
   :handle
   :params
   :return-type
   :success-codes
   :number-value
   :string-value
   :vk-hpp-name
   :single-bit-p
   :aliases
   :is-bitmask-p
   :enum-values
   :deprecated-by
   :obsoleted-by
   :platform
   :promoted-to
   :requirements
   :children
   :parents
   :non-dispatch-handle-p
   :delete-command
   :delete-pool
   :create-commands
   :no-autovalidity-p
   :selection
   :selector
   :allowed-values
   :members
   :used-constant
   :protect
   :allow-duplicate-p
   :is-union-p
   :returned-only-p
   :struct-extends
   :sub-struct
   :member
   :category
   :is-value-p
   :is-struct-p
   :calls
   :args
   :needs-explicit-loading-p

   ;; functions
   :sorted-elements
   :sorted-names
   :const-pointer-p
   :non-const-pointer-p
   :value-p
   :pointer-p
   :pointer-to-non-char-pointer-p
   :make-aliased-command
   :extension-command-p
   :structure-type-p
   :split-len-by-struct-member
   :len-by-struct-member-p
   :structure-chain-anchor-p
   :determine-vector-param-indices
   :determine-count-to-vector-param-indices
   :determine-non-const-pointer-param-indices
   :determine-const-pointer-param-indices
   :get-vector-params
   :get-vector-count-params
   :get-output-params
   :get-handle-params
   :get-non-struct-params
   :get-struct-params
   :get-required-params
   :get-optional-params
   :get-skipped-input-params
   :determine-command-type
   :determine-command-type-2
   :reverse-hash-table
   :handlep
   :get-handle
   :get-structure-type
   :get-type-name
   
   ;; vulkan-spec
   :vulkan-spec
   :base-types
   :bitmasks
   :commands
   :constants
   :defines
   :enums
   :extended-structs
   :extensions
   :extension-names
   :features
   :func-pointers
   :handles
   :includes
   :platforms
   :stucture-aliases
   :structures
   :tags
   :types
   :typesafe-check
   :version
   :vulkan-license-header

   ;; parse spec
   :parse-vk-xml))

(uiop:define-package :vk-generator
    (:use :cl
          :vulkan-spec
          :vulkan-spec.constants)
  (:export
   ;; vk.xml related
   :*versions*
   :get-xml-path
   :ensure-vk-xml

   ;; fix-name
   :fix-type-name
   :fix-function-name
   :fix-bit-name

   ;; write package
   :write-vk-package
   
   :generate))
