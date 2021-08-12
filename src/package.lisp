(defpackage #:vk-generator.constants
  (:use :cl)
  (:export
   ;; constants
   :*special-words*
   :*vk-platform*
   :*opaque-types*
   :*opaque-struct-types*
   :*fix-must-be*
   :*misc-os-types*
   :+ext-base+
   :+ext-block-size+))

(defpackage #:vulkan-spec
  (:documentation "A CLOS-based representation of the Vulkan specifcation parsed from a vk.xml.")
  (:use :cl
        :vk-generator.constants)
  (:export
   ;; vulkan-spec related
   ;; other accessors
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

   ;; functions
   :sorted-elements
   :sorted-names
   :const-pointer-p
   :non-const-pointer-p
   :value-p
   :make-aliased-command
   :extension-command-p
   :structure-type-p
   
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
          :vk-generator.constants)
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
