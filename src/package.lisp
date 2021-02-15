(uiop:define-package :vk-generator
    (:use :cl)
  (:export
   ;; vk.xml related
   :*versions*
   :get-xml-path
   :ensure-vk-xml

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
   :member-values
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
   :sorted-elements
   :sorted-names
   
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

   ;; constants
   :*special-words*
   :*vk-platform*
   :*opaque-types*
   :*opaque-struct-types*
   :*fix-must-be*
   :*misc-os-types*

   ;; fix-name
   :fix-type-name
   :fix-function-name
   :fix-bit-name

   ;; parse spec
   :parse-vk-xml

   ;; write package
   :write-vk-package
   
   :generate))
