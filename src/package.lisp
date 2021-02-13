(uiop:define-package :vk-generator/versions
    (:use :cl)
  (:export :*versions*
           :get-xml-path))

(uiop:define-package :vk-generator/ensure-vk-xml
    (:use :cl
          :vk-generator/versions)
  (:export :ensure-vk-xml))

(uiop:define-package :vk-generator/vk-spec
    (:use :cl)
  (:export
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
   :type-infor
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
   :constatns
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

   ;; type-utils
   :get-type
   :get-type/f
   :set-type

   ;; parse spec
   :parse-vk-xml))

(uiop:define-package :vk-generator/writer
    (:use :cl
          :vk-generator/vk-spec)
  (:export :write-vk-package))

(uiop:define-package :vk-generator
    (:use :cl
          :vk-generator/versions
          :vk-generator/ensure-vk-xml
          :vk-generator/vk-spec
          :vk-generator/writer)
  (:export :generate))
