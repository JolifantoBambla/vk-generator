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
   ;; vk-spec
   :vk-spec
   :version
   :copyright
   :vendor-ids
   :vk-api-version
   :api-constants
   :types
   :bitfields
   :enums
   :structs
   :functions
   :function-apis
   :extension-names
   :handle-types
   :alias-names

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
   :set-type))

(uiop:define-package :vk-generator/parser
    (:use :cl
          :vk-generator/vk-spec)
  (:export :parse-vk-xml))

(uiop:define-package :vk-generator/writer
    (:use :cl
          :vk-generator/vk-spec)
  (:export :write-vk-package))

(uiop:define-package :vk-generator
    (:use :cl
          :vk-generator/versions
          :vk-generator/ensure-vk-xml
          :vk-generator/parser
          :vk-generator/writer)
  (:export :generate))
