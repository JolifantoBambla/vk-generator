(uiop:define-package :vk-generator/versions
    (:use :cl)
  (:export :*versions*
           :get-xml-path))

(uiop:define-package :vk-generator/ensure-vk-xml
    (:use :cl
          :vk-generator/versions)
  (:export :make-vulkan-docs-name
           :make-vk-xml-name
           :make-zip-pathname
           :make-xml-pathname
           :download-vulkan-docs
           :extract-vk-xml
           :ensure-vk-xml))

(uiop:define-package :vk-generator/vk-spec
    (:use :cl)
  (:export :vk-spec
           :version
           :copyright
           :vendor-ids
           :types
           :bitfields
           :enums
           :structs
           :functions
           :function-apis
           :extension-names
           :handle-types
           :alias-names
           :get-type
           :get-type/f
           :set-type))

;;; PARSER

(uiop:define-package :vk-generator/parser/constants
    (:use :cl)
  (:export :*special-words*
           :*vk-platform*
           :*opaque-types*
           :*opaque-struct-types*
           :*fix-must-be*
           :*misc-os-types*))

(uiop:define-package :vk-generator/parser/make-keyword
    (:use :cl)
  (:export :make-keyword
           :make-const-keyword))

(uiop:define-package :vk-generator/parser/xml-utils
    (:use :cl)
  (:export :xps
           :attrib-names))

(uiop:define-package :vk-generator/parser/numeric-value
    (:use :cl)
  (:export :numeric-value))

(uiop:define-package :vk-generator/parser/extract-vendor-ids
    (:use :cl
          :vk-generator/parser/xml-utils)
  (:export :extract-vendor-ids))

(uiop:define-package :vk-generator/parser/fix-name
    (:use :cl
     :vk-generator/parser/constants)
  (:export :fix-type-name
           :fix-function-name
           :fix-bit-name))

(uiop:define-package :vk-generator/parser/parse-arg-type
    (:use :cl
          :vk-generator/parser/constants
          :vk-generator/parser/fix-name
          :vk-generator/parser/xml-utils
          :vk-generator/parser/make-keyword)
  (:export :parse-arg-type))

(uiop:define-package :vk-generator/write-types-file
    (:use :cl
          :vk-generator/parser/constants
          :vk-generator/parser/fix-name)
  (:export :write-types-file))

(uiop:define-package :vk-generator/generate
    (:use :cl
          :vk-generator/parser/constants
          :vk-generator/parser/make-keyword
          :vk-generator/parser/xml-utils
          :vk-generator/parser/numeric-value
          :vk-generator/parser/extract-vendor-ids
          :vk-generator/parser/fix-name
          :vk-generator/parser/parse-arg-type
          :vk-generator/write-types-file
          :vk-generator/vk-spec)
  (:export :generate-vk-package))



(uiop:define-package :vk-generator/make-vk
    (:use :cl
          :vk-generator/versions
          :vk-generator/ensure-vk-xml
          :vk-generator/generate)
  (:export :make-vk))

;;; VK-GENERATOR

(uiop:define-package :vk-generator
    (:use :cl
          :vk-generator/versions
          :vk-generator/ensure-vk-xml
          :vk-generator/generate
          :vk-generator/make-vk)
  (:reexport :vk-generator/versions)
  (:reexport :vk-generator/ensure-vk-xml)  
  (:reexport :vk-generator/generate) 
  (:reexport :vk-generator/make-vk))
