(uiop:define-package :vk-generator/versions
    (:use :cl)
  (:export :versions
           :get-xml-path))

(uiop:define-package :vk-generator/fetch-vk-xml
    (:use :cl
          :vk-generator/versions)
  (:export :make-vulkan-docs-name
           :make-vk-xml-name
           :make-zip-pathname
           :make-xml-pathname
           :download-specification
           :extract-vk-xml
           :fetch-vk-xml))

;;; PARSER

(uiop:define-package :vk-generator/parser/constants
    (:use :cl)
  (:export :*special-words*
           :*fix-must-be*))

(uiop:define-package :vk-generator/parser/xml-utils
    (:use :cl)
  (:export :xps
           :attrib-names))

(uiop:define-package :vk-generator/parser/numeric-value
    (:use :cl)
  (:export :numeric-value))

(uiop:define-package :vk-generator/generate
    (:use :cl
          :vk-generator/parser/constants
          :vk-generator/parser/xml-utils
          :vk-generator/parser/numeric-value)
  (:export :generate-vk-package))



(uiop:define-package :vk-generator/make-vk
    (:use :cl
          :vk-generator/versions
          :vk-generator/fetch-vk-xml
          :vk-generator/generate)
  (:export :make-vk))

;;; VK-GENERATOR

(uiop:define-package :vk-generator
    (:use :cl
          :vk-generator/versions
          :vk-generator/fetch-vk-xml
          :vk-generator/generate
          :vk-generator/make-vk)
  (:reexport :vk-generator/versions)
  (:reexport :vk-generator/fetch-vk-xml)  
  (:reexport :vk-generator/generate) 
  (:reexport :vk-generator/make-vk))
