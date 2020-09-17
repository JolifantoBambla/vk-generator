(uiop:define-package :vk-generator/versions
    (:use :cl)
  (:export :*versions*
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

(uiop:define-package :vk-generator/generate
    (:use :cl)
  (:export :generate-vk-package))

(uiop:define-package :vk-generator/make-vk
    (:use :cl
          :vk-generator/versions
          :vk-generator/fetch-vk-xml
          :vk-generator/generate)
  (:export :make-vk))

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
