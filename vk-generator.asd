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
               #:cl-ppcre
               #:cxml
               #:cxml-stp
               #:split-sequence
               #:trivial-download
               #:xpath
               #:zip)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "versions")
                             (:file "ensure-vk-xml")
                             (:file "parser/constants")
                             (:file "parser/make-keyword")
                             (:file "parser/numeric-value")
                             (:file "parser/xml-utils")
                             (:file "parser/extract-vendor-ids")
                             (:file "parser/parse-arg-type")
                             (:file "parser/fix-name")
                             (:file "generate")
                             (:file "make-vk")))))

