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
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "package")
     (:file "versions")
     (:file "ensure-vk-xml")
     (:module "vk-spec"
      :serial t
      :components
      ((:file "vk-spec")
       (:file "constants")
       (:file "fix-name")
       (:file "type-utils")))
     (:module "parser"
      :depends-on ("vk-spec")
      :serial t
      :components
      ((:file "constants")
       (:file "make-keyword")
       (:file "xml-utils")
       (:file "extract-vendor-ids")
       (:file "parse-arg-type")
       (:file "numeric-value")
       (:file "parser")))
     (:module "writer"
      :depends-on ("vk-spec")
      :serial t
      :components
      ((:file "write-types-file")
       (:file "writer")))
     (:file "generate")))))

