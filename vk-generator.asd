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
               #:cl-fad
               #:cl-ppcre
               #:cxml
               #:cxml-stp
               #:kebab
               #:split-sequence
               #:trivial-download
               #:xpath)
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "package")
     (:file "versions")
     (:file "ensure-vk-xml")
     (:file "vk-spec")
     (:file "constants")
     (:file "fix-name")
     (:file "xml-utils")
     (:file "numeric-value")
     (:file "parse")
     (:file "make-keyword")
     (:file "write-types-file")
     (:file "writer")
     (:file "generate")))))

