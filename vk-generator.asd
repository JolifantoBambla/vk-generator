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
     (:file "parse-utils")
     (:file "parse")
     (:file "write-utils")
     (:file "write-types-file")
     (:file "write-vk-types")
     (:file "write-wrappers")
     (:file "write-vk-package")
     (:file "generate")))))

