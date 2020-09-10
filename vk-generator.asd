;;;; vk-generator.asd

(defsystem vk-generator
  :version "0.0.0"
  :author "Lukas Herzberger"
  :mailto "my@email.com"
  :licence "MIT"
  :homepage "https://github.com"
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
                             (:file "fetch-vk-xml")
                             (:file "generate")
                             (:file "make-vk"))))
  :description "Creates the package vk which provides Vulkan bindings for Common Lisp")

