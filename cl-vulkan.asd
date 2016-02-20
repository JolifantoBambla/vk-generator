(defsystem cl-vulkan
  :description "Common Lisp bindings for Vulkan API."
  :depends-on (cffi alexandria)
  :license "MIT"
  :author "Bart Botta <00003b at gmail.com>"
  :components
  ((:module "vk"
    :serial t
    :components ((:file "bindings-package")
                 (:file "bindings")
                 (:file "types")
                 (:file "funcs")
                 (:file "translators")
                 (:file "package")
                 (:file "wrappers")))))
