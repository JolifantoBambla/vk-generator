;;;; make-vk

(in-package :vk-generator/make-vk)

;;(load "~/Projects/cl-vulkan/tools/fetch-vk-xml.lisp")
;;(load "~/Projects/cl-vulkan/tools/generate.lisp")

(defun make-vk (version out-dir)
  (generate-vk-package (fetch-vk-xml version) out-dir))
