(ql:quickload :vk-generator)
(in-package :vk-generator/vk-spec)

(vk-generator/ensure-vk-xml:ensure-vk-xml "v1.2.153")

(defvar spec)
(setf spec (parse-vk-xml "123" #P"/tmp/vk-v1.2.153.xml"))

(defun foo ()
  (ql:quickload :vk-generator)
  (setf spec (parse-vk-xml "123" #P"/tmp/vk-v1.2.153.xml")))

(array-sizes (first (member-values (gethash "VkClearColorValue" (structures spec)))))


(ql:quickload :vk-generator)
(in-package :vk-generator)
(defun bar ()
  (ql:quickload :vk-generator)
  (vk-generator:generate))
