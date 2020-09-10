;;;; fetch-vk-xml.lisp

(in-package :vk-generator/fetch-vk-xml)

(defun make-vulkan-docs-name (version)
  (format nil "Vulkan-Docs-~a" (subseq version 1)))

(defun make-vk-xml-name (version)
  (format nil "vk-~a" version))

(defun make-zip-pathname (version &optional (zip-dir "tmp"))
  (make-pathname :directory zip-dir :name (make-vulkan-docs-name version) :type "zip"))

(defun make-xml-pathname (version &optional xml-dir)
  (make-pathname :directory xml-dir
                 :name (make-vk-xml-name version)
                 :type "xml"))

(defun download-specification (version &optional (out-dir "tmp"))
  "Download the given version of the Vulkan specfication from the official github repostiory."
  (trivial-download:download
    (format nil "https://github.com/KhronosGroup/Vulkan-Docs/archive/~a.zip" version)
    (make-zip-pathname version out-dir)))

(defun extract-vk-xml (version &optional (zip-dir "tmp") (out-dir "tmp"))
  (zip:with-zipfile (vulkan-docs (make-zip-pathname version zip-dir))
    (with-open-file
        (stream (make-xml-pathname version out-dir)
                :direction :output
                :if-exists :supersede
                :element-type '(unsigned-byte 8))
      (zip:zipfile-entry-contents
       (zip:get-zipfile-entry (namestring
                               (merge-pathnames
                                (make-pathname :name "vk" :type "xml")
                                (merge-pathnames
                                 (get-xml-path version)
                                 (make-pathname :directory (list :relative (make-vulkan-docs-name version))))))
                              vulkan-docs)
       stream))))

(defun fetch-vk-xml (version &optional (vulkan-docs-dir "tmp") (vk-xml-dir "tmp"))
  (download-specification version vulkan-docs-dir)
  (extract-vk-xml version vulkan-docs-dir vk-xml-dir)
  (make-xml-pathname version vk-xml-dir))

