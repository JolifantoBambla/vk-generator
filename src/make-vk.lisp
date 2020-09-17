;;;; make-vk

(in-package :vk-generator/make-vk)


(defun make-vk (&key
                  (version (car (last *versions*)))
                  (package-dir (first ql:*local-project-directories*) package-dir-supplied-p)
                  (download-dir #P"/tmp/") ;; todo: make this cross-platform - there's probably a lib for that somewhere
                  (force-download nil))
  (declare (type string version))
  (declare (type pathname package-dir))
  (declare (type pathname download-dir))
  "Generates the package VK from an xml file.

  :VERSION        --- The version tag of the Vulkan version for which the VK package is generated.
                      See *VERSIONS* for known versions and the Vulkan-Docs repository for a list of exising version tags: https://github.com/KhronosGroup/Vulkan-Docs/releases
                      Default: (CAR (LAST VK-GENERATOR:*VERSIONS*))
  :PACKAGE-DIR    --- The directory where the VK package is created.
                      The sources of the created package will lie in <:PACKAGE-DIR>/vk.
                      Default: (FIRST QL:*LOCAL-PROJECT-DIRECTORIES*)
  :DOWNLOAD-DIR   --- The directory to which the Vulkan documentation is downloaded and to which the vk.xml is extracted.
                      If either a vk-<:VERSION>.xml or a Vulkan-Docs-<:VERSION>.zip exists at DOWNLOAD-DIR and :FORCE-DOWNLOAD is NIL, the extraction of the xml file and/or the download of the Vulkan documentation is omitted.
                      Default: #P\"/tmp\"
  :FORCE-DOWNLOAD --- If this is truthy the Vulkan documentation will be downloaded even if it already exists in the file system.
                      Default: NIL

See ENSURE-VK-XML
"
  (unless (find version *versions* :test #'string=)
    (warn "Unknown version: ~s, trying anyway" version))
  (when (and package-dir-supplied-p
             (not (find package-dir ql:*local-project-directories*)))
    (warn ":PACKAGE-DIR is not included in QL:*LOCAL-PROJECT-DIRECTORIES*: ~s" package-dir))
  (let ((vk-dir (merge-pathnames (make-pathname :directory '(:relative "vk")) package-dir)))
    (ensure-directories-exist vk-dir :verbose t)
    (generate-vk-package
     (ensure-vk-xml version :vulkan-docs-dir download-dir :force-download force-download)
     vk-dir)))

