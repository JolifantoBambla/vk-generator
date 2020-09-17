;;;; fetch-vk-xml.lisp

(in-package :vk-generator/ensure-vk-xml)


(defun make-vulkan-docs-name (version)
  (declare (type string version))
  "Creates the name for a Vulkan-Docs snapshot (without file type) from a VERSION tag.

The leading 'v' from the given version tag is omitted.
E.g. \"v1.2.153\" produces \"Vulkan-Docs-1.2.153\"

See *VERSIONS*
"
  (format nil "Vulkan-Docs-~a" (subseq version 1)))

(defun make-vk-xml-name (version)
  (declare (type string version))
  "Creates the name of a vk.xml file (without file type) from a VERSION tag.

E.g. \"v1.2.153\" produces \"vk-v1.2.153\"

See *VERSIONS*
"
  (format nil "vk-~a" version))

(defun make-zip-pathname (version zip-dir)
  (declare (type string version))
  (declare (type pathname zip-dir))
  "Creates a pathname for a Vulkan-Docs snapshot from a VERSION tag.

The leading 'v' from the given version tag is omitted.

E.g. #P\"/tmp/\" produces #P\"/tmp/Vulkan-Docs-1.2.153.zip\"

See *VERSIONS*
See MAKE-VULKAN-DOCS-NAME
"
  (merge-pathnames (make-pathname :name (make-vulkan-docs-name version) :type "zip")
                   zip-dir))

(defun make-xml-pathname (version xml-dir)
  (declare (type string version))
  (declare (type pathname xml-dir))
    "Creates a pathname for a vk.xml file from a VERSION tag.

E.g. #P\"/tmp/\" produces #P\"/tmp/vk-v1.2.153.xml\"

See *VERSIONS*
See MAKE-VK-XML-NAME
"
  (merge-pathnames (make-pathname :name (make-vk-xml-name version) :type "xml")
                   xml-dir))

(defun download-vulkan-docs (version out-dir)
  (declare (type string version))
  (declare (type pathname out-dir))
  "Downloads a Vulkan-Docs snapshot for a given VERSION tag from the official GitHub repostiory to a given directory.

See *VERSIONS*
See MAKE-ZIP-PATHNAME
See https://github.com/KhronosGroup/Vulkan-Docs
"
  (trivial-download:download
    (format nil "https://github.com/KhronosGroup/Vulkan-Docs/archive/~a.zip" version)
    (make-zip-pathname version out-dir)))

(defun extract-vk-xml (version zip-dir out-dir)
  (declare (type string version))
  (declare (type pathname zip-dir))
  (declare (type pathname out-dir))
  "Extracts a vk.xml file for a given VERSION tag from a local snapshot of Vulkan-Docs in a given directory to a given directory.

See *VERSIONS*
See MAKE-XML-PATHNAME
See MAKE-ZIP-PATHNAME
See GET-XML-PATH
"
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

(defun ensure-vk-xml (version &key
                                (vulkan-docs-dir
                                 #+win32 #P"\\temp\\" ; test if this is really the right path on windows...
                                 #-win32 #P"/tmp/")
                               (vk-xml-dir nil vk-xml-dir-supplied-p)
                               (force-download nil))
  (declare (type string version))
  (declare (type pathname vulkan-docs-dir))
  "Ensures a vk.xml file for a given VERSION tag exists in a given directory.

If the xml file for the given VERSION tag does not exists in VULKAN-DOCS-DIR (or VK-XML-DIR if given) it is
extracted from a Vulkan-Docs snapshot for the given VERSION tag in VULKAN-DOCS-DIR.
If no Vulkan-Docs snapshot for the given VERSION tag exists in VULKAN-DOCS-DIR the snapshot is downloaded.

If FORCE-DOWNLOAD is truthy a Vulkan-Docs snapshot is downloaded and its vk.xml file is extracted whether
either of the two exists or not.

See *VERSIONS*
See MAKE-XML-PATHNAME
See MAKE-ZIP-PATHNAME
See DOWNLOAD-VULKAN-DOCS
See EXTRACT-VK-XML
"
  (let* ((xml-dir (if vk-xml-dir-supplied-p
                     vk-xml-dir
                     vulkan-docs-dir))
         (xml-path-name (make-xml-pathname version xml-dir)))
    (ensure-directories-exist vulkan-docs-dir :verbose t)
    (ensure-directories-exist xml-dir :verbose t)
    (let ((xml-exists-p (probe-file xml-path-name)))
      (when (or force-download
                (and (not xml-exists-p)
                     (not (probe-file (make-zip-pathname version xml-dir)))))
        (format t "Downloading Vulkan-Docs~%")
        (download-vulkan-docs version vulkan-docs-dir))
      (when (or force-download
                (not xml-exists-p))
        (format t "Extracting vk.xml~%")
        (extract-vk-xml version vulkan-docs-dir xml-dir)))
    (format t "vk.xml is here: ~s~%" xml-path-name)
    xml-path-name))
