;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; generate.lisp --- generate CFFI bindings from vk.xml file.
;;;
;;; Copyright (c) 2016, Bart Botta  <00003b@gmail.com>
;;;   All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;

(in-package :vk-generator/vk-spec)


(defun fix-type-name (name vendor-ids)
  "Converts a given camel-cased NAME of a type from the C Vulkan API to a lispy name.

E.g.: \"VkPhysicalDevice\" becomes \"PHYSICAL-DEVICE\"

See *SPECIAL-WORDS*
See VENDOR-IDS
"
  (if (not (stringp name))
      name
      (let* ((start (if (alexandria:starts-with-subseq "Vk" name) 2 0)))
        (when (zerop start)
          (setf name (ppcre:regex-replace-all "PFN_vk" name "Pfn")))
        (cffi:translate-camelcase-name (subseq name start)
                                       :special-words (append *special-words*
                                                              vendor-ids)))))

(defun fix-function-name (name vendor-ids)
  "Converts a given camel-cased NAME of a function from the C Vulkan API to a lispy name.

E.g.: \"vkGetPhysicalDeviceMemoryProperties\" becomes \"GET-PHYSICAL-DEVICE-MEMORY-PROPERTIES\"

See *SPECIAL-WORDS*
See VENDOR-IDS
"
  (let* ((start (if (alexandria:starts-with-subseq "vk" name) 2 0)))
    (cffi:translate-camelcase-name (subseq name start)
                                   :special-words (append *special-words*
                                                          vendor-ids))))

(defun fix-bit-name (name vendor-ids &key (prefix "VK_"))
  "Converts a given camel-cased NAME of a special bit in a bitmask from the C Vulkan API to a lispy name.

E.g.: \"VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT\" becomes \"MEMORY-PROPERTY-DEVICE-LOCAL\"

See VENDOR-IDS
"
  ;; fixme: cache compiled regex instead of rebuilding from string on every call
  (substitute #\- #\_
              (ppcre:regex-replace-all (format nil "(^~a|_BIT(~{_~a~^|~})?$)"
                                               prefix vendor-ids)
                                       name "")))
