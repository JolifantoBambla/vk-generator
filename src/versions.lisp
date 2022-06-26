;;;; Contains all supported versions and some related functions

(in-package :vk-generator)

;; NOTE: this is ordered - do not change
(defparameter *versions*
  '("v1.0-core-20160216"     ;; until tag v1.1.72 xml is located in "src/spec/vk.xml"
    "v1.0-core+wsi-20160216"
    "v1.0-core-20160226"
    "v1.0-core+wsi-20160226"
    "v1.0-core-20160304"
    "v1.0-core+wsi-20160304"
    "v1.0-core-20160310"
    "v1.0-core+wsi-20160310"
    "v1.0-core-20160311"
    "v1.0-core+wsi-20160311"
    "v1.0-core-20160325"
    "v1.0-core+wsi-20160325"
    "v1.0-core-20160401"
    "v1.0-core+wsi-20160401"
    "v1.0-core-20160408"
    "v1.0-core+wsi-20160408"
    "v1.0-core-20160415"
    "v1.0-core+wsi-20160415"
    "v1.0-core-20160422"
    "v1.0-core+wsi-20160422"
    "v1.0-core-20160429"
    "v1.0-core+wsi-20160429"
    "v1.0-core-20160513"
    "v1.0-core+wsi-20160513"
    "v1.0-core-20160520"
    "v1.0-core+wsi-20160520"
    "v1.0-core-20160527"
    "v1.0-core+wsi-20160527"
    "v1.0-core-20160610"
    "v1.0-core+wsi-20160610"
    "v1.0-core-20160617"
    "v1.0-core+wsi-20160617"
    "v1.0-core-20160624"
    "v1.0-core+wsi-20160624"
    "v1.0-core-20160701"
    "v1.0-core+wsi-20160701"
    "v1.0-core-20160710"
    "v1.0-core+wsi-20160710"
    "v1.0-core-20160715"
    "v1.0-core+wsi-20160715"
    "v1.0-core-20160722"
    "v1.0-core+wsi-20160722"
    "v1.0-core-20160805"
    "v1.0-core+wsi-20160805"
    "v1.0-core-20160812"
    "v1.0-core+wsi-20160812"
    "v1.0-core-20160826"
    "v1.0-core-20160906"
    "v1.0-core-20160916"
    "v1.0-core-20160923"
    "v1.0-core-20160930" ;; adds the "returnedonly=false" attribute to WSI output structures
    "v1.0-core-20161007"
    "v1.0-core-20161014"
    "v1.0-core-20161025"
    "v1.0.33-core"
    "v1.0.34-core"
    "v1.0.35-core"
    "v1.0.36-core" ;; adds "pipeline type" attribute for commands
    "v1.0.38-core"
    "v1.0.39-core"
    "v1.0.40-core"
    "v1.0.41-core"
    "v1.0.42-core"
    "v1.0.43-core"
    "v1.0.44-core"
    "v1.0.45-core"
    "v1.0.46-core"
    "v1.0.47-core"
    "v1.0.48-core"
    "v1.0.49-core"
    "v1.0.50-core"
    "v1.0.51-core"
    "v1.0.53-core"
    "v1.0.54-core"
    "v1.0.55-core" ;; "validextensionstructs" (in <member>) replaced by "structextends" (in <type>); replace comments with <comment> and "comment" attributes
    "v1.0.56-core"
    "v1.0.57-core"
    "v1.0.58-core"
    "v1.0.59-core"
    "v1.0.60-core" ;; adds an "extension" attribute to <require> - looks like <require> attributes are not checked
    "v1.0.61-core" ;; adds an "altlen" attribute to <member>
    "v1.0.62-core"
    "v1.0.63-core"
    "v1.0.64-core"
    "v1.0.65-core"
    "v1.0.66-core" ;; add an API constant with value "(~0U-2)"
    "v1.0.67-core"
    "v1.0.68-core"
    "v1.0.69-core"
    "v1.1.70" ;; adds "alias" attribute which means that from now on each type may have an alias - alias not handled yet!
    "v1.1.71" ;; adds "const struct *" and "struct **", adds AHardwareBuffer
    "v1.1.72" ;; from tag v1.1.72 (number 91) xml is located in "xml/vk.xml", ALSO: "noautovalidity=true" is implied by "structextends" and no longer used
    "v1.1.73"
    "v1.1.74" 
    "v1.1.75" ;; adds generic VkBaseInStruct and VkBaseOutStruct which have members of the same generic type - requires fix for infinite recursion
    "v1.1.76"
    "v1.1.77"
    "v1.1.78"
    "v1.1.79"
    "v1.1.80"
    "v1.1.81"
    "v1.1.82"
    "v1.1.83"
    "v1.1.84"
    "v1.1.85"
    "v1.1.86"
    "v1.1.87"
    "v1.1.88"
    "v1.1.89"
    "v1.1.90"
    "v1.1.91"
    "v1.1.92"
    "v1.1.93"
    "v1.1.94"
    "v1.1.95"
    "v1.1.96"
    "v1.1.97"
    "v1.1.98"
    "v1.1.99"    
    "v1.1.100"
    "v1.1.101"
    "v1.1.102"
    "v1.1.103"
    "v1.1.104"
    "v1.1.105"
    "v1.1.106"
    "v1.1.107"
    "v1.1.108"
    "v1.1.109"
    "v1.1.110"
    "v1.1.111"
    "v1.1.112"
    "v1.1.113"
    "v1.1.114"
    "v1.1.115"
    "v1.1.116"
    "v1.1.117"
    "v1.1.118"
    "v1.1.119"
    "v1.1.120"
    "v1.1.121" ;; adds a "sortorder" attribute to <fetaure> and <extension>
    "v1.1.122"
    "v1.1.123"
    "v1.1.124" ;; adds empty enum "VkSemaphoreCreateFlagBits"
    "v1.1.125"
    "v1.1.126"
    "v1.1.127"
    "v1.1.128"
    "v1.1.129"
    "v1.1.130"
    "v1.2.131"
    "v1.2.132"
    "v1.2.133"
    "v1.2.134"
    "v1.2.135"
    "v1.2.136"
    "v1.2.137"
    "v1.2.138"
    "v1.2.139"
    "v1.2.140" ;; this changes the category of types like ANativeWindow from "define" to "basetype"; this introduces "allowduplicate" attribute for types in pNext-chain
    "v1.2.141"
    "v1.2.142" ;; introduces "selector" and "selection" for unions
    "v1.2.143"
    "v1.2.144"
    "v1.2.145"
    "v1.2.146"
    "v1.2.147"
    "v1.2.148"
    "v1.2.149"
    "v1.2.150"
    "v1.2.151"
    "v1.2.152"
    "v1.2.153"
    "v1.2.154"
    "v1.2.155"
    "v1.2.156"
    "v1.2.157"
    "v1.2.158"
    "v1.2.159"
    "v1.2.160"
    "v1.2.161"
    "v1.2.162"
    "v1.2.163"
    "v1.2.164"
    "v1.2.165"
    "v1.2.166"
    "v1.2.167"
    "v1.2.168"
    "v1.2.169"
    "v1.2.170"
    "v1.2.171"
    "v1.2.172"
    "v1.2.173"
    "v1.2.174"
    "v1.2.175"
    "v1.2.176"
    "v1.2.177"
    "v1.2.178"
    "v1.2.179"
    "v1.2.180"
    "v1.2.181"
    "v1.2.182"
    "v1.2.183"
    "v1.2.184"
    "v1.2.185"
    "v1.2.186"
    "v1.2.187"
    "v1.2.188"
    "v1.2.189"
    "v1.2.190"
    "v1.2.191"
    "v1.2.192"
    "v1.2.193"
    "v1.2.194"
    "v1.2.195"
    "v1.2.196"
    "v1.2.197"
    "v1.2.198"
    "v1.2.199"
    "v1.2.200"
    "v1.2.201"
    "v1.2.202"
    "v1.2.203"
    "v1.3.204"
    "v1.3.205"
    "v1.3.206"
    "v1.3.207"
    "v1.3.208"
    "v1.3.209"
    "v1.3.210"
    "v1.3.211"
    "v1.3.212"
    "v1.3.213"
    "v1.3.214"
    "v1.3.215"
    "v1.3.216")
  "A list of valid version tags in the Vulkan-Docs GitHub repository.

See https://github.com/KhronosGroup/Vulkan-Docs/releases
") ;; all versions supported

(defun get-version-index (version)
  (position version *versions* :test #'string=))

(defun version-compare (a b comparison)
  (funcall comparison (get-version-index a) (get-version-index b)))

(defun version< (a b)
  (version-compare a b #'<))

(defun version>= (a b)
  (not (version< a b)))

(defun is-v1.2 (version)
  (and (version< version "v1.3.204")
       (version>= version "v1.2.131")))

(defun get-base-doc-url-for-version (version)
  (format nil "https://www.khronos.org/registry/vulkan/specs/1.~:[3~;2~]-extensions/man/html/"
          (is-v1.2 version)))

(defun get-xml-path (version)
  (declare (type string version))
  "Returns the path to the vk.xml file in the Vulkan-Docs Github repository for a given version tag.

The Vulkan-Docs GitHub repository was restructured between v1.1.71 and v1.1.72.

See *VERSIONS*
See https://github.com/KhronosGroup/Vulkan-Docs
"
  (let ((version-index (position version *versions* :test #'string=)))
    (unless version-index (format t "Unknown version: ~a, trying anyway ..." version))
    (if (or (not version-index)
            (< (position version *versions* :test #'string=) 91))
        (make-pathname :directory '(:relative "src" "spec"))
        (make-pathname :directory '(:relative "xml")))))
