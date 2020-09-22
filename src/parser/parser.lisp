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

(in-package :vk-generator/parser)


(defun parse-copyright-notice (vk.xml)
  "Extracts the copyright notice from a parsed vk.xml."
  (let* ((copyright (xpath:string-value
                     (xpath:evaluate "/registry/comment" vk.xml)))
         (s (search "This file, vk.xml, is the " copyright)))
    ;; remove some other text from the comment if present
    (when s (setf copyright (string-trim '(#\space #\tab #\newline #\return)
                                         (subseq copyright 0 s))))
      ;; make sure we still have a copyright notice
      (assert (search "Copyright" copyright))
    copyright))

(defun parse-api-version (vk.xml)
  "Extracts the VK_API_VERSION from a parsed vk.xml."
  (let ((api (xpath:string-value
              (xpath:evaluate "/registry/types/type/name[.=\"VK_API_VERSION\"]/.." vk.xml))))
   ;; #define VK_API_VERSION VK_MAKE_VERSION(1, 0, 3)
    (map 'list 'parse-integer
         (nth-value 1 (ppcre::scan-to-strings "\\((\\d+),\\W*(\\d+),\\W*(\\d+)\\)" api)))))

(defun parse-structs (vk.xml vk-spec)
  "Extracts structs / unions from a parsed VK.XML and stores them in the given VK-SPEC instance."
  (xpath:do-node-set (node (xpath:evaluate "/registry/types/type[(@category=\"struct\") or (@category=\"union\")]" vk.xml))
    (let ((name (xps (xpath:evaluate "@name" node)))
          (category (xps (xpath:evaluate "@category" node))))
      (setf (gethash (fix-type-name name (vendor-ids vk-spec)) (structs vk-spec)) (make-keyword category)))))

(defun parse-api-constants (vk.xml)
  "Extracts API constants from a parsed VK.XML."
  (let ((api-constants (make-hash-table :test 'equal)))
    (xpath:do-node-set (enum (xpath:evaluate "/registry/enums[(@name=\"API Constants\")]/enum" vk.xml))
      (let ((name (xps (xpath:evaluate "@name" enum)))
            (value (numeric-value (xps (xpath:evaluate "@value" enum))))
            (alias (xps (xpath:evaluate "@alias" enum))))
        (if alias
            (warn "ALIAS NOT HANDLED YET: ~s is alias for ~s~%" name alias)
            (progn
              (when (gethash name api-constants)
                (assert (= value (gethash name api-constants))))
              (setf (gethash name api-constants) value)))))
    api-constants))

(defun parse-handle-types (vk.xml)
  "Extracts handle types from a parsed VK.XML."
  (let ((handle-types (make-hash-table :test 'equal)))
    (xpath:do-node-set (node (xpath:evaluate "/registry/types/type[(@category=\"handle\")]" vk.xml))
      (let ((name (xps (xpath:evaluate "name" node))))
        (setf (gethash name handle-types) t)))
    handle-types))

(defun parse-types (vk.xml vk-spec)
  "Extracts types and their alias names from a parsed VK.XML and stores them in the given VK-SPEC instance."
  ;; todo:? VK_DEFINE_HANDLE VK_DEFINE_NON_DISPATCHABLE_HANDLE
  ;; #define VK_NULL_HANDLE 0
  (xpath:do-node-set (node (xpath:evaluate "/registry/types/type[not(apientry) and not(@category=\"include\") and not(@category=\"define\")]" vk.xml))
    (let ((name (xps (xpath:evaluate "name" node)))
          (@name (xps (xpath:evaluate "@name" node)))
          (alias (xps (xpath:evaluate "@alias" node)))
          (type (xps (xpath:evaluate "type" node)))
          (type* (mapcar 'xpath:string-value (xpath:all-nodes (xpath:evaluate "type" node))))
          (category (xps (xpath:evaluate "@category" node)))
          (parent (xps (xpath:evaluate "@parent" node)))
          (requires (xps (xpath:evaluate "@requires" node)))
          (comment (xps (xpath:evaluate "@comment" node)))
          (returnedonly (xps (xpath:evaluate "@returnedonly" node)))
          (attribs (attrib-names node))
          ;; required since v1.2.140 which changes some "define" types to "basetype" types with prefix "struct"
          (prefix (xps (xpath:evaluate "type/preceding-sibling::text()" node))))      
      ;; make sure nobody added any attributes we might care about
      (assert (not (set-difference attribs                                   
                                   ;; todo: provide set per version-tag
                                   '("name" "category" "parent" "requires" "comment"
                                     ;; todo: the attributes below are not handled yet
                                     "returnedonly"
                                     ;; todo: only from v1.0.55-core
                                     "structextends"
                                     ;; todo: only from v1.1.70
                                     "alias"
                                     ;; todo: only from v1.2.140
                                     "allowduplicate")
                                   :test 'string=)))
      (cond
        ((string= requires "vk_platform")
         ;; make sure we have a mapping for everything in vk_platform.h
         (assert (gethash @name *vk-platform*)))
        ((and requires (search ".h" requires))
         ;; and make a note of other missing types
         ;; (not sure if we will need definitions for these or not?)
         (unless (gethash @name *vk-platform*)
           (format t "Unknown platform type ~s from ~s (~s)?~%" @name requires name)))
        (alias
         (push @name (alias-names vk-spec))
         (warn "ALIAS NOT HANDLED YET: ~s is alias for ~s and has category ~a~%" @name alias category))
        ((and (string= category "basetype")
              (not (string= prefix "typedef")))
         (format t "Skipping opaque type: ~s~%" name))
        ((string= category "basetype")
         ;; basetypes
         (assert (and name type))
         (format t "new base type ~s -> ~s~%" name type)
         (format t "vk-platform ~a~%" (gethash type *vk-platform*))
         (format t "fix-type-name ~a~%" (fix-type-name type (vendor-ids vk-spec)))
         (format t "name ~a~%" (or name @name))
         (set-type vk-spec
                   (or name @name)
                   (list :basetype (or (gethash type *vk-platform*)
                                       (fix-type-name type (vendor-ids vk-spec))))))
        ((string= category "bitmask")
         (format t "new bitmask ~s -> ~s~%  ~s~%" name type
                 (mapcar 'xps (xpath:all-nodes (xpath:evaluate "@*" node))))
         (setf (gethash name (bitfields vk-spec)) (list requires :type type))
         (set-type vk-spec
                   (or name @name)
                   (list :bitmask type)))
        ((string= category "handle")
         (let ((dispatch (cond
                           ((string= type "VK_DEFINE_HANDLE")
                            :handle)
                           ((string= type "VK_DEFINE_NON_DISPATCHABLE_HANDLE")
                            :non-dispatch-handle)
                           (t
                            (error "unknown handle type ~s?" type)))))
           (format t "new handle ~s / ~s ~s~%" parent name type)
           (set-type vk-spec
                     (or name @name)
                     (list dispatch type))))
        ((string= category "enum")
         (assert (not (or requires type name parent)))
         (format t "new enum type ~s ~s~%" @name type)
         (set-type vk-spec
                   (or name @name)
                   (list :enum type)))
        ((string= category "funcpointer")
         (format t "new function pointer type ~s ~s~%" name type*)
         (let* ((types (mapcar 'xps (xpath:all-nodes (xpath:evaluate "type" node))))
                (before-name (xps (xpath:evaluate "name/preceding-sibling::text()" node)))
                (rt (ppcre:regex-replace-all "(^typedef | \\(VKAPI_PTR \\*$)"
                                             before-name ""))
                (before (xpath:all-nodes (xpath:evaluate "type/preceding-sibling::text()" node)))
                (after (xpath:all-nodes (xpath:evaluate "type/following-sibling::text()" node)))
                (args (loop for at in types
                            for a in after
                            for b in before
                            for star = (count #\* (xps a))
                            for const = (search "const" (xps b))
                            for an = (ppcre:regex-replace-all "(\\*|\\W|,|const|\\)|;)" (xps a) "")
                            when (plusp star) do (assert (= star 1))
                            collect (list (format nil "~a~@[/const~]"
                                                  an const)
                                          (if (plusp star)
                                              `(:pointer ,(fix-type-name at (vendor-ids vk-spec)))
                                              (fix-type-name at (vendor-ids vk-spec)))))))
           (let ((c (count #\* rt)))
             (setf rt (fix-type-name (string-right-trim '(#\*) rt) (vendor-ids vk-spec)))
             (setf rt (or (gethash rt *vk-platform*) rt))
             (loop repeat c do (setf rt (list :pointer rt))))
           (set-type vk-spec
                     (or name @name)
                     (list :func :type (list rt args)))))
        ((or (string= category "struct")
             (string= category "union"))
         (let ((members nil))
           (xpath:do-node-set (member (xpath:evaluate "member" node))
             (push (parse-arg-type member
                                   (lambda (a) (gethash a (structs vk-spec)))
                                   (vendor-ids vk-spec)
                                   (api-constants vk-spec)
                                   (handle-types vk-spec)
                                   :stringify returnedonly)
                   members))
           (setf members (nreverse members))
           (format t "new ~s ~s: ~%~{  ~s~^~%~}~%"
                   category @name members)
           (set-type vk-spec
                     (or name @name)
                     `(,(if (string= category "struct")
                            :struct
                            :union)
                       , @name
                       :members ,members
                       :returned-only ,returnedonly
                       ,@(when comment (list :comment comment))))))
        (t
         (format t "unknown type category ~s for name ~s~%"
                 category (or name @name)))))))

(defun parse-enums (vk.xml vk-spec)
  "Extracts enums and their alias names from a parsed VK.XML and stores them in the given VK-SPEC instance."
  (xpath:do-node-set (node (xpath:evaluate "/registry/enums" vk.xml))
    (let* ((name (xps (xpath:evaluate "@name" node)))
           (comment (xps (xpath:evaluate "@comment" node)))
           (type (xps (xpath:evaluate "@type" node)))
           (expand (xps (xpath:evaluate "@expand" node)))
           (namespace (xps (xpath:evaluate "@namespace" node)))
           (attribs  (attrib-names node))
           (enums (xpath:all-nodes (xpath:evaluate "enum" node)))
           (enum-type (get-type vk-spec name)))     
      ;; make sure nobody added any attributes we might care about
      (assert (not (set-difference attribs
                                   '("namespace" "name"
                                     "type" "expand" "comment")
                                   :test 'string=)))
      (unless (string= name "API Constants")
        ;; v1.1.124 adds VkSemaphoreCreateFlagBits enum type which is missing in enum section and must be created here
        (if (and (not enum-type)
                 (string= name "VkSemaphoreCreateFlagBits"))
            (progn
              (push (cons name (list :enum nil)) (types vk-spec))
              (format t "new enum type ~s~%" name)
              (setf enum-type (get-type vk-spec name)))
            (assert (get-type vk-spec  name))))
      (assert (not (second enum-type)))
      (loop for enum in enums
            for name2 = (xps (xpath:evaluate "@name" enum))
            for value = (numeric-value (xps (xpath:evaluate "@value" enum)))
            for bitpos = (numeric-value (xps (xpath:evaluate "@bitpos" enum)))
            for comment2 = (xps (xpath:evaluate "@comment" enum))
            ;; since v1.1.83
            for alias = (xps (xpath:evaluate "@alias" enum))
            unless (string= name "API Constants")
            do (if alias
                   (warn "ALIAS NOT YET HANDLED: ~s is an alias for ~s" name2 alias)
                   (progn
                     (assert (not (and bitpos value)))
                     (assert (or bitpos value))
                     (push `(,name2 ,(or value (ash 1 bitpos))
                                    ,@(when comment2 (list :comment comment2)))
                           (second enum-type)))))
      (when (second enum-type)
        (setf (second enum-type)
              (nreverse (second enum-type))))
      (when type
        (setf (getf (cddr enum-type) :type) (make-keyword type))
        (format t "add bitmask ~s ~s~%" name type)
        (when (and (string= type "bitmask")
                   (not (gethash name (bitfields vk-spec))))
          (setf (gethash name (bitfields vk-spec))
                (list nil :type nil))))
      (when expand
        (setf (getf (cddr enum-type) :expand) expand))
      (when namespace
        (setf (getf (cddr enum-type) :namespace) namespace)))))

(defun parse-commands (vk.xml vk-spec)
  "Extracts commands and their alias names from a parsed VK.XML and stores them in the given VK-SPEC instance."
  (xpath:do-node-set (node (xpath:evaluate "/registry/commands/command" vk.xml))
    (let* ((name (xps (xpath:evaluate "proto/name" node)))
           (type (xps (xpath:evaluate "proto/type" node)))
           (alias (xps (xpath:evaluate "@alias" node)))
           (@name (xps (xpath:evaluate "@name" node)))
           #++(proto (xpath:evaluate "proto" node))
           (.params (xpath:all-nodes (xpath:evaluate "param" node)))
           (successcodes (xps (xpath:evaluate "@successcodes" node)))
           (errorcodes (xps (xpath:evaluate "@errorcodes" node)))
           (queues (xps (xpath:evaluate "@queues" node)))
           (cmdbufferlevel (xps (xpath:evaluate "@cmdbufferlevel" node)))
           (renderpass (xps (xpath:evaluate "@renderpass" node)))
           (pipeline (xps (xpath:evaluate "@pipeline" node)))
           (attribs (attrib-names node)))
      ;; make sure nobody added any attributes we might care about
      (assert (not (set-difference attribs
                                   '("successcodes" "errorcodes" "queues"
                                     "cmdbufferlevel" "renderpass"
                                     ;; todo:
                                     "pipeline" "comment"
                                     ;; todo: only from v1.1.70
                                     "alias" "name")
                                   :test 'string=)))
      (if alias
          (progn
            (push @name (alias-names vk-spec))
            (warn "ALIAS NOT HANDLED YET: ~s is alias for ~s~%" @name alias))
          (let ((params
                  (loop for p in .params
                        for optional = (xps (xpath:evaluate "@optional" p))
                        for externsync = (xps (xpath:evaluate "@externsync" p))
                        for len = (xps (xpath:evaluate "@len" p))
                        for noautovalidity = (xps (xpath:evaluate "@noautovalidity" p))
                        for desc = (parse-arg-type p
                                                   (lambda (a) (gethash a (structs vk-spec)))
                                                   (vendor-ids vk-spec)
                                                   (api-constants vk-spec)
                                                   (handle-types vk-spec)
                                                   :stringify t)
                        for attribs = (attrib-names p)
                        do
                        (assert (not (set-difference attribs
                                                     '("optional" "externsync"
                                                       "len" "noautovalidity")
                                                     :test 'string=)))
                        collect `(,desc
                                  ,@(when optional (list :optional optional))
                                  ,@(when len (list :len len))
                                  ,@(when noautovalidity (list :noautovalidity noautovalidity))
                                  ,@(when externsync (list :externsync externsync))))))
            (flet ((kw-list (x &key (translate #'make-keyword))
                     (mapcar translate
                             (split-sequence:split-sequence #\, x :remove-empty-subseqs t))))
              (setf (gethash name (functions vk-spec))
                    (list (or (gethash type *vk-platform*) type)
                          params
                          :success (kw-list successcodes
                                            :translate 'make-const-keyword)
                          :errors (kw-list errorcodes
                                           :translate 'make-const-keyword)
                          :queues (kw-list queues)
                          :command-buffer-level (kw-list cmdbufferlevel)
                          :renderpass (kw-list renderpass)))))))))

(defun parse-extensions (vk.xml vk-spec)
  "Extracts extensions and their alias names from a parsed VK.XML and stores them in the given VK-SPEC instance."
  ;; mostly just expanding the enums, since the new struct/functions
  ;; definitions are included with core definitions earlier.
  ;; probably will eventually want to mark which names go with which
  ;; version/extension though.
  (xpath:do-node-set (node (xpath:evaluate "/registry/extensions/extension/require/enum" vk.xml))
    (let* ((ext (xps (xpath:evaluate "../../@name" node)))
           (ext-number (parse-integer
                        (xps (xpath:evaluate "../../@number" node))))
           (api (xps (xpath:evaluate "../../@supported" node)))
           (value (xps (xpath:evaluate "@value" node)))
           (.name (xps (xpath:evaluate "@name" node)))
           (name (make-const-keyword .name))
           (alias (xps (xpath:evaluate "@alias" node)))
           (extends (xps (xpath:evaluate "@extends" node)))
           (offset (xps (xpath:evaluate "@offset" node)))
           (bitpos (xps (xpath:evaluate "@bitpos" node)))
           (dir (xps (xpath:evaluate "@dir" node)))
           (attribs (attrib-names node)))
      (assert (not (set-difference attribs
                                   '("value" "name" "extends" "offset" "dir" "bitpos"
                                     ;; todo:
                                     "comment"
                                     ;; todo: only from v1.1.70
                                     "extnumber" "alias")
                                   :test 'string=)))
      (if alias
          (progn
            (push .name (alias-names vk-spec))
            (warn "ALIAS NOT HANDLED YET: ~s is alias for ~s~%" .name alias))
          (progn
            (when (and (not extends)
                       (alexandria:ends-with-subseq "_EXTENSION_NAME" .name))
              ;; todo: do something with the version/ext name enums
              (setf (gethash ext (extension-names vk-spec))
                    (ppcre:regex-replace-all "&quot;" value "")))
            (when extends
              (let ((extend (get-type vk-spec extends)))
                (assert (or (and offset (not value) (not bitpos))
                            ;; this was a special case for (string= .name "VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAM_TO_EDGE") until version v1.1.70
                            (and (not offset) value (not bitpos))
                            (and (not offset) (not value) bitpos)))
                (setf (getf extend :enum)
                      (append (getf extend :enum)
                              (list (list .name (*
                                                 (if (equalp dir "-")
                                                     -1
                                                     1)
                                                 (or (and offset (+ +ext-base+
                                                                    (* +ext-block-size+ (1- ext-number))
                                                                    (parse-integer offset)))
                                                     (and value (parse-integer value))
                                                     (and bitpos (ash 1 (parse-integer bitpos)))))
                                          :ext (format nil "~a" ext)))))))
            (format t "ext: ~s ~s ~s ~s ~s~%" value name extends (or offset value bitpos) dir))))))

(defun parse-extension-functions (vk.xml vk-spec)
  "Extracts extension functions and their alias names from a parsed VK.XML and stores them in the given VK-SPEC instance."
  (xpath:do-node-set (node (xpath:evaluate "/registry/extensions/extension/require/command" vk.xml))
    (let* ((ext (xps (xpath:evaluate "../../@name" node)))
           (name (xps (xpath:evaluate "@name" node)))
           (attribs (attrib-names node)))
      (if (find name (alias-names vk-spec) :test #'string=)
          (warn "ALIAS NOT HANDLED YET: ~s is an alias~%" name) 
          (progn
            (assert (not (set-difference attribs
                                         '("name")
                                         :test 'string=)))
            (assert (gethash name (functions vk-spec)))
            (setf (getf (cddr (gethash name (functions vk-spec))) :ext)
                  ext)
            (format t "extf: ~s ~s~%" name ext))))))

(defun parse-vk-xml (version vk-xml-pathname)
  "Parses the vk.xml file at VK-XML-PATHNAME into a VK-SPEC instance."
  (let* ((vk.xml (cxml:parse-file vk-xml-pathname
                                  (cxml:make-whitespace-normalizer
                                   (stp:make-builder))))
         (vk-spec (make-instance 'vk-spec
                                 :version version
                                 :copyright (parse-copyright-notice vk.xml)
                                 :vendor-ids (extract-vendor-ids vk.xml) ;; extract tags / vendor-ids
                                 :vk-api-version (parse-api-version vk.xml) ;; extract version info
                                 :api-constants (parse-api-constants vk.xml) ;; extract "API constants" enum first too for member array sizes
                                 :handle-types (parse-handle-types vk.xml) ;; extract handle types so we can mark them as pointers for translators
                                 )))
    (parse-structs vk.xml vk-spec) ;; extra pass to find struct/unions so we can mark them correctly in member types
    (parse-types vk.xml vk-spec)
    (parse-enums vk.xml vk-spec)
    (parse-commands vk.xml vk-spec)
    (parse-extensions vk.xml vk-spec)
    (parse-extension-functions vk.xml vk-spec) ;; and also mark which functions are from extensions
    (setf (types vk-spec) (nreverse (types vk-spec)))
    vk-spec))
