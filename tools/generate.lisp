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
(require :asdf)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-systems 'alexandria 'cl-ppcre 'split-sequence
                     'cxml 'xpath 'cxml-stp 'cffi))


(defvar *base-dir* (make-pathname :directory (pathname-directory
                                              (or *compile-file-pathname*
                                                  *load-pathname*))))

(defparameter *in-package-name* "cl-vulkan-bindings")
(defparameter *package-nicknames* "#:%vk")
(defparameter *core-definer* "defvkfun")
(defparameter *ext-definer* "defvkextfun")

(defparameter *vk-api-version* nil) ;; (major minor patch)
(defparameter *vk-last-updated* nil)
;; should we store a git hash or something like the svn version in cl-opengl?

(defparameter *api-constants* (make-hash-table :test 'equal))

(defparameter *foo* nil)

(defparameter *vk-platform*
  (alexandria:plist-hash-table '("void" :void
                                 "char" :char
                                 "float" :float
                                 "uint8_t" :uint8
                                 "uint32_t" :uint32
                                 "uint64_t" :uint64
                                 "int32_t" :int32
                                 "size_t" size-t)
                               :test 'equal))
(defparameter *opaque-types*
  '("a-native-window" "mir-connection" "mir-surface"
    "xcb_connection_t" "display"))
(defparameter *opaque-struct-types*
  '("wl_display" "wl_surface"))
(defparameter *misc-os-types*
  '("hinstance" (:pointer :void)
    "hwnd" (:pointer :void)
    "xcb_window_t" :uint32
    "xcb_visualid_t" :uint32
    "window" :ulong
    "visual-id" :ulong))

(defparameter *vendor-ids* '("KHR" "EXT")) ;; todo: read from file
(defparameter *special-words* '("Bool32" "Win32"
                                "16" "32" "64"
                                "3d" "2d" "1d"
                                "3D" "2D" "1D"
                                "ID" "UUID"
                                "HINSTANCE" "HWND"
                                "ETC2" "ASTC" "ASTC_" "LDR" "BC"))

(defparameter *fix-must-be*
  (alexandria:alist-hash-table
   '((:pipeline-iinput-assembly-state-create-info
      . :pipeline-input-assembly-state-create-info))))
;; not sure if we should remove the type prefixes in struct members or
;; not?
;;(defparameter *type-prefixes* '("s-" "p-" "pfn-" "pp-"))

(defun xps (node)
  (let ((s (string-trim '(#\space #\tab) (xpath:string-value node))))
    (unless (string= s "") s)))

(defun numeric-value (str)
  (cond
    ((not str) nil)
    ((alexandria:starts-with-subseq "0x" str)
     (parse-integer str :start 2 :radix 16))
    ((ignore-errors (parse-integer str)))
    ((and (alexandria:ends-with #\f str)
          (ignore-errors (parse-number:parse-number str :end (1- (length str))))))
    ((multiple-value-bind (m matches)
         (ppcre:scan-to-strings "\\(~0U(LL)?\\)" str)
       (when m
         ;; fixme: is this right on all platforms? (or any for that matter?)
         (if (aref matches 0)
             (ldb (byte 64 0) -1)
             (if (= 4 (cffi:foreign-type-size :uint))
                 (ldb (byte 32 0) -1)
                 (ldb (byte 64 0) -1))))))
    (t
     (error "~s" str))))

(defun fix-type-name (name)
  (if (not (stringp name))
      name
      (let* ((start (if (alexandria:starts-with-subseq "Vk" name) 2 0)))
        (when (zerop start)
          (setf name (ppcre:regex-replace-all "PFN_vk" name "Pfn")))
        (cffi:translate-camelcase-name (subseq name start)
                                       :special-words (append *special-words*
                                                              *vendor-ids*)))))

(defun fix-function-name (name)
  (let* ((start (if (alexandria:starts-with-subseq "vk" name) 2 0)))
    (cffi:translate-camelcase-name (subseq name start)
                                   :special-words (append *special-words*
                                                          *vendor-ids*))))

(defun parse-arg-type (node gt &key stringify)
  (let* ((type-node (xpath:evaluate "type" node))
         (.type (xps type-node))
         (len (xps (xpath:evaluate "@len" node)))
         (optional (xps (xpath:evaluate "@optional" node)))
         (name (cffi:translate-camelcase-name
                (xps (xpath:evaluate "name" node))
                :special-words *special-words*))
         (type (or (gethash .type *vk-platform*) (fix-type-name .type)))
         (prefix (xps (xpath:evaluate "type/preceding-sibling::text()" node)))
         (suffix (xps (xpath:evaluate "type/following-sibling::text()" node)))
         (namesuf (xps (xpath:evaluate "name/following-sibling::text()" node)))
         (enum (xps (xpath:evaluate "enum" node)))
         (following (xps (xpath:evaluate "following-sibling::comment()" node)))
         (desc))
    (assert (not (set-difference (attrib-names node)
                                 '("len" "optional"
                                   ;; todo:
                                   "noautovalidity" "externsync")
                                 :test 'string=)))
    ;; just hard coding the const/pointer stuff for
    ;; now. adjust as the spec changes...
    (when prefix
      (assert (member prefix '("struct" "const") :test 'string=)))
    (when suffix
      (assert (member suffix '("*" "**" "* const*") :test 'string=)))
    ;; fixme: do something with the const? (generate comment if nothing else)
    (when gt
      (format t "@@@~s/~s (~s ~s)~%  -> ~s~%"
              name .type prefix suffix
              (funcall gt type)))
    (flet ((struct-type ()
             (if (and gt
                      (member (funcall gt type)
                              '(:struct :union)))
                 (list (funcall gt type) type)
                 type)))
      (cond
        ((and (or (not prefix)
                  (string= prefix "const"))
              (not suffix))
         (setf desc (list name (struct-type))))
        ((and (string= prefix "struct")
              (string= suffix "*"))
         (setf desc `(,name (:pointer (:struct ,type)))))
        ((and (or (not prefix)
                  (string= prefix "const"))
              (string= suffix "*"))
         (setf desc `(,name (:pointer ,(struct-type)))))
        ((and (string= prefix "const")
              (string= suffix "* const*"))
         (setf desc `(,name (:pointer (:pointer ,(struct-type))))))
        ((and (not prefix)
              (string= suffix "**"))
         (setf desc `(,name (:pointer (:pointer ,(struct-type))))))
        (t
         (error "failed to translate type ~s ~s ~s?" prefix type suffix)
         #++(setf desc (list :??? name type prefix suffix)))))
    (cond
      ((and stringify
            (equalp (second desc) '(:pointer :char))
            (string= len "null-terminated"))
       (setf (second desc) :string))
      #++((string= len "null-terminated")
       (error "unhandled len=~s" len)))
    (ppcre:register-groups-bind (must x)
        ("([Mm]ust be|[Ss]hould be) ([A-Z0-9_]+)" following)
      (declare (ignore must)) ;; not sure if there is a difference
                              ;; between "must be" and "should be"?
      (when x
        ;; fixme: handle "Must be" for other types?
        ;; need to figure out how much to remove from name first,
        ;; possibly store string -> symbol mapping somewhere?
        (when (alexandria:starts-with-subseq "VK_STRUCTURE_TYPE_" x)
          (let ((k (make-keyword
                    (substitute #\- #\_ (subseq x (length "VK_STRUCTURE_TYPE_"))))))
            (setf (getf (cddr desc) :must-be) (gethash k *fix-must-be* k))))))
    (when (or (find type *opaque-types* :test 'string-equal)
              (find type *opaque-struct-types* :test 'string-equal)
              (eql type :void))
      (format t "  opaque!~%")
      (setf (getf (cddr desc) :opaque) t))
    (when len
      (setf (getf (cddr desc) :len)
            (mapcar (lambda (a)
                      (cond
                        ((string= a "null-terminated")
                         :null-terminated)
                        ((alexandria:starts-with-subseq "latexmath:" a)
                         a)
                        ;; try to catch unmarked equations
                        ((ppcre:scan "[:+-/*]" a)
                         a)
                        (t
                         (make-keyword
                          (cffi:translate-camelcase-name
                           a :special-words *special-words*)))))
                    (split-sequence:split-sequence #\, len))))
    (when optional
      (setf (getf (cddr desc) :optional)
            (mapcar 'make-keyword
                    (split-sequence:split-sequence #\, optional))))
    (when enum
      (assert (gethash enum *api-constants*)))
    (if enum
        (push (gethash enum *api-constants*) (cddr desc))
        (push nil (cddr desc)))
    (ppcre:register-groups-bind (x) ("\\[(\\d+)\\]" namesuf)
      (when x
        (setf (caddr desc) (parse-integer x))))
    desc))

(defun attrib-names (node)
  (mapcar 'cxml-stp:local-name (cxml-stp:list-attributes node)))

(defun make-keyword (name)
  (alexandria:make-keyword (string-upcase name)))

(defun make-const-keyword (name)
  (let ((start (if (alexandria:starts-with-subseq "VK_" name) 3 0)))
    (alexandria:make-keyword
     (subseq (substitute #\- #\_ name) start))))

(defun fix-bit-name (name &key (prefix "VK_"))
  ;; fixme: cache compiled regex instead of rebuilding from string on every call
  (substitute #\- #\_
              (ppcre:regex-replace-all (format nil "(^~a|_BIT(~{_~a~^|~})?$)"
                                               prefix *vendor-ids*)
                                       name "")))

;; read from data files
(let* ((this-dir *base-dir*)
       (relative-vk (make-pathname :directory '(:relative :up "vk")))
       (vk-dir (merge-pathnames relative-vk this-dir))
       (relative-spec (make-pathname :directory '(:relative :up "spec")))
       (spec-dir (merge-pathnames relative-spec this-dir))
       (binding-package-file (merge-pathnames "bindings-package.lisp" vk-dir))
       (translators-file (merge-pathnames "translators.lisp" vk-dir))
       (types-file (merge-pathnames "types.lisp" vk-dir))
       (funcs-file (merge-pathnames "funcs.lisp" vk-dir))
       #++(name-map (read-name-map vk-dir))
       #++(types (read-known-types vk-dir))
       (vk.xml (cxml:parse-file (merge-pathnames "vk.xml" spec-dir)
                                (cxml:make-whitespace-normalizer
                                 (stp:make-builder))))
       (copyright (xpath:string-value
                   (xpath:evaluate "/registry/comment" vk.xml)))
       (bitfields (make-hash-table :test 'equal))
       #++(types (alexandria:copy-hash-table *vk-platform* :test 'equal))
       (types nil) ;; structs are ordered, so use an alist (actually need to order by hand anyway, so probably should switch back to hash)
       (enums (make-hash-table :test 'equal))
       (structs (make-hash-table :test 'equal))
       (funcs (make-hash-table :test 'equal))
       (function-apis (make-hash-table :test 'equal))
       #++(old-bindings (load-bindings vk-dir))
       #++(old-enums (load-enums vk-dir)))
  (flet ((get-type (name)
           (cdr (assoc name types :test 'string=)))
         (get-type/f (name)
           (cdr (assoc name types :test (lambda (a b)
                                          (equalp
                                           (fix-type-name a)
                                           (fix-type-name b)))))))
    ;; remove some other text from the comment if present
    (let ((s (search "This file, vk.xml, is the " copyright)))
      (when s (setf copyright (string-trim '(#\space #\tab #\newline #\return)
                                           (subseq copyright 0 s)))))
    ;; make sure we still have a copyright notice
    (assert (search "Copyright" copyright))

    ;; extract version info
    (let ((api (xpath:string-value
                (xpath:evaluate "/registry/types/type/name[.=\"VK_API_VERSION\"]/.." vk.xml))))
      ;; #define VK_API_VERSION VK_MAKE_VERSION(1, 0, 3)
      (setf *vk-api-version* (map 'list 'parse-integer
                                  (nth-value 1 (ppcre::scan-to-strings "\\((\\d+),\\W*(\\d+),\\W*(\\d+)\\)" api)))))

    ;; TODO:? extract vendorids
    ;; TODO:? extract tags

    ;; extra pass to find struct/unions so we can mark them correctly
    ;; in member types
    (xpath:do-node-set (node (xpath:evaluate "/registry/types/type[(@category=\"struct\") or (@category=\"union\")]" vk.xml))
      (let ((name (xps (xpath:evaluate "@name" node)))
            (category (xps (xpath:evaluate "@category" node))))
        (setf (gethash (fix-type-name name) structs)
              (make-keyword category))))

    ;; and extract "API constants" enum first too for member array sizes
    (xpath:do-node-set (enum (xpath:evaluate "/registry/enums[(@name=\"API Constants\")]/enum" vk.xml))
      (let ((name (xps (xpath:evaluate "@name" enum)))
            (value (numeric-value (xps (xpath:evaluate "@value" enum)))))
        (when (gethash name *api-constants*)
          (assert (= value (gethash name *api-constants*))))
        (setf (gethash name *api-constants*) value)))

    ;; extract types
    ;; todo:? VK_DEFINE_HANDLE VK_DEFINE_NON_DISPATCHABLE_HANDLE
    ;; #define VK_NULL_HANDLE 0
    (xpath:do-node-set (node (xpath:evaluate "/registry/types/type[not(apientry) and not(@category=\"include\") and not(@category=\"define\")]" vk.xml))
      (let ((name (xps (xpath:evaluate "name" node)))
            (@name (xps (xpath:evaluate "@name" node)))
            (type (xps (xpath:evaluate "type" node)))
            (type* (mapcar 'xpath:string-value (xpath:all-nodes (xpath:evaluate "type" node))))
            (category (xps (xpath:evaluate "@category" node)))
            (parent (xps (xpath:evaluate "@parent" node)))
            (requires (xps (xpath:evaluate "@requires" node)))
            (comment (xps (xpath:evaluate "@comment" node)))
            (returnedonly (xps (xpath:evaluate "@returnedonly" node)))
            (attribs (attrib-names node)))
        ;; make sure nobody added any attributes we might care about
        (assert (not (set-difference attribs
                                     '("name" "category" "parent" "requires"
                                       "comment"
                                       ;; todo:
                                       "returnedonly")
                                     :test 'string=)))
        (flet ((set-type (value)
                 (let ((name (or name @name)))
                   (if (get-type name)
                       (assert (equalp value (get-type name)))
                       (push (cons name value) types)))))
          (cond
            ((string= requires "vk_platform")
             ;; make sure we have a mapping for everything in vk_platform.h
             (assert (gethash @name *vk-platform*)))
            ((and requires (search ".h" requires))
             ;; and make a note of other missing types
             ;; (not sure if we will need definitions for these or not?)
             (unless (gethash @name *vk-platform*)
               (format t "Unknown platform type ~s from ~s (~s)?~%" @name requires name)))
            ((string= category "basetype")
             ;; type alias
             (assert (and name type))
             (format t "new base type ~s -> ~s~%" name type)
             (set-type (list :alias (or (gethash type *vk-platform*)
                                        (fix-type-name type)))))
            ((string= category "bitmask")
             (format t "new bitmask ~s -> ~s~%  ~s~%" name type
                     (mapcar 'xps (xpath:all-nodes (xpath:evaluate "@*" node))))
             (setf (gethash name bitfields) (list requires :type type))
             (set-type (list :bitmask type)))
            ((string= category "handle")
             (let ((dispatch (cond
                               ((string= type "VK_DEFINE_HANDLE")
                                :handle)
                               ((string= type "VK_DEFINE_NON_DISPATCHABLE_HANDLE")
                                :non-dispatch-handle)
                               (t
                                (error "unknown handle type ~s?" type)))))
               (format t "new handle ~s / ~s ~s~%" parent name type)
               (set-type (list dispatch type))))
            ((string= category "enum")
             (assert (not (or requires type name parent)))
             (format t "new enum type ~s~%" @name)
             (set-type (list :enum type)))
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
                                                    `(:pointer ,(fix-type-name at))
                                                    (fix-type-name at))))))
               (let ((c (count #\* rt)))
                 (setf rt (fix-type-name (string-right-trim '(#\*) rt)))
                 (setf rt (or (gethash rt *vk-platform*) rt))
                 (loop repeat c do (setf rt (list :pointer rt))))
               (set-type (list :func :type (list rt args)))))
            ((or (string= category "struct")
                 (string= category "union"))
             (let ((members nil))
               (xpath:do-node-set (member (xpath:evaluate "member" node))
                 (push (parse-arg-type member (lambda (a) (gethash a structs))
                                       :stringify returnedonly)
                       members))
               (setf members (nreverse members))
               (format t "new ~s ~s: ~%~{  ~s~^~%~}~%"
                       category @name members)
               (set-type `(,(if (string= category "struct")
                                :struct
                                :union)
                           , @name
                           :members ,members
                           :returned-only ,returnedonly
                           ,@(when comment (list :comment comment))))))
            (t
             (error "unknown type category ~s for name ~s~%"
                    category (or name @name)))))))

;;; enums*
    (xpath:do-node-set (node (xpath:evaluate "/registry/enums" vk.xml))
      (let* ((name (xps (xpath:evaluate "@name" node)))
             (comment (xps (xpath:evaluate "@comment" node)))
             (type (xps (xpath:evaluate "@type" node)))
             (expand (xps (xpath:evaluate "@expand" node)))
             (namespace (xps (xpath:evaluate "@namespace" node)))
             (attribs  (attrib-names node))
             (enums (xpath:all-nodes (xpath:evaluate "enum" node)))
             (enum-type (get-type name)))
        ;; make sure nobody added any attributes we might care about
        (assert (not (set-difference attribs
                                     '("namespace" "name"
                                       "type" "expand" "comment")
                                     :test 'string=)))
        (unless (string= name "API Constants")
          (assert (get-type name)))
        (assert (not (second enum-type)))
        (loop for enum in enums
              for name2 = (xps (xpath:evaluate "@name" enum))
              for value = (numeric-value (xps (xpath:evaluate "@value" enum)))
              for bitpos = (numeric-value (xps (xpath:evaluate "@bitpos" enum)))
              for comment2 = (xps (xpath:evaluate "@comment" enum))
              unless (string= name "API Constants")
                do (assert (not (and bitpos value)))
                   (assert (or bitpos value))
                   (push `(,name2 ,(or value (ash 1 bitpos))
                                  ,@(when comment2 (list :comment comment2)))
                         (second enum-type)))
        (when (second enum-type)
          (setf (second enum-type)
                (nreverse (second enum-type))))
        (when type
          (setf (getf (cddr enum-type) :type) (make-keyword type))
          (format t "add bitmask ~s ~s~%" name type)
          (when (and (string= type "bitmask")
                     (not (gethash name bitfields)))
            (setf (gethash name bitfields)
                  (list nil :type nil))))
        (when expand
          (setf (getf (cddr enum-type) :expand) expand))
        (when namespace
          (setf (getf (cddr enum-type) :namespace) namespace))))
;;; commands
    (xpath:do-node-set (node (xpath:evaluate "/registry/commands/command" vk.xml))
      (let* ((name (xps (xpath:evaluate "proto/name" node)))
             (type (xps (xpath:evaluate "proto/type" node)))
             #++(proto (xpath:evaluate "proto" node))
             (.params (xpath:all-nodes (xpath:evaluate "param" node)))
             (successcodes (xps (xpath:evaluate "@successcodes" node)))
             (errorcodes (xps (xpath:evaluate "@errorcodes" node)))
             (queues (xps (xpath:evaluate "@queues" node)))
             (cmdbufferlevel (xps (xpath:evaluate "@cmdbufferlevel" node)))
             (renderpass (xps (xpath:evaluate "@renderpass" node)))
             (attribs (attrib-names node)))
        ;; make sure nobody added any attributes we might care about
        (assert (not (set-difference attribs
                                     '("successcodes" "errorcodes" "queues"
                                       "cmdbufferlevel" "renderpass")
                                     :test 'string=)))
        (let ((params
                (loop for p in .params
                      for optional = (xps (xpath:evaluate "@optional" p))
                      for externsync = (xps (xpath:evaluate "@externsync" p))
                      for len = (xps (xpath:evaluate "@len" p))
                      for noautovalidity = (xps (xpath:evaluate "@noautovalidity" p))
                      for desc = (parse-arg-type p (lambda (a)
                                                     (gethash a structs))
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
            (setf (gethash name funcs)
                  (list (or (gethash type *vk-platform*) type)
                        params
                        :success (kw-list successcodes
                                          :translate 'make-const-keyword)
                        :errors (kw-list errorcodes
                                         :translate 'make-const-keyword)
                        :queues (kw-list queues)
                        :command-buffer-level (kw-list cmdbufferlevel)
                        :renderpass (kw-list renderpass)))))))

;;; TODO: feature
;;; TODO: extensions
    (setf types (nreverse types))

    ;; write types file
    (with-open-file (out types-file :direction :output :if-exists :supersede)
      (format out ";;; this file is automatically generated, do not edit~%")
      (format out "#||~%~a~%||#~%~%" copyright)
      (format out "(in-package #:cl-vulkan-bindings)~%~%")
      ;; type aliases
      (loop for (name . attribs) in (remove-if-not
                                     (lambda (x)
                                       (and (consp (cdr x))
                                            (eql (second x) :alias)))
                                     types)
            do (format out "~((defctype ~a ~s)~)~%~%"
                       (fix-type-name name) (second attribs)))
      (format out "(defctype handle :pointer)~%")
      (format out "#.(if (= 8 (foreign-type-size :pointer))~%  '(defctype non-dispatch-handle :pointer)~%  '(defctype non-dispatch-handle :uint64))~%~%")
      ;; misc OS types that are just passed around as pointers
      (loop for name in *opaque-types*
            ;; fixme: is there a better type to use here? or use empty struct?
            do (format out "~((defctype ~a :void)~)~%~%"
                       (fix-type-name name)))
      (loop for (name type) on *misc-os-types* by #'cddr
            do (format out "~((defctype ~a ~s)~)~%~%"
                       (fix-type-name name) type))
      (loop for name in *opaque-struct-types*
            do (format out "~((defcstruct ~a)~)~%~%"
                       (fix-type-name name)))
      ;; handles
      (loop for (name . attribs) in (remove-if-not
                                     (lambda (x)
                                       (and (consp (cdr x))
                                            (member (second x)
                                                    '(:handle
                                                      :non-dispatch-handle))))
                                     types)
            ;; handles are pointers to foo_T struct
            ;; on 32bit platform, 'non-dispatch' handles are 64bit int,
            ;; otherwise pointer to foo_T struct
            do (format out "(~(defctype ~a ~a~))~%~%"
                       (fix-type-name name)
                       (car attribs)))
      ;; bitfields
      (loop for (name . attribs) in (sort (alexandria:hash-table-alist bitfields)
                                          'string< :key 'car)
            for base-type = (second (get-type name))
            for requires = (first attribs)
            for bits = (if (consp base-type)
                           base-type
                           (second (when requires
                                     (get-type requires))))
            for prefix = "VK_"
            for fixed-name = (string (fix-type-name name))
            do (format out "(defbitfield (~(~a~[ ~a~]~))" fixed-name
                       (when (stringp base-type) (fix-type-name base-type)))
               ;; possibly shouldn't strip prefix from things like
               ;; VK_QUERY_RESULT_64_BIT or VK_SAMPLE_COUNT_1_BIT where
               ;; only :64 or :1 is left?
               (let ((p (search "-FLAG" fixed-name)))
                 (when p
                   (setf prefix (format nil "VK_~a"
                                        (substitute #\_ #\- (subseq fixed-name 0 (1+ p)))))
                   (format t "prefix -> ~s~%" prefix)))
               (loop for ((k . v) . more) on bits
                     for comment = (getf (cdr v) :comment)
                     do (format out "~%  (:~(~a~) #x~x)"
                                (fix-bit-name k :prefix prefix)
                                (first v))
                     unless more
                       do (format out ")")
                     when comment
                       do (format out " ;; ~a" comment))
               (format out "~:[)~;~]~%~%" bits))

      ;; enums
      (loop for (name . attribs) in (sort (remove-if-not
                                           (lambda (x)
                                             (and (consp (cdr x))
                                                  (eql (second x) :enum)))
                                           types)
                                          'string< :key 'car)
            for type = (getf (cddr attribs) :type)
            for expand = (getf (cddr attribs) :expand)
            for requires = (getf (cddr attribs) :requires)
            for bits =  (second attribs)
            for prefix = "VK_"
            for fixed-name = (string (fix-type-name name))
            unless (or (eq type :bitmask)
                       (and (not bits)
                            (alexandria:ends-with-subseq "Bits" name)))
              do
                 (if (string-equal fixed-name "RESULT")
                     ;; work around cffi bug: cffi always uses unsigned
                     ;; type for enums, and VkResult has negative values
                     (format out "(defcenum (~(~a :int~))" fixed-name)
                     (format out "(defcenum (~(~a~))" fixed-name))
                 (when bits
                   ;; find longest prefix out of VK_, name - vendor, and expand
                   (when expand
                     (let ((l (loop for (k) in bits
                                    minimize (or (mismatch expand k) 0))))
                       (when (> l (length prefix))
                         (setf prefix expand)))
                     (let* ((p (loop for v in *vendor-ids*
                                       thereis (search v fixed-name)))
                            (n (format nil "VK_~a"
                                       (substitute #\_ #\-
                                                   (if p
                                                       (subseq fixed-name 0 p)
                                                       fixed-name))))
                            (l (loop for (k) in bits
                                     minimize (or (mismatch n k) 0))))
                       (when (> l (length prefix))
                         (setf prefix expand)))))
                 (loop for ((k . v) . more) on bits
                       for comment = (getf (cdr v) :comment)
                       do (format out "~%  (:~(~a~) ~:[#x~x~;~d~])"
                                  (string-trim '(#\-) (fix-bit-name k :prefix prefix))
                                  (minusp (first v)) (first v))
                       unless more
                         do (format out ")")
                       when comment
                         do (format out " ;; ~a" comment))
                 (format out "~:[)~;~]~%~%" bits))

      ;; function pointer types
      (loop for (name . attribs) in (sort (remove-if-not
                                           (lambda (x)
                                             (and (consp (cdr x))
                                                  (eql (second x) :func)))
                                           types)
                                          'string< :key 'car)
            do (format out "~( ~<;; ~@;~a~;~:>~%(defctype ~a :pointer)~)~%~%"
                       (list (cons "defcallback x" (getf (cdr attribs) :type)))
                       (fix-type-name name)))

      ;; structs/unions
      (loop with dumped = (make-hash-table :test 'equal)
            for (name . attribs) in (sort (remove-if-not
                                           (lambda (x)
                                             (and (consp (cdr x))
                                                  (member (second x)
                                                          '(:struct :union))))
                                           types)
                                          'string< :key 'car)
            do (labels
                   ((dump (name)
                      (if (and (consp name)
                               (member (car name) '(:pointer :struct :union)))
                          (dump (second name))
                          (when (and (gethash (fix-type-name name) structs)
                                     (not (gethash name dumped)))
                            (let* ((attribs (get-type/f name))
                                   (members (getf (cddr attribs) :members)))
                              (loop for (nil mt) in members
                                    do (dump mt))
                              (format out "(defc~(~a~) ~(~a~)" (first attribs)
                                      (fix-type-name name))
                              (format out
                                      "~{~%  ~1{(:~(~a ~s~@[ :count ~a~])~^#|~@{~a~^ ~}|#~)~}~}"
                                      members)
                              (format out "~:[)~;~]~%~%" nil)
                              (setf (gethash name dumped) t))))))
                 (dump name))))

    ;; write functions file
    (with-open-file (out funcs-file :direction :output :if-exists :supersede)
      (format out ";;; this file is automatically generated, do not edit~%")
      (format out "#||~%~a~%||#~%~%" copyright)
      (format out "(in-package #:cl-vulkan-bindings)~%~%")
      (loop for (name . attribs) in (sort (alexandria:hash-table-alist funcs)
                                          'string< :key 'car)
            for ret = (first attribs)
            for args = (second attribs)
            for success = (getf (cddr attribs) :success)
            for errors = (getf (cddr attribs) :errors)
            for queues = (getf (cddr attribs) :queues)
            for cbl = (getf (cddr attribs) :command-buffer-level)
            for ext = (getf (cddr attribs) :extension)
            do (format out "(~a (~s ~(~a) ~a~)"
                       (if ext *ext-definer* *core-definer*)
                       name
                       (fix-function-name name)
                       (if (keywordp ret) (format nil "~s" ret)
                           (fix-type-name ret)))
               (loop with *print-right-margin* = 10000
                     for (arg . opts) in args
                     do (format out "~&  ~1{~((~a ~s)~)~}" arg)
                     when opts do (format out " ;; ~{~s~^ ~}~%" opts))
               (format out ")~%~%")))

    ;; write package file
    (with-open-file (out binding-package-file
                         :direction :output :if-exists :supersede)
      (format out ";;; this file is automatically generated, do not edit~%")
      (format out "#||~%~a~%||#~%~%" copyright)
      (format out "(defpackage #:cl-vulkan-bindings~%  (:use #:cl #:cffi)~%")
      (format out "  (:nicknames #:%vk)~%")
      (format out "  (:export~%")
      (loop for (type . (typetype)) in (sort (copy-list types)
                                             'string< :key 'car)
            do (format out "~(    #:~a ;; ~s~)~%"
                       (fix-type-name type) typetype))
      (format out "~%")
      (loop for (func) in (sort (alexandria:hash-table-alist funcs)
                                'string< :key 'car)
            do (format out "~(    #:~a~)~%" (fix-function-name func)))
      (format out "))~%"))

    ;; write struct translators
    ;; possibly should do this while dumping struct types?
    (with-open-file (out translators-file
                         :direction :output :if-exists :supersede)
      (format out ";;; this file is automatically generated, do not edit~%")
      (format out "#||~%~a~%||#~%~%" copyright)
      (format out "(in-package #:cl-vulkan-bindings)~%~%")
      (loop for (name . attribs) in (sort (remove-if-not
                                           (lambda (x)
                                             (and (consp (cdr x))
                                                  (member (second x)
                                                          '(:struct))))
                                           types)
                                          'string< :key 'car)
            for members = (getf (cddr attribs) :members)
            do (format out "~((def-translator ~a (deref-~a ~:[:fill fill-~a~;~])~)~%"
                       (fix-type-name name)
                       (fix-type-name name)
                       (getf (cddr attribs) :returned-only)
                       (fix-type-name name))
                  (loop for m in members
                        do (format out "~&  ~((:~{~s~^ ~})~)" m))
               (format out ")~%~%")))

    ;; todo: print out changes
    (force-output)
    nil))
