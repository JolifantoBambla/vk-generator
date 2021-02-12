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

(in-package :vk-generator/writer)

(defun sorted-elements (vk-elements)
  "Returns a sorted list of VK-ELEMENT instances, sorted by their ID in increasing order."
  (sort vk-elements #'< :key #'id))

(defun sorted-names (vk-elements)
  "Returns a sorted list of names of VK-ELEMENT instances, sorted by their ID in increasing order.

See SORTED-ELEMENTS
"
  (map 'list #'name (sorted-elements vk-elements)))

(defun write-extension-names (out vk-spec)
  (format out "(defparameter *extension-names*~%  (alexandria:plist-hash-table~%    '(~{~(:~a~) ~s~^~%     ~})))~%~%"
          (loop for name in (sorted-names (alexandria:hash-table-values (extensions vk-spec)))
                collect (ppcre:regex-replace-all
                         "^VK-" (substitute #\- #\_ name) "")
                collect name)))

(defun write-base-types (out vk-spec)
  (loop for base-type in (sorted-elements (alexandria:hash-table-values (base-types vk-spec)))
        do (format out "~((defctype ~a ~s)~)~%~%"
                   (fix-type-name (name base-type) (tags vk-spec))
                   (gethash (type-name base-type) *vk-platform*)))
  (format out "(defctype handle :pointer)~%")
  (format out "#.(if (= 8 (foreign-type-size :pointer))~%  '(defctype non-dispatch-handle :pointer)~%  '(defctype non-dispatch-handle :uint64))~%~%")

  ;; todo: get these from VK-SPEC (e.g. (find-if (lambda (t) (and (= (category t) :basetype) (not (gethash (name t) (base-types vk-spec))))) (alexandria:hash-table-values (types vk-spec))) ... )
  ;; misc OS types that are just passed around as pointers
  (loop for name in *opaque-types*
        ;; fixme: is there a better type to use here? or use empty struct?
        do (format out "~((defctype ~a :void)~)~%~%"
                   (fix-type-name name (tags vk-spec))))
  (loop for (name type) on *misc-os-types* by #'cddr
        do (format out "~((defctype ~a ~s)~)~%~%"
                   (fix-type-name name (tags vk-spec)) type))
  (loop for name in *opaque-struct-types*
        do (format out "~((defcstruct ~a)~)~%~%"
                   (fix-type-name name (tags vk-spec)))))

(defun write-handles (out vk-spec)
  (loop for handle in (sorted-elements (alexandria:hash-table-values (handles vk-spec)))
        ;; TODO: after 6 months I have no idea if what I'm doing here is correct... thanks past-me
        ;; there are some functions without a handle, belonging to a handle with an empty string as its name
        when (not (string= (name handle) ""))
        
        ;; handles are pointers to foo_T struct
        ;; on 32bit platform, 'non-dispatch' handles are 64bit int,
        ;; otherwise pointer to foo_T struct
        do (format out "(~(defctype ~a ~a~))~%~%"
                   (fix-type-name (name handle) (tags vk-spec))
                   (if (non-dispatch-handle-p handle)
                       "non-dispatch-handle"
                       "handle"))))

(defun write-bitfields (out vk-spec)
  ;; NOTE: in the original generator bitfields where created for the flags (a type) and the flagbits (an enum)
  ;;       now we only write the flags using the flagbits as values and later write the flagbits as a separate enum
  ;;       I guess comparisons between the two should still work after this, but I'll have to check
  (loop for bitmask in (sorted-elements (alexandria:hash-table-values (bitmasks vk-spec)))
        for base-type = (type-name bitmask)
        for requires = (requires bitmask)
        for enum = (gethash requires (enums vk-spec))
        for bits = (if enum (enum-values (gethash requires (enums vk-spec))) '())
        for last-bit = (alexandria:lastcar bits)
        for prefix = "VK_"
        for fixed-name = (string (fix-type-name (name bitmask) (tags vk-spec)))
        do (format out "(defbitfield (~(~a~@[ ~a~]~))"
                   fixed-name
                   (when (stringp base-type) (fix-type-name base-type (tags vk-spec))))
           ;; possibly shouldn't strip prefix from things like
           ;; VK_QUERY_RESULT_64_BIT or VK_SAMPLE_COUNT_1_BIT where
           ;; only :64 or :1 is left?
           (let ((p (search "-FLAG" fixed-name)))
             (when p
               (setf prefix (format nil "VK_~a"
                                    (substitute #\_ #\- (subseq fixed-name 0 (1+ p)))))))
           (loop for enum-value in bits
                 for comment = (comment enum-value)
                 do (format out "~%  (:~(~a~) #x~x)"
                            (fix-bit-name (name enum-value) (tags vk-spec) :prefix prefix)
                            (number-value enum-value))
                 when (string= (name last-bit) (name enum-value))
                 do (format out ")")
                 when comment
                 do (format out " ;; ~a" comment))
           (format out "~:[)~;~]~%~%" bits)))

(defun write-enums (out vk-spec)
  (loop for enum in (sorted-elements (alexandria:hash-table-values (enums vk-spec)))
        ;; TODO: before 1.07 there was an "expand" attribute for enum tags
        for values = (enum-values enum)
        for last-value = (alexandria:lastcar values)
        for prefix = "VK_"
        for fixed-name = (string (fix-type-name (name enum) (tags vk-spec)))
        do
          ;; originally bitmasks were omitted
          ;; originally flagbits without values were omitted
          (if (string-equal fixed-name "RESULT")
              ;; work around cffi bug: cffi always uses unsigned
              ;; type for enums, and VkResult has negative values
              (format out "(defcenum (~(~a :int~))" fixed-name)
              (format out "(defcenum (~(~a~))" fixed-name))
          (when values
            ;; find longest prefix out of VK_, name - vendor
            (let* ((p (loop for v in (tags vk-spec)
                            thereis (when (and
                                           (>= (length fixed-name) (length v))
                                           (string= v (subseq fixed-name (- (length fixed-name) (length v)))))
                                      (search v fixed-name :from-end t))))
                   (n (format nil "VK_~a"
                              (substitute #\_ #\-
                                          (if p
                                              (subseq fixed-name 0 (- p 1))
                                              fixed-name))))
                   (l (loop for enum-value in values
                            minimize (or (mismatch n (name enum-value)) 0))))
              (when (> l (length prefix))
                (setf prefix (subseq n 0 l)))))
          (loop for enum-value in values
                for comment = (comment enum-value)
                do (format out "~%  (:~(~a~) ~:[#x~x~;~d~])"
                           (string-trim '(#\-) (fix-bit-name (name enum-value) (tags vk-spec) :prefix prefix))
                           (minusp (number-value enum-value)) (number-value enum-value))
                when (string= (name last-value) (name enum-value))
                do (format out ")")
                when comment
                do (format out " ;; ~a" comment))
          (format out "~:[)~;~]~%~%" values)
          (when (string-equal fixed-name "RESULT")
            ;; write out error->comment, since they seem useful
            ;; enough to print out to users in errors
            (format out "(defparameter *result-comments*~%  (alexandria:plist-hash-table~%    '(~{~(:~a~) ~s~^~%     ~})))~%~%"
                    (loop for enum-value in values
                          collect (string-trim '(#\-) (fix-bit-name (name enum-value) (tags vk-spec) :prefix prefix))
                          collect (comment enum-value))))))

(defun write-function-pointer-types (out vk-spec)
  ;; TODO: return type and argument types should be documented somewhere (need to be stored in func-pointer first...)
  (loop for func-pointer in (sorted-elements (alexandria:hash-table-values (func-pointers vk-spec)))
        do (format out "~((defctype ~a :pointer)~)~%~%"
                   (fix-type-name (name func-pointer) (tags vk-spec)))))

(defun write-structs (out vk-spec)
  (loop with dumped = (make-hash-table :test 'equal)
        for structure in (sorted-elements (alexandria:hash-table-values (structures vk-spec)))
        do (labels
               ((dump (struct)
                  (setf (gethash (name struct) dumped) t)
                  (loop for member-value in (member-values struct)
                        for member-type = (type-name (type-info member-value))
                        when (and (gethash member-type (structures vk-spec))
                                  (not (gethash (type-name (type-info member-value)) dumped)))
                        do (dump (gethash member-type (structures vk-spec))))
                  (format out "(defc~(~a~) ~(~a~)"
                          (if (is-union-p struct) "union" "struct")
                          (fix-type-name (name struct) (tags vk-spec)))
                  (loop for member-value in (member-values struct)
                        for name = (fix-type-name (name member-value) (tags vk-spec))
                        for member-type = (type-name (type-info member-value)) 
                        for array-count = (array-sizes member-value)
                        ;; TODO: what exactly should bit-count do? see VkAccelerationStructureInstanceKHR
                        do
                          (let ((primitive-type (gethash member-type *vk-platform*))
                                (fixed-type-name (fix-type-name member-type (tags vk-spec))))
                            (setf member-type
                                  (if (alexandria:starts-with-subseq "P-" (string name))
                                      (list :pointer
                                            (if primitive-type
                                                primitive-type
                                                (list :struct fixed-type-name)))
                                      (if primitive-type
                                          primitive-type
                                          fixed-type-name))))
                          (cond
                            ;; TODO: I'll probably have to fix the array size for multidimensional arrays
                            ((= (length array-count) 1)
                             (setf array-count
                                   (if (alexandria:starts-with-subseq "VK_" (first array-count))
                                       (fix-bit-name (first array-count) (tags vk-spec))
                                       (parse-integer (first array-count))))))
                          (format out "~%  ~1{(:~(~a ~s~@[ :count ~a~])~)~}"
                                       (list name member-type array-count)))
                  (format out "~:[)~;~]~%~%" nil)))
             (dump structure))))

(defun write-types-file (types-file vk-spec)
  (with-open-file (out types-file :direction :output :if-exists :supersede)
    (format out ";;; this file is automatically generated, do not edit~%")
    (format out "#||~%~a~%||#~%~%" (vulkan-license-header vk-spec))
    (format out "(in-package :~a)~%~%" *in-package-name*)
    
    (write-extension-names out vk-spec)
    (write-base-types out vk-spec)
    (write-handles out vk-spec)
    (write-bitfields out vk-spec)
    (write-enums out vk-spec)
    (write-function-pointer-types out vk-spec)
    (write-structs out vk-spec)))
