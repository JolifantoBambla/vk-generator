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

(in-package :vk-generator/write-types-file)


;;todo: replace get-type and get-type/f with functions from vk-spec
(defun get-type (name types)
  (cdr (assoc name types :test 'string=)))
(defun get-type/f (name types vendor-ids)
  (cdr (assoc name types :test (lambda (a b)
                                 (equalp
                                  (fix-type-name a vendor-ids)
                                  (fix-type-name b vendor-ids))))))


(defun write-extension-names (out extension-names)
  (format out "(defparameter *extension-names*~%  (alexandria:plist-hash-table~%    '(~{~(:~a~) ~a~^~%     ~})))~%~%"
          (loop for (k . v) in (alexandria:hash-table-alist extension-names)
                collect (ppcre:regex-replace-all
                         "^VK-" (substitute #\- #\_ k) "")
                collect v)))

(defun write-base-types (out types vendor-ids)
  (loop for (name . attribs) in (remove-if-not
                                 (lambda (x)
                                   (and (consp (cdr x))
                                        (eql (second x) :basetype)))
                                 types)
        do (format out "~((defctype ~a ~s)~)~%~%"
                   (fix-type-name name vendor-ids) (second attribs)))
  (format out "(defctype handle :pointer)~%")
  (format out "#.(if (= 8 (foreign-type-size :pointer))~%  '(defctype non-dispatch-handle :pointer)~%  '(defctype non-dispatch-handle :uint64))~%~%")
        ;; misc OS types that are just passed around as pointers
  (loop for name in *opaque-types*
        ;; fixme: is there a better type to use here? or use empty struct?
        do (format out "~((defctype ~a :void)~)~%~%"
                   (fix-type-name name vendor-ids)))
  (loop for (name type) on *misc-os-types* by #'cddr
        do (format out "~((defctype ~a ~s)~)~%~%"
                   (fix-type-name name vendor-ids) type))
  (loop for name in *opaque-struct-types*
        do (format out "~((defcstruct ~a)~)~%~%"
                   (fix-type-name name vendor-ids))))

(defun write-handles (out types vendor-ids)
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
                   (fix-type-name name vendor-ids)
                   (car attribs))))

(defun write-bitfields (out bitfields types vendor-ids)
  (loop for (name . attribs) in (sort (alexandria:hash-table-alist bitfields)
                                      'string< :key 'car)
        for base-type = (second (get-type name types))
        for requires = (first attribs)
        for bits = (if (consp base-type)
                       base-type
                       (second (when requires
                                 (get-type requires types))))
        for prefix = "VK_"
        for fixed-name = (string (fix-type-name name vendor-ids))
        do (format out "(defbitfield (~(~a~@[ ~a~]~))" fixed-name
                   (when (stringp base-type) (fix-type-name base-type vendor-ids)))
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
                            (fix-bit-name k vendor-ids :prefix prefix)
                            (first v))
                 unless more
                 do (format out ")")
                 when comment
                 do (format out " ;; ~a" comment))
           (format out "~:[)~;~]~%~%" bits)))

(defun write-enums (out types vendor-ids)
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
        for fixed-name = (string (fix-type-name name vendor-ids))
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
                (setf prefix (subseq expand 0 l)))))
          (let* ((p (loop for v in vendor-ids
                          thereis (search v fixed-name)))
                 (n (format nil "VK_~a"
                            (substitute #\_ #\-
                                        (if p
                                            (subseq fixed-name 0 (- p 1))
                                            fixed-name))))
                 (l (loop for (k) in bits
                          minimize (or (mismatch n k) 0))))
            (when (> l (length prefix))
              (setf prefix (subseq n 0 l)))))
        (loop for ((k . v) . more) on bits
              for comment = (getf (cdr v) :comment)
              for ext = (getf (cdr v) :ext)
              do (format out "~%  (:~(~a~) ~:[#x~x~;~d~])"
                         (string-trim '(#\-) (fix-bit-name k vendor-ids :prefix prefix))
                         (minusp (first v)) (first v))
              unless more
              do (format out ")")
              when (or ext comment)
              do (format out " ;;~@[ ~a~]~@[ ~a~]" ext comment))
        (format out "~:[)~;~]~%~%" bits)
        (when (string-equal fixed-name "RESULT")
          ;; write out error->comment, since they seem useful
          ;; enough to print out to users in errors
          (format out "(defparameter *result-comments*~%  (alexandria:plist-hash-table~%    '(~{~(:~a~) ~s~^~%     ~})))~%~%"
                  (loop for (k nil . v) in bits
                        collect (string-trim '(#\-) (fix-bit-name k vendor-ids :prefix prefix))
                        collect (getf v :comment))))))

(defun write-function-pointer-types (out types vendor-ids)
  (loop for (name . attribs) in (sort (remove-if-not
                                       (lambda (x)
                                         (and (consp (cdr x))
                                              (eql (second x) :func)))
                                       types)
                                      'string< :key 'car)
        do (format out "~( ~<;; ~@;~a~;~:>~%(defctype ~a :pointer)~)~%~%"
                   (list (cons "defcallback x" (getf (cdr attribs) :type)))
                   (fix-type-name name vendor-ids))))

(defun write-structs (out types structs vendor-ids)
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
                      (when (and (gethash (fix-type-name name vendor-ids) structs)
                                 (not (gethash name dumped)))
                        (let* ((attribs (get-type/f name types vendor-ids))
                               (members (getf (cddr attribs) :members)))
                                ;; todo: test if this still works
                                ;; set dumped true already here to prevent infinite recursion - necessary since v1.1.75
                          (setf (gethash name dumped) t)

                          (loop for (nil mt) in members
                                do (dump mt))
                          (format out "(defc~(~a~) ~(~a~)" (first attribs)
                                  (fix-type-name name vendor-ids))
                          (format out
                                  "~{~%  ~1{(:~(~a ~s~@[ :count ~a~])~^#|~@{~a~^ ~}|#~)~}~}"
                                  members)
                          (format out "~:[)~;~]~%~%" nil)
                                ;; (setf (gethash name dumped) t) used to be here
                          )))))
             (dump name))) )

(defun write-types-file (types-file copyright extension-names types bitfields structs vendor-ids)
  (with-open-file (out types-file :direction :output :if-exists :supersede)
    (format out ";;; this file is automatically generated, do not edit~%")
    (format out "#||~%~a~%||#~%~%" copyright)
    (format out "(in-package #:cl-vulkan-bindings)~%~%")
    
    (write-extension-names out extension-names)
    (write-base-types out types vendor-ids)
    (write-handles out types vendor-ids)
    (write-bitfields out bitfields types vendor-ids)
    (write-enums out types vendor-ids)
    (write-function-pointer-types out types vendor-ids)
    (write-structs out types structs vendor-ids)))
