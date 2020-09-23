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


(defun write-extension-names (out vk-spec)
  (format out "(defparameter *extension-names*~%  (alexandria:plist-hash-table~%    '(~{~(:~a~) ~a~^~%     ~})))~%~%"
          (loop for (k . v) in (alexandria:hash-table-alist (extension-names vk-spec))
                collect (ppcre:regex-replace-all
                         "^VK-" (substitute #\- #\_ k) "")
                collect v)))

(defun write-base-types (out vk-spec)
  (loop for (name . attribs) in (remove-if-not
                                 (lambda (x)
                                   (and (consp (cdr x))
                                        (eql (second x) :basetype)))
                                 (types vk-spec))
        do (format out "~((defctype ~a ~s)~)~%~%"
                   (fix-type-name name (vendor-ids vk-spec)) (second attribs)))
  (format out "(defctype handle :pointer)~%")
  (format out "#.(if (= 8 (foreign-type-size :pointer))~%  '(defctype non-dispatch-handle :pointer)~%  '(defctype non-dispatch-handle :uint64))~%~%")
        ;; misc OS types that are just passed around as pointers
  (loop for name in *opaque-types*
        ;; fixme: is there a better type to use here? or use empty struct?
        do (format out "~((defctype ~a :void)~)~%~%"
                   (fix-type-name name (vendor-ids vk-spec))))
  (loop for (name type) on *misc-os-types* by #'cddr
        do (format out "~((defctype ~a ~s)~)~%~%"
                   (fix-type-name name (vendor-ids vk-spec)) type))
  (loop for name in *opaque-struct-types*
        do (format out "~((defcstruct ~a)~)~%~%"
                   (fix-type-name name (vendor-ids vk-spec)))))

(defun write-handles (out vk-spec)
  (loop for (name . attribs) in (remove-if-not
                                 (lambda (x)
                                   (and (consp (cdr x))
                                        (member (second x)
                                                '(:handle
                                                  :non-dispatch-handle))))
                                 (types vk-spec))
        ;; handles are pointers to foo_T struct
        ;; on 32bit platform, 'non-dispatch' handles are 64bit int,
        ;; otherwise pointer to foo_T struct
        do (format out "(~(defctype ~a ~a~))~%~%"
                   (fix-type-name name (vendor-ids vk-spec))
                   (car attribs))))

(defun write-bitfields (out vk-spec)
  (loop for (name . attribs) in (sort (alexandria:hash-table-alist (bitfields vk-spec))
                                      'string< :key 'car)
        for base-type = (second (get-type vk-spec name))
        for requires = (first attribs)
        for bits = (if (consp base-type)
                       base-type
                       (second (when requires
                                 (get-type vk-spec requires))))
        for prefix = "VK_"
        for fixed-name = (string (fix-type-name name (vendor-ids vk-spec)))
        do (format out "(defbitfield (~(~a~@[ ~a~]~))" fixed-name
                   (when (stringp base-type) (fix-type-name base-type (vendor-ids vk-spec))))
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
                            (fix-bit-name k (vendor-ids vk-spec) :prefix prefix)
                            (first v))
                 unless more
                 do (format out ")")
                 when comment
                 do (format out " ;; ~a" comment))
           (format out "~:[)~;~]~%~%" bits)))

(defun write-enums (out vk-spec)
  (loop for (name . attribs) in (sort (remove-if-not
                                       (lambda (x)
                                         (and (consp (cdr x))
                                              (eql (second x) :enum)))
                                       (types vk-spec))
                                      'string< :key 'car)
        for type = (getf (cddr attribs) :type)
        for expand = (getf (cddr attribs) :expand)
        for requires = (getf (cddr attribs) :requires)
        for bits =  (second attribs)
        for prefix = "VK_"
        for fixed-name = (string (fix-type-name name (vendor-ids vk-spec)))
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
          (let* ((p (loop for v in (vendor-ids vk-spec)
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
                         (string-trim '(#\-) (fix-bit-name k (vendor-ids vk-spec) :prefix prefix))
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
                        collect (string-trim '(#\-) (fix-bit-name k (vendor-ids vk-spec) :prefix prefix))
                        collect (getf v :comment))))))

(defun write-function-pointer-types (out vk-spec)
  (loop for (name . attribs) in (sort (remove-if-not
                                       (lambda (x)
                                         (and (consp (cdr x))
                                              (eql (second x) :func)))
                                       (types vk-spec))
                                      'string< :key 'car)
        do (format out "~( ~<;; ~@;~a~;~:>~%(defctype ~a :pointer)~)~%~%"
                   (list (cons "defcallback x" (getf (cdr attribs) :type)))
                   (fix-type-name name (vendor-ids vk-spec)))))

(defun write-structs (out vk-spec)
  (loop with dumped = (make-hash-table :test 'equal)
        for (name . attribs) in (sort (remove-if-not
                                       (lambda (x)
                                         (and (consp (cdr x))
                                              (member (second x)
                                                      '(:struct :union))))
                                       (types vk-spec))
                                      'string< :key 'car)
        do (labels
               ((dump (name)
                  (if (and (consp name)
                           (member (car name) '(:pointer :struct :union)))
                      (dump (second name))
                      (when (and (gethash (fix-type-name name (vendor-ids vk-spec)) (structs vk-spec))
                                 (not (gethash name dumped)))
                        (let* ((attribs (get-type/f vk-spec name))
                               (members (getf (cddr attribs) :members)))
                                ;; todo: test if this still works
                                ;; set dumped true already here to prevent infinite recursion - necessary since v1.1.75
                          (setf (gethash name dumped) t)

                          (loop for (nil mt) in members
                                do (dump mt))
                          (format out "(defc~(~a~) ~(~a~)" (first attribs)
                                  (fix-type-name name (vendor-ids vk-spec)))
                          (format out
                                  "~{~%  ~1{(:~(~a ~s~@[ :count ~a~])~^#|~@{~a~^ ~}|#~)~}~}"
                                  members)
                          (format out "~:[)~;~]~%~%" nil)
                                ;; (setf (gethash name dumped) t) used to be here
                          )))))
             (dump name))) )

(defun write-types-file (types-file vk-spec)
  (with-open-file (out types-file :direction :output :if-exists :supersede)
    (format out ";;; this file is automatically generated, do not edit~%")
    (format out "#||~%~a~%||#~%~%" (copyright vk-spec))
    (format out "(in-package :~a)~%~%" *in-package-name*)
    
    (write-extension-names out vk-spec)
    (write-base-types out vk-spec)
    (write-handles out vk-spec)
    (write-bitfields out vk-spec)
    (write-enums out vk-spec)
    (write-function-pointer-types out vk-spec)
    (write-structs out vk-spec)))
