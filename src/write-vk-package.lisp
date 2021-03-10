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

(in-package :vk-generator)

(defparameter *in-package-name* "vulkan")
(defparameter *package-nicknames* "#:%vk")
(defparameter *core-definer* "defvkfun")
(defparameter *ext-definer* "defvkextfun")

(defun write-api-constants-file (api-constants-file vk-spec)
  (with-open-file (out api-constants-file :direction :output :if-exists :supersede)
    (format out ";;; this file is automatically generated, do not edit~%")
    (format out "#||~%~a~%||#~%~%" (vulkan-license-header vk-spec))
    (format out "(in-package :~a)~%~%" *in-package-name*)
    (loop for api-constant in (sorted-elements (alexandria:hash-table-values (constants vk-spec)))
          do (format out "(defconstant ~(+~a+ ~a~)) ~%~%"
                     (fix-bit-name (name api-constant) (tags vk-spec))
                     (if (alias api-constant)
                         (format nil "+~(~a~)+"(fix-bit-name (alias api-constant) (tags vk-spec)))
                         (number-value api-constant))))))

(defun write-errors-file (errors-file vk-spec)
   (with-open-file (out errors-file :direction :output :if-exists :supersede)
     (format out ";;; this file is automatically generated, do not edit~%")
     (format out "#||~%~a~%||#~%~%" (vulkan-license-header vk-spec))
     (format out "(in-package :~a)~%~%" *in-package-name*)
     ;; define conditions
     (format out "(%define-conditions vk-condition")
     (loop for c in (enum-values (gethash "VkResult" (enums vk-spec)))
           when (> (number-value c) 0)
           do (format out "~&  ~a" (string-downcase (fix-bit-name (name c) (tags vk-spec)))))
     (format out ")~%~%")
     ;; define errors
     (format out "(%define-conditions vk-error")
     (loop for e in (enum-values (gethash "VkResult" (enums vk-spec)))
           when (< (number-value e) 0)
           do (format out "~&  ~a" (string-downcase (fix-bit-name (name e) (tags vk-spec)))))
     (format out ")~%")))

(defun write-func (out command vk-spec)
  (let ((name (name command))
        (ret (return-type command))
        (args (params command))
        (success (success-codes command))
        (errors (error-codes command))
        (ext (extensions command)))
    (format out "(~a (~s ~(~a) ~a~)"
            (if ext *ext-definer* *core-definer*)
            name
            (fix-function-name name (tags vk-spec))
            (cond
              ((string-equal ret "VkResult")
               "checked-result")
              ((gethash ret *vk-platform*)
               (format nil "~s" (gethash ret *vk-platform*)))
              (t (fix-type-name ret (tags vk-spec)))))
    (loop with *print-right-margin* = 10000
          for param in args
          for arg-name = (fix-type-name (name param) (tags vk-spec))
          for comment = (comment param)
          do (format out "~&  ~((~a ~s)~)"
                     arg-name
                     (make-arg-type arg-name (type-info param) vk-spec))
          unless (string= comment "") do (format out " ;; ~s ~%" comment))
    (format out ")~%~%")))

(defun write-funcs-file (funcs-file vk-spec)
  (with-open-file (out funcs-file :direction :output :if-exists :supersede)
    (format out ";;; this file is automatically generated, do not edit~%")
    (format out "#||~%~a~%||#~%~%" (vulkan-license-header vk-spec))
    (format out "(in-package :~a)~%~%" *in-package-name*)
    (loop for command in (sorted-elements (alexandria:hash-table-values (commands vk-spec)))
          do (write-func out command vk-spec)
          when (alias command)
          do (loop for alias in (alexandria:hash-table-values (alias command))
                   do (write-func out (make-aliased-command command alias) vk-spec)))))

(defun write-package-file (package-file vk-spec)
  ;; todo: clean this up
  (with-open-file (out package-file
                       :direction :output :if-exists :supersede)
    (format out ";;; this file is automatically generated, do not edit~%")
    (format out "#||~%~a~%||#~%~%" (vulkan-license-header vk-spec))
    
    ;; write vk-alloc
    (format out "(defpackage :vk-alloc~%  (:use #:cl)~%  (:export~%")
    (loop for sym in '("*allocated-foreign-objects*"
                       "*allocated-foreign-strings*"
                       "*allocate-foreign-object-func*"
                       "*free-foreign-object-func*"
                       "*allocate-foreign-string-func*"
                       "*free-foreign-string-func*"
                       "free-allocated-foreign-chain"
                       "free-allocated-children"
                       "foreign-allocate-and-fill"
                       "with-foreign-allocated-object"
                       "with-foreign-allocated-objects")
          do (format out "~(    #:~a~)~%" sym))
    (format out "))~%~%")

    ;; write vulkan/%vk
    (format out "(defpackage :~a~%  (:use #:cl #:cffi)~%" *in-package-name*)
    (format out "  (:nicknames ~a)~%" *package-nicknames*)
    (format out "  (:export~%")
    (format out "    #:size-t~%")
    (format out "    #:extension-loader~%")
    (format out "    #:make-extension-loader~%")
    (format out "    #:*default-extension-loader*~%")
    (format out "    #:size-t~%")
    (format out "~%")
    (loop for name in (sort (alexandria:hash-table-keys (extension-names vk-spec)) #'string<)
          do (format out "    #:+~(~a~)+~%"
                     (ppcre:regex-replace-all
                      "^VK-" (substitute #\- #\_ name) "")))
    (format out "~%")
    (labels ((sort-alphabetically (elements)
               (sort elements (lambda (a b) (string< (name a) (name b))))))
      (loop for type in (sort-alphabetically (alexandria:hash-table-values (types vk-spec)))
            unless (member (category type) '(:requires :unknown :define))
            do (format out "~(    #:~a ;; ~s~)~%"
                       (fix-type-name (name type) (tags vk-spec))
                       (category type)))
      (format out "~%")
      (loop for command in (sort-alphabetically (alexandria:hash-table-values (commands vk-spec)))
            do (format out "~(    #:~a~)~%" (fix-function-name (name command) (tags vk-spec)))
            when (alias command)
            do (loop for alias in (alexandria:hash-table-values (alias command))
                     do (format out "~(    #:~a~)~%" (fix-function-name (name alias) (tags vk-spec))))))
    (format out "))~%~%")

    ;; write vk
    ;; todo: other types
    (format out "(defpackage :vk~%  (:use #:cl)~%  (:shadow~%    #:format~%    #:set~%    #:stream~%    #:type~%    #:values)~%")
    (format out "  (:import-from #:%vk~%")
    (format out "   #:make-extension-loader~%")
    (format out "   #:*default-extension-loader*")
    (loop for name in (sort (alexandria:hash-table-keys (extension-names vk-spec)) #'string<)
          do (format out "~%    #:+~(~a~)+"
                     (ppcre:regex-replace-all
                      "^VK-" (substitute #\- #\_ name) "")))
    (format out ")~%")
    (format out "  (:export~%")
    (format out "    #:make-extension-loader~%")
    (format out "    #:*default-allocator*~%")
    (format out "    #:*default-extension-loader*~%")
    (format out "    #:make-api-version~%")
    (format out "    #:split-api-version~%")
    (format out "    #:format-api-version~%")
    (format out "~%")
    ;; extension names
    (loop for name in (sort (alexandria:hash-table-keys (extension-names vk-spec)) #'string<)
          do (format out "    #:+~(~a~)+~%"
                     (ppcre:regex-replace-all
                      "^VK-" (substitute #\- #\_ name) "")))
    (format out "~%")
    ;; types and commands
    (labels ((sort-alphabetically (elements)
               (sort elements (lambda (a b) (string< (name a) (name b))))))
      (loop for type in (sort-alphabetically (alexandria:hash-table-values (structures vk-spec)))
            do (format out "~(    #:~a ;; ~s~)~%"
                       (fix-type-name (name type) (tags vk-spec))
                       :class))
      (format out "~%")
      (loop for m in (remove-duplicates
                      (mapcar (lambda (m)
                                (fix-slot-name (name m) (type-name (type-info m)) vk-spec t))
                              (sort-alphabetically
                               (alexandria:flatten
                                (loop for struct in (alexandria:hash-table-values (structures vk-spec))
                                      collect (members struct)))))
                      :test #'string=)
            do (format out "~(    #:~a ;; ~s~)~%"
                       m
                       :accessor))
      (format out "~%")
      (loop for command in (sort-alphabetically (alexandria:hash-table-values (commands vk-spec)))
            do (format out "~(    #:~a~)~%" (fix-function-name (name command) (tags vk-spec)))
            when (alias command)
            do (loop for alias in (alexandria:hash-table-values (alias command))
                     do (format out "~(    #:~a~)~%" (fix-function-name (name alias) (tags vk-spec))))))
    (format out "))~%")))

(defun write-vk-package (vk-spec vk-package-dir)
  (let* ((additional-files-dir
           (asdf:system-relative-pathname 'vk-generator
                                          (make-pathname :directory '(:relative "src" "additional-files"))))
         (vk-dir (merge-pathnames (make-pathname :directory '(:relative "src")) vk-package-dir))
         (package-file (merge-pathnames "package.lisp" vk-dir))
         (translators-file (merge-pathnames "translators.lisp" vk-dir))
         (api-constants-file (merge-pathnames "api-constants.lisp" vk-dir))
         (errors-file (merge-pathnames "errors.lisp" vk-dir))
         (types-file (merge-pathnames "types.lisp" vk-dir))
         (funcs-file (merge-pathnames "funcs.lisp" vk-dir))
         (vk-types-file (merge-pathnames "vk-types.lisp" vk-dir))
         (vk-functions-file (merge-pathnames "vk-functions.lisp" vk-dir))
         (copy-files ;; todo: clean this up
           (list
            (list (merge-pathnames "vk-base.lisp" additional-files-dir)
                  (merge-pathnames "vk-base.lisp" vk-dir))
            (list (merge-pathnames "vk-alloc.lisp" additional-files-dir)
                  (merge-pathnames "vk-alloc.lisp" vk-dir))
            (list (merge-pathnames "bindings.lisp.template" additional-files-dir)
                  (merge-pathnames "bindings.lisp" vk-dir))
            (list (merge-pathnames "define-conditions.lisp.template" additional-files-dir)
                  (merge-pathnames "define-conditions.lisp" vk-dir))
            (list (merge-pathnames "extra-types.lisp.template" additional-files-dir)
                  (merge-pathnames "extra-types.lisp" vk-dir))  
            (list (merge-pathnames "vk.asd.template" additional-files-dir)
                  (merge-pathnames "vk.asd" vk-package-dir)))))
    (ensure-directories-exist vk-dir :verbose t)

    (write-api-constants-file api-constants-file vk-spec)
    (write-errors-file errors-file vk-spec)
    (write-types-file types-file vk-spec)
    (write-funcs-file funcs-file vk-spec)
    (write-package-file package-file vk-spec)

    (write-vk-types-file vk-types-file vk-spec)
    (write-vk-struct-translators-file translators-file vk-spec)
    (write-vk-functions vk-functions-file vk-spec)
    
    ;; copy additional files
    (loop for to-copy in copy-files
          do (cl-fad:copy-file (first to-copy) (second to-copy) :overwrite t))
    
    ;; todo: print out changes
    (force-output)
    nil))
