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

(defun get-type (vk-spec name)
  "Gets the type description for a given NAME from a VK-SPEC instance.

Uses STRING= to compare type descriptions.

See VK-SPEC
"
  (format t "get-type called ~a ~%" name)
  (cdr (assoc name (types vk-spec) :test 'string=)))

(defun get-type/f (vk-spec name)
  "Gets the type description of a given NAME from a VK-SPEC instance.

Uses FIX-TYPE-NAME to compare type descriptions.

See VK-SPEC
See GET-TYPE
See FIX-TYPE-NAME
"
  (cdr (assoc name (types vk-spec) :test (lambda (a b)
                                           (equalp
                                            (fix-type-name a (vendor-ids vk-spec))
                                            (fix-type-name b (vendor-ids vk-spec)))))))

(defun set-type (vk-spec name value)
  (format t "set type called ~%")
  (let ((existing-type (get-type vk-spec name)))
    (format t "existing type evaluated~%")
    (if existing-type
        (assert (equalp value existing-type))
        (push (cons name value) (types vk-spec)))))
