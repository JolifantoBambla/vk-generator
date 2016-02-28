;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
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

(in-package #:cl-vulkan-bindings)


(define-foreign-type checked-result ()
  ()
  (:actual-type result)
  (:simple-parser checked-result))


(defmethod translate-from-foreign (v (type checked-result))
  ;; todo: expand-from-foreign/-dyn
  (if (zerop v) ;; optimize common case
      :success
      (let ((enum (foreign-enum-keyword 'result v :errorp nil)))
        (when (< v 0)
          (error "cl-vulkan error: ~a (~s = ~a)"
                 (gethash enum *result-comments*)
                 v (or enum "??")))
        ;; these should possibly be conditions instead?
        (when (> v 0)
          (warn "cl-vulkan warning: ~a (~s = ~a)"
                (gethash enum *result-comments*)
                v (or enum "??")))
        (or enum v))))


;; override the default so we can convert to/from T/NIL
(define-foreign-type bool32 ()
  ()
  (:actual-type :uint32)
  (:simple-parser bool32))

(defmethod translate-from-foreign (v (type bool32))
  (not (zerop v)))
(defmethod translate-to-foreign (v (type bool32))
  (if v 1 0))
(defmethod expand-to-foreign (v (type bool32))
  (if (constantp v)
      (if (eval v) 1 0)
      `(if ,v 1 0)))
(defmethod expand-from-foreign (v (type bool32))
  (if (constantp v)
      (not (zerop (eval v)))
      `(not (zerop ,v))))
