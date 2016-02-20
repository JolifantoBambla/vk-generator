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

(define-foreign-library vulkan
  #++(:darwin (:framework "?"))
  (:windows "vulkan-1.dll")
  (:unix (:or "libvulkan.so")))

(use-foreign-library vulkan)

(defmacro defvkfun ((cname lname) result-type &body body)
  `(defcfun (,cname ,lname :library vulkan) ,result-type ,@body))


(if (= 8 (foreign-type-size :pointer))
    (defctype size-t :uint64)
    (defctype size-t :uint32))

(defun generate-reader (struct-type function-name members
                        &aux (struct-name struct-type))
  ;; todo: just use mem-ref for simple structs?
  (when (consp struct-name)
    (setf struct-name (second struct-name)))
  (labels ((get-reader-name (type-name)
             ;; hmm, hard-coding the prefix here is ugly possibly
             ;; should have some way to look it up?
             (intern (format nil "~a~a"'deref- type-name)))
           (len-reader (len)
             (when (symbolp (car len))
               (assert
                (not
                 (or
                  (ignore-errors (cffi:foreign-enum-keyword-list (car len)))
                  (ignore-errors (cffi:foreign-bitfield-symbol-list (car len)))))))
             (if (stringp (car len))
                 (cond
                   ((string= (car len) "codesize/4")
                    `(floor (foreign-slot-value p ',struct-type :code-size) 4))
                   ((string= (car len) "latexmath:[$\\lceil{\\mathit{rasterizationsamples} \\over 32}\\rceil$]")
                    ;; slot is an enum (which happens to mean same
                    ;; thing as its numeric value) so read as number
                    ;; so we can use it to calculate length
                    `(ceiling (mem-aref ,(slot-pointer :rasterization-samples)
                                        :int)
                              32))
                   (t (error "unhandled len='~s'" len)))
                 `(foreign-slot-value p ',struct-type ,(first len))))
           (slot-pointer (slot &key offset)
             (let ((fso (foreign-slot-offset struct-type slot)))
               `(cffi:inc-pointer
                 p
                 ,(if offset
                      `(+ ,fso ,offset)
                      fso))))
           (make-reader (member)
             (destructuring-bind (name type count
                                  &key len optional opaque must-be) member
               ;; possibly should berify must-be on read too?
               (declare (ignore must-be))
               (let ((cffi-type (foreign-slot-type struct-type
                                                   name))
                     (aggregate (and (consp type)
                                     (or (eq (car type) :union)
                                         (eq (car type) :struct)))))
                 (declare (ignore cffi-type))
                 (cond
                   ;; simple type
                   ((not (or count aggregate (consp type)))
                    (list name `(foreign-slot-value p ',struct-type
                                                    ,name)))
                   ;; single aggregate
                   ((and aggregate (not (or len count)))
                    (list name
                          `(,(get-reader-name (second type))
                            ,(slot-pointer name))))
                   ;; void* and pointers to OS structs/etc
                   (opaque
                    (list name `(let ((s (foreign-slot-value p ',struct-type
                                                             ,name)))
                                  (if (null-pointer-p s) nil s))))
                   ;; c-string pointer
                   ((and (equal type '(:pointer :char))
                         (equal len '(:null-terminated)))
                    (list name `(foreign-string-to-lisp
                                 (foreign-slot-value p ',struct-type ,name))))
                   ;; fixed-length string member
                   ((and (eq type :char) count)
                    ;; fixme: is there a better way to read
                    ;; 0-terminated string restricted to max N bytes?
                    (list name `(let ((c (loop for i below ,count
                                               when (zerop (mem-aref ,(slot-pointer name) :char i))
                                                 return i
                                               finally (return ,count))))
                                  (foreign-string-to-lisp
                                   ,(slot-pointer name)
                                   :max-chars c))))
                   ;; other fixed-length array members
                   (count
                    (list name
                          (cond
                            ((symbolp type)
                             `(foreign-array-to-lisp ,(slot-pointer name)
                                                     '(:array ,type ,count)))
                            ((typep type '(cons (member :struct :union)))
                             `(loop with s = ,(foreign-type-size type)
                                    for i below ,count
                                    collect (,(get-reader-name (second type))
                                             ,(slot-pointer name
                                                            :offset `(* i s)))))
                            (t (error "~s[~s] not done yet" type count)))))
                   ;; pointer to struct/union
                   ((and (not count)
                         (not len)
                         (typep type '(cons (eql :pointer)
                                       (cons
                                        (cons (member :struct :union))))))
                    (list name
                          `(,(get-reader-name (second (second type)))
                            (foreign-slot-value p ',struct-type ,name))))
                   ;; pointer to counted array
                   ((and len
                         (consp type)
                         (eql (first type) :pointer)
                         ;; pointer to array of simple type
                         (or (symbolp (second type))
                             ;; or array of struct/union
                             (typep (second type) '(cons (member
                                                          :struct :union)))
                             ;; or array of (pointer to) c-string
                             (and (equal (second type) '(:pointer :char))
                                  (eql (second len) :null-terminated))))
                    (let ((atype (if (consp type)
                                     (second type)
                                     type)))
                      (list name
                            `(loop with c = ,(len-reader len)
                                   with a = (foreign-slot-value p ',struct-type
                                                                ,name)
                                   for i below c
                                   collect
                                   ;; possibly should use foreign-array-to-lisp
                                   ;; for some of these?
                                   ;; (maybe only for static sizes?)
                                   ,(cond
                                      ((symbolp atype)
                                       ;; simple types
                                       `(mem-aref a ',atype i))
                                      ;; strings
                                      ((equal atype '(:pointer :char))
                                       `(mem-aref a :string i))
                                      ;; structs
                                      (t
                                       `(,(get-reader-name (second atype))
                                         (inc-pointer a
                                                      (* i ,(foreign-type-size
                                                             atype))))))))))
                   ;; todo
                   (t (error "not done yet? ~s ~s ~s ~s ~s" name type count len optional)))))))
    (let ((forms (mapcan #'make-reader members)))
      `(defun ,function-name (p)
         (unless (null-pointer-p p)
           (list ,@forms))))))

(defmacro def-translator (struct-name (reader &key fill) &body members)
  (let ((struct-type
          (cond
            ;; fixme: pass from generator or find better way to detect types
            ((ignore-errors (foreign-type-size `(:struct ,struct-name)))
             `(:struct ,struct-name))
            ((ignore-errors (foreign-type-size `(:union ,struct-name)))
             `(:union ,struct-name))
            (t (error "type ~s doesn't seem to be struct or union?"
                      struct-name)))))
   `(progn
      ,(generate-reader struct-type reader members)
      #++,@(when fill
             (list (generate-filler struct-type fill members))))))
