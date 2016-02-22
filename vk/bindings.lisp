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
               ;; possibly should verify must-be on read too?
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
                                    for i below ,(if len
                                                     `(min ,count
                                                           ,(len-reader len))
                                                     count)
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


(defvar *allocated-strings*)
(defvar *allocated-objects*)

(defun generate-member-filler (struct-name member p val
                               &key is-count
                               &aux (struct-type (list :struct struct-name)))
  (destructuring-bind (name type count
                       &key len optional opaque must-be
                       &aux (struct (and (consp type)
                                         (eq (car type) :struct))))
      member
    (labels ((get-value (name)
               `(getf ,val ,name))
             (get-writer-name (type-name)
               ;; hmm, hard-coding the prefix here is ugly possibly
               ;; should have some way to look it up?
               ;; (can't use *translators* here unless we sort
               ;;  translators.lisp)
               (intern (format nil "~a~a" 'fill- type-name)))
             (slot-pointer (slot &key offset)
               (let ((fso (foreign-slot-offset struct-type slot)))
                 `(cffi:inc-pointer ,p ,(if offset
                                            `(+ ,fso ,offset)
                                            fso))))
             (len-writer (len val)
               (if (stringp (car len))
                   (cond
                     ((string= (car len) "codesize/4")
                      `(setf (foreign-slot-value ,p ',struct-type :code-size)
                             (* ,val 4)))
                     ((string= (car len) "latexmath:[$\\lceil{\\mathit{rasterizationsamples} \\over 32}\\rceil$]")
                      ;; can't invert the equation, so just require the
                      ;; user to set it
                      nil)
                     (t (error "unhandled len='~s'" len)))
                   `(setf (foreign-slot-value ,p ',struct-type ,(first len))
                          ,val))))
      (cond
        ;; members with fixed value: set it and make sure caller
        ;; didn't pass something else
        (must-be
         (let ((must-be-val (foreign-enum-value type must-be
                                                ;; temporary hack until extension parsing is implemented in generator
                                                :errorp nil)))
           (list `(progn
                    (assert ,must-be-val ()
                            "couldn't find enum ~s in type ~s?"
                            ,must-be ',type)
                    (setf (foreign-slot-value ,p ',struct-type ,name)
                         ,must-be-val))
                 `(let ((v ,(get-value name)))
                    (assert (or (not v) (not (= v ,must-be-val)))
                            (v)
                            "~s must be ~s for type ~s if specified, got ~s"
                            ,name ,must-be-val ',struct-name v)))))
        ;; 'count' members are set by the member they count: just
        ;; return a check to make sure caller didn't send a value that
        ;; doesn't match
        (is-count
         (list nil ;; no code to set value
               `(alexandria:when-let (value ,(get-value name))
                  (assert
                   (= value (foreign-slot-value ,p ',struct-type ,name))
                   ()
                   "supplied count ~s = ~s doesn't match calculated count ~s.~% (Specifying a count is optional, so it should probably be skipped unless a mismatch would indicate some error in the calling code.)"
                   ,name value
                   (foreign-slot-value ,p ',struct-type ,name)))))
        ;; simple type
        ((and (or (symbolp type)
                  (typep type '(cons (eql :union))))
              (not (or count len)))
         (list `(setf (foreign-slot-value ,p ',struct-type ,name)
                      ,(get-value name))))
        ;; single struct
        ((and struct (not (or len count)))
         (list `(,(get-writer-name (second type))
                 ,(slot-pointer name)
                 ,(get-value name))))
        ;; void* and pointers to OS structs/etc
        (opaque
         (list `(setf (foreign-slot-value ,p ',struct-type ,name)
                      (or ,(get-value name) (null-pointer)))))
        ;; c-string pointer
        ((and (equal type '(:pointer :char))
              (equal len '(:null-terminated)))
         (list `(let ((s (foreign-string-alloc ,(get-value name))))
                  (push s *allocated-strings*)
                  #++(format t "allocated string ~s->~s~%" ,(get-value name) s)
                  (setf (foreign-slot-value ,p ',struct-type ,name)
                        s))))
        ;; fixed-length string member
        ((and (eq type :char) count)
         ;; assuming these are only in returnedonly
         ;; structs for now..
         (error "writing fixed-length strings not implemented"))
        ;; other fixed-length array members
        (count
         (list
          `(progn
             ,(cond
                ((or (symbolp type)
                     (typep type '(cons (eql :union))))
                 `(lisp-array-to-foreign (coerce ,val 'vector)
                                         ,(slot-pointer name)
                                         '(:array ,type ,count)))
                ;; not used?
                #++((typep type '(cons (eql :struct)))
                 `(loop with s = ,(foreign-type-size type)
                        for i below ,count
                        for v in val
                        collect (,(get-writer-name (second type))
                                 ,(slot-pointer name
                                                :offset `(* i s))
                                 v)))
                (t (error "~s[~s] not done yet" type count))))))
        ;; pointer to single struct
        ((and (not count)
              (not len)
              (typep type '(cons (eql :pointer)
                            (cons
                             (cons (member :struct))))))
         (let ((var (gensym (string name))))
           (list `(let ((,var (foreign-alloc ',(second type))))
                    (push ,var *allocated-objects*)
                    #++(format t "allocated ~s -> ~s~%" ',(second type) ,var)
                    (,(get-writer-name (second (second type)))
                     ,var
                     ,(get-value name))
                    (setf (foreign-slot-value ,p ',struct-type ,name)
                          ,var)))))
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
                          type))
               (var (gensym (string name))))
           (list
            `(let* ((vval ,(get-value name))
                    (vlen (length vval))
                    (,var (if (plusp vlen)
                              (foreign-alloc ',type :count vlen)
                              (null-pointer))))
               (when (plusp vlen)
                 (push ,var *allocated-objects*)
                 #++(format t "allocated ~s * ~s-> ~s~%" ',type vlen ,var))
               ,(len-writer len 'vlen)
               (setf (foreign-slot-value ,p ',struct-type ,name)
                     ,var)
               (loop with c = vlen
                     for v in vval
                     for i below c
                     do
                     ,(cond
                        ((or (symbolp atype)
                             (eq (car atype) :union))
                         ;; simple types
                         `(setf (mem-aref ,var ',atype i) v))
                        ;; strings
                        ((equal atype '(:pointer :char))
                         `(progn ;;format t ".allocated ~s / ~s ->~s~%"
                                 ;; ,name v
                                  (car (push (setf (mem-aref ,var ',atype i)
                                                   (foreign-string-alloc v))
                                             *allocated-strings*))))
                        ;; structs
                        ((eq (car atype) :struct)
                         `(,(get-writer-name (second atype))
                           (inc-pointer ,var
                                        (* i ,(foreign-type-size atype)))
                           v))
                        (t (error "unexpected type ~s?" type))))))))
        (t (error "not done yet? ~s ~s ~s ~s ~s" name type count len optional))))))

(defun generate-filler (struct-name function-name members)
  (let ((size-members (make-hash-table)))

    ;; find any members used to store size of another member, so we know
    ;; not to set them from input (and possibly error if they are set
    ;; in input and don't match?)
    (loop for (nil nil nil . attribs) in members
          for len = (getf attribs :len)
          when len
            do (loop for l in len
                     when (string= l "codesize/4")
                       do (setf (gethash :code-size size-members) t)
                     do (setf (gethash l size-members) t)))

    `(defun ,function-name (.p .val)
       ,@(loop for member in members
               for (fill check)
                 = (generate-member-filler struct-name member
                                           '.p '.val
                                           :is-count (gethash (first member)
                                                              size-members))
               collect fill into fills
               collect check into checks
               finally (return (remove 'nil (append fills checks)))))))

(defparameter *translators* (make-hash-table))

(defmacro with-vk-structs (((var type value) &rest more-bindings) &body body)
  `(let ((*allocated-strings* nil)
         (*allocated-objects* nil))
     (with-foreign-objects ((,var '(:struct ,type))
                            ,@(loop for (var type) in more-bindings
                                    collect (list var `(:struct ,type))))
       (unwind-protect
            (progn
              (,(gethash type *translators*) ,var ,value)
              ,@(loop for (var type VALUE) in more-bindings
                      collect `(,(gethash type *translators*) ,var ,value))
              ,@body)
         (loop for i in *allocated-strings*
               do #++(format t "~&free string ~s~%" i)
                  (foreign-string-free i))
         (loop for i in *allocated-objects*
               do #++(format t "~&free object ~s~%" i)
                  (foreign-free i))))))

(defmacro def-translator (struct-name (reader &key fill) &body members)
  (let ((struct-type
          (cond
            ((ignore-errors (foreign-type-size `(:struct ,struct-name)))
             `(:struct ,struct-name))
            (t (error "type ~s doesn't seem to be struct?"
                      struct-name)))))
   `(progn
      ,(generate-reader struct-type reader members)
      ,@(when fill
          (list `(setf (gethash ',struct-name *translators*) ',fill)
                (generate-filler struct-name fill members))))))

