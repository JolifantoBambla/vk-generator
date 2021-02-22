#|
 Copyright(c) 2020 - Lukas Herzberger <herzberger.lukas@gmail.com>
 SPDX-License-Identifier: MIT
|#

(in-package :vk-alloc)

(defparameter *allocated-foreign-objects* (make-hash-table)
  "A hash table storing allocated foreign objects and dependencies between them.
Each foreign object (key) is associated with a list of other foreign objects that were allocated during allocation of the key and should be freed when the key is freed.

Each foreign object must appear once at most within a value of this hash table.
E.g.
((<foreign1> '(<foreign2> <foreign3>))
 (<foreign2> nil)
 (<foreign3> '(<foreign4>))
 (<foreign4> nil))

A not on multithreading: Hash tables are by default not thread safe. Since entries in this hash table are not meant to be shared between threads, you can bind *ALLOCATED-FOREIGN-OBJECTS* to a thread-local variable and it should be fine. E.g. with BOURDEAUX-THREADS you can use *SPECIAL-DEFAULT-BINDINGS* for this.")

(defparameter *allocated-foreign-strings* (make-hash-table)
  "A hash table storing allocated foreign strings for foreign allocated objects.
Each foreign object (key) is associated with a list of foreign strings that were allocated during allocation of the key and should be freed when the key is freed.

A not on multithreading: Hash tables are by default not thread safe. Since entries in this hash table are not meant to be shared between threads, you can bind *ALLOCATED-FOREIGN-OBJECTS* to a thread-local variable and it should be fine. E.g. with BOURDEAUX-THREADS you can use *SPECIAL-DEFAULT-BINDINGS* for this.")

(defparameter *allocate-foreign-object-func* #'cffi:foreign-alloc
  "Configures how foreign resources are allocated. You might want to use this for implementing a memory pool to reuse allocated resources, etc.")

(defparameter *free-foreign-object-func* #'cffi:foreign-free
  "Configures how foreign resources are freed. You might want to use this for implementing a memory pool to reuse allocated resources, etc.")

(defparameter *allocate-foreign-string-func* #'cffi:foreign-string-alloc
  "Configures how foreign strings are allocated. You might want to use this for implementing a memory pool to reuse allocated resources, etc.")

(defparameter *free-foreign-string-func* #'cffi:foreign-string-free
  "Configures how foreign strings are freed. You might want to use this for implementing a memory pool to reuse allocated resources, etc.")

(defun free-allocated-foreign-chain (foreign-obj)
  "Frees all foreign objects ...

See *ALLOCATED-FOREIGN-OBJECTS*
See *FREE-FOREIGN-OBJECT-FUNC*"
  (funcall *free-foreign-object-func* foreign-obj)
  (dolist (str (gethash foreign-obj *allocated-foreign-strings*)) (funcall *free-foreign-string-func* str))
  (dolist (child (gethash foreign-obj *allocated-foreign-objects*)) (free-allocated-foreign-chain child))
  (remhash foreign-obj *allocated-foreign-strings*)
  (remhash foreign-obj *allocated-foreign-objects*))

(defun free-allocated-children (foreign-obj)
  "Frees all children that were allocated during allocation of FOREIGN-OBJ which is assumed to be a stack-allocated resource, whereas its children were heap-allocated.

See FREE-ALLOCATED-FOREIGN-CHAIN"
  (dolist (child (gethash foreign-obj *allocated-foreign-objects*)) (free-allocated-foreign-chain child))
  (dolist (str (gethash foreign-obj *allocated-foreign-strings*)) (funcall *free-foreign-string-func* str))
  (remhash foreign-obj *allocated-foreign-strings*)
  (remhash foreign-obj *allocated-foreign-objects*))

(defun foreign-allocate-and-fill (type content)
  "Allocates a foreign object of TYPE and fill it with CONTENT.

See *ALLOCATED-FOREIGN-OBJECTS*
See *ALLOCATE-FOREIGN-FUNC*"
  (let* ((sequence-p (or (listp content)
                         (arrayp content)))
         (count (if sequence-p (length content) 1))
         (p-resource (funcall *allocate-foreign-object-func* type count)))
    (cond
      ((not sequence-p)
       (setf (cffi:mem-aref p-resource type) content))
      ((listp content)
       (loop for i from 0 below count
             for c in content
             do (setf (cffi:mem-aref p-resource type i) c)))
      ((arrayp content)
       (cffi:lisp-array-to-foreign content p-resource type)))
    p-resource))

(defmacro with-foreign-allocated-object ((var type content) &body body)
  "Bind VAR and translate CONTENT to a foreign pointer of TYPE during BODY.
The pointer in VAR is invalid beyond the dynamic extent of BODY.

See CFFI:WITH-FOREIGN-OBJECT"
  `(cffi:with-foreign-object (,var ,type)
     (unwind-protect
          (progn
            (setf (cffi:mem-aref ,var ,type) ,content)
            ,@body)
       (free-allocated-children ,var))))

(defmacro with-foreign-allocated-objects (bindings &rest body)
  "Behaves like WITH-FOREIGN-ALLOCATED-OBJECT but for multiple BINDINGS instead of just one.
 
See WITH-FOREIGN-ALLOCATED-OBJECT"
  (if bindings
      `(with-foreign-allocated-object ,(car bindings)
         (with-foreign-allocated-objects ,(cdr bindings)
           ,@body))
      `(progn ,@body)))
