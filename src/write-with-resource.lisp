;;;; write-with-resource.lisp

(in-package #:vk-generator)

;; must look something like this
(defmacro define-with-resource (name constructor destructor args (multiple-handles-p nil))
  ;; TODO: args need to be split into constructor and destructor args
  ;; TODO: must determine if single handle or multiple handles are created
  ;; TODO: must match constructor args and destructor args (e.g. instance in vkCreateDevice and vkDestroyDevice)
  (let* ((resource (gensym "RESOURCE"))
         (element (gensym "ELEMENT"))
         (arg-symbols (map 'list
                           (lambda (arg)
                             (gensym (first arg))) ;; assuming (first arg) is the arg's name
                           args))
         (required-args nil)
         (optional-args nil)
         (constructor-args nil)
         (destructor-args nil))
   `(defmacro ,name ((,@required-args &optional ,@optional-args) &body body)
      (let (,resource (,,constructor ,@constructor-args))
        (unwind-protect
             (progn ,@body)
          ,(if multiple-handles-p
               `(loop for ,element in ,resource do
                      (,,destructor ,@destructor-args))
               `(,,destructor ,@destructor-args)))))))

(defun make-def-with-name (name vk-spec)
  (format nil "with-~(~a~)"
          (fix-function-name (if (alexandria:starts-with-subseq "vkCreate" name)
                                 (subseq name 8)
                                 (subseq name 10))
                             (tags vk-spec))))

(defun write-with-resource-macros (vk-spec)
  (loop for handle in (sorted-elements (alexandria:hash-table-values (handles vk-spec)))
        when (and (create-commands handle)
                  (delete-command handle))
        do (format t "~a~%  created by: ~a~%  destroyed by: ~a~%  defwith: ~a~%"
                   handle
                   (create-commands handle)
                   (delete-command handle)
                   (loop for c in (create-commands handle)
                         collect (make-def-with-name c vk-spec)))))
