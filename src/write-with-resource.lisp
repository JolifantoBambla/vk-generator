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
   `(defmacro ,name ((,@required-args &key ,@optional-args) &body body)
      `(let (,resource (,,constructor ,@constructor-args))
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

(defun write-with-resource (handle create-command-name delete-command-name vk-spec)
  (format t "  ~a: ~a -> ~a~%"
          (make-def-with-name create-command-name vk-spec)
          create-command-name
          delete-command-name)
  (let* ((create-command (gethash create-command-name (commands vk-spec)))
         (delete-command (gethash delete-command-name (commands vk-spec)))
         (required-create-params (get-required-params create-command vk-spec))
         (optional-create-params (get-optional-params create-command vk-spec))
         (skipped-create-params (get-skipped-input-params create-command vk-spec))
         (required-delete-params (get-required-params delete-command vk-spec))
         (optional-delete-params (get-optional-params delete-command vk-spec))
         (skipped-delete-params (get-skipped-input-params delete-command vk-spec))
         (extensionp (needs-explicit-loading-p create-command))
         (command-type (determine-command-type create-command vk-spec)))
    (format t "    required: ~a optional: ~a skipped: ~a~%"
            required-create-params
            optional-create-params
            skipped-create-params)
    (format t "    required: ~a optional: ~a skipped: ~a~%"
            required-delete-params
            optional-delete-params
            skipped-delete-params)
    (format t "    extension: ~a type:~a~%"
            extensionp
            command-type)))

(defun write-with-resource-macros (vk-spec)
  (loop for handle in (sorted-elements (alexandria:hash-table-values (handles vk-spec)))
        when (and (create-commands handle)
                  (delete-command handle))
        do (progn
             (format t "~%handle ~a:~%"
                     handle)
             (loop for c in (create-commands handle)
                   do (write-with-resource handle c (delete-command handle) vk-spec)))))

