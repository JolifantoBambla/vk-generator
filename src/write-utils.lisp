(in-package :vk-generator)

(defun determine-param-default-value-string (param vk-spec)
  (with-slots (name type-info) param
    (with-slots (type-name postfix) type-info
      (if (string= "pAllocator" name)
          "vk:*default-allocator*"
          (cond
            ((handlep type-name vk-spec)
             (let ((h (get-handle type-name vk-spec)))
               (format nil "(vk:make-~a-wrapper (cffi:null-pointer))"
                       (fix-type-name type-name (tags vk-spec)))))
            ((and (string= type-name "char")
                  (not (string= postfix "**")))
             "\"\"")
            (t nil))))))
