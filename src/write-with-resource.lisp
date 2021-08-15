;;;; write-with-resource.lisp

(in-package #:vk-generator)

(defun make-def-with-name (name vk-spec)
  (format nil "with-~(~a~)"
          (fix-function-name (if (alexandria:starts-with-subseq "vkCreate" name)
                                 (subseq name 8)
                                 (subseq name 10))
                             (tags vk-spec))))

(defun write-with-resource-macros (vk-spec)
  (loop for handle in (sorted-elements (alexandria:hash-table-values (handles vk-spec)))
        when (and (create-command handle)
                  (delete-command handle))
        do (format t "~a~%  created by: ~a~%  destroyed by: ~a~%  defwith: ~a~%"
                   handle
                   (create-command handle)
                   (delete-command handle)
                   (loop for c in (create-command handle)
                         collect (make-def-with-name c vk-spec)))))
