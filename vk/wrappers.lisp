(in-package #:cl-vulkan)

(defun api-version (major minor patch)
  (logior (ash major 22)
          (ash minor 12)
          patch))

(defparameter *api-version* (api-version 1 0 3))

(defmacro with-foreign-string-arrays (((pointer-var lisp-var)
                                       &rest more-var-defs)
                                      &body body)
  (let ((string-pointers (gensym (string lisp-var))))
    `(let* ((,string-pointers (map 'vector 'foreign-string-alloc ,lisp-var)))
       (unwind-protect
            (with-foreign-array (,pointer-var ,string-pointers
                                              `(:array :pointer ,(length ,lisp-var)))
              ,(if more-var-defs
                   `(with-foreign-string-arrays (,@more-var-defs)
                      ,@body)
                   `(progn ,@body)))
         (map nil 'foreign-string-free ,string-pointers)))))

(defun create-instance (&key exts layers (app "cl-vulkan test")
                          (engine "cl-vulkam"))
  (with-foreign-string-arrays ((p-exts exts)
                               (p-layers layers))
    (with-foreign-strings ((app app)
                           (engine engine))
      (with-foreign-objects ((ai '(:struct %vk::application-info))
                             (ici '(:struct %vk::instance-create-info)))
        (setf (mem-ref ai '(:struct %vk::application-info))
              `(:s-type :application-info
                :p-next ,(null-pointer)
                :p-application-name ,app
                :application-version 0
                :p-engine-name ,engine
                :engine-version 0
                :api-version ,*api-version*))
        (setf (mem-ref ici '(:struct %vk::instance-create-info))
              `(:s-type :instance-create-info
                :p-next ,(null-pointer)
                :flags 0
                :p-application-info ,ai
                :enabled-layer-count ,(length layers)
                :pp-enabled-layer-names ,p-layers
                :enabled-extension-count ,(length exts)
                :pp-enabled-extension-names ,p-exts))
        (with-foreign-object (p :pointer)
          (let ((ret (%vk::create-instance ici (null-pointer) p)))
            (format t "created instance, ret = ~s~%" ret))
          (mem-ref p :pointer))))))
