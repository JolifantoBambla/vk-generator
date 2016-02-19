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
            (format t "created instance, ret = ~s~%" ret)
            (values (mem-ref p :pointer) ret)))))))

(defun enumerate-physical-devices (instance)
  (with-foreign-object (p-count :uint32)
    (setf (mem-ref p-count :uint32) 0)
    (%vk:enumerate-physical-devices instance p-count (null-pointer))
    (let ((count (mem-ref p-count :uint32)))
      (with-foreign-object (phys '%vk:physical-device count)
        (let ((ret (%vk:enumerate-physical-devices instance p-count phys)))
          (values (loop for i below count
                        collect (mem-aref phys '%vk:physical-device i))
                  ret))))))
#++
(defun deref (p type)
  (let ((d (cffi:mem-ref p type)))
    (loop for (k v) on d by #'cddr
          when (typep v 'foreign-pointer)
            do (let ((stype (foreign-slot-type type k))
                     (scount (foreign-slot-count type k)))
                 (progn ;unless (eq stype :char)
                   (setf v (loop for i below scount
                                 collect (mem-aref v stype i)))))
          collect k collect v)))

(defun get-physical-device-properties (device)
  (with-foreign-object (p '(:struct %vk:physical-device-properties))
    (%vk:get-physical-device-properties device p)
    (mem-ref p '(:struct %vk:physical-device-properties))
    #++(deref p '(:struct %vk:physical-device-properties))))

(defun get-physical-device-features (device)
  (with-foreign-object (p '(:struct %vk:physical-device-features))
    (%vk:get-physical-device-features device p)
    (mem-ref p '(:struct %vk:physical-device-features))
    #++(deref p '(:struct %vk:physical-device-features))))
