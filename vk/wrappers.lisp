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
                          (engine "cl-vulkan"))
  (%vk::with-vk-structs ((ici %vk:instance-create-info
                              `(;:s-type :instance-create-info
                                :p-next nil
                                :flags 0
                                :p-application-info (;:s-type :application-info
                                                     :p-next nil
                                                     :p-application-name ,app
                                                     :application-version 0
                                                     :p-engine-name ,engine
                                                     :engine-version 0
                                                     :api-version ,*api-version*)
                                ;:enabled-layer-count ,(length layers)
                                :pp-enabled-layer-names ,layers
                                ;:enabled-extension-count ,(length exts)
                                :pp-enabled-extension-names ,exts)))
    (with-foreign-object (p :pointer)
      (let* ((ret (%vk::create-instance ici (null-pointer) p))
             (instance (mem-ref p :pointer)))
        (format t "created instance2, ret = ~s~%" ret)
        (values (unless (null-pointer-p instance) instance) ret)))))

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

(macrolet
    ((getter (.object fun .type &rest args)
       (let* ((%vk (find-package '%vk))
              (counted (typep .type '(cons (eql :count))))
              (type (if (consp .type) (second .type) .type))
              (vkfun (find-symbol (string fun) %vk))
              (deref (find-symbol (format nil "DEREF-~a" type) %vk))
              (object (when .object (list .object))))
         (assert vkfun () "~s not found?" fun)
        (print (if counted
              `(defun ,fun (,@object ,@args)
                 (let ((count 0))
                   (with-foreign-object (c :uint32)
                     (setf (mem-ref c :uint32) 0)
                     (,vkfun ,@object ,@args c (null-pointer))
                     (setf count (mem-ref c :uint32))
                     (with-foreign-object (p '(:struct ,type) count)
                       (,vkfun ,@object ,@args c p)
                       (loop for i below count
                             collect (,deref
                                      (inc-pointer p
                                                   (* i (foreign-type-size
                                                         '(:struct ,type))))))))))
              `(defun ,fun (,@object ,@args)
                 (with-foreign-object (p '(:struct ,type))
                   (,vkfun ,@object ,@args p)
                   (,deref p)))))))
     (getters (object &body defs)
       (cons 'progn
             (loop for def in defs
                   collect `(getter ,object ,@def)))))

  (getters nil
           (enumerate-instance-extension-properties (:count %vk:extension-properties) layer-name)
           (enumerate-instance-layer-properties (:count %vk:layer-properties))
)

  (getters device
           (get-buffer-memory-requirements %vk:memory-requirements buffer)
           (get-image-memory-requirements %vk:memory-requirements image)
           (get-image-sparse-memory-requirements (:count %vk:sparse-image-memory-requirements) image)
           (get-render-area-granularity %vk:extent-2d render-pass))

  (getters physical-device
           (enumerate-device-extension-properties (:count %vk:extension-properties) layer-name)
           (enumerate-device-layer-properties (:count %vk:layer-properties))
           (get-display-mode-properties-khr (:count %vk:display-mode-properties-khr) display)
           (get-display-plane-capabilities-khr %vk:display-plane-capabilities-khr mode plane-index)
           #++(get-display-plane-supported-displays-khr (:count %vk:display-khr) plane-index)
           (get-physical-device-display-plane-properties-khr (:count %vk:display-plane-properties-khr))
           (get-physical-device-display-properties-khr (:count %vk:display-properties-khr))
           (get-physical-device-features %vk:physical-device-features)
           (get-physical-device-format-properties %vk:format-properties format)
           (get-physical-device-image-format-properties %vk:image-format-properties format type tiling usage flags)
           (get-physical-device-memory-properties %vk:physical-device-memory-properties)
           (get-physical-device-properties %vk:physical-device-properties)
           (get-physical-device-queue-family-properties (:count %vk:queue-family-properties))
           (get-physical-device-sparse-image-format-properties (:count %vk:sparse-image-format-properties) format type samples usage tiiling)
           (get-physical-device-surface-capabilities-khr %vk:surface-capabilities-khr surface)
           (get-physical-device-surface-formats-khr (:count %vk:surface-format-khr) surface)))

(defun get-device-memory-commitment (device memory)
  (with-foreign-object (p '%vk:device-size)
    (%vk:get-device-memory-commitment device memory p)
    (mem-ref p '%vk:device-size)))

(defun get-device-queue (device queue-family-index queue-index)
  (with-foreign-object (p '%vk:queue)
    (%vk:get-device-queue device queue-family-index queue-index p)
    (mem-ref p '%vk:queue)))

(defun get-physical-device-surface-support-khr (physical-device queue-family-index)
  (with-foreign-object (p '%vk:bool32)
    (%vk:get-physical-device-surface-support-khr physical-device queue-family-index p)
    (mem-ref p '%vk:bool32)))


#++
(defun get-physical-device-mir-presentation-support-khr (physical-device queue-family-index)
  (with-foreign-object (p '%vk::mir-connection)
    (%vk:get-physical-device-mir-presentation-support-khr device queue-family-index p)
    (mem-ref p '%vk::mir-connection)))


(defun get-image-subresource-layout (device image subresource)
  (%vk::with-vk-structs ((sr %vk:image-subresource subresource))
    (with-foreign-object (p '(:struct %vk:subresource-layout))
      (%vk:get-image-subresource-layout device image sr p)
      (%vk::deref-physical-device-memory-properties p))))
