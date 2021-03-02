;; TODO: copyright

(in-package :vk)

(defparameter *default-allocator* (cffi:null-pointer))

;; TODO:
(defun make-api-version (major minor patch)
  0)

;; TODO: should return a string containing "major.minor.patch"
(defun format-api-version (version))

;; TODO: all 14 sub-cases of vk-functions as macros

;; this works: (vk-alloc:with-foreign-allocated-objects () (+ 1 2))

(defun process-variables (variables)
  "Elements of VARIABLES should look like this:
(arg-name arg-type contents ...options)
options are :handle, :in/:out, :optional
"
  (let ((translated-args nil)
        (vk-input-args nil)
        (output-args nil))
    (loop for var in variables
          do (let ((var-sym (gensym)))
               (push
                (if (and (find :handle var)
                         (find :in var))
                    (first var)
                    var-sym)
                vk-input-args)
               (if (and (not (find :handle var))
                        (not (find :out var)))
                   (push
                    (list var-sym
                          (second var) ;; type
                          (third var)) ;; contents
                    translated-args))
               (if (find :out var)
                   (push
                    (list (first var)
                          (list var-sym
                                (second var))) ;; type, TODO: how to handle count?
                    output-args))))
    (values (reverse translated-args) (reverse vk-input-args) (reverse output-args))))

;;; ---------------------------------------------- 0 output parameters -----------------------------------------------------------------

;; case 0a: no or implicit return value - e.g. vkDestroyInstance
;; case 0b: non-trivial return value - e.g. vkGetInstanceProcAddr
(defmacro defvk-simple-fun ((name vulkan-fun docstring required-args optional-args &optional (return-type nil)) &body variables)
  "Defines a function named NAME which wraps VULKAN-FUN.
VULKAN-FUN is a function that has no output parameters, but might return a value.
E.g. no value returned: vkDestroyInstance
E.g. value returned: vkGetInstanceProcAddr

Note: VkBool32 and VkResult are treated as no return values, since they are implicitly converted to lisp values and don't have to be translated explicitely."
  (multiple-value-bind (translated-args vk-input-args) (process-variables variables)
    `(defun ,name (,@required-args &optional ,@optional-args)
       ,docstring
      (vk-alloc:with-foreign-allocated-objects (,@translated-args)
        ,(if return-type
             `(cffi:mem-aref (,vulkan-fun ,@vk-input-args) ,return-type)
             `(,vulkan-fun ,@vk-input-args))))))


;;; --------------------------------------------- 1 output parameter -------------------------------------------------------------------

;; case 1a-1: create a handle - e.g. vkCreateInstance
;; case 1a-2: get an existing handle - e.g. vkGetDeviceQueue
(defmacro defvk-create-handle-fun ((name vulkan-fun docstring required-args optional-args &optional (no-result-p nil)) &body variables)
  "Defines a function named NAME which wraps VULKAN-FUN.
VULKAN-FUN is function that creates or gets some sort of handle and might return a RESULT.
E.g. returning a result: vkCreateInstance
E.g. returning no result: vkGetDeviceQueue"
  (multiple-value-bind (translated-args vk-input-args output-args) (process-variables variables)
    (let ((handle-def (second (first output-args)))
          (result (gensym)))
      `(defun ,name (,@required-args &optional ,@optional-args)
         ,docstring
         (vk-alloc:with-foreign-allocated-objects (,@translated-args)
           (cffi:with-foreign-object (,@handle-def)
             ,(if no-result-p
                  `(progn
                     (,vulkan-fun ,@vk-input-args)
                     (cffi:mem-aref ,@handle-def))
                  `(let ((,result (,vulkan-fun ,@vk-input-args)))
                     (values (cffi:mem-aref ,@handle-def)
                             ,result)))))))))

;; case 1b-1: create a list of handles (len by arg) - e.g. vkCreateGraphicsPipelines
;; case 1b-2: create a list of handles (len by struct member) - e.g. vkAllocateCommandBuffers
(defmacro defvk-create-handles-fun ((name vulkan-fun docstring required-args optional-args len-provider) &body variables)
    "Defines a function named NAME which wraps VULKAN-FUN.
VULKAN-FUN is function that creates multiple handles and returns a RESULT.
The number of created handles is given by LEN-PROVIDER.
E.g. LEN-PROVIDER is the length of an input parameter: vkCreateGraphicsPipelines
E.g. LEN-PROVIDER is a slot value of an input parameter: vkAllocateCommandBuffers"
  (multiple-value-bind (translated-args vk-input-args output-args) (process-variables variables)
    (let ((handle-def (second (first output-args)))
          (result (gensym)))
      `(defun ,name (,@required-args &optional ,@optional-args)
         ,docstring
         (vk-alloc:with-foreign-allocated-objects (,@translated-args)
           (cffi:with-foreign-object (,@handle-def ,len-provider)
             (let ((,result (,vulkan-fun ,@vk-input-args)))
               (values (loop for i from 0 below ,len-provider
                             collect (cffi:mem-aref ,@handle-def i))
                       ,result))))))))

;; case 1c-1: get a struct extended by its NEXT slot - e.g. vkGetPhysicalDeviceFeatures2
;; case 1c-2: get a struct without NEXT slot - e.g. vkGetPhysicalDeviceProperties
(defmacro defvk-get-struct-fun ((name vulkan-fun docstring required-args optional-args) &body variables)
  "Defines a function named NAME which wraps VULKAN-FUN.
VULKAN-FUN is function that gets a struct and returns a RESULT.
E.g. vkGetPhysicalDeviceProperties"
  (multiple-value-bind (translated-args vk-input-args output-args) (process-variables variables)
    (let ((struct-def (second (first output-args))))
      `(defun ,name (,@required-args &optional ,@optional-args)
         ,docstring
         (vk-alloc:with-foreign-allocated-objects (,@translated-args)
           (cffi:with-foreign-object (,@struct-def)
             (,vulkan-fun ,@vk-input-args)
             (cffi:mem-aref ,@struct-def))))))) ;; TODO: translate-from-foreign including NEXT translation!!

;; TODO: maybe take a type and a size instead?
;; case 1d: arbitrary data as output param - e.g. vkGetQueryPoolResults
(defmacro defvk-fill-arbitrary-buffer-fun ((name vulkan-fun docstring required-args optional-args) &body variables)
  "Defines a function named NAME which wraps VULKAN-FUN.
VULKAN-FUN is function that fills a buffer of arbitrary size.
The allocated buffer as well as its size are provided by the user.
E.g. vkGetQueryPoolResults"
  (multiple-value-bind (translated-args vk-input-args) (process-variables variables)
    `(defun ,name (,@required-args &optional ,@optional-args)
       ,docstring
       (vk-alloc:with-foreign-allocated-objects (,@translated-args)
         (,vulkan-fun ,@vk-input-args)))))


;;; --------------------------------------------- 2 output parameters ------------------------------------------------------------------

;; case 2a: e.g. vkGetPhysicalDeviceQueueFamilyProperties2
(defmacro defvk-get-structs-fun ((name vulkan-fun docstring required-args optional-args count-arg-name array-arg-name &optional (no-result-p nil)) &body variables)
  "Defines a function named NAME which wraps VULKAN-FUN.
VULKAN-FUN is function that gets multiple structs and might return a RESULT.
E.g. vkGetPhysicalDeviceQueueFamilyProperties2"
  (multiple-value-bind (translated-args vk-input-args output-args) (process-variables variables)
    (let ((count-arg (find-if (lambda (a)
                                (eq (first a) count-arg-name))
                              output-args))
          (array-arg (find-if (lambda (a)
                                (eq (first a) array-arg-name))
                              output-args))
          (translated-count (gensym))
          (result (gensym)))
      `(defun ,name (,@required-args &optional ,@optional-args)
         ,docstring
         (vk-alloc:with-foreign-allocated-objects (,@translated-args)
           (cffi:with-foreign-object (,@ (second count-arg))
             (let ((,(first (second array-arg)) (cffi:null-pointer)))
               (,vulkan-fun ,@vk-input-args)
               (let ((,translated-count (cffi:mem-aref ,@ (second count-arg))))
                 (cffi:with-foreign-object (,@ (second array-arg) ,translated-count)
                         ,(if no-result-p
                              `(progn
                                 (,vulkan-fun ,@vk-input-args)
                                 (loop for i from 0 below ,translated-count
                                       collect (cffi:mem-aref ,@ (second array-arg) i)))
                              `(let ((,result (,vulkan-fun ,@vk-input-args)))
                                 (values (loop for i from 0 beloq ,translated-count
                                               collect (cffi:mem-aref ,@ (second array-arg) i))))))))))))))

;; case 2b: ???

;; TODO: this needs to look like this
#|
"(do () ((not (eq result :incomplete))) ~a (when (eq result :success) (let ((count (cffi:mem-aref p~a ~s))) (if (> 0 count) ~a nil))))"
|#
;; case 2c: enumerate - e.g. vkEnumeratePhysicalDevices
(defmacro defvk-enumerate-fun ((name vulkan-fun docstring required-args optional-args count-arg-name array-arg-name &optional (no-result-p nil)) &body variables)
  "Defines a function named NAME which wraps VULKAN-FUN.
VULKAN-FUN is function that gets multiple structs and might return a RESULT.
E.g. vkGetPhysicalDeviceQueueFamilyProperties2"
  (multiple-value-bind (translated-args vk-input-args output-args) (process-variables variables)
    (let ((count-arg (find-if (lambda (a)
                                (eq (first a) count-arg-name))
                              output-args))
          (array-arg (find-if (lambda (a)
                                (eq (first a) array-arg-name))
                              output-args))
          (translated-count (gensym))
          (result (gensym)))
      `(defun ,name (,@required-args &optional ,@optional-args)
         ,docstring
         (vk-alloc:with-foreign-allocated-objects (,@translated-args)
           (cffi:with-foreign-object (,@ (second count-arg))
             (let ((,(first (second array-arg)) (cffi:null-pointer)))
               (,vulkan-fun ,@vk-input-args)
               (let ((,translated-count (cffi:mem-aref ,@ (second count-arg))))
                 (cffi:with-foreign-object (,@ (second array-arg) ,translated-count)
                         ,(if no-result-p
                              `(progn
                                 (,vulkan-fun ,@vk-input-args)
                                 (loop for i from 0 below ,translated-count
                                       collect (cffi:mem-aref ,@ (second array-arg) i)))
                              `(let ((,result (,vulkan-fun ,@vk-input-args)))
                                 (values (loop for i from 0 beloq ,translated-count
                                               collect (cffi:mem-aref ,@ (second array-arg) i))))))))))))))

;; case 2d: ???

;;; --------------------------------------------- 3 output parameters ------------------------------------------------------------------

;; case 3:

;; tests

;; 0a
(defvk-simple-fun (destroy-instance
                   %vk:destroy-instance
                   "docs"
                   nil
                   ((instance nil) (allocator *default-allocator*)))
  (instance '%vk-instance instance :in :handle :optional)
  (allocator '(:struct %vk:allocation-callbacks) allocator :in :optional))

;; 0b
(defvk-simple-fun (get-instance-proc-addr
                   %vk:get-instance-proc-addr
                   "docs"
                   (name)
                   ((instance nil)))
  (name :string name :in)
  (instance '%vk:instance instance :in :handle :optional))

;; 1a-1
(defvk-create-handle-fun (create-instance
                          %vk:create-instance
                          "docs"
                          (create-info)
                          ((allocator *default-allocator*)))
  (create-info '(:struct %vk:instance-create-info) create-info :in)
  (allocator '(:struct %vk:allocation-callbacks) allocator :in :optional)
  (instance '%vk:instance nil :out))

;; 1a-2
(defvk-create-handle-fun (get-device-queue
                          %vk:get-device-queue
                          "docs"
                          (device queue-family-index queue-index)
                          nil
                          t)
  (device '%vk:device device :in :handle)
  (queue-family-index :uint32 queue-family-index :in)
  (queue-index :uint32 queue-index :in)
  (queue '%vk:queue nil :out))

;; 1b-1
(defvk-create-handles-fun (create-graphics-pipelines
                           %vk:create-graphics-pipelines
                           "docs"
                           (device create-infos)
                           ((pipeline-cache (cffi:null-pointer)) (allocator *default-allocator*))
                           (length create-infos))
  (device '%vk:device device :in :handle)
  (pipeline-cache '%vk:pipeline-cache pipeline-cache :in :handle :optional)
  (create-info-count :uint32 (length create-infos) :in)
  (create-infos '(:struct %vk:graphics-pipeline-create-info) create-infos :in)
  (allocator '(:struct %vk:allocation-callbacks) allocator :in :optional)
  (pipelines '%vk:pipeline nil :out))

;; 1b-2
(defvk-create-handles-fun (allocate-command-buffers
                           %vk:allocate-command-buffers
                           "docs"
                           (device allocate-info)
                           nil
                           (vk:command-buffer-count allocate-info))
  (device '%vk:device device :in :handle)
  (allocate-info '(:struct %vk:command-buffer-allocate-info) allocate-info :in)
  (command-buffers '%vk:command-buffer nil :out))

;; 1c-1
(defvk-get-struct-fun (get-physical-device-properties-2
                       %vk:get-physical-device-properties-2
                       "docs"
                       (physical-device)
                       nil)
  (physical-device '%vk:physical-device physical-device :in :handle)
  (properties '(:struct %vk:physical-device-properties-2) nil :out))

;; 1c-2
(defvk-get-struct-fun (get-physical-device-properties
                       %vk:get-physical-device-properties
                       "docs"
                       (physical-device)
                       nil)
  (physical-device '%vk:physical-device physical-device :in :handle)
  (properties '(:struct %vk:physical-device-properties) nil :out))

;; 1d
(defvk-fill-arbitrary-buffer-fun (get-query-pool-results
                                  %vk:get-query-pool-results
                                  "docs"
                                  (device query-pool first-query query-count data-size data stride)
                                  ((flags nil)))
  (device '%vk:device device :in :handle)
  (query-pool '%vk:query-pool query-pool :in :handle)
  (first-query :uint32 first-query :in)
  (query-count :uint32 query-count :in)
  (data-size :uint32 data-size :in)
  (data :pointer data :in :handle) ;; that's actually not a handle, maybe add a second keyword (just find :pointer?)
  (stride '%vk:device-size stride :in)
  (flags '%vk:query-result-flags flags :in :optional))

(defvk-get-structs-fun (get-physical-device-queue-family-properties-2
                        %vk:get-physical-device-queue-family-properties-2
                        "docs"
                        (physical-device)
                        nil
                        queue-family-device-property-count
                        queue-family-properties
                        t)
  (physical-device '%vk:physical-device physical-device :in :handle)
  (queue-family-device-property-count :uint32 nil :out)
  (queue-family-properties '(:struct %vk:queue-family-properties-2) nil :out))
