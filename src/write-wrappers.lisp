#|
 Copyright(c) 2021 - Lukas Herzberger <herzberger.lukas@gmail.com>
 SPDX-License-Identifier: MIT

 Copyright(c) 2015-2020 - NVIDIA CORPORATION
 SPDX-License-Identifier: Apache-2.0
|#

(in-package :vk-generator)

(defparameter *special-pointer-types*
  '("Display"
    "IDirectFB"
    "wl_display"
    "xcb_connection_t")
  "A sequence of pointer types which are never used as const-qualified call arguments, but are never used as return arguments.")

(defun len-by-struct-member-p (name param vk-spec)
  "Checks if a given NAME references a MEMBER-DATA instance of a STRUCT instance and this STRUCT instances is the type of the given PARAM instance.
 
E.g.: In \"vkAllocateDescriptorSets\" the \"len\" \"pAllocateInfo->descriptorSetCount\" of parameter \"pDescriptorSets\" references the parameter \"pAllocateInfo\" of type \"VkDescriptorSetAllocateInfo\" which has a member named \"descriptorSetCount\".
"
  (let ((name-parts (cl-ppcre:split "->" name)))
    (when (= (length name-parts) 1)
      ;; older version of the Vulkan API registry used the notation parameter::member instead of parameter->member
      (setf name-parts (cl-ppcre:split "::" name)))
    (when (and (= (length name-parts) 2)
               (string= (first name-parts) (name param)))
      (let ((struct (gethash (type-name (type-info param)) (structures vk-spec))))
        (assert struct
                () "Undefined structure <~a>" (type-name (type-info param)))
        (assert (find-if (lambda (m)
                           (string= (second name-data) (name struct)))
                         (members struct))
                () "Structure <~a> has no member named <~a>" (name struct) (second name-parts))
        t))))

(defun determine-vector-param-indices (params vk-spec)
  "Creates a mapping of indices of array arguments to indices of the arguments specifying the number of elements within the array in a given sequence of PARAM instances.

E.g.: In \"vkQueueSubmit\" the parameter \"submitCount\" specifies the number of \"VkSubmitInfo\" instances in \"pSubmits\".
"
  (let ((vector-param-indices (make-hash-table :test 'equal)))
    (loop for param in params and param-index from 0
          for len = (len param)
          unless len
          do (let ((len-param-index
                     (position-if
                      (lambda (p)
                        (or (string= len (name p))
                            (len-by-struct-member-p len p vk-spec)))
                      params)))
               (when len-param-index
                 (setf (gethash param-index vector-param-indices) len-param-index))))
    vector-param-indices))

(defun determine-non-const-pointer-param-indices (params)
  "Find all indices of PARAM instances describing output arguments in a given sequence of PARAM instances."
  (loop for param in params and param-index from 0
        when (and (non-const-pointer-p (type-info param))
                   (not (find (type-name (type-info param)) *special-pointer-types* :test #'string=)))
        collect param-index))

(defun determine-const-pointer-param-indices (params vk-spec)
  "Find all indices of PARAM instances describing input arguments in a given sequence of PARAM instances."
  (let ((non-const-pointer-param-indices (determine-non-const-pointer-param-indices params)))
    (loop for param-index from 0 to (1- (length params))
          unless (find param-index non-const-pointer-param-indices)
          collect param-index)))

(defun construct-command-standard (name command-data definition-p)
  )

#|
template <typename ExtensionPropertiesAllocator, typename Dispatch>
  VULKAN_HPP_NODISCARD VULKAN_HPP_INLINE
    typename ResultValueType<std::vector<ExtensionProperties, ExtensionPropertiesAllocator>>::type
    PhysicalDevice::enumerateDeviceExtensionProperties( Optional<const std::string> layerName,
                                                        Dispatch const &            d ) const
  {
    std::vector<ExtensionProperties, ExtensionPropertiesAllocator> properties;
    uint32_t                                                       propertyCount;
    Result                                                         result;
    do
    {
      result = static_cast<Result>( d.vkEnumerateDeviceExtensionProperties(
        m_physicalDevice, layerName ? layerName->c_str() : nullptr, &propertyCount, nullptr ) );
      if ( ( result == Result::eSuccess ) && propertyCount )
      {
        properties.resize( propertyCount );
        result = static_cast<Result>(
          d.vkEnumerateDeviceExtensionProperties( m_physicalDevice,
                                                  layerName ? layerName->c_str() : nullptr,
                                                  &propertyCount,
                                                  reinterpret_cast<VkExtensionProperties *>( properties.data() ) ) );
        VULKAN_HPP_ASSERT( propertyCount <= properties.size() );
      }
    } while ( result == Result::eIncomplete );
    if ( ( result == Result::eSuccess ) && ( propertyCount < properties.size() ) )
    {
      properties.resize( propertyCount );
    }
    return createResultValue(
      result, properties, VULKAN_HPP_NAMESPACE_STRING "::PhysicalDevice::enumerateDeviceExtensionProperties" );
  }
|#

(defun make-command-docstring (command required-params optional-params vk-spec)
  ;; todo: reference VkStructs, list arguments, document return type, if command starts with vkGet -> gets foo, if command is registered as a create-func or destroy-func -> Creates/Destroys a foo handle from bla belonging to a bar, document success codes, document error codes (= signalled conditions)
  (format nil "Represents <~a>"
          (name command)))

(defparameter *result-var-name* "vk-call-result")

(defun COMMAND! (out command vk-spec)
  (let* ((fixed-function-name (fix-function-name (name command) (tags vk-spec)))
         ;; maps from array-param to array-count-param
         (vector-param-indices (determine-vector-param-indices (params command) vk-spec))
         (vector-count-params (loop for i in (alexandria:hash-table-values vector-param-indiecs)
                                    collect (nth i (params command))))
         (non-const-pointer-param-indices (determine-non-const-pointer-param-indices (params command)))
         (output-params (loop for i in non-const-param-indices
                              collect (nth i (params command))))
         (has-return-p (not (string= (return-type command) "void")))
         (non-struct-params (sorted-elements
                             (remove-if (lambda (p)
                                          (or (gethash (type-name (type-info p)) (structures vk-spec))
                                              (gethash (type-name (type-info p)) (handles vk-spec))
                                              (find p vector-count-params)
                                              (find p output-params)))
                                        (params command))))
         (struct-params (sorted-elements
                         (remove-if-not (lambda (p)
                                          (and (gethash (type-name (type-info p)) (structures vk-spec))
                                               (not (find p output-params))))
                                        (params command))))
         (required-non-struct-params (remove-if (lambda (p)
                                                    (optional-p p))
                                                non-struct-params))
         (required-struct-params (remove-if-not (lambda (p)
                                                        (optional-p p))
                                                non-struct-params))
         (optional-non-struct-params (remove-if (lambda (p)
                                                  (optional-p p))
                                                struct-params))
         (optional-struct-params (remove-if-not (lambda (p)
                                                  (optional-p p))
                                                struct-params))
         (required-params (concatenate 'list required-non-struct-params required-struct-params))
         (optional-params (concatenate 'list optional-non-struct-params optional-struct-params))
         (accumulated-indent "")
         (closing-parens 1))
    ;; base defun
    (format out "(defun ~(~a~) (~(~{~a~}~@[ &optional ~{~a~}~]~))~%  ~s~%"
            fixed-function-name
            (loop for p in (sorted-elements required-params)
                  collect (fix-slot-name (name p) (type-name (type-info p)) (tags vk-spec)))
            (loop for p in (sorted-elements optional-params)
                  collect (if (string= (name p) "pAllocator")
                              "(p-allocator *default-allocator*)"
                              (format nil "(~(~a ~a-supplied-p (cffi:null-pointer)~))"
                                      (fix-slot-name (name p) (type-name (type-info p)) (tags vk-spec)))))
            (make-command-docstring command required-params optional-params vk-spec))

    ;; if the function has a return value (this will be VkResult or VkBool32) prepare a variable to store it
    (when has-return-p
      (setf accumulated-indent (concatenate 'string "  " accumulated-indent))
      (incf closing-parens)
      (format out "~a(let ((~a nil))"
              accumulated-indent
              *result-var-name*))

    ;; TODO: YOU ARE HERE!!
    ;; if the function has primitive input args they need to be allocated
    (when non-struct-params
      (setf accumulated-indent (concatenate 'string "  " accumulated-indent))
      (incf closing-parens)
      (format out "~a(cffi:with-foreign-objects (~(~{~a~}~))"
              accumulated-indent
              (loop for p in non-struct-params
                    collect (format nil "(~(p-~a ~a~))"
                                    ;; todo: determine type of param
                                    ;; todo: determine if its an array, if so: :count!
                                    )))
      ;; todo: map input params to allocated memory

      ;; map optional input params to allocated memory (for the actual function call ~a-supplied-p must be checked again!)
      (when optional-non-struct-input-args
        (format out "~a~(~{~%  ~a~}~)"
                accumulated-indent
                (loop for arg in optional-non-struct-input-args
                      collect (format nil "(when ~a-supplied-p (setf (cffi:mem-aref p-~a ~a) ~a))"
                                      fixed-arg-name
                                      fixed-arg-name
                                      arg-type
                                      fixed-arg-name)))))
    (when struct-input-args
      (setf accumulated-indent (concatenate 'string "  " accumulated-indent))
      (incf closing-parens)
      (format out "~a(vk-alloc:with-foreign-allocated-objects (~(~{~a~}~))"
              accumulated-indent
              struct-input-args))

 ;; insert stuff from APPEND-COMMAND here

    ;; close all forms
    (format out "~{~a~}~%~%"
            (loop for i from 0 below closing-parens
                  collect ")"))))

;; actually each command call looks ultimately like this
(defun append-call (fixed-function-name skipped-args)
  (format out
          (if command-returns-something
              "~((setf result (%vk:~a~{ ~a~}))~)"
              "(%vk:~a~{ ~a~})")
          fixed-command-name
          (loop for arg in args
                collect (if (find-if (lambda (a) (string= a arg-name))
                                     skipped-args)
                            "(cffi:null-pointer)"
                            (if (optional-p arg)
                                (format nil "(if ~a-supplied-p p-~a ~a)" ;; allocator is special->never allocated by this function!
                                        arg-name
                                        arg-name
                                        arg-name)
                                (format nil "~a"
                                        arg-name))))))

(defun append-return-form (return-values)
  (format nil "(values~(~(~{ ~a~} result~)~))"
          return-values))

(defun append-enumeration-return-stuff ()
  (format nil "(loop for i from 0 below count collect (cffi:mem-aref p-~a ~a i))"
          array-arg
          array-type))

(defun append-enumerating-body ()
  (format nil "(do () ((not (eq result :incomplete))) ~a (when (eq result :success) (let ((count (cffi:mem-aref p~a ~s))) (if (> 0 count) ~a nil))))"
          (append-call output-vector-args)
          counter-arg-name
          counter-type
          (append-enumeration-return-stuff)))

;; this should construct the body of an enumerating function correctly
(append-return-form (list (append-enumerating-body)))

(defun append-command (out name command-data definition-p vk-spec)
  "Writes a command to an output stream."
  (let ((appended-function nil)
        (vector-param-indices (determine-vector-param-indices (params command-data vk-spec)))
        (non-const-pointer-param-indices (determine-non-const-pointer-param-indices (params command-data))))
    (cond
      ((= (length non-const-pointer-param-indices) 0)
       ;; three cases
       ;; 1) only VkResult returned
       ;; 2) something else returned
       ;; 3) void returned
       
       (let ((const-pointer-param-indices (determine-const-pointer-param-indices (params command-data))))
         (if (and (= (hash-table-count vector-param-indices) 0)
                  (find-if (lambda (idx)
                             (not (string= "void" (type-name (type-info (nth idx (params command-data)))))))
                           (determine-const-pointer-param-indices (params command-data))))
             (if (string= (return-type command-data) "VkResult")
                 (append-command-standard-or-enhanced)
                 (append-command-standard))
             (append-command-standard-and-enhanced))))
      ((= (length non-const-pointer-param-indices) 1)
       ;; eight cases
       ;; 1) handle type
       ;; 1a-1) single handle
       ;; 1a-2) single handle but it is not created but "vkGet"-prefixed instead
       ;; 1b-1) vector of handles using 2 vector param indices
       ;; 1b-2) vector of handles using len-by-struct-member
       ;; 2) structure chain anchor (wat?)
       ;; 2a)   appendCommandChained
       ;; 2b-1) returns VkBool32 || VkResult || void -> appendCommandStandardAndEnhanced
       ;; 2b-2) appendCommandSingular
       )
      ((= (length non-const-pointer-param-indices) 2)
       ;; four cases
       ;; 1) structure chain anchor (wat?) -> appendCommandVectorChained
       ;; 2) 0 vector-param-indices -> two returns and a non-trivial success code
       ;; 3) 1 vector-param-indices -> the size is a return value as well -> enumerate the values
       ;; 4) 2 vector-param-indices -> two returns and two vectors! But one input vector, one output vector of the same size, and one output value
       )
      ((= (length non-const-pointer-param-indices) 3)
       ;; 1 case
       ;; 1) the two vectors use the very same size parameter
       )
      (t (error "Never encountered a function like <~a>!" (name command-data))))
    (when (alias command-data)
      (loop for alias in (alias command-data)
            do (append-command out (name alias) (make-aliased-command command-data alias) definition-p vk-spec)))))
