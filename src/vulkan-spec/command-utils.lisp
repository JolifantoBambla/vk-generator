#|
 Copyright(c) 2021 - Lukas Herzberger <herzberger.lukas@gmail.com>
 SPDX-License-Identifier: MIT

 Copyright(c) 2015-2020 - NVIDIA CORPORATION
 SPDX-License-Identifier: Apache-2.0
|#

(in-package #:vulkan-spec)

(defun reverse-hash-table (hash-table)
  (let ((result (make-hash-table)))
    (loop for k being each hash-key of hash-table using (hash-value v)
          do (push k (gethash v result)))
    result))

(defun split-len-by-struct-member (name)
  "Splits a given len NAME into two parts: the name of the referenced parameter and the slot name containing the length within the referenced parameter.
Returns NIL if the given NAME does not reference a slot of another parameter."
  (let ((name-parts (cl-ppcre:split "->" name)))
    (when (= (length name-parts) 1)
      ;; older version of the Vulkan API registry used the notation parameter::member instead of parameter->member
      (setf name-parts (cl-ppcre:split "::" name)))
    (when (and (= (length name-parts) 2))
      name-parts)))

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
                           (string= (second name-parts) (name m)))
                         (members struct))
                () "Structure <~a> has no member named <~a>" (name struct) (second name-parts))
        t))))

(defun structure-chain-anchor-p (struct-name vk-spec)
  "Determines if STRUCT-NAME names a struct type in the Vulkan API that is a structure chain anchor."
  (when (alexandria:starts-with-subseq "Vk" struct-name)
    (let ((struct (or (gethash struct-name (structures vk-spec))
                      (find-if (lambda (s)
                                 (find struct-name (aliases s) :test #'string=))
                               (alexandria:hash-table-values (structures vk-spec))))))
      (and struct
           (find (name struct) (extended-structs vk-spec) :test #'string=)))))

(defun determine-vector-param-indices (params vk-spec)
  "Creates a mapping of indices of array arguments to indices of the arguments specifying the number of elements within the array in a given sequence of PARAM instances.

E.g.: In \"vkQueueSubmit\" the parameter \"submitCount\" specifies the number of \"VkSubmitInfo\" instances in \"pSubmits\".

Note: For arbitrary data sizes (i.e. the vector parameter is a void pointer) the vector parameter and its size parameter are ignored.
Both are treated as unrelated input parameters of the resulting function.
E.g.: \"pData\" and \"dataSize\" in \"vkGetQueryPoolResults\".
"
  (let ((vector-param-indices (make-hash-table :test 'equal)))
    (loop for param in params and param-index from 0
          for len = (len param)
          when (and len
                    (not (string= "void" (type-name (type-info param)))))
          do (let ((len-param-index
                     (position-if
                      (lambda (p)
                        (or (string= len (name p))
                            (len-by-struct-member-p len p vk-spec)))
                      params)))
               (when len-param-index
                 (setf (gethash param-index vector-param-indices) len-param-index))))
    vector-param-indices))

(defun determine-count-to-vector-param-indices (params vk-spec)
  (reverse-hash-table (determine-vector-param-indices params vk-spec)))

(defun determine-non-const-pointer-param-indices (params)
  "Find all indices of PARAM instances describing output arguments in a given sequence of PARAM instances."
  (loop for param in params and param-index from 0
        when (and (non-const-pointer-p (type-info param))
                  (not (find (type-name (type-info param)) *special-pointer-types* :test #'string=))
                  (not (string= "void" (type-name (type-info param)))))
        collect param-index))

(defun determine-const-pointer-param-indices (params vk-spec)
  "Find all indices of PARAM instances describing input arguments in a given sequence of PARAM instances."
  (let ((non-const-pointer-param-indices (determine-non-const-pointer-param-indices params)))
    (loop for param-index from 0 to (1- (length params))
          unless (find param-index non-const-pointer-param-indices)
          collect param-index)))

(defun get-vector-params (command vk-spec)
  (with-slots (params) command
    (loop for i in (alexandria:hash-table-keys
                    (determine-vector-param-indices params vk-spec))
          collect (nth i params))))

(defun get-vector-count-params (command vk-spec)
  (with-slots (params) command
    (loop for i in (alexandria:hash-table-values
                    (determine-vector-param-indices params vk-spec))
          collect (nth i params))))

(defun get-output-params (command)
  (with-slots (params) command
    (loop for i in (determine-non-const-pointer-param-indices params)
          collect (nth i params))))

(defun get-handle-params (command vk-spec)
  (let ((output-params (get-output-params command)))
    (remove-if-not (lambda (p)
                     (and (not (find p output-params))
                          (handlep (type-name (type-info p)) vk-spec)))
                   (params command))))

(defun get-non-struct-params (command vk-spec)
  (sorted-elements
   (let ((vector-count-params (get-vector-count-params command vk-spec))
         (output-params (get-output-params command)))
     (with-slots (structures handles) vk-spec
       (remove-if (lambda (p)
                    (or (gethash (type-name (type-info p)) structures)
                        (gethash (type-name (type-info p)) handles)
                        (find p vector-count-params)
                        (find p output-params)))
                  (params command))))))

(defun get-struct-params (command vk-spec)
  (sorted-elements
   (let ((output-params (get-output-params command)))
     (with-slots (structures) vk-spec
       (remove-if-not (lambda (p)
                        (and (gethash (type-name (type-info p)) structures)
                             (not (find p output-params))))
                      (params command))))))

(defun get-required-params (command vk-spec)
  (sorted-elements (remove-if #'optional-p
                              (concatenate 'list
                                           (get-handle-params command vk-spec)
                                           (get-non-struct-params command vk-spec)
                                           (get-struct-params command vk-spec)))))

(defun get-optional-params (command vk-spec)
  (sorted-elements (remove-if-not #'optional-p
                                  (concatenate 'list
                                               (get-handle-params command vk-spec)
                                               (get-non-struct-params command vk-spec)
                                               (get-struct-params command vk-spec)))))

(defun get-skipped-input-params (command vk-spec)
  (with-slots (params) command
    (let ((vector-count-params (get-vector-count-params command vk-spec))
          (count-to-vector-param-indices (determine-count-to-vector-param-indices params vk-spec))
          (non-const-pointer-param-indices (determine-non-const-pointer-param-indices params))
          (required-params (get-required-params command vk-spec))
          (optional-params (get-optional-params command vk-spec)))
      (remove-if-not (lambda (p)
                       (and (find p vector-count-params)
                            (not (intersection (gethash (position p params) count-to-vector-param-indices)
                                               non-const-pointer-param-indices))))
                     (concatenate 'list
                                  required-params
                                  optional-params)))))

(defun determine-command-type (command vk-spec)
  (with-slots (name
               params
               return-type)
      command
    (with-slots (handles
                 base-types)
        vk-spec
      (let ((output-params (get-output-params command))
            (non-const-pointer-param-indices (determine-non-const-pointer-param-indices params))
            (vector-params (get-vector-params command vk-spec))
            (vector-param-indices (determine-vector-param-indices params vk-spec)))
        (cond
          ((not output-params)
           ;; case 0a: no or implicit return value - e.g. vkDestroyInstance
           ;; case 0b: non-trivial return value - e.g. vkGetInstanceProcAddr
           :simple)
          ((= (length non-const-pointer-param-indices) 1)
           (let ((ret (first output-params)))
             (if (or (handlep (type-name (type-info ret)) vk-spec)
                     ;; added the next two conditions, since function return e.g. a uint64_t* were
                     ;; falsely classified as get-struct-funs
                     (gethash (type-name (type-info ret)) *vk-platform*)
                     (gethash (type-name (type-info ret)) base-types))
                 ;; 1) handle type
                 (if (not (find ret vector-params))
                     ;; case 1a-1: create a handle - e.g. vkCreateInstance
                     ;; case 1a-2: get an existing handle - e.g. vkGetDeviceQueue
                     :create-single-handle
                     ;; 1b-1) vector of handles, where the output vector uses the same len as an input vector - e.g. vkCreateGraphicsPipelines
                     ;; 1b-2) vector of handles using len-by-struct-member - e.g. vkAllocateCommandBuffers
                     :create-multiple-handles)
                 ;; 2) structure chain anchor
                 (if (or (structure-chain-anchor-p (type-name (type-info (nth (first non-const-pointer-param-indices) params)))
                                                   vk-spec)
                         (not (gethash (first non-const-pointer-param-indices) vector-param-indices)))
                     ;; case 1c-1: get a struct extended by its NEXT slot - e.g. vkGetPhysicalDeviceFeatures2
                     ;; case 1c-2: get a struct without NEXT slot - e.g. vkGetPhysicalDeviceProperties
                     (if (structure-chain-anchor-p (type-name (type-info (nth (first non-const-pointer-param-indices) params))) vk-spec)
                         (progn (format t "structure chain anchor: ~a~%" (type-name (type-info (nth (first non-const-pointer-param-indices) params))))
                                :get-single-struct)
                         :get-single-struct)
                     ;; TODO: with the current implementation such functions are actually simple-funs which don't return anything -> breaks consistency!
                     ;; case 1d: arbitrary data as output param - e.g. vkGetQueryPoolResults
                     :fill-arbitrary-buffer))))
          ((= (length non-const-pointer-param-indices) 2)
           (if (or (structure-chain-anchor-p (type-name (type-info (nth (second non-const-pointer-param-indices) params)))
                                             vk-spec)
                   (and (= (hash-table-count vector-param-indices) 1)
                        (string= "void" return-type)))
               ;; case 2a: get list of structs - e.g. vkGetPhysicalDeviceQueueFamilyProperties2
               :get-multiple-structs
               (cond
                 ;; todo: find out which function falls (or should fall) into that category
                 ;; case 2b: two returns and a non-trivial success code, no array - e.g. ???
                 ((= (hash-table-count vector-param-indices) 0)
                  :get-two-non-array-values)
                 ;; case 2c: enumerate - e.g. vkEnumeratePhysicalDevices
                 ((= (hash-table-count vector-param-indices) 1)
                  :enumerate-single-array)
                 ;; case 2d: return multiple values. one array of the same size as an input array and one additional non-array value - e.g. vkGetCalibratedTimestampsEXT
                 ((= (hash-table-count vector-param-indices) 2)
                  :get-array-and-non-array-value))))
          ((= (length non-const-pointer-param-indices) 3)
           ;; case 3: return two arrays using the same counter which is also an output argument - e.g vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR
           :enumerate-two-arrays)
          (t
           (warn "Never encountered a function like <~a>!" name)
           :unknown))))))

(defun determine-command-type-2 (command vk-spec)
  (with-slots (name
               params
               return-type
               success-codes
               error-codes)
      command
    (with-slots (handles
                 base-types)
        vk-spec
      (let ((return-param-indices (determine-non-const-pointer-param-indices params))
            (vector-param-indices (determine-vector-param-indices params vk-spec))
            (const-pointer-param-indices (determine-const-pointer-param-indices params vk-spec)))
        (if (string= "VkResult" return-type)
                  (if (= 1 (length success-codes))
                      (if (not error-codes)
                          ;single-success-no-errors
                          (if (not return-param-indices) then [else])
                              (if (not vector-param-indices)
                                  (if (not const-pointer-param-indices)
                                      :case1)
                                  :unknown)
                              (if (= 1 (length vector-param-indices))
                                  (if (value-p (type-info (nth (first vector-param-indices) params))) ;; note: this is vpi[0]->second
                                      (if (handle-p (type-name (type-info (nth (first vector-param-indices) params)))) ;; note: this is vpi[0]->first
                                          :case2
                                          :unknown)
                                      :unknown)
                                  :unknown))
                          ;single-success-with-errors
                          (cond
                            ((= 0 (length return-param-indices))
                             ;result-single-success-with-errors-0-return
                             (cond
                               ((= 0 (length vector-param-indices))
                                (cond
                                  ((= 0 (length const-pointer-param-indices))
                                   :case3)
                                  ((= 1 (length const-pointer-param-indices))
                                   (if (not (string= "void" (type-name (type-info (nth (first non-const-pointer-param-indices) params))))) ;; note: this is vpi[0]->first
                                        :case4
                                        :unknown))
                                  (t :unknown)))
                               ((= 1 (length vector-param-indices))
                                (if (value-p (type-info (nth (first vector-param-indices) params))) ;; note: this is vpi[0]->second
                                    (if (not (string= "void" (type-name (type-info (nth (first vector-param-indices) params))))) ;; note: this is vpi[0]->first
                                        :case5
                                        :unknown)
                                    :unknown))
                               (t :unknown)))
                            ((= 1 (length return-param-indices))
                             :result-single-success-with-errors-1-return)
                            ((= 2 (length return-param-indices))
                             :result-single-success-with-errors-2-return)
                            (t :unknown)))
                      (if (not error-codes)
                          :multi-success-no-errors
                          :multi-success-with-errors))
                  (if (string= "void" return-type)
                        (cond
                          ((= 0 (length return-param-indices))
                           :void-0-return)
                          ((= 1 (length return-param-indices))
                           :void-1-return)
                          ((= 2 (length return-param-indices))
                           :void-2-return)
                          (t :unknown))
                      :value)))

      ;;;; old stuff

      
      (let ((output-params (get-output-params command))
            (non-const-pointer-param-indices (determine-non-const-pointer-param-indices params))
            (vector-params (get-vector-params command vk-spec))
            (vector-param-indices (determine-vector-param-indices params vk-spec)))
        (cond
          ((not output-params)
           ;; case 0a: no or implicit return value - e.g. vkDestroyInstance
           ;; case 0b: non-trivial return value - e.g. vkGetInstanceProcAddr
           :simple)
          ((= (length non-const-pointer-param-indices) 1)
           (let ((ret (first output-params)))
             (if (or (handlep (type-name (type-info ret)) vk-spec)
                     ;; added the next two conditions, since function return e.g. a uint64_t* were
                     ;; falsely classified as get-struct-funs
                     (gethash (type-name (type-info ret)) *vk-platform*)
                     (gethash (type-name (type-info ret)) base-types))
                 ;; 1) handle type
                 (if (not (find ret vector-params))
                     ;; case 1a-1: create a handle - e.g. vkCreateInstance
                     ;; case 1a-2: get an existing handle - e.g. vkGetDeviceQueue
                     :create-single-handle
                     ;; 1b-1) vector of handles, where the output vector uses the same len as an input vector - e.g. vkCreateGraphicsPipelines
                     ;; 1b-2) vector of handles using len-by-struct-member - e.g. vkAllocateCommandBuffers
                     :create-multiple-handles)
                 ;; 2) structure chain anchor
                 (if (or (structure-chain-anchor-p (type-name (type-info (nth (first non-const-pointer-param-indices) params)))
                                                   vk-spec)
                         (not (gethash (first non-const-pointer-param-indices) vector-param-indices)))
                     ;; case 1c-1: get a struct extended by its NEXT slot - e.g. vkGetPhysicalDeviceFeatures2
                     ;; case 1c-2: get a struct without NEXT slot - e.g. vkGetPhysicalDeviceProperties
                     (if (structure-chain-anchor-p (type-name (type-info (nth (first non-const-pointer-param-indices) params))) vk-spec)
                         (progn (format t "structure chain anchor: ~a~%" (type-name (type-info (nth (first non-const-pointer-param-indices) params))))
                                :get-single-struct)
                         :get-single-struct)
                     ;; TODO: with the current implementation such functions are actually simple-funs which don't return anything -> breaks consistency!
                     ;; case 1d: arbitrary data as output param - e.g. vkGetQueryPoolResults
                     :fill-arbitrary-buffer))))
          ((= (length non-const-pointer-param-indices) 2)
           (if (or (structure-chain-anchor-p (type-name (type-info (nth (second non-const-pointer-param-indices) params)))
                                             vk-spec)
                   (and (= (hash-table-count vector-param-indices) 1)
                        (string= "void" return-type)))
               ;; case 2a: get list of structs - e.g. vkGetPhysicalDeviceQueueFamilyProperties2
               :get-multiple-structs
               (cond
                 ;; todo: find out which function falls (or should fall) into that category
                 ;; case 2b: two returns and a non-trivial success code, no array - e.g. ???
                 ((= (hash-table-count vector-param-indices) 0)
                  :get-two-non-array-values)
                 ;; case 2c: enumerate - e.g. vkEnumeratePhysicalDevices
                 ((= (hash-table-count vector-param-indices) 1)
                  :enumerate-single-array)
                 ;; case 2d: return multiple values. one array of the same size as an input array and one additional non-array value - e.g. vkGetCalibratedTimestampsEXT
                 ((= (hash-table-count vector-param-indices) 2)
                  :get-array-and-non-array-value))))
          ((= (length non-const-pointer-param-indices) 3)
           ;; case 3: return two arrays using the same counter which is also an output argument - e.g vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR
           :enumerate-two-arrays)
          (t
           (warn "Never encountered a function like <~a>!" name)
           :unknown))))))
