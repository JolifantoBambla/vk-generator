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

(defun determine-vector-param-indices (params vk-spec &optional (ignore-void t))
  "Creates a mapping of indices of array arguments to indices of the arguments specifying the number of elements within the array in a given sequence of PARAM instances.

E.g.: In \"vkQueueSubmit\" the parameter \"submitCount\" specifies the number of \"VkSubmitInfo\" instances in \"pSubmits\".

Note: For arbitrary data sizes (i.e. the vector parameter is a void pointer) the vector parameter and its size parameter are ignored by default.
Both are treated as unrelated input parameters of the resulting function.
E.g.: \"pData\" and \"dataSize\" in \"vkGetQueryPoolResults\".
To disable this behaviour pass NIL as IGNORE-VOID parameter.
"
  (let ((vector-param-indices (make-hash-table :test 'equal)))
    (loop for param in params and param-index from 0
          for len = (len param)
          when (and len
                    (or (not ignore-void)
                        (not (string= "void" (type-name (type-info param))))))
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

(defun determine-non-const-pointer-param-indices (params &optional (ignore-void t))
  "Find all indices of PARAM instances describing output arguments in a given sequence of PARAM instances.

Note: By default void* are ignored and treated as input arguments.
To disable this behaviour pass NIL as the IGNORE-VOID parameter.
"
  (loop for param in params and param-index from 0
        when (and (non-const-pointer-p (type-info param))
                  (not (find (type-name (type-info param)) *special-pointer-types* :test #'string=))
                  (or (not ignore-void)
                      (not (string= "void" (type-name (type-info param))))))
        collect param-index))

(defun determine-const-pointer-param-indices (params)
  "Find all indices of PARAM instances describing const pointer arguments in a given sequence of PARAM instances."
  (loop for param in params and param-index from 0
        when (or (const-pointer-p (type-info param))
                 (and (not (non-const-pointer-p (type-info param)))
                      (member (type-name (type-info param)) *special-pointer-types* :test #'string=)))
        collect param-index))

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
                     #|(if (structure-chain-anchor-p (type-name (type-info (nth (first non-const-pointer-param-indices) params))) vk-spec)
                         (progn
                           (format t "structure chain anchor: ~a~%" (type-name (type-info (nth (first non-const-pointer-param-indices) params))))
                           :get-single-struct)
                         (progn
                           (if (gethash (type-name (type-info (nth (first non-const-pointer-param-indices) params))) (structures vk-spec))
                               (format t "has ~:[no~;a~] next slot: ~a~%"
                                       (find-if (lambda (m)
                                                  (string= "pNext" (name m)))
                                                (members (gethash (type-name (type-info (nth (first non-const-pointer-param-indices) params))) (structures vk-spec))))
                                       (type-name (type-info (nth (first non-const-pointer-param-indices) params))))
                               (format t "not a struct type: ~a~%"
                                       (type-name (type-info (nth (first non-const-pointer-param-indices) params)))))
                           :get-single-struct))|#
                     :get-single-struct
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

(define-condition command-classification-error (error)
  ((detail :initarg :detail
           :initform ""
           :reader detail)
   (command :initarg :command
            :initform ""
            :reader command)
   (vk-spec :initarg :vk-spec
            :initform (error "VK-SPEC must be provided")
            :reader vk-spec))
  (:report (lambda (condition stream)
             (with-slots (condition
                          vk-spec
                          detail)
                 condition
               (with-slots (params)
                   command
                 (let ((return-param-indices (loop for param in params and param-index from 0
                                                   when (and (non-const-pointer-p (type-info param))
                                                             (not (find (type-name (type-info param)) *special-pointer-types* :test #'string=)))
                                                   collect param-index)) ;; (determine-non-const-pointer-param-indices params)
                       (vector-param-indices (make-hash-table :test 'equal)) ;; (determine-vector-param-indices params vk-spec)
                       (const-pointer-param-indices (determine-const-pointer-param-indices params)))
                   (loop for param in params and param-index from 0
                         for len = (len param)
                         when len
                         do (let ((len-param-index
                                    (position-if
                                     (lambda (p)
                                       (or (string= len (name p))
                                           (len-by-struct-member-p len p vk-spec)))
                                     params)))
                              (when len-param-index
                                (setf (gethash param-index vector-param-indices) len-param-index))))
                   (format stream "Never encountered a command like <~a>~%~t~a~%~tReturn parameters: ~a~%~tVector parameters: ~a~%~tConst pointer parameters~a~&"
                           command
                           detail
                           (loop for idx in (sort return-param-indices #'<)
                                 collect (nth idx params))
                           (loop for idx in (sort (alexandria:hash-table-keys vector-param-indices) #'<)
                                 for vec-param = (nth idx params)
                                 for counter-param = (nth idx params)
                                 collect (format nil "~a counted by ~a" vec-param counter-param))
                           (loop for idx in (sort const-pointer-param-indices #'<)
                                 collect (nth idx params)))))))))

(defmacro %with-cmd-data ((params
                           non-const-pointer-param-indices
                           const-pointer-param-indices
                           vector-param-indices
                           check-cmd
                           command
                           vk-spec
                           &key (ignore-void t))
                          &body body)
  (let ((validp (gensym))
        (reason (gensym)))
    `(with-slots ((,params params))
         ,command
       (let ((,non-const-pointer-param-indices (determine-non-const-pointer-param-indices ,params ,ignore-void))
             (,const-pointer-param-indices (determine-const-pointer-param-indices ,params))
             (,vector-param-indices (determine-vector-param-indices ,params ,vk-spec ,ignore-void)))
         (flet ((,check-cmd (,validp ,reason)
                  (unless ,validp
                    (error 'command-classification-error ,reason ,command ,vk-spec))))
           (progn ,@body))))))

;; generateCommandResultSingleSuccessNoErrors
(defun classify-result-single-success-no-errors (command params return-param-indices vector-param-indices const-pointer-param-indices vk-spec)
  (assert (not return-param-indices) () "Expected no return param indices for command ~a" command)
  (if (= 0 (hash-table-count vector-param-indices))
      (progn
        (assert (not const-pointer-param-indices) () "Expected no const pointer param indices for command ~a" command)
        :case-1)
      (progn
        (assert (= 1 (hash-table-count vector-param-indices)) () "Expected exactly one vector param index for command ~a" command)
        (let ((vector-param-index (first (alexandria:hash-table-keys vector-param-indices))))
          (assert (value-p (type-info (nth (gethash vector-param-index vector-param-indices) params))) () "Expected counter param to be of a value type ~a" command)
          (assert (handlep (type-name (type-info (nth vector-param-index params))) vk-spec) () "Expected vector param to be of handle type ~a" command))
        :case-2)))

;; generateCommandResultSingleSuccessWithErrors0Return
(defun classify-result-single-siccess-with-errors-0-return (command params return-param-indices vector-param-indices const-pointer-param-indices vk-spec)
  (assert (< (hash-table-count vector-param-indices) 2) () "Expected less than 2 vector param indices for command ~a" command)
  (cond
    ((= 0 (hash-table-count vector-param-indices))
     (assert (< (length const-pointer-param-indices) 2) () "Expected less than 2 const pointer param indices for command ~a" command)
     (cond
       ((= 0 (length const-pointer-param-indices))
        :case-3)
       ((= 1 (length const-pointer-param-indices))
        (assert (not (string= "void" (type-name (type-info (nth (first const-pointer-param-indices) params))))) () "Expected type of output param to be void for command ~a" command)
        :case-4)))
    ((= 1 (hash-table-count vector-param-indices))
     (let ((vector-param-index (first (alexandria:hash-table-keys vector-param-indices))))
       (assert (value-p (type-info (nth (gethash vector-param-index vector-param-indices) params))) () "Expected counter param to be of a value type ~a" command)
       (assert (not (string= "void" (type-name (type-info (nth vector-param-index params))))) () "Expected vector param to not be void for command ~a" command))
     :case-5)))

;; generateCommandResultSingleSuccessWithErrors1ReturnHandle
(defun classify-result-single-success-with-errors-1-return-handle (command params return-param-indices vector-param-indices const-pointer-param-indices return-param-index return-param vk-spec)
  (assert (< (hash-table-count vector-param-indices) 3) () "Expected less than 3 vector param indices for command ~a" command)
  (cond
    ;; generateCommandResultSingleSuccessWithErrors1ReturnHandle0Vector
    ((= 0 (hash-table-count vector-param-indices))
     :case-6)

    ;; generateCommandResultSingleSuccessWithErrors1ReturnHandle1Vector
    ((= 1 (hash-table-count vector-param-indices))
     (let ((vector-param-index (first (alexandria:hash-table-keys vector-param-indices))))
       (assert (eq return-param-index vector-param-index) () "Expected return param to be the first vector param for command ~a" command)
       (assert (len-by-struct-member-p (len (nth vector-param-index params)) (nth (gethash vector-param-index vector-param-indices) params) vk-spec) () "Expected vector to be counted by a struct member for command ~a" command))
     :case-7)

    ;; generateCommandResultSingleSuccessWithErrors1ReturnHandle2Vector
    ((= 2 (hash-table-count vector-param-indices))
     (let* ((vector-param-indices-keys (sort (alexandria:hash-table-keys vector-param-indices) #'<))
            (first-vector-param-index (first vector-param-indices-keys))
            (second-vector-param-index (second vector-param-indices-keys)))
       (assert (= return-param-index second-vector-param-index) () "Expected return param to be the second vector param for command ~a" command)
       (assert (= (gethash first-vector-param-index vector-param-indices)
                  (gethash second-vector-param-index vector-param-indices))
               () "Expected both vector params to be counted by same param for command ~a" command)
       (assert (value-p (type-info (nth (gethash first-vector-param-index vector-param-indices) params)))
               () "Expected counter to be a value for command ~a" command)
       (assert (structure-chain-anchor-p (type-name (type-info (nth first-vector-param-index params))) vk-spec)
               () "Expected first vector param to be a structure chain anchor for command ~a" command))
     :case-8)))

;; generateCommandResultSingleSuccessWithErrors1ReturnChain
(defun classify-result-single-success-with-errors-1-return-chain (command params return-param-indices vector-param-indices const-pointer-param-indices return-param-index return-param vk-spec)
  (assert (= 0 (hash-table-count vector-param-indices))
          () "Expected no vector params for command ~a" command)
  :case-9)

;; generateCommandResultSingleSuccessWithErrors1ReturnVoid
(defun classify-result-single-success-with-errors-1-return-void (command params return-param-indices vector-param-indices const-pointer-param-indices return-param-index return-param vk-spec)
  (assert (< (hash-table-count vector-param-indices) 3)
          () "Expected less than 3 vector params for command ~a" command)
  (cond
    ((= 0 (hash-table-count vector-param-indices))
     :case-10)
    ((= 1 (hash-table-count vector-param-indices))
     (let ((vector-param-index (first (alexandria:hash-table-keys vector-param-indices))))
       (assert (= return-param-index vector-param-index)
               () "Expected return param be the vector param for command ~a" command)
       (assert (value-p (type-info (nth (gethash vector-param-index vector-param-indices) params)))
               () "Expected counter for vector param to be a value for command ~a" command))
     :case-11)
    ((= 2 (hash-table-count vector-param-indices))
     (let* ((vector-param-indices-keys (sort (alexandria:hash-table-keys vector-param-indices) #'<))
            (first-vector-param-index (first vector-param-indices-keys))
            (second-vector-param-index (second vector-param-indices-keys)))
       (assert (= return-param-index second-vector-param-index)
               () "Expected return param to be the second vector param for command ~a" command)
       (assert (not (= (gethash first-vector-param-index vector-param-indices)
                       (gethash second-vector-param-index vector-param-indices)))
               () "Expected vector params not to be counted by same param for command ~a" command)
       (assert (value-p (type-info (nth (gethash first-vector-param-index vector-param-indices) params)))
               () "Expected counter for first vector param to be of a value type for command ~a" command)
       (assert (handlep (type-name (type-info (nth first-vector-param-index params))) vk-spec)
               () "Expected first vector param to be a handle for command ~a" command)
       (assert (value-p (type-info (nth (gethash second-vector-param-index vector-param-indices) params)))
               () "Expected counter for second vector param to be of a value type for command ~a" command))
     :case-12)))

;; generateCommandResultSingleSuccessWithErrors1ReturnValue
(defun classify-result-single-success-with-errors-1-return-value (command params return-param-indices vector-param-indices const-pointer-param-indices return-param-index return-param vk-spec)
  (assert (= 0 (hash-table-count vector-param-indices))
          () "Expected no vector params for command ~a" command)
  :case-13)

;; generateCommandResultSingleSuccessWithErrors1Return
(defun classify-result-single-success-with-errors-1-return (command params return-param-indices vector-param-indices const-pointer-param-indices vk-spec)
  (let* ((return-param-index (first return-param-indices))
         (return-param (nth return-param-index params)))
    (cond
      ;; generateCommandResultSingleSuccessWithErrors1ReturnHandle
      ((handlep (type-name (type-info return-param)) vk-spec)
       (classify-result-single-success-with-errors-1-return-handle command params return-param-indices vector-param-indices const-pointer-param-indices return-param-index return-param vk-spec))

      ;; generateCommandResultSingleSuccessWithErrors1ReturnChain
      ((structure-chain-anchor-p (type-name (type-info return-param)) vk-spec)
       (classify-result-single-success-with-errors-1-return-chain command params return-param-indices vector-param-indices const-pointer-param-indices return-param-index return-param vk-spec))

      ;; generateCommandResultSingleSuccessWithErrors1ReturnVoid
      ((string= "void" (type-name (type-info return-param)))
       (classify-result-single-success-with-errors-1-return-void command params return-param-indices vector-param-indices const-pointer-param-indices return-param-index return-param vk-spec))

      ;; generateCommandResultSingleSuccessWithErrors1ReturnValue
      (t
       (classify-result-single-success-with-errors-1-return-value command params return-param-indices vector-param-indices const-pointer-param-indices return-param-index return-param vk-spec)))))

;; generateCommandResultSingleSuccessWithErrors2Return
(defun classify-result-single-success-with-errors-2-return (command params return-param-indices vector-param-indices const-pointer-param-indices vk-spec)
  (let ((first-return-param (type-name (type-info (nth (first return-param-indices) params)))))
    (assert (and (not (string= "void" first-return-param))
                 (not (handlep first-return-param vk-spec))
                 (not (structure-chain-anchor-p first-return-param vk-spec)))
            () "Expected first return param neither void, nor a handle nor a structure chain anchor for command ~a" command))
  (let ((second-return-param (type-name (type-info (nth (second return-param-indices) params)))))
    (assert (and (not (string= "void" second-return-param))
                 (not (handlep second-return-param vk-spec))
                 (not (structure-chain-anchor-p second-return-param vk-spec)))
            () "Expected second return param neither void, nor a handle nor a structure chain anchor for command ~a" command))
  (assert (= 2 (hash-table-count vector-param-indices))
          () "Expected exactly two vector param indices for command ~a" command)
  (let* ((vector-param-indices-keys (sort (alexandria:hash-table-keys vector-param-indices) #'<))
         (first-vector-param-index (first vector-param-indices-keys))
         (second-vector-param-index (second vector-param-indices-keys)))
    (assert (= (first return-param-indices) second-vector-param-index)
            () "Expected first return param to be the second vector param for command ~a" command)
    (assert (not (member (second return-param-indices) vector-param-indices-keys))
            () "Expected second return param not to be a vector param for command ~a" command)
    (assert (and (not (= (first return-param-indices) (gethash first-vector-param-index vector-param-indices)))
                 (not (= (first return-param-indices) (gethash second-vector-param-index vector-param-indices))))
            () "Expected second return param not to be the counter for any of the vector params for command ~a" command)
    (assert (= (gethash first-vector-param-index vector-param-indices)
               (gethash second-vector-param-index vector-param-indices))
            () "Expected vector params to be counted by same param for command ~a" command)
    (assert (value-p (type-info (nth (gethash first-vector-param-index vector-param-indices) params)))
            () "Expected counter for first vector param to be of a value type for command ~a" command)
    (let ((first-vector-param (type-name (type-info (nth first-vector-param-index params)))))
      (assert (and (not (string= "void" first-vector-param))
                   (not (handlep first-vector-param vk-spec))
                   (not (structure-chain-anchor-p first-vector-param vk-spec)))
              () "Expected first vector param neither void, nor a handle nor a structure chain anchor for command ~a" command)))
  :case-14)

;; generateCommandResultSingleSuccessWithErrors
(defun classify-result-single-success-with-errors (command params return-param-indices vector-param-indices const-pointer-param-indices vk-spec)
  (assert (< (length return-param-indices) 3) () "Expected less than 3 return param indices for command ~a" command)
  (cond
    ;; generateCommandResultSingleSuccessWithErrors0Return
    ((= 0 (length return-param-indices))
     (classify-result-single-siccess-with-errors-0-return command params return-param-indices vector-param-indices const-pointer-param-indices vk-spec))

    ;; generateCommandResultSingleSuccessWithErrors1Return
    ((= 1 (length return-param-indices))
     (classify-result-single-success-with-errors-1-return command params return-param-indices vector-param-indices const-pointer-param-indices vk-spec))

    ;; generateCommandResultSingleSuccessWithErrors2Return
    ((= 2 (length return-param-indices))
     (classify-result-single-success-with-errors-2-return command params return-param-indices vector-param-indices const-pointer-param-indices vk-spec))))


;; generateCommandResultMultiSuccessNoErrors
(defun classify-result-multi-success-no-errors (command params return-param-indices vector-param-indices const-pointer-param-indices vk-spec)
  (assert (not return-param-indices)
          () "Expected no return params for command ~a" command)
  (assert (= 0 (hash-table-count vector-param-indices))
          () "Expected no vector param indices for command ~a" command)
  (assert (not const-pointer-param-indices)
          () "Expected no const pointer param indices for command ~a" command)
  :case-15)

;; generateCommandResultMultiSuccessWithErrors0Return
(defun classify-result-multi-success-with-errors-0-return (command params return-param-indices vector-param-indices const-pointer-param-indices vk-spec)
  (assert (< (hash-table-count vector-param-indices) 3)
          () "Expected less than 3 vector param indices for command ~a" command)
  (cond
    ((= 0 (hash-table-count vector-param-indices))
     (assert (< (length const-pointer-param-indices) 2)
             () "Expected less than 2 const pointer params for command ~a" command)
     (cond
       ((= 0 (length const-pointer-param-indices))
        :case-16)
       ((= 1 (length const-pointer-param-indices))
        (assert (not (string= "void" (type-name (type-info (nth (first const-pointer-param-indices) params)))))
                () "Expected first cost pointer param not to be of type void for command ~a" command)
        :case-17)))
    ((= 1 (hash-table-count vector-param-indices))
     (let ((vector-param-index (first (alexandria:hash-table-keys vector-param-indices))))
       (assert (value-p (type-info (nth (gethash vector-param-index vector-param-indices) params)))
               () "Expected counter for vector param to be of a value type for command ~a" command)
       (assert (handlep (type-name (type-info (nth vector-param-index params))) vk-spec)
               () "Expected vector param to be of a handle type for command ~a" command))
     :case-18)
    ((= 2 (hash-table-count vector-param-indices))
     (let* ((vector-param-indices-keys (sort (alexandria:hash-table-keys vector-param-indices) #'<))
            (first-vector-param-index (first vector-param-indices-keys))
            (second-vector-param-index (second vector-param-indices-keys)))
       (assert (= (gethash first-vector-param-index vector-param-indices)
                  (gethash second-vector-param-index vector-param-indices))
               () "Expected vector params to be counted by same param for command ~a" command)
       (assert (string= "uint32_t" (type-name (type-info (nth (gethash first-vector-param-index vector-param-indices) params))))
               () "Expected vector params to be counted by a uint32_t for command ~a" command)
       (let ((first-vector-param (type-name (type-info (nth first-vector-param-index params)))))
         (assert (and (not (string= "void" first-vector-param))
                      (not (handlep first-vector-param vk-spec))
                      (not (structure-chain-anchor-p first-vector-param vk-spec)))
                 () "Expected first vector param neither void, nor a handle nor a structure chain anchor for command ~a" command))
       (let ((second-vector-param (type-name (type-info (nth second-vector-param-index params)))))
         (assert (and (not (string= "void" second-vector-param))
                      (not (handlep second-vector-param vk-spec))
                      (not (structure-chain-anchor-p second-vector-param vk-spec)))
                 () "Expected second vector param neither void, nor a handle nor a structure chain anchor for command ~a" command)))
     :case-19)))

;; generateCommandResultMultiSuccessWithErrors1Return
(defun classify-result-multi-success-with-errors-1-return (command params return-param-indices vector-param-indices const-pointer-param-indices vk-spec)
  (let* ((return-param-index (first return-param-indices))
         (return-param (nth return-param-index params)))
    (cond
      ((string= "void" (type-name (type-info return-param)))
       (assert (= 1 (hash-table-count vector-param-indices))
               () "Expected exactly one vector param for command ~a" command)
       (let ((vector-param-index (first (alexandria:hash-table-keys vector-param-indices))))
         (assert (= return-param-index vector-param-index)
                 () "Expected return param to be the vector param for command ~a" command)
         (assert (value-p (type-info (nth (gethash vector-param-index vector-param-indices) params)))
                 () "Expected counter for vector param to be of a value type for command ~a" command))
       :case-20)
      ((handlep (type-name (type-info return-param)) vk-spec)
       (assert (= 2 (hash-table-count vector-param-indices))
               () "Expected exactly 2 vector params for command ~a" command)
       (let* ((vector-param-indices-keys (sort (alexandria:hash-table-keys vector-param-indices) #'<))
              (first-vector-param-index (first vector-param-indices-keys))
              (second-vector-param-index (second vector-param-indices-keys)))
         (assert (= return-param-index second-vector-param-index)
                 () "Expected return param to be the second vector param for command ~a: return ~a second vec ~a" command (nth return-param-index params) (nth second-vector-param-index params))
         (assert (= (gethash first-vector-param-index vector-param-indices)
                    (gethash second-vector-param-index vector-param-indices))
                 () "Expected vector params to be counted by same param for command ~a" command)
         (assert (string= "uint32_t" (type-name (type-info (nth (gethash first-vector-param-index vector-param-indices) params))))
                 () "Expected vector params to be counted by a uint32_t for command ~a" command)
         (assert (structure-chain-anchor-p (type-name (type-info (nth first-vector-param-index params))) vk-spec)
                 () "Expected first vector param to be a structure chain anchor for command ~a" command))
       :case-21)
      ((not (structure-chain-anchor-p (type-name (type-info return-param)) vk-spec))
       (assert (= 0 (hash-table-count vector-param-indices))
               () "Expected no vector params for command ~a" command)
       :case-22)
      (t
       (error "Expected return param to be either void, a handle or not to be a structure chain anchor for command ~a: ~a" command return-param)))))

;; generateCommandResultMultiSuccessWithErrors2Return
(defun classify-result-multi-success-with-errors-2-return (command params return-param-indices vector-param-indices const-pointer-param-indices vk-spec)
  (let* ((first-return-param-index (first return-param-indices))
         (second-return-param-index (second return-param-indices))
         (first-return-param (type-name (type-info (nth first-return-param-index params))))
         (second-return-param (type-name (type-info (nth second-return-param-index params)))))
    (assert (or (string= "size_t" first-return-param)
                (string= "uint32_t" first-return-param))
            () "Expected first return param to be either size_t or uint32_t for command ~a" command)
    (assert (not (structure-chain-anchor-p second-return-param vk-spec))
            () "Expected second return param not to be a structure chain anchor for command ~a" command)
    (assert (= 1 (hash-table-count vector-param-indices))
            () "Expected exactly one vector param for command ~a" command)
    (let ((vector-param-index (first (alexandria:hash-table-keys vector-param-indices))))
      (assert (= first-return-param-index (gethash vector-param-index vector-param-indices))
              () "Expected first return param to be the counter of the vector param for command ~a" command)
      (assert (= second-return-param-index vector-param-index)
              () "Expected second return param to be the vector param for command ~a" command)))
  :case-23)

;; generateCommandResultMultiSuccessWithErrors3Return
(defun classify-result-multi-success-with-errors-3-return (command params return-param-indices vector-param-indices const-pointer-param-indices vk-spec)
  (let* ((first-return-param-index (first return-param-indices))
         (second-return-param-index (second return-param-indices))
         (third-return-param-index (third return-param-indices))
         (first-return-param (type-name (type-info (nth first-return-param-index params))))
         (second-return-param (type-name (type-info (nth second-return-param-index params))))
         (third-return-param (type-name (type-info (nth third-return-param-index params)))))
    (assert (string= "uint32_t" first-return-param)
            () "Expected first return param to be a uint32_t for command ~a" command)
    (assert (and (not (string= "void" second-return-param))
                 (not (handlep second-return-param vk-spec))
                 (not (structure-chain-anchor-p second-return-param vk-spec)))
            () "Expected second return param neither void, nor a handle nor a structure chain anchor for command ~a" command)
    (assert (and (not (string= "void" third-return-param))
                 (not (handlep third-return-param vk-spec))
                 (not (structure-chain-anchor-p third-return-param vk-spec)))
            () "Expected third return param neither void, nor a handle nor a structure chain anchor for command ~a" command)
    (assert (= 2 (hash-table-count vector-param-indices))
            () "Expected exactly two vector params for command ~a" command)
    (let* ((vector-param-indices-keys (sort (alexandria:hash-table-keys vector-param-indices) #'<))
           (first-vector-param-index (first vector-param-indices-keys))
           (second-vector-param-index (second vector-param-indices-keys)))
      (assert (= (gethash first-vector-param-index vector-param-indices)
                 (gethash second-vector-param-index vector-param-indices))
              () "Expected vector params to be counted by same param for command ~a" command)
      (assert (= first-return-param-index (gethash first-vector-param-index vector-param-indices))
              () "Expected first return param to be the counter of the vector params for command ~a" command)
      (assert (= second-return-param-index first-vector-param-index)
              () "Expected second return param to be the first vector param for command ~a" command)
      (assert (= third-return-param-index second-vector-param-index)
              () "Expected third return param to be the second vector param for command ~a" command)))
  :case-24)

;; generateCommandResultMultiSuccessWithErrors
(defun classify-result-multi-success-with-errors (command params return-param-indices vector-param-indices const-pointer-param-indices vk-spec)
  (assert (< (length return-param-indices) 4)
          () "Expected less than 4 return params for command ~a" command)
  (cond
    ;; generateCommandResultMultiSuccessWithErrors0Return
    ((= 0 (length return-param-indices))
     (classify-result-multi-success-with-errors-0-return command params return-param-indices vector-param-indices const-pointer-param-indices vk-spec))

    ;; generateCommandResultMultiSuccessWithErrors1Return
    ((= 1 (length return-param-indices))
     (classify-result-multi-success-with-errors-1-return command params return-param-indices vector-param-indices const-pointer-param-indices vk-spec))

    ;; generateCommandResultMultiSuccessWithErrors2Return
    ((= 2 (length return-param-indices))
     (classify-result-multi-success-with-errors-2-return command params return-param-indices vector-param-indices const-pointer-param-indices vk-spec))

    ;; generateCommandResultMultiSuccessWithErrors3Return
    ((= 3 (length return-param-indices))
     (classify-result-multi-success-with-errors-3-return command params return-param-indices vector-param-indices const-pointer-param-indices vk-spec))))

;; generateCommandVoid0Return
(defun classify-void-0-return (command params return-param-indices vector-param-indices const-pointer-param-indices vk-spec)
  (cond
    ((= 0 (hash-table-count vector-param-indices))
     (if (not (find-if (lambda (idx)
                         (not (string= "void" (type-name (type-info (nth idx params))))))
                       const-pointer-param-indices))
         :case-25
         :case-26))
    ((= 1 (hash-table-count vector-param-indices))
     (let ((counter-type (type-info (nth (first (alexandria:hash-table-values vector-param-indices)) params))))
       (assert (value-p counter-type)
               () "Expected counter param to be of a value type for command ~a" command)
       (assert (or (string= "uint32_t" (type-name counter-type))
                   (string= "VkDeviceSize" (type-name counter-type)))
               () "Expected counter type to be either uint32_t or VkDeviceSize for command ~a" command))
     :case-27)
    ((not (find-if (lambda (p)
                     (let ((counter-param (nth (gethash p vector-param-indices) params))
                           (vector-param (nth p params)))
                       (or (not (value-p (type-info counter-param)))
                           (not (string= "uint32_t" (type-name (type-info counter-param))))
                           (string= "void" (type-name (type-info vector-param))))))
                   (alexandria:hash-table-keys vector-param-indices)))
     :case-28)
    (t
     (error "Expected less than 2 vector params or one counter param being either not of a value type or not a uint32_t or a void type for command ~a" command))))

;; generateCommandVoid1Return
(defun classify-void-1-return (command params return-param-indices vector-param-indices const-pointer-param-indices vk-spec)
  (let* ((return-param-index (first return-param-indices))
         (return-param-type-name (type-name (type-info (nth return-param-index params)))))
    (cond
      ((handlep return-param-type-name vk-spec)
       (assert (= 0 (hash-table-count vector-param-indices))
               () "Expected no vector params for command ~a" command)
       :case-29)
      ((structure-chain-anchor-p return-param-type-name vk-spec)
       (assert (= 0 (hash-table-count vector-param-indices))
               () "Expected no vector params for command ~a" command)
       :case-30)
      ((not (string= "void" return-param-type-name))
       (assert (< (hash-table-count vector-param-indices) 2)
               () "Expected less than 2 vector params for command ~a" command)
       (cond
         ((= 0 (hash-table-count vector-param-indices))
          :case-31)
         ((= 1 (hash-table-count vector-param-indices))
          (let* ((vector-param-index (first (alexandria:hash-table-keys vector-param-indices)))
                 (vector-param (nth vector-param-index params))
                 (vector-param-type-name (type-name (type-info vector-param)))
                 (counter-param (nth (gethash vector-param-index vector-param-indices) params)))
            (assert (not (= return-param-index vector-param-index))
                    () "Expected return param not to be the vector param for command ~a" command)
            (assert (and (not (handlep vector-param-type-name vk-spec))
                         (not (structure-chain-anchor-p vector-param-type-name vk-spec))
                         (not (string= "void" vector-param-type-name)))
                    () "Expected vector param not to be a handle or void type or not to be a structure chain anchor for command ~a" command)
            (assert (len-by-struct-member-p (len vector-param) counter-param vk-spec)
                    () "Expected vector param to be counted by a struct member for command ~a" command))
          :case-32)))
      (t
       (error "Expected return param to be either a handle type or a structure chain anchor or not to be of type void for command ~a" command)))))

;; generateCommandVoid2Return
(defun classify-void-2-return (command params return-param-indices vector-param-indices const-pointer-param-indices vk-spec)
  (let* ((first-return-param-index (first return-param-indices))
         (second-return-param-index (second return-param-indices))
         (first-return-param-type-name (type-name (type-info (nth first-return-param-index params))))
         (second-return-param-type-name (type-name (type-info (nth second-return-param-index params)))))
    (assert (string= "uint32_t" first-return-param-type-name)
            () "Expected first return param to be a uint32_t for command ~a" command)
    (cond
      ((structure-chain-anchor-p first-return-param-type-name vk-spec)
       (assert (= 1 (hash-table-count vector-param-indices))
               () "Expected exactly one vector param for command ~a" command)
       (let ((vector-param-index (first (alexandria:hash-table-keys vector-param-indices))))
         (assert (= first-return-param-index (gethash vector-param-index vector-param-indices))
                 () "Expected first return param to be the counter of the vector param for command ~a" command)
         (assert (= second-return-param-index vector-param-index)
                 () "Expected second return param to be the vector param for command ~a" command))
       :case-33)
      ((and (not (string= "void" second-return-param-type-name))
            (not (handlep second-return-param-type-name vk-spec)))
       (assert (= 1 (hash-table-count vector-param-indices))
               () "Expected exactly one vector param for command ~a" command)
       (let ((vector-param-index (first (alexandria:hash-table-keys vector-param-indices))))
         (assert (= first-return-param-index (gethash vector-param-index vector-param-indices))
                 () "Expected first return param to be the counter of the vector param for command ~a" command)
         (assert (= second-return-param-index vector-param-index)
                 () "Expected second return param to be the vector param for command ~a" command))
       :case-34)
      (t
       (error "Expected for command ~a" command)))))

;; generateCommandValue
(defun classify-value (command vk-spec)
  (%with-cmd-data (params return-param-indices const-pointer-param-indices vector-param-indices check-cmd command vk-spec :ignore-void t)
    (check-cmd (= 0 (length return-param-indices)) "Expected no return params")
    (check-cmd (= 0 (hash-table-count vector-param-indices)) "Expected no vector params")
    (check-cmd (< (length const-pointer-param-indices) 2) "Expected less than 2 const pointer params")
    (cond
      ((= 0 (length const-pointer-param-indices))
       :case-35)
      ((= 1 (length const-pointer-param-indices))
       (check-cmd (not (string= "void" (type-name (type-info (nth (first const-pointer-param-indices) params)))))
                  "Expected const pointer param not to be of void type")
       :case-36))))

;; todo: map new cases to old ones
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
      (let ((resultp (string= "VkResult" return-type))
            (voidp (string= "void" return-type))
            (return-param-indices (determine-non-const-pointer-param-indices params nil))
            (vector-param-indices (determine-vector-param-indices params vk-spec nil)) 
            (const-pointer-param-indices (determine-const-pointer-param-indices params)))
        (if (string= "VkResult" return-type)
            (if (= 1 (length success-codes))
                (if (not error-codes)
                    (classify-result-single-success-no-errors command params return-param-indices vector-param-indices const-pointer-param-indices vk-spec)
                    (classify-result-single-success-with-errors command params return-param-indices vector-param-indices const-pointer-param-indices vk-spec))
                (if (not error-codes)
                    (classify-result-multi-success-no-errors command params return-param-indices vector-param-indices const-pointer-param-indices vk-spec)
                    (classify-result-multi-success-with-errors command params return-param-indices vector-param-indices const-pointer-param-indices vk-spec)))
            (if (string= "void" return-type)
                (cond
                  ((= 0 (length return-param-indices))
                   (classify-void-0-return command params return-param-indices vector-param-indices const-pointer-param-indices vk-spec))
                  ((= 1 (length return-param-indices))
                   (classify-void-1-return command params return-param-indices vector-param-indices const-pointer-param-indices vk-spec))
                  ((= 2 (length return-param-indices))
                   (classify-void-2-return command params return-param-indices vector-param-indices const-pointer-param-indices vk-spec))
                  (t
                   (error "Expected less than 3 return params for void command ~a" command)))
                (classify-value command vk-spec)))))))
