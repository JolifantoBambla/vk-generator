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

(defun append-command (out name command-data definition-p vk-spec)
  "Writes a command to an output stream."
  (let ((appended-function nil)
        (vector-param-indices (determine-vector-param-indices (params command-data vk-spec)))
        (non-const-pointer-param-indices (determine-non-const-pointer-param-indices (params command-data))))
    (cond
      ((= (length non-const-pointer-param-indices) 0)
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
       ;;todo
       )
      ((= (length non-const-pointer-param-indices) 2))
      ((= (length non-const-pointer-param-indices) 3))
      (t (error "Never encountered a function like <~a>!" (name command-data))))
    (when (alias command-data)
      (loop for alias in (alias command-data)
            do (append-command out (name alias) (make-aliased-command command-data alias) definition-p vk-spec)))))
