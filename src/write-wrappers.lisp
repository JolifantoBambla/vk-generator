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

(defparameter *special-base-types*
  '("ANativeWindow"
    "AHardwareBuffer"))

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

(defun make-command-docstring (command required-params optional-params output-params vector-params vk-spec)
  ;; todo: reference VkStructs, list arguments, document return type, if command starts with vkGet -> gets foo, if command is registered as a create-func or destroy-func -> Creates/Destroys a foo handle from bla belonging to a bar, document success codes, document error codes (= signalled conditions)
  (flet ((format-arg-doc (param &optional (optional-p nil) (default-arg nil))
           (let ((sequence-p (member param vector-params)))
             (format nil "~% - ~a~@[~a~]: a ~@[~a~]~a~@[, defaults to: ~a~]"
                     (fix-slot-name (name param) (type-name (type-info param)) vk-spec)
                     (when optional-p
                       " (optional)")
                     (when sequence-p
                       "(OR LIST VECTOR) of ")
                     (cond
                       ((gethash (type-name (type-info param)) (structures vk-spec))
                        (format nil "(OR ~a CFFI:FOREIGN-POINTER)~@[~a~]"
                                (fix-type-name (type-name (type-info param)) (tags vk-spec))
                                (when sequence-p " instances")))
                       ((gethash (type-name (type-info param)) (handles vk-spec))
                        (format nil "~a~@[~a~]"
                                (fix-type-name (type-name (type-info param)) (tags vk-spec))
                                (when sequence-p " handles")))
                       ((not (gethash (type-name (type-info param)) *vk-platform*))
                        (format nil "~a~@[~a~]"
                                (fix-type-name (type-name (type-info param)) (tags vk-spec))
                                (when sequence-p "s")))
                       (t (format nil "~:@(~a~)"
                                  (get-type-to-declare (type-name (type-info param)) vk-spec param vector-params))))
                     default-arg))))
    (let ((referenced-types (sort
                             (remove-duplicates
                              (loop for param in (concatenate 'list
                                                              required-params
                                                              optional-params
                                                              output-params)
                                    for type-name = (type-name (type-info param))
                                    unless (gethash type-name *vk-platform*)
                                    collect (fix-type-name type-name (tags vk-spec))))
                             #'string<))
          (formatted-required-args (loop for param in required-params
                                         collect (format-arg-doc param)))
          (formatted-optional-args (loop for param in optional-params
                                         collect (format-arg-doc
                                                  param
                                                  t
                                                  (cond
                                                    ((string= "pAllocator" (name param))
                                                     "*DEFAULT-ALLOCATOR*")
                                                    (t "NIL")))))
          (formatted-output-args (loop for param in output-params
                                       collect (if (gethash (type-name (type-info param)) *vk-platform*)
                                                   (cond
                                                     ((or (string= "size_t" (type-name (type-info param)))
                                                          (alexandria:starts-with-subseq "uint" (type-name (type-info param))))
                                                      "UNSIGNED-BYTE")
                                                     ((alexandria:starts-with-subseq "int" (type-name (type-info param)))
                                                      "INTEGER")
                                                     (t (error "unhandled output arg in doc generation: ~a" (type-name (type-info param)))))
                                                   (fix-type-name (type-name (type-info param)) (tags vk-spec))))))
      (when (extension-command-p command)
        (setf formatted-optional-args
              (concatenate 'list
                           formatted-optional-args
                           (list (format nil "~% - EXTENSION-LOADER (optional): an EXTENSION-LOADER, defaults to: *DEFAULT-EXTENSION-LOADER*"))))
        (setf referenced-types
              (sort
               (concatenate 'list
                            referenced-types
                            '("EXTENSION-LOADER"))
               #'string<)))
      (unless (string= (return-type command) "void")
        (setf formatted-output-args
              (concatenate 'list
                           formatted-output-args
                           (list (cond
                                   ((or (string= "size_t" (return-type command))
                                        (alexandria:starts-with-subseq "uint" (return-type command)))
                                    "UNSIGNED-BYTE")
                                   ((alexandria:starts-with-subseq "int" (return-type command))
                                    "INTEGER")
                                   ((string= "PFN_vkVoidFunction" (return-type command))
                                    "CFFI:FOREIGN-POINTER ;; a function pointer")
                                   ((string= "VkResult" (return-type command))
                                    "RESULT")
                                   ((string= "VkBool32" (return-type command))
                                    "BOOLEAN")
                                   ((string= "VkDeviceSize" (return-type command))
                                    "DEVICE-SIZE ;; (UNSIGNED-BYTE 64)")
                                   ((string= "VkDeviceAddress" (return-type command))
                                    "DEVICE-ADDRESS ;; (UNSIGNED-BYTE 64)")
                                   (t (error "unhandled return type in doc generation: ~a" (return-type command)))))))
        (when (and (alexandria:starts-with-subseq "Vk" (return-type command))
                   (not (string= "VkBool32" (return-type command))))
          (setf referenced-types
                (sort
                 (remove-duplicates
                  (concatenate 'list
                               referenced-types
                               (list (fix-type-name (return-type command) (tags vk-spec)))))
                 #'string<))))
      (format nil "Represents [~a](https://www.khronos.org/registry/vulkan/specs/1.2-extensions/man/html/~a.html).

Args:~{~a~}~{~a~}~@[

Returns:
  (CL:VALUES~{~%    ~a~})~]~@[

Success codes:~{~% - ~a~}~]~@[

Errors signalled on codes:~{~% - ~a~}~]
~{~%See ~a~}~@[
See ~a~]~@[
See ~a~]
"
              (name command)
              (name command)
              formatted-required-args
              formatted-optional-args
              formatted-output-args
              (loop for c in (success-codes command)
                    collect (fix-bit-name c (tags vk-spec)))
              (loop for c in (error-codes command)
                    collect (fix-bit-name c (tags vk-spec)))
              referenced-types
              (when (member-if (lambda (p)
                                 (string= "pAllocator" (name p)))
                               (params command))
                "*DEFAULT-ALLOCATOR*")
              (when (extension-command-p command)
                "*EXTENSION-LOADER*")))))

(defun reverse-hash-table (hash-table)
  (let ((result (make-hash-table)))
    (loop for k being each hash-key of hash-table using (hash-value v)
          do (push k (gethash v result)))
    result))

(defun get-type-to-declare (type-name vk-spec &optional param vector-params)
  (cond
    ;; the types that can't be translated and must not come in a list
    ((or (string= "void" type-name)
         (find type-name *special-base-types* :test #'string=)
         (and (gethash type-name (types vk-spec))
              (eq :requires (category (gethash type-name (types vk-spec))))
              (not (gethash type-name *vk-platform*))))
     "cffi:foreign-pointer")
    ((and param
          vector-params
          (find param vector-params))
     "(or list vector)")
    ;; handles must come after "list" because a list of handles should be declared as type list
    ((gethash type-name (handles vk-spec))
     "cffi:foreign-pointer")
    ((string= "VkBool32" type-name)
     "boolean")
    ((or (alexandria:starts-with-subseq "float" type-name)
         (string= "double" type-name))
     "real")
    ((or (alexandria:starts-with-subseq "uint" type-name)
         (string= "size_t" type-name))
     "unsigned-byte")
    ((alexandria:starts-with-subseq "int" type-name)
     "integer")
    ((string= "char" type-name)
     "string")
    ((gethash type-name (structures vk-spec))
     (format nil "(or vk:~(~a~) cffi:foreign-pointer)"
             (fix-type-name type-name (tags vk-spec))))
    ((gethash type-name (bitmasks vk-spec))
     "(or unsigned-byte list)")
    ((or (and (gethash type-name (enums vk-spec))
              (not (is-bitmask-p (gethash type-name (enums vk-spec)))))
         (search "FlagBits" type-name))
     "keyword")
    ((gethash type-name (base-types vk-spec))
     (get-type-to-declare (type-name (gethash type-name (base-types vk-spec))) vk-spec))
    (t (error "No type declaration for: ~a" type-name))))

(defun format-type-to-declare (param vector-params vk-spec)
  (format nil "~(~a~)"
          (let ((type-name (type-name (type-info param))))
            (get-type-to-declare type-name vk-spec param vector-params))))


(defun format-required-args (required-args vector-params vk-spec)
  "Maps a list of PARAM-DATA instances into a list of argument names for the lambda list of a function in package VK."
  (loop for arg in required-args
        for i from 1
        collect (format nil "~((~a ~a)~)~@[ ~]"
                        (fix-slot-name (name arg) (type-name (type-info arg)) vk-spec)
                        (format-type-to-declare arg vector-params vk-spec)
                        (< i (length required-args)))))

(defun format-optional-args (optional-args vector-params vk-spec)
  "Maps a list of PARAM-DATA instances into a list of default argument lists for the lambda list of a function in package VK."
  (loop for arg in optional-args
        for i from 1
        collect (format nil "((~(~a ~a) ~a~))~@[ ~]"
                        (fix-slot-name (name arg) (type-name (type-info arg)) vk-spec)
                        (if (string= "pAllocator" (name arg))
                            "*default-allocator*"
                            (cond
                              ((gethash (type-name (type-info arg)) (handles vk-spec))
                               "(cffi:null-pointer)")
                              ((and (string= (type-name (type-info arg)) "char")
                                    (not (string= (postfix (type-info arg)) "**")))
                               "\"\"")
                              (t nil)))
                        (format-type-to-declare arg vector-params vk-spec)
                        (< i (length optional-args)))))

(defun format-type-name (type-name vk-spec)
  (cond
    ((find type-name *special-base-types* :test #'string=)
     "'(:pointer :void)")
    ((gethash type-name (structures vk-spec))
     (format nil "'(:~a %vk:~(~a~))"
             (if (is-union-p (gethash type-name (structures vk-spec)))
                 "union"
                 "struct")
             (fix-type-name type-name (tags vk-spec))))
    ((gethash type-name (handles vk-spec))
     (format nil "'%vk:~(~a~)" (fix-type-name type-name (tags vk-spec))))
    ((gethash type-name (enums vk-spec))
     (format nil "'%vk:~(~a~)" (fix-type-name type-name (tags vk-spec))))
    ((string= "void" type-name)
     "'(:pointer :void)")
    ((string= "char" type-name)
     ":string")
    ((string= "size_t" type-name)
     "'%vk:size-t")
    ((gethash type-name *vk-platform*)
     (format nil "~(~s~)" (gethash type-name *vk-platform*)))
    ((getf *misc-os-types* type-name)
     (format nil "~:[~;'~]~(~s~)"
             (consp (getf *misc-os-types* type-name))
             (gethash *misc-os-types* type-name)))
    ((and (gethash type-name (types vk-spec))
          (eq :requires (category (gethash type-name (types vk-spec)))))
     "'(:pointer :void)")
    (t
     (format nil "'%vk:~(~a~)" (fix-type-name type-name (tags vk-spec))))))

(defun format-arg-type (arg vk-spec)
  ""
  (format-type-name (type-name (type-info arg)) vk-spec))

(defun make-arg-qualifier-list (arg output-params optional-params vector-params vk-spec)
  (let ((qualifiers nil))
    (when (find arg optional-params)
      (push :optional qualifiers))
    (when (member arg vector-params)
      (push :list qualifiers))
    ;; todo: maybe just split all of these up into :raw and :translate - this would be way less confusing...
    ;; todo: void pointers should also be treaded as :raw instead of :handle to get rid of the ambiguity
    (when (or (and (gethash (type-name (type-info arg)) *vk-platform*)
                   (value-p (type-info arg)))
              (and (string= "char" (type-name (type-info arg)))
                   (not (string= "**" (postfix (type-info arg)))))
              (gethash (type-name (type-info arg)) (enums vk-spec))
              (gethash (type-name (type-info arg)) (bitmasks vk-spec))
              (and (gethash (type-name (type-info arg)) (base-types vk-spec))
                   (value-p (type-info arg))))
      (push :raw qualifiers))
    (when (or (gethash (type-name (type-info arg)) (handles vk-spec)) ;; it's a handle
              (string= "void" (type-name (type-info arg))) ;; it's a void pointer
              (and (gethash (type-name (type-info arg)) (types vk-spec)) ;; it's a vk-defined type
                   (eq :requires (category (gethash (type-name (type-info arg)) (types vk-spec))))
                   (not (gethash (type-name (type-info arg)) *vk-platform*))))
      (push :handle qualifiers))
    (push (if (find arg output-params)
              :out
              :in)
          qualifiers)
    qualifiers))

(defun format-vk-args (vk-args count-to-vector-param-indices output-params optional-params vector-params vk-spec)
  ""
  (loop for arg in vk-args
        for i from 0
        collect (format nil "(~(~a ~a ~a~{ ~s~}~))~@[~%~]"
                        (fix-slot-name (name arg) (type-name (type-info arg)) vk-spec)
                        (format-arg-type arg vk-spec)
                        (if (and (gethash i count-to-vector-param-indices)
                                 (not (gethash (type-name (type-info arg))
                                               (structures vk-spec)))
                                 (not (find arg output-params)))
                            (let* ((array-args (loop for j in (gethash i count-to-vector-param-indices)
                                                     for array-arg = (nth j vk-args)
                                                     unless (find array-arg output-params)
                                                     collect array-arg))
                                   (array-arg (first array-args)))
                              (format nil "(length ~(~a~))"
                                      (fix-slot-name (name array-arg) (type-name (type-info array-arg)) vk-spec)))
                            (fix-slot-name (name arg) (type-name (type-info arg)) vk-spec))
                        (make-arg-qualifier-list arg output-params optional-params vector-params vk-spec)
                        (< (+ i 1) (length vk-args)))))

(defun write-simple-fun (out command fixed-function-name required-params optional-params output-params count-to-vector-param-indices vector-params vk-spec)
  (format out "(defvk-simple-fun (~(~a~)~%" fixed-function-name)
  (format out "                   ~(%vk:~a~)~%" fixed-function-name)
  (format out "                   ~s~%" (make-command-docstring command required-params optional-params output-params vector-params vk-spec))
  (format out "                   (~(~{~a~}~))~%" (format-required-args required-params vector-params vk-spec))
  (format out "                   (~(~{~a~}~))" (format-optional-args optional-params vector-params vk-spec))
  (format out "~%                  ~(~a~)"
          (if (not (find (return-type command) '("void" "VkBool32" "VkResult") :test #'string=))
              (format-type-name (return-type command) vk-spec)
              nil))
  (when (extension-command-p command)
    (format out "~%                  t"))
  (format out ")~%")
  (format out "~{  ~(~a~)~})~%~%" (format-vk-args (params command) count-to-vector-param-indices output-params optional-params vector-params vk-spec)))

(defun write-create-handle-fun (out command fixed-function-name required-params optional-params output-params count-to-vector-param-indices vector-params vk-spec)
  (format out "(defvk-create-handle-fun (~(~a~)~%" fixed-function-name)
  (format out "                          ~(%vk:~a~)~%" fixed-function-name)
  (format out "                          ~s~%" (make-command-docstring command required-params optional-params output-params vector-params vk-spec))
  (format out "                          (~(~{~a~}~))~%" (format-required-args required-params vector-params vk-spec))
  (format out "                          (~(~{~a~}~))" (format-optional-args optional-params vector-params vk-spec))
  (format out "~%                          ~:[nil~;t~]"
          (string= "void" (return-type command)))
  (when (extension-command-p command)
    (format out "~%                          t"))
  (format out ")~%")
  (format out "~{  ~(~a~)~})~%~%" (format-vk-args (params command) count-to-vector-param-indices output-params optional-params vector-params vk-spec)))

(defun write-create-handles-fun (out command fixed-function-name required-params optional-params output-params count-to-vector-param-indices vector-params vk-spec)
  (format out "(defvk-create-handles-fun (~(~a~)~%" fixed-function-name)
  (format out "                           ~(%vk:~a~)~%" fixed-function-name)
  (format out "                           ~s~%" (make-command-docstring command required-params optional-params output-params vector-params vk-spec))
  (format out "                           (~(~{~a~}~))~%" (format-required-args required-params vector-params vk-spec))
  (format out "                           (~(~{~a~}~))~%" (format-optional-args optional-params vector-params vk-spec))
  (format out "                           ~(~a~)" (if (= (length vector-params) 2)
                                                      ;; the length of the output array is the same size as an input array
                                                      (format nil "(length ~(~a~))"
                                                              (let ((array-arg (find-if-not (lambda (arg)
                                                                                              (find arg output-params))
                                                                                            vector-params)))
                                                                (fix-slot-name (name array-arg) (type-name (type-info array-arg)) vk-spec)))
                                                      ;; the length of the output array depends on the slot of an input parameter
                                                      ;; now we need to find out which slot this is and - more importantly - if it is one
                                                      ;; of the omitted slots because it redundantly describes the length of yet another
                                                      ;; slot of the parameter
                                                      (let* ((split-len-data (split-len-by-struct-member (len (first output-params))))
                                                             (count-arg (find-if (lambda (p)
                                                                                   (string= (first split-len-data) (name p)))
                                                                                 (params command)))
                                                             (count-struct (gethash (type-name (type-info count-arg)) (structures vk-spec)))
                                                             (struct-count-member-names (get-count-member-names count-struct))
                                                             (count-slot-name (second split-len-data)))
                                                        (if (member count-slot-name struct-count-member-names :test #'string=)
                                                            (let ((count-slot (find-if (lambda (m)
                                                                                         (string= count-slot-name (car (len m))))
                                                                                       (members count-struct))))
                                                              (format nil "(length (vk:~(~a ~a~)))"
                                                                (fix-slot-name (name count-slot) (type-name (type-info count-slot)) vk-spec t)
                                                                (fix-slot-name (name count-arg) (type-name (type-info count-arg)) vk-spec)))
                                                            (let ((count-slot (find-if (lambda (m)
                                                                                         (string= count-slot-name (name m)))
                                                                                       (members count-struct))))
                                                              (format nil "(vk:~(~a ~a~))"
                                                                (fix-slot-name (name count-slot) (type-name (type-info count-slot)) vk-spec t)
                                                                (fix-slot-name (name count-arg) (type-name (type-info count-arg)) vk-spec)))))))
  (when (extension-command-p command)
    (format out "~%                           t"))
  (format out ")~%")
  (format out "~{  ~(~a~)~})~%~%" (format-vk-args (params command) count-to-vector-param-indices output-params optional-params vector-params vk-spec)))

(defun write-get-struct-fun (out command fixed-function-name required-params optional-params output-params count-to-vector-param-indices vector-params vk-spec)
  (format out "(defvk-get-struct-fun (~(~a~)~%" fixed-function-name)
  (format out "                       ~(%vk:~a~)~%" fixed-function-name)
  (format out "                       ~s~%" (make-command-docstring command required-params optional-params output-params vector-params vk-spec))
  (format out "                       (~(~{~a~}~))~%" (format-required-args required-params vector-params vk-spec))
  (format out "                       (~(~{~a~}~))" (format-optional-args optional-params vector-params vk-spec))
  (when (extension-command-p command)
    (format out "~%                       t"))
  (format out ")~%")
  (format out "~{  ~(~a~)~})~%~%" (format-vk-args (params command) count-to-vector-param-indices output-params optional-params vector-params vk-spec)))

(defun write-fill-arbitrary-buffer-fun (out command fixed-function-name required-params optional-params output-params count-to-vector-param-indices vector-params vk-spec)
  (format out "(defvk-fill-arbitrary-buffer-fun (~(~a~)~%" fixed-function-name)
  (format out "                                  ~(%vk:~a~)~%" fixed-function-name)
  (format out "                                  ~s~%" (make-command-docstring command required-params optional-params output-params vector-params vk-spec))
  (format out "                                  (~(~{~a~}~))~%" (format-required-args required-params vector-params vk-spec))
  (format out "                                  (~(~{~a~}~))" (format-optional-args optional-params vector-params vk-spec))
  (when (extension-command-p command)
    (format out "~%                                  t"))
  (format out ")~%")
  (format out "~{  ~(~a~)~})~%~%" (format-vk-args (params command) count-to-vector-param-indices output-params optional-params vector-params vk-spec)))

(defun write-get-structs-fun (out command fixed-function-name required-params optional-params output-params count-to-vector-param-indices vector-params vk-spec)
  (format out "(defvk-get-structs-fun (~(~a~)~%" fixed-function-name)
  (format out "                        ~(%vk:~a~)~%" fixed-function-name)
  (format out "                        ~s~%" (make-command-docstring command required-params optional-params output-params vector-params vk-spec))
  (format out "                        (~(~{~a~}~))~%" (format-required-args required-params vector-params vk-spec))
  (format out "                        (~(~{~a~}~))~%" (format-optional-args optional-params vector-params vk-spec))
  (format out "                        ~(~a~)~%" (let ((count-arg (find-if-not #'len output-params)))
                                                   (fix-slot-name (name count-arg) (type-name (type-info count-arg)) vk-spec)))
  (format out "                        ~(~a~)" (let ((array-arg (find-if #'len output-params)))
                                                 (fix-slot-name (name array-arg) (type-name (type-info array-arg)) vk-spec)))
  (format out "~%                      ~:[nil~;t~]"
          (string= "void" (return-type command)))
  (when (extension-command-p command)
    (format out "~%                        t"))
  (format out ")~%")
  (format out "~{  ~(~a~)~})~%~%" (format-vk-args (params command) count-to-vector-param-indices output-params optional-params vector-params vk-spec)))

(defun write-multiple-singular-returns-fun (out command fixed-function-name required-params optional-params output-params count-to-vector-param-indices vector-params vk-spec)
  (format out "(defvk-multiple-singular-returns-fun (~(~a~)~%" fixed-function-name)
  (format out "                                      ~(%vk:~a~)~%" fixed-function-name)
  (format out "                                      ~s~%" (make-command-docstring command required-params optional-params output-params vector-params vk-spec))
  (format out "                                      (~(~{~a~}~))~%" (format-required-args required-params vector-params vk-spec))
  (format out "                                      (~(~{~a~}~))" (format-optional-args optional-params vector-params vk-spec))
  (when (extension-command-p command)
    (format out "~%                                      t"))
  (format out ")~%")
  (format out "~{  ~(~a~)~})~%~%" (format-vk-args (params command) count-to-vector-param-indices output-params optional-params vector-params vk-spec)))

(defun write-enumerate-fun (out command fixed-function-name required-params optional-params output-params count-to-vector-param-indices vector-params vk-spec)
  (format out "(defvk-enumerate-fun (~(~a~)~%" fixed-function-name)
  (format out "                      ~(%vk:~a~)~%" fixed-function-name)
  (format out "                      ~s~%" (make-command-docstring command required-params optional-params output-params vector-params vk-spec))
  (format out "                      (~(~{~a~}~))~%" (format-required-args required-params vector-params vk-spec))
  (format out "                      (~(~{~a~}~))~%" (format-optional-args optional-params vector-params vk-spec))
  (format out "                      ~(~a~)~%" (let ((count-arg (find-if-not #'len output-params)))
                                                 (fix-slot-name (name count-arg) (type-name (type-info count-arg)) vk-spec)))
  (format out "                      ~(~a~)" (let ((array-arg (find-if #'len output-params)))
                                               (fix-slot-name (name array-arg) (type-name (type-info array-arg)) vk-spec)))
  (format out "~%                      ~:[nil~;t~]"
          (string= "void" (return-type command)))
  (when (extension-command-p command)
    (format out "~%                      t"))
  (format out ")~%")
  (format out "~{  ~(~a~)~})~%~%" (format-vk-args (params command) count-to-vector-param-indices output-params optional-params vector-params vk-spec)))

(defun write-get-array-and-singular-fun (out command fixed-function-name required-params optional-params output-params count-to-vector-param-indices vector-params vk-spec)
  (format out "(defvk-get-array-and-singular-fun (~(~a~)~%" fixed-function-name)
  (format out "                                   ~(%vk:~a~)~%" fixed-function-name)
  (format out "                                   ~s~%" (make-command-docstring command required-params optional-params output-params vector-params vk-spec))
  (format out "                                   (~(~{~a~}~))~%" (format-required-args required-params vector-params vk-spec))
  (format out "                                   (~(~{~a~}~))~%" (format-optional-args optional-params vector-params vk-spec))
  (format out "                                   ~((length ~a)~)~%" (let ((array-arg (find-if-not (lambda (arg) ;; len-provider is the length of the input array
                                                                                                     (find arg output-params))
                                                                                                   vector-params)))
                                                                       (fix-slot-name (name array-arg) (type-name (type-info array-arg)) vk-spec)))
  (format out "                                   ~(~a~)" (let ((array-arg (find-if (lambda (arg) ;; array-arg is the output array (of the same size as input array)
                                                                                         (find arg output-params))
                                                                                       vector-params)))
                                                               (fix-slot-name (name array-arg) (type-name (type-info array-arg)) vk-spec)))
  (when (extension-command-p command)
    (format out "~%                                   t"))
  (format out ")~%")
  (format out "~{  ~(~a~)~})~%~%" (format-vk-args (params command) count-to-vector-param-indices output-params optional-params vector-params vk-spec)))

(defun write-enumerate-two-arrays-fun (out command fixed-function-name required-params optional-params output-params count-to-vector-param-indices vector-params vk-spec)
  (format out "(defvk-enumerate-two-arrays-fun (~(~a~)~%" fixed-function-name)
  (format out "                                 ~(%vk:~a~)~%" fixed-function-name)
  (format out "                                 ~s~%" (make-command-docstring command required-params optional-params output-params vector-params vk-spec))
  (format out "                                 (~(~{~a~}~))~%" (format-required-args required-params vector-params vk-spec))
  (format out "                                 (~(~{~a~}~))~%" (format-optional-args optional-params vector-params vk-spec))
  (format out "                                 ~(~a~)~%" (let ((count-arg (find-if-not #'len output-params)))
                                                            (fix-slot-name (name count-arg) (type-name (type-info count-arg)) vk-spec)))
  (format out "                                 ~((~a)~)" (let* ((array-args (remove-if-not #'len output-params))
                                                                 (first-arg (first array-args))
                                                                 (second-arg (second array-args)))
                                                            (format nil "~(~a ~a~)"
                                                                    (fix-slot-name (name first-arg) (type-name (type-info first-arg)) vk-spec)
                                                                    (fix-slot-name (name second-arg) (type-name (type-info second-arg)) vk-spec))))
  (format out "~%                                 ~:[nil~;t~]"
          (string= "void" (return-type command)))
  (when (extension-command-p command)
    (format out "~%                                 t"))
  (format out ")~%")
  (format out "~{  ~(~a~)~})~%~%" (format-vk-args (params command) count-to-vector-param-indices output-params optional-params vector-params vk-spec)))

;; todo: check if vkGetShaderInfoAMD is written correctly (what is the info parameter exactly? - check spec)
(defun write-command (out command vk-spec)
  (let* ((fixed-function-name (fix-function-name (name command) (tags vk-spec)))
         ;; maps from array-param to array-count-param
         (vector-param-indices (determine-vector-param-indices (params command) vk-spec))
         (count-to-vector-param-indices (reverse-hash-table vector-param-indices))
         (vector-params (loop for i in (alexandria:hash-table-keys vector-param-indices)
                              collect (nth i (params command))))
         (vector-count-params (loop for i in (alexandria:hash-table-values vector-param-indices)
                                    collect (nth i (params command))))
         (non-const-pointer-param-indices (determine-non-const-pointer-param-indices (params command)))
         (output-params (loop for i in non-const-pointer-param-indices
                              collect (nth i (params command))))
         (has-return-p (not (string= (return-type command) "void")))
         (handle-params (remove-if-not (lambda (p)
                                         (and (not (find p output-params))
                                              (gethash (type-name (type-info p)) (handles vk-spec))))
                                       (params command)))
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
         (required-handle-params (remove-if (lambda (p)
                                              (optional-p p))
                                            handle-params))
         (required-non-struct-params (remove-if (lambda (p)
                                                  (optional-p p))
                                                non-struct-params))
         (required-struct-params (remove-if (lambda (p)
                                              (optional-p p))
                                            struct-params))
         (optional-handle-params (remove-if-not (lambda (p)
                                                  (optional-p p))
                                                handle-params))
         (optional-non-struct-params (remove-if-not (lambda (p)
                                                      (optional-p p))
                                                    non-struct-params))
         (optional-struct-params (remove-if-not (lambda (p)
                                                  (optional-p p))
                                                struct-params))
         (required-params (sorted-elements (concatenate 'list required-handle-params required-non-struct-params required-struct-params)))
         (optional-params (sorted-elements (concatenate 'list optional-handle-params optional-non-struct-params optional-struct-params)))
         ;; collect redundant input parameters (i.e. if they specify the length of another input (!) parameter)
         (skipped-input-params (remove-if-not (lambda (p)
                                                (and (find p vector-count-params)
                                                     (not (intersection (gethash (position p (params command)) count-to-vector-param-indices)
                                                                        non-const-pointer-param-indices))))
                                              (concatenate 'list required-params optional-params))))

    (when nil
      (format t "~%handle: ~{~a, ~}~%"
             (loop for p in handle-params 
                   collect (name p)))
     (format t "~%non-struct: ~{~a, ~}~%"
             (loop for p in non-struct-params
                   collect (name p)))
     (format t "~%struct: ~{~a, ~}~%"
             (loop for p in struct-params
                   collect (name p)))
     (format t "~%output: ~{~a, ~}~%"
             (loop for p in output-params
                   collect (name p))))

    ;; todo: port conditions from vulkanhppgenerator to check if really all commands are written correctly. (e.g. there is one case without a single function - probably a bug)
    (cond
      ((not output-params)
       ;; case 0a: no or implicit return value - e.g. vkDestroyInstance
       ;; case 0b: non-trivial return value - e.g. vkGetInstanceProcAddr
       (write-simple-fun out
                         command
                         fixed-function-name
                         required-params
                         optional-params
                         output-params
                         count-to-vector-param-indices
                         vector-params
                         vk-spec))
      ((= (length non-const-pointer-param-indices) 1)
       (let ((ret (first output-params)))
         (if (or (gethash (type-name (type-info ret)) (handles vk-spec))
                 ;; added the next two conditions, since function return e.g. a uint64_t* were
                 ;; falsely classified as get-struct-funs
                 (gethash (type-name (type-info ret)) *vk-platform*)
                 (gethash (type-name (type-info ret)) (base-types vk-spec)))
             ;; 1) handle type
             (if (not (find ret vector-params))
                 ;; case 1a-1: create a handle - e.g. vkCreateInstance
                 ;; case 1a-2: get an existing handle - e.g. vkGetDeviceQueue
                 (write-create-handle-fun out
                                          command
                                          fixed-function-name
                                          required-params
                                          optional-params
                                          output-params
                                          count-to-vector-param-indices
                                          vector-params
                                          vk-spec)
                 ;; 1b-1) vector of handles, where the output vector uses the same len as an input vector - e.g. vkCreateGraphicsPipelines
                 ;; 1b-2) vector of handles using len-by-struct-member - e.g. vkAllocateCommandBuffers
                 (write-create-handles-fun out
                                           command
                                           fixed-function-name
                                           required-params
                                           optional-params
                                           output-params
                                           count-to-vector-param-indices
                                           vector-params
                                           vk-spec))
             ;; 2) structure chain anchor
             (if (or (structure-chain-anchor-p (type-name (type-info (nth (first non-const-pointer-param-indices) (params command))))
                                               vk-spec)
                     (not (gethash (first non-const-pointer-param-indices) vector-param-indices)))
                 ;; case 1c-1: get a struct extended by its NEXT slot - e.g. vkGetPhysicalDeviceFeatures2
                 ;; case 1c-2: get a struct without NEXT slot - e.g. vkGetPhysicalDeviceProperties
                 (write-get-struct-fun out
                                       command
                                       fixed-function-name
                                       required-params
                                       optional-params
                                       output-params
                                       count-to-vector-param-indices
                                       vector-params
                                       vk-spec)
                 ;; TODO: with the current implementation such functions are actually simple-funs which don't return anything -> breaks consistency!
                 ;; case 1d: arbitrary data as output param - e.g. vkGetQueryPoolResults
                 (write-fill-arbitrary-buffer-fun out
                                                  command
                                                  fixed-function-name
                                                  required-params
                                                  optional-params
                                                  output-params
                                                  count-to-vector-param-indices
                                                  vector-params
                                                  vk-spec)))))
      ((= (length non-const-pointer-param-indices) 2)
       (if (or (structure-chain-anchor-p (type-name (type-info (nth (second non-const-pointer-param-indices) (params command))))
                                         vk-spec)
               (and (= (hash-table-count vector-param-indices) 1)
                    (string= "void" (return-type command))))
           ;; case 2a: get list of structs - e.g. vkGetPhysicalDeviceQueueFamilyProperties2
           (write-get-structs-fun out
                                  command
                                  fixed-function-name
                                  required-params
                                  optional-params
                                  output-params
                                  count-to-vector-param-indices
                                  vector-params
                                  vk-spec)
           (cond
             ;; todo: find out which function falls (or should fall) into that category
             ;; case 2b: two returns and a non-trivial success code, no array - e.g. ???
             ((= (hash-table-count vector-param-indices) 0)
              (write-multiple-singular-returns-fun out
                                                   command
                                                   fixed-function-name
                                                   required-params
                                                   optional-params
                                                   output-params
                                                   count-to-vector-param-indices
                                                   vector-params
                                                   vk-spec))
             ;; case 2c: enumerate - e.g. vkEnumeratePhysicalDevices
             ((= (hash-table-count vector-param-indices) 1)
              (write-enumerate-fun out
                                   command
                                   fixed-function-name
                                   required-params
                                   optional-params
                                   output-params
                                   count-to-vector-param-indices
                                   vector-params
                                   vk-spec))
             ;; case 2d: return multiple values. one array of the same size as an input array and one additional non-array value - e.g. vkGetCalibratedTimestampsEXT
             ((= (hash-table-count vector-param-indices) 2)
              (write-get-array-and-singular-fun out
                                                command
                                                fixed-function-name
                                                required-params
                                                optional-params
                                                output-params
                                                count-to-vector-param-indices
                                                vector-params
                                                vk-spec)))))
      ((= (length non-const-pointer-param-indices) 3)
       ;; case 3: return two arrays using the same counter which is also an output argument - e.g vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR
       (write-enumerate-two-arrays-fun out
                                       command
                                       fixed-function-name
                                       required-params
                                       optional-params
                                       output-params
                                       count-to-vector-param-indices
                                       vector-params
                                       vk-spec))
      (t (warn "Never encountered a function like <~a>!" (name command))))))

(defun write-vk-functions (vk-functions-file vk-spec)
  (with-open-file (out vk-functions-file :direction :output :if-exists :supersede)
    (format out ";;; this file is automatically generated, do not edit~%")
    (format out "#||~%~a~%||#~%~%" (vulkan-license-header vk-spec))
    (format out "(in-package :vk)~%~%")

    
    (loop for command in (sorted-elements (alexandria:hash-table-values (commands vk-spec)))
          do (write-command out command vk-spec)
          when (alias command)
          do (loop for alias in (alexandria:hash-table-values (alias command))
                   do (write-command out (make-aliased-command command alias) vk-spec)))))


(defun foo () (ql:quickload :vk-generator) (generate :version "v1.2.153"))
