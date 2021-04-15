(in-package :vk-generator)

(defun fix-slot-name (name type-name vk-spec &optional (as-accessor-p nil))
  "Fixes the NAME of a slot for a class representation of a struct in the Vulkan API.
Removes the \"P-\"-prefix for pointer types.
Removes the \"PP-\"-prefix for pointers to string arrays.
Changes the \"PP-\"-prefix to \"P-\" for pointers to pointer arrays (e.g. ppGeometries in VkAccelerationStructureBuildGeometryInfoKHR)
"
  (let ((fixed-type-name (string (fix-type-name name (tags vk-spec)))))
    (cond
      ;; there is already a function named WAIT-SEMAPHORES
      ((and as-accessor-p
            (string= "P-WAIT-SEMAPHORES" fixed-type-name))
       fixed-type-name)
      ((alexandria:starts-with-subseq "P-" fixed-type-name)
       (subseq fixed-type-name 2))
      ((and (alexandria:starts-with-subseq "PP-" fixed-type-name)
            (string= type-name "char"))
       (subseq fixed-type-name 3))
      ((alexandria:starts-with-subseq "PP-" fixed-type-name)
       (subseq fixed-type-name 1))
      (t fixed-type-name))))

;; todo: if the member specifies the size of a
(defun get-count-member-names (struct)
  "Returns a list of all names of members in STRUCT which specify the number of elements within an array member of the same STRUCT."
  (loop for m in (members struct)
        when (or (string= (name m) "codeSize") ;; codeSize is a special case where the len is actually codeSize/4
                 (find-if (lambda (other)
                            (and (string= (car (len other)) (name m))
                                 ;; the following are count members which have to be explicitly set
                                 ;; because the array member can be null, but the count can not
                                 ;; see #20
                                 (not (and (string= "VkDescriptorSetLayoutBinding" (name struct))
                                           (string= "descriptorCount" (name m))))
                                 (not (and (string= "VkPresentRegionsKHR" (name struct))
                                           (string= "swapchainCount" (name m))))
                                 (not (and (string= "VkPresentTimesInfoGOOGLE" (name struct))
                                           (string= "swapchainCount" (name m))))
                                 (not (string= (type-name (type-info other)) "void")))) ;; we can't do anything for void pointer arrays, must be supplied by the user
                          (members struct)))
        collect (name m)))

(defun fix-slot-type-for-documentation (member-data vk-spec)
  (let ((slot-name (name member-data))
        (type-name (type-name (type-info member-data))))
    (cond
      ((string= "pNext" slot-name)
       "instance of a class extending this class")
      ((and (not (value-p (type-info member-data)))
            (len member-data))
       (format nil "foreign pointer to a buffer of size ~a" (string (fix-type-name (car (len member-data)) (tags vk-spec)))))
      ((string= "void" type-name)
       "foreign pointer")
      ((string= "char" type-name)
       "string")
      ((string= "float" type-name)
       "single-float")
      ((string= "double" type-name)
       "double-float")
      ((alexandria:starts-with-subseq "uint" type-name)
       (let ((bits (subseq (if (alexandria:ends-with-subseq "_t" type-name)
                               (subseq type-name 0 (- (length type-name) 2))
                               type-name)
                           4)))
         (format nil "positive (~a-bit) integer" bits)))
      ((string= "size_t" type-name)
       "positive integer")
      ((alexandria:starts-with-subseq "int" type-name)
       (format nil "~@[(~a-bit) ~]integer" (when (> (length type-name) 3)
                                             (subseq (if (alexandria:ends-with-subseq "_t" type-name)
                                                         (subseq type-name 0 (- (length type-name) 2))
                                                         type-name)
                                                     3))))
      ((gethash type-name (bitmasks vk-spec))
       (format nil "list containing a valid combination of ~a"
               (string (fix-type-name type-name (tags vk-spec)))))
      ((gethash type-name (enums vk-spec))
       (format nil "enum value of ~a" (string (fix-type-name type-name (tags vk-spec)))))
      ((string= "VkBool32" type-name)
       "boolean")
      (t (string (fix-type-name type-name (tags vk-spec)))))))

(defun make-documentation (struct count-member-names vk-spec)
  (let* ((referenced-types (remove-duplicates
                            (loop for m in (members struct)
                                  for type-name = (type-name (type-info m))
                                  unless (or (gethash type-name *vk-platform*)
                                             (= (length (allowed-values m)) 1)
                                             (string= "VkBool32" (type-name (type-info m))))
                                  collect (fix-type-name type-name (tags vk-spec)))))
         (slots (loop for m in (members struct)
                      for fixed-type-name = (fix-slot-type-for-documentation
                                             m
                                             vk-spec)
                      for list-p = (and (len m)
                                        (find (car (len m)) count-member-names :test #'string=))
                      for multi-dim-array-p = (> (length (array-sizes m)) 1)
                      unless (or (find (name m) count-member-names :test #'string=)
                                 (= (length (allowed-values m)) 1))
                      collect (format nil "~% - ~a~:[~; (optional)~]: ~a ~:[~;list of ~]~:[~;multidimensional array of ~]~a~:[~;s~]~@[ of size ~a~]."
                                  (fix-slot-name (name m) (type-name (type-info m)) vk-spec)
                                  (or (optional-p m)
                                      (string= (name m) "pNext"))
                                  (if (and (not list-p)
                                           (find (subseq (string-downcase fixed-type-name) 0 1) '("a" "e" "i" "o" "u") :test #'string=))
                                      "an" "a")
                                  list-p
                                  multi-dim-array-p
                                  fixed-type-name ;; pNext and other void types should be handled here as well, char->string, uint32->positive int, float ->float, flags->combination of flagbits...
                                  (or list-p multi-dim-array-p)
                                  (if multi-dim-array-p
                                      (format nil "~ax~a" (first (array-sizes m)) (second (array-sizes m)))
                                      nil))))
         (struct-extends (loop for s in (struct-extends struct)
                               collect (fix-type-name s (tags vk-spec))))
         (extended-by (loop for s in (get-extends struct vk-spec)
                            collect (fix-type-name (name s) (tags vk-spec))))
         (used-in-functions (sort
                             (loop for c in (alexandria:hash-table-values (commands vk-spec))
                                   when (member-if
                                         (lambda (p)
                                           (string= (name struct)
                                                    (type-name (type-info p))))
                                         (params c))
                                   collect (fix-function-name (name c) (tags vk-spec)))
                             #'string<)))
    (format nil "Represents the ~:[struct~;union~] [~a](https://www.khronos.org/registry/vulkan/specs/1.2-extensions/man/html/~a.html).

Slots:~{~a~}~@[

Slot types:~{~%See ~a~}~]~@[

Instances of this class can be extended by the following classes (using the NEXT slot):~{~%See ~a~}~]~@[

Instances of this class can be used to extend the following classes (using their NEXT slot):~{~%See ~a~}~]~@[

Instances of this class are used as parameters of the following functions:~{~%See ~a~}~]
"
            (is-union-p struct)
            (name struct)
            (name struct)
            slots
            referenced-types
            extended-by
            struct-extends
            used-in-functions)))

;; todo: check out how they do it in void VulkanHppGenerator::appendStructConstructors or VulkanHppGenerator::appendStructConstructorsEnhanced
(defun write-classes (out vk-spec)
  (loop for struct in (sorted-elements (alexandria:hash-table-values (structures vk-spec)))
        for count-member-names = (get-count-member-names struct)
        for wrote-first-member = nil
        do
        (format out "~%(defclass ~(~a~) ()" (fix-type-name (name struct) (tags vk-spec)))
        (loop for m in (members struct)
              for fixed-slot-name = (fix-slot-name (name m) (type-name (type-info m)) vk-spec)
              for array-member-p = (and (len m)
                                        (member (first (len m)) count-member-names :test #'string=))
              unless (or (member (name m) count-member-names :test #'string=) ;; we'll determine the count during translation
                         (= (length (allowed-values m)) 1)) ;; slots which must be the same for all instances only have to be set during translation
              do
              (format out "~%  ~:[ ~;(~](~(~a~%     :initarg :~a~%~@[~a~]     :accessor ~a~))"
                      (not wrote-first-member)
                      fixed-slot-name
                      fixed-slot-name
                      (unless (is-union-p struct)
                        (format nil "     :initform ~(~s~)~%"
                                (cond
                                  (array-member-p
                                   nil)
                                  ((string= "char" (type-name (type-info m)))
                                    "")
                                  ((and (gethash (type-name (type-info m)) *vk-platform*)
                                        (or (search "int" (type-name (type-info m)))
                                            (string= "size_t" (type-name (type-info m)))))
                                   0)
                                  ((and (gethash (type-name (type-info m)) *vk-platform*)
                                        (or (string= "float" (type-name (type-info m)))
                                            (string= "double" (type-name (type-info m)))))
                                   0.0)
                                  (t nil))))
                      (fix-slot-name (name m) (type-name (type-info m)) vk-spec t))
              (setf wrote-first-member t))
        (format out ")~%  (:documentation ~s))~%" (make-documentation struct count-member-names vk-spec)))
  ;; todo: pretty-printing of classes
  )

(defun get-extends (struct vk-spec)
  (remove-if-not (lambda (s)
                   (find (name struct) (struct-extends s) :test #'string=))
                 (alexandria:hash-table-values (structures vk-spec))))

(defun get-value-setter (member-data struct count-member-names macro-p vk-spec &optional (slot-provider "value"))
  (let ((fixed-slot-name (fix-slot-name (name member-data) (type-name (type-info member-data)) vk-spec))
        (fixed-accessor-name (fix-slot-name (name member-data) (type-name (type-info member-data)) vk-spec t))
        (value-str (format nil "~:[~;,~]~a" macro-p slot-provider))
        (ptr-str (format nil "~:[~;,~]ptr" macro-p)))
    (cond
      ;; type of "pNext" must be determined at runtime
      ((string= "pNext" (name member-data))
       ;; first translate to VkBaseOutStructure to get the structure type and then translate to actual class
       (if (or (get-extends struct vk-spec)
               (struct-extends struct))
           (format nil "~((if (vk:next ~a) (vk-alloc:foreign-allocate-and-fill (list :struct (find-symbol (string (class-name (class-of (vk:next ~a)))) :%vk)) (vk:next ~a) ~a) (cffi:null-pointer))~)"
                      value-str
                      value-str
                      value-str
                      ptr-str)
           "(cffi:null-pointer)"))
      
      ;; void pointer - must be handled by user
      ((string= "void" (type-name (type-info member-data)))
       (format nil "(if (vk:~(~a~) ~a) (vk:~(~a~) ~a) (cffi:null-pointer))"
               fixed-accessor-name
               value-str
               fixed-accessor-name
               value-str))
      
      ;; members with constant values (such as "sType")
      ((= (length (allowed-values member-data)) 1)
       (format nil ":~(~a~)"
               (fix-bit-name (first (allowed-values member-data))
                             (tags vk-spec)
                             :prefix (find-enum-prefix
                                      (string (fix-type-name (type-name (type-info member-data)) (tags vk-spec)))
                                      (enum-values (gethash (type-name (type-info member-data)) (enums vk-spec)))
                                      (tags vk-spec)))))
      
      ;; members of some VK-defined type (lists or single instances)
      ((gethash (type-name (type-info member-data)) (structures vk-spec))
       (format nil "(vk-alloc:foreign-allocate-and-fill '(~:[:struct~;:union~] %vk:~(~a~)) (~(vk:~a~) ~a) ~a)"
               (is-union-p (gethash (type-name (type-info member-data)) (structures vk-spec)))
               (fix-type-name (type-name (type-info member-data)) (tags vk-spec))
               fixed-accessor-name
               value-str
               ptr-str))

      ;; lists of strings and primitive values
      ((and (len member-data)
            (or (search "codeSize" (car (len member-data))) ;; VkShaderModuleCreateInfo
                (find-if (lambda (count-member)
                           (string= (car (len member-data)) count-member))
                         count-member-names))
            (gethash (type-name (type-info member-data)) *vk-platform*))
       (let ((foreign-type-name (if (string= (type-name (type-info member-data)) "char")
                                    :string
                                    (gethash (type-name (type-info member-data)) *vk-platform*))))
         (format nil "(vk-alloc:foreign-allocate-and-fill ~(~s~) (~(vk:~a~) ~a) ~a)"
                 foreign-type-name
                 fixed-accessor-name
                 value-str
                 ptr-str)))

      ;; lists of handles
      ((and (len member-data)
            (find-if (lambda (count-member)
                       (string= (car (len member-data)) count-member))
                     count-member-names))
       (format nil "(vk-alloc:foreign-allocate-and-fill '%vk:~(~a~) (~(vk:~a~) ~a) ~a)"
               (fix-type-name (type-name (type-info member-data)) (tags vk-spec))
               fixed-accessor-name
               value-str
               ptr-str))

      ;; single handles - take as is or null pointer
      ((gethash (type-name (type-info member-data)) (handles vk-spec))
       (format nil "(if (vk:~(~a~) ~a) (vk:~(~a~) ~a) (cffi:null-pointer))"
               fixed-accessor-name
               value-str
               fixed-accessor-name
               value-str))

      ;; base type pointers like VkPipelineMultisampleStateCreateInfo.pSampleMask
      ((and (gethash (type-name (type-info member-data)) (base-types vk-spec))
            (not (value-p (type-info member-data))))
       (format nil "(if (vk:~(~a~) ~a) (vk:~(~a~) ~a) (cffi:null-pointer))"
               fixed-accessor-name
               value-str
               fixed-accessor-name
               value-str))

      ;; set size-members based on list lengths
      ((find (name member-data) count-member-names :test #'string=)
       (let* ((slots (remove-if-not (lambda (s)
                                      (or (string= (car (len s)) (name member-data))
                                          (search (name member-data) (car (len s)))))
                                    (members struct)))
              (size-getter (cond
                             ((> (length slots) 1)
                              (format nil "(reduce #'max (list ~{~a~}))"
                                      (mapcar (lambda (s)
                                                (format nil "~((length (vk:~a ~a))~)"
                                                        (fix-slot-name (name s) (type-name (type-info s)) vk-spec t)
                                                        value-str))
                                              slots)))
                             ((= (length slots) 1)
                              (format nil "~((length (vk:~a ~a))~)"
                                      (fix-slot-name (name (first slots)) (type-name (type-info (first slots))) vk-spec t)
                                      value-str))
                             (t (error "slot has no size-getter! struct: ~a slot:~a slots:~a" struct member-data slots)))))
         (cond
           ((string= "codeSize" (name member-data))
            (format nil "(* 4 ~a)" size-getter))
           (t size-getter))))

      ;; basic slots (strings & primitive types)
      (t (format nil "(~(vk:~a~) ~a)" fixed-slot-name value-str)))))

(defun get-slot-setter (member-data struct count-member-names macro-p vk-spec)
  (let ((fixed-slot-name (fix-slot-name (name member-data) (type-name (type-info member-data)) vk-spec))
        (fixed-accessor-name (fix-slot-name (name member-data) (type-name (type-info member-data)) vk-spec t)))
    (cond
      ;; union slots: determine at runtime which slot is bound and then translate this slot
      ((and (gethash (type-name (type-info member-data)) (structures vk-spec))
            (is-union-p  (gethash (type-name (type-info member-data)) (structures vk-spec))))
       ;; todo: handle union slots
       nil)
      
      ;; "pNext" of a "VkBaseOutStructure" can't be translated - would lead to infinite recursion
      ((and (string= "pNext" (name member-data))
            (string= "VkBaseOutStructure" (name struct)))
       (format nil "~(%vk:p-next~)"))
      
      ;; type of "pNext" must be determined at runtime
      ((string= "pNext" (name member-data))
       ;; first translate to VkBaseOutStructure to get the structure type and then translate to actual class
       (format nil "~((when (not (cffi:null-pointer-p %vk:p-next)) (let ((base-out (cffi:mem-aref %vk:p-next '(:struct %vk:base-out-structure)))) (cffi:mem-aref %vk:p-next (list :struct (find-symbol (string (vk:s-type base-out)) :%vk)))))~)"))
      
      ;; void pointer - keep as is, must be handled by user
      ((string= "void" (type-name (type-info member-data)))
       (format nil "~(%vk:~a~)"
               (fix-type-name (name member-data) (tags vk-spec))))
      
      ;; members of some VK-defined type (lists or single instances)
      ((and (gethash (type-name (type-info member-data)) (structures vk-spec))
            (find-if (lambda (count-member)
                       (string= (car (len member-data)) count-member))
                     count-member-names))
       (format nil "~((loop for i from 0 below %vk:~a collect (cffi:mem-aref %vk:~a '(:struct %vk:~a) i))~)"
               (let ((count-member (find-if (lambda (m)
                                              (string= (car (len member-data)) (name m)))
                                            (members struct))))
                 (fix-type-name (name count-member) (tags vk-spec)))
               (fix-type-name (name member-data) (tags vk-spec))
               (fix-type-name (type-name (type-info member-data)) (tags vk-spec))))

      ;; lists of strings and primitive values
      ((and (len member-data)
            (or (search "codeSize" (car (len member-data)))
                (find-if (lambda (count-member)
                           (string= (car (len member-data)) count-member))
                         count-member-names))
            (gethash (type-name (type-info member-data)) *vk-platform*))
       (format nil "~((loop for i from 0 below %vk:~a collect (cffi:mem-aref %vk:~a ~s i))~)"
               (let ((count-member (find-if (lambda (m)
                                              (or (string= (car (len member-data)) (name m))
                                                  (and (search "codeSize" (car (len member-data)))
                                                       (string= "codeSize" (name m)))))
                                            (members struct))))
                 (fix-type-name (name count-member) (tags vk-spec)))
               (fix-type-name (name member-data) (tags vk-spec))
               (if (string= (type-name (type-info member-data)) "char")
                   :string
                   (gethash (type-name (type-info member-data)) *vk-platform*))))

      ;; lists of handles
      ((and (len member-data)
            (find-if (lambda (count-member)
                       (string= (car (len member-data)) count-member))
                     count-member-names))
       (format nil "~((loop for i from 0 below %vk:~a collect (cffi:mem-aref %vk:~a '%vk:~a i))~)"
               (let ((count-member (find-if (lambda (m)
                                              (string= (car (len member-data)) (name m)))
                                            (members struct))))
                 (fix-type-name (name count-member) (tags vk-spec)))
               (fix-type-name (name member-data) (tags vk-spec))
               (fix-type-name (type-name (type-info member-data)) (tags vk-spec))))

      ;; strings that are defined as: ":char :count <len>" instead of ":string"
      ((and (string= "char" (type-name (type-info member-data)))
            (and (not (string= (postfix (type-info member-data)) "*"))
                 (not (string= (postfix (type-info member-data)) "**"))
                 (not (string= (postfix (type-info member-data)) "* const*"))))
       (format nil "(cffi:foreign-string-to-lisp ~(%vk:~a~))"
               (fix-type-name (name member-data) (tags vk-spec))))

      ;; todo: multidimensional arrays should be translated into arrays instead of lists
      ((array-sizes member-data)
       (let ((array-size (prepare-array-sizes (array-sizes member-data) vk-spec)))
         (format nil "(loop for i from 0 below ~a collect (cffi:mem-aref ~(~a ~a~) i))"
                 array-size
                 ;; todo: file a bug report over at CFFI: :count is not respected if the type of a struct member is also a struct 
                 (if (gethash (type-name (type-info member-data)) (structures vk-spec))
                     (format nil "~((cffi:foreign-slot-pointer ~:[~;,~]ptr '(:struct %vk:~a) '%vk:~a)~)"
                             macro-p
                             (fix-type-name (name struct) (tags vk-spec))
                             (fix-type-name (name member-data) (tags vk-spec)))
                     (format nil "~(%vk:~a~)"
                             (fix-type-name (name member-data) (tags vk-spec))))
                 (cond
                   ((gethash (type-name (type-info member-data)) *vk-platform*)
                    (format nil "~(~s~)"
                            (gethash (type-name (type-info member-data)) *vk-platform*)))
                   ((not (gethash (type-name (type-info member-data)) (structures vk-spec)))
                    (format nil "~('%vk:~a~)"
                            (fix-type-name (type-name (type-info member-data)) (tags vk-spec))))
                   (t (format nil "~('(:struct %vk:~a)~)"
                              (fix-type-name (type-name (type-info member-data)) (tags vk-spec))))))))

      ;; most types can be automatically translated (primitive types, bitfields, enums, structs)
      (t
       (format nil "~(%vk:~a~)"
               (fix-type-name (name member-data) (tags vk-spec)))))))

(defun write-translate-to (out struct expand-p vk-spec)
  (let ((fixed-type-name (fix-type-name (name struct) (tags vk-spec)))
        (count-member-names (get-count-member-names struct))
        (macro-name (if expand-p
                        "expand-into-foreign-memory"
                        "translate-into-foreign-memory")))
    (format out "(defmethod cffi:~a (value (type %vk:c-~(~a~)) ptr)~%  ~:[~;`~](cffi:with-foreign-slots"
            macro-name
            fixed-type-name
            expand-p)
    (loop for m in (members struct)
          for i from 1
          do (format out "~%      ~:[  ~;((~]~(%vk:~a~)~:[~;)~]"
                     (= 1 i)
                     (fix-type-name (name m) (tags vk-spec))
                     (= i (length (members struct)))))
    (format out "~%       ~:[~;,~]ptr~%       (:struct %vk:~(~a~)))"
            expand-p
            fixed-type-name)
    (loop for m in (members struct)
          for i from 1
          unless (and (array-sizes m)
                      (not (string= "char" (type-name (type-info m))))
                      (gethash (type-name (type-info m)) *vk-platform*))
          do (format out "~%~:[          ~;    (setf ~]~(%vk:~a~) ~a"
                     (= 1 i)
                     (fix-type-name (name m) (tags vk-spec))
                     (get-value-setter m struct count-member-names expand-p vk-spec)))
    (when (remove-if (lambda (m)
                       (and (array-sizes m)
                            (not (string= "char" (type-name (type-info m))))
                            (gethash (type-name (type-info m)) *vk-platform*)))
                     (members struct))
        (format out ")"))
    (loop for m in (members struct)
          for i from 1
          when (and (array-sizes m)
                    (not (string= "char" (type-name (type-info m))))
                    (gethash (type-name (type-info m)) *vk-platform*))
          do (format out "~%    (cffi:lisp-array-to-foreign ~((vk:~a ~a) %vk:~a '(:array ~s~{ ~a~})~))"
                     (fix-slot-name (name m) (type-name (type-info m)) vk-spec t)
                     (if expand-p ",value" "value")
                     (fix-type-name (name m) (tags vk-spec))
                     (gethash (type-name (type-info m)) *vk-platform*)
                     (array-sizes m)))
    (format out "))~%~%")))

(defun write-translate-union-to (out struct expand-p vk-spec)
  (let ((fixed-type-name (fix-type-name (name struct) (tags vk-spec)))
        (count-member-names (get-count-member-names struct))
        (macro-name (if expand-p
                        "expand-into-foreign-memory"
                        "translate-into-foreign-memory")))
    (format out "(defmethod cffi:~a ((value vk:~(~a~)) type ptr)~%  ~:[~;`~](cffi:with-foreign-slots"
            macro-name
            fixed-type-name
            expand-p)
    (loop for m in (members struct)
          for i from 1
          do (format out "~%      ~:[  ~;((~]~(%vk:~a~)~:[~;)~]"
                     (= 1 i)
                     (fix-type-name (name m) (tags vk-spec))
                     (= i (length (members struct)))))
    (format out "~%       ~:[~;,~]ptr~%       (:union %vk:~(~a~)))"
            expand-p
            fixed-type-name)
    (format out "~%    ~:[~; ~](cond"
            expand-p)
    (loop with value-str = (format nil "~:[~;,~]value" expand-p)
          with ptr-str = (format nil "~:[~;,~]ptr" expand-p)
          for m in (members struct)
          for primitive-p = (and (not (array-sizes m))
                                 (or (gethash (type-name (type-info m)) *vk-platform*)
                                     (member (type-name (type-info m)) '("VkBool32" "VkResult") :test #'string=)
                                     (gethash (type-name (type-info m)) (enums vk-spec))
                                     (gethash (type-name (type-info m)) (bitmasks vk-spec))
                                     (gethash (type-name (type-info m)) (base-types vk-spec))))
          for member-type-specifier = (cond
                                        ((string= "char" (type-name (type-info m)))
                                         "string")
                                        ((gethash (type-name (type-info m)) *vk-platform*)
                                         (format nil "~(~s~)" (gethash (type-name (type-info m)) *vk-platform*)))
                                        ((gethash (type-name (type-info m)) (structures vk-spec))
                                         (format nil "'(~:[:struct~;:union~] %vk:~(~a~))"
                                                 (is-union-p (gethash (type-name (type-info m)) (structures vk-spec)))
                                                 (fix-type-name (type-name (type-info m)) (tags vk-spec))))
                                        ((not primitive-p)
                                         (error "Non-primitive member type <~a> not handled for union <~a>"
                                                (type-name (type-info m))
                                                struct)))
          for i from 1 do
          (format out "~%      ((slot-boundp ~a 'vk:~(~a~))" value-str (fix-type-name (name m) (tags vk-spec)))
          (cond
            ((and (array-sizes m)
                  (gethash (type-name (type-info m)) *vk-platform*))
             (format out "~%       (cffi:lisp-array-to-foreign (vk:~(~a~) ~a) ~a '(:array ~(~s~) ~a)))"
                     (fix-type-name (name m) (tags vk-spec))
                     value-str
                     ptr-str
                     (gethash (type-name (type-info m)) *vk-platform*)
                     (first (array-sizes m))))
            (primitive-p
             (format out "~%       (setf %vk:~(~a~)" (fix-type-name (name m) (tags vk-spec)))
             (format out "~%             (vk:~(~a~) ~a)))" (fix-type-name (name m) (tags vk-spec)) value-str))
            (t
             (format out "~%       (setf %vk:~(~a~)" (fix-type-name (name m) (tags vk-spec)))
             (format out "~%             (vk-alloc:foreign-allocate-and-fill ~(~a~)" member-type-specifier)
             (format out "~%                                                  (vk:~(~a~) ~a)" (fix-type-name (name m) (tags vk-spec)) value-str)
             (format out "~%                                                  ~a)))" ptr-str))))
    (format out ")))~%~%")))

(defun write-translate-from (out struct expand-p vk-spec)
  (let ((fixed-type-name (fix-type-name (name struct) (tags vk-spec)))
        (count-member-names (get-count-member-names struct))
        (macro-name (if expand-p
                        "expand-from-foreign"
                        "translate-from-foreign")))
    (format out "(defmethod cffi:~a (ptr (type %vk:c-~(~a~)))~%  ~:[~;`~](cffi:with-foreign-slots"
            macro-name
            fixed-type-name
            expand-p)
    (loop for m in (members struct)
          for i from 1
          do (format out "~%      ~:[  ~;((~]~(%vk:~a~)~:[~;)~]"
                     (= 1 i)
                     (fix-type-name (name m) (tags vk-spec))
                     (= i (length (members struct)))))
    (format out "~%       ~:[~;,~]ptr~%       (:struct %vk:~(~a~)))"
            expand-p
            fixed-type-name)
    (format out "~%    (make-instance 'vk:~(~a~)" fixed-type-name)
    (loop for m in (members struct)
          unless (or (find (name m) count-member-names :test #'string=) ;; redundant count values
                     (= (length (allowed-values m)) 1)) ;; redundant constant values
          do (format out "~%                   ~(:~a ~a~)"
                     (fix-slot-name (name m) (type-name (type-info m)) vk-spec)
                     (get-slot-setter m struct count-member-names expand-p vk-spec)))
    (format out ")))~%~%")))

(defun write-vk-enums (out vk-spec)
  (loop for e in (sorted-elements (alexandria:hash-table-values (enums vk-spec)))
        for fixed-name = (string (fix-type-name (name e) (tags vk-spec)))
        for prefix = (when (enum-values e) (find-enum-prefix fixed-name (enum-values e) (tags vk-spec)))
        for fixed-value-names = (loop for v in (enum-values e)
                                      collect (fix-bit-name (name v) (tags vk-spec) :prefix prefix))
        do (format out "~%(deftype ~(~a~) ()
  \"Represents the enum [~a](https://www.khronos.org/registry/vulkan/specs/1.2-extensions/man/html/~a.html).~@[

Has the values:~{~% - :~a~}~]\"
  '(member nil ~{~%    :~(~a~)~}))~%"
                   fixed-name
                   (name e)
                   (name e)
                   fixed-value-names
                   fixed-value-names))
  (format out "~%~%"))

(defun write-vk-handles (out vk-spec)
  (loop for h in (sorted-elements (alexandria:hash-table-values (handles vk-spec)))
        unless (string= "" (name h))
        do (format out "~%(deftype ~(~a~) ()
  \"Represents the ~@[~a ~]handle [~a](https://www.khronos.org/registry/vulkan/specs/1.2-extensions/man/html/~a.html).\"
  'cffi:foreign-pointer)~%"
                   (fix-type-name (name h) (tags vk-spec))
                   (when (non-dispatch-handle-p h) "(non-dispatchable)")
                   (name h)
                   (name h)))
  (format out "~%~%"))

(defun write-vk-struct-translators-file (translate-to-file translate-from-file expand-to-file expand-from-file vk-spec)
  (with-open-file (out translate-to-file :direction :output :if-exists :supersede)
    (format out ";;; this file is automatically generated, do not edit~%")
    (format out "#||~%~a~%||#~%~%" (vulkan-license-header vk-spec))
    (format out "(in-package :vk)~%~%")

    (loop for struct in (sorted-elements (alexandria:hash-table-values (structures vk-spec)))
          if (is-union-p struct) do (write-translate-union-to out struct nil vk-spec)
          else do (write-translate-to out struct nil vk-spec)))
  
  (with-open-file (out expand-to-file :direction :output :if-exists :supersede)
    (format out ";;; this file is automatically generated, do not edit~%")
    (format out "#||~%~a~%||#~%~%" (vulkan-license-header vk-spec))
    (format out "(in-package :vk)~%~%")

    (loop for struct in (sorted-elements (alexandria:hash-table-values (structures vk-spec)))
          if (is-union-p struct) do (write-translate-union-to out struct t vk-spec)
          else do (write-translate-to out struct t vk-spec)))
  
  (with-open-file (out translate-from-file :direction :output :if-exists :supersede)
    (format out ";;; this file is automatically generated, do not edit~%")
    (format out "#||~%~a~%||#~%~%" (vulkan-license-header vk-spec))
    (format out "(in-package :vk)~%~%")

    (loop for struct in (sorted-elements (alexandria:hash-table-values (structures vk-spec)))
          unless (is-union-p struct)
          do (write-translate-from out struct nil vk-spec)))
  
  (with-open-file (out expand-from-file :direction :output :if-exists :supersede)
    (format out ";;; this file is automatically generated, do not edit~%")
    (format out "#||~%~a~%||#~%~%" (vulkan-license-header vk-spec))
    (format out "(in-package :vk)~%~%")

    (loop for struct in (sorted-elements (alexandria:hash-table-values (structures vk-spec)))
          unless (is-union-p struct)
          do (write-translate-from out struct t vk-spec))))

(defun write-vk-types-file (types-file vk-spec)
  ;;todo: write enums and bitfields
  ;;todo: write classes
  (with-open-file (out types-file :direction :output :if-exists :supersede)
    (format out ";;; this file is automatically generated, do not edit~%")
    (format out "#||~%~a~%||#~%~%" (vulkan-license-header vk-spec))
    (format out "(in-package :vk)~%~%")

    ;; todo: write base types which alias primitives (VkDeviceSize, VkDeviceAddress, etc.)
    (when (gethash "VkDeviceSize" (base-types vk-spec))
        (format out "(deftype device-size ()
  \"Represents the type [VkDeviceSize](https://www.khronos.org/registry/vulkan/specs/1.2-extensions/man/html/VkDeviceSize.html) as a (UNSIGNED-BYTE 64).\"
  '(unsigned-byte 64))~%~%"))
    (when (gethash "VkDeviceAddress" (base-types vk-spec))
      (format out "(deftype device-address ()
  \"Represents the type [VkDeviceAddress](https://www.khronos.org/registry/vulkan/specs/1.2-extensions/man/html/VkDeviceAddress.html) as a (UNSIGNED-BYTE 64).\"
  '(unsigned-byte 64))~%"))
    (write-vk-enums out vk-spec)
    (write-vk-handles out vk-spec)
    (write-classes out vk-spec)))
