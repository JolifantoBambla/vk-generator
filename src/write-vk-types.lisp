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
      (t (string (fix-type-name type-name (tags vk-spec)))))))

(defun make-documentation (struct count-member-names vk-spec)
  (let* ((referenced-types (remove-duplicates
                            (loop for m in (members struct)
                                  for type-name = (type-name (type-info m))
                                  unless (or (gethash type-name *vk-platform*)
                                             (= (length (allowed-values m)) 1))
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
                               collect (fix-type-name s (tags vk-spec)))))
    (format nil "Represents the ~:[struct~;union~] [~a](https://www.khronos.org/registry/vulkan/specs/1.2-extensions/man/html/~a.html).

Slots:~{~a~}~@[~%~{~%See ~a~}~]~@[~%~%Instances of this class can be used to extend the following classes (using their NEXT slot):~{~%See ~a~}~]
"
            (is-union-p struct)
            (name struct)
            (name struct)
            slots
            referenced-types
            struct-extends)))

;; todo: check out how they do it in void VulkanHppGenerator::appendStructConstructors or VulkanHppGenerator::appendStructConstructorsEnhanced
(defun write-classes (out vk-spec)
  (loop for struct in (sorted-elements (alexandria:hash-table-values (structures vk-spec)))
        for count-member-names = (get-count-member-names struct)
        for wrote-first-member = nil
        do
        (format out "~%(defclass ~(~a~) ()" (fix-type-name (name struct) (tags vk-spec)))
        (loop for m in (members struct)
              for fixed-slot-name = (fix-slot-name (name m) (type-name (type-info m)) vk-spec)
              unless (or (find (name m) count-member-names :test #'string=) ;; we'll determine the count during translation
                         (= (length (allowed-values m)) 1)) ;; slots which must be the same for all instances only have to be set during translation
              do
              (format out "~%  ~:[ ~;(~](~(~a~%     :initarg :~a~%~:[     :initform nil~%~;~]     :accessor ~a~))"
                      (not wrote-first-member)
                      fixed-slot-name
                      fixed-slot-name
                      (is-union-p struct)
                      (fix-slot-name (name m) (type-name (type-info m)) vk-spec t))
              (setf wrote-first-member t))
        (format out ")~%  (:documentation ~s))~%" (make-documentation struct count-member-names vk-spec)))
  ;; todo: translate/expand-to/from (for unions it must be checked which slot is bound during translation in a cond)
  ;; todo: pretty-printing of classes
  ;; todo: aliases can just be subclasses of the aliases classes without extra slots
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
      ;; union slots: determine at runtime which slot is bound and then translate this slot
      ((and (gethash (type-name (type-info member-data)) (structures vk-spec))
            (is-union-p  (gethash (type-name (type-info member-data)) (structures vk-spec))))
       (let* ((union (gethash (type-name (type-info member-data)) (structures vk-spec)))
              (union-members (members union)))
         (format nil "(let ((slot (vk:~(~a~) ~a))) (cond ~{~a~}))"
                 fixed-accessor-name
                 value-str
                 (loop for m in union-members
                       collect (format nil "((slot-boundp slot ~(~a~)) ~a)"
                                       (fix-slot-name (name m) (type-name (type-info m)) vk-spec)
                                       (get-value-setter m union (get-count-member-names union) nil vk-spec "slot"))))))

      ;; todo: shouldn't create the type mappings every call (but for the most part it shouldn't be a problem either, because only few structs can be extended by many other structs)
      ;; type of "pNext" must be determined at runtime
      ((string= "pNext" (name member-data))
       (let ((extending-structs (get-extends struct vk-spec)))
         (if extending-structs
             (format nil "(if (vk:next ~a)~%                     (translate-next-chain (vk:next ~a)~%                                           (alexandria:plist-hash-table~%                                            '(~{~(~a~)~}))~%                                           ~('vk:~s~)~%                                           ~a)~%                     (cffi:null-pointer))"
                     value-str
                     value-str
                     (loop for extending-struct in extending-structs
                           for i from 0
                           for struct-name = (fix-type-name (name extending-struct) (tags vk-spec))
                           collect (format nil "~:[~%                                              ~;~]vk:~(~a~) (:struct %vk:~(~a~))"
                                           (= i 0)
                                           struct-name
                                           struct-name))
                     (fix-type-name (name struct) (tags vk-spec))
                     ptr-str)
             "(cffi:null-pointer)")))
      
      ;; void pointer - must be handled by user
      ((string= "void" (type-name (type-info member-data)))
       (format nil "(vk:~(~a~) ~a)" fixed-accessor-name value-str))
      
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
       (format nil "(vk-alloc:foreign-allocate-and-fill '(:struct ~(~a~)) (~(vk:~a~) ~a) ~a)"
               (fix-type-name (type-name (type-info member-data)) (tags vk-spec))
               fixed-accessor-name
               value-str
               ptr-str))

      ;; lists of strings and primitive values
      ((and (len member-data)
            (find-if (lambda (count-member)
                       (string= (car (len member-data)) count-member))
                     count-member-names)
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
       (format nil "(vk-alloc:foreign-allocate-and-fill ~(~a~) (~(vk:~a~) ~a) ~a)"
               (fix-type-name (type-name (type-info member-data)) (tags vk-spec))
               fixed-accessor-name
               value-str
               ptr-str))

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
      
      ;; type of "pNext" must be determined at runtime
      ((string= "pNext" (name member-data))
       ;; first translate to VkBaseOutStructure to get the structure type and then translate to actual class
       (format nil "~((when (not (cffi:null-pointer-p p-next)) (let ((base-out (cffi:mem-aref p-next '(:struct %vk:base-out-structure)))) (cffi:mem-aref p-next (list :struct (find-symbol (string (vk:s-type base-out)) :%vk)))))~)"))
      
      ;; void pointer - keep as is, must be handled by user
      ((string= "void" (type-name (type-info member-data)))
       (format nil "~(~a~)"
               (fix-type-name (name member-data) (tags vk-spec))))
      
      ;; members of some VK-defined type (lists or single instances)
      ((and (gethash (type-name (type-info member-data)) (structures vk-spec))
            (find-if (lambda (count-member)
                       (string= (car (len member-data)) count-member))
                     count-member-names))
       (format nil "~((loop for i from 0 below ~a collect (cffi:mem-aref ~a '(:struct %vk:~a) i))~)"
               (let ((count-member (find-if (lambda (m)
                                              (string= (car (len member-data)) (name m)))
                                            (members struct))))
                 (fix-type-name (name count-member) (tags vk-spec)))
               (fix-type-name (name member-data) (tags vk-spec))
               (fix-type-name (type-name (type-info member-data)) (tags vk-spec))))

      ;; lists of strings and primitive values
      ((and (len member-data)
            (find-if (lambda (count-member)
                       (string= (car (len member-data)) count-member))
                     count-member-names)
            (gethash (type-name (type-info member-data)) *vk-platform*))
       (format nil "~((loop for i from 0 below ~a collect (cffi:mem-aref ~a ~s i))~)"
               (let ((count-member (find-if (lambda (m)
                                              (string= (car (len member-data)) (name m)))
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
       (format nil "~((loop for i from 0 below ~a collect (cffi:mem-aref ~a '%vk:~a i))~)"
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
       (format nil "(foreign-string-to-lisp ~(~a~))"
               (fix-type-name (name member-data) (tags vk-spec))))
      
      ;; most types can be automatically translated (primitive types, bitfields, enums, structs)
      (t
       (format nil "~(~a~)"
               (fix-type-name (name member-data) (tags vk-spec)))))))

(defun write-vk-translators (out struct vk-spec)
  (let ((fixed-type-name (fix-type-name (name struct) (tags vk-spec)))
        (count-member-names (get-count-member-names struct)))
    (format out "(defmethod translate-into-foreign-memory (value (type c-~(~a~)) ptr)~%  (with-foreign-slots" fixed-type-name)
    (loop for m in (members struct)
          for i from 1
          do (format out "~%      ~:[  ~;((~]~(~a~)~:[~;)~]"
                     (= 1 i)
                     (fix-type-name (name m) (tags vk-spec))
                     (= i (length (members struct)))))
    (format out "~%       ptr~%       (:struct ~(~a~)))" fixed-type-name)
    (loop for m in (members struct)
          for i from 1
          do (format out "~%~:[          ~;    (setf ~]~(~a~) ~a~:[~;)))~%~%~]"
                     (= 1 i)
                     (fix-type-name (name m) (tags vk-spec))
                     (get-value-setter m struct count-member-names nil vk-spec)
                     (= i (length (members struct)))))

    #|
    ;; write expand-into-foreign-memory
    (format out "(defmethod expand-into-foreign-memory (value (type c-~(~a~)) ptr)~%  `(with-foreign-slots" fixed-type-name)
    (loop for m in (members struct)
          for i from 1
          do (format out "~%      ~:[  ~;((~]~(~a~)~:[~;)~]"
                     (= 1 i)
                     (fix-type-name (name m) (tags vk-spec))
                     (= i (length (members struct)))))
    (format out "~%       ,ptr~%       (:struct ~(~a~)))" fixed-type-name)
    (loop for m in (members struct)
          for i from 1
          do (format out "~%~:[          ~;    (setf ~]~(~a~) ~a~:[~;)))~%~%~]"
                     (= 1 i)
                     (fix-type-name (name m) (tags vk-spec))
                     (get-value-setter m struct count-member-names t vk-spec)
                     (= i (length (members struct)))))
    |#
    
    ;; write translate-from-foreign-memory
    (format out "(defmethod translate-from-foreign (ptr (type c-~(~a~)))~%  (with-foreign-slots" fixed-type-name)
    (loop for m in (members struct)
          for i from 1
          do (format out "~%      ~:[  ~;((~]~(~a~)~:[~;)~]"
                     (= 1 i)
                     (fix-type-name (name m) (tags vk-spec))
                     (= i (length (members struct)))))
    (format out "~%       ptr~%       (:struct ~(~a~)))" fixed-type-name)
    (format out "~%    (make-instance 'vk:~(~a~)" fixed-type-name)
    (loop for m in (members struct)
          unless (or (find (name m) count-member-names :test #'string=) ;; redundant count values
                     (= (length (allowed-values m)) 1)) ;; redundant constant values
          do (format out "~%                   ~(:~a ~a~)"
                     (fix-slot-name (name m) (type-name (type-info m)) vk-spec)
                     (get-slot-setter m struct count-member-names nil vk-spec)))
    (format out ")))~%~%")
    ;; todo: write expand-from-foreign-memory
    ))

(defun write-vk-struct-translators-file (translators-file vk-spec)
  (with-open-file (out translators-file :direction :output :if-exists :supersede)
    (format out ";;; this file is automatically generated, do not edit~%")
    (format out "#||~%~a~%||#~%~%" (vulkan-license-header vk-spec))
    (format out "(in-package :~a)~%~%" *in-package-name*)

    (loop for struct in (sorted-elements (alexandria:hash-table-values (structures vk-spec)))
          unless (is-union-p struct)
          do (write-vk-translators out struct vk-spec))))

(defun write-vk-types-file (types-file vk-spec)
  ;;todo: write header
  ;;todo: write enums and bitfields
  ;;todo: write classes
  (with-open-file (out types-file :direction :output :if-exists :supersede)
    (format out ";;; this file is automatically generated, do not edit~%")
    (format out "#||~%~a~%||#~%~%" (vulkan-license-header vk-spec))
    (format out "(in-package :vk)~%~%")

    (write-classes out vk-spec)))
