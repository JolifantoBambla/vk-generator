(in-package :vk-generator)

(defun fix-slot-name (name type-name vk-spec)
  "Fixes the NAME of a slot for a class representation of a struct in the Vulkan API.
Removes the \"P-\"-prefix for pointer types.
Removes the \"PP-\"-prefix for pointers to string arrays.
Changes the \"PP-\"-prefix to \"P-\" for pointers to pointer arrays (e.g. ppGeometries in VkAccelerationStructureBuildGeometryInfoKHR)
"
  (let ((fixed-type-name (string (fix-type-name name (tags vk-spec)))))
    (cond
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
                      fixed-slot-name)
              (setf wrote-first-member t))
        (format out ")~%  (:documentation ~s))~%" (make-documentation struct count-member-names vk-spec)))
  ;; todo: translate/expand-to/from (for unions it must be checked which slot is bound during translation in a cond)
  ;; todo: pretty-printing of classes
  ;; todo: aliases can just be subclasses of the aliases classes without extra slots
  )

(defparameter *vk-to-%vk-type-name* "*vk-to-%vk-type*")
(defparameter *%vk-to-vk-type-name* "*%vk-to-vk-type*")

(defun get-value-setter (member-data struct count-member-names macro-p vk-spec)
  (let ((fixed-slot-name (fix-slot-name (name member-data) (type-name (type-info member-data)) vk-spec)))
    (cond
      ;; todo: check if member type is a union and then do a cond for the translation
      ;; type of "pNext" must be determined at runtime
      ((string= "pNext" (name member-data))
       ;; todo: pNext-validation (or maybe specialize on setf for this - translation might be a bit late for that)
       "(cffi:null-pointer)") ;; todo: find possible extension structs and make a cond
       ;; (format nil "(let ((slot (vk:next value))) (if slot (vk-alloc:foreign-allocate-and-fill (list :struct (gethash (class-name (class-of slot)) ~a)) slot ptr) (cffi:null-pointer)))" *vk-to-%vk-type-name*))
      ;; void pointer - must be handled by user
      ((string= "void" (type-name (type-info member-data)))
       (format nil "(vk:~(~a~) value)" fixed-slot-name))
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
       (format nil "(let ((slot (~(vk:~a~) value))) (vk-alloc:foreign-allocate-and-fill '(:struct ~(~a~)) slot ptr))"
               fixed-slot-name
               (fix-type-name (type-name (type-info member-data)) (tags vk-spec))))
      ;; lists of strings and primitive values
      ((and (len member-data)
            (find-if (lambda (count-member)
                       (string= (car (len member-data)) count-member))
                     count-member-names)
            (gethash (type-name (type-info member-data)) *vk-platform*))
       (let ((foreign-type-name (if (string= (type-name (type-info member-data)) "char")
                                    :string
                                    (gethash (type-name (type-info member-data)) *vk-platform*))))
         (format nil "(let ((slot (~(vk:~a~) value))) (vk-alloc:foreign-allocate-and-fill ~(~s~) slot ptr))"
                 fixed-slot-name
                 foreign-type-name)))
      ;; lists of handles
      ((and (len member-data)
            (find-if (lambda (count-member)
                       (string= (car (len member-data)) count-member))
                     count-member-names))
       (format nil "(let ((slot (~(vk:~a~) value))) (vk-alloc:foreign-allocate-and-fill ~(~a~) slot ptr))"
               fixed-slot-name
               (fix-type-name (type-name (type-info member-data)) (tags vk-spec))))
      ;; set size-members based on list lengths
      ((find (name member-data) count-member-names :test #'string=)
       (let* ((slots (remove-if-not (lambda (s)
                                      (string= (car (len s)) (name member-data)))
                                    (members struct)))
              (size-getter (format nil "(reduce #'max (list ~{~((length (vk:~a value))~)~}))"
                                   (mapcar (lambda (s) (fix-slot-name (name s) (type-name (type-info s)) vk-spec))
                                           slots))))
         (cond
           ((string= "codeSize" (name member-data))
            (format nil "(* 4 ~a)" size-getter))
           (t size-getter))))
      (t (format nil "(~(vk:~a~) value)" fixed-slot-name)))))

(defun write-vk-translators (out struct vk-spec)
  ;; todo: a union should only set the member which has a bound slot in "value"
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
                     (= i (length (members struct)))))))

(defun write-type-mappings (out vk-spec)
  (format out "(defparameter ~a~%  (make-hash-table))~%" *vk-to-%vk-type-name*)
  (format out "(defparameter ~a~%  (make-hash-table))~%" *%vk-to-vk-type-name*)
  (loop for struct in (sorted-elements (alexandria:hash-table-values (structures vk-spec)))
        for fixed-type-name = (fix-type-name (name struct) (tags vk-spec))
        do
          (format out "(setf (gethash 'vk:~(~a~) ~a) ~(~a~))~%"
                  fixed-type-name
                  *vk-to-%vk-type-name*
                  fixed-type-name)
          (format out "(setf (gethash '~(~a~) ~a) vk:~(~a~))~%"
                  fixed-type-name
                  *%vk-to-vk-type-name*
                  fixed-type-name))
  (format out "~%"))

(defun write-vk-struct-translators-file (translators-file vk-spec)
  (with-open-file (out translators-file :direction :output :if-exists :supersede)
    (format out ";;; this file is automatically generated, do not edit~%")
    (format out "#||~%~a~%||#~%~%" (vulkan-license-header vk-spec))
    (format out "(in-package :~a)~%~%" *in-package-name*)

    ;;(write-type-mappings out vk-spec)
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
