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
            (string= fixed-type-name "char"))
       (subseq fixed-type-name 3))
      ((alexandria:starts-with-subseq "PP-" fixed-type-name)
       (subseq fixed-type-name 1))
      (t fixed-type-name))))

(defun get-count-member-names (struct)
  "Returns a list of all names of members in STRUCT which specify the number of elements within an array member of the same STRUCT."
  (loop for m in (members struct)
        when (or (string= (name m) "codeSize") ;; codeSize is a special case where the len is actually codeSize/4
                 (find-if (lambda (other)
                            (string= (car (len other)) (name m)))
                          (members struct)))
        collect (name m)))

(defun fix-slot-type-for-documentation (slot-name type-name vk-spec)
  (cond
    ((string= "pNext" slot-name)
     "instance of a class extending this class")
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
    (t (string (fix-type-name type-name (tags vk-spec))))))

(defun make-documentation (struct count-member-names vk-spec)
  (let* ((referenced-types (remove-duplicates
                            (loop for m in (members struct)
                                  for type-name = (type-name (type-info m))
                                  unless (or (gethash type-name *vk-platform*)
                                             (= (length (allowed-values m)) 1))
                                  collect (fix-type-name type-name (tags vk-spec)))))
         (slots (loop for m in (members struct)
                      for fixed-type-name = (fix-slot-type-for-documentation
                                             (name m)
                                             (type-name (type-info m))
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

(defun get-value-setter (member-data struct count-member-names vk-spec)
  (cond
    ((string= "pNext" (name member-data))
     ;; todo: pNext-validation
     (format nil "(let ((slot (~(a~) value))) (if (slot) (vk-alloc:foreign-allocate-and-fill '(:struct (class-name (class-of value)))) (cffi:null-pointer)))"))
    ((= (length (allowed-values member-data)) 1)
     (format nil ":~(~a~)"
             (fix-bit-name (first (allowed-values member-data))
                           (tags vk-spec)
                           :prefix (find-enum-prefix
                                    (string (fix-type-name (type-name (type-info member-data) (tags vk-spec))))
                                    (enum-values (gethash (type-name (type-info member-data))))
                                    (tags vk-spec)))))
    ((gethash (type-name (type-info member-data)) (structures vk-spec))
     (format nil "(let ((slot (~(~a~) value))) (if (slot) (vk-alloc:foreign-allocate-and-fill '(:struct ~(~a~)) slot) (cffi:null-pointer)))"
             (fix-type-name (name member-data) (tags vk-spec)) ;; todo: this needs to be a fixed slot value!
             (fix-type-name (type-name (type-info member-data)) (tags vk-spec))))
    ;; todo: check if it's a list of strings, ints, voids or whatever
    ((find (name member-data) count-member-names :test #'string=)
     (let ((slots (remove-if-not (lambda (s)
                                   (string= (car (len s)) (name member-data)))
                                 (members struct))))
       ;; todo: if multiple slots: only write the slot that has the most entries; else: write the one slot
       (cond
         ((string= "codeSize" (name member-data))
          ;; divide by 4 or something
          ())
         )
       (format nil "()")))
    (t (format nil "(~(~a~) value)" (fix-type-name (name member-data) (tags vk-spec))))))

(defun write-vk-translators (out struct vk-spec)
  ;; todo: a union should only set the member which has a bound slot in "value"
  (let ((fixed-type-name (fix-type-name (name struct) (tags vk-spec)))
        (count-member-names (get-count-member-names struct)))
    (format out "~%(defmethod translate-into-foreign-memory (value (type c-~(~a~)) ptr)~%  (with-foreign-slots" fixed-type-name)
    (loop for m in (members struct)
          for i from 1
          do (format "~%      ~:[  ~;((~]~(~a~)~:[~;)~]"
                     (= 1 i)
                     (fix-type-name (name m) (tags vk-spec))
                     (= i (length (members struct)))))
    (format "~%        ptr~%        (:struct ~(~a~))" fixed-type-name)
    (loop for m in (members struct)
          for i from 1
          (format "~%~:[          ;    (setf ~]~a ~a~:[~;)))~]~%"
                  (= 1 i)
                  (fix-type-name (name m) (tags vk-spec))
                  (get-value-setter m struct count-member-names vk-spec)
                  (= 1 (length (members struct)))))))

(defun write-vk-types-file (types-file vk-spec)
  ;;todo: write header
  ;;todo: write enums and bitfields
  ;;todo: write classes
  (with-open-file (out types-file :direction :output :if-exists :supersede)
    (format out ";;; this file is automatically generated, do not edit~%")
    (format out "#||~%~a~%||#~%~%" (vulkan-license-header vk-spec))
    (format out "(in-package :vk)~%~%")

    (write-classes out vk-spec)))
