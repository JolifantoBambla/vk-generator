(in-package :vk-generator)

(defun get-default-slot-value-string (member-data count-member-names vk-spec)
  (let ((array-member-p (and (len member-data)
                             (member (first (len member-data))
                                     count-member-names
                                     :test #'string=)))
        (type-name (type-name (type-info member-data))))
    (if (and (value-p (type-info member-data))
             (structure-type-p type-name vk-spec)
             (not (is-union-p (get-structure-type type-name vk-spec))))
        ;; structs which aren't pointers need to be initialized during translation
        (format nil "(vk:make-~(~a~))"
                (fix-type-name type-name (tags vk-spec)))
        (format nil "~(~s~)"
                (cond
                  (array-member-p
                   nil)
                  ((string= "char" type-name)
                   "")
                  ((or (and (gethash type-name *vk-platform*)
                            (or (search "int" type-name)
                                (string= "size_t" type-name)
                                ))
                       (member type-name '("VkDeviceSize" "VkDeviceAddress") :test #'string=))
                   0)
                  ((and (gethash type-name *vk-platform*)
                        (or (string= "float" type-name)
                            (string= "double" type-name)))
                   0.0)
                  (t nil))))))

(defun make-slot-data (struct vk-spec)
  (let ((count-member-names (get-count-member-names struct)))
    (loop for m in (members struct)
          unless (or (member (name m) count-member-names :test #'string=)
                     (= (length (allowed-values m)) 1))
          collect (list (fix-slot-name (name m) (type-name (type-info m)) vk-spec)
                        (get-default-slot-value-string m count-member-names vk-spec)))))

(defun write-constructors (ctor-file vk-spec &optional dry-run)
  (flet ((write-to-stream (out)
           (format out ";;; this file is automatically generated, do not edit~%~%")
           (format out "(in-package :vk)~%~%")
           (loop for struct in (sorted-elements (alexandria:hash-table-values (structures vk-spec)))
                 for fixed-type-name = (fix-type-name (name struct) (tags vk-spec))
                 for slot-data = (make-slot-data struct vk-spec)
                 for unionp = (is-union-p struct)
                 do (format out "
(defun make-~(~a~) ~a~a~a~a)~%"
                            fixed-type-name
                            (concatenate 'list
                                         '("&key")
                                         (loop for s in slot-data
                                               collect (if unionp
                                                           (format nil "(~(~a~) nil)"
                                                                   (first s))
                                                           (format nil "(~(~a~) ~a)"
                                                                   (first s)
                                                                   (second s)))))
                            (format nil "
\"Creates an instance of ~a.
The arguments of this function correspond to the slots of ~a.
~a

See ~a\""
                                    fixed-type-name
                                    fixed-type-name
                                    (if unionp
                                        (format nil "~%Since ~a represents a union, exactly one argument must be supplied."
                                                fixed-type-name)
                                        "")
                                    fixed-type-name)
                            (if unionp
                                (let ((arg-names (loop for s in slot-data
                                                       collect (format nil "~(~a~)" (first s)))))
                                  (format nil "
  (assert (= 1 (count-if #'identity (list~{ ~a~})))
          ~a
          \"Exactly one argument must be non-nil.\")"
                                          arg-names
                                          arg-names))
                                "")
                            (if unionp
                                (format nil "
  (cond ~{~a~})"
                                        (loop for s in slot-data
                                              collect (format nil "
    (~(~a~)~%     (make-instance 'vk:~(~a~) :~(~a~) ~(~a~)))"
                                                              (first s)
                                                              fixed-type-name
                                                              (first s)
                                                              (first s))))
                                (format nil "
  (make-instance 'vk:~(~a~) ~{~%                 ~a~})"
                                        fixed-type-name
                                        (loop for s in slot-data
                                              collect (format nil ":~(~a~) ~(~a~)"
                                                              (first s)
                                                              (first s)))))))))
    (if dry-run
        (write-to-stream t)
        (with-open-file (out ctor-file :direction :output :if-exists :supersede)
          (write-to-stream out)))))
