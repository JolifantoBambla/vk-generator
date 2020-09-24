
(in-package :vk-generator:vk-spec)

(defun parse-modifiers (node)
  (let ((array-sizes nil)
        (bit-count ""))
    (when (node)
      (let ((value (xps (xpath:evaluate "::text()" node))))
        (when (and value
                   (> (length value) 0))
          (cond
            ((string= (first value) "[")
             (let ((end-pos 0))
               (loop while (not (= (+ end-pos) (length value)))
                     do (let ((start-pos (position #\[ :start end-pos)))
                          (assert start-pos
                                  () "could not find '[' in <~a>" value)
                          (setf end-pos (position #\] :start start-pos))
                          (assert end-pos
                                  () "could not find ']' in <~a>" value)
                          (assert (<= (+ start-pos 2) end-pos)
                                  () "missing content between '[' and ']' in <~a>" value)
                          (push (subseq value (1+ start-pos) (- end-pos start-pos 1))
                                array-sizes)))))
            ((string= (first value) ":")
             (setf bit-count (cdr value))
             )
            (t
             (assert (or (string= (first value) ";")
                         (string= (first value) ")"))
                     () "unknown modifier <~a>" value))))))
    (values array-sizes bit-count)))

(defun parse-name-data (node)
  "TODO"
  ;; todo: check attributes
  (let ((name (xps (xpath:evaluate "name" node))))
    (multiple-value-bind (array-sizes bit-count)
        (parse-modifiers (xpath:evaluate "name/following-sibling" node))
      (make-instance 'name-data
                     :name name
                     :array-sizes array-sizes
                     :bit-count bit-count))))

(defun parse-type-info (node)
  "TODO"
  ;; todo: check attributes
  (let ((type (xps "type" node))
        (prefix (xps (xpath:evaluate "type/preceding-sibling::text()" node)))
        (postfix (xps (xpath:evaluate "type/following-sibling::text()" node))))
    (make-instance 'type-info
                   :type (or type "")
                   :prefix (or prefix "")
                   :postfix (or postfix ""))))

(defun parse-basetype (node vk-spec)
  "TODO"
  ;; todo: check attributes
  (let* ((attributes (attrib-names node))
         (name-data (parse-name-data node))
         (type-info (parse-type-info node)))
    (assert (not (array-sizes name-data))
            () "name <~a> with unsupported array-sizes" (name name-data))
    (assert (not (bit-count name-data))
            () "name <~a> with unsupported bit-count <~a>" (name name-data) (bit-count name-data))
    (assert (or (= (length (type type-info) 0))
                (string= (prefix type-info) "typedef"))
            () "unexpected type prefix <~a>" (prefix type))
    (assert (or (= (length (prefix type-info) 0))
                (string= (prefix type-info "typedef")))
            () "unexpected type prefix <~a>" (prefix type))
    (assert (= (length (postfix type-info)) 0)
            () "unexpected type postfix <~a>" (postfix type))
    (when (> (length (type type-info)) 0)
      (assert (not (gethash (name name-data) (base-types vk-spec)))
              () "basetype <~a> already specified" (name name-data))
      (setf (gethash (name name-data) (base-types vk-spec))
            (make-instance 'base-type
                           :name (name name-data)
                           :lisp-name "TODO: fix name"
                           :xml-line (context-position node)
                           :type (type type-info))))
    (assert (not (gethash (name name-data) (types vk-spec)))
            () "basetype <~a> already specified as a type" (name name-data))
    (setf (gethash (name name-data) (types vk-spec)) :basetype))

(defun parse-bitmask-alias (node vk-spec)
  "TODO"
  (let* ((alias (xps (xpath:evaluate "@alias" node)))
         (name (xps (xpath:evaluate "@name" node)))
         (bitmask (gethash name (bitmasks vk-spec))))
    (assert bitmask
            () "missing alias <~a>" alias)
    (assert (= (length (alias bitmask)))
            () "alias for bitmask <~a> already specified as <~a>" (name bitmask) (alias bitmask))
    (setf (alias bitmask) name)
    (assert (not (gethash name (types vk-spec)))
            () "aliased bitmask <~a> already specified as a type" name)
    (setf (gethash name (types vk-spec)) :bitmask)))

(defun parse-bitmask (node vk-spec)
  "TODO"
  (let ((alias (xps (xpath:evaluate "@alias" node))))
    (if (alias)
        (parse-bitmask-alias node vk-spec)
        (let ((name-data (parse-name-data node))
              (type-info (parse-type-info node))
              (requirements (xps (xpath:evaluate "@requires" node))))
          (assert (string= (subseq (name name-data) 0 1) "Vk")
                  () "name <~a> does not begin with <VK>" (name name-data))
          (assert (= (length (array-sizes name-data)) 0)
                  () "name <~a> with unsupported array-sizes" (array-sizes name-data))
          (when (find (type type-info) '("VkFlags" "VkFlags64"))
            (warn "unexpected bitmask type <~a>" (type type-info)))
          (assert (string= (prefix type-info) "typedef")
                  () "unexpected type prefix <~a>" (prefix type-info))
          (assert (= (length (postfix type-info)) 0)
                  () "unexpected type postfix <~a>" (postfix type-info))
          (assert (not (gethash (name name-data) (commands vk-spec)))
                  () "command <~a> already specified" (name name-data))
          (setf (gethash (name name-data) (bitmasks vk-spec))
                (make-instance 'bitmask
                               :name (name name-data)
                               :lisp-name "TODO: fix name"
                               :xml-line (context-position node)
                               :type (type type-info)
                               :requirements requirements))
          (assert (not (gethash (name name-data) (types vk-spec)))
                  () "bitmask <~a> already specified as a type" (name name-data))
          (setf (gethash (name name-data) (types vk-spec)) :bitmask)))))

(defun parse-types (vk.xml vk-spec)
  "TODO"
  (xpath:do-node-set (node (xpath:evaluate "/registry/types[@category=basetype]" vk.xml))
    (parse-basetype node vk-spec))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types[@category=bitmask]" vk.xml))
    (parse-bitmask node vk-spec))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types[@category=define]" vk.xml)))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types[@category=enum]" vk.xml)))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types[@category=funcpointer]" vk.xml)))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types[@category=handle]" vk.xml)))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types[@category=include]" vk.xml)))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types[@category=struct]" vk.xml)))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types[@category=union]" vk.xml)))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types[not(@category)]" vk.xml))))))
