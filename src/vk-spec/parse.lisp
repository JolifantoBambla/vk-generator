
(in-package :vk-generator/vk-spec)

(defun begins-with-p (str substr)
  (declare (type string str))
  (declare (type string substr))
  "Checks whether or not the given string STR begins with the string SUBSTR."
  (string= (subseq str 0 (length substr)) substr))

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
                   :type-name (or type "")
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
    (assert (or (= (length (type-name type-info) 0))
                (string= (prefix type-info) "typedef"))
            () "unexpected type prefix <~a>" (prefix type))
    (assert (or (= (length (prefix type-info) 0))
                (string= (prefix type-info "typedef")))
            () "unexpected type prefix <~a>" (prefix type))
    (assert (= (length (postfix type-info)) 0)
            () "unexpected type postfix <~a>" (postfix type))
    (when (> (length (type-name type-info)) 0)
      (assert (not (gethash (name name-data) (base-types vk-spec)))
              () "basetype <~a> already specified" (name name-data))
      (setf (gethash (name name-data) (base-types vk-spec))
            (make-instance 'base-type
                           :name (name name-data)
                           :lisp-name "TODO: fix name"
                           :xml-line (context-position node)
                           :type (type-name type-info))))
    (assert (not (gethash (name name-data) (types vk-spec)))
            () "basetype <~a> already specified as a type" (name name-data))
    (setf (gethash (name name-data) (types vk-spec)) :basetype)))

(defun parse-bitmask (node vk-spec)
  "TODO"
  (let ((alias (xps (xpath:evaluate "@alias" node))))
    (if alias
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
          (setf (gethash name (types vk-spec)) :bitmask))
        (let ((name-data (parse-name-data node))
              (type-info (parse-type-info node))
              (requirements (xps (xpath:evaluate "@requires" node))))
          (assert (begins-with-p (name name-data) "Vk")
                  () "name <~a> does not begin with <VK>" (name name-data))
          (assert (= (length (array-sizes name-data)) 0)
                  () "name <~a> with unsupported array-sizes" (array-sizes name-data))
          (when (find (type-name type-info) '("VkFlags" "VkFlags64"))
            (warn "unexpected bitmask type <~a>" (type-name type-info)))
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
                               :type (type-name type-info)
                               :requirements requirements))
          (assert (not (gethash (name name-data) (types vk-spec)))
                  () "bitmask <~a> already specified as a type" (name name-data))
          (setf (gethash (name name-data) (types vk-spec)) :bitmask)))))

(defun parse-define (node vk-spec)
  "TODO"
  (let ((xml-line (context-position node))
        (name (xps (xpath:evaluate "@name" node)))
        (requires (xps (xpath:evaluate "@requires" node))))
    (warn "defines not handled yet")))

(defun parse-enum-type (node vk-spec)
  "TODO"
  (let ((xml-line (context-position node))
        (alias (xps (xpath:evaluate "@alias" node)))
        (name (xps (xpath:evaluate "@name" node))))
    (if alias
        (progn
          (assert (> (length alias) 0)
                  (alias) "Line ~a: enum with empty alias" xml-line)
          (let ((enum (gethash alias (enums vk-spec))))
            (assert enum
                    () "Line ~a: enum with unknown alias <~a>" xml-line alias)
            (assert (= (length (alias enum)) 0)
                    () "Line ~a: enum <~a> already has an alias <~a>" xml-line (name enum) (alias enum))
            (setf (alias enum) alias)))
        (progn
          (assert (not (gethash name (enums vk-spec)))
                  (name) "Line ~a: enum <~a> already specified" xml-line name)
          (setf (gethash name (enums vk-spec))
                (make-instance 'enum
                               :name name
                               :lisp-name "TODO: fix lisp names"
                               :xml-line xml-line
                               :alias alias))))
    (assert (not (gethash name (types vk-spec)))
            (name) "Line ~a: enum <~a> already specified as a type" xml-line name)
    (setf (gethash name (types (vk-spec)))
          :enum)))

(defun str-not-empty-p (str)
  (and str (> (length str) 0)))

(defun parse-funcpointer (node vk-spec)
  "TODO"
  (let ((xml-line (context-position node))
        (requirements (xps (xpath:evaluate "@requires" node)))
        (name (xps (xpath:evaluate "name" node))))
    (assert (str-not-empty-p name)
            (name) "Line ~a: funcpointer with empty name" xml-line)
    (assert (not (gethash name (func-pointers vk-spec)))
            (name) "Line ~a: funcpointer <~a> already specified" xml-line name)
    (setf (gethash name (func-pointers vk-spec))
          (make-instance 'func-pointer
                         :name name
                         :lisp-name "TODO: fix me"
                         :xml-line xml-line
                         :requirements requirements))
    (assert (not (gethash name (types vk-spec)))
            (name) "Line ~a: funcpointer <~a> already specified as a type" xml-line name)
    (setf (gethath name (types vk-spec))
          :funcpointer)
    (let* ((types (mapcar 'xps (xpath:all-nodes (xpath:evaluate "type" node)))))
      (loop for type in types
            do (progn
                 (assert (str-not-empty-p type)
                         (type) "Line ~a: funcpointer argument with empty type" xml-line)
                 (assert (or (gethash type (types vk-spec))
                             (string= type requirements))
                         (type) "Line ~a: funcpointer argument of unknown type <~a>" xml-line type))))))

(defun parse-handle (node vk-spec)
  "TODO"
  (let ((xml-line (context-position node))
        (alias (xps (xpath:evaluate "@alias" node))))
    (if alias
        (let ((handle (gethash alias (handles vk-spec)))
              (name (xps (xpath:evaluate "@name" node))))
          (assert handle
                  (alias)
                  "Line ~a: using unspecified alias <~a>" xml-line alias)
          (assert (not (alias handle))
                  (alias handle)
                  "Line ~a: handle <~a> already has an alias <~a>" xml-line (name handle) (alias name))
          (setf (alias handle) name)
          (assert (not (gethash name (types vk-spec)))
                  (name)
                  "Line ~a: handle alias <~a> already specified as a type" xml-line name)
          (setf (gethash name (types vk-spec))
                :handle))
        (let ((parent (xps (xpath:evaluate "@parent" node)))
              (name-data (parse-name-data node))
              (type-info (parse-type-info node)))
          (assert (begins-with-p (name name-data) "Vk")
                  (name-data)
                  "Line ~a: name <~a> does not begin with <Vk>" xml-line (name name-data))
          (assert (= (length (array-sizes name-data)) 0)
                  (name-data)
                  "Line ~a: name <~a> with unsupported array-sizes" xml-line (name name-data))
          (assert (= (length (bit-count name-data)) 0)
                  (name-data)
                  "Line ~a: name <~a> with unsupported bit-count <~a>" xml-line (name name-data) (bit-count name-data))
          (assert (or (string= (type-name type-info) "VK_DEFINE_HANDLE")
                      (string= (type-name type-info) "VK_DEFINE_NON_DISPATCHABLE_HANDLE"))
                  (type-info)
                  "Line ~a: handle with invalid type <~a>" xml-line (type-name type-info))
          (assert (= (length (prefix type-info)) 0)
                  (type-info)
                  "Line ~a: unexpected type prefix <~a>" xml-line (prefix type-info))
          (assert (string= (postfix type-info) "(")
                  (type-info)
                  "Line ~a: unexpected type postfix <~a>" xml-line (postfix type-info))
          (assert (not (gethash (name name-data) (handles vk-spec)))
                  (name-data)
                  "Line ~a: handle <~a> already specified" xml-line (name name-data))
          (setf (gethash (name name-data) (handles vk-spec))
                (make-instance 'handle
                               :name (name name-data)
                               :lisp-name "TODO: fix this"
                               :xml-line xml-line
                               :parents (split-sequence:split-sequence #\, parent)))
          (assert (not (gethash (name name-data) (types vk-spec)))
                  (name-data)
                  "Line ~a: handle <~a> already specified as a type" xml-line (name name-data))
          (setf (gethash (name name-data) (types vk-spec))
                :handle)))))

(defun parse-type-include (node vk-spec)
  "TODO"
  (let ((xml-line (context-position node))
        (name (xps (xpath:evaluate "@name" node))))
    (assert (not (find name (includes vk-spec)))
            (name)
            "Line ~a: include named <~a> already specified" xml-line name)
    (push name (includes vk-spec))))

(defun determine-sub-struct (structure vk-spec)
  "TODO"
  (loop for other-struct being each hash-values of (stuctures vk-spec)
        when (and (string= (name structure) (name other-struct))
                      (< (length (members other-struct))
                         (length (members structure)))
                      (not (string= (first (members other-struct))
                                    "sType"))
                      (every (lambda (m1 m2)
                               (and (string= (type m1)
                                             (type m2))
                                    (string= (name m1)
                                             (name m2))))
                             (members other-struct)
                             (subseq (members structure)
                                     0 (length (members other-struct)))))
        return (name other-struct)))

(defun parse-struct (node vk-spec)
  "TODO"
  (let ((xml-line (context-position node))
        (alias (xps (xpath:evaluate "@alias" node)))
        (name (xps (xpath:evaluate "@name" node))))
    (if alias
        (let ((struct (gethash alias (structures vk-spec))))
          (assert struct
                  (alias)
                  "Line ~a: missing alias <~a>" xml-line alias)
          (assert (not (find name (aliases struct)))
                  ()
                  "Line ~a: struct <~a> already uses alias <~a>" xml-line alias name)
          (push name (aliases struct))
          (assert (not (gethash name (structure-aliases vk-spec)))
                  (name)
                  "Line ~a: structure alias <~a> already used" xml-line name)
          (setf (gethash name (structure-aliases vk-spec))
                alias)
          (assert (not (gethash name (types vk-spec)))
                  (name)
                  "Line ~a: struct <~a> already specified as a type" xml-line name)
          (setf (gethash name (types vk-spec))
                :struct))
        (let ((allow-duplicate-p (xps (xpath:evaluate "@allowduplicate" node)))
              (is-union-p (string= (xps (xpath:evaluate "@category" node)) "union"))
              (returned-only-p (xps (xpath:evaluate "@returnedonly" node)))
              (struct-extends (split-sequence:split-sequence #\, (xps (xpath:evaluate "@structextends" node)))))
          (assert name
                  (name)
                  "Line ~a: struct has no name" xml-line)
          ;; todo: this should be an assert in a future version
          (when (or (not allow-duplicate-p)
                    (> (length struct-extends) 0))
            (warn "Line ~a: attribute <allowduplicate> is true, but no structures are listed in <structextends>" xml-line))
          (assert (not (gethash name (structures vk-spec)))
                  (name)
                  "Line ~a: struct <~a> already specified" xml-line name)
          (setf (gethash name (structures vk-spec))
                (make-instance 'structure
                               :name name
                               :lisp-name "TODO: fix me"
                               :xml-line xml-line
                               :struct-extends struct-extends
                               :allow-duplicate-p allow-duplicate-p
                               :returned-only-p returned-only-p
                               :is-union-p is-union-p))
          ;; todo: parse-struct-member
          (setf (sub-struct (gethash name (structures vk-spec)))
                (determine-sub-struct (gethash name (structures vk-spec))
                                      vk-spec))
          (setf (extended-structs vk-spec)
                (remove-duplicates
                 (append struct-extends (extended-structs vk-spec))
                 :test #'string=))
          (assert (not (gethash name (types vk-spec)))
                  (name)
                  "Line ~a: struct <~a> already specified as a type" xml-line name)))))


(defun parse-types (vk.xml vk-spec)
  "TODO"
  (xpath:do-node-set (node (xpath:evaluate "/registry/types[@category=basetype]" vk.xml))
    (parse-basetype node vk-spec))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types[@category=bitmask]" vk.xml))
    (parse-bitmask node vk-spec))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types[@category=define]" vk.xml))
    (parse-define node vk-spec))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types[@category=enum]" vk.xml))
    (parse-enum-type node vk-spec))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types[@category=funcpointer]" vk.xml))
    (parse-funcpointer node vk-spec))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types[@category=handle]" vk.xml))
    (parse-handle node vk-spec))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types[@category=include]" vk.xml))
    (parse-handle node vk-spec))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types[@category=struct]" vk.xml))
    (parse-handle node vk-spec))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types[@category=union]" vk.xml))
    (parse-handle node vk-spec))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types[not(@category)]" vk.xml))))
