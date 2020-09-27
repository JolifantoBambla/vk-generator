
(in-package :vk-generator/vk-spec)

(defun begins-with-p (str substr)
  (declare (type string str))
  (declare (type string substr))
  "Checks whether or not the given string STR begins with the string SUBSTR."
  (string= (subseq str 0 (length substr)) substr))

(defun parse-boolean (node)
  "Checks whether or not the given node NODE holds a string that equals 'true'."
  (let ((str (xps node)))
    (and str (string= str "true"))))

(defun parse-modifiers (node)
  (let ((array-sizes nil)
        (bit-count nil))
    (unless (xpath:node-set-empty-p node)
      (let ((value (xps (xpath:evaluate "text()" node))))
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
  (let ((type-name (xps (xpath:evaluate "type" node)))
        (prefix (xps (xpath:evaluate "type/preceding-sibling::text()" node)))
        (postfix (xps (xpath:evaluate "type/following-sibling::text()" node))))
    (make-instance 'type-info
                   :type-name (or type-name "")
                   :prefix prefix
                   :postfix postfix)))

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
    (assert (or (= (length (type-name type-info)) 0)
                (string= (prefix type-info) "typedef"))
            () "unexpected type prefix <~a>" (prefix type))
    (assert (or (= (length (prefix type-info)) 0)
                (string= (prefix type-info) "typedef"))
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
                           :xml-line (xpath:evaluate "position()" node)
                           :type-name (type-name type-info))))
    (assert (not (gethash (name name-data) (types vk-spec)))
            () "basetype <~a> already specified as a type" (name name-data))
    (setf (gethash (name name-data) (types vk-spec)) :basetype)))

(defun parse-bitmask (node vk-spec)
  "TODO"
  (let ((alias (xps (xpath:evaluate "@alias" node))))
    (if alias
        (let* ((alias (xps (xpath:evaluate "@alias" node)))
               (name (xps (xpath:evaluate "@name" node)))
               (bitmask (gethash alias (bitmasks vk-spec))))
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
                               :xml-line (xpath:evaluate "position()" node)
                               :type-name (type-name type-info)
                               :requirements requirements))
          (assert (not (gethash (name name-data) (types vk-spec)))
                  () "bitmask <~a> already specified as a type" (name name-data))
          (setf (gethash (name name-data) (types vk-spec)) :bitmask)))))

(defun parse-define (node vk-spec)
  "TODO"
  (let* ((xml-line (xpath:evaluate "position()" node))
         (name (xps (xpath:evaluate "name" node)))
         (@name (xps (xpath:evaluate "@name" node)))
         (type (xps (xpath:evaluate "type" node)))
         (requires (xps (xpath:evaluate "@requires" node)))
         (args (xps (xpath:evaluate (cond
                                      (type "type/following-sibling::text()")
                                      (name "name/following-sibling::text()")
                                      (@name "text()")
                                      (t (error "Line ~a: Unknown define args path for define" xml-line)))
                                    node)))
         (is-value-p (begins-with-p args "("))
         (is-struct-p (search "struct" (xps node))))
    (when is-struct-p
        (assert (not (gethash name (types vk-spec)))
                ()
                "Line ~a: type <~a> has already been specified" xml-line name)
        (setf (gethash (or name @name) (types vk-spec))
              :define))
    (when @name
      (assert (string= @name "VK_DEFINE_NON_DISPATCHABLE_HANDLE")
              (@name)
              "Line ~a: unknown category=define name <~a>" xml-line @name)
      (setf name @name)
      (setf is-value-p nil)
      (setf args (xps node)))    
    (assert (not (gethash name (defines vk-spec)))
            ()
            "Line ~a: define <~a> has already been specified" xml-line name)
    (setf (gethash name (defines vk-spec))
          (make-instance 'define
                         :name name
                         :lisp-name "TODO: fix me"
                         :xml-line xml-line
                         :is-value-p is-value-p
                         :is-struct-p is-struct-p
                         :requires requires
                         :calls type
                         :args args))))

(defun parse-enum-type (node vk-spec)
  "TODO"
  (let ((xml-line (xpath:evaluate "position()" node))
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
    (setf (gethash name (types vk-spec))
          :enum)))

(defun str-not-empty-p (str)
  (and str (> (length str) 0)))

(defun parse-funcpointer (node vk-spec)
  "TODO"
  (let ((xml-line (xpath:evaluate "position()" node))
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
    (setf (gethash name (types vk-spec))
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
  (let ((xml-line (xpath:evaluate "position()" node))
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
  (let ((xml-line (xpath:evaluate "position()" node))
        (name (xps (xpath:evaluate "@name" node))))
    (assert (not (find name (includes vk-spec)))
            (name)
            "Line ~a: include named <~a> already specified" xml-line name)
    (push name (includes vk-spec))))

(defun determine-sub-struct (structure vk-spec)
  "TODO"
  (loop for other-struct being each hash-values of (structures vk-spec)
        when (and (string= (name structure) (name other-struct))
                      (< (length (member-values other-struct))
                         (length (member-values structure)))
                      (not (string= (first (member-values other-struct))
                                    "sType"))
                      (every (lambda (m1 m2)
                               (and (string= (type-name m1)
                                             (type-name m2))
                                    (string= (name m1)
                                             (name m2))))
                             (member-values other-struct)
                             (subseq (member-values structure)
                                     0 (length (member-values other-struct)))))
        return (name other-struct)))

(defparameter *ignore-lens*
  '("null-terminated"
    "latexmath:[\\lceil{\\mathit{rasterizationSamples} \\over 32}\\rceil]"
    "2*VK_UUID_SIZE"
    "2*ename:VK_UUID_SIZE")
  "A list of <len> attributes in <member> tags.")

(defun parse-struct-member (node structure vk-spec)
  "TODO"
  (let* ((xml-line (xpath:evaluate "position()" node))
         (name-data (parse-name-data node))
         (type-info (parse-type-info node))
         (enum (xps (xpath:evaluate "enum" node)))
         (len (xps (xpath:evaluate "@len" node)))
         (no-autovalidity-p (parse-boolean (xpath:evaluate "@noautovalidity" node)))
         (optional-p (parse-boolean (xpath:evaluate "@optional" node)))
         (selection (xps (xpath:evaluate "@selection" node)))
         (selector (xps (xpath:evaluate "@selector" node)))
         (member-values (split-sequence:split-sequence #\, (xps (xpath:evaluate "@values" node))))
         (comment (xps (xpath:evaluate "comment" node)))
         (member-data (make-instance 'member-data
                                     :name (name name-data)
                                     :lisp-name "TODO: fix me"
                                     :xml-line xml-line
                                     :comment comment
                                     :array-sizes (array-sizes name-data)
                                     :bit-count (bit-count name-data)
                                     :type-info type-info
                                     :no-autovalidity-p no-autovalidity-p
                                     :optional-p optional-p
                                     :selection selection
                                     :selector selector
                                     :member-values member-values)))
    (assert (not (find-if (lambda (m) (string= (name member-data) (name m)))
                          (member-values structure)))
            (member-data)
            "Line ~a: structure member name <~a> already used" xml-line (name member-data))
    (when enum
      ;; this is fucked up: enum/preceding-sibling::text() is always NIL, so let's hope that <name> always comes before <enum>...
      (let ((enum-prefix (xps (xpath:evaluate "name/following-sibling::text()" node)))
            (enum-postfix (xps (xpath:evaluate "enum/following-sibling::text()" node))))
        (assert (and enum-prefix (string= enum-prefix "[")
                     enum-postfix (string= enum-postfix "]"))
                ()
                "Line ~a: structure member array specification is ill-formatted: <~a>" xml-line enum)
        (push enum (array-sizes member-data))))
    (when len
      (setf (len member-data) (split-sequence:split-sequence #\, len))
      (assert (<= (length (len member-data)) 2)
              ()
              "Line ~a: member attribute <len> holds unknown number of data: ~a" xml-line (length (len member-data)))
      (let* ((first-len (first (len member-data)))
             (len-member (find-if (lambda (m) (string= first-len (name m)))
                                  (member-values structure))))
        (assert (or len-member
                    (find first-len *ignore-lens* :test #'string=)
                    (string= first-len "latexmath:[\\textrm{codeSize} \\over 4]"))
                ()
                "Line ~a: member attribute <len> holds unknown value <~a>" xml-line first-len)
        (when len-member
          (assert (and (not (prefix (type-info len-member)))
                       (not (postfix (type-info len-member))))
                  ()
                  "Line ~a: member attribute <len> references a member of unexpected type <~a>" xml-line (type-info len-member)))
        (when (< 1 (length (len member-data)))
          (assert (find (second (len member-data)) '("1" "null-terminated") :test #'string=)
                  ()
                  "Line ~a: member attribute <len> holds unknown second value <~a>" xml-line (second (len member-data))))))
    (when selection
      (assert (is-union-p structure)
              ()
              "Line ~a: attribute <selection> is used with non-union structure." xml-line))
    (when selector
      (let ((member-selector (find-if (lambda (m) (string= selector (name m)))
                                      (member-values structure))))
        (assert member-selector
                ()
                "Line ~a: member attribute <selector> holds unknown value <~a>" xml-line selector)
        (assert (gethash (type-name (type-info member-selector)) (enums vk-spec))
                ()
                "Line ~a: member attribute references unknown enum type <~a>" xml-line (type-name (type-info member-selector)))))
    member-data))

(defun parse-struct (node vk-spec)
  "TODO"
  (let ((xml-line (xpath:evaluate "position()" node))
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
        (let ((allow-duplicate-p (parse-boolean (xpath:evaluate "@allowduplicate" node)))
              (is-union-p (string= (xps (xpath:evaluate "@category" node)) "union"))
              (returned-only-p (parse-boolean (xpath:evaluate "@returnedonly" node)))
              (struct-extends (split-sequence:split-sequence #\, (xps (xpath:evaluate "@structextends" node)))))
          (assert name
                  (name)
                  "Line ~a: struct has no name" xml-line)
          ;; todo: this should be an assert in a future version
          (when (and allow-duplicate-p
                     (> (length struct-extends) 0))
            (warn "Line ~a: attribute <allowduplicate> is true, but no structures are listed in <structextends>" xml-line))
          (assert (not (gethash name (structures vk-spec)))
                  (name)
                  "Line ~a: struct <~a> already specified" xml-line name)
          (setf (gethash name (structures vk-spec))
                (make-instance 'struct
                               :name name
                               :lisp-name "TODO: fix me"
                               :xml-line xml-line
                               :struct-extends struct-extends
                               :allow-duplicate-p allow-duplicate-p
                               :returned-only-p returned-only-p
                               :is-union-p is-union-p))
          (xpath:do-node-set (member-node (xpath:evaluate "member" node))
            (push (parse-struct-member member-node
                                       (gethash name (structures vk-spec))
                                       vk-spec)
                  (member-values (gethash name (structures vk-spec)))))
          (setf (sub-struct (gethash name (structures vk-spec)))
                (determine-sub-struct (gethash name (structures vk-spec))
                                      vk-spec))
          (setf (extended-structs vk-spec)
                (remove-duplicates
                 (append struct-extends (extended-structs vk-spec))
                 :test #'string=))
          (assert (not (gethash name (types vk-spec)))
                  (name)
                  "Line ~a: struct <~a> already specified as a type" xml-line name)
          (setf (gethash name (types vk-spec))
                (if is-union-p
                    :union
                    :struct))))))


(defun parse-requires (node vk-spec)
  "TODO"
  (let ((xml-line (xpath:evaluate "position()" node))
        (name (xps (xpath:evaluate "@name" node)))
        (requires (xps (xpath:evaluate "@requires" node))))
    (assert (not (gethash name (types vk-spec)))
            ()
            "Line ~a: type <~a> already specified as a type" xml-line name)
    (if requires
        (progn
          (assert (find requires (includes vk-spec) :test #'string=)
                  ()
                  "Line ~a: type requires unknown include <~a>" xml-line requires)
          (setf (gethash name (types vk-spec))
                :requires))
        (progn
          (assert (string= name "int")
                  ()
                  "Line ~a: unknown type" xml-line)
          (setf (gethash name (types vk-spec))
                :unknown)))))

(defun parse-types (vk.xml vk-spec)
  "TODO"
  (xpath:do-node-set (node (xpath:evaluate "/registry/types/type[(@category=\"include\")]" vk.xml))
    (format t "parsing include~%")
    (parse-type-include node vk-spec))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types/type[not(@category)]" vk.xml))
    (format t "parsing requires~%")
    (parse-requires node vk-spec))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types/type[@category=\"basetype\"]" vk.xml))
    (format t "parsing basetype~%")
    (parse-basetype node vk-spec))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types/type[@category=\"bitmask\"]" vk.xml))
    (format t "parsing bitmask~%")
    (parse-bitmask node vk-spec))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types/type[@category=\"define\"]" vk.xml))
    (format t "parsing define~%")
    (parse-define node vk-spec))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types/type[@category=\"enum\"]" vk.xml))
    (format t "parsing enum~%")
    (parse-enum-type node vk-spec))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types/type[@category=\"handle\"]" vk.xml))
    (format t "parsing handle~%")
    (parse-handle node vk-spec))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types/type[@category=\"struct\"]" vk.xml))
    (format t "parsing struct~%")
    (parse-struct node vk-spec))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types/type[@category=\"union\"]" vk.xml))
    (format t "parsing union~%")
    (parse-struct node vk-spec))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types/type[@category=\"funcpointer\"]" vk.xml))
    (format t "parsing funcpointer~%")
    (parse-funcpointer node vk-spec)))

(defun parse-vk-xml (version vk-xml-pathname)
  "Parses the vk.xml file at VK-XML-PATHNAME into a VK-SPEC instance."
  (let* ((vk.xml (cxml:parse-file vk-xml-pathname
                                  (cxml:make-whitespace-normalizer
                                   (stp:make-builder))))
         (vk-spec (make-instance 'vulkan-spec)))
    (parse-types vk.xml vk-spec)
    vk-spec))
