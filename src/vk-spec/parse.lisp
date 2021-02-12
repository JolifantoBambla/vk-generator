#|
 Copyright(c) 2020 - Lukas Herzberger <herzberger.lukas@gmail.com>
 SPDX-License-Identifier: MIT

 Copyright(c) 2015-2020 - NVIDIA CORPORATION
 SPDX-License-Identifier: Apache-2.0
|#

(in-package :vk-generator/vk-spec)

(defun tokenize (str)
  "Splits a comma-separated string."
  (let ((tokenized (split-sequence:split-sequence #\, str)))
    (if (member nil tokenized)
        nil
        tokenized)))

(defun parse-boolean (node)
  "Checks whether or not the given node NODE holds a string that equals 'true'."
  (let ((str (xps node)))
    (and str (string= str "true"))))

(defun parse-modifiers (modifiers)
  "Parses a string of modifiers for a name node into a list of ARRAY-SIZES and a list of BIT-COUNT values.

Known formats are:
- bit count: \":<number>\"
E.g.:
<member>
  <type>uint32_t</type>
  <name>mask</name>
  :8
</member>

- array size(s): \"[<number>]+\"
E.g one-dimensional array size:
<param>
  const
  <type>float</type>
  <name>blendConstants</name>
  [4]
</param>

E.g. multidimensional array sizes:
<member>
  <type>float</type>
  <name>matrix</name>
  [3][4]
</member>

Ignored formats are:
- \")\"
- \";\"
- array size of a struct member referenced by a constant: \"[\"
E.g.:
<member>
  <type>char</type>
  <name>deviceName</name>
  [
  <enum>VK_MAX_PHYSICAL_DEVICE_NAME_SIZE</enum>
  ]
</member>

Note:array sizes of struct members are handled by PARSE-STRUCT-MEMBER

See also:
- PARSE-NAME-DATA
- NAME-DATA
- PARSE-STRUCT-MEMBER
- VULKAN-SPEC
"
  (let ((array-sizes nil)
        (bit-count nil)
        (value modifiers))
    (when (and value
               (> (length value) 0))
      (let ((first-char (subseq value 0 1)))
        (cond
          ((and (not (string= value "["))
                 (string= first-char "["))
           (let ((end-pos 0))
             (loop while (not (= (1+ end-pos) (length value)))
                   do (let ((start-pos (position #\[ value :start end-pos)))
                        (assert start-pos
                                () "could not find '[' in <~a>" value)
                        (setf end-pos (position #\] value :start start-pos))
                        (assert end-pos
                                () "could not find ']' in <~a>" value)
                        (assert (<= (+ start-pos 2) end-pos)
                                () "missing content between '[' and ']' in <~a>" value)                       
                        (push (subseq value (1+ start-pos) end-pos)
                              array-sizes)))))
          ((string= first-char ":")
           (setf bit-count (subseq value 1)))
          (t
           (assert (or (string= first-char "[")
                       (string= first-char ";")
                       (string= first-char ")"))
                   () "unknown modifier <~a>" value)))))
    (values array-sizes bit-count)))

(defun parse-name-data (node)
  "Parses name data from a node into a NAME-DATA instance.

E.g.:
<param>
  const
  <type>float</type>
  <name>blendConstants</name>
  [4]
</param>

See also:
- PARSE-MODIFIERS
- NAME-DATA
- VULKAN-SPEC
"
  ;; todo: check attributes
  (let ((name (xps (xpath:evaluate "name" node))))
    (multiple-value-bind (array-sizes bit-count)
        (parse-modifiers (xps (xpath:evaluate "name/following-sibling::text()" node)))
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
                           :type-name (type-name type-info))))
    (assert (not (gethash (name name-data) (types vk-spec)))
            () "basetype <~a> already specified as a type" (name name-data))
    (setf (gethash (name name-data) (types vk-spec))
          (make-instance 'vk-type
                         :name (name name-data)
                         :category :basetype))))

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
          (setf (gethash name (types vk-spec))
                (make-instance 'vk-type
                               :name name
                               :category :bitmask)))
        (let ((name-data (parse-name-data node))
              (type-info (parse-type-info node))
              (requires (xps (xpath:evaluate "@requires" node))))
          (assert (alexandria:starts-with-subseq "Vk" (name name-data))
                  () "name <~a> does not begin with <VK>" (name name-data))
          (assert (= (length (array-sizes name-data)) 0)
                  () "name <~a> with unsupported array-sizes" (array-sizes name-data))
          (unless (find (type-name type-info) '("VkFlags" "VkFlags64") :test #'string=)
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
                               :type-name (type-name type-info)
                               :requires requires))
          (assert (not (gethash (name name-data) (types vk-spec)))
                  () "bitmask <~a> already specified as a type" (name name-data))
          (setf (gethash (name name-data) (types vk-spec))
                (make-instance 'vk-type
                               :name (name name-data)
                               :category :bitmask))))))

(defun parse-define (node vk-spec)
  "TODO"
  (let* ((name (xps (xpath:evaluate "name" node)))
         (@name (xps (xpath:evaluate "@name" node)))
         (type (xps (xpath:evaluate "type" node)))
         (requires (xps (xpath:evaluate "@requires" node)))
         (args (xps (xpath:evaluate (cond
                                      (type "type/following-sibling::text()")
                                      (name "name/following-sibling::text()")
                                      (@name "text()")
                                      (t (error "unknown define args path for define")))
                                    node)))
         (is-value-p (alexandria:starts-with-subseq "(" args))
         (is-struct-p (search "struct" (xps node))))
    (when is-struct-p
        (assert (not (gethash name (types vk-spec)))
                () "type <~a> has already been specified" name)
        (setf (gethash (or name @name) (types vk-spec))
              (make-instance 'vk-type
                             :name (or name @name)
                             :category :define)))
    (when @name
      (assert (string= @name "VK_DEFINE_NON_DISPATCHABLE_HANDLE")
              () "unknown category=define name <~a>" @name)
      (setf name @name)
      (setf is-value-p nil)
      (setf args (xps node)))    
    (assert (not (gethash name (defines vk-spec)))
            () "define <~a> has already been specified" name)
    (setf (gethash name (defines vk-spec))
          (make-instance 'define
                         :name name
                         :is-value-p is-value-p
                         :is-struct-p is-struct-p
                         :requires requires
                         :calls type
                         :args args))))

(defun parse-enum-type (node vk-spec)
  "TODO"
  (let ((alias (xps (xpath:evaluate "@alias" node)))
        (name (xps (xpath:evaluate "@name" node))))
    (if alias
        (progn
          (assert (> (length alias) 0)
                  () "enum with empty alias")
          (let ((enum (gethash alias (enums vk-spec))))
            (assert enum
                    () "enum with unknown alias <~a>" alias)
            (assert (= (length (alias enum)) 0)
                    () "enum <~a> already has an alias <~a>" (name enum) (alias enum))
            (setf (alias enum) alias)))
        (progn
          (assert (not (gethash name (enums vk-spec)))
                  () "enum <~a> already specified" name)
          (setf (gethash name (enums vk-spec))
                (make-instance 'enum
                               :name name
                               :alias alias))))
    (assert (not (gethash name (types vk-spec)))
            () "enum <~a> already specified as a type" name)
    (setf (gethash name (types vk-spec))
          (make-instance 'vk-type
                         :name name
                         :category :enum))))

(defun str-not-empty-p (str)
  (and str (> (length str) 0)))

(defun parse-funcpointer (node vk-spec)
  "TODO"
  (let ((requirements (xps (xpath:evaluate "@requires" node)))
        (name (xps (xpath:evaluate "name" node))))
    (assert (str-not-empty-p name)
            () "funcpointer with empty name")
    (assert (not (gethash name (func-pointers vk-spec)))
            () "funcpointer <~a> already specified" name)
    (setf (gethash name (func-pointers vk-spec))
          (make-instance 'func-pointer
                         :name name
                         :requirements requirements))
    (assert (not (gethash name (types vk-spec)))
            () "funcpointer <~a> already specified as a type" name)
    (setf (gethash name (types vk-spec))
          (make-instance 'vk-type
                         :name name
                         :category :funcpointer))
    (let* ((types (mapcar 'xps (xpath:all-nodes (xpath:evaluate "type" node)))))
      (loop for type in types
            do (progn
                 (assert (str-not-empty-p type)
                         () "funcpointer argument with empty type")
                 (assert (or (gethash type (types vk-spec))
                             (string= type requirements))
                         () "funcpointer argument of unknown type <~a>" type))))))

(defun parse-handle (node vk-spec)
  "TODO"
  (let ((alias (xps (xpath:evaluate "@alias" node))))
    (if alias
        (let ((handle (gethash alias (handles vk-spec)))
              (name (xps (xpath:evaluate "@name" node))))
          (assert handle
                  () "using unspecified alias <~a>" alias)
          (assert (not (alias handle))
                  () "handle <~a> already has an alias <~a>" (name handle) (alias name))
          (setf (alias handle) name)
          (assert (not (gethash name (types vk-spec)))
                  () "handle alias <~a> already specified as a type" name)
          (setf (gethash name (types vk-spec))
                (make-instance 'vk-type
                               :name name
                               :category :handle)))
        (let ((parent (xps (xpath:evaluate "@parent" node)))
              (name-data (parse-name-data node))
              (type-info (parse-type-info node)))
          (assert (alexandria:starts-with-subseq "Vk" (name name-data))
                  () "name <~a> does not begin with <Vk>" (name name-data))
          (assert (= (length (array-sizes name-data)) 0)
                  () "name <~a> with unsupported array-sizes" (name name-data))
          (assert (= (length (bit-count name-data)) 0)
                  () "name <~a> with unsupported bit-count <~a>" (name name-data) (bit-count name-data))
          (assert (or (string= (type-name type-info) "VK_DEFINE_HANDLE")
                      (string= (type-name type-info) "VK_DEFINE_NON_DISPATCHABLE_HANDLE"))
                  () "handle with invalid type <~a>" (type-name type-info))
          (assert (= (length (prefix type-info)) 0)
                  () "unexpected type prefix <~a>" (prefix type-info))
          (assert (string= (postfix type-info) "(")
                  () "unexpected type postfix <~a>" (postfix type-info))
          (assert (not (gethash (name name-data) (handles vk-spec)))
                  () "handle <~a> already specified" (name name-data))
          (setf (gethash (name name-data) (handles vk-spec))
                (make-instance 'handle
                               :name (name name-data)
                               :parents (tokenize parent)
                               :non-dispatch-handle-p (string= (type-name type-info) "VK_DEFINE_NON_DISPATCHABLE_HANDLE")))
          (assert (not (gethash (name name-data) (types vk-spec)))
                  () "handle <~a> already specified as a type" (name name-data))
          (setf (gethash (name name-data) (types vk-spec))
                (make-instance 'vk-type
                               :name (name name-data)
                               :category :handle))))))

(defun parse-type-include (node vk-spec)
  "TODO"
  (let ((name (xps (xpath:evaluate "@name" node))))
    (assert (not (find name (includes vk-spec)))
            () "include named <~a> already specified" name)
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
  (let* ((name-data (parse-name-data node))
         (type-info (parse-type-info node))
         (enum (xps (xpath:evaluate "enum" node)))
         (len (xps (xpath:evaluate "@len" node)))
         (no-autovalidity-p (parse-boolean (xpath:evaluate "@noautovalidity" node)))
         (optional-p (parse-boolean (xpath:evaluate "@optional" node)))
         (selection (xps (xpath:evaluate "@selection" node)))
         (selector (xps (xpath:evaluate "@selector" node)))
         (member-values (tokenize (xps (xpath:evaluate "@values" node))))
         (comment (xps (xpath:evaluate "comment" node)))
         (member-data (make-instance 'member-data
                                     :name (name name-data)
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
            () "structure member name <~a> already used" (name member-data))
    (when enum
      ;; this is fucked up: enum/preceding-sibling::text() is always NIL, so let's hope that <name> always comes before <enum>...
      (let ((enum-prefix (xps (xpath:evaluate "name/following-sibling::text()" node)))
            (enum-postfix (xps (xpath:evaluate "enum/following-sibling::text()" node))))
        (assert (and enum-prefix (string= enum-prefix "[")
                     enum-postfix (string= enum-postfix "]"))
                () "structure member array specification is ill-formatted: <~a>" enum)
        (push enum (array-sizes member-data))))
    (when len
      (setf (len member-data) (tokenize len))
      (assert (<= (length (len member-data)) 2)
              () "member attribute <len> holds unknown number of data: ~a" (length (len member-data)))
      (let* ((first-len (first (len member-data)))
             (len-member (find-if (lambda (m) (string= first-len (name m)))
                                  (member-values structure))))
        (assert (or len-member
                    (find first-len *ignore-lens* :test #'string=)
                    (string= first-len "latexmath:[\\textrm{codeSize} \\over 4]"))
                () "member attribute <len> holds unknown value <~a>" first-len)
        (when len-member
          (assert (and (not (prefix (type-info len-member)))
                       (not (postfix (type-info len-member))))
                  () "member attribute <len> references a member of unexpected type <~a>" (type-info len-member)))
        (when (< 1 (length (len member-data)))
          (assert (find (second (len member-data)) '("1" "null-terminated") :test #'string=)
                  () "member attribute <len> holds unknown second value <~a>" (second (len member-data))))))
    (when selection
      (assert (is-union-p structure)
              () "attribute <selection> is used with non-union structure."))
    (when selector
      (let ((member-selector (find-if (lambda (m) (string= selector (name m)))
                                      (member-values structure))))
        (assert member-selector
                () "member attribute <selector> holds unknown value <~a>" selector)
        (assert (gethash (type-name (type-info member-selector)) (enums vk-spec))
                () "member attribute references unknown enum type <~a>" (type-name (type-info member-selector)))))
    member-data))

(defun parse-struct (node vk-spec)
  "TODO"
  (let ((alias (xps (xpath:evaluate "@alias" node)))
        (name (xps (xpath:evaluate "@name" node))))
    (if alias
        (let ((struct (gethash alias (structures vk-spec))))
          (assert struct
                  () "missing alias <~a>" alias)
          (assert (not (find name (aliases struct)))
                  () "struct <~a> already uses alias <~a>" alias name)
          (push name (aliases struct))
          (assert (not (gethash name (structure-aliases vk-spec)))
                  () "structure alias <~a> already used" name)
          (setf (gethash name (structure-aliases vk-spec))
                alias)
          (assert (not (gethash name (types vk-spec)))
                  () "struct <~a> already specified as a type" name)
          (setf (gethash name (types vk-spec))
                (make-instance 'vk-type
                               :name name
                               :category :struct)))
        (let ((allow-duplicate-p (parse-boolean (xpath:evaluate "@allowduplicate" node)))
              (is-union-p (string= (xps (xpath:evaluate "@category" node)) "union"))
              (returned-only-p (parse-boolean (xpath:evaluate "@returnedonly" node)))
              (struct-extends (tokenize (xps (xpath:evaluate "@structextends" node)))))
          (assert name
                  () "struct has no name")
          ;; todo: this should be an assert in a future version
          (when (and allow-duplicate-p
                     (> (length struct-extends) 0))
            (warn "attribute <allowduplicate> is true, but no structures are listed in <structextends>"))
          (assert (not (gethash name (structures vk-spec)))
                  () "struct <~a> already specified" name)
          (setf (gethash name (structures vk-spec))
                (make-instance 'struct
                               :name name
                               :struct-extends struct-extends
                               :allow-duplicate-p allow-duplicate-p
                               :returned-only-p returned-only-p
                               :is-union-p is-union-p))
          (xpath:do-node-set (member-node (xpath:evaluate "member" node))
            (push (parse-struct-member member-node
                                       (gethash name (structures vk-spec))
                                       vk-spec)
                  (member-values (gethash name (structures vk-spec)))))
          (setf (member-values (gethash name (structures vk-spec)))
                (reverse (member-values (gethash name (structures vk-spec)))))
          (setf (sub-struct (gethash name (structures vk-spec)))
                (determine-sub-struct (gethash name (structures vk-spec))
                                      vk-spec))
          (setf (extended-structs vk-spec)
                (remove-duplicates
                 (append struct-extends (extended-structs vk-spec))
                 :test #'string=))
          (assert (not (gethash name (types vk-spec)))
                  () "struct <~a> already specified as a type" name)
          (setf (gethash name (types vk-spec))
                (make-instance 'vk-type
                               :name name
                               :category (if is-union-p
                                             :union
                                             :struct)))))))


(defun parse-requires (node vk-spec)
  "TODO"
  (let ((name (xps (xpath:evaluate "@name" node)))
        (requires (xps (xpath:evaluate "@requires" node))))
    (assert (not (gethash name (types vk-spec)))
            () "type <~a> already specified as a type" name)
    (if requires
        (progn
          (assert (find requires (includes vk-spec) :test #'string=)
                  () "type requires unknown include <~a>" requires)
          (setf (gethash name (types vk-spec))
                (make-instance 'vk-type
                               :name name
                               :category :requires)))
        (progn
          (assert (string= name "int")
                  () "unknown type")
          (setf (gethash name (types vk-spec))
                (make-instance 'vk-type
                               :name name
                               :category :unknown))))))

(defun parse-types (vk.xml vk-spec)
  "TODO"
  (xpath:do-node-set (node (xpath:evaluate "/registry/types/type[(@category=\"include\")]" vk.xml))
    (parse-type-include node vk-spec))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types/type[not(@category)]" vk.xml))
    (parse-requires node vk-spec))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types/type[@category=\"basetype\"]" vk.xml))
    (parse-basetype node vk-spec))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types/type[@category=\"bitmask\"]" vk.xml))
    (parse-bitmask node vk-spec))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types/type[@category=\"define\"]" vk.xml))
    (parse-define node vk-spec))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types/type[@category=\"enum\"]" vk.xml))
    (parse-enum-type node vk-spec))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types/type[@category=\"handle\"]" vk.xml))
    (parse-handle node vk-spec))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types/type[@category=\"struct\" or @category=\"union\"]" vk.xml))
    (parse-struct node vk-spec))
  (xpath:do-node-set (node (xpath:evaluate "/registry/types/type[@category=\"funcpointer\"]" vk.xml))
    (parse-funcpointer node vk-spec)))

(defun parse-enum-constant (node vk-spec)
  "TODO"
  (let ((name (xps (xpath:evaluate "@name" node)))
        (alias (xps (xpath:evaluate "@alias" node)))
        (comment (xps (xpath:evaluate "@comment" node))))
    (if alias
        (let ((constant (gethash alias (constants vk-spec))))
          (assert constant
                  () "unknown enum constant alias <~a>" alias)
          (setf (gethash name (constants vk-spec))
                (make-instance 'api-constant
                               :name name
                               :alias alias
                               :comment comment
                               :number-value (number-value constant)
                               :string-value (string-value constant)
                               :vk-hpp-name (vk-hpp-name constant)
                               :single-bit-p (single-bit-p constant))))
        (let* ((string-value (xps (xpath:evaluate "@value" node)))
               (number-value (numeric-value string-value)))
          (assert (not (gethash name (constants vk-spec)))
                  () "already specified enum constant <~a>" name)
          (assert number-value
                  () "non-alias enum constant <~a> has no value" name)
          (setf (gethash name (constants vk-spec))
                (make-instance 'api-constant
                               :name name
                               :comment comment
                               :number-value number-value
                               :string-value string-value))))))

(defun to-upper-snake (str)
  "Transforms a given string to a snake-cased string, but all characters are uppercased.

E.g.: \"VkResult\" becomes \"VK_RESULT\". 
"
  (string-upcase (kebab:to-snake-case str)))

(defun is-vk-result (str)
  "Checks if a string is equal to \"VkResult\"."
  (string= str "VkResult"))

(defun get-enum-prefix (name is-bitmaks-p)
  "TODO"
  (cond
    ((is-vk-result name) "VK_")
    (is-bitmaks-p
     (let ((flag-bits-pos (search "FlagBits" name)))
       (assert flag-bits-pos
               () "bitmask <~a> does not contain <FlagBits> as substring")
       (concatenate 'string (to-upper-snake (subseq name 0 flag-bits-pos)) "_")))
    (t
     (concatenate 'string (to-upper-snake name) "_"))))

(defun get-enum-pre-and-postfix (name is-bitmask-p tags)
  (let ((prefix (get-enum-prefix name is-bitmask-p))
        (postfix ""))
    (unless (is-vk-result name)
      (let ((tag (find-if (lambda (tag)
                            (or (alexandria:ends-with-subseq (concatenate 'string tag "_") prefix)
                                (alexandria:ends-with-subseq tag name)))
                          tags)))
        (when tag
          (when (alexandria:ends-with-subseq (concatenate 'string tag "_") prefix)
            (setf prefix (subseq prefix 0 (- (length prefix) (length tag) 1))))
          (setf postfix (concatenate 'string "_" tag)))))
    (values prefix postfix)))

(defun find-tag (tags name postfix)
  "TODO"
  (or
   (find-if (lambda (tag)
              (alexandria:ends-with-subseq
               (concatenate 'string tag postfix)
               name))
            tags)
   ""))

(defun strip-prefix (str prefix)
  (if (and prefix
           (> (length prefix) 0)
           (alexandria:starts-with-subseq prefix str))
      (subseq str (length prefix))
      str))

(defun strip-postfix (str postfix)
  (if (and postfix
           (> (length postfix) 0)
           (alexandria:ends-with-subseq postfix str))
      (subseq str 0 (search postfix str))
      str))

(defun upper-snake-to-pascal-case (str)
  "Transforms a string in uppercased snake-case to a pascal-cased string.

E.g. \"VK_RESULT\" becomes \"VkResult\".
"
  (kebab:to-pascal-case (string-downcase str)))

(defun create-enum-vk-hpp-name (name prefix postfix is-bitmask-p tag)
  "TODO"
  (let ((result (concatenate
                 'string
                 "e"
                 (upper-snake-to-pascal-case
                  (strip-postfix (strip-prefix name prefix) postfix)))))
    (when is-bitmask-p
      (setf result (subseq result 0 (search "Bit" result))))
    (when (and (> (length tag) 0)
               (string= (upper-snake-to-pascal-case tag)
                        (subseq result 0 (- (length result) (length tag)))))
      (setf result (subseq result 0 (- (length result) (length tag)))))
    result))

(defun add-enum-alias (enum name alias-name vk-hpp-name)
  "TODO"
  (assert (or (find-if (lambda (v)
                           (string= alias-name (name v)))
                         (enum-values enum))
                (gethash alias-name (aliases enum)))
            () "unknown enum alias <~a>" alias-name)
  (let ((alias (gethash name (aliases enum))))
    (assert (or (not alias)
                (string= (first alias) alias-name))
            () "enum alias <~a> already listed for a different enum value" alias-name))
  ;; only list aliases that map to different vk-hpp-names
  (unless (find-if (lambda (alias-data)
                     (string= vk-hpp-name (second alias-data)))
                   (alexandria:hash-table-values (aliases enum)))
    (setf (gethash name (aliases enum))
          (list alias-name vk-hpp-name))))

(defun parse-enum-value (node enum is-bitmask-p prefix postfix vk-spec)
  "Parses an enum value node into an ENUM-VALUE instance and stores it in the given ENUM instance.

A node could look like this:
<enum value=\"0\" name=\"VK_FRONT_FACE_COUNTER_CLOCKWISE\"/>

Which could be a child of the following enum node:
<enums name=\"VkFrontFace\" type=\"enum\">
  <enum value=\"0\" name=\"VK_FRONT_FACE_COUNTER_CLOCKWISE\"/>
  <enum value=\"1\" name=\"VK_FRONT_FACE_CLOCKWISE\"/>
</enums>

Note that the created ENUM-VALUE instance is appended at the front of the ENUM-VALUES slot of the given ENUM instance.
Depending on what you want to do with it you might want to reverse the order of the list after parsing all values of an enum like it is done in PARSE-ENUMS.

See also:
- PARSE-ENUMS
- ENUM-VALUE
- ENUM
- VULKAN-SPEC
"
  (let* ((name (xps (xpath:evaluate "@name" node)))
         (alias (xps (xpath:evaluate "@alias" node)))
         (tag (find-tag (tags vk-spec) name postfix))
         (vk-hpp-name
           (create-enum-vk-hpp-name name
                                    prefix
                                    postfix
                                    is-bitmask-p
                                    tag))
         (comment (xps (xpath:evaluate "@comment" node))))
    (if alias
        (add-enum-alias enum name alias vk-hpp-name)
        (let* ((bitpos-string (xps (xpath:evaluate "@bitpos" node)))
               (value-string (xps (xpath:evaluate "@value" node)))
               (bitpos (numeric-value bitpos-string))
               (value (numeric-value value-string))
               (enum-value (find-if (lambda (e)
                                      (string= (vk-hpp-name e) vk-hpp-name))
                                    (enum-values enum))))
          (assert (or (and (not bitpos) value)
                      (and bitpos (not value)))
                  () "invalid set of attributes for enum <~a> name")
          (if enum-value
              (assert (string= (name enum-value) name)
                      () "enum value <~a> maps to same vk-hpp-name as <~a>" name (name enum-value))
              (push (make-instance 'enum-value
                                   :name name
                                   :comment comment
                                   :number-value (or value (ash 1 bitpos))
                                   :string-value (or value-string bitpos-string)
                                   :vk-hpp-name vk-hpp-name
                                   :single-bit-p (not value))
                    (enum-values enum)))))))

(defun parse-enums (vk.xml vk-spec)
  "Parses an enum node into an ENUM instance and stores it in the given VULKAN-SPEC instance.

A node could look like this:
<enums name=\"VkFrontFace\" type=\"enum\">
  <enum value=\"0\" name=\"VK_FRONT_FACE_COUNTER_CLOCKWISE\"/>
  <enum value=\"1\" name=\"VK_FRONT_FACE_CLOCKWISE\"/>
</enums>

The enum's child nodes are parsed into ENUM-VALUE instances in the process.

See also:
- PARSE-ENUM-VALUE
- ENUM
- ENUM-VALUE
- VULKAN-SPEC
"
  (xpath:do-node-set (node (xpath:evaluate "/registry/enums[@name=\"API Constants\"]/enum" vk.xml))
    (parse-enum-constant node vk-spec))
  (xpath:do-node-set (node (xpath:evaluate "/registry/enums[not(@name=\"API Constants\")]" vk.xml))
    (let* ((name (xps (xpath:evaluate "@name" node)))
           (type (xps (xpath:evaluate "@type" node)))
           (comment (xps (xpath:evaluate "@comment" node)))
           (is-bitmask-p (string= type "bitmask"))
           (enum (gethash name (enums vk-spec))))
      (unless enum
        (warn "enum <~a> is not listed as enum in the types section" name)
        (setf (gethash name (enums vk-spec))
              (make-instance 'enum
                             :name name
                             :is-bitmask-p is-bitmask-p
                             :comment comment))
        (assert (not (gethash name (types vk-spec)))
                () "enum <~a> already specified as a type" name)
        (setf (gethash name (types vk-spec))
              (make-instance 'vk-type
                             :name name
                             :category :enum))
        (setf enum (gethash name (enums vk-spec))))
      (assert (not (enum-values enum))
              () "enum <~a> already holds values" name)
      (setf (is-bitmask-p enum) is-bitmask-p)
      (setf (comment enum) comment)
      (when is-bitmask-p
        (let ((bitmask (loop for b being each hash-values of (bitmasks vk-spec)
                             when (string= (requires b) name)
                             return b)))
          (unless bitmask
            (warn "enum <~a> is not listed as an requires for any bitmask in the types section" name)
            (let ((flag-bits-pos (search "FlagBits" name)))
              (assert flag-bits-pos
                      () "enum <~a> does not contain <FlagBits> as substring")
              (let* ((bitmask-name (concatenate 'string
                                                (subseq name 0 flag-bits-pos)
                                                "Flags"
                                                (subseq name (+ flag-bits-pos 8)))))
                (setf bitmask (gethash bitmask-name (bitmasks vk-spec)))
                (assert bitmask
                        () "enum <~a> has no corresponding bitmask <~a> listed in the types section" name bitmask-name)
                (setf (requires bitmask) name))))))
      (multiple-value-bind (prefix postfix) (get-enum-pre-and-postfix name is-bitmask-p (tags vk-spec))
        (xpath:do-node-set (enum-value-node (xpath:evaluate "enum" node))
          (parse-enum-value enum-value-node enum is-bitmask-p prefix postfix vk-spec)))
      (setf (enum-values enum)
            (remove-duplicates (reverse (enum-values enum)))))))


(defun parse-tags (vk.xml vk-spec)
  "TODO"
  (xpath:do-node-set (node (xpath:evaluate "/registry/tags/tag" vk.xml))
    (let ((name (xps (xpath:evaluate "@name" node))))
      (assert (not (find name (tags vk-spec)))
              () "tag named <~a> has already been specified")
      (push name (tags vk-spec)))))

(defun parse-command-name (node vk-spec)
  "TODO"
  (let ((name-data (parse-name-data (xpath:first-node (xpath:evaluate "proto" node)))))
    (assert (alexandria:starts-with-subseq "vk" (name name-data))
            () "name <~a> does not begin with <vk>" (name name-data))
    (assert (not (array-sizes name-data))
            () "name <~a> with unsupported array-sizes <~a>" (name name-data) (array-sizes name-data))
    (assert (not (bit-count name-data))
            () "name <~a> with unsupported bit-count <~a>" (name name-data) (bit-count name-data))
    (assert (not (gethash (name name-data) (commands vk-spec)))
            () "command <~a> already specified" (name name-data))
    (name name-data)))

(defun parse-command-return-type (node vk-spec)
  "TODO"
  (let ((type-info (parse-type-info (xpath:first-node (xpath:evaluate "proto" node)))))
    (assert (gethash (type-name type-info) (types vk-spec))
            () "unknown type <~a>" (type-name type-info))
    (assert (not (prefix type-info))
            () "unexpected type prefix <~a>" (prefix type-info))
    (assert (not (postfix type-info))
            () "unexpected type postfix <~a>" (postfix type-info))
    (type-name type-info)))

(defun is-param-indirect (name params vk-spec)
  (let ((delimiter-pos (or (search "->" name) (search "::" name))))
    (when (and delimiter-pos (< delimiter-pos (- (length name) 2)))
      (let* ((param-name (subseq name 0 delimiter-pos))
             (param-member (subseq name (+ delimiter-pos 2)))
             (param (find-if (lambda (p)
                               (string= (name p) param-name))
                             params)))
        (when param
          (let ((struct (gethash (type-name (type-info param)) (structures vk-spec))))
            (when struct
              (find-if (lambda (m)
                         (string= (name m) param-member))
                       (member-values struct)))))))))

(defun parse-command-param (node params vk-spec)
  "TODO"
  (let ((len (xps (xpath:evaluate "@len" node)))
        (optional-p (parse-boolean (xpath:evaluate "@optional" node)))
        (name-data (parse-name-data node))
        (type-info (parse-type-info node)))
    (assert (or (not len)
                (string= len "null-terminated")
                (find-if (lambda (p)
                           (string= (name p) len))
                         params)
                (is-param-indirect len params vk-spec))
            () "command param len <~a> is not recognized as a valid len value" len)
    (assert (not (bit-count name-data))
            () "name <~a> with unsupported bit-count <~a>" (name name-data) (bit-count name-data))
    (assert (gethash (type-name type-info) (types vk-spec))
            () "unknown type <~a>" (type-name type-info))
    (assert (or (not (prefix type-info))
                (string= (prefix type-info) "const")
                (string= (prefix type-info) "const struct")
                (string= (prefix type-info) "struct"))
            () "unexpected type prefix <~a>" (prefix type-info))
    (assert (or (not (postfix type-info))
                (string= (postfix type-info) "*")
                (string= (postfix type-info) "**")
                (string= (postfix type-info) "* const*"))
            () "unexpected type postfix <~a>" (postfix type-info))
    (assert (not (find-if (lambda (p)
                            (string= (name name-data) (name p)))
                          params))
            () "command param <~a> already used" (name name-data))
    (make-instance 'param
                   :name (name name-data)
                   :array-sizes (array-sizes name-data)
                   :type-info type-info
                   :len len
                   :optional-p optional-p)))

(defun register-deleter (command vk-spec)
  "TODO"
  (let* ((params (params command))
         (num-params (length params))
         (key nil)
         (value-index nil))
    (flet ((get-type-name (param)
             (type-name (type-info param))))
      (cond
        ((or (= 2 num-params)
             (= 3 num-params))
         (assert (string= (get-type-name (alexandria:lastcar params))
                          "VkAllocationCallbacks")
                 () "unexpected last paramer for deleter <~a>" (get-type-name (alexandria:lastcar params)))
         (if (= num-params 2)
             (setf key "")
             (setf key (get-type-name (first params))))
         (setf value-index (- num-params 2)))
        ((= num-params 4)
         (setf key (get-type-name (first params)))
         (setf value-index 3)
         (let ((handle (gethash (get-type-name (nth value-index params)) (handles vk-spec))))
           (assert (gethash (get-type-name (nth value-index params)) (handles vk-spec))
                   () "missing handle <~a>" (get-type-name (nth value-index params)))
           (setf (delete-pool handle)
                 (get-type-name (first params)))))
        (t (error "illegal number of params for deleter <~a>" num-params)))
      (let* ((value-name (get-type-name (nth value-index params)))
             (key-handle (gethash key (handles vk-spec)))
             (handle (gethash value-name (handles vk-spec))))
        (assert (and key-handle
                     (not (find value-name (children handle))))
                () "handle <~a> already specified as a child of <~a>" value-name key)
        (push value-name (children key-handle))
        (assert handle
                () "missing handle <~a>" value-name)
        (setf (delete-command handle) (name command))))))

(defun parse-commands (vk.xml vk-spec)
  "TODO"
  (xpath:do-node-set (node (xpath:evaluate "/registry/commands/command" vk.xml))
    (let ((alias (xps (xpath:evaluate "@alias" node))))
      (if alias
          (let ((name (xps (xpath:evaluate "@name" node)))
                (command (gethash alias (commands vk-spec))))
            (assert (alexandria:starts-with-subseq "vk" name)
                    () "name <~a> should begin with <vk>" name)
            (assert command
                    () "missing command <~a>" alias)
            (assert (not (gethash name (alias command)))
                    () "command <~a> already listed as alias to <~a>" name alias)
            (setf (gethash name (alias command))
                  (make-instance 'command-alias
                                 :name name)))
          (let* ((error-codes (tokenize (xps (xpath:evaluate "@errorcodes" node))))
                 (success-codes (tokenize (xps (xpath:evaluate "@successcodes" node))))
                 (name (parse-command-name node vk-spec))
                 (return-type (parse-command-return-type node vk-spec))
                 (command (make-instance 'command
                                         :name name
                                         :return-type return-type
                                         :success-codes success-codes
                                         :error-codes error-codes)))
            (xpath:do-node-set (param-node (xpath:evaluate "param" node))
              (push (parse-command-param param-node (params command) vk-spec)
                    (params command)))
            (setf (params command) (reverse (params command)))
            (when (or (string= (subseq name 2 9) "Destroy")
                      (string= (subseq name 2 6) "Free"))
              (register-deleter command vk-spec))
            (assert (> (length (params command)) 0)
                    () "command <~a> with no params" name)
            (let ((handle (gethash (name (first (params command))) (handles vk-spec))))
              (unless handle
                (setf handle (gethash "" (handles vk-spec))))
              (assert handle
                      () "could not find a handle to hold command <~a>" name)
              (setf (handle command) (name handle))
              (assert (not (gethash name (commands vk-spec)))
                      () "already encountered command <~a>" name)
              (setf (gethash name (commands vk-spec))
                    command)
              (assert (not (find name (commands handle) :test 'string=))
                      () "command list of handle <~a> already holds a command <~a>" (name handle) name)
              (push name (commands handle))))))))

(defun parse-require-enum (node tag vk-spec)
  "TODO"
  (let ((alias (xps (xpath:evaluate "@alias" node)))
        (name (xps (xpath:evaluate "@name" node)))
        (extends (xps (xpath:evaluate "@extends" node))))
    (if alias
        (let ((enum (gethash extends (enums vk-spec))))
          (assert enum
                  () "feature extends unknown enum <~a>" extends)
          (multiple-value-bind (prefix postfix) (get-enum-pre-and-postfix extends (is-bitmask-p enum) (tags vk-spec))
            (let ((vk-hpp-name (create-enum-vk-hpp-name name prefix postfix (is-bitmask-p enum) tag)))
              (when (alias enum)
                (multiple-value-bind (alias-prefix alias-postfix) (get-enum-pre-and-postfix (alias enum) (is-bitmask-p enum) (tags vk-spec))
                  (when (alexandria:ends-with-subseq postfix name)
                    (setf vk-hpp-name (create-enum-vk-hpp-name name alias-prefix alias-postfix (is-bitmask-p enum) tag)))))
              (add-enum-alias enum name alias vk-hpp-name))))
        (let ((value-string (xps (xpath:evaluate "@value" node))))
          (if extends
              (let* ((enum (gethash extends (enums vk-spec)))
                     (dir-string (xps (xpath:evaluate "@dir" node)))
                     (dir (if (and dir-string (string= dir-string "-"))
                              -1
                              1))
                     (bitpos-string (xps (xpath:evaluate "@bitpos" node)))
                     (bitpos (numeric-value bitpos-string))
                     (value (numeric-value value-string))
                     (offset-string (xps (xpath:evaluate "@offset" node)))
                     (offset (numeric-value offset-string))
                     (extension-number (numeric-value (and offset
                                                           (or (xps (xpath:evaluate "@extnumber" node))
                                                               (xps (xpath:evaluate "../../@number" node)))))))
                (assert enum
                        () "feature extends unknown enum <~a>" extends)
                (multiple-value-bind (prefix postfix) (get-enum-pre-and-postfix extends (is-bitmask-p enum) (tags vk-spec))
                  (assert (or (and bitpos-string (not value-string) (not offset-string))
                              (and (not bitpos-string) value-string (not offset-string))
                              (and (not bitpos-string) (not value-string) offset-string))
                          () "exactly one of bitpos = <~a>, offset = <~a>, and value = <~a> is supposed to be set" bitpos-string offset-string value-string)
                  (push (make-instance 'enum-value
                                       :name name
                                       :number-value (* dir
                                                        (or (and offset (+ +ext-base+
                                                                           (* +ext-block-size+ (1- extension-number))
                                                                           offset))
                                                            value
                                                            (ash 1 bitpos)))
                                       :string-value (or value-string bitpos-string offset-string)
                                       :vk-hpp-name (create-enum-vk-hpp-name name prefix postfix (is-bitmask-p enum) tag)
                                       :single-bit-p (not value))
                        (enum-values enum))))
              (unless value-string
                (assert (gethash name (constants vk-spec))
                        () "unknown required enum <~a>" name)))))))

(defun parse-extension-require-command (node extension-name vk-spec)
  "TODO"
  (let* ((name (xps (xpath:evaluate "@name" node)))
         (command (gethash name (commands vk-spec))))
    (if command
        (push (extensions command) extension-name)
        (progn
          (setf command (find-if (lambda (c)
                                   (gethash name (alias c)))
                                 (alexandria:hash-table-values (commands vk-spec))))
          (assert command
                  () "extension <~a> requires unknown command <~a>" extension-name name)
          (push (extensions (gethash name (alias command)))
                extension-name)))))

(defun get-platforms (extension-names vk-spec)
  "Returns a list of unique platform names for a given sequence of extension names."
  (remove-duplicates
   (loop for e in extension-names
         collect (platform (gethash e (extensions vk-spec))))))

(defun parse-extension-require-type (node extension-name vk-spec)
  "TODO"
  (let* ((name (xps (xpath:evaluate "@name" node)))
         (type (gethash name (types vk-spec))))
    (assert type
            () "failed to find required type <~a>" name)
    (push extension-name (extensions type))
    (assert (= (length (get-platforms (extensions type) vk-spec))
               1)
            () "type <~a> is protected by more than one platform" name)))

(defun parse-extensions (vk.xml vk-spec)
  "TODO"
  (xpath:do-node-set (node (xpath:evaluate "/registry/extensions/extension" vk.xml))
    (let ((name (xps (xpath:evaluate "@name" node)))
          (platform (xps (xpath:evaluate "@platform" node)))
          (deprecated-by (xps (xpath:evaluate "@deprecatedby" node)))
          (obsoleted-by (xps (xpath:evaluate "@obsoletedby" node)))
          (promoted-to (xps (xpath:evaluate "@promotedto" node)))
          (requirements
            (remove-duplicates
             (tokenize (xps (xpath:evaluate "@requires" node)))
             :test 'string=))
          (requires-core (xps (xpath:evaluate "@requiresCore" node)))
          (supported (xps (xpath:evaluate "@supported" node))))
      (assert (or (not platform)
                  (gethash platform (platforms vk-spec)))
              () "unknown platform <~a>" platform)
      (assert (or (not requires-core)
                  (find-if (lambda (f)
                             (string= f requires-core))
                           (alexandria:hash-table-values (features vk-spec))))
              () "unknown feature number <~a>" requires-core)
      (if (string= supported "disabled")
          (xpath:do-node-set (require-node (xpath:evaluate "require" node))
            ;; todo: remove disabled stuff
            (xpath:do-node-set (disable-node (xpath:evaluate "command" require-node))
              (let* ((command-name (xps (xpath:evaluate "@name" disable-node)))
                    (command (gethash command-name (commands vk-spec))))
                (assert command
                        () "try to remove unknown command <~a>" command-name)
                (remhash command-name (commands vk-spec))
                (let ((handle (gethash (handle command) (handles vk-spec))))
                  (assert handle
                          () "cannot find handle corresponding to command <~a>" command-name)
                  (remove command-name (commands handle) :test 'string=))))
            (xpath:do-node-set (disable-node (xpath:evaluate "enum" require-node))
              (let ((enum-name (xps (xpath:evaluate "@name" disable-node)))
                    (enum-extends (xps (xpath:evaluate "@extends" disable-node))))
                (when enum-extends
                  (let ((enum (gethash enum-extends (enums vk-spec))))
                    (assert enum
                            () "disabled extension <~a> references unknown enum <~a>" name enum-extends)
                    (assert (not (find-if (lambda (v)
                                       (string= (name v) enum-name))
                                     (enum-values enum)))
                            () "disabled extension <~a> references known enum value <~a>" name enum-name)))))
            (xpath:do-node-set (disable-node (xpath:evaluate "type" require-node))
              (let* ((type-name (xps (xpath:evaluate "@name" disable-node)))
                     (type (gethash type-name (types vk-spec))))
                (assert type
                        () "trying to remove unknown type <~a>" type-name)
                (cond
                  ((eq (category type) :bitmask)
                   (let ((bitmask (gethash type-name (bitmasks vk-spec))))
                     (assert bitmask
                             () "trying to remove unknown bitmask <~a>" type-name)
                     (assert (not (alias bitmask))
                             () "trying to remove disabled bitmask <~a> which has alias <~a>" type-name (alias bitmask))
                     (remhash type-name (bitmasks vk-spec))))
                  ((eq (category type) :enum)
                   (let ((enum (gethash type-name (enums vk-spec))))
                     (assert enum
                             () "trying to remove unknown enum <~a>" type-name)
                     (assert (not (alias enum))
                             () "trying to remove disabled enum <~a> which has alias <~a>" type-name (alias enum))
                     (remhash type-name (enums vk-spec))))
                  ((eq (category type) :struct)
                   (let ((struct (gethash type-name (structures vk-spec))))
                     (assert struct
                             () "trying to remove unknown structure <~a>" type-name)
                     (assert (= (length (aliases struct)) 0)
                             () "trying to remove disabled structure <~a> which has ~a aliases" type-name (length (aliases struct)))
                     (remhash type-name (structures vk-spec))))
                  (t (error "trying to remove <~a> of unhandled type <~a>" type-name (category type)))))))
          (let ((extension (make-instance 'extension
                                          :name name
                                          :platform platform
                                          :deprecated-by deprecated-by
                                          :obsoleted-by obsoleted-by
                                          :promoted-to promoted-to
                                          :requirements requirements)))
            (assert (not (gethash name (extensions vk-spec)))
                    () "already encountered extension <~a>" name)
            (setf (gethash name (extensions vk-spec))
                  extension)
            (xpath:do-node-set (require-node (xpath:evaluate "require" node))
              (let ((@extension (xps (xpath:evaluate "@extension" require-node)))
                    (@feature (xps (xpath:evaluate "@feature" require-node))))
                (when @extension
                  (assert (not (find @extension (requirements extension) :test 'string=))
                          () "require extension <~a> already listed" @extension)
                  (push @extension (requirements extension)))
                (assert (or (not @feature)
                            (gethash @feature (features vk-spec)))
                        () "unknown feature <~a>" @feature))
              (let ((tag (second (split-sequence:split-sequence #\_ name))))
                (assert (find tag (tags vk-spec) :test 'string=)
                        () "name <~a> is using an unknown tag <~a>" name tag)
                (xpath:do-node-set (command-node (xpath:evaluate "command" require-node))
                  (parse-extension-require-command command-node name vk-spec))
                (xpath:do-node-set (enum-node (xpath:evaluate "enum" require-node))
                  (parse-require-enum enum-node tag vk-spec))
                (xpath:do-node-set (type-node (xpath:evaluate "type" require-node))
                  (parse-extension-require-type type-node name vk-spec)))))))))

(defun parse-platforms (vk.xml vk-spec)
  "TODO"
  (xpath:do-node-set (node (xpath:evaluate "/registry/platforms/platform" vk.xml))
    (let ((name (xps (xpath:evaluate "@name" node)))
          (protect (xps (xpath:evaluate "@protect" node))))
      (assert (not (find-if (lambda (p)
                              (string= (protect p) protect))
                            (alexandria:hash-table-values (platforms vk-spec))))
              () "platform protect <~a> already specified" protect)
      (assert (not (gethash name (platforms vk-spec)))
              () "platform name <~a> already specified" name)
      (setf (gethash name (platforms vk-spec))
            (make-instance 'platform
                           :name name
                           :protect protect)))))

(defun parse-features (vk.xml vk-spec)
  "TODO"
  (xpath:do-node-set (node (xpath:evaluate "/registry/feature" vk.xml))
    (let* ((name (xps (xpath:evaluate "@name" node)))
           (number (xps (xpath:evaluate "@number" node))))
      (assert (string= name
                       (concatenate 'string "VK_VERSION_" (substitute #\_ #\. number)))
              () "unexpected formatting of name <~a>" name)
      (assert (not (gethash name (features vk-spec)))
              () "already specified feature <~a>" name)
      (setf (gethash name (features vk-spec))
            number)
      (xpath:do-node-set (require-node (xpath:evaluate "require" node))
        (xpath:do-node-set (command-node (xpath:evaluate "command" require-node))
          (let* ((command-name (xps (xpath:evaluate "@name" command-node)))
                 (command (gethash command-name (commands vk-spec))))
            (assert command
                    () "feature requires unknown command <~a>" command-name)
            (assert (not (feature command))
                    () "command <~a> already listed with feature <~a>" command-name (feature command))
            (setf (feature command) name)))
        (xpath:do-node-set (enum-node (xpath:evaluate "enum" require-node))
          (parse-require-enum enum-node "" vk-spec))
        (xpath:do-node-set (type-node (xpath:evaluate "type" require-node))
          (let ((type-name (xps (xpath:evaluate "@name" type-node))))
            (when (and (not (gethash type-name (defines vk-spec)))
                       (not (find type-name (includes vk-spec) :test 'string=)))
              (let ((type (gethash type-name (types vk-spec))))
                (assert type
                        () "feature requires unknown type <~a>" type-name)
                (assert (or (not (feature type))
                            (string= (feature type) name))
                        () "type <~a> already listed on feature <~a>" type-name (feature type))
                (setf (feature type)
                      name)))))))))

(defun parse-copyright (vk.xml vk-spec)
  "TODO"
  (xpath:do-node-set (node (xpath:evaluate "/registry/comment" vk.xml))
    (let ((text (xps node)))
      (when (search "Copyright" text)
        (setf (vulkan-license-header vk-spec)
              (string-trim '(#\Space #\Tab #\Newline #\return)
                           (subseq text 0 (search "This file, vk.xml, is the" text)))))))
  (assert (vulkan-license-header vk-spec)
          () "no copyright notice found in vk.xml"))

(defun parse-vk-xml (version vk-xml-pathname)
  "Parses the vk.xml file at VK-XML-PATHNAME into a VK-SPEC instance."
  (let* ((vk.xml (cxml:parse-file vk-xml-pathname
                                  (cxml:make-whitespace-normalizer
                                   (stp:make-builder))))
         (vk-spec (make-instance 'vulkan-spec)))
    ;; insert default handle for create-instance and such
    (setf (gethash "" (handles vk-spec))
          (make-instance 'handle
                         :name ""))

    ;; parse vk.xml
    (parse-copyright vk.xml vk-spec)
    (parse-platforms vk.xml vk-spec)
    (parse-tags vk.xml vk-spec)
    (parse-types vk.xml vk-spec)
    (parse-enums vk.xml vk-spec)
    (parse-commands vk.xml vk-spec)
    (parse-features vk.xml vk-spec)
    (parse-extensions vk.xml vk-spec)
    
    ;; reverse order of lists 
    (setf (extended-structs vk-spec)
          (remove-duplicates (reverse (extended-structs vk-spec))))
    (setf (includes vk-spec)
          (reverse (includes vk-spec)))
    (setf (tags vk-spec)
          (reverse (tags vk-spec)))
    
    vk-spec))
