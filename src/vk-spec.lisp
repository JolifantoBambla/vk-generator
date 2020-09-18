(in-package :vk-generator/vk-spec)

;; todo: aliases
;; todo: extensions
;; todo: pNext chains
;; todo: create docstrings for types
(defclass vk-spec ()
  ((version
    :initarg :version
    :reader version)
   (copyright
    :initarg :copyright
    :reader copyright)
   (vendor-ids
    :initarg :vendor-id
    :initform (list)
    :accessor vendor-ids)
   (types ;; todo: this is a list for legacy reasons, but should probably also be a hash table
    :initarg :types
    :initform (list)
    :accessor types)
   (bitfields
    :initarg :bitfields
    :iniform (make-hash-table :test 'equal)
    :accessor bitfields)
   (enums
    :initarg :enums
    :iniform (make-hash-table :test 'equal)
    :accessor enums)
   (structs
    :initarg :structs
    :initform (make-hash-table :test 'equal)
    :accessor structs)
   (functions
    :initarg :funcs
    :initform (make-hash-table :test 'equal)
    :accessor functions)
   (function-apis
    :initarg :function-apis
    :initform (make-hash-table :test 'equal)
    :accessor function-apis)
   (extension-names
    :initarg :extension-names
    :initform (make-hash-table :test 'equal)
    :accessor extension-names)
   (handle-types
    :initarg :handle-types
    :initform (make-hash-table :test 'equal)
    :accessor handle-types)
   (alias-names ;; todo: handle aliases
    :initarg :alia-names
    :iniform (list)
    :accessor alias-names))
  (:documentation "Vulkan Specification"))

(defun get-type (vk-spec name)
  (cdr (assoc name (types vk-spec) :test 'string=)))

(defun get-type/f (vk-spec name)
  (cdr (assoc name (types vk-spec) :test (lambda (a b)
                                           (equalp
                                            (fix-type-name a (vendor-ids vk-spec))
                                            (fix-type-name b (vendor-ids vk-spec)))))))

(defun set-type (vk-spec name value)
  (let ((existing-type (get-type vk-spec name)))
    (if existing-type
        (assert (equalp value existing-type))
        (push (cons name value) (types vk-spec)))))
