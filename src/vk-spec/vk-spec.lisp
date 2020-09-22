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
    :initarg :vendor-ids
    :initform (list)
    :accessor vendor-ids)
   (vk-api-version
    :initarg :vk-api-version
    :initform (list)
    :accessor vk-api-version)
   (api-constants
    :initarg :api-constants
    :initform (make-hash-table :test 'equal)
    :accessor api-constants)
   (types ;; todo: this is a list for legacy reasons, but should probably also be a hash table
    :initarg :types
    :initform (list)
    :accessor types)
   (bitfields
    :initarg :bitfields
    :initform (make-hash-table :test 'equal)
    :accessor bitfields)
   (enums
    :initarg :enums
    :initform (make-hash-table :test 'equal)
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
    :initform (list)
    :accessor alias-names))
  (:documentation "Vulkan Specification"))
