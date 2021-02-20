;;;; Welcome to Portacle, the Portable Common Lisp Environment.
;; For information on Portacle and how to use it, please read
;;   https://portacle.github.io or *portacle-help*
;; To report problems and to get help with issues,  please visit
;;   https://github.com/portacle/portacle/issues
;; Portacle is currently running SLIME , but you can
;;   Switch to SLY
;; You should also configure Portacle with the
;;   First-time setup
;; 
;; You can use this buffer for notes and tinkering with code.


(defcstruct application-info
  (:s-type structure-type)#|must-be application-info|#
  (:p-next (:pointer :void))#|opaque t|#
  (:p-application-name (:pointer :char))#|optional (true) len (null-terminated)|#
  (:application-version :uint32)
  (:p-engine-name (:pointer :char))#|optional (true) len (null-terminated)|#
  (:engine-version :uint32)
  (:api-version :uint32))

(defcstruct instance-create-info
  (:s-type structure-type)#|must-be instance-create-info|#
  (:p-next (:pointer :void))#|opaque t|#
  (:flags instance-create-flags)#|optional (true)|#
  (:p-application-info (:pointer (:struct application-info)))#|optional (true)|#
  (:enabled-layer-count :uint32)#|optional (true)|#
  (:pp-enabled-layer-names (:pointer (:pointer :char)))#|len (enabled-layer-count
  null-terminated)|#
  (:enabled-extension-count :uint32)#|optional (true)|#
  (:pp-enabled-extension-names (:pointer (:pointer :char)))#|len (enabled-extension-count
  null-terminated)|#)

;; https://common-lisp.net/project/cffi/manual/html_node/Foreign-Structure-Types.html

(defstruct (application-info
            (:conc-name nil))
  ;; s-type must be :application-info
  ;; p-next can be null-pointer
  (application-name "test" :type string)
  (application-version 0 :type integer)
  (engine-name "test" :type string)
  (engine-version 0 :type integer)
  (api-version 4202644))

(defclass vk-struct ()
  ((s-type :reader s-type)))

(defclass application-info (vk-struct)
  ;; s-type must be :application-info
  ;; p-next can be null-pointer
  ((s-type :initform :application-info
           :reader s-type)
   (p-next :initform (null-pointer)
           :initarg :p-next
           :accessor p-next)
   (application-name :initform "test"
                     :initarg :application-name
                     :accessor application-name)
   (application-version :initform 0
                        :initarg :application-version
                        :accessor application-version)
   (engine-name :initform "test"
                :initarg :engine-name
                :accessor engine-name)
   (engine-version :initform 0
                   :initarg :engine-version
                   :accessor engine-version)
   (api-version :initform 4202644
                :initarg :api-version
                :accessor api-version)))

;; todo: define pretty printing for all vk-struct instances (base is just s-type)
;; all pointers to other structs are (expand-from-foreign (<pointer-slot> obj))

(defmethod translate-from-foreign (ptr (type c-application-info))
  (with-foreign-slots ((p-application-name
                        application-version
                        p-engine-name
                        engine-version
                        api-version) ptr (:struct application-info))
    (make-instance 'application-info :application-name p-application-name
                           :application-version application-version
                           :engine-name p-engine-name
                           :engine-version engine-version
                           :api-version api-version)))



(defmethod expand-from-foreign (ptr (type c-application-info))
  `(with-foreign-slots ((p-application-name
                         application-version
                         p-engine-name
                         engine-version
                         api-version)
                        ,ptr
                        (:struct application-info))
     (make-application-info :application-name p-application-name
                            :application-version application-version
                            :engine-name p-engine-name
                            :engine-version engine-version
                            :api-version api-version)))

(defmethod translate-into-foreign-memory (value (type c-application-info) ptr)
  (with-foreign-slots ((s-type
                        p-next
                        p-application-name
                        application-version
                        p-engine-name
                        engine-version
                        api-version)
                       ptr
                       (:struct application-info))
    (setf s-type :application-info
          p-next (null-pointer)
          p-application-name (application-name value)
          application-version (application-version value)
          p-engine-name (engine-name value)
          engine-version (engine-version value)
          api-version (api-version value))))

(defmethod expand-into-foreign-memory (value (type c-application-info) ptr)
  `(with-foreign-slots ((s-type
                         p-next
                         p-application-name
                         application-version
                         p-engine-name
                         engine-version
                         api-version)
                       ,ptr
                       (:struct application-info))
    (setf s-type :application-info
          p-next (null-pointer)
          p-application-name (application-name ,value)
          application-version (application-version ,value)
          p-engine-name (engine-name ,value)
          engine-version (engine-version ,value)
          api-version (api-version ,value))))


(defstruct (instance-create-info
            (:conc-name nil))
  ;; s-type must be :instance-create-info
  ;; p-next can be null-pointer
  (flags nil :type list)
  (application-info (make-application-info) :type application-info)
  (enabled-layers nil :type list)
  (enabled-extensions nil :type list))

(defclass instance-create-info (vk-struct)
  ((s-type :initform :instance-create-info
           :reader :s-type)
   (p-next :initform (null-pointer)
           :initarg :p-next
           :accessor p-next)
   (flags :initform nil
          :accessor flags)
   (p-application-info :initform (error "application-info not supplied")
                       :initarg :p-application-info
                       :accessor p-application-info)
   (enabled-layers :initform nil
                   :accessor enabled-layers)
   (enabled-extensions :initform nil
                       :accessor enabled-extensions)))

(defmethod translate-from-foreign (ptr (type c-instance-create-info))
  (with-foreign-slots ((flags
                        p-application-info
                        enabled-layers-count
                        pp-enabled-layer-names
                        enabled-extension-count
                        pp-enabled-extension-names)
                       ptr (:struct instance-create-info))
    (make-instance 'instance-create-info :flags flags
                               :p-application-info p-application-info
                               :enabled-layers nil
                               :enabled-extensions nil)))

(defmethod expand-from-foreign (ptr (type c-instance-create-info))
  `(with-foreign-slots ((flags
                         p-application-info
                         enabled-layers-count
                         pp-enabled-layer-names
                         enabled-extension-count
                         pp-enabled-extension-names)
                        ,ptr (:struct instance-create-info))
     (make-instance-create-info :flags flags
                                :application-info (expand-from-foreign p-application-info)
                                :enabled-layers nil
                                :enabled-extensions nil)))

(defmethod translate-into-foreign-memory (value (type c-instance-create-info) ptr)
  (with-foreign-slots ((s-type
                        p-next
                        flags
                        p-application-info
                        enabled-layer-count
                        pp-enabled-layer-names
                        enabled-extension-count
                        pp-enabled-extension-names)
                       ptr
                       (:struct instance-create-info))
    (setf s-type :instance-create-info
          p-next (null-pointer)
          flags 0
          p-application-info (p-application-info value)
          enabled-layer-count 0
          pp-enabled-layer-names (null-pointer)
          enabled-extension-count 0
          pp-enabled-extension-names (null-pointer))))

(defun test-instance ()
  (with-foreign-objects ((app-info '(:struct application-info))
                         (create-info '(:struct instance-create-info))
                         (instance 'instance))
    (let* ((lisp-app-info (make-instance 'application-info))
           (lisp-create-info (make-instance 'instance-create-info
                                            :p-application-info app-info)))
      (setf (mem-aref app-info '(:struct application-info)) lisp-app-info)
      ;;(setf (p-application-info lisp-create-info) app-info)
      (setf (mem-aref create-info '(:struct instance-create-info)) lisp-create-info)
      (print (null-pointer-p instance))
      (print (create-instance create-info (null-pointer) instance))
      (print "yeah")
      (print (null-pointer-p instance))
      (with-foreign-object (p-devices-count :int)
        (enumerate-physical-devices instance p-devices-count (null-pointer))
        (print (mem-aref p-devices-count :int)))
      ;; looks like just the descruction doesn't work - find out how and we're good to go!
      (destroy-instance instance (null-pointer))
      (print "maybe the error occurs after that?"))))

(defmethod expand-into-foreign-memory (value (type c-application-info) ptr)
  `(with-foreign-slots ((s-type
                         p-next
                         p-application-name
                         application-version
                         p-engine-name
                         engine-version
                         api-version)
                       ,ptr
                       (:struct application-info))
    (setf s-type :application-info
          p-next (null-pointer)
          p-application-name (application-name ,value)
          application-version (application-version ,value)
          p-engine-name (engine-name ,value)
          engine-version (engine-version ,value)
          api-version (api-version ,value))))

(defcstruct (application-info :class c-application-info)
  (s-type structure-type)
  (p-next (:pointer :void))
  (p-application-name :string)
  (application-version :uint32)
  (p-engine-name :string)
  (engine-version :uint32)
  (api-version :uint32))


;;;


(defparameter *default-allocator* (cffi:null-pointer))

(defun create-instance (application-info &optional (allocator *default-allocator*))
  (cffi:with-foreign-object (p-instance '%vk:instance)
    (%vk:create-instance application-info allocator p-instance)
    (cffi:mem-aref p-instance '%vk:instance)))

(defun destroy-instance (instance &optional (allocator *default-allocator*))
  (%vk:destroy-instance instance allocator))

(defun test-instance ()
  (cffi:with-foreign-objects ((p-app-info '(:struct %vk:application-info))
                              (p-create-info '(:struct %vk:instance-create-info)))
    (let* ((app-info (make-instance '%vk:application-info))
           (create-info (make-instance '%vk:instance-create-info
                                       :p-application-info p-app-info)))
      (setf (cffi:mem-aref p-app-info '(:struct %vk:application-info)) app-info)
      (setf (cffi:mem-aref p-create-info '(:struct %vk:instance-create-info)) create-info)
      (let ((instance (create-instance p-create-info)))
        (cffi:with-foreign-object (p-devices-count :int)
          (%vk:enumerate-physical-devices instance p-devices-count (cffi:null-pointer))
          (print (cffi:mem-aref p-devices-count :int)))
        (destroy-instance instance)))))

;; I guess I want something like this:
(defun create-instance (instance-create-info &optional (allocator *default-allocator*))
  (cffi:with-foreign-objects ((p-instance-create-info '(:struct %vk:instance-create-info))
                              (p-instance '%vk:instance))
    ;; this would be wrapped within an unwind-protect
    (setf (cffi:mem-aref p-instance-create-info '(:struct %vk:instance-create-info)) instance-create-info)
    (%vk:create-instance p-instance-create-info allocator p-instance)
    (%vk:free-allocated-foreign-chain (first (gethash p-instance-create-info %vk:*allocated-foreign-objects*)))
    (remhash p-instance-create-info %vk:*allocated-foreign-objects*)
    (cffi:mem-aref p-instance '%vk:instance)))

(defun destroy-instance (instance &optional (allocator *default-allocator*))
  (%vk:destroy-instance instance allocator))

(defun test-instance ()
  (let ((instance (create-instance (make-instance '%vk:instance-create-info))))
    ;; enumerate-physical-devices would also be wrapped
    (cffi:with-foreign-object (p-devices-count :int)
      (%vk:enumerate-physical-devices instance p-devices-count (cffi:null-pointer))
      (print (cffi:mem-aref p-devices-count :int)))
    (destroy-instance instance)))


;; I want to be able to write something like this:
(destroy-instance (create-instance (make-instance 'instance-create-info)))

;; all functions returning a VkResult should return (values <actual-return-value> result), where <actual-return-value> is either nil or an opaque handle (or whatever)


(defparameter *allocated-foreign-objects* (make-hash-table)
  "A hash table storing allocated foreign objects and dependencies between them.
Each foreign object (key-object) is associated with a list of other foreign objects that were allocated during allocation of the key-object and must be freed when the key-object is freed.

Each foreign object must appear once at most within a value of this hash table.

E.g.
((<foreign1> '(<foreign2> <foreign3>))
 (<foreign2> nil)
 (<foreign3> '(<foreign4>))
 (<foreign4> nil))
")

;; most of the time the first key will be stack-allocated (using with-foreign-object(s)) and the rest will be heap-allocated (using foreign-alloc)
;; this will cause errors, so in the \"final\" version this should maybe be split into two functions, one that is called with the stack-allocated object that then calls the other one with all heap-allocated objects, which is called recursively to free all objects.
;; all of this should be used in the clean-up form of vk-functions
(defun free-allocated-foreign-chain (foreign-obj)
  "Frees all foreign objects ..."
  (let ((children (gethash foreign-obj *allocated-foreign-objects*)))
    (remhash foreign-obj *allocated-foreign-objects*)
    (foreign-free foreign-obj)
    (dolist (child children) (free-allocated-foreign-chain child))))


;; just saving this as well - start writing all wrappers next week:


(in-package :%vk)

(defclass vk-struct ()
  ((s-type :reader s-type)))

(defclass application-info (vk-struct)
  ;; s-type must be :application-info
  ;; p-next can be null-pointer
  ((s-type :initform :application-info
           :reader s-type)
   (p-next :initform (null-pointer)
           :initarg :p-next
           :accessor p-next)
   (application-name :initform "test"
                     :initarg :application-name
                     :accessor application-name)
   (application-version :initform 0
                        :initarg :application-version
                        :accessor application-version)
   (engine-name :initform "test"
                :initarg :engine-name
                :accessor engine-name)
   (engine-version :initform 0
                   :initarg :engine-version
                   :accessor engine-version)
   (api-version :initform 4202644
                :initarg :api-version
                :accessor api-version)))

(defclass instance-create-info (vk-struct)
  ((s-type :initform :instance-create-info
           :reader :s-type)
   (p-next :initform (null-pointer)
           :initarg :p-next
           :accessor p-next)
   (flags :initform nil
          :accessor flags)
   (application-info :initform (make-instance 'application-info)
                     :initarg :application-info
                     :accessor application-info)
   (enabled-layers :initform nil
                   :accessor enabled-layers)
   (enabled-extensions :initform nil
                       :accessor enabled-extensions)))

(defmethod translate-into-foreign-memory (value (type c-application-info) ptr)
  (with-foreign-slots ((s-type
                        p-next
                        p-application-name
                        application-version
                        p-engine-name
                        engine-version
                        api-version)
                       ptr
                       (:struct application-info))
    (print "translating application info")
    (setf s-type :application-info
          p-next (null-pointer)
          p-application-name (application-name value)
          application-version (application-version value)
          p-engine-name (engine-name value)
          engine-version (engine-version value)
          api-version (api-version value))))


(defparameter *allocated-foreign-objects* (make-hash-table)
  "A hash table storing allocated foreign objects and dependencies between them.
Each foreign object (key-object) is associated with a list of other foreign objects that were allocated during allocation of the key-ob>

Each foreign object must appear once at most within a value of this hash table.
E.g.
((<foreign1> '(<foreign2> <foreign3>))
 (<foreign2> nil)
 (<foreign3> '(<foreign4>))
 (<foreign4> nil))
")

(defun free-allocated-foreign-chain (foreign-obj)
  "Frees all foreign objects ..."
  (let ((children (gethash foreign-obj *allocated-foreign-objects*)))
    (remhash foreign-obj *allocated-foreign-objects*)
    (foreign-free foreign-obj)
    (dolist (child children) (free-allocated-foreign-chain child))))


(defmethod translate-into-foreign-memory (value (type c-instance-create-info) ptr)
  (with-foreign-slots ((s-type
                        p-next
                        flags
                        p-application-info
                        enabled-layer-count
                        pp-enabled-layer-names
                        enabled-extension-count
                        pp-enabled-extension-names)
                       ptr
                       (:struct instance-create-info))
    (let ((alloc-p-application-info (foreign-alloc '(:struct application-info))))
      (push alloc-p-application-info (gethash ptr *allocated-foreign-objects*))
      (setf (mem-aref alloc-p-application-info '(:struct application-info)) (application-info value))
      (print "translated application info")
      (setf s-type :instance-create-info
            p-next (null-pointer)
            flags 0
            p-application-info alloc-p-application-info
            enabled-layer-count 0
            pp-enabled-layer-names (null-pointer)
            enabled-extension-count 0
            pp-enabled-extension-names (null-pointer)))))

(defun test-instance ()
  (with-foreign-objects ((p-create-info '(:struct instance-create-info))
                         (p-instance 'instance))
    (let* ((create-info (make-instance 'instance-create-info))
           (instance nil))
      (setf (mem-aref p-create-info '(:struct instance-create-info)) create-info)
      (print (create-instance p-create-info (null-pointer) p-instance))
      (free-allocated-foreign-chain (first (gethash p-create-info *allocated-foreign-objects*)))
      (setf instance (mem-aref p-instance 'instance))
      (with-foreign-object (p-devices-count :int)
        (enumerate-physical-devices instance p-devices-count (null-pointer))
        (print (mem-aref p-devices-count :int)))
      (destroy-instance instance (null-pointer)))))


