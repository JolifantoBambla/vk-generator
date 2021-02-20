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
   (p-application-info :initform (error "application-info not supplied")
                       :initarg :p-application-info
                       :accessor p-application-info)
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
    (setf s-type :application-info
          p-next (null-pointer)
          p-application-name (application-name value)
          application-version (application-version value)
          p-engine-name (engine-name value)
          engine-version (engine-version value)
          api-version (api-version value))))

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
                         (p-instance 'instance))
    (let* ((lisp-app-info (make-instance 'application-info))
           (lisp-create-info (make-instance 'instance-create-info
                                            :p-application-info app-info))
           (instance nil))
      (setf (mem-aref app-info '(:struct application-info)) lisp-app-info)
      ;;(setf (p-application-info lisp-create-info) app-info)
      (setf (mem-aref create-info '(:struct instance-create-info)) lisp-create-info)
      (print (null-pointer-p p-instance))
      (print (create-instance create-info (null-pointer) p-instance))
      (print "yeah")
      (print (null-pointer-p p-instance))
      (setf instance (mem-aref p-instance 'instance))
      (with-foreign-object (p-devices-count :int)
        (enumerate-physical-devices instance p-devices-count (null-pointer))
        (print (mem-aref p-devices-count :int)))
      ;; looks like just the descruction doesn't work - find out how and we're good to go!
      (destroy-instance instance (null-pointer))
      (print "maybe the error occurs after that?"))))
