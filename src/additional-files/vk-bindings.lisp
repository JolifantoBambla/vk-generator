#|
 Copyright(c) 2020 - Lukas Herzberger <herzberger.lukas@gmail.com>
 SPDX-License-Identifier: MIT
|#

(in-package :vk)

(eval-when (:compile-toplevel)
  (defun process-args (args optional-p &optional (extension-p nil))
    "Splits ARGS into a list of argument names and a list of types which can be used for type declarations.
If OPTIONAL-P is truthy NULL is appended to each type declaration."
    (multiple-value-bind (arg-names
                          arg-types
                          unused-args)
        (loop for arg in args
              collect (first arg) into arg-names
              collect (list 'declare
                            (if optional-p
                                (list (list 'or (second arg) 'null) (first (first arg)))
                                (list (second arg) (first arg))))
              into arg-types
              when (third arg) collect (list 'declare (list 'ignore (first (first arg)))) into unused-args
              finally (return (cl:values arg-names arg-types unused-args)))
      (when (and optional-p
                 extension-p)
        (setf arg-names (reverse arg-names))
        (setf arg-types (reverse arg-types))
        (push (list 'extension-loader '*default-extension-loader*)
              arg-names)
        (push (list 'declare (list '%vk:extension-loader 'extension-loader))
              arg-types)
        (setf arg-names (reverse arg-names))
        (setf arg-types (reverse arg-types)))
      (cl:values arg-names arg-types unused-args)))
  
  (defun process-variables (variables &optional (extension-p nil))
    "Elements of VARIABLES should look like this:
(arg-name arg-type contents ...options)
options are :handle, :in/:out, :optional

arg-name is the name of the symbol used when calling the %vk function
contents is the source of the data which should be translated to the args memory location (i.e. the argument of the vk-function or something like the length of an argument of the vk-function)
"
    (let ((translated-args nil)
          (vk-input-args nil)
          (output-args nil)
          (let-args nil))
      (loop for var in variables
            do (let ((var-sym (gensym (string (first var)))))
                 (push
                  ;; single handles and single raw values are used directly
                  (if (or (and (member :handle var)
                               (member :in var)
                               (not (member :list var)))
                          (and (member :raw var)
                               (member :in var) ;; added this without really thinking
                               (eq (first var) (third var))))
                      (first var)
                      var-sym)
                  vk-input-args)
                 ;; all input variables which are not single handles or single raw values have to be translated
                 (when (or (and (member :in var)
                                (member :out var)
                                (not (member :ignore-in var)))
                           (and (not (member :out var))
                                (not (and (not (member :list var))
                                          (or (member :handle var)
                                              (member :raw var))))))
                     (push
                      (list var-sym
                            (second var) ;; type
                            (third var)) ;; contents
                      translated-args))
                 (when (member :out var)
                   (push
                     (list (first var)
                           (list var-sym
                                 (second var))) ;; type
                     output-args))
                 ;; single raw  values which depend on another variable (e.g. (length <other-var>) should be in a let-form
                 (if (and (member :raw var)
                          (member :in var)
                          (not (eq (first var) (third var))))
                     (push
                      (list var-sym (third var))
                      let-args))))
      (when extension-p (push 'extension-loader vk-input-args))
      (cl:values (reverse translated-args) (reverse let-args) (reverse vk-input-args) (reverse output-args)))))

;; TODO: there is a lot of duplicated code here - clean this up

;; simple-fun: done!
;; - return-type -> trivial-return-type
;; - (not return-type) -> (eq trivial-return-type :trivial)
;; fill-arbitrary-buffer-fun: done!
;; - same as simple-fun with :trivial-return-type :trivial
;; handle-fun: done!
;; - no-result-p -> no-vk-result-p
;; get-struct-fun: done!
;; - needs to set -> :no-vk-result-p t
;; get-struct-chain-fun: done!
;; - same as get-struct-fun
;; - must set :returns-struct-chain-p t
;; create-handles-fun: done!
;; - len-provider is now a keyword argument
;; get-structs-fun: done!
;; - no-result-p -> no-vk-result-p
;; - array-arg-name -> first-array-arg-name
;; get-struct-chains-fun: done!
;; - same as get-structs-fun
;; - must set :returns-struct-chain-p t
;; - vk-constructor is now a keyword argument
;; enumerate-fun:
;; - array-arg-name -> first-array-arg-name
;; - must set :enumerate t
;; enumerate-struct-chains-fun:
;; - same as enumerate-fun
;; - must set :returns-struct-chain-p t
;; - doesn't need a vk-constructor!
;; enumerate-two-arrays-fun:
;; - must set first-array-arg-name
;; - must set count-arg-name
;; - must set :enumerate t
;; - must set second-array-arg-name
;; TODO: check for use of shadowed symbols

(defmacro defvkfun ((name
                     vulkan-fun
                     required-args
                     optional-args
                     &key
                       (no-vk-result-p nil)
                       (trivial-return-type nil)
                       (extension-p nil)
                       (len-provider 1)
                       (enumerate-p nil)
                       (first-array-arg-name nil)
                       (second-array-arg-name nil)
                       (count-arg-name nil)
                       (vk-constructor nil)
                       (returns-struct-chain-p nil))
                    &body body)
  "Defines a function wrapping a function in the VULKAN package.

Arguments:
NAME
VULKAN-FUN
REQUIRED-ARGS
OPTIONAL-ARGS

Keyword Arguments:
NO-VK-RESULT 
TRIVIAL-RETURN-TYPE
EXTENSION-P
LEN-PROVIDER
ENUMERATE-P
FIRST-ARRAY-ARG-NAME
SECOND-ARRAY-ARG-NAME
COUNT-ARG-NAME
VK-CONSTRUCTOR
RETURNS-STRUCT-CHAIN-P

Body:
DOCUMENTATION
VARIABLES
"
  (let* ((docstring (car body))
         (variables (cdr body))
         (single-array-result-p (and (and first-array-arg-name
                                          count-arg-name)
                                     (not second-array-arg-name)))
         (two-array-results-p (and first-array-arg-name
                                   second-array-arg-name
                                   count-arg-name))
         (single-array-single-value-p (and (not single-array-result-p)
                                           first-array-arg-name
                                           (not (eq len-provider 1)))))
    (assert (not (and enumerate-p
                      (not (or single-array-result-p
                               two-array-results-p))))
            () "Enumerate functions must have at least one array result.")
    (assert (not (and two-array-results-p
                      (not enumerate-p)))
            () "Function returning two lists must be an enumerate function.")
    (multiple-value-bind (required-arg-names
                          required-arg-declares)
        (process-args required-args nil)
      (multiple-value-bind (optional-arg-names
                            optional-arg-declares
                            ignore-arg-declares)
          (process-args optional-args t extension-p)
        (multiple-value-bind (translated-args
                              let-args
                              vk-input-args
                              output-args)
            (process-variables variables extension-p)
          (let ((handle-or-struct-def (second (first output-args)))
                (first-array-arg (when first-array-arg-name
                                   (find-if (lambda (a)
                                              (eq (first a) first-array-arg-name))
                                            output-args)))
                (second-array-arg (when second-array-arg-name
                                    (find-if (lambda (a)
                                               (eq (first a) second-array-arg-name))
                                             output-args)))
                (count-arg (when count-arg-name
                             (find-if (lambda (a)
                                        (eq (first a) count-arg-name))
                                      output-args)))
                (other-return-arg (when single-array-single-value-p
                                    (find-if-not (lambda (a)
                                                   (eq (first a) first-array-arg-name))
                                                 output-args)))
                (result (gensym "RESULT"))
                (i (gensym "INDEX"))
                (translated-count (gensym "TRANSLATED-COUNT"))
                (first-result-array (gensym "FIRST-RESULT-ARRAY"))
                (second-result-array (gensym "SECOND-RESULT-ARRAY")))
            `(defun ,name (,@required-arg-names &optional ,@optional-arg-names)
               ,docstring
               ;;(declare (optimize (speed 3)))
               ,@required-arg-declares
               ,@optional-arg-declares
               ,@ignore-arg-declares
               (let (,@let-args)
                 (vk-alloc:with-foreign-allocated-objects (,@(remove-if (lambda (arg)
                                                                          ;; if a list of struct chains is returned
                                                                          ;; we don't want to translate it just yet
                                                                          (when first-array-arg-name
                                                                            (eq (third arg)
                                                                                first-array-arg-name)))
                                                                        translated-args))
                   ,(cond
                      ;; trivial & fill-arbitrary-buffer
                      (trivial-return-type
                       (if (eq trivial-return-type :trivial)
                           `(,vulkan-fun ,@vk-input-args)
                           `(let ((,result (,vulkan-fun ,@vk-input-args)))
                              (unless (cffi:null-pointer-p ,result)
                                ,result))))
                      ;; single-array-single-result
                      (single-array-single-value-p
                       `(cffi:with-foreign-objects ((,@ (second first-array-arg) ,len-provider)
                                                    (,@ (second other-return-arg)))
                          (let ((,result (,vulkan-fun ,@vk-input-args)))
                            (cl:values (loop for ,i from 0 below ,len-provider
                                             collect (cffi:mem-aref ,@(second first-array-arg) ,i))
                                       (cffi:mem-aref ,@(second other-return-arg))
                                       ,result))))
                      ;; get-struct(-chain)s & enumerate(-struct-chains)
                      (single-array-result-p
                       (flet ((return-enumerated-list ()
                                `(let ((,result :incomplete))
                                   (loop do (setf ,result (,vulkan-fun ,@vk-input-args))
                                         while (eq ,result :incomplete)
                                         finally (return (when (eq ,result :success)
                                                           (let ((,translated-count (cffi:mem-aref ,@(second count-arg))))
                                                             (cl:values
                                                              (when (> ,translated-count 0)
                                                                (cffi:with-foreign-object (,@(second first-array-arg) ,translated-count)
                                                                  (setf ,result (,vulkan-fun ,@vk-input-args))
                                                                  (loop for ,i from 0 below ,translated-count
                                                                        collect (cffi:mem-aref ,@(second first-array-arg) ,i))))
                                                              ,result)))))))
                              (return-list (use-translated-count-p)
                                (if no-vk-result-p
                                    `(progn
                                       (,vulkan-fun ,@vk-input-args)
                                       (loop for ,i from 0 below ,(if use-translated-count-p
                                                                      translated-count
                                                                      (list 'length
                                                                            first-array-arg-name))
                                             collect (cffi:mem-aref ,@(second first-array-arg) ,i)))
                                    `(let ((,result (,vulkan-fun ,@vk-input-args)))
                                       (cl:values
                                        (loop for ,i from 0 below ,(if use-translated-count-p
                                                                       translated-count
                                                                       (list 'length
                                                                             first-array-arg-name))
                                              collect (cffi:mem-aref ,@(second first-array-arg) ,i))
                                        ,result)))))
                         (if enumerate-p
                             (if returns-struct-chain-p
                                 ;; enumerate-struct-chains
                                 `(if ,first-array-arg-name
                                      (vk-alloc:with-foreign-allocated-objects (,@(remove-if-not (lambda (arg)
                                                                                                   (when first-array-arg-name
                                                                                                     (eq (third arg)
                                                                                                         first-array-arg-name)))
                                                                                                 translated-args)
                                                                                (,@(second count-arg)
                                                                                 (cl:length ,first-array-arg-name)
                                                                                 nil))
                                        ,(return-enumerated-list))
                                      (cffi:with-foreign-object (,@(second count-arg))
                                        (let ((,(first (second first-array-arg)) (cffi:null-pointer)))
                                          ,(return-enumerated-list))))
                                 ;; enumerate
                                 `(cffi:with-foreign-object (,@(second count-arg))
                                    (let ((,(first (second first-array-arg)) (cffi:null-pointer)))
                                      ,(return-enumerated-list))))
                             (if returns-struct-chain-p
                                 ;; get-struct-chains
                                 (progn
                                   (assert vk-constructor
                                           () "FIRST-VK-CONSTRUCTOR must be provided for get-struct-chains functions")
                                   `(if ,first-array-arg-name
                                        (vk-alloc:with-foreign-allocated-objects (,@(remove-if-not (lambda (arg)
                                                                                                     (when first-array-arg-name
                                                                                                       (eq (third arg)
                                                                                                           first-array-arg-name)))
                                                                                                   translated-args)
                                                                                  (,@(second count-arg)
                                                                                   (cl:length ,first-array-arg-name)
                                                                                   nil))
                                          ,(return-list nil))
                                        (cffi:with-foreign-object (,@(second count-arg))
                                          (let ((,(first (second first-array-arg)) (cffi:null-pointer)))
                                            (,vulkan-fun ,@vk-input-args)
                                            (let ((,translated-count (cffi:mem-aref ,@(second count-arg))))
                                              (vk-alloc:with-foreign-allocated-object (,@(second first-array-arg)
                                                                                       (make-list ,translated-count
                                                                                                  :initial-element (,vk-constructor)))
                                                ,(return-list t)))))))
                                 ;; get-structs
                                 `(cffi:with-foreign-object (,@(second count-arg))
                                    (let ((,(first (second first-array-arg)) (cffi:null-pointer)))
                                      (,vulkan-fun ,@vk-input-args)
                                      (let ((,translated-count (cffi:mem-aref ,@ (second count-arg))))
                                        (cffi:with-foreign-object (,@(second first-array-arg) ,translated-count)
                                          ,(return-list t)))))))))
                      ;; enumerate two struct chains
                      (two-array-results-p
                       `(cffi:with-foreign-object (,@(second count-arg))
                          (let ((,(first (second first-array-arg)) (cffi:null-pointer))
                                (,(first (second second-array-arg)) (cffi:null-pointer))
                                (,result :incomplete))
                            (loop do (setf ,result (,vulkan-fun ,@vk-input-args))
                                  while (eq ,result :incomplete)
                                  finally (return (when (eq ,result :success)
                                                    (let ((,translated-count (cffi:mem-aref ,@(second count-arg))))
                                                      (if (> ,translated-count 0)
                                                          (cffi:with-foreign-objects ((,@(second first-array-arg) ,translated-count)
                                                                                      (,@(second second-array-arg) ,translated-count))
                                                            (setf ,result (,vulkan-fun ,@vk-input-args))
                                                            (loop for ,i from 0 below ,translated-count
                                                                  collect (cffi:mem-aref ,@(second first-array-arg) ,i) into ,first-result-array
                                                                  collect (cffi:mem-aref ,@(second second-array-arg) ,i) into ,second-result-array
                                                                  finally (return (cl:values ,first-result-array ,second-result-array ,result))))
                                                          (cl:values nil nil ,result)))))))))
                      (t
                       (flet ((return-no-vk-result ()
                                (progn
                                  (assert (eq len-provider 1)
                                          () "LEN-PROVIDER must be 1 for get-structs function which don't return a VkResult.")
                                  `(progn
                                     (,vulkan-fun ,@vk-input-args)
                                     (cffi:mem-aref ,@handle-or-struct-def)))))
                         (if returns-struct-chain-p
                             ;; get-struct-chain
                             (progn
                               (assert no-vk-result-p
                                       () "Functions returning struct chains must not return a VkResult.")
                               (return-no-vk-result))
                             ;; create-handle & create-handles & get-struct
                             `(cffi:with-foreign-object (,@handle-or-struct-def ,len-provider)
                                ,(if no-vk-result-p
                                     (return-no-vk-result)
                                     `(let ((,result (,vulkan-fun ,@vk-input-args)))
                                        (cl:values
                                         ,(cond
                                            ((not (eq len-provider 1))
                                             `(loop for ,i from 0 below ,len-provider
                                                    collect (cffi:mem-aref ,@handle-or-struct-def ,i)))
                                            (t
                                             `(cffi:mem-aref ,@handle-or-struct-def)))
                                         ,result)))))))))))))))))

;;; --------------------------------------------- 2 output parameters ------------------------------------------------------------------

;; case 2a: e.g. vkGetPhysicalDeviceSparseImageFormatProperties
;; returned struct have no next slot
(defmacro defvk-get-structs-fun ((name vulkan-fun docstring required-args optional-args count-arg-name array-arg-name &optional (no-result-p nil) (extension-p nil)) &body variables)
  "Defines a function named NAME which wraps VULKAN-FUN.
VULKAN-FUN is function that gets multiple structs and might return a RESULT.
E.g. vkGetPhysicalDeviceSparseImageFormatProperties"
  (multiple-value-bind (required-arg-names required-arg-declares) (process-args required-args nil)
    (multiple-value-bind (optional-arg-names optional-arg-declares) (process-args optional-args t extension-p)
      (multiple-value-bind (translated-args let-args vk-input-args output-args) (process-variables variables extension-p)
        (let ((count-arg (find-if (lambda (a)
                                    (eq (first a) count-arg-name))
                                  output-args))
              (array-arg (find-if (lambda (a)
                                    (eq (first a) array-arg-name))
                                  output-args))
              (translated-count (gensym "COUNT"))
              (result (gensym "RESULT"))
              (i (gensym "INDEX")))
          `(defun ,name (,@required-arg-names &optional ,@optional-arg-names)
             ,docstring
             ,@required-arg-declares
             ,@optional-arg-declares
             (let (,@let-args)
               (vk-alloc:with-foreign-allocated-objects (,@translated-args)
                 (cffi:with-foreign-object (,@ (second count-arg))
                   (let ((,(first (second array-arg)) (cffi:null-pointer)))
                     (,vulkan-fun ,@vk-input-args)
                     (let ((,translated-count (cffi:mem-aref ,@ (second count-arg))))
                       (cffi:with-foreign-object (,@ (second array-arg) ,translated-count)
                         ,(if no-result-p
                              `(progn
                                 (,vulkan-fun ,@vk-input-args)
                                 (loop for ,i from 0 below ,translated-count
                                       collect (cffi:mem-aref ,@ (second array-arg) ,i)))
                              `(let ((,result (,vulkan-fun ,@vk-input-args)))
                                 (cl:values (loop for ,i from 0 below ,translated-count
                                                  collect (cffi:mem-aref ,@ (second array-arg) ,i)))))))))))))))))

;; case 2a: e.g. vkGetPhysicalDeviceQueueFamilyProperties2
;; structs with next slot
(defmacro defvk-get-struct-chains-fun ((name vulkan-fun docstring required-args optional-args count-arg-name array-arg-name vk-constructor &optional (no-result-p nil) (extension-p nil)) &body variables)
  "Defines a function named NAME which wraps VULKAN-FUN.
VULKAN-FUN is function that gets multiple structs and might return a RESULT.
E.g. vkGetPhysicalDeviceQueueFamilyProperties2"
  (multiple-value-bind (required-arg-names required-arg-declares) (process-args required-args nil)
    (multiple-value-bind (optional-arg-names optional-arg-declares) (process-args optional-args t extension-p)
      (multiple-value-bind (translated-args let-args vk-input-args output-args) (process-variables variables extension-p)
        (let ((count-arg (find-if (lambda (a)
                                    (eq (first a) count-arg-name))
                                  output-args))
              (array-arg (find-if (lambda (a)
                                    (eq (first a) array-arg-name))
                                  output-args))
              (translated-count (gensym "COUNT"))
              (result (gensym "RESULT"))
              (i (gensym "INDEX")))
          `(defun ,name (,@required-arg-names &optional ,@optional-arg-names)
             ,docstring
             ,@required-arg-declares
             ,@optional-arg-declares
             (let (,@let-args)
               (if ,array-arg-name
                   (vk-alloc:with-foreign-allocated-objects (,@translated-args
                                                             (,@ (second count-arg) (length ,array-arg-name) nil))
                     ,(if no-result-p
                          `(progn
                             (,vulkan-fun ,@vk-input-args)
                             (loop for ,i from 0 below (length ,array-arg-name)
                                   collect (cffi:mem-aref ,@ (second array-arg) ,i)))
                          `(let ((,result (,vulkan-fun ,@vk-input-args)))
                             (cl:values (loop for ,i from 0 below (length ,array-arg-name)
                                              collect (cffi:mem-aref ,@ (second array-arg) ,i))))))
                   (vk-alloc:with-foreign-allocated-objects (,@ (remove-if (lambda (a)
                                                                             ;; the third element is actually the source
                                                                             ;; of the data to fill the arg with, but here
                                                                             ;; this should always be the same as the
                                                                             ;; array-arg-name
                                                                             (eq (third a) array-arg-name))
                                                                           translated-args))
                     (cffi:with-foreign-object (,@ (second count-arg))
                       (let ((,(first (second array-arg)) (cffi:null-pointer)))
                         (,vulkan-fun ,@vk-input-args)
                         (let ((,translated-count (cffi:mem-aref ,@ (second count-arg))))
                           (vk-alloc:with-foreign-allocated-object (,@ (second array-arg)
                                                                       (make-list ,translated-count :initial-element (,vk-constructor)))
                             ,(if no-result-p
                                  `(progn
                                     (,vulkan-fun ,@vk-input-args)
                                     (loop for ,i from 0 below ,translated-count
                                           collect (cffi:mem-aref ,@ (second array-arg) ,i)))
                                  `(let ((,result (,vulkan-fun ,@vk-input-args)))
                                     (cl:values (loop for ,i from 0 below ,translated-count
                                                      collect (cffi:mem-aref ,@ (second array-arg) ,i))))))))))))))))))

;; case 2c: enumerate - e.g. vkEnumeratePhysicalDevices
(defmacro defvk-enumerate-fun ((name vulkan-fun docstring required-args optional-args count-arg-name array-arg-name &optional (extension-p nil)) &body variables)
  "Defines a function named NAME which wraps VULKAN-FUN.
VULKAN-FUN is function that enumerates values and returns a RESULT.
E.g. vkEnumeratePhysicalDevices"
  (multiple-value-bind (required-arg-names required-arg-declares) (process-args required-args nil)
    (multiple-value-bind (optional-arg-names optional-arg-declares) (process-args optional-args t extension-p)
      (multiple-value-bind (translated-args let-args vk-input-args output-args) (process-variables variables extension-p)
        (let ((count-arg (find-if (lambda (a)
                                    (eq (first a) count-arg-name))
                                  output-args))
              (array-arg (find-if (lambda (a)
                                    (eq (first a) array-arg-name))
                                  output-args))
              (translated-count (gensym "COUNT"))
              (result (gensym "RESULT"))
              (i (gensym "INDEX")))
          `(defun ,name (,@required-arg-names &optional ,@optional-arg-names)
             ,docstring
             ,@required-arg-declares
             ,@optional-arg-declares
             (let (,@let-args)
               (vk-alloc:with-foreign-allocated-objects (,@translated-args)
                 (cffi:with-foreign-object (,@ (second count-arg))
                   (let ((,(first (second array-arg)) (cffi:null-pointer))
                         (,result :incomplete))
                     (loop do (setf ,result (,vulkan-fun ,@vk-input-args))
                           while (eq ,result :incomplete)
                           finally (return (when (eq ,result :success)
                                             (let ((,translated-count (cffi:mem-aref ,@ (second count-arg))))
                                               (cl:values
                                                (when (> ,translated-count 0)
                                                  (cffi:with-foreign-object (,@ (second array-arg) ,translated-count)
                                                    (setf ,result (,vulkan-fun ,@vk-input-args))
                                                    (loop for ,i from 0 below ,translated-count
                                                          collect (cffi:mem-aref ,@ (second array-arg) ,i))))
                                                ,result)))))))))))))))
(defmacro defvk-enumerate-struct-chains-fun ((name vulkan-fun docstring required-args optional-args count-arg-name array-arg-name vk-constructor &optional (extension-p nil)) &body variables)
  "Defines a function named NAME which wraps VULKAN-FUN.
VULKAN-FUN is function that enumerates values and returns a RESULT.
E.g. vkEnumeratePhysicalDevices"
  (multiple-value-bind (required-arg-names required-arg-declares) (process-args required-args nil)
    (multiple-value-bind (optional-arg-names optional-arg-declares) (process-args optional-args t extension-p)
      (multiple-value-bind (translated-args let-args vk-input-args output-args) (process-variables variables extension-p)
        (let ((count-arg (find-if (lambda (a)
                                    (eq (first a) count-arg-name))
                                  output-args))
              (array-arg (find-if (lambda (a)
                                    (eq (first a) array-arg-name))
                                  output-args))
              (translated-count (gensym "COUNT"))
              (result (gensym "RESULT"))
              (i (gensym "INDEX")))
          `(defun ,name (,@required-arg-names &optional ,@optional-arg-names)
             ,docstring
             ,@required-arg-declares
             ,@optional-arg-declares
             (let (,@let-args)
               (if ,array-arg-name
                   (vk-alloc:with-foreign-allocated-objects (,@translated-args
                                                             (,@(second count-arg) (length ,array-arg-name) nil))
                     (let ((,result :incomplete))
                       (loop do (setf ,result (,vulkan-fun ,@vk-input-args))
                             while (eq ,result :incomplete)
                             finally (return (when (eq ,result :success)
                                               (let ((,translated-count (cffi:mem-aref ,@(second count-arg))))
                                                 (cl:values
                                                  (when (> ,translated-count 0)
                                                    (cffi:with-foreign-object (,@(second array-arg) ,translated-count)
                                                      (setf ,result (,vulkan-fun ,@vk-input-args))
                                                      (loop for ,i from 0 below ,translated-count
                                                            collect (cffi:mem-aref ,@(second array-arg) ,i))))
                                                  ,result)))))))
                   (vk-alloc:with-foreign-allocated-objects (,@(remove-if (lambda (a)
                                                                            ;; the third element is actually the source
                                                                            ;; of the data to fill the arg with, but here
                                                                            ;; this should always be the same as the
                                                                            ;; array-arg-name
                                                                            (eq (third a) array-arg-name))
                                                                          translated-args))
                     (cffi:with-foreign-object (,@(second count-arg))
                       (let ((,(first (second array-arg)) (cffi:null-pointer))
                             (,result :incomplete))
                         (loop do (setf ,result (,vulkan-fun ,@vk-input-args))
                               while (eq ,result :incomplete)
                               finally (return (when (eq ,result :success)
                                                 (let ((,translated-count (cffi:mem-aref ,@(second count-arg))))
                                                   (cl:values
                                                    (when (> ,translated-count 0)
                                                      (vk-alloc:with-foreign-allocated-object (,@(second array-arg)
                                                                                                 (make-list ,translated-count :initial-element (,vk-constructor)))
                                                        (setf ,result (,vulkan-fun ,@vk-input-args))
                                                        (loop for ,i from 0 below ,translated-count
                                                              collect (cffi:mem-aref ,@(second array-arg) ,i))))
                                                    ,result))))))))))))))))

;; case 2d: return multiple values. one array of the same size as an input array and one additional non-array value - e.g. vkGetCalibratedTimestampsEXT
(defmacro defvk-get-array-and-singular-fun ((name vulkan-fun docstring required-args optional-args len-provider array-arg-name &optional (extension-p nil)) &body variables)
  "Defines a function named NAME which wraps VULKAN-FUN.
VULKAN-FUN is function that gets an array of values and a single value and returns a RESULT.
E.g. vkGetCalibratedTimestampsEXT"
  (multiple-value-bind (required-arg-names required-arg-declares) (process-args required-args nil)
    (multiple-value-bind (optional-arg-names optional-arg-declares) (process-args optional-args t extension-p)
      (multiple-value-bind (translated-args let-args vk-input-args output-args) (process-variables variables extension-p)
        (let ((array-arg (find-if (lambda (a)
                                    (eq (first a) array-arg-name))
                                  output-args))
              (other-output (find-if-not (lambda (a)
                                           (eq (first a) array-arg-name))
                                         output-args))
              (result (gensym "RESULT"))
              (i (gensym "INDEX")))
          `(defun ,name (,@required-arg-names &optional ,@optional-arg-names)
             ,docstring
             ,@required-arg-declares
             ,@optional-arg-declares
             (let (,@let-args)
               (vk-alloc:with-foreign-allocated-objects (,@translated-args)
                 (cffi:with-foreign-objects ((,@ (second array-arg) ,len-provider)
                                             (,@ (second other-output)))
                   (let ((,result (,vulkan-fun ,@vk-input-args)))
                     (cl:values (loop for ,i from 0 below ,len-provider
                                      collect (cffi:mem-aref ,@ (second array-arg) ,i))
                                (cffi:mem-aref ,@ (second other-output))
                                ,result)))))))))))

;;; --------------------------------------------- 3 output parameters ------------------------------------------------------------------

;; case 3: return two arrays using the same counter which is also an output argument - e.g vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR
(defmacro defvk-enumerate-two-arrays-fun ((name vulkan-fun docstring required-args optional-args count-arg-name array-arg-names &optional (extension-p nil)) &body variables)
  "Defines a function named NAME which wraps VULKAN-FUN.
VULKAN-FUN is function that enumerates two kinds of values and returns a RESULT.
E.g. vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR"
  (multiple-value-bind (required-arg-names required-arg-declares) (process-args required-args nil)
    (multiple-value-bind (optional-arg-names optional-arg-declares ignore-arg-declares) (process-args optional-args t) extension-p
      (multiple-value-bind (translated-args let-args vk-input-args output-args) (process-variables variables extension-p)
        (let ((count-arg (find-if (lambda (a)
                                    (eq (first a) count-arg-name))
                                  output-args))
              (array-args (remove-if-not (lambda (a)
                                           (find (first a) array-arg-names))
                                   output-args))
              (translated-count (gensym "COUNT"))
              (result (gensym "RESULT"))
              (i (gensym "INDEX"))
              (first-array (gensym (string (first array-arg-names))))
              (second-array (gensym (string (second array-arg-names)))))
          `(defun ,name (,@required-arg-names &optional ,@optional-arg-names)
             ,docstring
             ,@required-arg-declares
             ,@optional-arg-declares
             ,@ignore-arg-declares
             (let (,@let-args)
               (vk-alloc:with-foreign-allocated-objects (,@translated-args)
                 (cffi:with-foreign-object (,@ (second count-arg))
                   (let ((,(first (second (first array-args))) (cffi:null-pointer))
                         (,(first (second (second array-args))) (cffi:null-pointer))
                         (,result :incomplete))
                     (loop do (setf ,result (,vulkan-fun ,@vk-input-args))
                           while (eq ,result :incomplete)
                           finally (return (when (eq ,result :success)
                                             (let ((,translated-count (cffi:mem-aref ,@ (second count-arg))))
                                               (if (> ,translated-count 0)
                                                   (cffi:with-foreign-objects ((,@ (second (first array-args)) ,translated-count)
                                                                               (,@ (second (second array-args)) ,translated-count))
                                                     (setf ,result (,vulkan-fun ,@vk-input-args))
                                                     (loop for ,i from 0 below ,translated-count
                                                           collect (cffi:mem-aref ,@ (second (first array-args)) ,i) into ,first-array
                                                           collect (cffi:mem-aref ,@ (second (second array-args)) ,i) into ,second-array
                                                           finally (return (cl:values ,first-array ,second-array ,result))))
                                                   (cl:values nil nil ,result))))))))))))))))
