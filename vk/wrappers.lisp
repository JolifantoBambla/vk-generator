(in-package #:cl-vulkan)

(defun api-version (major minor patch)
  (logior (ash major 22)
          (ash minor 12)
          patch))

(defun decode-version (v)
  (list (ldb (byte 12 22) v)
        (ldb (byte 10 12) v)
        (ldb (byte 12 0) v)))
(defparameter *api-version* (api-version 1 0 3))

(defparameter *debug-report-callback* nil)
(defparameter *debug-report-callback-handle* nil)

#++
(defmacro with-foreign-string-arrays (((pointer-var lisp-var)
                                       &rest more-var-defs)
                                      &body body)
  (let ((string-pointers (gensym (string lisp-var))))
    `(let* ((,string-pointers (map 'vector 'foreign-string-alloc ,lisp-var)))
       (unwind-protect
            (with-foreign-array (,pointer-var ,string-pointers
                                              `(:array :pointer ,(length ,lisp-var)))
              ,(if more-var-defs
                   `(with-foreign-string-arrays (,@more-var-defs)
                      ,@body)
                   `(progn ,@body)))
         (map nil 'foreign-string-free ,string-pointers)))))

(macrolet ((define-creator (fun &body args)
             (let ((structs
                     (loop for a in args
                           when (consp a)
                             collect (list (first a)
                                           (find-symbol (string (second a))
                                                        (find-package :%vk))
                                           (second a))))
                   (p (gensym "POINTER")))
               (print
                `(defun ,fun (,@(loop for a in args
                                      collect (if (consp a) (second a) a)))
                   (%vk::with-vk-structs (,@structs)
                     (with-foreign-object (,p :pointer)
                       (setf (mem-ref ,p :pointer) (null-pointer))
                       (,(intern (string-trim "%"(string fun))
                                 (find-package :%vk))
                        ,@(loop for a in args
                                      collect (if (consp a) (car a) a))
                        (null-pointer) ;; no allocator support for now...
                        ,p)
                       (let ((v (mem-ref ,p :pointer)))
                         (unless (null-pointer-p v) v)))))))))
  (define-creator %create-instance (ici instance-create-info))
  (define-creator %create-device physical-device (dci device-create-info))
  (define-creator %create-android-surface-khr instance (asci android-surface-create-info-khr))
  (define-creator %create-buffer device (bci buffer-create-info))
  (define-creator %create-buffer-view device (bvci buffer-view-create-info))
  (define-creator %create-command-pool device (cpci command-pool-create-info))
  #++
  (define-creator %create-compute-pipelines device pipeline-cache
    (cpci compute-pipeline-create-info :counted t))
  (define-creator %create-debug-report-callback-ext instance
    (drcci debug-report-callback-create-info-ext))
  (define-creator %create-descriptor-pool device (dpci descriptor-pool-create-info))
  (define-creator %create-descriptor-set-layout device (dslci descriptor-set-layout-create-info))
  (define-creator %create-display-mode-khr physical-device display
    (dmci display-mode-create-info-khr))
  (define-creator %create-display-plane-surface-khr instance (dsci display-surface-create-info-khr))
  (define-creator %create-event device (eci event-create-info))
  (define-creator %create-fence device (fci fence-create-info))
  (define-creator %create-framebuffer device (fci framebuffer-create-info))
  #++
  (define-creator %create-graphics-pipelines device pipeline-cache
    (gpci graphics-pipeline-create-info :counted t))
  (define-creator %create-image device (ici image-create-info))
  (define-creator %create-image-view device (ivci image-view-create-info))
  (define-creator %create-mir-surface-khr instance (msci mir-surface-create-info-khr))
  (define-creator %create-pipeline-cache device (pcci pipeline-cache-create-info))
  (define-creator %create-pipeline-layout device (plci pipeline-layout-create-info))
  (define-creator %create-query-pool device (qpci query-pool-create-info))
  (define-creator %create-render-pass device (rpci render-pass-create-info))
  (define-creator %create-sampler device (sci sampler-create-info))
  (define-creator %create-semaphore device (sci semaphore-create-info))
  (define-creator %create-shader-module device (smci shader-module-create-info))
  #++
  (define-creator %create-shared-swapchains-khr device (sci swapchain-create-info-khr :counted t))
  (define-creator %create-swapchain-khr device (sci swapchain-create-info-khr))
  (define-creator %create-wayland-surface-khr instance (wsci wayland-surface-create-info-khr))
  (define-creator %create-win32-surface-khr instance (wsci win32-surface-create-info-khr))
  (define-creator %create-xcb-surface-khr instance (xsci xcb-surface-create-info-khr))
  (define-creator %create-xlib-surface-khr instance (xsci xlib-surface-create-info-khr)))

(macrolet ((define-destroy (fun &body args)
             `(defun ,fun (,@args)
                (,(intern (string-trim "%"(string fun))
                          (find-package :%vk))
                 ,@args
                 ;; no allocator support for now...
                 (null-pointer)))))
  (define-destroy destroy-swapchain-khr device swapchain)
  (define-destroy destroy-command-pool device pool)
  (define-destroy destroy-image-view device image-view))



(defun %allocate-command-buffers (device comand-buffer-allocate-info)
  (%vk::with-vk-structs ((ai %vk:command-buffer-allocate-info
                        comand-buffer-allocate-info))
    (with-foreign-object (p '%vk:command-buffer
                          (getf comand-buffer-allocate-info
                                :command-buffer-count))
      (%vk:allocate-command-buffers device ai p)
      (loop for i below (getf comand-buffer-allocate-info
                              :command-buffer-count)
            collect (mem-aref p '%vk:command-buffer i)))))

#++
(macrolet
    ((wrap-creator (name with-name destroy (&rest args) (&rest call)
                    &key filter)
       (print
         `(progn
           (defun ,name (,@args)
             ,filter
             (,@call))
           (defmacro with-name ((var ,@args) &body body)
             `(let ((,var (,',name ,,@(loop with key = nil
                                            for a in args
                                            for an = (if (consp a)
                                                         (car a)
                                                         a)
                                            when (eq a '&key)
                                              do (setf key t)
                                            else
                                              append (if key
                                                         (list (alexandria:make-keyword an) an)
                                                         (list an))))))
                (unwind-protect
                     (progn ,@body)
                  (when ,var (,',destroy ,var (null-pointer))))))))))
  (wrap-creator create-instance with-instance %vk:destroy-instance
                (&key exts layers (app "cl-vulkan test")
                      (engine "cl-vulkan"))
                (%create-instance
                 `(:p-next nil
                   :flags 0
                   :p-application-info (:p-next nil
                                        :p-application-name ,app
                                        :application-version 0
                                        :p-engine-name ,engine
                                        :engine-version 0
                                        :api-version ,*api-version*)
                   :pp-enabled-layer-names ,layers
                   :pp-enabled-extension-names ,exts))
                :filter (setf exts (loop for x in exts
                                         when (keywordp x)
                                           collect (gethash x %vk::*extension-names* x)
                                         else collect x))))

(defmacro with-with ((with-name destroy
                       &key (allocator t)
                       pre-bindings
                       post-bindings)
                     &body defun)
  (let* ((name (second (first defun)))
         (fn-args (third (first defun)))
         (macro-args (loop for a in fn-args
                           if (consp a)
                             collect (list* (car a)
                                            `',(cadr a)
                                            (cddr a))
                           else collect a))
         (call-args (loop with key = nil
                          for a in fn-args
                          for an = (if (consp a)
                                       (car a)
                                       a)
                          when (eq a '&key)
                            do (setf key t)
                          else
                            append (if key
                                       (list (alexandria:make-keyword an) an)
                                       (list an)))))
    (print
     `(progn
        ,@defun
        (defmacro ,with-name ((var ,@macro-args) &body body)
          (flet ((pre-bindings ()
                   ,pre-bindings)
                 (post-bindings ()
                   ,post-bindings))
           `(let* (,@(pre-bindings)
                   (,var (,',name ,,@call-args))
                   ,@(post-bindings))
              (unwind-protect
                   (progn
                     ,@body)
                (when ,var (,@(list ',(car destroy) ,@ (cdr destroy)) ,var ,@',(when allocator `((null-pointer)))))))))))))

(defmacro without-fp-traps (&body body)
  #+(and sbcl (or x86 x86-64))
  `(sb-int:with-float-traps-masked (:invalid :divide-by-zero)
     ,@body)
  #-(and sbcl (or x86 x86-64))
  `(progn ,@body))

;; todo: add option to warn when optional layer/ext aren't found?
(defun check-layers (layers)
  (let ((available (mapcar (lambda (a)
                             (getf a :layer-name))
                           (vk:enumerate-instance-layer-properties))))
    (loop for .layer in layers
          for layer = (if (consp .layer) (car .layer) .layer)
          for optional = (when (consp .layer) (getf (cdr .layer) :optional))
          for found = (position layer available :test 'string=)
          when (and (not found) (not optional))
            do (error "required layer ~s not found.~%available: ~s" layer
                      available)
          when found
            collect layer)))

(defun check-extensions (extensions layers)
  ;; we need to check all the layers we will activate, since the
  ;; extension might be from one of them...
  (let ((available
          (loop for l in (cons (null-pointer) layers)
                append (mapcar (lambda (a)
                                 (getf a :extension-name))
                               (enumerate-instance-extension-properties l)))))
    (loop for .ext in extensions
          for ext = (if (consp .ext) (car .ext) .ext)
          for optional = (when (consp .ext) (getf (cdr .ext) :optional))
          for found = (position ext available :test 'string=)
          when (and (not found) (not optional))
            ;; todo: enumerate extensions from layers we don't activate so
            ;; we can give a more specific message if the desired extension
            ;; if from one of those.
            do (error "required extension ~s not found. Available: ~s" ext
                      available)
          when found
            collect ext
          else do (format t "missing extension ~s? / ~s~%" ext available))))


(with-with (with-instance (%vk:destroy-instance)
             :pre-bindings
             `((%vk::*instance-extensions* (make-hash-table))
               (%vk::*instance-params* (print (list :layers nil :exts nil))))
             :post-bindings
             `((%vk::*instance* ,var)))
  ;; EXTS and LAYERS are lists of names or (name &key optional)
  ;; extension names can be string or keyword for known extensions,
  ;; layer names are strings
  (defun create-instance (&key exts layers
                            (app "cl-vulkan test")
                            (app-version 0)
                            (engine "cl-vulkan")
                            (engine-version 1))
    (setf layers (check-layers layers))
    (setf exts (check-extensions
                (loop for x in exts
                      when (keywordp x)
                        collect (gethash x %vk::*extension-names* x)
                      else when (typep x '(cons keyword))
                             collect (list* (gethash (car x)
                                                     %vk::*extension-names*
                                                     (car x))
                                            (cdr x))
                      else collect x)
                layers))
    (when (boundp '%vk::*instance-params*)
      (setf (getf %vk::*instance-params* :exts) exts)
      (setf (getf %vk::*instance-params* :layers) layers)
      (print %vk::*instance-params*))
    (format t "creating instance with~% layers ~s~% exts ~s~%" layers exts)
    (without-fp-traps
     (%create-instance `(               ;:s-type :instance-create-info
                         :p-next nil
                         :flags 0
                         :p-application-info ( ;:s-type :application-info
                                              :p-next nil
                                              :p-application-name ,app
                                              :application-version ,app-version
                                              :p-engine-name ,engine
                                              :engine-version ,engine-version
                                              :api-version ,*api-version*)
                                        ;:enabled-layer-count ,(length layers)
                         :pp-enabled-layer-names ,layers
                                        ;:enabled-extension-count ,(length exts)
                         :pp-enabled-extension-names ,exts)))))

#++
(defmacro with-instance ((var &key exts layers (app "cl-vulkan test")
                                (engine "cl-vulkan"))
                         &body body)
  `(let ((,var (create-instance :exts ,exts :layers ,layers
                                :app ,app :engine ,engine)))
     (unwind-protect
          (progn ,@body)
       (when ,var
         (%vk:destroy-instance ,var (null-pointer))))))

(defcallback %debug-report-callback %vk:bool32
    ((flags %vk:debug-report-flags-ext)
     (objecttype %vk:debug-report-object-type-ext)
     (object :uint64)
     (location %vk::size-t)
     (messagecode :int32)
     (playerprefix :string)
     (pmessage/const :string)
     (puserdata/const (:pointer :void)))
  (declare (ignore puserdata/const))
  (when *debug-report-callback*
    (funcall *debug-report-callback*
             flags objecttype object location messagecode
             playerprefix pmessage/const))
  ;; if callback returns T, the calling API function will fail with
  ;; :error-validation-failed-ext. Better to just error here if we
  ;; want that though, so return NIL
  nil)

(defun default-debug-report-callback (flags object-type object location
                                      message-code player-prefix message)
  (declare (ignorable flags object-type object location
                      message-code player-prefix message))
  (progn ;unless (member :information flags) ;;todo: make this configurable
    (format t "~&~a:~s ~s~%"
            player-prefix flags message)
    #++(format t "code=~s, object=~s/~x, location=~s"
               message-code object object-type location))
  (when (member :error flags)
    (cerror "continue"
            "vulkan validation error ~s:~% ~s.~% code=~s, object=~s/~x, location=~s"
           player-prefix message message-code object-type object location)))

(defmacro with-debug-report ((instance &key (callback ''default-debug-report-callback))
                             &body body)
  (let ((cb (gensym "CALLBACK-HANDLE")))
   `(let ((*debug-report-callback* ,callback)
          (,cb (when (position "VK_EXT_debug_report"
                               (getf (print %vk::*instance-params*) :exts)
                               :test 'string=)
                 (%create-debug-report-callback-ext
                         ,instance `(:flags (:information
                                             :warning :performance-warning
                                             :error :debug)
                                     :pfn-callback ,(cffi:callback %debug-report-callback))))))
      (unwind-protect
           (progn ,@body)
        (when ,cb
          (%vk:destroy-debug-report-callback-ext ,instance ,cb (null-pointer)))))))


;; todo: add option to warn when optional layer/ext aren't found?
(defun check-device-layers (physical-device layers)
  (let ((available (mapcar (lambda (a)
                             (getf a :layer-name))
                           (enumerate-device-layer-properties physical-device))))
    (loop for .layer in layers
          for layer = (if (consp .layer) (car .layer) .layer)
          for optional = (when (consp .layer) (getf (cdr .layer) :optional))
          for found = (position layer available :test 'string=)
          when (and (not found) (not optional))
            do (error "required device layer ~s not found.~%available: ~s" layer
                      available)
          when found
            collect layer)))

(defun check-device-extensions (physical-device extensions layers)
  ;; we need to check all the layers we will activate, since the
  ;; extension might be from one of them...
  (let ((available
          (loop for l in (cons (null-pointer) layers)
                append (mapcar (lambda (a)
                                 (getf a :extension-name))
                               (enumerate-device-extension-properties
                                physical-device l)))))
    (loop for .ext in extensions
          for ext = (if (consp .ext) (car .ext) .ext)
          for optional = (when (consp .ext) (getf (cdr .ext) :optional))
          for found = (position ext available :test 'string=)
          when (and (not found) (not optional))
            ;; todo: enumerate extensions from layers we don't activate so
            ;; we can give a more specific message if the desired extension
            ;; if from one of those.
            do (error "required device extension ~s not found. Available: ~s" ext
                      available)
          when found
            collect ext)))



(with-with (with-device (%vk:destroy-device))
  (defun create-device (physical-device &key (queue-family-index 0)
                                          (priorities '(0.0))
                                          layers exts
                                          features)
    (setf layers (check-device-layers physical-device layers))
    (setf exts (check-device-extensions
                physical-device (loop for x in exts
                       when (keywordp x)
                         collect (gethash x %vk::*extension-names* x)
                       else collect x)
                layers))
    ;; todo: verify numbers are between 0.0 and 1.0 inclusive
    (if (consp queue-family-index)
        ;; if using multiple queue families, priorities must be an
        ;; equal length list of lists of numbers
        (assert (and (= (length queue-family-index)
                        (length priorities))
                     (every (lambda (a) (every 'realp a)) priorities)))
        ;; if we have 1 index, priorities must be a list of numbers from 0-1
        (assert (and (plusp (length priorities))
                     (every 'realp priorities))))
    (without-fp-traps
      (%create-device physical-device
                      (print `(:flags 0
                         :p-queue-create-infos
                         ,(loop for i in (alexandria:ensure-list
                                          queue-family-index)
                                for p in (if (consp queue-family-index)
                                             priorities
                                             (list priorities))
                                collect `(:flags 0
                                          :queue-family-index ,i
                                          :p-queue-priorities ,p))
                         :pp-enabled-layer-names ,layers
                         :pp-enabled-extension-names ,exts
                         :p-enabled-features ,features))))))

#++
(defmacro with-device ((var  physical-device
                        &key (queue-family-index 0)
                          (priorities ''(1.0))
                          layers exts
                          (features '(get-physical-device-features physical-device)))
                         &body body)
  `(let ((,var (create-device ,physical-device
                              :queue-family-index ,queue-family-index
                              :priorities ,priorities
                              :layers ,layers
                              :exts ,exts
                              :features ,features)))
     (unwind-protect
          ,@body
       (when ,var
         (%vk:destroy-device ,var (null-pointer))))))

(with-with (with-win32-surface-khr (%vk:destroy-surface-khr instance))
  (defun create-win32-surface-khr (instance hinstance hwnd)
    (%create-win32-surface-khr instance `(:flags 0
                                          :hinstance ,hinstance
                                          :hwnd ,hwnd))))

(with-with (with-swapchain (%vk:destroy-swapchain-khr device))
  (defun create-swapchain-khr (device surface
                               width height
                               &key (min-image-count 1) ;; single buffer
                                 (image-format :r8g8b8a8-uint)
                                 (image-color-space :srgb-nonlinear-khr)
                                 (image-array-layers 1)
                                 ;; image-usage-flags
                                 (image-usage '(:color-attachment
                                                :depth-stencil-attachment))
                                 ;; sharing-mode
                                 (image-sharing-mode :exclusive)
                                 (queue-family-indices '())
                                 ;; surface-transform-flag-bits-khr
                                 (pre-transform :identity)
                                 (composite-alpha :opaque)
                                 (present-mode :fifo-khr) ;; vsync
                                 (clipped t)
                                 (old-swapchain nil))
    (%create-swapchain-khr device
                           `(:p-next ,(null-pointer)
                             :flags 0
                             :surface ,surface
                             :min-image-count ,min-image-count
                             :image-format ,image-format
                             :image-color-space ,image-color-space
                             :image-extent (:width ,width :height ,height)

                             :image-array-layers ,image-array-layers
                             :image-usage ,image-usage
                             :image-sharing-mode ,image-sharing-mode
                             :p-queue-family-indices ,queue-family-indices
                             :pre-transform ,pre-transform
                             :composite-alpha ,composite-alpha
                             :present-mode ,present-mode
                             :clipped ,clipped
                             :old-swapchain ,(or old-swapchain
                                                 (null-pointer))))))

(with-with (with-fence (%vk:destroy-fence device))
  (defun create-fence (device &key signaled)
    (%create-fence device `(:flags ,(if signaled '(:signaled) 0)))))

(with-with (with-semaphore (%vk:destroy-semaphore device))
  (defun create-semaphore (device)
    (%create-semaphore device `(:flags 0))))


(with-with (with-command-pool (%vk:destroy-command-pool device))
  (defun create-command-pool (device &key (queue-family 0)
                                       flags)
    (%create-command-pool device `(:flags ,(or flags 0)
                                   :queue-family-index ,queue-family))))

(with-with (with-command-buffers (free-command-buffers device command-pool)
             :allocator nil)
  (defun allocate-command-buffers (device command-pool count
                                   &key (level :primary))
    (%allocate-command-buffers device
                               `(:command-pool ,command-pool
                                 :level ,level
                                 :command-buffer-count ,count))))

(defmacro with-commands ((command-buffer &key begin-flags) &body body)
  `(progn
     (%vk::with-vk-structs ((bi %vk:command-buffer-begin-info
                                '(:flags ,(or (alexandria:ensure-list begin-flags)
                                           0)
                                  :p-inheritance-info
                                  (   ;:render-pass nil;; render-pass
                                   :subpass 0
                                        ;:framebuffer nil;; framebuffer
                                   :occlusion-query-enable nil
                                   ;; query-control-flags
                                   :query-flags 0
                                   ;; query-pipeline-statistic-flags
                                   :pipeline-statistics 0))))
      (%vk:begin-command-buffer ,command-buffer bi)
       ,@body
       (%vk:end-command-buffer ,command-buffer))))



(defun create-image-view (device image
                          &key flags
                            (view-type :2d)
                            (format)
                            (components '(:r :r :g :g :b :b :a :a))
                            (subresource-range '(:aspect-mask :color
                                                 :base-mip-level 0
                                                 :level-count 1
                                                 :base-array-layer 0
                                                 :layer-count 1)))
  (%create-image-view device `(:image ,image
                               :format ,format
                               :flags ,flags
                               :view-type ,view-type
                               :components ,components
                               :subresource-range ,subresource-range)))



;; fixme: handle case where amount of objects changes between when we
;; query count and when we query actual values.
;; (possibly for example in enumerate-instance-layer/extension-properties
;;  if someone installs more at that exact instant)
(defun enumerate-physical-devices (instance)
  (with-foreign-object (p-count :uint32)
    (setf (mem-ref p-count :uint32) 0)
    (%vk:enumerate-physical-devices instance p-count (null-pointer))
    (let ((count (mem-ref p-count :uint32)))
      (with-foreign-object (phys '%vk:physical-device count)
        (let ((ret (%vk:enumerate-physical-devices instance p-count phys)))
          (values (loop for i below count
                        collect (mem-aref phys '%vk:physical-device i))
                  ret))))))

(macrolet
    ((getter (.object fun .type &rest args)
       (let* ((%vk (find-package '%vk))
              (counted (typep .type '(cons (eql :count))))
              (type-name (if (consp .type) (second .type) .type))
              (vkfun (find-symbol (string fun) %vk))
              (deref (find-symbol (format nil "DEREF-~a" type-name) %vk))
              (type (if deref `(:struct ,type-name) type-name))
              (object (when .object (list .object))))
         (flet ((ref (p)
                      (if deref
                          `(,deref ,p)
                          `(mem-ref ,p ',type))))
           (assert vkfun () "~s not found?" fun)
           (if counted
               (print `(defun ,fun (,@object ,@args)
                   (let ((count 0))
                     (with-foreign-object (c :uint32)
                       (setf (mem-ref c :uint32) 0)
                       (,vkfun ,@object ,@args c (null-pointer))
                       (setf count (mem-ref c :uint32))
                       (with-foreign-object (p ',type count)
                         (,vkfun ,@object ,@args c p)
                         (loop for i below count
                               collect ,(ref
                                         `(inc-pointer p
                                                       (* i ,(foreign-type-size
                                                              type))))))))))
               (print  `(defun ,fun (,@object ,@args)
                          (with-foreign-object (p ',type)
                            (,vkfun ,@object ,@args p)
                            ,(ref 'p))))))))
     (getters (object &body defs)
       (cons 'progn
             (loop for def in defs
                   collect `(getter ,object ,@def)))))

  (getters nil
           (enumerate-instance-extension-properties (:count %vk:extension-properties) layer-name)
           (enumerate-instance-layer-properties (:count %vk:layer-properties))
)

  (getters device
           (get-buffer-memory-requirements %vk:memory-requirements buffer)
           (get-image-memory-requirements %vk:memory-requirements image)
           (get-image-sparse-memory-requirements (:count %vk:sparse-image-memory-requirements) image)
           (get-render-area-granularity %vk:extent-2d render-pass)
           (get-swapchain-images-khr (:count %vk:image) swapchain)
           (get-device-memory-commitment %vk:device-size memory)
           (acquire-next-image-khr :uint32 swapchain timeout semaphore fence)
)

  (getters physical-device
           (enumerate-device-extension-properties (:count %vk:extension-properties) layer-name)
           (enumerate-device-layer-properties (:count %vk:layer-properties))
           (get-display-mode-properties-khr (:count %vk:display-mode-properties-khr) display)
           (get-display-plane-capabilities-khr %vk:display-plane-capabilities-khr mode plane-index)
           #++(get-display-plane-supported-displays-khr (:count %vk:display-khr) plane-index)
           (get-physical-device-display-plane-properties-khr (:count %vk:display-plane-properties-khr))
           (get-physical-device-display-properties-khr (:count %vk:display-properties-khr))
           (get-physical-device-features %vk:physical-device-features)
           (get-physical-device-format-properties %vk:format-properties format)
           (get-physical-device-image-format-properties %vk:image-format-properties format type tiling usage flags)
           (get-physical-device-memory-properties %vk:physical-device-memory-properties)
           (get-physical-device-properties %vk:physical-device-properties)
           (get-physical-device-queue-family-properties (:count %vk:queue-family-properties))
           (get-physical-device-sparse-image-format-properties (:count %vk:sparse-image-format-properties) format type samples usage tiiling)
           (get-physical-device-surface-capabilities-khr %vk:surface-capabilities-khr surface)
           (get-physical-device-surface-formats-khr (:count %vk:surface-format-khr) surface)
           (get-physical-device-surface-present-modes-khr (:count %vk:present-mode-khr) surface)))

#++
(defun get-device-memory-commitment (device memory)
  (with-foreign-object (p '%vk:device-size)
    (%vk:get-device-memory-commitment device memory p)
    (mem-ref p '%vk:device-size)))

(defun get-device-queue (device queue-family-index queue-index)
  (with-foreign-object (p '%vk:queue)
    (%vk:get-device-queue device queue-family-index queue-index p)
    (mem-ref p '%vk:queue)))

(defun get-physical-device-surface-support-khr (physical-device queue-family-index surface)
  (with-foreign-object (p '%vk:bool32)
    (%vk:get-physical-device-surface-support-khr physical-device queue-family-index surface p)
    (mem-ref p '%vk:bool32)))


(defun get-image-subresource-layout (device image subresource)
  (%vk::with-vk-structs ((sr %vk:image-subresource subresource))
    (with-foreign-object (p '(:struct %vk:subresource-layout))
      (%vk:get-image-subresource-layout device image sr p)
      (%vk::deref-physical-device-memory-properties p))))


(macrolet
    ((wrap-counted-args (name &body args)
       (let ((pointers (loop for a in args
                             if (consp a)
                               collect (gensym "P")
                             else collect nil))
             (i (gensym "I"))
             (o (gensym "O")))
         (print
          `(defun ,name ,(loop for a in args
                                if (consp a)
                                  collect (car a)
                                else collect a)
             (with-foreign-objects
                 (,@(loop for a in args
                          for p in pointers
                          when (consp a)
                            collect (list p `',(second a)
                                          (if (eq (third a) :pointer)
                                              1
                                              `(length ,(first a))))))
               ,@(loop for a in args
                       for p in pointers
                       for at = (when (consp a) (second a))
                       for setter = `(setf (mem-aref ,p ',at ,i) ,o)
                       when (consp a)
                         do (when (consp at)
                              (setf setter
                                    `(,(find-symbol (format nil "FILL-~a"
                                                            (second at))
                                                    (find-package :%vk))
                                      ,p ,o)))
                         and collect (if (eq (third a) :pointer)
                                         `(let ((,i 0)
                                                (,o ,(first a)))
                                            (declare (ignorable ,i))
                                            ,setter)
                                         `(loop for ,o in ,(first a)
                                                  for ,i from 0
                                                  do ,setter)))
               (,(find-symbol (string name) (find-package :%vk))
                ,@(loop for a in args
                        for p in pointers
                        if (and (consp a) (not (eq (third a) :pointer)))
                          collect `(length ,(first a))
                        if (consp a) collect p
                        else collect a))))))))
  (wrap-counted-args reset-fences device (fences %vk:fence))
  (wrap-counted-args wait-for-fences device (fences %vk:fence) wait-all timeout)
  (wrap-counted-args free-command-buffers device command-pool (command-buffers %vk:command-buffer))
  (wrap-counted-args cmd-clear-color-image command-buffer image image-layout
                     (color (:union %vk:clear-color-value) :pointer)
                     (ranges (:struct %vk:image-subresource-range))))


;; not sure about full version of this, API would be messier, and probably
;; if submitting a bunch at once mattered, we'd also want to pre-allocate
;; and reuse the foreign struct anyway
(defun queue-submit1 (queue buffer &key wait-semaphores wait-dst-stage-mask
                                     signal-semaphores
                                     fence)
  "simplified queue submit that only accepts a single command
buffer (or single batch of command buffers with same waits/signals)"
  (%vk::with-vk-structs ((vsi %vk:submit-info
                              `(:wait-semaphores ,wait-semaphores
                                :wait-dst-stage-mask ,wait-dst-stage-mask
                                :signal-semaphores ,signal-semaphores
                                :buffers ,(alexandria:ensure-list buffer))))
    (%vk:queue-submit queue 1 vsi
                      (or fence (cffi:null-pointer)))))

(defun cmd-pipeline-barrier (command-buffer
                             src-stage-mask dst-stage-mask
                             &key dependency-flags
                               memory-barriers
                               buffer-memory-barriers
                               image-memory-barriers)
  (let ((lmb (length memory-barriers))
        (lbmb (length buffer-memory-barriers))
        (limb (length image-memory-barriers)))
    (%vk::with-vk-structs ((mb %vk:memory-barrier memory-barriers lmb)
                           (bmb %vk:buffer-memory-barrier buffer-memory-barriers lbmb)
                           (imb %vk:image-memory-barrier image-memory-barriers limb))
      (%vk:cmd-pipeline-barrier command-buffer src-stage-mask dst-stage-mask
                                dependency-flags
                                lmb mb
                                lbmb bmb
                                limb imb))))
