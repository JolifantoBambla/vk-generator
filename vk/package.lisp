(defpackage #:cl-vulkan
  (:use :cl :cffi)
  (:nicknames :vk)
  (:import-from
   #:%vk
   #:get-device-proc-addr
   #:get-event-status
   #:get-fence-status
   #:get-instance-proc-addr
   ))
