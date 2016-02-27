(defpackage #:cl-vulkan
  (:use :cl :cffi)
  (:nicknames :vk)
  (:import-from
   #:%vk
   #:get-device-proc-addr
   #:get-event-status
   #:get-fence-status
   #:get-instance-proc-addr
   #:get-physical-device-mir-presentation-support-khr
   #:get-physical-device-wayland-presentation-support-khr
   #:get-physical-device-win32-presentation-support-khr
   #:get-physical-device-xlib-presentation-support-khr
   #:get-physical-device-xcb-presentation-support-khr
   )
  (:export
   #:with-instance))
