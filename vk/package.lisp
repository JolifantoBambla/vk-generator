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
   #:with-instance
   #:enumerate-physical-devices
   #:get-physical-device-properties
   #:get-physical-device-queue-family-properties
   #:get-physical-device-memory-properties
   #:with-device
   #:get-physical-device-features
   #:decode-version
   #:enumerate-instance-layer-properties
   #:enumerate-instance-extension-properties
   #:with-debug-report
   #:get-physical-device-surface-support-khr
   #:get-physical-device-mir-presentation-support-khr
   #:get-physical-device-wayland-presentation-support-khr
   #:get-physical-device-win32-presentation-support-khr
   #:get-physical-device-xlib-presentation-support-khr
   #:get-physical-device-xcb-presentation-support-khr
   #:with-win32-surface-khr
   #:get-physical-device-surface-formats-khr
   #:with-swapchain
   #:get-physical-device-surface-present-modes-khr))
