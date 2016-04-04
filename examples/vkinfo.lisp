(in-package #:vk)

(vk:with-instance (instance)
  (when instance
    (let ((devices (enumerate-physical-devices instance)))
      (format t "~&instance layers: ~s~%"
              (enumerate-instance-layer-properties))
      (format t "~&instance extensions: ~s~%"
              (enumerate-instance-extension-properties ""))
      (format t "~&got ~d devices~%" (length devices))
      (loop for device in devices
            for i from 0
            for props = (get-physical-device-properties device)
            do (format t "device ~d: ~a~%"
                       i (getf props :device-name))
               (format t "device layers: ~s~%"
                       (enumerate-device-layer-properties device))
               (format t "device extensions: ~s~%"
                       (enumerate-device-extension-properties device ""))
               (format t "queue families: ~s~%"
                       (get-physical-device-queue-family-properties device))
               (format t "  limits:~%    ~{~s ~s~^~%    ~}~%" (getf props :limits))
               (remf props :limits)
               (format t "  properties:~%    ~{~s ~s~^~%    ~}~%" props)
               (format t "  features:~%    ~{~s ~S~^~%    ~}~%"
                       (get-physical-device-features device))
               (let ((format :r8-snorm))
                 (format t "  properties of format ~s :~%~{    ~s ~s~%~}" format
                         (get-physical-device-format-properties device format)))
               (format t "  physical device memory properties:~%~{    ~s ~s~%~}"
                       (get-physical-device-memory-properties device))))))
