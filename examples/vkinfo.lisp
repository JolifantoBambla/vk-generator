(in-package #:vk)

(defvar *instance* (create-instance))

(let ((devices (enumerate-physical-devices *instance*)))
  (format t "~&got ~d devices~%" (length devices))
  (loop for device in devices
        for i from 0
        for props = (get-physical-device-properties device)
        do (format t "device ~d: ~a~%"
                   i (getf props :device-name))
           (format t "  limits:~%    ~{~s ~s~^~%    ~}~%" (getf props :limits))
           (remf props :limits)
           (format t "  properties:~%    ~{~s ~s~^~%    ~}~%" props)
           (format t "  features:~%    ~{~s ~S~^~%    ~}~%"
                   (get-physical-device-features device))))

(%vk::destroy-instance *instance* (null-pointer))
