;;; Copyright (c) 2016, Bart Botta  <00003b@gmail.com>
;;;   All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;

(in-package :vk-generator)

(defun numeric-value (str)
  (cond
    ((not str) nil)
    ((alexandria:starts-with-subseq "0x" str)
     (parse-integer str :start 2 :radix 16))
    ((ignore-errors (parse-integer str)))
    ((and (alexandria:ends-with #\f str)
          (ignore-errors (parse-number:parse-number str :end (1- (length str))))))
    ;; todo: test and clean up
    ((multiple-value-bind (m matches)
         (ppcre:scan-to-strings "\\(~0U(LL)?(-1)?(-2)?\\)" str)
       (when m
         ;; fixme: is this right on all platforms? (or any for that matter?)
         ;; this works: (at least on my machine)
         ;; (~0U)   should be e.g.  VK_REMAINING_MIP_LEVELS: 4294967295
         ;; (~0ULL) should be e.g.  VK_WHOLE_SIZE: 18446744073709551615
         ;; (~0U-1) should be e.g.  VK_QUEUE_FAMILY_EXTERNAL_KHR: 4294967294
         ;; (~0U-2) should be e.g.  VK_QUEUE_FAMILY_FOREIGN_EXT: 4294967293
         (let ((off (cond ((aref matches 1) -2)
                          ((aref matches 2) -3)
                          (t -1))))
           (if (aref matches 0)
              (ldb (byte 64 0) off)
              (if (= 4 (cffi:foreign-type-size :uint))
                  (ldb (byte 32 0) off)
                  (ldb (byte 64 0) off)))))))
    (t
     (error "~s" str))))

