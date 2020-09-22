;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; generate.lisp --- generate CFFI bindings from vk.xml file.
;;;
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

(in-package :vk-generator/vk-spec)


(defparameter *special-words*
  '("Bool32" "Win32"
    "16" "32" "64"
    "3d" "2d" "1d"
    "3D" "2D" "1D"
    "ID" "UUID"
    "HINSTANCE" "HWND" "HANDLE" "DWORD" "LPCWSTR" "SECURITY_ATTRIBUTES"
    "ETC2" "ASTC" "ASTC_" "LDR" "BC" "RR")
  "A sequence of special words that are used in the specification of the Vulkan API.")

(defparameter *vk-platform*
  (alexandria:plist-hash-table
   '("void" :void
     "char" :char
     "float" :float
     "double" :double ;; added this after v1.1.119 failed (not sure which version added this)
     "uint8_t" :uint8
     "uint16_t" :uint16 ;; added this after v1.1.93 failed (not sure which version added this)
     "uint32_t" :uint32
     "uint64_t" :uint64
     "int32_t" :int32
     "int64_t" :int64 ;; added this after v1.1.119 failed (not sure which version added this)
     "int" :int
     "size_t" size-t)
   :test 'equal))

(defparameter *opaque-types*
  '("a-native-window"
    "a-hardware-buffer" ;; added in v1.1.71
    "mir-connection"
    "mir-surface"
    "xcb_connection_t"
    "display"))

(defparameter *opaque-struct-types*
  '("wl_display"
    "wl_surface"
    "SECURITY_ATTRIBUTES"))

(defparameter *fix-must-be*
  (alexandria:alist-hash-table
   '((:pipeline-iinput-assembly-state-create-info
      . :pipeline-input-assembly-state-create-info)
     (:debug-report-callback-create-info
      . :debug-report-create-info-ext))))

(defparameter *misc-os-types*
  '("hinstance" (:pointer :void)
    "hwnd" (:pointer :void)
    "HANDLE" (:pointer :void)
    "DWORD" :uint32
    "LPCWSTR" (:pointer :void)
    "RROutput" :ulong
    "xcb_window_t" :uint32
    "xcb_visualid_t" :uint32
    "window" :ulong
    "visual-id" :ulong))
