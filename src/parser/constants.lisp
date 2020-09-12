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


(in-package :vk-generator/parser/constants)

(defparameter *special-words* '("Bool32" "Win32"
                                "16" "32" "64"
                                "3d" "2d" "1d"
                                "3D" "2D" "1D"
                                "ID" "UUID"
                                "HINSTANCE" "HWND" "HANDLE" "DWORD" "LPCWSTR" "SECURITY_ATTRIBUTES"
                                "ETC2" "ASTC" "ASTC_" "LDR" "BC" "RR"))

(defparameter *fix-must-be*
  (alexandria:alist-hash-table
   '((:pipeline-iinput-assembly-state-create-info
      . :pipeline-input-assembly-state-create-info)
     (:debug-report-callback-create-info
      . :debug-report-create-info-ext))))
