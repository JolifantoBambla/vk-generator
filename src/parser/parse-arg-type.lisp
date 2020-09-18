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

(in-package :vk-generator/parser/parse-arg-type)


(defun parse-arg-type (node gt vendor-ids api-constants handle-types &key stringify)
  (let* ((type-node (xpath:evaluate "type" node))
         (.type (xps type-node))
         (values (xps (xpath:evaluate "@values" node)))
         (len (xps (xpath:evaluate "@len" node)))
         (optional (xps (xpath:evaluate "@optional" node)))
         (name (cffi:translate-camelcase-name
                (xps (xpath:evaluate "name" node))
                :special-words *special-words*))
         (type (or (gethash .type *vk-platform*) (fix-type-name .type vendor-ids)))
         (prefix (xps (xpath:evaluate "type/preceding-sibling::text()" node)))
         (suffix (xps (xpath:evaluate "type/following-sibling::text()" node)))
         (namesuf (xps (xpath:evaluate "name/following-sibling::text()" node)))
         (enum (xps (xpath:evaluate "enum" node)))
         (following (xps (xpath:evaluate "following-sibling::comment()" node)))
         (desc))
    (assert (not (set-difference (attrib-names node)
                                                                  ;; todo: provide different sets based on version-tag
                                 '("len" "optional" "values"
                                   ;; todo:the following attributes are not handled yet
                                   "externsync"

                                   ;; this is deprecated in version v1.1.72 because it's implied by "structextends" in <type>
                                   "noautovalidity"
                                   
                                   ;; deprecated in version v1.0.55-core
                                   "validextensionstructs"

                                   ;; v1.0.61-core adds
                                   "altlen"

                                   ;; v1.2.142 adds
                                   "selection" "selector"
                                   )
                                 :test 'string=)))
    ;; just hard coding the const/pointer stuff for
    ;; now. adjust as the spec changes...
    (when prefix
      (assert (member prefix '("struct" "const"
                               ;; todo: only from v1.1.71
                               "const struct")
                      :test 'string=)))
    (when suffix
      (assert (member suffix '("*" "**" "* const*") :test 'string=)))
    ;; fixme: do something with the const? (generate comment if nothing else)
        (when gt
      (format t "@@@~s/~s (~s ~s)~%  -> ~s~%"
              name .type prefix suffix
              (funcall gt type)))
    (flet ((struct-type ()
             (if (and gt
                      (member (funcall gt type)
                              '(:struct :union)))
                 (list (funcall gt type) type)
                 type)))
      (cond
        ((and (or (not prefix)
                  (string= prefix "const"))
              (not suffix))
         (setf desc (list name (struct-type))))
        ;; todo: "const struct *" was added in v1.1.70 - I guess it should be handled like "struct *", but it should be tested
        ((and (string= prefix "const struct")
              (string= suffix "*"))
         (setf desc `(,name (:pointer (:struct ,type)))))
        ((and (string= prefix "struct")
              (string= suffix "*"))
         (setf desc `(,name (:pointer (:struct ,type)))))
        ;; todo "struct **" was added in v1.1.71 - test this
        ((and (string= prefix "struct")
              (string= suffix "**"))
         (setf desc `(,name (:pointer (:pointer (:struct ,type))))))
        ((and (or (not prefix)
                  (string= prefix "const"))
              (string= suffix "*"))
         (setf desc `(,name (:pointer ,(struct-type)))))
               ((and (string= prefix "const")
              (string= suffix "* const*"))
         (setf desc `(,name (:pointer (:pointer ,(struct-type))))))
        ((and (not prefix)
              (string= suffix "**"))
         (setf desc `(,name (:pointer (:pointer ,(struct-type))))))
        (t
         (error "failed to translate type ~s ~s ~s?" prefix type suffix)
         #++(setf desc (list :??? name type prefix suffix)))))
    (cond
      ((and stringify
            (equalp (second desc) '(:pointer :char))
            (string= len "null-terminated"))
       (setf (second desc) :string))
      #++((string= len "null-terminated")
       (error "unhandled len=~s" len)))
    (cond
      ((and values (alexandria:starts-with-subseq "VK_STRUCTURE_TYPE_" values))
       (let ((k (make-keyword
                 (substitute #\- #\_ (subseq values (length "VK_STRUCTURE_TYPE_"))))))
         (setf (getf (cddr desc) :must-be) (gethash k *fix-must-be* k)))))
    (when (or (find type *opaque-types* :test 'string-equal)
              (find type *opaque-struct-types* :test 'string-equal)
              (gethash .type handle-types)
              (eql type :void))
      (format t "  opaque!~%")
      (setf (getf (cddr desc) :opaque) t))
        (when len
      (setf (getf (cddr desc) :len)
            (mapcar (lambda (a)
                      (cond
                        ((string= a "null-terminated")
                         :null-terminated)
                        ((alexandria:starts-with-subseq "latexmath:" a)
                         a)
                        ;; try to catch unmarked equations
                        ((ppcre:scan "[:+-/*]" a)
                         a)
                        (t
                         (make-keyword
                          (cffi:translate-camelcase-name
                           a :special-words *special-words*)))))
                    (split-sequence:split-sequence #\, len))))
    (when optional
      (setf (getf (cddr desc) :optional)
            (mapcar 'make-keyword
                    (split-sequence:split-sequence #\, optional))))
    (when enum
      (assert (gethash enum api-constants)))
    (if enum
        (push (gethash enum api-constants) (cddr desc))
        (push nil (cddr desc)))
    (ppcre:register-groups-bind (x) ("\\[(\\d+)\\]" namesuf)
      (when x
        (setf (caddr desc) (parse-integer x))))
    desc))
