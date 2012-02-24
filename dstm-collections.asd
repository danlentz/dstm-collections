;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-user)

#-cldoc
(eval-when (:load-toplevel :compile-toplevel :execute)
  (asdf:load-system :cldoc))

#+cldoc
(import '(cldoc:doc-op cldoc:document-system cldoc:cldoc cldoc:print-op cldoc:print-system))



(asdf:defsystem :dstm-collections
  :description "Functional Collections and Lock Free DSTM"
  :long-description "Software Transactional Collections"
  :version "0.2.0"
  :author "Dr David McClain"
  :author "Dan Lentz"
  :serial t
  :depends-on (:cldoc :unicly :closer-mop :lparallel :named-readtables :local-time :contextl)
  :components ((:file "package")
                (:file "dstm-collections")
                (:file "printv")
                (:file "util")
                (:file "quad")
                (:file "mmap-stream")
                (:file "cstm")
                (:file "dstm")
                (:file "ord")
                (:file "wbtree")
                (:file "tree")
                (:file "cursor")
                (:file "set")
                (:file "map")
                (:file "seq")
                (:file "printer")
                (:file "reader")
#+cldoc         (:cldoc          :dstm-collections-documentation
                                 :target-system :dstm-collections
                                 :pathname "doc/html/")))


(defmethod asdf:perform :after ((op asdf:load-op) (sys (eql (asdf:find-system :dstm-collections))))
  (pushnew :dstm *features*)
  (let ((dclx-package  (find-package :dstm-collections)))
    (when (symbol-value (intern (symbol-name :*default-syntax-startup-enabled*) dclx-package))
      (funcall (intern (symbol-name :enable-syntax) dclx-package)))
    (when (symbol-value (intern (symbol-name :*default-kernel-startup-enabled*) dclx-package))
      (funcall (intern (symbol-name :ensure-kernel) dclx-package)))))


(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :dstm-collections))))
  (asdf:load-system :dstm-collections-test)
  (funcall (intern (symbol-name :funcall-test-with-feedback-message)
             (find-package :hu.dwim.stefil))
    (read-from-string "'dstm-collections-test::dstm-collections")))
 
