;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-user)

;; (pushnew :lparallel.with-debug *features*)

(asdf:defsystem :dstm-collections
  :description "Functional Collections and Lock Free DSTM"
  :long-description "Red-Black Trees / Dynamic Software Transactional Memory after Herlihy, et. al."
  :version "0.2.0"
  :author "Dr David McClain"
  :author "Dan Lentz"
  :serial t
  :depends-on (:closer-mop :lparallel :named-readtables :cl-store :local-time)
  :components ((:file "package")
                (:file "dstm-collections")
                (:file "printv")
                (:file "quad")
                (:file "dstm")
                (:file "ord")
                (:file "tree")
                (:file "set")
                (:file "map")
                (:file "seq")
                (:file "printer")
                (:file "reader")
                (:file "cursor")
                ))


(defmethod asdf:perform :after ((op asdf:load-op) (sys (eql (asdf:find-system :dstm-collections))))
  (pushnew :dstm *features*)
  (let ((dclx-package  (find-package :dstm-collections)))
    (when (symbol-value (intern (symbol-name :*default-syntax-startup-enabled*) dclx-package))
      (funcall (intern (symbol-name :enable-syntax) dclx-package)))
    (when (symbol-value (intern (symbol-name :*default-kernel-startup-enabled*) dclx-package))
      (funcall (intern (symbol-name :enable-kernel) dclx-package)))))


(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :dstm-collections))))
  (asdf:load-system :dstm-collections-test)
  (funcall (intern (symbol-name :funcall-test-with-feedback-message)
             (find-package :hu.dwim.stefil))
    (read-from-string "'dstm-collections-test::dstm-collections")))
 
