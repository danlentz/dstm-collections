;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-user)


(asdf:defsystem :dstm-collections
  :description "Functional Collections and Lock Free DSTM"
  :long-description "Red-Black Trees / Dynamic Software Transactional Memory after Herlihy, et. al."
  :version "0.2.0"
  :author "Dr David McClain"
  :author "Dan Lentz"
  :serial t
  :depends-on (:closer-mop :eager-future2 :named-readtables :cl-store :local-time)
  :components ((:file "package")
                (:file "dstm-collections")
                (:file "quad")
                (:file "dstm")
                (:file "ord")
                (:file "tree")
                (:file "set")
                (:file "map")
                (:file "seq")
                (:file "printer")
                (:file "reader")
                ))


(defmethod asdf:perform :after ((op asdf:load-op) (sys (eql (asdf:find-system :dstm-collections))))
  (pushnew :dstm *features*)
  (read-from-string "#.(setf eager-future2:*default-future-type* :eager)")
  (if (symbol-value (intern (symbol-name :*default-syntax-startup-enabled*)
                      (find-package :dstm-collections)))
    (funcall (intern (symbol-name :enable-syntax)
                      (find-package :dstm-collections)))))
  


(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :dstm-collections))))
  (asdf:load-system :dstm-collections-test)
  (funcall (intern (symbol-name :funcall-test-with-feedback-message)
             (find-package :hu.dwim.stefil)) (read-from-string
                                               "'dstm-collections-test::dstm-collections")))
 
