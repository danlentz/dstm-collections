;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-user)


(asdf:defsystem :dstm-collections
  :description "Functional Collections and Lock Free DSTM"
  :long-description "Red-Black Trees / Dynamic Software Transactional Memory after Herlihy, et. al."
  :version "0.2.0"
  :author "Dr David McClain"
  :maintainer "Dan Lentz"
  :serial t
  :depends-on (:closer-mop :eager-future2 :named-readtables)
  :components ((:file "package") 
                (:file "quad")
                (:file "dstm")
                (:file "ord")
                (:file "tree")
                (:file "set")
                (:file "map")
                (:file "seq")
         ;;;       (:file "readtable")
                ))


(defmethod asdf:perform :after ((op asdf:load-op) (sys (eql (asdf:find-system :dstm-collections))))
  (pushnew :dstm *features*))


(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :dstm-collections))))
  (asdf:load-system :dstm-collections-test)
  (funcall (intern (symbol-name :dstm-collections) (find-package :dstm-collections-test))))


(defmethod asdf:operation-done-p ((o asdf:test-op) (c (eql (asdf:find-system :dstm-collections))))
  nil)
