;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-user)


(asdf:defsystem :dstm-collections
  :serial t
  :depends-on (:closer-mop :bordeaux-threads)
  :description "Functional Collections and Lock Free DSTM"
  :long-description "Red-Black Trees / Dynamic Software Transactional Memory after Herlihy, et. al."
  :components ((:file "package") 
                (:file "quad")
                (:file "ord")
                (:file "tree")
                (:file "set")
                (:file "map")
                (:file "seq")                               
                #+(or sbcl lispworks)
                (:file "dstm")))


(defmethod asdf:perform :after ((op asdf:load-op) (sys (eql (asdf:find-system :dstm-collections))))
  (pushnew :dstm *features*))


(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :dstm-collections))))
  (asdf:load-system :dstm-collections-test)
  (funcall (intern (symbol-name :dstm-collections) (find-package :dstm-collections-test))))


(defmethod asdf:operation-done-p ((o asdf:test-op) (c (eql (asdf:find-system :dstm-collections))))
  nil)
