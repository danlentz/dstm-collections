;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-user)


(asdf:defsystem :dstm-collections
  :serial t
  :depends-on (:bordeaux-threads)
  :components ((:file "package") 
                (:file "quad")
                (:file "ord")
                (:file "tree")
                (:file "set")
                (:file "map")
                #+(or sbcl lispworks)
                (:file "dstm")))

(defmethod asdf:perform :after ((op asdf:load-op) (sys (eql (asdf:find-system :dstm-collections))))
  (pushnew :dstm *features*))


