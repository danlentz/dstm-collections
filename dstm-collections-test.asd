;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-user)


(asdf:defsystem :dstm-collections-test
  :serial t
  :description      "Regression test-suite for dstm-collections"
  :long-description "Operate as: (asdf:test-system :dstm-collections)"
  :license          "MIT"
  :depends-on (:dstm-collections
                :bordeaux-threads
                :local-time
                :asdf-system-connections
                :hu.dwim.stefil+hu.dwim.def+swank 
                :hu.dwim.defclass-star)
  :components ((:file "test")))



(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :dstm-collections-test))))
  (asdf:load-system :dstm-collections-test)
  (funcall (intern (symbol-name :dstm-collections) (find-package :dstm-collections-test))))


(defmethod asdf:operation-done-p ((o asdf:test-op)
                                   (c (eql (asdf:find-system :dstm-collections-test))))
  nil)
