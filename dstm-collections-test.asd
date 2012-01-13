;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-user)


(asdf:defsystem :dstm-collections-test
  :serial t
  :depends-on (:dstm-collections
                :bordeaux-threads
                :local-time
                :hu.dwim.stefil+hu.dwim.def+swank 
                :hu.dwim.defclass-star)
  :components ((:file "test")))
                
