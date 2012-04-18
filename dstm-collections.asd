;;;;; -*- mode: common-lisp;   common-lisp-style: sbcl;    coding: utf-8; -*-
;;;;;

(defpackage :dstm-collections-system
  (:use :common-lisp :asdf))

(in-package :dstm-collections-system)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prerequisite 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#-cldoc
(eval-when (:load-toplevel :compile-toplevel :execute)
  (ignore-errors
    (asdf:load-system :cldoc)))

#+cldoc
(import '(cldoc:doc-op cldoc:document-system cldoc:cldoc cldoc:print-op cldoc:print-system))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ASDF Component Specializtion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar *file-local-variables* ()
  "List of file-local special variables.")

(defun make-variable-file-local (symbol)
  "Make special variable named by SYMBOL have a file-local value."
  (pushnew symbol *file-local-variables*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass dstm-collections-component
  (asdf:component)
  ((changelog    :initform nil :accessor changelog :initarg :changelog)
    (history     :initform nil :accessor history   :initarg :history)))

(defclass dstm-collections-static-file
  (dstm-collections-component asdf:static-file)
  ((text         :initform nil :accessor text      :initarg :text)
    (forms       :initform nil :accessor forms     :initarg :forms)))

(defclass dstm-collections-source-file
  (dstm-collections-component asdf:cl-source-file)
  ((readtable-name :initform nil :accessor readtable-name :initarg :readtable-name)
    (text          :initform nil :accessor text           :initarg :text)
    (forms         :initform nil :accessor forms          :initarg :forms)    
    (debug-opt     :initform 3   :accessor debug-opt      :initarg :debug-opt)
    (speed-opt     :initform 0   :accessor speed-opt      :initarg :speed-opt)
    (space-opt     :initform 0   :accessor space-opt      :initarg :space-opt)
    (time-opt      :initform 0   :accessor time-opt       :initarg :time-opt)))

(defclass dstm-collections-package-file
  (dstm-collections-source-file) ())

(defclass dstm-collections-system-source-file
  (dstm-collections-static-file) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod asdf:perform :before (operation (file dstm-collections-component))
  (log:info "ASDF: Operation ~A on ~A" operation file)
  (push (cons file operation) (history file)))

(defmethod asdf:perform :after  (operation (file dstm-collections-component))
  (log:info (with-output-to-string (out)
              (describe file out)
              (terpri out) 
              (describe operation out))))

  
(defmethod asdf:perform :around ((operation asdf:load-op)
                                      (file dstm-collections-source-file)) 
  "Establish new dynamic bindings for file-local variables."
  (progv *file-local-variables*
    (mapcar #'symbol-value *file-local-variables*) (call-next-method)))

(defmethod asdf:perform :around ((operation asdf:compile-op)
                                  (file dstm-collections-source-file))
  "Establish new dynamic bindings for file-local variables."
  ;; (proclaim (optimize (debug (debug-opt file)) (speed (speed-opt file))
  ;;            (space (space-opt file)) (time (time-opt file))))
  (progv *file-local-variables*
    (mapcar #'symbol-value *file-local-variables*)
    (call-next-method)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project System Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(asdf:defsystem :dstm-collections :default-component-class dstm-collections-source-file
  
  :description "Transactional Set, Map, and Seq Functional Collections Library"
  :serial       t  
  :version     "0.6.0"
  :license     "LLGPL"
  
  :author      "Dan Lentz. Source code available at <http://github.com/danlentz/dstm-collections/>"
  :maintainer  "Dan Lentz. Please direct comments and questions to <danlentz@gmail.com>"

  :author      "Context Oriented Software Transactional Memory based on source originally by:
                Drs. Pascal Costanza, Drew Crampsie, and Charlotte Herzeel."
  :author      "Herlithy Dynamic Software Transactional Memory based on source originally by:
                Dr. David McClain"
  :author      "Weight-Balanced Binary Tree Index based on source originally developed by:
                Drs. Stephen Adams, Yoichi Hirai, and Kazuhiko Yamamoto"

  :long-description "An extensible framework based on functional-style implementations of
  balanced binary tree indexes, currently including complete implementations of
  both weight-balanced and height-balanced (red-black) data structures with comprehensive
  concurrency support including both MVCC 'append-only' style persistence as well as for
  Software Transactional Memory incorporating the flexibility of on-the-fly runtime
  selection of both 'direct-update' (locking) style and 'deferred-update' (optimistic)
  style transaction models, independently configurable on a per-thread basis."
  
  :depends-on (:closer-mop :contextl :named-readtables :local-time :unicly :cldoc
                :cl-store :manardb)
  
  :components ((:dstm-collections-system-source-file "dstm-collections.asd")
                (:dstm-collections-package-file      "package")
                
                (:file "dstm-collections")             
                (:file "printv")
                (:file "util")
                (:file "timing")
                (:file "lock")
                (:file "io")
                (:file "mmap-stream")
                (:file "deref")
                (:file "ord")
                (:file "cstm")
                (:file "node")
                (:file "mmap-node")
                ;; (:file "tree")
                ;; (:file "wbtree")
                ;; (:file "rbtree")
                ;; (:file "cursor")
                ;; (:file "set")  
                ;; (:file "map")
                ;; (:file "seq")
                ;; (:file "printer")
                ;; (:file "reader")
#+cldoc         (:cldoc          :dstm-collections-documentation
                                 :target-system :dstm-collections
                                 :pathname "doc/html/")))


(defmethod asdf:perform :after ((op asdf:load-op) (sys (eql (asdf:find-system :dstm-collections))))
  (pushnew :dstm *features*)
  (let ((dclx-package  (find-package :dstm-collections)))
    (when (symbol-value (intern (symbol-name :*default-syntax-startup-enabled*) dclx-package))
      (funcall (intern (symbol-name :enable-syntax) dclx-package)))
 #+()   (when (symbol-value (intern (symbol-name :*default-kernel-startup-enabled*) dclx-package))
      (funcall (intern (symbol-name :ensure-kernel) dclx-package)))))


(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :dstm-collections))))
  (asdf:load-system :dstm-collections-test)
  (funcall (intern (symbol-name :funcall-test-with-feedback-message)
             (find-package :hu.dwim.stefil))
    (read-from-string "'dstm-collections-test::dstm-collections")))
 
