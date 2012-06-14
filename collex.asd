;;;;; -*- mode: common-lisp;   common-lisp-style: sbcl;    coding: utf-8; -*-
;;;;;

(defpackage :collex-system
  (:use :common-lisp :asdf))

(in-package :collex-system)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prerequisite 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#-cldoc
(eval-when (:load-toplevel :compile-toplevel :execute)
  (ignore-errors
    (asdf:load-system :cldoc)))

;; #+cldoc
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

(defclass collex-component
  (asdf:component)
  ((changelog    :initform nil :accessor changelog :initarg :changelog)
    (history     :initform nil :accessor history   :initarg :history)))

(defclass collex-static-file
  (collex-component asdf:static-file)
  ((text         :initform nil :accessor text      :initarg :text)
    (forms       :initform nil :accessor forms     :initarg :forms)))

(defclass collex-source-file
  (collex-component asdf:cl-source-file)
  ((readtable-name :initform nil :accessor readtable-name :initarg :readtable-name)
    (text          :initform nil :accessor text           :initarg :text)
    (forms         :initform nil :accessor forms          :initarg :forms)    
    (debug-opt     :initform 3   :accessor debug-opt      :initarg :debug-opt)
    (speed-opt     :initform 0   :accessor speed-opt      :initarg :speed-opt)
    (space-opt     :initform 0   :accessor space-opt      :initarg :space-opt)
    (time-opt      :initform 0   :accessor time-opt       :initarg :time-opt)))

(defclass collex-package-file
  (collex-source-file) ())

(defclass collex-system-source-file
  (collex-static-file) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod asdf:perform :before (operation (file collex-component))
  (log:info "ASDF: Operation ~A on ~A" operation file)
  (push (cons file operation) (history file)))

(defmethod asdf:perform :after  (operation (file collex-component))
  (log:info "~A" (with-output-to-string (out)
                   (describe file out)
                   (terpri out) 
                   (describe operation out))))

(defmethod asdf:perform :around ((operation asdf:load-op)
                                      (file collex-source-file)) 
  "Establish new dynamic bindings for file-local variables."
  (progv *file-local-variables*
    (mapcar #'symbol-value *file-local-variables*) (call-next-method)))

(defmethod asdf:perform :around ((operation asdf:compile-op)
                                  (file collex-source-file))
  "Establish new dynamic bindings for file-local variables."
  ;; (proclaim (optimize (debug (debug-opt file)) (speed (speed-opt file))
  ;;            (space (space-opt file)) (time (time-opt file))))
  (progv *file-local-variables*
    (mapcar #'symbol-value *file-local-variables*)
    (call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project System Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(asdf:defsystem :collex :default-component-class collex-source-file
  
  :description "Transactional Set, Map, and Seq Functional Collections Library"
  :serial       t  
  :version     "1.1.6"
  :license     "LLGPL"
  
  :author      "Dan Lentz. Source code available at <http://github.com/danlentz/collex/>"
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

  :defsystem-depends-on (:cldoc)

  :depends-on          (:closer-mop :contextl :filtered-functions :lisp-unit
                         :cl-syntax :alexandria :cldoc :log4cl :unicly
                         :cffi-objects :manardb :osicat :drakma
                         :hu.dwim.serializer :rucksack :cl-store)
    
  :components ((:collex-system-source-file "collex.asd")
                (:collex-static-file       "readme.org")
                (:file "collex-package")                
                (:file "collex-special")             
                (:file "collex-utility")
                (:file "debug")
                (:file "omega")
                (:file "pandora")
                (:file "io")
                (:file "pointer")
                (:file "mmap-package")
                (:file "mmap-types")
                (:file "mmap-serializer")
                (:file "mmap-access")
                (:file "mmap-files")
                (:file "mmap-storage")
                (:file "ord")
                (:file "cstm")
                ;; (:file "tree-package")
                ;; (:file "tree-node")
                ;; (:file "tree-node-vector")
                ;; (:file "tree-node-mmap")
                ;; (:file "tree-node-manardb")
                ;; (:file "tree-common")
                ;; (:file "tree-red-black")
                ;; (:file "tree-weight-balanced")                
                ;; (:file "tree-cursor")
                ;; (:file "set")  
                ;; (:file "map")
                ;; (:file "seq")
                ;; (:file "collex-printer")
                ;; (:file "collex-reader")
#+cldoc         (:cldoc          :collex-documentation
                                 :target-system :collex
                                 :pathname "doc/html/")))


(defmethod asdf:perform :after ((op asdf:load-op) (sys (eql (asdf:find-system :collex))))
  (pushnew :collex *features*)
#+()  (let ((dclx-package  (find-package :collex)))
   #+()(when (symbol-value (intern (symbol-name :*default-syntax-startup-enabled*) dclx-package))
      (funcall (intern (symbol-name :enable-syntax) dclx-package)))
 #+()   (when (symbol-value (intern (symbol-name :*default-kernel-startup-enabled*) dclx-package))
      (funcall (intern (symbol-name :ensure-kernel) dclx-package)))))


(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :collex))))
  (asdf:load-system :collex-test)
  (funcall (intern (symbol-name :funcall-test-with-feedback-message)
             (find-package :hu.dwim.stefil))
    (read-from-string "'collex-test::collex")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ASDF Support Utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun asdf::find-package-module (package-designator)
  (let ((pathname (cadadr (swank-backend:find-source-location
                            (find-package package-designator)))))
    (when pathname (maphash (lambda (key value)
                              (when (find (pathname pathname)
                                      (asdf:module-components (cdr value))
                                      :key 'asdf:component-pathname
                                      :test #'equalp)
                                (return-from asdf::find-package-module key)))
                     asdf::*defined-systems*))))

(defun asdf::find-package-system (package-designator)
  (asdf:component-system (asdf::find-package-module package-designator)))

(unexport '(asdf::find-package-module asdf::find-package-system) :asdf)
