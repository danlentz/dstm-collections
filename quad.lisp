;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;


(in-package :quad)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocol Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass tuple ()())

(defgeneric qar (tuple))
(defgeneric qbr (tuple))
(defgeneric qcr (tuple))
(defgeneric qdr (tuple))

(defmethod qar ((tuple tuple))
  (error "cannot define constituent accessors on protocol class."))

(defmethod qbr ((tuple tuple))
  (error "cannot define constituent accessors on protocol class."))

(defmethod qcr ((tuple tuple))
  (error "cannot define constituent accessors on protocol class."))

(defmethod qdr ((tuple tuple))
  (error "cannot define constituent accessors on protocol class."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sequence "Class"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod qar ((tuple sequence))
  (assert (eql (length tuple) 4))
  (elt tuple 0))

(defmethod qbr ((tuple sequence))
  (assert (eql (length tuple) 4))
  (elt tuple 1))

(defmethod qcr ((tuple sequence))
  (assert (eql (length tuple) 4))
  (elt tuple 2))

(defmethod qdr ((tuple sequence))
  (assert (eql (length tuple) 4))
  (elt tuple 3))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abstract-Quad Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass abstract-quad (tuple)
  ((a :accessor qar :initform nil :initarg :a)
    (b :accessor qbr :initform nil :initarg :b)
    (c :accessor qcr :initform nil :initarg :c)
    (d :accessor qdr :initform nil :initarg :d))
  (:documentation "a generic quadruple without specific semantic
  connotations, based on the idea posted by Erik Naggum,
  comp.lang.lisp, 2004-01-19. The intent is to provide a
  common superclass that may be shared by a number of otherwise
  unrelated applications that compute their  value based on use of
  a four-element tuple data structure.  This common ancestor can then
  serve as a shared  basis for considerations such as structure specific
  but application agnostic persistent storage. An example use case might
  be a common persistent storage implementation for a quad-encoded
  RDF graph and for a quad based implementation of a red-black tree index
  into the quads of that graph."))
