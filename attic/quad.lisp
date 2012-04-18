;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;


(in-package :quad)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quad Protocol Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass protocol () ())

(defgeneric qar  (thing))
(defgeneric qbr  (thing))
(defgeneric qcr  (thing))
(defgeneric qdr  (thing))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abstract-Quad Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass abstract-quad (protocol)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; typeless quad common  superclass
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass quad (abstract-quad)
  ()
  (:default-initargs :a nil :b nil :c nil :d nil))

(defgeneric quad (a b c d))

(defmethod quad (a b c d)
  (make-instance 'quad :a a :b b :c c :d d))


(defmethod print-object ((thing quad) stream)
  (with-slots (a b c d) thing
    (format stream "(quad ~S ~S ~S ~S)" a b c d)))


;; (defgeneric qons (cell next  &key &allow-other-keys))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quad Protocol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod qar ((thing abstract-quad))
  (error "cannot define constituent accessors on protocol class."))

(defmethod qbr ((thing abstract-quad))
  (error "cannot define constituent accessors on protocol class."))

(defmethod qcr ((thing abstract-quad))
  (error "cannot define constituent accessors on protocol class."))

(defmethod qdr ((thing abstract-quad))
  (error "cannot define constituent accessors on protocol class."))


(defmethod qar ((thing null))
  nil)

(defmethod qbr ((thing null))
  nil)

(defmethod qcr ((thing null))
  nil)

(defmethod qdr ((thing null))
  nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sequence compatibity
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

