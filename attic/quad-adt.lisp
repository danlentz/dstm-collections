;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;


(in-package :quad)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Algebraic Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalgebraic <quad> unbound
  (quad
    (a :initform unbound)
    (b :initform unbound)
    (c :initform unbound)
    (d :initform unbound)))


(defmethod print-object ((quad quad) stream)
  (with-slots (a b c d) quad
    (format stream "(~s ~s ~s ~s ~s)" (class-name (class-of quad))
      a b c d)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Housebreaking 'NIL' in 35 easy steps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; there is a lot to say about NIL vs EMPTY vs UNBOUND but I think whichever
;; school of nullary philosophy you subscribe to, the most important thing
;; is to be as consistent as possible in your use of that representation so
;; that there are just a few, well defined, places where you must bridge the
;; gap between these concepts.  In particular, introducing algebraic-style
;; datatype classes, which in most regards have had a very positive effect on
;; conceptual clarity and simplicity of implementation, exacerbated this
;; particular issue to the point to which for a little while it almost started
;; to seem completely intractible.  No matter what the approach happened to be,
;; ugly, problematic, inconsistent warts seemed to keep popping up down the line 
;; as some situation occurs where the OTHER representation is needed.  Happily,
;; though, after several trips around this particular block, the strategy as
;; now implemented enjoys the best of both worlds. Here are the bullet points:
;;
;; * The symbol UNBOUND represents the nullary value for abstact datatype <quad>.
;;   It is both an algebraic constant and an algebraic constructor in the
;;   ADT functional model.
;;
;; * As our ADTs are impemented with clos classes, it
;;   is therefore an instance of the class #<STANDARD-CLASS UNBOUND>, which
;;   is a direct subclass of the abstract protocol class <STANDARD-CLASS <QUAD>>,
;;   and it has  neither any direct or effective slots.
;;
;; * It is both bound and fbound, and, further, satisfies the invariant
;;   (EQ UNBOUND (UNBOUND)) => T 
;;
;;

(define-symbol-macro |<>| (unbound))

(defmethod print-object ((thing unbound) stream)
  (princ "<>" stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quad Protocol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric quad/a (thing))
(defgeneric quad/b (thing))
(defgeneric quad/c (thing))
(defgeneric quad/d (thing))

(defmethod qar ((thing <quad>))
  (error "cannot define constituent accessors on protocol class."))

(defmethod qbr ((thing <quad>))
  (error "cannot define constituent accessors on protocol class."))

(defmethod qcr ((thing <quad>))
  (error "cannot define constituent accessors on protocol class."))

(defmethod qdr ((thing <quad>))
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
