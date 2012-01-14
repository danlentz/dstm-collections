;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;


(in-package :ord)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; boxed comparison
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct ci-char   c)
(defstruct ci-string s)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Explicit Comparitors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric compare (a b))

(defmethod  compare (a b)
  (if (eq       (type-of a) (type-of b))
    #+() (or
        (eq       (type-of a) (type-of b))
        (subtypep (type-of a) (type-of b))
        (subtypep (type-of b) (type-of a)))
    (error "ord:compare of argument-types ~S and ~S not defined" (type-of a) (type-of b))
    (ord:compare (princ-to-string (type-of a)) (princ-to-string (type-of b)))))


(defmethod  compare ((a number) (b number))
  (cond
    ((eql a b)  0)
    ((<   a b) -1)
    (t          1)))


(defmethod  compare ((a real) (b real))
  (cond
    ((eql a b)  0)
    ((<   a b) -1)
    (t          1)))


(defmethod  compare ((a character) (b character))
  (cond
    ((char= a b)  0)
    ((char< a b) -1)
    (t 1)))


(defmethod  compare ((a string) (b string))
  (cond
    ((string= a b)  0)
    ((string< a b) -1)
    (t 1)))


(defmethod  compare ((a hash-table) (b hash-table))
  (cond
    ((string=  (princ-to-string a) (princ-to-string b))  0)
    ((string<  (princ-to-string a) (princ-to-string b)) -1)
    (t 1)))


(defmethod  compare ((a symbol) (b symbol))
  (let ((pkgcmp (compare (symbol-package a) (symbol-package b))))
    (if (zerop pkgcmp)
      (compare (symbol-name a) (symbol-name b))
      pkgcmp)))


(defmethod  compare ((a pathname) (b pathname))
   (compare (namestring a) (namestring b)))


(defmethod  compare ((a package) (b package))
   (compare (package-name a) (package-name b)))


(defmethod  compare ((a ci-char) (b ci-char))
   (cond ((char-equal (ci-char-c a) (ci-char-c b))  0)
         ((char-lessp (ci-char-c a) (ci-char-c b)) -1)
         (t 1)))


(defmethod  compare ((a ci-string) (b ci-string))
   (cond ((string-equal (ci-string-s a) (ci-string-s b))  0)
         ((string-lessp (ci-string-s a) (ci-string-s b)) -1)
         (t 1)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Object Comparison
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric slots-to-compare (thing))


(defmethod  slots-to-compare ((thing standard-class))
  "comparible slots may be cutomized by class, with the default being all slots"
  (mapcar #'c2mop:slot-definition-name 
    (c2mop:class-slots thing)))


(defmethod  slots-to-compare ((thing standard-object))
  "comparible slots of a standard-object are defined by specialization on it class"
  (slots-to-compare (class-of thing)))


(defmethod ord:compare ((a standard-object) (b standard-object))
  "ordinal comparison of arbitrary standard-objects is performed as follows:
   1 - objects of different classes ordered by lexical comparison of class name
   2 - objects of a class for which slots-to-compare returns null are ordered by lexical
       comparison of printed representation.  For standard print-unreadable-object output,
       this achieves equality on the objects being #'eq, otherwise returns a consistent
       but arbitrary ordinal comparison value for the lifetime of these specific instances.
       Customized print-unreadable-object representations also provides a simple means
       of adjustment to the resulting comparison.
   3 - objects of identical class are compared based on the boundness and slot-value of
       the slots-names in list returned by slots-to-compare.  Slots unbound in both
       obects are considered equal.  Unbound slots are considered greater than bound slots of the
       same slot-name. Two bound slots-values with the same slot-name are compared recursively
       with ord:compare.
   4 - when all preceding steps complete without ordinal determination, the objects are
       considered equal"
  (if (not (eq (class-of a) (class-of b)))
    (ord:compare
      (princ-to-string (class-name (class-of a)))
      (princ-to-string (class-name (class-of b))))
    (let ((slots (slots-to-compare a)))
      (when (null slots)
        (return-from compare (ord:compare (princ-to-string a) (princ-to-string b))))      
      (loop
        :for x :in slots
        :do (cond
              ((and (not (slot-boundp a x)) (not (slot-boundp b x)))  nil)            
              ((not (slot-boundp a x)) (return-from compare   1))
              ((not (slot-boundp b x)) (return-from compare  -1))
              (t
                (let ((c (ord:compare (slot-value a x) (slot-value b x))))
                  (unless (zerop c)
                    (return-from compare c))))))
      0)))
    

        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implicit Comparitors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compare< (a b)
   (minusp (compare a b)))

(defun compare<= (a b)
   (not (plusp (compare a b))))

(defun compare= (a b)
   (zerop (compare a b)))

(defun compare>= (a b)
   (not (minusp (compare a b))))

(defun |COMPARE>| (a b)
   (plusp (compare a b)))
