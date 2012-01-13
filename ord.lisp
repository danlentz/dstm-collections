;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;


(in-package :ord)


(defstruct ci-char   c)
(defstruct ci-string s)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Explicit Comparitors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric compare (a b))

(defmethod  compare ((a number) (b number))
   (- a b))

(defmethod  compare ((a real) (b real))
   (- a b))

(defmethod  compare ((a character) (b character))
   (cond ((char= a b)  0)
         ((char< a b) -1)
         (t 1)))

(defmethod  compare ((a string) (b string))
   (cond ((string= a b)  0)
         ((string< a b) -1)
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
