;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(defpackage :pandora
  (:use :common-lisp #+lisp-unit :lisp-unit)
  (:export
    :mkstr
    :symb
    :fbind
    :alambda
    :dlambda
    :alet
    :let-binding-transform
    :pandoriclet-get
    :pandoriclet-set
    :pandoriclet
    :get-pandoric
    :with-pandoric
    :pandoric-hotpatch
    :pandoric-recode
    :plambda
    :*pandoric-eval-tunnel*
    :pandoric-eval))

(in-package :pandora)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #` Reader Macro 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun mkstr (&rest args)
    (with-output-to-string (s)
      (dolist (a args) (princ a s))))

  (defun symb (&rest args)
    (values (intern (apply #'mkstr args))))

  (defun |#`-reader| (stream sub-char numarg)
    (declare (ignore sub-char))
    (unless numarg (setq numarg 1))
    `(lambda ,(loop for i from 1 to numarg collect (symb 'a i))
       ,(funcall (get-macro-character #\`) stream nil)))

  (set-dispatch-macro-character  #\# #\` #'|#`-reader|))

#+named-readtables
(named-readtables:defreadtable :sharp-backtick
  (:merge :standard)
  (:dispatch-macro-char #\#   #\`   #'|#`-reader|))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specialized Binding Forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro fbind ((name form) &body body)
  (let ((gname (gensym (string name))))
    `(let ((,gname ,form))
       (declare (function ,gname))
       (flet ((,name (&rest args) (apply ,gname args)))
         ,@body))))

;; (fbind (foo (lambda (x) (print (list 'foo x))))
;;   (foo 1)
;;   (foo 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pandoric Lexical Closures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))


(defmacro dlambda (&rest dispatch-table)
  (let* ((arglist (gensym "ARGS")))
    `(lambda (&rest ,arglist)
       (case (car ,arglist)
         ,@(mapcar (lambda (d)
                     `(,(if (eq t (car d))
                          t
                          (list (car d)))
                        (apply (lambda ,@(cdr d))
                          ,(if (eq t (car d))
                             arglist
                             `(cdr ,arglist)))))
             dispatch-table)))))


(defmacro alet (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     (lambda (&rest params)
       (apply this params))))


(defun let-binding-transform (bs)
  (if bs
    (cons
      (cond ((symbolp (car bs))
              (list (car bs)))
        ((consp (car bs))
          (car bs))
        (t (error "Bad let bindings")))
      (let-binding-transform (cdr bs)))))


(defun pandoriclet-get (letargs)
  `(case sym
     ,@(mapcar #`((,(car a1)) ,(car a1))
         letargs)
     (t (error "Unknown pandoric get: ~a" sym))))


(defun pandoriclet-set (letargs)
  `(case sym
     ,@(mapcar #`((,(car a1))
                   (setq ,(car a1) val))
         letargs)
     (t (error "Unknown pandoric set: ~a" sym))))


(defmacro pandoriclet (letargs &rest body)
  (let ((letargs (cons '(this) (let-binding-transform letargs))))
    `(let (,@letargs)
       (setq this ,@(last body))
       ,@(butlast body)
       (dlambda 
         (:pandoric-get (sym)        ,(pandoriclet-get letargs))
         (:pandoric-set (sym val)    ,(pandoriclet-set letargs))
         (t             (&rest args)  (apply this args))))))


(declaim (inline get-pandoric))

(defun get-pandoric (box sym)
  (funcall box :pandoric-get sym))


(defsetf get-pandoric (box sym) (val)
  `(progn
     (funcall ,box :pandoric-set ,sym ,val)
     ,val))


(defmacro with-pandoric (syms box &rest body)
  (let ((g!box (gensym "box")))
    `(let ((,g!box ,box))
       (declare (ignorable ,g!box))
       (symbol-macrolet (,@(mapcar #`(,a1 (get-pandoric ,g!box ',a1))
                             syms))
         ,@body))))


(defun pandoric-hotpatch (box new)
  (with-pandoric (this) box
    (setq this new)))


(defmacro pandoric-recode (vars box new)
  `(with-pandoric (this ,@vars) ,box
     (setq this ,new)))


(defmacro plambda (largs pargs &rest body)
  (let ((pargs (mapcar #'list pargs)))
    `(let (this self)
       (declare (ignorable this self))
       (setq
         this (lambda ,largs ,@body)
         self (dlambda
                (:pandoric-get (sym)        ,(pandoriclet-get pargs))
                (:pandoric-set (sym val)    ,(pandoriclet-set pargs))
                (t             (&rest args)  (apply this args)))))))

(defvar *pandoric-eval-tunnel*)

(defmacro pandoric-eval (vars expr)
  `(let ((*pandoric-eval-tunnel* (plambda () ,vars t)))
     (eval `(with-pandoric ,',vars *pandoric-eval-tunnel* ,,expr))))

