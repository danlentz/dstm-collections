;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(defpackage :adt
  (:documentation
    "Algebraic data type and pattern matching framework that is
    primarily based on the implementation CL-ADT by Tomohiro Matsuyama,
    and, to a lesser extent, the paper 'Implementing Algebraic Data Types,
    Lazy Evaluation, And Monads In Common Lisp' by Slava Akhmechet and the
    reference implementation he describes, CL-MONAD.
      Algebraic data types, type constructors, and data constructors are
    all implemented using CLOS classes, and are integrated with an extensible
    collection of destructuring pattern-matching operators in order to provide
    a uniform, intuitive, and comprehensive platform on which to implement
    the binary search tree data structures underlying the collections we have
    sought to provide to provide with this library.")
    (:use :common-lisp)     
  (:export
    :defalgebraic
    :match-error
    :with-matching
    :match
    :ematch    
    :defpattern))
    
(in-package :adt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UTIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun make-keyword (name)
  (intern (string name) :keyword))


(defmacro with-gensyms (symbols &body body)
  `(let ,(mapcar (lambda (symbol)
                   (let* ((symbol-name (symbol-name symbol))
                           (stem (if (every #'alpha-char-p symbol-name)
                                   symbol-name
                                   (concatenate 'string symbol-name "-"))))
                     `(,symbol (gensym ,stem))))
           symbols)
     ,@body))


(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop repeat (length names) collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names
                   collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names for g in gensyms
                     collect `(,n ,g)))
             ,@body)))))



(defun quotep (form)
  "Returns true if FORM has a form of quote."
  (and (consp form)
       (eq (car form) 'quote)))


(defun group (list &key (test #'eql) (key #'identity))
  "Divides LIST by TEST and returns a list of groups.
  TEST ia a function that takes two arguments and returns true if both
  belong to same group, as shown in the following examples.
  ;;;    (group '(1 1 2 3 3 3)) => ((1 1) (2) (3 3 3))
  ;;;    (group '(:a a b c) :test #'string=) => ((:A A) (B) (C))
  ;;;    (group '((:a 1) (:a 2) (:b 3)) :key #'car) => (((:A 1) (:A 2)) ((:B 3)))"
  (loop
    :with group
    :with groups
    :for x = (first list) :then y
    :for y :in list
    :if (funcall test (funcall key x) (funcall key y)) :do (push y group)
    :else :do (push (nreverse group) groups)  (setq group (list y))
    :finally (return (nreverse (if group
                                 (cons group groups)
                                 groups)))))


(defun make-bindings (args &key (prefix "VAR"))
  (loop
    :for arg :in args
    :collect `(,(gensym prefix) ,arg)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CASE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro %equal (pattern value)
  (if (quotep pattern)
      `(eq ',(second pattern) ,value)
      (typecase pattern
        (null                  `(null ,value))
        ((or symbol character) `(eq ,pattern ,value))
        (number                `(eql ,pattern ,value))
        (otherwise             `(equal ,pattern ,value)))))


(defun compile-case-clause (var clause else)
  (destructuring-bind (pattern then) clause
    (ecase (pattern-type pattern)
      (:constant
       `(if (%equal ,pattern ,var) ,then ,else))
      (:variable
        `(let ((,pattern ,var)) ,then)))))


(defun compile-case (var clauses else)
  (reduce (lambda (clause else) (compile-case-clause var clause else))
          clauses
          :initial-value else
    :from-end t))


(defmacro %case (arg clauses else)
  (if (atom arg)
    (compile-case arg clauses else)
    (once-only (arg)
      (compile-case arg clauses else))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PATTERN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro defpattern (name lambda-list &body body)
  "Defines a constructor pattern named NAME as a function internally
  that takes LAMBDA-LIST as arguments and evaluates BODY when called. A
  constructor pattern has a form `(NAME . ARGS)` in pattern-matcing
  clauses. When such a pattern is found during pattern-matching, this
  function will called with arguments: VAR of matching object and rest
  of ARGS. The result must be a list of `(TEST ARGS PATTERNS)` where
  TEST is a form that predicates VAR is matched or not, ARGS is a list
  of objects that are destructured by this pattern, and PATTERNS is a
  list of patterns that should be matched with each element of ARGS
  respectively. ARGS and PATTERNS must have same length. The following
  example shows the implementation of a CONS pattern.
  ;;;
  ;;; (defpattern cons (var car-pattern cdr-pattern)
  ;;;   `((consp ,var)
  ;;;     ((car ,var) (cdr ,var))
  ;;;     (,car-pattern ,cdr-pattern)))
  ;;;
  ;;; (match (cons 1 2)
  ;;;   ((cons x y) (+ x y))) => 3
  ;;;"
  `(setf (get ',name 'compile-pattern)
     (lambda ,lambda-list
       ,@body)))


(defun pattern-type (pattern)
  "Returns a type of PATTERN."
  (etypecase pattern
    ((member t nil) :constant)
    (keyword        :constant)
    (symbol         :variable)
    (cons           (if (quotep pattern)
                      :constant
                      (car pattern)))
    (atom           :constant)))



;;;  The following definitions implement a basic complement 
;;; of useful patterns and illustrate the technique with which
;;; others might be built


(defpattern cons (var car-pattern cdr-pattern)
  `((consp        ,var)
    ((car ,var)   (cdr ,var))
    (,car-pattern ,cdr-pattern)))


(defpattern list (var &rest patterns)
  (if patterns
    `((consp           ,var)
      ((car ,var)      (cdr ,var))
      (,(car patterns) (list ,@(cdr patterns))))
    `((null ,var)      () ())))


(defpattern tuple (var &rest patterns)
  (if patterns
      `((consp ,var)
        ((car ,var) (cdr ,var))
        (,(car patterns) (list ,@(cdr patterns))))
      `((null ,var) () ())))


(defpattern vector (var &rest patterns)
  `((vectorp ,var)
    ((coerce ,var 'list))
    ((list ,@patterns))))


(defpattern sequence (var &rest patterns)
  `((vectorp ,var)
    ((coerce ,var 'list))
    ((list ,@patterns))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; match
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun group-match-clauses (clauses)
  (group clauses :key (lambda (clause) (pattern-type (caar clause)))))


(defun compile-match-variable (vars clauses else)
  `(%match
    ,(cdr vars)
    ,(loop for ((var . rest) . then) in clauses
           if (string= var "_")
             collect `(,rest ,@then)
           else
             collect `(,rest (let ((,var ,(car vars))) ,@then)))
    ,else))


(defun compile-match-constant (vars clauses else)
  (loop with alist
        for ((constant . rest) . then) in clauses
        for sub-clause = (cons rest then)
        for assoc = (assoc constant alist :test #'equal)
        if assoc
          do (setf (cdr assoc) (cons sub-clause (cdr assoc)))
        else
          do (push `(,constant ,sub-clause) alist)
        finally
           (return
             `(%case
               ,(car vars)
               ,(loop for pair in (nreverse alist)
                      for constant = (car pair)
                      for sub-clauses = (nreverse (cdr pair))
                      collect `(,constant (%match ,(cdr vars) ,sub-clauses ,else)))
               ,else))))


(defun compile-match-constructor (name vars clauses else)
  (loop with var = (car vars)
        with compile-pattern-function = (get name 'compile-pattern)
        for (((nil . pattern-args) . rest) . then) in clauses
        for (test args patterns)
          = (apply compile-pattern-function var pattern-args)
        collect `((,@patterns ,@rest) ,@then) into new-clauses
        finally
           (return
             (let* ((bindings (make-bindings args))
                    (new-vars (mapcar #'car bindings)))
               `(if ,test
                    (let ,bindings
                      (declare (ignorable ,@new-vars))
                      (%match
                       ,(append new-vars (cdr vars))
                       ,new-clauses
                       ,else))
                    ,else)))))


(defun compile-match-empty (clauses else)
  (loop for (pattern . then) in clauses
        if (null pattern)
          do (return `(progn ,@then))
        finally (return else)))


(defun compile-match-group (vars group else)
  (if vars
      (let ((group-type (pattern-type (caaar group))))
        (case group-type
          (:variable (compile-match-variable vars group else))
          (:constant (compile-match-constant vars group else))
          (otherwise (compile-match-constructor group-type vars group else))))
      (compile-match-empty group else)))


(defun compile-match-groups (vars groups else)
  (reduce (lambda (group else) (compile-match-group vars group else))
          groups
          :initial-value else
          :from-end t))


(defmacro %match (vars clauses else)
  (let ((groups (group-match-clauses clauses)))
    (compile-match-groups vars groups else)))


(defmacro match* (args clauses else)
  (let* ((bindings (make-bindings args))
         (vars (mapcar #'car bindings)))
    `(let ,bindings
       (declare (ignorable ,@vars))
       (%match ,vars ,clauses ,else))))


(define-condition match-error (error)
  ((argument :initarg :argument
             :initform nil
             :reader match-argument)
   (patterns :initarg :patterns
             :initform nil
             :reader match-patterns))
  (:report (lambda (condition stream)
             (format stream "Can't match ~A with ~{~S~^ or ~}."
                     (match-argument condition)
                     (match-patterns condition)))))



(defun %match-error (&optional arg patterns)
  (cerror "Continue."
          'match-error
          :argument arg
          :patterns patterns))


(defmacro match (arg &body clauses)
  "If ARG matches against CLAUSES, returns the last evaluated value of
  the body of CLAUSES, otherwise returns NIL. CLAUSES is a list
  of (PATTERN . THEN) where PATTERN is a pattern and THEN is an implicit
  progn that will be evaulated when matched.
  A pattern must be one of

  - a constant (self-evaluating object) such as `1` and `\"str\"`
  - a quote that means constant such as `'foo` and `'(a b c)`
  - a symbol such as `foo`
  - a constructor of `(CTOR . ARGS)`

  * A constant pattern will be matched by proper test functions such like
  `EQ`, `EQL`, and `EQUAL` on each constants.

*Examples:

  ;;; (match 1 (1 t))             => T
  ;;; (match 3.14 (3.14 t))       => T
  ;;; (match \"str\" (\"str\" t)) => T

   A quote pattern is same as a constant pattern except that a body of
   the form will be used as a constant.

  ;;; (match 'foo ('foo T)) => T

  A symbol pattern binds a variable of the symbol to ARG.

  ;;; (match 1 (a a)) => 1

  A constructor pattern will be matched by calling a function on the
  constructor that is defined with DEFPATTERN. Pre-defined constructors
  are the following:

  - `(cons car-pattern cdr-pattern)`
  - `(list pattern*)`
  - `(tuple pattern*)`
  - `(vector pattern*)`

  Examples:

    (match (cons 1 2) ((cons x y) (+ x y)))       => 3
    (match (list 1 2 3) ((list x y z) (+ x y z))) => 6"
  `(match* (,arg)
     ,(loop for (pattern . then) in clauses
            collect `((,pattern) ,@then))
     nil))


(defmacro ematch (arg &body clauses)
  "Same as MATCH except MATCH-ERROR will be raised unless matched."
  (once-only (arg)
    `(match* (,arg)
       ,(loop for (pattern . then) in clauses
              collect `((,pattern) ,@then))
       (%match-error ,arg ',(mapcar #'car clauses)))))


(defmacro with-matching (pattern arg &body body)
  "Equivalent to `(ematch ARG (PATTERN . BODY))`."
  `(ematch ,arg (,pattern ,@body)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ADT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defmacro defalgebraic (name &body specs)
  "Defines an algebraic data type. Each element of SPECS must have a
  form of `CTOR` or `(CTOR . SLOTS)`. The former has no slots (zero
  arity), meaning this defines a constant. The latter has one or more
  slots, meaning this defines a data constructor. Each element of ARGS
  must be slot specifier.
     Algebraic data types are defined as classes. In addition, all of
  constants and constructors are also be defined as subclasses of it.
  Accessing internal data of algebraic data types can be done by
  pattern-matching. A few examples follow
  ;;;
  ;;; (defalgebraic btree
  ;;;   empty
  ;;;   (node left value right))
  ;;;  
  ;;; empty                               => #<EMPTY {1004FFA7C1}>
  ;;; (empty)                             => #<EMPTY {1004FFA7C1}>
  ;;; (typep empty 'btree)                => T
  ;;; (typep empty 'empty)                => T  
  ;;; (node empty 1 empty)                => #<NODE {1003021DF1}>
  ;;; (typep (node empty 1 empty) 'btree) => T
  ;;; (typep (node empty 1 empty) 'node)  => T
  ;;; (match empty ((empty) 'empty))      => EMPTY
  ;;; (match empty ((node _ _ _) 'node))  => NIL
  ;;; (match (node empty 1 empty)
  ;;;   ((node l x r) (list l x r)))      => (#<EMPTY {1004FFA7C1}> 1 #<EMPTY {1004FFA7C1}>)"
  `(progn
     (defclass ,name () ())
     ,@(loop for spec in specs
             for (ctor . slots) = (if (atom spec) (list spec) spec)
             for slot-names = (loop for slot in slots
                                    collect (if (consp slot) (car slot) slot))
             collect `(defclass ,ctor (,name) ,slots)
             if slots collect
               `(defun ,ctor ,slot-names
                  (let ((instance (make-instance ',ctor)))
                    ,@(loop for slot-name in slot-names
                            collect `(setf (slot-value instance ',slot-name) ,slot-name))
                    instance))
             else
               collect `(defparameter ,ctor (make-instance ',ctor))
               and collect `(defun ,ctor () ,ctor)
             collect
             `(defpattern ,ctor (,ctor ,@slot-names)
                `((typep ,,ctor ',',ctor)
                  (,,@(loop for slot-name in slot-names
                            collect ``(slot-value ,,ctor ',',slot-name)))
                  (,,@slot-names))))))

