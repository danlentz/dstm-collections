;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :dclx)

(defmacro report-and-ignore-errors (&body body)
  `(handler-case (progn ,@body)
     (error (condition) 
            (format *debug-stream* "~%Error: ~A~%" condition)
	    (values nil condition))))

(defun quotep (form)
  "Returns true if FORM has a form of quote."
  (and (consp form)
       (eq (car form) 'quote)))


(defun symbolicate (&rest things)
  (let* ((length (reduce #'+ things
                   :key (lambda (x) (length (string x)))))
          (name (make-array length :element-type 'character)))
    (let ((index 0))
      (dolist (thing things (values (intern name)))
        (let* ((x (string thing))
                (len (length x)))
          (replace name x :start1 index)
          (incf index len))))))


(defun make-keyword (name)
  (intern (string name) :keyword))


(defun keywordicate (&rest things)
  (let ((*package* (find-package :keyword)))
    (apply #'symbolicate things)))


(defun ensure-list (thing)
  (if (atom thing)
    (list thing)
    thing))


(defmacro with-gensyms (symbols &body body)
  `(let ,(mapcar (lambda (symbol)
                   (let* ((symbol-name (symbol-name symbol))
                           (stem (if (every #'alpha-char-p symbol-name)
                                   symbol-name
                                   (concatenate 'string symbol-name "-"))))
                     `(,symbol (gensym ,stem))))
           symbols)
     ,@body))


(defun make-gensym-list (n &optional name)
  (when (eq t name)
    (break))
  (if name
      (loop repeat n collect (gensym (string name)))
      (loop repeat n collect (gensym))))


(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop repeat (length names) collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names
                   collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names for g in gensyms
                     collect `(,n ,g)))
             ,@body)))))


;; Example:
;; (defmacro do-primes ((var start end) &body body)
;;   (ONCE-ONLY (start end)
;;     `(do ((,var (next-prime ,start)
;;             (next-prime (1+ ,var))))
;;        ((> ,var ,end))
;;        ,@body)))

(defmacro nlet (name bindings &body body)
  (let ((args (mapcar #'first (mapcar #'ensure-list bindings)))
         (vals (mapcar #'second (mapcar #'ensure-list bindings))))
    `(labels ((,name ,args ,@body))
       (,name ,@vals))
    ))

;; (ppmx
;; (nlet fact ((n 3))
;;   (if (zerop n) 1 (* n (fact (- n 1))))))



(defun reuse-cons (x y x-y)
  "Return (cons x y), or reuse x-y if it is equal to (cons x y)"
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
      x-y
    (cons x y)))


(define-modify-macro conc1f (obj)
  (lambda (place obj)
    (nconc place (list obj))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; anaphoramobila
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro anaphoric (op test &body body)
  `(let ((it ,test))
     (,op it ,@body)))

(defmacro symbolic (op test &body body &environment env)
  (declare (ignorable env))
  `(symbol-macrolet ((it ,test))
     (,op it ,@body)))


(defmacro aprog1 (first &body rest)
  "Binds IT to the first form so that it can be used in the rest of the
  forms. The whole thing returns IT."
  `(anaphoric prog1 ,first ,@rest))

(defmacro awhen (test &body body)
  "Like WHEN, except binds the result of the test to IT (via LET) for the scope
  of the body."
  `(anaphoric when ,test ,@body))

(defmacro axtypecase (keyform &body cases)
  "Like TYPECASE, except binds the result of the keyform to IT (via LET) for
  the scope of the cases."
  `(anaphoric typecase ,keyform ,@cases))

(defmacro acase (keyform &body cases)
  "Like CASE, except binds the result of the keyform to IT (via LET) for the
  scope of the cases."
  `(anaphoric case ,keyform ,@cases))

(defmacro acond (&body clauses)
  "Like COND, except result of each test-form is bound to IT (via LET) for the
  scope of the corresponding clause."
  (labels ((rec (clauses)
             (if clauses
               (destructuring-bind ((test &body body) . rest)  clauses
                 (if body
                   `(anaphoric if ,test (progn ,@body) ,(rec rest))
                   `(anaphoric if ,test it ,(rec rest))))
               nil)))
    (rec clauses)))

(defmacro swhen (test &body body)
  "Like WHEN, except binds the test form to IT (via SYMBOL-MACROLET) for the
  scope of the body. IT can be set with SETF."
  `(symbolic when ,test ,@body))

(defmacro scase (keyform &body cases)
  "Like CASE, except binds the keyform to IT (via SYMBOL-MACROLET) for the
  scope of the body. IT can be set with SETF."
  `(symbolic case ,keyform ,@cases))

(defmacro scond (&body clauses)
  "Like COND, except each test-form is bound to IT (via SYMBOL-MACROLET) for the
  scope of the corresponsing clause. IT can be set with SETF."
  (labels ((rec (clauses)
	     (if clauses
		 (destructuring-bind ((test &body body) . rest) clauses
		   (if body
		       `(symbolic if ,test (progn ,@body) ,(rec rest))
		       `(symbolic if ,test it ,(rec rest))))
		 nil)))
    (rec clauses)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Debugging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Substitute these for let or let* to get a trace of the binding forms. 


(defmacro plet* (bind-forms &body body)
  `(let* ,(mapcar #'(lambda (form)
		      (if (listp form)
			  `(,(car form) (let ((v ,(cadr form)))
					  (print (list ',(car form) v))
					  v))
			  form))
		  bind-forms)
     ,@body))


(defmacro plet (bind-forms &body body)
  `(let ,(mapcar #'(lambda (form)
		     (if (listp form)
		      `(,(car form) (let ((v ,(cadr form)))
				      (print (list ',(car form) v))
				      v))
		      form))
		  bind-forms)
     ,@body))


(defmacro pcond (&body clauses)
  `(cond ,@(mapcar #'(lambda (clause)
		      `((let ((x ,(car clause)))
			  (print (list ',(car clause) '=> x))
			  x)
			,@(cdr clause)))
		  clauses)))


(defun ps (thing) (princ-to-string thing))

(defun class-proto (c)
  (let ((cc (find-class c)))
    (c2mop:finalize-inheritance cc)
    (c2mop:class-prototype cc)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functional plumbing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(declaim (inline ensure-function))	; to propagate return type.
(declaim (ftype (function (t) (values function &optional)) ensure-function))

(defun ensure-function (function-designator)
  "Returns the function designated by FUNCTION-DESIGNATOR:
if FUNCTION-DESIGNATOR is a function, it is returned, otherwise
it must be a function name and its FDEFINITION is returned."
  (if (functionp function-designator)
    function-designator
    (fdefinition function-designator)))


(defun compose (function &rest more-functions)
  "Returns a function composed of FUNCTION and MORE-FUNCTIONS that applies its
arguments to to each in turn, starting from the rightmost of MORE-FUNCTIONS,
and then calling the next one with the primary value of the last."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (reduce (lambda (f g)
	    (let ((f (ensure-function f))
                   (g (ensure-function g)))
	      (lambda (&rest arguments)
		(declare (dynamic-extent arguments))
		(funcall f (apply g arguments)))))
    more-functions :initial-value function))



(defun multiple-value-compose (function &rest more-functions)
  "Returns a function composed of FUNCTION and MORE-FUNCTIONS that applies
its arguments to to each in turn, starting from the rightmost of
MORE-FUNCTIONS, and then calling the next one with all the return values of
the last."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (reduce (lambda (f g)
	    (let ((f (ensure-function f))
                   (g (ensure-function g)))
	      (lambda (&rest arguments)
		(declare (dynamic-extent arguments))
		(multiple-value-call f (apply g arguments)))))
    more-functions :initial-value function))

#+()
(defun :break (name &rest values)
  (break "~S = ~{~S~^, ~}" name values)
  (values-list values))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SETFable Places
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro get-place (place &environment env)
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place env)
    (let ((writer `(let (,@(mapcar #'list vars vals))
                     (lambda (,@store-vars)
                       ,writer-form)))
          (reader `(let (,@(mapcar #'list vars vals))
                     (lambda () ,reader-form))))
      `(values ,writer ,reader))))

#|
(defparameter *x* '(1 2 3))
(defparameter *write-x* (get-place (car *x*)))
(funcall *write-x* 4)
(print *x*)
(defun no-really (set-place)
  (let ((*x* 42))
   (funcall set-place 7)))
(no-really *write-x*)
|#

;; Erik Naggum in comp.lang.lisp.
;; http://groups.google.fr/group/comp.lang.lisp/msg/ac10b819b1117c4f

(defmacro drop (object place &rest keys &key key test test-not &environment environment)
  "Drop a particular OBJECT from list in PLACE.  (Intended as counterpart to PUSH/-NEW.)
Copyright 1999 by Erik Naggum.  Verbatim inclusion and redistribution permitted.
For any other use, contact Erik Naggum."
  (declare (ignore key test test-not))
  (multiple-value-bind (vars vals store-vars writer reader)
      (get-setf-expansion place environment)
    (let ((evaled-value (gensym))
          (store-var (first store-vars)))
      (if (cdr store-vars)
        `(let* ((,evaled-value ,object)
                ,@(mapcar #'list vars vals))
           (multiple-value-bind ,store-vars ,reader
             (setq ,store-var (delete ,evaled-value ,store-var :count 1 ,@keys))
             ,writer))
        `(let* ((,evaled-value ,object)
                ,@(mapcar #'list vars vals)
                (,store-var (delete ,evaled-value ,reader :count 1 ,@keys)))
           ,writer))))) 


(defmacro using ((&rest packages) &body body)
  "this morning got me wondering whether there was a local alternative to package use...
   --  Nick Levine, http://enlivend.livejournal.com/36650.html"
  (let ((packages (mapcar 'find-package packages)))
    (labels ((symbol-try (symbol package)
               (multiple-value-bind (symbol status) (find-symbol (symbol-name symbol) package)
                 (when (eq status :external)                   
                   symbol)))
              (symbol (symbol)
                (let ((possibles (remove nil
                                   (mapcar (lambda (package)
                                             (symbol-try symbol package))
                                     packages))))
                  (cond
                    ((cdr possibles)
                      (error "Symbol ~a exported from more than one package: ~{~a~^, ~}"
                        symbol (mapcar 'package-name possibles))) 
                    (possibles (car possibles)))))
              (form (form)
                (loop for thing in form collect
                  (cond ((symbolp thing) (or (symbol thing) thing))
                    ((consp thing) (form thing))
                    (t thing)))))
      (let ((expansion (form body)))
        (if (cdr expansion)
          `(progn ,@expansion)
          (car expansion))))))

#|
(using (:io)
  (writing-nicely
    (pprint (read-file-to-list #P"~/site/dstm-collections/package.lisp"))))
|#
