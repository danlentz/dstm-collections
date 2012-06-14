;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(defpackage :debug
  (:use :closer-common-lisp)
  (:export
    :ppmx
    :printv
    :v
    :v/
    :v//
    :v///
    :v+
    :v++
    :v+++
    :ap
    :??
    :?
    :?*
    :?**
    :?***
    :info
    :info/
    :info//
    :info///
    :dis))

(in-package :debug)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common Idioms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun car-equalp (cons1 cons2)
  "compare lists by first element"
  (equalp (car cons1) (car cons2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; describe
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-symbol-macro ?    (prog1 *   (describe *)))
(define-symbol-macro ?*   (prog1 *   (describe *)))
(define-symbol-macro ?**  (prog1 **  (describe **)))
(define-symbol-macro ?*** (prog1 *** (describe ***)))

(defun info (&rest args)
  (mapc #'describe args))

(define-symbol-macro info/   (info (multiple-value-list /)))
(define-symbol-macro info//  (info (multiple-value-list //)))
(define-symbol-macro info/// (info (multiple-value-list ///)))

;; (pushnew 'cl:equalp  (get 'car-equalp :see-also))
;; (pushnew (list '#1=(car-equalp (cons 5 6) (cons 5 7)) #1#) (get 'car-equalp :examples)
;;   :test #'car-equalp)
;; (pushnew (list '#1=(ap "value" "list") #1#)  (get 'ap :examples) :test #'car-equalp)
;; (pushnew 'cl:apropos-list  (get 'ap :see-also))
;; (pushnew '??  (get 'ap :see-also))
;; (pushnew '???  (get 'ap :see-also))
;; (setf (get 'ap :examples) (remove-duplicates (get 'ap :examples) :test #'equalp))
;; (setf (get 'ap :examples) nil)

(defmethod describe-object :before ((x symbol) stream)
  (setf (get x :source) nil)
  (when (fboundp x)
    (push (cons :function (sb-introspect:find-definition-source (fdefinition x)))
      (get x :source)))
  (when (boundp x)
    (push (cons :variable (sb-introspect:find-definition-source (symbol-value x)))
      (get x :source)))
  (when (find-class x nil)
    (push (cons :class (sb-introspect:find-definition-source (find-class x)))
      (get x :source)))
  (when (macro-function x)
    (push (cons :macro (sb-introspect:find-definition-source (macro-function x)))
      (get x :source)))  
  (format stream "~%~A~%~%" (make-string *print-right-margin* :initial-element #\=))
  (princ (swank:documentation-symbol (symbol-name x)) stream)
  (format stream "~%~A~%~%" (make-string *print-right-margin* :initial-element #\.)))

(defmethod describe-object :after ((x symbol) stream)
  (let ((ex (get x :examples)))
    (when ex
      (format stream "~%~A~%" (make-string *print-right-margin* :initial-element #\.))
      (format stream "~%~S Example Usage:~% ~:{~%USER>  ~:@W  -->~%~%       ~S~%~}~%" x ex)))
  (let ((so (get x :see-also)))
    (when so
      (format stream "~A~%" (make-string *print-right-margin* :initial-element #\.))
      (format stream "~%SEE-ALSO: ~{~:@W ~}~%~%" so)))  
  (format stream "~A~%~%" (make-string *print-right-margin* :initial-element #\=)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defmacro define-examples (symbol &rest forms) ;; result-string)
;;   (let ((sym (gensym "symbol"))
;;          (fn (gensym "function"))
;;          (frms (gensym "form"))
;;          (res (gensym "result")))
;;     `(let* ((,sym (quote ,symbol))
;;             (,fn (function ,symbol))
;;             (,frms (quote ,forms))
;;              (,res (loop for f in ,frms collect (list f (eval f)))))
;;        (defmethod describe-object :before ((x (eql  ,sym)) stream)
;;          (format stream "~%~A~%" (make-string *print-right-margin* :initial-element #\-)))
;;        (defmethod describe-object :after ((x (eql  ,sym)) stream)
;;                (format stream "~%~A~%" (make-string *print-right-margin* :initial-element #\.))
;;          (format stream "~%~W Example Usage:~% ~:{~%USER>  ~S  -->~%~%   ~S~%~%~}~%" ,fn ,res)
;;           (format stream "~&~A~%" (make-string *print-right-margin* :initial-element #\-)))
;;        (defmethod describe-object :after ((x (eql ,fn)) stream)
;;          (describe-object ,sym stream)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PPMX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro ppmx (form)
  "Pretty prints the macro expansion of FORM."
  `(let* ((exp1 (macroexpand-1 ',form))
           (exp (macroexpand exp1))
           (*print-circle* nil))
     (format *trace-output* "~%;; Form: ~W"  (quote ,form))
     #+() (pprint (quote ,form) *trace-output*)
     (cond ((equal exp exp1)
             (format *trace-output* "~%;;~%;; Macro expansion:~%")
             (pprint exp *trace-output*))
       (t (format *trace-output* "~&;; First step of expansion:~%")
         (pprint exp1 *trace-output*)
         (format *trace-output* "~%;;~%;; Final expansion:~%")
         (pprint exp *trace-output*)))
     (format *trace-output* "~%;;~%;; ")
     (values)))

#|

 (ppmx (with-output-to-string (x) t)) =>

;; Form: (WITH-OUTPUT-TO-STRING (X) T)
;;
;; Macro expansion:

 (LET ((X (MAKE-STRING-OUTPUT-STREAM :ELEMENT-TYPE 'CHARACTER)))
  (UNWIND-PROTECT (PROGN T) (CLOSE X))
  (GET-OUTPUT-STREAM-STRING X))
;;

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Break
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf (symbol-function :break) #'(lambda (&optional (why "break") &rest values)
                                   (break "~A: ~{~S~^, ~}" why values)
                                   (values-list values)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; printv
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adapted from the Handy PRINTV Macro Written by Dan Corkill
;; Copyright (C) 2006-2010, Dan Corkill <corkill@GBBopen.org>
;; Licensed under Apache License 2.0 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun printv-minor-separator ()
  (format *trace-output* "~&;; ~60,,,'-<-~>~%")
  (force-output *trace-output*))

(defun printv-major-separator ()
  (format *trace-output* "~&;;~%")
  (princ
    (concatenate 'string ";;"
      (make-string (- *print-right-margin* 5) :initial-element #\=)) *trace-output*)
  (force-output *trace-output*))

(defun printv-form-printer (form)
  (typecase form
    ;; String (label):
    (string (format *trace-output* "~&;; ~a~%" form))
    ;; Evaluated form:
    ((or cons (and symbol (not keyword)))
      (format *trace-output* "~&;;   ~w =>" form))
    (vector (format *trace-output* "~&;;   ~s~%" form)) 
    ;; Self-evaluating form:
    (t (format *trace-output* "~&;;   ~s~%" form)))
  (force-output *trace-output*))

(defun printv-values-printer (values-list)
  (format *trace-output* "~:[ [returned 0 values]~;~:*~{ ~w~^;~}~]~%"  values-list)
  (force-output *trace-output*))

(defun printv-expander (forms &optional values-trans-fn) ;; Allow for customized printv'ers:
  (let ((result-sym (gensym)))
    `(let ((*print-readably* nil) ,result-sym)
       ,@(loop for form in forms nconcing
           (cond
             ;; Markup form:
             ((eq form ':ff) (list '(printv-major-separator)))
             ((eq form ':hr) (list '(printv-minor-separator)))
             ;; Evaluated form:
             ((or (consp form) (and (symbolp form) (not (keywordp form))))
               `((printv-form-printer ',form)
                  (printv-values-printer
                    (setf ,result-sym ,(if values-trans-fn
                                         `(funcall ,values-trans-fn
                                            (multiple-value-list ,form))
                                         `(multiple-value-list ,form))))))
             ;; Self-evaluating form:
             (t `((printv-form-printer 
                    (car (setf ,result-sym (list ,form))))))))
       (values-list ,result-sym))))

(defmacro printv (&rest forms)
  (printv-expander forms))

(defmacro v (&rest forms)
  (printv-expander forms))

(setf (symbol-function :printv) #'printv-expander)

(define-symbol-macro v/   (printv /))
(define-symbol-macro v//  (printv //))
(define-symbol-macro v/// (printv ///))

(define-symbol-macro v+   (printv +))
(define-symbol-macro v++  (printv ++))
(define-symbol-macro v+++ (printv +++))

(defmethod describe-object :after ((x (eql 'printv)) stream)
  (format stream "~%~%Example:~%~%~A~%~A~%~%"
    "(assert (equalp (list 7 5)
          (multiple-value-list 
            (printv :ff
              \"some simple examples\" :hr \"\"
              nil t  (list :s :p :o :c) #(1 2 3 4) :foo *package* (make-instance 'standard-object) 
              \"\" \"multiple value examples\" :hr \"\"
              (values) (gethash 'x (make-hash-table)) (values (+ 3 4) (+ 2 3)) :ff))))"
"
;; ===============================================================================================
;; some simple examples
;; ------------------------------------------------------------
;; 
;;   NIL => NIL
;;   T => T
;;   (LIST :S :P :O :C) => (:S :P :O :C)
;;   #(1 2 3 4)
;;   :FOO
;;   *PACKAGE* => #<PACKAGE \"VIVACE-GRAPH-V2\">
;;   (MAKE-INSTANCE 'STANDARD-OBJECT) => #<STANDARD-OBJECT {100471CA71}>
;; 
;; multiple value examples
;; ------------------------------------------------------------
;; 
;;   (VALUES) => [returned 0 values]
;;   (GETHASH 'X (MAKE-HASH-TABLE)) => NIL; NIL
;;   (VALUES (+ 3 4) (+ 2 3)) => 7; 5
;;
;; ===============================================================================================
"))

;; (defmethod describe-object :after ((x (eql :printv)) stream)
;;   (describe-object 'printv stream))


;; (defmethod describe-object :after ((x (eql #'printv)) stream)
;;   (describe-object 'printv stream))


;; (describe #'printv)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; apropos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ap (&rest args)
  (let ((package nil)
  (search-list nil))
    (loop for arg in args do
   (if (and (symbolp arg) (find-package arg))
       (setf package (find-package arg))
       (push arg search-list)))
    (%ap (nreverse search-list) package)))

(defgeneric %ap (thing &optional package))

(defmethod %ap ((thing string) &optional package)
  (let ((*package* (or (and package (find-package package))
           *package*)))
    (apropos-list thing package)))

(defmethod %ap ((thing symbol) &optional package)
  (%ap (symbol-name thing) package))

(defmethod %ap ((thing list) &optional package)
  (cond ((null thing) nil)
  ((null (rest thing)) (%ap (first thing) package))
  (t
   (let ((current (%ap (first thing) package)))
     (dolist (next (rest thing))
       (setf current (intersection current (%ap next package))))
     current))))

;; (defmethod describe-object :after ((x (eql #'ap)) stream)
;;   (format stream "~%Examples:~%~%~A~%~%"
;;  "(ap \"bind\" \"value\") =>  (multiple-value-bind               compiler::pa-multiple-value-bind
;;                           excl::walk-multiple-value-bind    excl::bindstack-value-prev
;;                           excl::bindstack-value             *bind-treat-values-as-values*
;;                           bind-missing-value-form-warning   sparql.executor::binding-values)

;;  (ap \"bind\" \"value\" :cl-user) =>  (multiple-value-bind)"))

;; (defmethod describe-object :after ((x (eql 'ap)) stream)
;;   (describe-object #'ap stream))

;; (defmacro define-example (symbol form result-string)
;;   (let ((sym (gensym "symbol"))
;;          (fn (gensym "function"))
;;          (frm (gensym "form"))
;;          (res (gensym "result")))
;;     `(let ((,sym (quote ,symbol))
;;             (,fn (function ,symbol))
;;             (,frm (quote ,form))
;;             (,res ,result-string))
;;        (defmethod describe-object :before ((x (eql  ,sym)) stream)
;;          (format stream "~%~A~%" (make-string *print-right-margin* :initial-element #\-)))
;;        (defmethod describe-object :after ((x (eql  ,sym)) stream)
;;          (format stream "~%~A~%" (make-string *print-right-margin* :initial-element #\.))
;;          (format stream "~%~W EXAMPLE:~%~%;;  ~W  -->~%;;~%~<;;   ~@;~A~; ~:>~%~%"
;;            ,fn ,frm (list ,form) ); ,res))
;;           (format stream "~&~A~%" (make-string *print-right-margin* :initial-element #\-)))
;;        (defmethod describe-object :after ((x (eql ,fn)) stream)
;;          (describe-object ,sym stream)))))


(defun ensure-list (thing)
  (if (atom thing)
    (list thing)
    thing))


;; (ppmx (define-examples ap (ap "bind" "value") (ap "values" :cl)))
  
;; (define-example ap (ap "bind" "value")
;; "'(multiple-value-bind               compiler::pa-multiple-value-bind
;;   excl::walk-multiple-value-bind    excl::bindstack-value-prev
;;   excl::bindstack-value             *bind-treat-values-as-values*
;;   bind-missing-value-form-warning   sparql.executor::binding-values)")
;; (info 'ap)
  
(defun ?? (string-designator &optional package external-only)
  (apply #'apropos string-designator package external-only nil))

(setf (symbol-function '???) #'ap)

;;; (defun ??? (string-designator &optional package external-only)
;;;  (apply #'apropos-list string-designator package external-only nil))



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

#+()
(defmacro pcond (&body clauses)
  `(cond ,@(mapcar #'(lambda (clause)
		      `((let ((x ,(car clause)))
			  (print (list ',(car clause) '=> x))
			  x)
			,@(cdr clause)))
		  clauses)))




(defmacro report-and-ignore-errors (&body body)
  `(handler-case (progn ,@body)
     (error (condition) 
            (format *debug-stream* "~%Error: ~A~%" condition)
	    (values nil condition))))




#+()
(defmacro dis (args &rest body)
  `(disassemble
     (compile nil
       (lambda ,(mapcar (lambda (a)
                          (if (consp a)
                            (cadr a)
                            a))
                        args)
         (declare
           ,@(mapcar
               #`(type ,(car a1) ,(cadr a1))
               (remove-if-not #'consp args)))
         ,@body))))
