;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :collex)


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



(defun reuse-cons (x y x-y)
  "Return (cons x y), or reuse x-y if it is equal to (cons x y)"
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
      x-y
    (cons x y)))


(define-modify-macro conc1f (obj)
  (lambda (place obj)
    (nconc place (list obj))))


(defmacro multiple-setf (value &rest places)
  "The multiple-setf macro was written by Mario Castel√°n. It is a
 beautiful form to support multiple places in zerof and nilf."
  (once-only (value)
    `(setf ,@(loop for place in places
               append `(,place ,value)))))


(defmacro aconsf (place key value &environment env)
  "CONS is to PUSH as ACONS is to ACONSF; it pushes (cons KEY VALUE) to the PLACE."
  (multiple-value-bind (temps vals stores set-value get-value)
      (get-setf-expansion place env)
    (unless (null (cdr stores))
      (error "ACONSF can't store to this form: ~:_~S" place))
    (once-only (key value)
      `(let* (,@(mapcar 'list temps vals)
              (,(car stores)
               (acons ,key ,value ,get-value)))
         ,set-value
         ,value))))

(define-modify-macro nconcf (&rest lists)
  nconc
  "Modify-macro for NCONC. Sets place designated by the first argument to
the result of calling NCONC with the place and the LISTS.")

(defmacro nilf (&rest places)
  "Set PLACES to nil"
  `(multiple-setf nil ,@places))
  
(defmacro definline (name args &body body)
  "Like `defun' but declare the function as inline"
  `(progn
     (declaim (inline ,name))
     (defun ,name ,args ,@body)))

(defmacro let1 (var value &body body)
  "Make a single `let' binding, heroically saving three columns."
  `(let ((,var ,value))
     ,@body))

  
(defun mapappend (fun &rest args)
   (if (some 'null args)
       '()
       (append (apply fun (mapcar 'car args))
               (mapappend fun (mapcar 'cdr args)))))
   
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

(defmacro atypecase (keyform &body cases)
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
;; CLOS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun proto (thing)
  (flet ((get-proto (c)
           (let ((cc (find-class c)))
             (c2mop:finalize-inheritance cc)
             (c2mop:class-prototype cc))))
    (etypecase thing
      (class  (get-proto thing))
      (standard-object (get-proto (class-of thing)))
      (symbol (get-proto  thing)))))


(defvar *object->sexp-visited-objects* nil)

(defun object->sexp (obj &key suppress-types suppress-properties)
  "Converts arbitrary CLOS objects into s-expressions that can easily be used in tests."
  (if (and (subtypep (type-of obj) 'standard-object)
        (find obj *object->sexp-visited-objects*))
    :recursive-reference
    (let ((*object->sexp-visited-objects* (cons obj *object->sexp-visited-objects*)))
      (cond ((find (type-of obj) suppress-types) :suppressed)
        ((subtypep (type-of obj) 'standard-object)
          (multiple-value-bind (instance slots)
            (make-load-form-saving-slots obj)
            (let ((class (cadr (cadadr instance)))
                   (bound-slots (mapcar #'(lambda (s)
                                            (list (second (first (last (second s))))
                                              (object->sexp (second (third s))
                                                :suppress-types suppress-types
                                                :suppress-properties suppress-properties)))
                                  (remove 'slot-makunbound (rest slots) :key #'first))))
              (list* class
                (sort (remove-if #'(lambda (slot)
                                     (find (first slot) suppress-properties))
                        bound-slots)
                  #'string< :key (compose #'symbol-name #'first))))))
        ((null obj) nil)
        ((listp obj)
          (mapcar #'(lambda (obj) (object->sexp obj
                                    :suppress-types suppress-types
                                    :suppress-properties suppress-properties)) obj))
        (t obj)))))




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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defmacro!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ensure-list (thing)
    (if (atom thing)
      (list thing)
      thing))

(defun curry (fn &rest pref-args)
  (lambda (&rest suf-args)
    (apply fn (append pref-args suf-args))))

(defun rcurry (fn &rest suf-args)
  (lambda (&rest pref-args)
    (apply fn (append pref-args suf-args))))

(defun collect-if (predicate proseq &rest rest)
  (apply 'remove-if-not predicate proseq rest))


(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
               ((atom x) (cons x acc))
               (t (rec
                    (car x)
                    (rec (cdr x) acc))))))
    (rec x nil)))

;; Bang-symbols

(defun bang-symbol-p (prefix s)
  (and (symbolp s)
    (> (length (symbol-name s)) 2)
    (string= (symbol-name s) prefix
      :start1 0
      :end1   2)))

(defun get-bang-symbols (prefix body)
  (remove-duplicates
    (collect-if (curry #'bang-symbol-p prefix) (flatten body))))

(defun bang-symbol-name (s)
  (subseq (symbol-name s) 2))


(defun raw-mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args)
      (princ a s))
    ))

(defun mkstr (&rest args)
  (with-standard-io-syntax
    (apply #'raw-mkstr args)))

(defun correct-for-symbol-character-case (str)
  ;; a portable way to make symbol strings Modern Mode vs ANSI
  (if (eql #\a (char (string :a) 0))
    (string-downcase (string str))
    (string-upcase (string str))))

(defun intern-symbol (str &rest package)
  (apply #'intern (correct-for-symbol-character-case str) package))

(defun symb (&rest args)
  (values (intern-symbol (apply #'mkstr args))))

(defmacro nlet (name bindings &body body)
  (let ((args (mapcar #'first (mapcar #'ensure-list bindings)))
         (vals (mapcar #'second (mapcar #'ensure-list bindings))))
    `(labels ((,name ,args ,@body))
       (,name ,@vals))
    ))

;; (ppmx
;; (nlet fact ((n 3))
;;   (if (zerop n) 1 (* n (fact (- n 1))))))


(defun |reader-for-#`| (stream sub-char numarg)
  "Reader macro for #` for producing parameterized BQ lists Produces a
  function that can be applied to arguments"
  (declare (ignore sub-char))
  (unless numarg (setq numarg 1))
  (let ((a-args (loop for i from 1 to numarg
                  collect (symb 'a i))))
    `(lambda ,a-args
       (declare (ignorable ,@a-args))
       ,(funcall
          (get-macro-character #\`) stream nil))))

(set-dispatch-macro-character  #\# #\` #'|reader-for-#`|)
)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defmacro defmacro/a! (name args &body body)
  "A-Bang symbols -- create anaphoric symbol names
   in package of macro expansion"
  (let ((syms (get-bang-symbols #.(symbol-name :A!) body)))
    (if syms
      `(defmacro ,name ,args
         (let ,(mapcar #`(,a1 (intern ,(bang-symbol-name a1))) syms)
           ,@body))
      `(defmacro ,name ,args ,@body))))


(defmacro defmacro/g! (name args &body body)
  "G-Bang symbols -- auto generated gensyms"
  (let ((syms (get-bang-symbols #.(symbol-name :G!) body)))
    (if syms
      `(defmacro/a! ,name ,args
         (let ,(mapcar #`(,a1 (gensym ,(bang-symbol-name a1))) syms)
           ,@body))
      `(defmacro/a! ,name ,args ,@body))))


(defun o!-symbol-to-g!-symbol (s)
  "O-Bang symbols -- once-only eval gensyms"
  (symb #.(symbol-name :G!)
    (bang-symbol-name s)))
)

(defmacro defmacro! (name args &body body)
  (let* ((os (get-bang-symbols #.(symbol-name :O!) args))
          (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    ;; o-bang symbols can interfere with find-source
    (if os
      `(defmacro/g! ,name ,args
         `(let ,(mapcar #'list (list ,@gs) (list ,@os))
            ,(progn
               ,@body)))
      `(defmacro/g! ,name ,args ,@body)) ))


(defun collect-decls (forms)
  (nlet iter ((forms forms)
               (decls nil))
    (let ((form (car forms)))
      (if (or (stringp form)
            (and (consp form)
              (eq (car form) 'declare)))
        (iter (cdr forms) (cons form decls))
        (values forms (nreverse decls))))))


(defmacro! nlet-tail (n letargs &rest body)
  (let ((gs (mapcar (lambda (arg)
                      (declare (ignore arg))
                      (gensym))
              letargs))
         (gsx (mapcar (lambda (arg)
                        (declare (ignore arg))
                        (gensym))
                letargs)))
    (multiple-value-bind (body decls)
      (collect-decls body)
      `(macrolet
         ((,n ,gs
            `(progn
               (psetq
                 ,@(apply #'nconc
                     (mapcar
                       #'list
                       ',gsx
                       (list ,@gs))))
               (go ,',g!n))))
         (block ,g!b
           (let ,(mapcar #2`(,a1 ,(cadr a2)) gsx letargs)
             (tagbody
               ,g!n
               (let ,(mapcar #2`(,(car a2) ,a1) gsx letargs)
                 ,@decls
                 (return-from
                   ,g!b (progn ,@body))))))))))

 
