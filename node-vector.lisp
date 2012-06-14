;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;


(defpackage :tree
  (:documentation "")
  (:use :closer-common-lisp :contextl :ptc :debug :pandora))

(in-package :tree)

(deflayer allocation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Node Instance Access defines the low-level interface to storage allocation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar %leaf%     nil)
(defvar %unbound% '%unbound%)

(defun leaf ()
  %leaf%)

(defun unbound ()
  %unbound%)

;; (defvar %slots% (loop for i from 0 for k in '(:k :v :l :r :x) collect (cons k i)))

(deflayer memory (allocation))


(defclass root-object ()
  ()
  (:metaclass funcallable-standard-class))


(defstruct (tuple
             (:type vector) 
             (:conc-name tuple-) 
             (:constructor allocate-tuple))
  (k (unbound))
  (v (unbound))
  (l (unbound))
  (r (unbound))
  (x (unbound)))

(defun wrap-tuple (tuple &optional parent root &aux properties k v l r x)
  (plambda (&optional arg &rest rest) (tuple properties root parent k v l r x)
    (declare (ignorable rest))
    (when (not root) (setq root this))
    (etypecase arg
      (null   tuple)
      (vector (wrap-tuple arg this root))
      (keyword (ecase arg
                 (:k    (tuple-k tuple))
                 (:v    (tuple-v tuple))
                 (:x    (tuple-x tuple))
                 (:l    (wrap-tuple (tuple-l tuple) this root))
                 (:r    (wrap-tuple (tuple-r tuple) this root))
                 (:this this)
                 (:self self)
                 (:top  root)
                 (:up   parent))))))

    (if (null args) tuple
      (wrap-tuple args this))))


      (let* ((parent this)
              (tuple args))
        this))))

;;  (let* (tuple properties parent k v l r x) 

(defparameter t0 (wrap-tuple (allocate-tuple :k 0 :v 0 :l nil :r nil :x nil)))
(defparameter t1 (wrap-tuple (allocate-tuple :k 1 :v 1 :l nil :r nil :x nil)))
(defparameter t2 (wrap-tuple (allocate-tuple :k 2 :v 2 :l nil :r nil :x nil)))
(defparameter x  (wrap-tuple (allocate-tuple)))

(assert (equalp
          (funcall x)
          (funcall (funcall (funcall x (funcall t0)) :top))))

(assert (equalp
          (funcall x)
          (funcall (funcall (funcall x (funcall t0)) :up))))

(assert (equalp
          (funcall x)
          (funcall (funcall (funcall (funcall (funcall x (funcall t0)) (funcall t1)) :up) :up))))




#(%UNBOUND% %UNBOUND% %UNBOUND% %UNBOUND% %UNBOUND%)

(%UNBOUND% %UNBOUND% %UNBOUND% %UNBOUND% %UNBOUND%)
((%UNBOUND%) (%UNBOUND%) (%UNBOUND%) (%UNBOUND%) (%UNBOUND%))



(defun tuple (&rest elements)
  (let* (k v l r x 
          (object (make-instance 'tuple))
          (slots %slots%)
          (init (or elements (make-list (length %slots%) :initial-element %unbound%)))
          (vector (make-array (length init) :initial-contents init)))
    (set-funcallable-instance-function object
      (pandora:plambda (args) (k v l r x object slots vector)
        (let ((selection (or (assoc args slots) (rassoc args slots))))
          (case (car selection)
            
            ((:k :v :x) (svref vector (cdr selection)))
            ((:l :r)  (apply 'tuple (coerce vector 'list))) 
            (t (if (null args) pandora::this (coerce vector 'list)))))))
    object))
          
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (node
             (:type vector) :named
             (:conc-name %node-) 
             (:constructor %allocate-node))
  (k (unbound))
  (v (unbound))
  (l (leaf))
  (r (leaf))
  (x -1))


(define-layered-function make-node (k v l r x &optional allocator &rest args)
  (:method (k v l r x &optional (allocator '%allocate-node) &rest args)
    (apply allocator :k k :v v :l l :r r :x x args)))

(defun looks-nodish-to-me (thing)
  (and (typep thing '(simple-vector 6))
         (eq (elt thing 0) 'node)))  ;; for now

(deftype standard-node ()
  `(and (simple-vector 6) (satisfies looks-nodish-to-me)))


(defmethod pointer:deref ((thing simple-vector) &optional (type 'standard-node) &rest args)
  (declare (ignore args))
  (if (typep thing type)
    (values thing t)
    (call-next-method)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "THE NODE 'CLASS'"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; (describe 'node) =>
;;
;; TREE:NODE [symbol]
;;
;; NODE names a compiled function:
;;   Lambda-list: (K V L R &REST ARGS &AUX (X? (WHEN ARGS (LIST* X ARGS))))
;;   Derived type: (FUNCTION (T T T T &REST T)
;;                  (VALUES (SIMPLE-VECTOR 6) &OPTIONAL))
;;   Source file: /Volumes/u/dan/src/development/gs/ebu/dstm-collections/tree.lisp
;;
;; NODE names a type-specifier:
;;   Lambda-list: ()
;;   Expansion: (AND (SIMPLE-VECTOR 6) (SATISFIES LOOKS-NODISH-TO-ME))
;;
;; (allocate-standard-node)           => #(NODE %UNBOUND% %UNBOUND% NIL NIL 1)
;; (type-of (allocate-standard-node)) => (SIMPLE-VECTOR 6)
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public Node Access Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These constitute the interface abstacting the underlying interaction with
;; storage and allocation facilities


(defun node/k (putative-node &aux (node (pointer:deref putative-node)))
  "Public API to access the K constituent of node"
  (%node-k node))

(defun node/v (putative-node &aux (node (pointer:deref putative-node)))
  "Public API to access the V constituent of node"
  (%node-v node))

(defun node/l (putative-node &aux (node (pointer:deref putative-node)))
  "Public API to access the L constituent of node"
  (%node-l node))

(defun node/r (putative-node &aux (node (pointer:deref putative-node)))
  "Public API to access the R constituent of node"
  (%node-r node))

(defun node/x (putative-node &aux (node (pointer:deref putative-node)))
  "Public API to access the X constituent of node"
  (%node-x node))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compound "convenience"  Accessors built on the above Primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun node/kv (putative-node &aux (node (pointer:deref putative-node)))
  "return a list containing k and v constituent values of node"
  (list (node/k node) (node/v node)))

(defun node/lr (putative-node &aux (node (pointer:deref putative-node)))
  "return a list containing l and r constituent values of node"
  (list (node/l node) (node/r node)))

(defun node/kvlr (putative-node &aux (node (pointer:deref putative-node)))
  "return a list containing k, v, l, and r constituent values of node"
  (list (node/k node) (node/v node) (node/l node) (node/r node)))

(defun node/kvlrx (putative-node &aux (node (pointer:deref putative-node)))
  "return a list containing k, v, l, r, and x constituent values of node"
  (list (node/k node) (node/v node) (node/l node) (node/r node) (node/x node)))

(defun node/constituents (putative-node &aux (node (pointer:deref putative-node)))
   "return a list containing all constituent values of node"
  (node/kvlrx node))

(defun node/values (putative-node &aux (node (pointer:deref putative-node)))
  "return all constituents of node as multiple values"
  (apply #'values (node/constituents node)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Destructuring Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro kv ((k v) node &body body)
  "destructure tree node: key, value"
   (let ((gtree (gensym (symbol-name :tree-))))
     `(let ((,gtree ,node))
        (let ((,k  (node/k ,gtree))
              (,v  (node/v ,gtree)))
          ,@body))))

(defmacro lr ((l r) node &body body)
  "destructure tree node: left, right"
   (let ((gtree (gensym (symbol-name :tree-))))
     `(let ((,gtree ,node))
        (let ((,l  (node/l ,gtree))
              (,r  (node/r ,gtree)))
          ,@body))))

(defmacro kvlr ((k v l r) node &body body)
  "destructure tree node: key, value, left, right"
  (let ((gtree (gensym (symbol-name :tree-))))
    `(let ((,gtree ,node))
       (let ((,k  (node/k ,gtree))
              (,v  (node/v ,gtree))
              (,l  (node/l ,gtree))
              (,r  (node/r ,gtree)))
         ,@body))))

(defmacro kvlrx ((k v l r x) node &body body)
  "destructure tree node: key, value, left, right, balance-param"
  (let ((gtree (gensym (symbol-name :tree-))))
    `(let ((,gtree ,node))
       (let ((,k  (node/k ,gtree))
              (,v  (node/v ,gtree))
              (,l  (node/l ,gtree))
              (,r  (node/r ,gtree))
              (,x  (node/x ,gtree)))
         ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(defun pipeop (q)
  (labels ((pipeop-n (expr rest)
             (let ((expr (read-from-string expr)))
               `(,@(if (numberp expr)
                     `(nth ,(1- expr))
                     (list expr))
                  ,@(if rest (list rest) rest))))
            (recur (q acc)
              (let* ((cmds (string q))
                      (pos (position #\/ cmds)))
                (if pos
                 (recur (subseq cmds (1+ pos))
                    (pipeop-n (subseq cmds 0 pos) acc))
                  (pipeop-n cmds acc)))))
    (recur q () )))

#+()
(set-dispatch-macro-character #\# #\/  (lambda (str char arg)
                                         (declare (ignore char arg))
                                           (pipeop (read str nil nil nil))))

;; (progn #/person/car/father/name/last/1)
;; => (PROGN (NTH 0 (LAST (NAME (FATHER (CAR (PERSON)))))))
|#
