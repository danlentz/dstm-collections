;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(defpackage :wbtree
  (:use :common-lisp)
  (:nicknames :wb)
  (:export
    :*delta*
    :*gamma*
    :tree
    :node)
  (:documentation

   "This is an implementation of a weight-balanced binary tree data
    structure based on the following references:

   --  Adams

   --  Hiro

   --  MIT Scheme weight balanced tree as reimplemented by Yoichi Hirai and Kazuhiko Yamamoto
      based on the revised non-variant algorithm and integer balance parameters.
      {https://github.com/kazu-yamamoto/wttree}

   -- Bounded Balance

   -- Making Data Structures Persistent

   *   Overview

   A weight balanced binary search tree is a binary tree which maintains height in
   logarithmic proportion to the number of elements contained by "

    ))


(in-package :wb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rotation Coefficients
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;   These parameters are, in general, not user-configurable as they
;;   must satisfy very specific mathematical requirements.  These
;;   particular parameters have been chosen because they are the only
;;   integer solution pair.


(defvar *delta* 5
  "The primary parameter used for the determination whether two subtrees
   of a node are in balance or require adjustment by means of some  rotation
   operation.  The specific rotation to be performed is determined by
   {defvar wb::*gamma*}")


(defvar *gamma* 3
  "")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Supporting Metaclasses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass abstract-class (standard-class) ()
  (:documentation ""))


(defmethod make-instance ((c abstract-class) &rest args)
  (declare (ignore args))
  (error "Trying to instantiate the abstract class ~A." (class-name c)))


(defmethod c2mop:validate-superclass ((class abstract-class) (superclass standard-class))
  t)


(defmethod c2mop:validate-superclass ((class standard-class) (superclass abstract-class))
  t)


(defclass abstract-tree () ()
  (:metaclass abstract-class)
  (:documentation ""))


(defclass abstract-node () ()
  (:metaclass abstract-class)
  (:documentation ""))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fundamental Tree Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass tree (abstract-tree)
  ((id
     :initform      (princ-to-string (unicly:make-v4-uuid))
     :initarg       :id
     :reader        tree/id
     :documentation "")
    (node-class
      :initarg      :node-class
      :reader       tree/node-class
      :documentation "")
    (root
      :initform      nil
      :initarg       :root
      :accessor      tree/root
      :documentation ""))
  (:default-initargs :node-class 'node)
  (:documentation ""))


(defun make (class &rest args)
  ""
  (apply #'make-instance class args))


(defun make-tree (&optional root)
  ""
  (make 'tree :root root))


(defmethod print-object ((tree tree) stream)
  (print-unreadable-object (tree stream :type t :identity t)
    (with-slots (id root) tree
      (princ id stream)
      (princ " root: " stream)
      (princ (or root "EMPTY") stream))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fundamental Node Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass node (abstract-node)
  ((K
     :initarg :K
     :reader  node/K)
    (V
      :initarg :V
      :reader  node/V)
    (L
      :initarg :L
      :reader  node/L)
    (R
      :initarg :R
      :reader  node/R)
    (W
      :initarg :W
      :reader  node/W)))


(defmethod print-object ((node node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (with-slots (L K V W R) node
      (format stream "(~S . ~S) L:~S R:~A W:~D" K V L R W))))


(defun make-node (K V L R W)
  (make 'node :K K :V V :L L :R R :W W))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Degenerate Instances
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun empty ()
  ""
  nil)


(defun emptyp (thing)
  ""
  (null thing))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Node Primatives 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun node/size (node)
  ""
  (if (emptyp node) 0 (node/W node)))


(defun node/weight (node)
  ""
  (+ 1 (node/size node)))


(defun node/singleton (k v)
  ""
  (make-node k v (empty) (empty) 1))


(defmacro with-node ((node) &body body)
  ""
  `(with-slots (k v l r w) ,node
     ,@body))


(defun node/call (node fn)
  ""
  (with-node (node)
    (funcall fn k v l r)))


(defun n-join (k v l r)
  ""
  (make-node k v l r (+ 1 (node/size l) (node/size r))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rotation Operators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun single-l (a.k a.v x r)
  "Perform a single left rotation, moving Y, the left subtree of the right subtree of A,
  into the left subtree (shown below).
    This must occur in order to restore proper balance when the weight of the left
  subtree of node A is less then the weight of the right subtree of node A
  multiplied by rotation coefficient {defvar wb::*delta*}  and the weight of the
  left subtree of node B is less than the weight of the right subtree of node B
  multiplied by rotation coefficient {defvar wb::*gamma*} 
  ;;; .
  ;;;              ,---,                                  ,---,
  ;;;              | A |                                  | B |
  ;;;              :---:                                  :---:
  ;;;             :     :                                :     :
  ;;;        ,---:       :---,                      ,---:       :---, 
  ;;;        | X |       | B |           =>         | A |       | Z | 
  ;;;        '---'       :---:                      :---:       '---'
  ;;;               ,---:     :---,            ,---:     :---,
  ;;;               | Y |     | Z |            | X |     | Y |
  ;;;               '---'     '---'            '---'     '---'
  ;;; ."
  (node/call r
    #'(lambda (b.k b.v y z)
        (n-join b.k b.v (n-join a.k a.v x y) z))))



(defun double-l (a.k a.v x r)
  "Perform a double left rotation, moving Y1, the left subtree of the left subtree
  of the right subtree of A, into the left subtree (shown below).
    This must occur in order to restore proper balance when the weight of the left
  subtree of node A is less then the weight of the right subtree of node A
  multiplied by rotation coefficient {defvar wb::*delta*}  and the weight of the
  left subtree of node B is greater than or equal to the weight of the right subtree 
  of node B multiplied by rotation coefficient {defvar wb::*gamma*} 
  ;;; .
  ;;;              ,---,                                    ,---,             
  ;;;              | A |                                    | B |             
  ;;;           ___:---:___                             ____:---:____          
  ;;;      ,---:           :---,                   ,---:             :---,       
  ;;;      | X |           | C |                   | A |             | C |       
  ;;;      '---'           :---:         =>        :---:             :---:
  ;;;                 ,---:     :---,         ,---:     :---,   ,---:     :---,  
  ;;;                 | B |     | Z |         | X |     | y1|   | y2|     | Z |  
  ;;;                 :---:     '---'         '---'     '---'   '---'     '---'
  ;;;            ,---:     :---,   
  ;;;            | y1|     | y2|  
  ;;;            '---'     '---'
  ;;; ."
  (node/call r
    #'(lambda (c.k c.v b z)
        (node/call b
          #'(lambda (b.k b.v y1 y2)
              (n-join b.k b.v
                (n-join a.k a.v x y1)
                (n-join c.k c.v y2 z)))))))



(defun single-r (b.k b.v l z)
  "Perform a single right rotation, moving Y, the right subtree of the left subtree of B,
  into the right subtree (shown below).
    This must occur in order to restore proper balance when the weight of the right
  subtree of node B is less then the weight of the left subtree of node B
  multiplied by rotation coefficient {defvar wb::*delta*}  and the weight of the
  right subtree of node A is less than the weight of the left subtree of node A
  multiplied by rotation coefficient {defvar wb::*gamma*} 
  ;;; .
  ;;;              ,---,                                  ,---,             
  ;;;              | B |                                  | A |             
  ;;;              :---:                                  :---:             
  ;;;             :     :                                :     :            
  ;;;        ,---:       :---,                      ,---:       :---,       
  ;;;        | A |       | Z |          =>          | X |       | B |       
  ;;;        :---:       '---'                      '---'       :---:       
  ;;;   ,---:     :---,                                    ,---:     :---,  
  ;;;   | X |     | Y |                                    | Y |     | Z |  
  ;;;   '---'     '---'                                    '---'     '---'  
  ;;; ."
  (node/call l
    #'(lambda (a.k a.v x y)
        (n-join a.k a.v x (n-join b.k b.v y z)))))


(defun double-r (c.k c.v l z)
  "Perform a double right rotation, moving Y2, the right subtree of the right subtree
  of the left subtree of C, into the right subtree (shown below).
    This must occur in order to restore proper balance when the weight of the right
  subtree of node C is less then the weight of the left subtree of node C
  multiplied by rotation coefficient {defvar wb::*delta*}  and the weight of the
  right subtree of node B is greater than or equal to the weight of the left subtree 
  of node B multiplied by rotation coefficient {defvar wb::*gamma*} 
  ;;; .
  ;;;              ,---,                                    ,---,             
  ;;;              | C |                                    | B |             
  ;;;           ___:---:___                             ____:---:____          
  ;;;      ,---:           :---,                   ,---:             :---,       
  ;;;      | A |           | Z |                   | A |             | C |       
  ;;;      :---:           '---'        =>         :---:             :---:
  ;;; ,---:     :---,                         ,---:     :---,   ,---:     :---,  
  ;;; | X |     | B |                         | X |     | y1|   | y2|     | Z |  
  ;;; '---'     :---:                         '---'     '---'   '---'     '---'
  ;;;      ,---:     :---,   
  ;;;      | y1|     | y2|  
  ;;;      '---'     '---'
  ;;; ."
  (node/call l
    #'(lambda (a.k a.v x b)
        (node/call b
          #'(lambda (b.k b.v y1 y2)
              (n-join b.k b.v
                (n-join a.k a.v x y1)
                (n-join c.k c.v y2 z)))))))


