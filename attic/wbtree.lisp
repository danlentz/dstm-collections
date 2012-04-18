;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;


(defpackage :wbtree (:nicknames :wb)
  (:documentation
   "This is an implementation of a weight-balanced binary tree data
    structure based on the following references:

   --  Adams (1992) 'Implementing Sets Efficiently in a Functional Language'
      Technical Report CSTR 92-10, University of Southampton.

   --  Hirai and Yamamoto (2011) 'Balancing Weight-Balanced Trees'
      Journal of Functional Programming / 21 (3): 287-307.

   --  MIT Scheme weight balanced tree as reimplemented by Yoichi Hirai and Kazuhiko Yamamoto
      using the revised non-variant algorithm recommended integer balance parameters from
      (Hirai/Yamomoto 2011).  {https://github.com/kazu-yamamoto/wttree}

   -- Bounded Balance

   -- Making Data Structures Persistent

   *   Overview

   A weight balanced binary search tree is a binary tree which maintains height in
   logarithmic proportion to the number of elements contained by
  ")
  (:use :common-lisp :named-readtables :dclx)
  (:export
    :+delta+
    :+gamma+
    :node
    :empty
    :emptyp
    :leaf
    :kv
    :lr
    :kvlr
    :kvlrs
    :node/k
    :node/v
    :node/l
    :node/r
    :node/s
    :node/kvlrs
    :node/kvlr
    :node/kv
    :node/lr
    :node/call
    :node/size
    :node/weight
    :node/create
    :node/singleton
    :node/values
    :node/join
    :node/min
    :node/max
    :node/remove-min
    :node/remove-max
    :node/concat2
    :node/inorder-fold
    :node/for-each
    :node/height
    :node/at-index
    :node/find
    :node/rank
    :node/add
    :node/remove
    :node/concat3
    :node/concat
    :node/split-lesser
    :node/split-greater
    :node/union
    :node/union-merge
    :node/difference
    :node/intersection
    :node/subsetp
    ))

(in-package :wbtree)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rotation Coefficients
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;   These parameters are, in general, not user-configurable as they
;;   must satisfy very specific mathematical requirements.  These
;;   particular parameters have been chosen because they are the only
;;   integer solution pair.  For further discussion on this subject, refer
;;   to {http://xxxx.xxx/}


(defvar +delta+ 3
  "The primary balancing rotation parameter that is used for the determination
   whether two subtrees of a node are in balance or require adjustment by means of a
   rotation operation.  The specific rotation to be performed is determined by
   {defvar wb::+gamma+}")


(defvar +gamma+ 2
  "The secondary balancing rotation parameter that is used for the determination
   whether a single or double rotation operation should occur, once it has been
   decided based on {defvar wb::+delta+} that a rotation is indeed required.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tree/type class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(defclass leaf (collection) ())

(defclass node ()
  ((k :initarg :k :initform nil :reader node/k
     )
    (v :initarg :v :initform nil :reader node/v
      )
    (l :initarg :l :initform nil :reader node/l
      )
    (r :initarg :r :initform nil :reader node/r
      )
    (s :initarg :s :initform 1   :reader node/s
      )))




;; (manardb:defmmclass persistent-node ()
;;   ((k :initarg :k :initform nil :reader node/k :persistent t)
;;     (v :initarg :v :initform nil :reader node/v :persistent t)
;;     (l :initarg :l :initform nil :reader node/l :persistent t)
;;     (r :initarg :r :initform nil :reader node/r :persistent t)
;;     (s :initarg :s :initform 1   :reader node/s :persistent t)))
;;   (:metaclass manardb:mm-metaclass))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple Instantiators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun node (k v l r s &optional (node-class 'node))
  (make-instance node-class :k k :v v :l l :r r :s s))

;; (defun pnode (k v l r s)
;;   (node k v l r s 'persistent-node))


(defvar leaf nil)
(defvar |<>| nil)

(defun  leaf () nil)
(defun  |<>| () nil)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; root container
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+()
(defclass root ()
  ((name   :initarg :name)
    (value :initarg :root)
    (key<  :initarg :key<)
    (node  :initarg :node)
    (leaf  :initarg :leaf))
  (:default-initargs
    :leaf (leaf)
    :name (gentemp "tree")
    :key< #'ORD:|COMPARE<|
    :node #'node
    :root (leaf))
  (:metaclass c2mop:funcallable-standard-class))

#+()
(defun make-tree (&rest protocol)
  (let (;;(tree-leaf (leaf))
         (tree-root (apply #'make-instance 'root protocol)))
    (prog1 tree-root 
      (with-slots (name key< node root) tree-root
        (macrolet ((key< (&rest args)
                     `(funcall ord:|COMPARE<| ,@args))
                    (node (&rest args)
                      `(apply `,node ,args)))
          (c2mop:set-funcallable-instance-function tree-root
            (lambda (op &rest args &aux (self tree-root))
              (printv op args self)))
          (setf (symbol-function name) tree-root)
          (setf (symbol-value name)    tree-root))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Readable / Printable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+()
(defmethod print-object ((x leaf) stream)
  (prog1 x
    (princ "<>" stream)))


(defmethod print-object ((x node) stream)
  (prog1 x
    (format stream "(node~{ ~S~})"
      (map 'list
        #'(lambda (s &aux (sn (c2mop:slot-definition-name s)))
            (if (slot-boundp x sn) (slot-value x sn) :||))
        (c2mop:class-slots (class-of x))))))


(defmethod print-object ((x pnode) stream)
  (prog1 x
    (format stream "(pnode~{ ~S~})"
      (map 'list
        #'(lambda (s &aux (sn (c2mop:slot-definition-name s)))
            (if (slot-boundp x sn) (slot-value x sn) :||))
        (c2mop:class-slots (class-of x))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constituent Accessors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun node/kvlrs (node)
  (with-slots (k v l r s) node
    (list k v l r s)))


(defun node/kvlr (node)
  (with-slots (k v l r) node
    (list k v l r)))


(defun node/kv (node)
  (with-slots (k v) node
    (list k v)))


(defun node/lr (node)
  (with-slots (l r) node
    (list l r)))


(eval-when (:execute)
  (let ((dummy (node :k :v :l :r :s)))
    (assert (notany #'null
              (list
                (eq     (node/k (node :k :v :l :r :s)) :K)
                (eq     (node/v (node :k :v :l :r :s)) :V)
                (eq     (node/l (node :k :v :l :r :s)) :L)
                (eq     (node/r (node :k :v :l :r :s)) :R)
                (eq     (node/s (node :k :v :l :r :s)) :S)
                (equalp (node/kvlr dummy) (list :K :V :L :R))
                (equalp (node/kv   dummy) (list :K :V))
                (equalp (node/lr   dummy) (list :L :R)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Destructuring Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro kv ((k v) node &body body)
  "destructure tree node: key value"
   (let ((gtree (gensym (symbol-name :tree-))))
     `(let ((,gtree ,node))
        (let ((,k  (node/k ,gtree))
              (,v  (node/v ,gtree)))
          ,@body))))


(defmacro lr ((l r) node &body body)
  "destructure tree node: left right"
   (let ((gtree (gensym (symbol-name :tree-))))
     `(let ((,gtree ,node))
        (let ((,l  (node/l ,gtree))
              (,r  (node/r ,gtree)))
          ,@body))))


(defmacro kvlr ((k v l r) node &body body)
  "destructure tree node: key value left right"
  (let ((gtree (gensym (symbol-name :tree-))))
    `(let ((,gtree ,node))
       (let ((,k  (node/k ,gtree))
              (,v  (node/v ,gtree))
              (,l  (node/l ,gtree))
              (,r  (node/r ,gtree)))
         ,@body))))


(defmacro kvlrs ((k v l r s) node &body body)
  "destructure tree node: key value left right size"
  (let ((gtree (gensym (symbol-name :tree-))))
    `(let ((,gtree ,node))
       (let ((,k  (node/k ,gtree))
              (,v  (node/v ,gtree))
              (,l  (node/l ,gtree))
              (,r  (node/r ,gtree))
              (,s  (node/s ,gtree)))
         ,@body))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Degenerate Instances
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun empty ()
  "create and return a value representing a tree which satisfies {defun wb::emptyp}"
  (leaf))


(defun emptyp (thing)
  "returns t if THING represents a tree containing no associations, otherwise nil"
  (null thing))


(eval-when (:execute)
  (assert (every #'identity
            (list (emptyp leaf) (emptyp |<>|) (emptyp (<>)) (emptyp nil)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Node Primatives 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun node/size (node)
  "returns the number of associations in tree rooted at node, with constant time
   complexity"
  (if (emptyp node) 0 (node/s node)))


(defun node/weight (node)
  "returns node weight as appropriate for rotation calculations using the
  'revised non-variant algorithm' for weight balanced binary tree."
  (+ 1 (node/size node)))


(defun node/singleton (k &optional (v t))
  "create and return a newly allocated weight balanced tree containing a single
   association, that value V with key K."
  (node k v nil nil 1))


(defun node/values (node)
  "destructure and return node constituents as multiple values"    
  (apply #'values (node/kvlrs)))


(defun node/call (node fn)
  "apply FN to the destructured constituent values of NODE. FN is a function taking
   four parameters: K, V, L, and R, where K is the key of NODE, V is the value of NODE,
   L is the left subtree of NODE, and R is the right subtree of NODE."
  (apply fn (node/kvlr node)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Construction and Rotation Operators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun node/create (k v l r)
  "Join left and right subtrees at root k/v.  Assumes all keys in l < k < all keys in r.
Also requires that the weight of each subtree is less than +delta+ times the weight of
the other."
  (node k v l r (+ 1 (node/size l) (node/size r))))

(eval-when (:execute)
  (assert
    (eq :success
      (match (node/create :k :v (node/singleton :lk :lv) (node/singleton :rk :rv))
        ((node :K :V (node :LK :LV leaf leaf 1) (node :RK :RV leaf leaf 1) 3)
          :success)))))


(defun single-l (a.k a.v x r)
  "Perform a single left rotation, moving Y, the left subtree of the right subtree of A,
  into the left subtree (shown below).
    This must occur in order to restore proper balance when the weight of the left
  subtree of node A is less then the weight of the right subtree of node A
  multiplied by rotation coefficient {defvar wb::+delta+}  and the weight of the
  left subtree of node B is less than the weight of the right subtree of node B
  multiplied by rotation coefficient {defvar wb::+gamma+} 
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
  (node/call R
    (lambda (b.k b.v y z)
      (node/create b.k b.v (node/create a.k a.v x y) z))))

(eval-when (:execute)
  (assert
    (eq :success
      (match
        (single-l :AK :AV
          (node :XK :XV (<>) (<>) 1)
          (node :BK :BV (node :YK :YV (<>) (<>) 1) (node :ZK :XZ (<>) (<>) 1) 3))
        ((node :BK :BV
           (node :AK :AV (node :XK :XV (<>) (<>) 1) (node :YK :YV (<>) (<>) 1) 3)
           (node :ZK :XZ (<>) (<>) 1) 5)
          :success)))))


(defun double-l (a.k a.v x r)
  "Perform a double left rotation, moving Y1, the left subtree of the left subtree
  of the right subtree of A, into the left subtree (shown below).
    This must occur in order to restore proper balance when the weight of the left
  subtree of node A is less then the weight of the right subtree of node A
  multiplied by rotation coefficient {defvar wb::+delta+}  and the weight of the
  left subtree of node B is greater than or equal to the weight of the right subtree 
  of node B multiplied by rotation coefficient {defvar wb::+gamma+} 
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
  (node/call R
    (lambda (c.k c.v b z)
      (node/call b
        (lambda (b.k b.v y1 y2)
          (node/create b.k b.v
            (node/create a.k a.v x y1)
            (node/create c.k c.v y2 z)))))))

(eval-when (:execute)
  (assert
    (eq :success
      (match
        (double-l :AK :AV
          (node :XK :XV (<>) (<>) 1)
          (node :CK :CV
            (node :BK :BV (node :Y1K :Y1V (<>) (<>) 1) (node :Y2K :Y2V (<>) (<>) 1) 3)
            (node :ZK :ZV (<>) (<>) 1) 5))
        ((node :BK :BV
           (node :AK :AV (node :XK :XV (<>) (<>) 1) (node :Y1K :Y1V (<>) (<>) 1) 3)
           (node :CK :CV (node :Y2K :Y2V (<>) (<>) 1) (node :ZK :ZV (<>) (<>) 1) 3) 7)
          :success))))) 



(defun single-r (b.k b.v l z)
  "Perform a single right rotation, moving Y, the right subtree of the left subtree of B,
  into the right subtree (shown below).
    This must occur in order to restore proper balance when the weight of the right
  subtree of node B is less then the weight of the left subtree of node B
  multiplied by rotation coefficient {defvar wb::+delta+}  and the weight of the
  right subtree of node A is less than the weight of the left subtree of node A
  multiplied by rotation coefficient {defvar wb::+gamma+} 
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
  (node/call L
    (lambda (a.k a.v x y)
      (node/create a.k a.v x (node/create b.k b.v y z)))))

(eval-when (:execute)
  (assert
    (eq :success
      (match
        (single-r :BK :BV
          (node :AK :AV (node :XK :XV (<>) (<>) 1) (node :YK :YV (<>) (<>) 1) 3)
          (node :ZK :XZ (<>) (<>) 1))
        ((node :AK :AV
           (node :XK :XV (<>) (<>) 1)
           (node :BK :BV (node :YK :YV (<>) (<>) 1) (node :ZK :XZ (<>) (<>) 1) 3) 5)
          :success)))))
        

(defun double-r (c.k c.v l z)
  "Perform a double right rotation, moving Y2, the right subtree of the right subtree
  of the left subtree of C, into the right subtree (shown below).
    This must occur in order to restore proper balance when the weight of the right
  subtree of node C is less then the weight of the left subtree of node C
  multiplied by rotation coefficient {defvar wb::+delta+}  and the weight of the
  right subtree of node B is greater than or equal to the weight of the left subtree 
  of node B multiplied by rotation coefficient {defvar wb::+gamma+} 
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
  (node/call L
    (lambda (a.k a.v x b)
      (node/call b
        (lambda (b.k b.v y1 y2)
          (node/create b.k b.v
            (node/create a.k a.v x y1)
            (node/create c.k c.v y2 z)))))))

(eval-when (:execute)
  (assert
    (eq :success
      (match
        (double-r :CK :CV
          (node :aK :aV
            (node :XK :XV (<>) (<>) 1)
            (node :BK :BV (node :Y1K :Y1V (<>) (<>) 1) (node :Y2K :Y2V (<>) (<>) 1) 3) 5)
          (node :ZK :ZV (<>) (<>) 1))
        ((node :BK :BV
           (node :AK :AV (node :XK :XV (<>) (<>) 1) (node :Y1K :Y1V (<>) (<>) 1) 3)
           (node :CK :CV (node :Y2K :Y2V (<>) (<>) 1) (node :ZK :ZV (<>) (<>) 1) 3) 7)
          :success)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Node/Join: Rebalance after any single insertion or deletion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun node/join (k v l r)
  "Join left and right subtrees at root k/v, performing a single or double rotation
to balance the resulting tree, if needed.  Assumes all keys in l < k < all keys in r,
and the relative weight balance of the left and right subtrees is such that no more than
one single/double rotation will result in each subtree being less than +delta+ times the
weight of the other."
  (let ((l.w (node/weight l)) (r.w (node/weight r)))    
    ;; (log:trace "node/join:  L:~D R:~D" l.w r.w)
    (cond
      ;; -- right too big --
      ((> r.w (* +delta+ l.w))
        (let ((r.l.w (node/weight (node/l r)))
               (r.r.w (node/weight (node/r r))))
          (if (< r.l.w (* +gamma+ r.r.w))
            (progn #+() (log:trace "single-l") (single-l k v l r))
            (progn #+() (log:trace "double-l") (double-l k v l r)))))
      ;; -- left too big --
      ((> l.w (* +delta+ r.w))
        (let ((l.l.w (node/weight (node/l l)))
               (l.r.w (node/weight (node/r l))))
          (if (< l.r.w (* +gamma+ l.l.w))
            (progn #+() (log:trace "single-r") (single-r k v l r))
            (progn #+() (log:trace "double-r") (double-r k v l r)))))
      ;; -- just right --
      (t
          (node/create k v l r)))))
               
          
;; (defun node/join (k v l r)
;;   (match (cons l r)
;;     ((cons (node _ _ _ _ l.w) (node _ _ _ _ r.w))
;;       (cond
;;         ((> r.w (* +delta+ l.w))
;;           (with-matching (node _ _ (node _ _ _ _ r.l.w) (node _ _ _ _ r.r.w) _)
;;             R
;;             (if (< r.l.w (* +gamma+ r.r.w))
;;               (single-l k v l r)
;;               (double-l k v l r))))
;;         ((> l.w (* +delta+ r.w)) 
;;           (with-matching (node _ _ (node _ _ _ _ l.l.w) (node _ _ _ _ l.r.w) _)
;;             L
;;             (if (< l.r.w (* +gamma+ l.l.w))
;;               (single-r k v l r)
;;               (double-r k v l r))))
;;         (t
;;           (node/create k v l r))))))

 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ordering Relation Independent Operators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun node/min (node)
  "Return the node containing the minimum key of the tree rooted at NODE"
  (cond
    ((emptyp node)          (error "min: empty tree"))
    ((emptyp (node/l node)) node)
    (t                      (node/min (node/l node)))))


(defun node/max (node)
  "Return the node containing the maximum key of the tree rooted at NODE"
  (cond
    ((emptyp node)          (error "max: empty tree"))
    ((emptyp (node/r node)) node)
    (t                      (node/max (node/r node)))))


(defun node/remove-min (node)
  "Return a tree the same as the one rooted at NODE, with the node containing the
   minimum key removed. See {defun wb::node/min}"
  (cond
    ((emptyp node)          (error "remove-min: empty tree"))
    ((emptyp (node/l node))  (node/r node))
    (t                       (node/join (node/k node) (node/v node)
                              (node/remove-min (node/l node)) (node/r node)))))


(defun node/remove-max (node)
  "Return a tree the same as the one rooted at NODE, with the node containing the
   maximum key removed. See {defun wb::node/max}"
  (cond
    ((emptyp node)          (error "remove-max: empty tree"))
    ((emptyp (node/r node))  (node/l node))
    (t                       (node/join (node/k node) (node/v node)
                                (node/l node) (node/remove-max (node/r node))))))


;; (node/remove-min (node t t <> (node/singleton :boo :hoo) t))
;;    => (node :BOO :HOO <> <> 1)


(defun node/concat2 (node1 node2)
  "Join two trees, the left rooted at NODE1, and the right at NODE2, performing a single
   or double rotation to balance the resulting tree, if needed. Assumes all keys in NODE1
   are smaller than all keys in NODE2, and the relative weight balance of NODE1 and NODE2
   is such that no more than one single/double rotation will result in each subtree being
   less than +delta+ times the weight of the other."
  (cond
    ((emptyp node1) node2)
    ((emptyp node2) node1)
    (t              (kv (k v) (node/min node2)
                      (node/join k v node1 (node/remove-min node2))))))


;; (node/concat2 (node 1 :LEFT <> <> 1) (node 2 :RIGHT <> <> 1))
;;    => (node 2 :RIGHT (node 1 :LEFT <> <> 1) <> 2)
;; (node/concat2 (node 2 2 (node 1 1 <> <> 1) <> 2) (node 3 3 <> <> 1))
;;    => (node 3 3 (node 2 2 (node 1 1 <> <> 1) <> 2) <> 3)
;; (node/concat2 <3> <5>)
;;    => (node #:|one17119| T
;;         (node #:|three17114| T
;;           (node #:|one17115| T <> <> 1)
;;           (node #:|one17116| T <> <> 1) 3)
;;         (node #:|five17117| T
;;           (node #:|three17118| T <>
;;             (node #:|one17120| T <> <> 1) 2)
;;           (node #:|one17121| T <> <> 1) 4)
;;         8)


(defun node/inorder-fold (fn base node)
  ""
  (labels ((the-fn (k &optional v a)
             (funcall (dclx::ensure-function fn) k v a))
            (fold (base node)
              (if (emptyp node) base
                (kvlr (k v l r)  node
                  (fold (the-fn k v (fold base r)) l)))))
    (fold base node)))
                          

;; (node/inorder-fold #'list nil <5>)
;;    => (#:|one17154| T (#:|three17153| T (#:|one17155| T (#:|five17152| T (#:|one17156| T NIL)))))
;;
;; (node/inorder-fold (dclx::compose #'nconc #'remove-duplicates #'list) nil <7>)
;;    => (:ONE (:SMALL (:ONE (:SEVEN (:ONE (:SMALL (:ONE NIL)))))))


(defun node/for-each (fn node)
  "For the side-effect, apply FN to each node of the tree rooted at NODE"
  (when (not (emptyp node))
    (kvlr (k v l r) node
      (node/for-each fn l)
      (funcall (ensure-function fn) k v)
      (node/for-each fn r))))

;; (node/for-each (dclx::compose #'print #'cons)  <7>)
;; =>  (:ONE   . :ONE) 
;;     (:SMALL . :SMALL) 
;;     (:ONE   . :ONE) 
;;     (:SEVEN . :SEVEN) 
;;     (:ONE   . :ONE) 
;;     (:SMALL . :SMALL) 
;;     (:ONE   . :ONE)


(defun node/height (node)
  "Return the distance from tree root NODE to the furthest of its leaves.  Operates
   with logarithmic speed complexity."
  (if (emptyp node)
    0
    (lr (l r) node
      (+ 1 (cl:max (node/height l) (node/height r))))))

;; (node/height <3>)   => 2
;; (node/height <5>)   => 3
;; (node/height <19>)  => 5
;; (node/height <183>) => 10


(defun node/at-index (node index &optional caller)
  (labels ((recur (node index)
             (lr (l r) node
               (let ((l.size (node/size l)))
                 (cond
                   ((< index l.size) (recur l index))
                   ((> index l.size) (recur r (- index (+ 1 l.size))))
                   (t node))))))
    (let ((bound (node/size node)))
      (if (not (and (<= 0 index) (< index bound)))
        (error "illegal range argument ~D ~S" index caller))
      (recur node index))))

;; (loop for i from 0 to 6  collect (node/k (node/at-index <7> i)))
;; (:ONE :SMALL :ONE :SEVEN :ONE :SMALL :ONE)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instance Methods dependant on ordinality relation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun node/find (k node)
  "takes d comparisons (d is depth of tree) rather than the trsaditional
   compare low compare high which takes on avg 1.5*(d - 1)"
  (labels ((recur (this best)
             (cond
               ((emptyp this)                    best)
               ((ORD:|COMPARE<| k (node/k this)) (recur (node/l this) best))
               (t                                (recur (node/r this) this)))))
    (let ((best (recur node nil)))
      (when best (unless (ORD:|COMPARE<| (node/k best) k) best)))))


;; (let (acc)
;;  (dolist (x '#1=(64 14 24 10 32 12 16))
;;    (push (node/k (node/find x
;;                    (node/create 16 :o 
;;                      (node/create 12 :y (node/singleton 10 :child) (node/singleton 14 :child))
;;                      (node/create 32 :y (node/singleton 24 :child) (node/singleton 64 :child)))))
;;      acc))
;;    acc)
;;
;; => (16 12 32 10 24 14 64)


(defun node/rank (k node rank)
  (cond
    ((emptyp node)                     nil)
    ((ORD:|COMPARE<| k (node/k node)) (node/rank
                                        k (node/l node)
                                        rank))
    ((ORD:|COMPARE>| k (node/k node)) (node/rank
                                        k (node/r node)
                                        (+ 1 rank (node/size (node/l node)))))
    (t
      (+ rank (node/size (node/l node))))))



(defun node/add (node k v)
  (if (emptyp node)
    (node/singleton k v)
    (node/call node
      (lambda (key val l r)
        (cond
          ((ORD:|COMPARE<| k key) (node/join key val (node/add l k v) r))
          ((ORD:|COMPARE<| key k) (node/join key val l (node/add r k v)))
          (t
            (node/create key v l r)))))))


;; (node/add nil 1 :x)
;; (node/add (node 1 :X (<>) (<>) 1) 2 :y)
;; (node 1 :X <> (node 2 :Y <> <> 1) 2)
(eval-when (:execute)
  (assert (eql 512 (node/size (loop :with tree :for i :from 1 :to 512
                                :do (setf tree (node/add tree i (code-char i)))
                                :finally (return tree))))))


(defun node/remove (x node)
  (if (emptyp node)
    (empty)
    (node/call node
      (lambda (key val l r)
        (cond
          ((ORD:|COMPARE<| x key) (node/join key val (node/remove x l) r))
          ((ORD:|COMPARE<| key x) (node/join key val l (node/remove x r)))
          (t
            (node/concat2 l r)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Full Concatentate Operations (Regardless of Weight)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun node/concat3 (k v l r)
  (cond
    ((emptyp l) (node/add r k v))
    ((emptyp r) (node/add l k v))
    (t          (let ((w1 (node/weight l))
                       (w2 (node/weight r)))
                  (cond
                    ((< (* +delta+ w1) w2) (node/call r
                                             (lambda (k2 v2 l2 r2)
                                               (node/join k2 v2 (node/concat3 k v l l2) r2))))
                    ((< (* +delta+ w2) w1) (node/call l
                                             (lambda (k1 v1 l1 r1)
                                               (node/join k1 v1 l1 (node/concat3 k v r1 r)))))
                    (t
                      (node/create k v l r)))))))



(defun node/concat (tree1 tree2)
  (cond
    ((emptyp tree1) tree2)
    ((emptyp tree2) tree1)
    (t
      (let ((minimum (node/min tree2)))
        (node/concat3
          (node/k minimum)
          (node/v minimum)
          tree1
          (node/remove-min tree2))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun node/split-lesser (node x)
  (cond
    ((emptyp node)                    (empty))
    ((ORD:|COMPARE<| x (node/k node)) (node/split-lesser (node/l node) x))    
    ((ORD:|COMPARE>| x (node/k node)) (node/concat3
                                        (node/k node)
                                        (node/v node)
                                        (node/l node)
                                        (node/split-lesser (node/r node) x)))
    (t (node/l node))))



(defun node/split-greater (node x)
  (cond
    ((emptyp node)            (empty))
    ((ORD:|COMPARE<| (node/k node) x) (node/split-greater (node/r node) x))    
    ((ORD:|COMPARE>| (node/k node) x) (node/concat3
                                        (node/k node)
                                        (node/v node)
                                        (node/split-greater (node/l node) x)
                                        (node/r node)))
    (t (node/r node))))



(defun node/union (tree1 tree2)
  (cond
    ((emptyp tree1) tree2)
    ((emptyp tree2) tree1)
    (t (node/call tree2
         (lambda (ak av l r)
           (let ((l1 (node/split-lesser tree1 ak))
                  (r1 (node/split-greater tree1 ak)))
             (node/concat3 ak av
               (node/union l1 l)
               (node/union r1 r))))))))

(deftype merge-direction ()
  `(member :left :right))


(defun node/union-merge (tree1 tree2 &optional (merge :right))
  (let ((do-merge-values
          (case merge
            (:left  #'first)
            (:right #'second)
            (t      (error "invalid merge type ~S, must be :left or :right" merge)))))
    (cond
      ((emptyp tree1) tree2)
      ((emptyp tree2) tree1)
      (t
        (node/call tree2
          (lambda (ak av l r)
            (let* ((node1 (node/find ak tree1))
                    (l1  (node/split-lesser  tree1 ak))
                    (r1  (node/split-greater tree1 ak))
                    (val (if node1
                           (funcall do-merge-values (list (node/v node1) av))
                           av)))
              (node/concat3 ak val
                (node/union-merge l1 l merge)
                (node/union-merge r1 r merge)))))))))


(defun node/difference (tree1 tree2)
  (cond
    ((emptyp tree1) (empty))
    ((emptyp tree2) tree1)
    (t
      (node/call tree2
        (lambda (ak av l r)
          (declare (ignore av))
          (let ((l1 (node/split-lesser tree1 ak))
                 (r1 (node/split-greater tree1 ak)))
            (node/concat
              (node/difference l1 l)
              (node/difference r1 r))))))))


(defun node/intersection (tree1 tree2)
  (cond
    ((emptyp tree1) (empty))
    ((emptyp tree2) (empty))
    (t
      (node/call tree2
        (lambda (ak av l r)
          (let ((l1 (node/split-lesser tree1 ak))
                 (r1 (node/split-greater tree1 ak)))
            (if (node/find ak tree1)
              (node/concat3 ak av
                (node/intersection l1 l)
                (node/intersection r1 r))
              (node/concat
                (node/intersection l1 l)
                (node/intersection r1 r)))))))))


(defun node/subsetp (tree1 tree2)
  (or (emptyp tree1)
    (and (<= (node/size tree1) (node/size tree2))
      (node/call tree1
        (lambda (k v l r)
          (declare (ignore v))
          (cond
            ((ORD:|COMPARE<| k (node/k tree2)) (and
                                         (node/subsetp l (node/l tree2))
                                         (node/find    k tree2)
                                         (node/subsetp r tree2)))
            ((ORD:|COMPARE>| k (node/k tree2)) (and
                                         (node/subsetp r (node/r tree2))
                                         (node/find    k tree2)
                                         (node/subsetp l tree2)))
            (t                         (and
                                         (node/subsetp l (node/l tree2))
                                         (node/subsetp r (node/r tree2))))))))))


              
(defun node/cons-enum (tree enum)
  (cond
    ((emptyp tree) enum)
    (t
      (kvlr (k v l r) tree
        (node/cons-enum l (list (cons k v) r enum))))))


  
