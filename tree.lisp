;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;


(in-package :tree)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conditions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; placeholders for now

(defun invalid-argument (fn-name)
   (error "Invalid argument in: ~A" fn-name))

(defun not-found ()
   (error "Not found"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RB-Tree Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass tree:rb-tree (abstract-quad)
  ((quad:a
     :accessor tree:rb-tree-l
     :initform nil
     :initarg :l
     :documentation "left child of this node containing all elements less than v")
    (quad:b
      :accessor tree:rb-tree-v
      :initform nil
      :initarg :v
      :documentation "the value or value-cell represented by this node")
    (quad:c
      :accessor tree:rb-tree-r
      :initform nil
      :initarg :r
      :documentation "right child of this node containing all elements greater than v")
    (quad:d
      :accessor tree:rb-tree-h
      :initform 1
      :initarg :h
      :documentation "the height of this node, as the distance in child links to the
                      furthest leaf.  Rather than annotation using literal :red :black
                      properties, this red-black tree implementation maintains balance
                      based on the relative heights of left and right subtrees, which
                      may never exceed 3."))
  (:documentation
    "Sets are represented by balanced binary trees The heights of
     children differ by at most 2.  Tree nodes are quadruples (l v r h)
     where: l = left child, v = value, r = right child, h = height"))


(defmethod print-object ((tree rb-tree) stream)
  (print-unreadable-object (tree stream :type t :identity t)
    (format stream "{| V: ~A H: ~A |}"
      (qbr tree)
      (qdr tree))))


(defun tree:make-rb-tree (&rest args)
  "convenience api routine for rb-tree initialization"
  (apply #'make-instance 'rb-tree args))


(defgeneric tree:rb-tree-p (x))


(defmethod  tree:rb-tree-p (x)
   (declare (ignore x))
   nil)


(defmethod  tree:rb-tree-p ((tree rb-tree))
   tree)


(defun tree:typep (thing)
  "generic rb-tree type-predicate"
  (if (rb-tree-p thing)
    t))


(deftype tree:type ()
  "self-balancing binary tree used as a basis for higher-level collection types"
  `(satisfies tree:typep))


(defun tree:create (l v r &optional (hl (height l)) (hr (height r)))
  "create a tree node with left son l, value v, and right son r.
   Must have all elements of l < v < all elements of r.
   l and r must be balanced and have a height difference =< 2"
  (make-rb-tree
    :l l
    :v v
    :r r
    :h (1+ (cl:max hl hr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; destructuring macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro tree:lr ((l r) tree &body body)
  "destructure tree node: left right"
   (let ((gtree (gensym (symbol-name :tree-))))
     `(let ((,gtree ,tree))
        (let ((,l  (rb-tree-l ,gtree))
              (,r  (rb-tree-r ,gtree)))
          ,@body)) ))


(defmacro tree:lvr ((l v r) tree &body body)
  "destructure tree node: left value right"
   (let ((gtree (gensym (symbol-name :tree-))))
     `(let ((,gtree ,tree))
        (let ((,l  (rb-tree-l ,gtree))
              (,v  (rb-tree-v ,gtree))
              (,r  (rb-tree-r ,gtree)))
          ,@body)) ))


(defmacro tree:lvrh ((l v r h) tree &body body)
  "destructure tree node: left value right height"
   (let ((gtree (gensym (symbol-name :tree-))))
     `(let ((,gtree ,tree))
        (let ((,l  (rb-tree-l ,gtree))
              (,v  (rb-tree-v ,gtree))
              (,r  (rb-tree-r ,gtree))
              (,h  (rb-tree-h ,gtree)))
          ,@body)) ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree Manipulations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tree:bal (l v r)
  "bal -- similar to create, but also performs one step of rebalancing, if necessary.
   assumes l and r balanced and height difference <= 3"
  (flet ((invalid-arg ()   (invalid-argument "tree:bal")))
    (let ((hl (height l))
           (hr (height r)))
      (cond
        ((> hl (+ 2 hr))   (cond
                             ((rb-tree-p l) (lvr (ll lv lr) l
                                              (if (>= (height ll) (height lr))
                                                (create ll lv (create lr v r))
                                                (cond ((rb-tree-p lr) (lvr (lrl lrv lrr) lr
                                                                        (create
                                                                          (create ll lv lrl)
                                                                          lrv
                                                                          (create lrr v r))))
                                                  (t (invalid-arg))))))
                             (t (invalid-arg))))
        ((> hr (+ 2 hl))  (cond
                            ((rb-tree-p r) (lvr (rl rv rr) r
                                             (if (>= (height rr) (height rl))
                                               (create (create l v rl) rv rr)
                                               (cond ((rb-tree-p rl) (lvr (rll rlv rlr) rl
                                                                       (create
                                                                         (create l v rll)
                                                                         rlv
                                                                         (create  rlr rv rr))))
                                                 (t (invalid-arg))))))
                            (t (invalid-arg))))
        (t (create l v r hl hr))))))


(defun tree:join (l v r)
  "join -- same as create and bal, but no assumptions are made on the
   relative heights of l and r"
  (cond ((null l) (add v r))
    ((null r) (add v l))
    (t (lvrh (ll lv lr lh) l
         (lvrh (rl rv rr rh) r
           (cond ((> lh (+ 2 rh)) (bal ll lv (join lr v r)))
             ((> rh (+ 2 lh)) (bal (join l v rl) rv rr))
             (t (create l v r))
             )))) ))


(defun tree:merge (t1 t2)
  "merge -- merge two trees l and r into one.
   All elements of l must precede the elements of r
   Assume height difference <= 2"
   (cond ((null t1) t2)
         ((null t2) t1)
         (t (bal t1 (tree:min t2) (remove-min t2)))))


(defun tree:concat (t1 t2)
  "concat - merge two trees l and r into one.
   All elements of l must precede the elements of r.
   No assumptions on the heights of l and r."
  (cond ((null t1) t2)
    ((null t2) t1)
    (t (join t1 (tree:min t2) (remove-min t2)))))


(defun tree:cons-enum (s e)
  "efficient mechanism to accomplish partial enumeration of tree-structure into
   a consp representation without incurring the overhead of operating over the
   entire tree.  Used internally for implementation of higher-level collection
   api routines"
  (cond ((null s) e)
    (t (lvr (l v r) s
         (cons-enum l (list v r e))))))

