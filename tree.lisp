;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;


(in-package :tree)

(eval-when (:compile-toplevel :load-toplevel :execute)
   (import '(set:height
             set:add
             set:min-elt
             set:remove-min-elt)))

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

(defclass rb-tree (abstract-quad)
  ((quad:a   :accessor rb-tree-l  :initform nil  :initarg :l)
    (quad:b   :accessor rb-tree-v  :initform nil  :initarg :v)
    (quad:c   :accessor rb-tree-r  :initform nil  :initarg :r)
    (quad:d   :accessor rb-tree-h  :initform 1    :initarg :h))
  (:documentation "Sets are represented by balanced binary trees The
 heights of children differ by at most 2.  Tree nodes are
 quadruples (l v r h) where: - l = left child, - v = value, - r =
 right child, - h = height"))


(defmethod print-object ((tree rb-tree) stream)
  (print-unreadable-object (tree stream :type t :identity t)
    (format stream "{| V: ~A H: ~A |}"
      (qbr tree)
      (qdr tree))))


(defun make-rb-tree (&rest args)
  (apply #'make-instance 'rb-tree args))


(defgeneric rb-tree-p (x))


(defmethod  rb-tree-p (x)
   (declare (ignore x))
   nil)


(defmethod  rb-tree-p ((tree rb-tree))
   tree)


(defun tree:typep (thing)
  (if (rb-tree-p thing)
    t))


(deftype tree:type (thing)
  '(satisfies #'tree:typep thing))


(defun create (l v r &optional (hl (height l)) (hr (height r)))
  "create a tree node with left son l, value v, and right son r.
   Must have all elements of l < v < all elements of r.
   l and r must be balanced and have a height difference =< 2"
  (make-rb-tree
    :l l
    :v v
    :r r
    :h (1+ (max hl hr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; destructuring macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro lr ((l r) tree &body body)
  "destructure tree node: left right"
   (let ((gtree (gensym (symbol-name :tree-))))
     `(let ((,gtree ,tree))
        (let ((,l  (rb-tree-l ,gtree))
              (,r  (rb-tree-r ,gtree)))
          ,@body)) ))


(defmacro lvr ((l v r) tree &body body)
  "destructure tree node: left value right"
   (let ((gtree (gensym (symbol-name :tree-))))
     `(let ((,gtree ,tree))
        (let ((,l  (rb-tree-l ,gtree))
              (,v  (rb-tree-v ,gtree))
              (,r  (rb-tree-r ,gtree)))
          ,@body)) ))


(defmacro lvrh ((l v r h) tree &body body)
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

(defun bal (l v r)
  "same as create, but performs one step of rebalancing if  necessary
   assumes l and r balanced and height difference <= 3"
  (flet ((invalid-arg ()
           (invalid-argument "Set:bal")))
    (let ((hl (height l))
           (hr (height r)))
      (cond ((> hl (+ 2 hr))
              (cond ((rb-tree-p l)
                      (lvr (ll lv lr) l
                        (if (>= (height ll) (height lr))
                          (create ll lv (create lr v r))
                          ;; else
                          (cond ((rb-tree-p lr)
                                  (lvr (lrl lrv lrr) lr
                                    (create (create ll lv lrl) lrv  (create lrr v r))))

                            (t (invalid-arg)) )) ))
                (t (invalid-arg)) ))

        ((> hr (+ 2 hl))
          (cond ((rb-tree-p r)
                  (lvr (rl rv rr) r
                    (if (>= (height rr) (height rl))
                      (create (create l v rl) rv rr)
                      ;; else
                      (cond ((rb-tree-p rl)
                              (lvr (rll rlv rlr) rl
                                (create (create l v rll) rlv (create  rlr rv rr))))

                        (t (invalid-arg)) )) ))

            (t (invalid-arg)) ))

        (t (create l v r hl hr)) )) ))


(defun join (l v r)
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


(defun merge (t1 t2)
  "merge -- merge two trees l and r into one.
   All elements of l must precede the elements of r
   Assume height difference <= 2"
   (cond ((null t1) t2)
         ((null t2) t1)
         (t (bal t1 (min-elt t2) (remove-min-elt t2)))))


(defun concat (t1 t2)
  "concat - merge two trees l and r into one.
   All elements of l must precede the elements of r.
   No assumptions on the heights of l and r."
  (cond ((null t1) t2)
    ((null t2) t1)
    (t (join t1 (min-elt t2) (remove-min-elt t2)))))


(defun cons-enum (s e)
  (cond ((null s) e)
    (t (lvr (l v r) s
         (cons-enum l (list v r e))))))

