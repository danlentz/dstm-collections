;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;


(in-package :set)


(defun height (node)
   (cond ((null node) 0)
         (t           (rb-tree-h node)) ))


;; add - insertion of one element
(defun add (x node)
  (labels ((addx (node)
             (cond ((null node) (values (singleton x) t))
               (t (lvrh (l v r h) node
                    (if (eql x v)
                      node
                      (let ((c (ord:compare x v)))
                        (cond ((zerop c) (make-rb-tree
                                           :l l
                                           :v x
                                           :r r
                                           :h h))
                          ;; to support maps (see below)
                          ;; cause new map value to be  substituted for old value
                          ((minusp c)
                            (multiple-value-bind (new-left  needs-rebal)
                              (addx l)
                              (cond ((eq l new-left) node)
                                (needs-rebal     (values  (bal new-left v r) t))
                                (t               (create  new-left v r))
                                )))
                          (t
                            (multiple-value-bind (new-right needs-rebal)
                              (addx r)
                              (cond ((eq r new-right) node)
                                (needs-rebal      (values (bal l v new-right) t))
                                (t                (create l v new-right))
                                )))
                          )))))
               )))
    (addx node)))


(defun min-elt (node)
   (cond ((null node) (not-found))
         ((null (rb-tree-l node)) (rb-tree-v node))
         (t     (min-elt (rb-tree-l node)))))


(defun max-elt (node)
   (cond ((null node) (not-found))
         ((null (rb-tree-r node)) (rb-tree-v node))
     (t (max-elt (rb-tree-r node)))))


;; remove-min-elt - remove the smallest element of the set
;; also useful for priority-queues

(defun remove-min-elt (node)
   (cond ((null node) (invalid-argument "Sets-internal::remove-min-elt"))
         ((null (rb-tree-l node)) (rb-tree-r node))
         (t (lvr (l v r) node
              (bal (remove-min-elt l) v r)))
         ))


(defun split (x tree)
  "split - split x s returns a triple (l present r) where
 l is the set of elements of s that are < x
 r is the set of elements of s that are > x
 present is false if s contains no element equal to x
 or true if s contains an element equal to x"
  (cond ((null tree) (list nil nil nil))
    (t (lvr (l v r) tree
         (let ((c (ord:compare x v)))
           (cond ((zerop c) (list l t r))
             ((minusp c)
               (destructuring-bind (ll pres rl) (split x l)
                 (list ll pres (join rl v r)) ))
             (t (destructuring-bind (lr pres rr) (split x r)
                  (list (join l v lr) pres rr) ))))))))


(defun empty ()
   nil)


(defun is-empty (tree)
   (null tree))


(defun mem (x tree)
   (cond ((null tree) nil)
         (t (lvr (l v r) tree
              (let ((c (ord:compare x v)))
                (or (zerop c)
                    (mem x (if (minusp c) l r)))
                )))
         ))


(defun singleton (x)
   (make-rb-tree
    :v x))


(defun remove (x tree)
  (cond ((null tree) nil)
    (t (lvr (l v r) tree
         (let ((c (ord:compare x v)))
           (cond ((zerop c) (merge l r))
             ((minusp c) (bal (remove x l) v r))
             (t (bal l v (remove x r)))
             ))))))


(defun union (s1 s2)
  (cond ((null s1) s2)
    ((null s2) s1)
    (t (lvrh (l1 v1 r1 h1) s1
         (lvrh (l2 v2 r2 h2) s2
           (cond ((>= h1 h2)
                   (if (= h2 1)
                     (add v2 s1)
                     (destructuring-bind (l2 _ r2) (split v1 s2)
                       (declare (ignore _))
                       (join (union l1 l2) v1 (union r1 r2)) )))
             (t (if (= h1 1)
                  (add v1 s2)
                  (destructuring-bind (l1 _ r1) (split v2 s1)
                    (declare (ignore _))
                    (join (union l1 l2) v2 (union r1 r2)))))))))))


(defun inter (s1 s2)
   (cond ((null s1) nil)
         ((null s2) nil)
         (t (lvr (l1 v1 r1) s1
              (destructuring-bind (l2 ans r2) (split v1 s2)
                (if ans
                    (join (inter l1 l2) v1 (inter r1 r2))
                  (concat (inter l1 l2) (inter r1 r2)) )
                )))
         ))

(defun diff (s1 s2)
   (cond ((null s1) nil)
         ((null s2) nil)
         (t (lvr (l1 v1 r1) s1
              (destructuring-bind (l2 ans r2) (split v1 s2)
                (if ans
                    (concat (diff l1 l2) (diff r1 r2))
                  (join (diff l1 l2) v1 (diff r1 r2)) )
                )))
         ))

(defun compare (s1 s2)
   (tagbody
    again
    (let ((e1 (cons-enum s1 nil))
          (e2 (cons-enum s2 nil)))
      (return-from compare
        (cond ((and (null e1) (null e2)) 0)
              ((null e1)      -1)
              ((null e2)       1)
              (t (destructuring-bind (v1 r1 ee1) e1
                   (destructuring-bind (v2 r2 ee2) e2
                     (let ((c (ord:compare v1 v2)))
                       (if (zerop c)
                           (progn
                             (setf e1 (cons-enum r1 ee1)
                                   e2 (cons-enum r2 ee2))
                             (go again))
                         ;; else
                         c)) )))
              )) )))

(defun equal (s1 s2)
  (and
    (set:subset s1 s2)
    (set:subset s2 s1)))

;; TODO: faster but does not work?
;;  (zerop (compare s1 s2)))


(defun subset (s1 s2)
   (cond ((null s1) t)
         ((null s2) nil)
         (t (lvr (l1 v1 r1) s1
              (lvr (l2 v2 r2) s2
                (let ((c (ord:compare v1 v2)))
                  (cond ((zerop c) (and (subset l1 l2)
                                        (subset r1 r2)))
                        ((minusp c) (and (subset (make-rb-tree
                                                  :l l1
                                                  :v v1)
                                                 l2)
                                         (subset r1 s2)))
                        (t (and (subset (make-rb-tree
                                         :v v1
                                         :r r1)
                                        r2)
                                (subset l1 s2))))))))))


(defun iter (fn s)
   (cond ((null s) nil)
         (t        (lvr (l v r) s
                     (iter fn l)
                     (funcall fn v)
                     (iter fn r)))))



(defun fold (fn s accu)
   (cond ((null s) accu)
         (t       (lvr (l v r) s
                    (fold fn r (funcall fn v (fold fn l accu)))))))



(defun for-all (pred s fn)
   (cond ((null s) t)
         (t        (lvr (l v r) s
                     (and (funcall pred v)
                       (funcall fn v))
                     (for-all pred l fn)
                     (for-all pred r fn)
                       ))))


(defun exists (pred s)
   (cond ((null s) nil)
         (t        (lvr (l v r) s
                     (or (funcall pred v)
                         (exists pred l)
                       (exists pred r))))))


(defun filter (pred s)
   (labels ((filt (accu s)
              (cond ((null s)  accu)
                    (t         (lvr (l v r) s
                                 (filt (filt (if (funcall pred v)
                                                 (add v accu)
                                               accu)
                                             l)
                                       r)))
                    )))
     (filt nil s)))


(defun partition (pred s)
   (labels ((part (pair s)
              (destructuring-bind (tp fp) pair
                (cond ((null s) pair)
                      (t        (lvr (l v r) s
                                  (part (part (if (funcall pred v)
                                                  (list (add v tp) fp)
                                                (list tp (add v fp)))
                                              l)
                                        r)))
                      ))))
     (part (list nil nil) s)))


(defun cardinal (s)
   (cond ((null s) 0)
         (t     (lr (l r) s
                  (+ (cardinal l) 1 (cardinal r))))
         ))


(defun elements (s)
   (labels ((iter (accu s)
              (cond ((null s)  accu)
                    (t         (lvr (l v r) s
                                 (iter (cons v (iter accu r)) l)))
                    )))
     (iter nil s)))


(defun choose (s)
   (min-elt s))
