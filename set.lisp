;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;


(in-package :set)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set collection classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass set* (dstm:dstm-var)
  ()
  (:documentation "A DSTM transactional variant of the base functional data structure
   set implementation which may be used in a manner similar to ordinary mutable
   collection types, such as a list."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set:type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun set:typep (thing)
  "set collection type-predicate"
  (or
    (null thing)
    (cl:typep thing 'set:set*) 
    (tree:typep thing)))


(deftype set:type ()
  "collection of arbitrary, unique, elements in order determined by defined ordinal
   comparison relations on element types and content values"
  `(satisfies set:typep))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set api
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tree:height (node)
  "convenience api for returning rb-tree height of collection node"
   (cond ((null (dstm:safe-value node)) 0)
         (t           (rb-tree-h (dstm:safe-value node)))))


(defun set:add (x collection)
  "return a set the same as node with element 'x' added if not already present. as
   a second return value, return true if actually added"
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
    (addx (dstm:safe-value collection))))


(defun set:add* (element collection)
  ""
  (check-type collection dstm::var)
  (setf (dstm:value collection) (set:add element collection))
  collection)


(defun set:min (collection)
  "return the smallest element present in the collection"
  (let ((node (dstm:safe-value collection)))
    (cond ((null node) (tree::not-found))
      ((null (rb-tree-l node)) (rb-tree-v node))
      (t     (tree:min (rb-tree-l node))))))


(defun set:max (collection)
  "return the greatest element present in the collection"
  (let ((node (dstm:safe-value collection)))
    (cond ((null node) (tree::not-found))
      ((null (rb-tree-r node)) (rb-tree-v node))
      (t (tree:max (rb-tree-r node))))))


(defun set:remove-min (collection)
  "return a collection with the smallest element removed -- useful for priority-queues"
  (let ((node (dstm:safe-value collection))) 
    (cond ((null node)         (tree::invalid-argument "tree:remove-min"))
      ((null (rb-tree-l node)) (rb-tree-r node))
      (t                       (lvr (l v r) node
                                 (bal (remove-min l) v r))))))


(defun set:remove-min* (collection)
  ""
  (check-type collection dstm::var)
  (setf (dstm:value collection) (set:remove-min collection))
  collection)


(defun set:remove-max (collection)
  "return a collection with the greatest element removed -- useful for priority-queues"
  (let ((node (dstm:safe-value node)))
    (cond ((null node)         (tree::invalid-argument "set:remove-max"))
      ((null (rb-tree-r node)) (rb-tree-l node))
      (t                       (lvr (l v r) node
                                 (bal l v (remove-max r)))))))


(defun set:remove-max* (collection)
  ""
  (check-type collection dstm::var)
  (setf (dstm:value collection) (set:remove-max collection))
  collection)


(defun set:split (x tree)
  "returns a triple (l present r) where:
     l       - is the set of elements of s that are < x
     r       - is the set of elements of s that are > x
     present - is false if s contains no element equal to x
               or true if s contains an element equal to x"
  (let ((tree (dstm:safe-value tree)))
    (cond ((null tree) (list nil nil nil))
      (t (lvr (l v r) tree
           (let ((c (ord:compare x v)))
             (cond ((zerop c) (list l t r))
               ((minusp c)
                 (destructuring-bind (ll pres rl) (split x l)
                   (list ll pres (join rl v r)) ))
               (t (destructuring-bind (lr pres rr) (split x r)
                    (list (join l v lr) pres rr))))))))))


(defun set:empty ()
  "create empty set"
   nil)


(defun set:empty* ()
  "create a transactional empty set"
   (set:make* nil))


(defun set:is-empty (collection)
  "return true if set contains no elements, otherwise false"
  (let ((tree (dstm:safe-value collection)))
    (null tree)))


(defun set:mem (x collection)
  "return true if set contains element x"
  (let ((tree (dstm:safe-value collection)))
    (cond
      ((null tree) nil)
      (t           (lvr (l v r) tree
                     (let ((c (ord:compare x v)))
                       (or (zerop c)
                         (mem x (if (minusp c) l r)))))))))


(defun set:singleton (x)
  "create set containing only the element x"
  (make-rb-tree :v x))

(defun set:singleton* (x)
  "create transactional collection containing only the element x"
  (set:make* (make-rb-tree :v x)))
  

(defun set:remove (x collection)
  "return a collection the same as argument with the element 'x' removed if present"
  (let ((tree (dstm:safe-value collection)))
    (cond
      ((null tree) nil)
      (t           (lvr (l v r) tree
                     (let ((c (ord:compare x v)))
                       (cond
                         ((zerop  c) (merge l r))
                         ((minusp c) (bal (remove x l) v r))
                         (t          (bal l v (remove x r))))))))))


(defun set:remove* (x collection)
  ""
  (check-type collection dstm::var)
  (setf (dstm:value collection) (set:remove x collection))
  collection)


(defun parallel-union (s1 s2)
  "return a collection containing all the elements (without duplicates) of s1 and s2"
  (let ((s1 (dstm:safe-value s1))
         (s2 (dstm:safe-value s2)))
    (cond
      ((null s1) s2)
      ((null s2) s1)
      (t         (lvrh (l1 v1 r1 h1) s1
                   (lvrh (l2 v2 r2 h2) s2
                     (cond ((>= h1 h2)
                             (if (= h2 1)
                               (add v2 s1)
                               (destructuring-bind (l2 _ r2) (split v1 s2)
                                 (declare (ignore _))
                                 (lparallel:plet ((lunion (parallel-union l1 l2))
                                                   (runion (parallel-union r1 r2)))
                                   (join lunion v1 runion) ))))
                       (t (if (= h1 1)
                            (add v1 s2)
                            (destructuring-bind (l1 _ r1) (split v2 s1)
                              (declare (ignore _))
                              (lparallel:plet ((lunion (parallel-union l1 l2))
                                                (runion (parallel-union r1 r2)))
                                (join lunion v2 runion))))))))))))


(defun set:union (s1 s2)
  "return a collection containing all the elements (without duplicates) of s1 and s2"
  (let ((s1 (dstm:safe-value s1))
         (s2 (dstm:safe-value s2)))
    (cond
      ((null s1) s2)
      ((null s2) s1)
      (t         (lvrh (l1 v1 r1 h1) s1
                   (lvrh (l2 v2 r2 h2) s2
                     (cond ((>= h1 h2)
                             (if (= h2 1)
                               (add v2 s1)
                               (destructuring-bind (l2 _ r2) (split v1 s2)
                                 (declare (ignore _))
                                 (funcall #'join (union l1 l2) v1 (union r1 r2)) )))
                       (t (if (= h1 1)
                            (add v1 s2)
                            (destructuring-bind (l1 _ r1) (split v2 s1)
                              (declare (ignore _))
                              (funcall #'join (union l1 l2) v2 (union r1 r2))))))))))))


(defun set:union* (tx-collection tx-or-non-tx-collection)
  ""
  (check-type tx-collection dstm::var)
  (check-type tx-or-non-tx-collection set:type)
  (setf (dstm:value tx-collection) (set:union tx-collection tx-or-non-tx-collection))
  tx-collection)


(defun set:diff (s1 s2)
  (let ((s1 (dstm:safe-value s1))
         (s2 (dstm:safe-value s2)))
    (cond ((null s1) nil)
      ((null s2) nil)
      (t (lvr (l1 v1 r1) s1
           (destructuring-bind (l2 ans r2) (split v1 s2)
             (if ans
               (concat (diff l1 l2) (diff r1 r2))
               (join (diff l1 l2) v1 (diff r1 r2)))))))))



(defun set:inter (s1 s2)
  "return a collection containing all elements that are present in both s1 and s2"
  (let ((s1 (dstm:safe-value s1))
         (s2 (dstm:safe-value s2)))
    (cond
      ((null s1) nil)
      ((null s2) nil)
      (t         (lvr (l1 v1 r1) s1
                   (destructuring-bind (l2 ans r2) (split v1 s2)
                     (if ans
                       (join (inter l1 l2) v1 (inter r1 r2))
                       (concat (inter l1 l2) (inter r1 r2)))))))))


(defun set:inter* (tx-collection tx-or-non-tx-collection)
  ""
  (check-type tx-collection dstm::var)
  (check-type tx-or-non-tx-collection set:type)
  (setf (dstm:value tx-collection) (set:inter tx-collection tx-or-non-tx-collection))
  tx-collection)


(defun parallel-inter (s1 s2)
  "return a collection containing all elements that are present in both s1 and s2"
  (let ((s1 (dstm:safe-value s1))
         (s2 (dstm:safe-value s2)))
    (cond
      ((null s1) nil)
      ((null s2) nil)
      (t         (lvr (l1 v1 r1) s1
                   (destructuring-bind (l2 ans r2) (split v1 s2)
                     (plet ((linter (parallel-inter l1 l2)) (rinter (parallel-inter r1 r2)))
                       (if ans
                         (join linter v1 rinter)
                         (concat linter rinter)))))))))



(defun set:compare (s1 s2 &optional (cmp #'ord:compare)) 
  "return 3-way ordinal comparison of sets s1 and s2 with the following return-value semantics:
    0  -> set0 is EQAL-TO      set1
   -1  -> set0 is LESS-THAN    set1
    1  -> set0 is GREATER-THAN set1"
  (let* ((s1 (dstm:safe-value s1))
          (s2 (dstm:safe-value s2))
          (e1 (cons-enum s1 nil))
          (e2 (cons-enum s2 nil)))
    (tagbody again
      (return-from compare
        (cond
          ((and (null e1) (null e2)) 0)
          ((null e1)                -1)
          ((null e2)                 1)
          (t                         (destructuring-bind (v1 r1 ee1) e1
                                       (destructuring-bind (v2 r2 ee2) e2
                                         (let ((c (funcall cmp v1 v2)))
                                           (if (zerop c)
                                             (progn
                                               (setf
                                                 e1 (cons-enum r1 ee1)
                                                 e2 (cons-enum r2 ee2))
                                               (go again))
                                             c))))))))))


(defun set:equal (s1 s2)
  "return true if both hold:  s1 is a subset of s2, and s2 is a subset of s1"
  (let ((s1 (dstm:safe-value s1))
         (s2 (dstm:safe-value s2)))
    (zerop (compare s1 s2))))


(defun set:subset (s1 s2)
  "return true if all elements of s1 are present in s2"
  (let ((s1 (dstm:safe-value s1))
         (s2 (dstm:safe-value s2)))
    (cond
      ((null s1)   t)
      ((null s2) nil)
      (t      (lvr (l1 v1 r1) s1
                (lvr (l2 v2 r2) s2
                  (let ((c (ord:compare v1 v2)))
                    (cond
                      ((zerop c)  (and
                                    (subset l1 l2)
                                    (subset r1 r2)))
                      ((minusp c) (and
                                    (subset (make-rb-tree :l l1 :v v1) l2)
                                    (subset r1 s2)))
                      (t          (and
                                    (subset (make-rb-tree :v v1 :r r1) r2)
                                    (subset l1 s2)))))))))))


(defun set:iter (fn s)
  "funcall fn on each element of set s"
  (let ((s (dstm:safe-value s)))
    (cond
      ((null s) nil)
      (t        (lvr (l v r) s
                  (iter fn l)
                  (funcall fn v)
                  (iter fn r))))))


(defun set:fold (fn s accu)
  "similar to reduce, takes three argument function f as in: (f key value accumulator)"
  (let ((s (dstm:safe-value s)))
    (cond ((null s) accu)
      (t       (lvr (l v r) s
                 (fold fn r (funcall fn v (fold fn l accu))))))))


(defun set:for-all (pred s fn)
  "funcall fn on all elements of set s satisfying pred"
  (let ((s (dstm:safe-value s)))
    (cond ((null s) t)
      (t        (lvr (l v r) s
                  (when (funcall pred v) (funcall fn v))
                  (for-all pred l fn)
                  (for-all pred r fn))))))


(defun set:exists (pred s)
  "return true if any element of s satisfies pred"
  (let ((s (dstm:safe-value s)))
    (cond ((null s) nil)
      (t        (lvr (l v r) s
                  (or (funcall pred v)
                    (exists pred l)
                    (exists pred r)))))))


(defun set:filter (pred s)
  "return a new set containing all elements of s which satisfy pred"
  (let ((s (dstm:safe-value s)))
    (labels ((filt (accu s)
               (cond ((null s)  accu)
                 (t         (lvr (l v r) s
                              (filt (filt (if (funcall pred v)
                                            (add v accu)
                                            accu)
                                      l)
                                r)))
                 )))
      (filt nil s))))


(defun set:filter* (pred s)
  ""
  (check-type s dstm::var)
  (setf (dstm:value s) (set:filter pred s))
  s)


(defun set:partition (pred s)
  "return a list containing two new sets: the first containing those elements
   of s which satisfy pred, and the second containing those which do not"
  (let ((s (dstm:safe-value s)))
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
      (part (list nil nil) s))))


(defun set:cardinal (s)
  "return the number of elements contained in set s"
  (let ((s (dstm:safe-value s)))
    (cond ((null s) 0)
      (t     (lr (l r) s
               (+ (cardinal l) 1 (cardinal r)))))))


(defun set:elements (s)
  "return a list containing all elements of s"
  (let ((s (dstm:safe-value s)))
    (labels ((iter (accu s)
               (cond ((null s)  accu)
                 (t         (lvr (l v r) s
                              (iter (cons v (iter accu r)) l)))
                 )))
      (iter nil s))))


(defun set:dup (s)
  "return a new set which is set:equal the original s"
  (let ((s (dstm:safe-value s)))
    (cond
      ((null s) nil)
      (t (lvr (l v r) s
           (set:add v (set:union (set:dup l) (set:dup r))))))))


(defun dup-alt (s)
  "return a new set which is set:equal the original s"
  (let ((s (dstm:safe-value s)))
    (let (new-set)
      (dolist (elem (set:elements s))
        (setq new-set (set:add elem new-set)))
      new-set)))

  
(defun set:make (&optional (from (set:empty)))
  "end-user api for construction of a new set optionally initialized to contain
   elements derived from various types of source data"
  (etypecase from
    (null     (set:empty))
    (dstm:var (set:make (dstm:safe-value from)))
    (ord:proper-list  (let (set)
                          (dolist (elem from)
                            (if (or (atom elem) (ord:proper-list-p elem))
                              (setq set (set:add elem set))
                              (error "Cannot add ~S. Sets admit Only atom or proper-lists"
                                from)))
                        set))
    (cons     (error "only proper-lists may be members of a set"))
    (seq:type (set:make (seq:list from)))
    (map:type (error "sets cannot be created from  maps"))
    (set:type (set:dup from))
    (sequence (set:make (coerce from 'cl:list)))
    (atom     (set:singleton from))))


(defun set:make* (&optional (from (set:empty)))
  (dstm:create-var (set:make from) 'set*))
