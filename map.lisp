;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;


(in-package :map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class map*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass map* (set:set*)
  ())

#+()
(defmethod print-object ((s set*) stream)
  (let ((val (dstm:atomic (dstm:read-var s)))
         (*package* (find-package :cl))))
    (format stream "#{ ~{~s ~}}" (set:elements val)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mapped pair
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass map-cell ()
  ((key
     :accessor map-cell-key
     :initform nil
     :initarg :key
     :documentation "domain constituent")
    (val
      :accessor map-cell-val
      :initform nil
      :initarg :val
      :documentation "range constituent"))
  (:documentation "storage cell for implementation of tree:rb-tree based key-value maps"))


(defun make-map-cell (&rest args)
  "convenience api routine for map-cell initialization"
  (apply #'make-instance 'map-cell args))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; map:type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun map:typep (thing)
  (or
    (null thing)
    (and (tree:typep thing) (cl:typep (tree:rb-tree-v thing) 'map-cell))))


(deftype map:type ()
  `(satisfies map:typep))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; map related ordinality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod ord:compare ((a map-cell) (b map-cell))
  "ordinal comparison of two map cells -- used by set:add"
  (ord:compare (map-cell-key a) (map-cell-key b)))


(defmethod ord:compare (a (b map-cell))
  "ordinal comparison of keys against map-cells"
  (ord:compare a (map-cell-key b)))


(defmethod ord:compare ((a map-cell) b)
  "ordinal comparison of map-cells against keys"
  (ord:compare (map-cell-key a) b))


(defmethod ord:compare ((a null) (b tree:rb-tree))
  "ordinal comparison of null against set elements and map cells"
  -1)


(defmethod ord:compare ((a tree:rb-tree) (b null))
  "ordinal comparison of null against set elements and map cells"
  1)


(defmethod ord:compare ((a null) (b null))
  "ordinal comparison of null values"
  0)


(defmethod ord:compare ((a tree:rb-tree) (b tree:rb-tree))
  "ordinal comparison of two collections of indeterminant type"
  (cond
    ((and (null a) (null b))  0)
    ((null a)                -1)
    ((null b)                 1)
    (t                        (let ((a-elem (tree:rb-tree-v a))
                                     (b-elem (tree:rb-tree-v b)))
                                (if (and
                                      (cl:typep a-elem 'map-cell)
                                      (cl:typep b-elem 'map-cell))
                                  (map:compare a b)
                                  (set:compare a b))))))


(defun empty ()
  "create empty map"
  (set:empty))


(defun is-empty (map)
  "return true if map contains no elements, otherwise false"
  (set:is-empty map))


(defun add (key val map)
  "retun map with key-val association added or replaced if already present"
  (set:add (make-map-cell :key key :val val)
    map))


(defun find-cell (key map &optional default)
  "return key-value cell with key in map, or default if not present"
  (cond ((null map) (values default nil))
    (t  (lvr (l v r) map
          (let ((c (ord:compare key (map-cell-key v))))
            (cond ((zerop c) (values  v t))
              (t         (find key (if (minusp c) l r)))
              ))))))


(defun mem (x map)
  "return true if map contains mapping 'x' -- x may be key or key-value cell"
  (typecase x
    (map-cell (set:mem x map))
    (t        (if (find-cell (map-cell-key x) map) t nil))))


(defun remove (x map)
  "return map with mapping 'x' removed  -- x may be key or key-value cell"
  (typecase x
    (map-cell (set:remove x map))
    (t        (set:remove (find-cell x map) map))))


(defun find (key map &optional default)
  "return value mapped to key in map, or default if not present"
  (cond
    ((null map) (values default nil))
    (t           (lvr (l v r) map
                   (let ((c (ord:compare key (map-cell-key v))))
                     (cond ((zerop c) (values (map-cell-val v) t))
                       (t         (find key (if (minusp c) l r)))))))))


(defun compare (map1 map2 &optional (cmp #'ord:compare))
  "ordinal comparison of two maps"
  (let ((e1 (cons-enum map1 nil))
         (e2 (cons-enum map2 nil)))
    (tagbody again
      (return-from compare
        (cond
          ((and (null e1) (null e2))   0)
          ((null e1)                  -1)
          ((null e2)                   1)
          (t (destructuring-bind (v1 r1 ee1) e1
               (destructuring-bind (v2 r2 ee2) e2
                 (let ((c (funcall cmp (map-cell-key v1) (map-cell-key v2))))
                   (cond
                     ((not (zerop c)) c)
                     (t              (let ((c (funcall cmp (map-cell-val v1) (map-cell-val v2))))
                                       (cond
                                         ((not (zerop c)) c)
                                         (t               (progn
                                                            (setf e1 (cons-enum r1 ee1)
                                                                  e2 (cons-enum r2 ee2))
                                                            (go again)) ))) ))))) ))))))


(defun equal (map1 map2 &optional (cmp #'ord:compare))
  "compare two maps for equality using cmp"
   (zerop (compare map1 map2 cmp)))


(defun fold (f map accu)
  "similar to reduce, takes three argument function f as in: (f key value accumulator)"
  (cond
    ((null map) accu)
    (t          (lvr (l v r) map
                  (fold f r
                    (funcall f
                      (map-cell-key v)
                      (map-cell-val v)
                      (fold f l accu)))))))


(defun map (f map)
  "return a newly created map where the keys of 'map' are associated with function f applied
   to the corresponding map values"
   (cond ((null map) nil)
     (t (let (new-map)
          (dolist (elem (set:elements map))
            (setf new-map (map:add (map-cell-key elem) (funcall f (map-cell-val elem)) new-map)))
          new-map))))
                

(defun mapi (f map)
  "return a newly created map where f is applied to the keys of 'map' and associated with
   the original corresponding map values"
   (cond ((null map) nil)
     (t (let (new-map)
          (dolist (elem (set:elements map))
            (setf new-map (map:add  (funcall f (map-cell-key elem)) (map-cell-val elem) new-map)))
          new-map))))


(defun iter (f map)
  "funcall a function f as (funcall #'f k v) on each key-value pair contained in map"
   (cond ((null map) nil)
         (t  (lvr (l v r) map
               (iter f l)
               (funcall f (map-cell-key v) (map-cell-val v))
               (iter f r)))
         ))
