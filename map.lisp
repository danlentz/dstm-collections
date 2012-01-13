;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;


(in-package :map)


(eval-when (:compile-toplevel :load-toplevel :execute)
   (import '(tree:cons-enum)))

;; --------------------------------------------

(defclass map-cell ()
   ((key  :accessor map-cell-key  :initform nil :initarg :key)
    (val  :accessor map-cell-val  :initform nil :initarg :val)))

(defun make-map-cell (&rest args)
   (apply #'make-instance 'map-cell args))

(defmethod ord:compare ((a map-cell) (b map-cell))
   ;; for comparing two map cells
   ;; used by set:add
   (ord:compare (map-cell-key a) (map-cell-key b)))

(defmethod ord:compare (a (b map-cell))
   ;; for comparing keys against map-cells
   (ord:compare a (map-cell-key b)))

(defun empty ()
   (set:empty))

(defun is-empty (map)
   (set:is-empty map))

(defun mem (x map)
   (set:mem x map))

(defun remove (x map)
   (set:remove x map))

(defun add (key val map)
   (set:add (make-map-cell
              :key key
              :val val)
             map))

(defun find (key map &optional default)
   (cond ((null map) (values default nil))
         (t  (lvr (l v r) map
               (let ((c (ord:compare key (map-cell-key v))))
                 (cond ((zerop c) (values (map-cell-val v) t))
                       (t         (find key (if (minusp c) l r)))
                       ))))
         ))

(defun compare (cmp map1 map2)
   (tagbody
    again
    (let ((e1 (cons-enum map1 nil))
          (e2 (cons-enum map2 nil)))

      (return-from compare
        (cond ((and (null e1)
                    (null e2))
               0)

              ((null e1) -1)
              ((null e2)  1)
              (t (destructuring-bind (v1 r1 ee1) e1
                   (destructuring-bind (v2 r2 ee2) e2
                     (let ((c (ord:compare (map-cell-key v1) (map-cell-key v2))))
                       (cond ((not (zerop c)) c)
                         (t (let ((c (funcall cmp (map-cell-val v1) (map-cell-val v2)) ))
                               (cond ((not (zerop c)) c)
                                     (t (setf e1 (cons-enum r1 ee1)
                                              e2 (cons-enum r2 ee2))
                                        (go again)) ))) ))))) )))))

(defun equal (cmp map1 map2)
   (zerop (compare cmp map1 map2)))

(defun fold (f map accu)
   (cond ((null map) accu)
         (t   (lvr (l v r) map
                (fold f r
                      (funcall f
                               (map-cell-key v)
                               (map-cell-val v)
                               (fold f l accu))) ))
         ))


(defun map (f map)
   (cond ((null map) nil)
     (t (let (new-map)
          (dolist (elem (set:elements map))
            (setf new-map (map:add (map-cell-key elem) (funcall f (map-cell-val elem)) new-map)))
          new-map))))
                

(defun mapi (f map)
   (cond ((null map) nil)
     (t (let (new-map)
          (dolist (elem (set:elements map))
            (setf new-map (map:add  (funcall f (map-cell-key elem)) (map-cell-val elem) new-map)))
          new-map))))


(defun iter (f map)
   (cond ((null map) nil)
         (t  (lvr (l v r) map
               (iter f l)
               (funcall f (map-cell-key v) (map-cell-val v))
               (iter f r)))
         ))
