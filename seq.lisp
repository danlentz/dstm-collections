;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;


(in-package :seq)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; seq element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass seq-cell (map::map-cell)
  ((map::key
     :accessor seq-cell-key
     :initform 0
     :initarg :key
     :documentation "position constituent")
    (map::val
      :accessor seq-cell-val
      :initform nil
      :initarg :val
      :documentation "item constituent"))
  (:documentation "storage cell for implementation of tree:rb-tree based sequences"))


(defun make-seq-cell (&rest args)
  "convenience api routine for map-cell initialization"
  (apply #'make-instance 'seq-cell args))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; seq:type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun seq:typep (thing)
  "seq collection type-predicate"
  (or
    (null thing)
    (and (tree:typep thing) (cl:typep (tree:rb-tree-v thing) 'seq-cell))))

(deftype seq:type ()
  "collection of arbitrary, possibly non-unique, elements in some specific lexical order"
  `(satisfies seq:typep))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set:type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun set:typep (thing)
  "set collection type-predicate"
  (or
    (null thing)
    (tree:typep thing)))

(deftype set:type ()
  "collection of arbitrary, unique, elements in order determined by defined ordinal
   comparison relations on element types and content values"
  `(satisfies set:typep))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; seq api
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun empty ()
  "create empty seq"
  (set:empty))


(defun is-empty (s)
  "return true if seq contains no elements, otherwise false"
  (map:is-empty s))
  

(defun length (s)
  "return the number of elements in seq, similar to (cl:length list)"
  (set:cardinal s))


(defun first (s)
  "return the first element in seq, similar to (cl:first list)"
  (if (null s)
    nil
    (seq-cell-val (set:min s))))


(defun last (s)
  "return the last element in seq, similar to (cl:car (cl:last list))"
  (if (null s)
    nil
    (seq-cell-val (set:max s))))


(defun min-key (s)
  "return the key of the first element of seq"
  (if (null s)
    0
    (seq-cell-key (set:min s))))


(defun max-key (s)
  "return the key of the last element of seq"
  (if (null s)
    0
    (seq-cell-key (set:max s))))


(defun rest (s)
  "return a new seq containing all but the first element of s"
  (if (null s)
    nil
    (set:remove-min s)))


(defun butlast (s)
  "return a new seq containing all but the last element of s"
  (if (null s)
    nil
    (set:remove-max s)))


(defun push (elem &optional seq)
  "return a new seq identical to seq but with elem prepended"
  (set:add
    (make-seq-cell :key (- (min-key seq) 1) :val elem) seq))


(defun add (elem &optional seq)
  "return a new seq identical to seq but with elem appended"
  (set:add
    (make-seq-cell :key (+ (max-key seq) 1) :val elem) seq))


(defun list (s)
  "return the list-based equivalent to seq s, containing all elements of s in the same order"
  (mapcar #'seq-cell-val (set:elements s)))


(defun dup (s)
  "return a new seq that is seq:equal to the original seq s"
  (cond
    ((null s)  nil)
    (t         (let* ((min-cell (set:min s))
                       (new-cell (make-seq-cell
                                   :key (seq-cell-key min-cell)
                                   :val (seq-cell-val min-cell))))
                 (set:add new-cell (dup (set:remove-min s)))))))


(defun mapi (f seq)
  (cond
    ((null seq) nil)
    (t (let (new-seq)
          (dolist (elem (set:elements seq))
            (setf new-seq (set:add  (make-seq-cell
                                      :key (funcall f (seq-cell-key elem))
                                      :val (seq-cell-val elem)) new-seq)))
         new-seq))))


(defun concat (s0 &rest args)
  (let ((max-key-s0 (max-key s0))
         (min-key-s1 (if args (min-key (car args)) 0)))        
    (cond
      ((null args) s0)
      (t (apply #'seq:concat
           (set:union s0
             (seq::mapi #'(lambda (k) (+ 1
                                       (if (minusp max-key-s0) 1 max-key-s0)
                                       (if (minusp min-key-s1) (- min-key-s1) 1)
                                       k))
               (car args))) (cl:rest args))))))


(defun map (f seq)
   (cond ((null seq) nil)
     (t (let (new-seq)
          (dolist (elem (set:elements seq))
            (setf new-seq (set:add (make-seq-cell
                                      :key (seq-cell-key elem)
                                     :val (funcall f (seq-cell-val elem)))
                            new-seq)))
          new-seq))))


(defun make (&optional (from (seq:empty)))
  (etypecase from
    (null     (seq:empty))
    (cl:list  (let (seq)
                (dolist (elem from)
                  (setq seq (seq:add elem seq)))
                seq))
    (seq:type (seq:dup from))
    (map:type (error "seqs cannot be created from ordinary maps"))
    (set:type (seq:make (set:elements from)))
    (sequence (seq:make (coerce from 'cl:list)))
    (atom     (seq:add from))))


(defun elt (index seq)
  (cl:elt (seq:list seq) index))


(defun reduce (fn seq &key key from-end (start 0) end initial-value)
  (cl:reduce fn (seq:list seq)
    :key key
    :from-end from-end
    :start start
    :end end
    :initial-value initial-value))



(defun compare (seq1 seq2 &optional (cmp #'ord:compare))
  "ordinal comparison of two seqs"
  (let ((e1 (tree::cons-enum seq1 nil))
         (e2 (tree::cons-enum seq2 nil)))
    (tagbody again
      (return-from compare
        (cond
          ((and (null e1) (null e2))   0)
          ((null e1)                  -1)
          ((null e2)                   1)
          (t (destructuring-bind (v1 r1 ee1) e1
               (destructuring-bind (v2 r2 ee2) e2
                 (let ((c (funcall cmp (seq-cell-val v1) (seq-cell-val v2))))
                   (cond
                     ((not (zerop c)) c)
                     (t               (progn
                                        (setf e1 (tree::cons-enum r1 ee1)
                                              e2 (tree::cons-enum r2 ee2))
                                        (go again)) ))) ))))) )))


(defun equal (seq1 seq2 &optional (cmp #'ord:compare))
  "compare two seqs for element-wise equality using cmp"
  (zerop (compare seq1 seq2 cmp)))
