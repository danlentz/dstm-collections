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
  (or
    (null thing)
    (and (tree:typep thing) (cl:typep (tree:rb-tree-v thing) 'seq-cell))))


(deftype seq:type ()
  `(satisfies seq:typep))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set:type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun set:typep (thing)
  (or
    (null thing)
    (tree:typep thing)))


(deftype set:type ()
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
  (set:cardinal s))


(defun first (s)
  (if (null s)
    nil
    (seq-cell-val (set:min-elt s))))


(defun last (s)
  (if (null s)
    nil
    (seq-cell-val (set:max-elt s))))


(defun min-key (s)
  (if (null s)
    0
    (seq-cell-key (set:min-elt s))))


(defun max-key (s)
  (if (null s)
    0
    (seq-cell-key (set:max-elt s))))


(defun rest (s)
  (if (null s)
    nil
    (set:remove-min-elt s)))


(defun butlast (s)
  (if (null s)
    nil
    (set:remove-max-elt s)))


(defun push (elem &optional seq)
  (set:add
    (make-seq-cell :key (- (min-key seq) 1) :val elem) seq))


(defun add (elem &optional seq)
  (set:add
    (make-seq-cell :key (+ (max-key seq) 1) :val elem) seq))


(defun list (s)
  (mapcar #'seq-cell-val (set:elements s)))


(defun dup (s)
  (cond
    ((null s)  nil)
    (t         (let* ((min-cell (set:min-elt s))
                       (new-cell (make-seq-cell
                                   :key (seq-cell-key min-cell)
                                   :val (seq-cell-val min-cell))))
                 (set:add new-cell (dup (set:remove-min-elt s)))))))


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


(defun create (&optional (from (seq:empty)))
  (etypecase from
    (null     (seq:empty))
    (cl:list  (let (seq)
                (dolist (elem from)
                  (setq seq (seq:add elem seq)))
                seq))
    (seq:type (seq:dup from))
    (map:type (error "seqs cannot be created from ordinary maps"))
    (set:type (seq:create (set:elements from)))
    (sequence (seq:create (coerce from 'cl:list)))
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
