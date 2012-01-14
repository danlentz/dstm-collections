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


(defun push (elem seq)
  (set:add
    (make-seq-cell :key (- (min-key seq) 1) :val elem) seq))


(defun pushend (elem seq)
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


