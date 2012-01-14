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


(defun empty ()
  "create empty seq"
  (set:empty))



