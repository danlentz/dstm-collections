;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-user)


(defpackage :quad
  (:use :common-lisp :eager-future2 :named-readtables)
  (:export
    :tuple
    :abstract-quad
    :qar
    :qbr
    :qcr
    :qdr
    :a
    :b
    :c
    :d))


    
(defpackage :dstm
  (:use :common-lisp :eager-future2 :named-readtables)
  (:shadow :read :write)
  (:export
    :var
    :dstm-var
    :*transaction*
    :transaction
    :create-var
    :read
    :write
    :write-vars
    :atomic
    :orelse
    :rollback
    :rmw
    :check
    :reset))


(defpackage :ord
  (:use :common-lisp :eager-future2 :named-readtables)
  (:export
    :|COMPARE>|
    :compare<
    :compare<=
    :compare=
    :compare>=
    :compare
    :make-ci-char
    :make-ci-string
    :slots-to-compare
    :writing-readably
    :association-list
    ))


(defpackage :tree
  (:use :common-lisp :quad :eager-future2 :named-readtables)
  (:shadow :merge :typep :type  :min  :max)
  (:export
    :rb-tree
    :make-rb-tree
    :rb-tree-p
    :rb-tree-l
    :rb-tree-v
    :rb-tree-r
    :rb-tree-h
    :create
    :height
    :add
    :min
    :max
    :remove-min
    :remove-max    
    :bal
    :join
    :merge
    :concat
    :cons-enum
    :lr
    :lvr
    :lvrh
    :typep
    :type
    ))


(defpackage :set
  (:use :common-lisp :eager-future2 :named-readtables)
  (:import-from :tree :create :bal :join :concat :cons-enum ;;:not-found :invalid-argument
    :lr :lvr :lvrh :make-rb-tree :rb-tree-p :rb-tree-l :rb-tree-v :rb-tree-r :rb-tree-h
    :height :add :remove-min :remove-max)
  (:shadowing-import-from :tree :merge  :max :min)
  (:shadow :equal :remove :union :typep :type)
  (:export
    :syntax
    :set*
    :height
    :empty
    :is-empty
    :make
    :make*
    :dup
    :mem
    :add
    :singleton
    :remove
    :remove-min
    :remove-max
    :min
    :max
    :union
    :inter
    :diff
    :compare
    :equal
    :subset
    :iter
    :fold
    :for-all
    :exists
    :filter
    :split
    :partition
    :cardinal
    :elements
    :typep
    :type))


(defpackage :map
  (:use :common-lisp :eager-future2 :named-readtables)
  (:shadow :find :equal :map :remove :typep :type)
  (:import-from :tree :lr :lvr :lvrh  :cons-enum)
  (:export
    :syntax
    :map*
    :empty
    :is-empty
    :make
    :make*
    :add
    :find
    :remove
    :mem
    :iter
    :map
    :mapi
    :fold
    :compare
    :equal
    :typep
    :type
    ))


(defpackage :seq
  (:shadow  :push  :pop    :first    :second    :third    :elt :butlast
    :last  :rest  :length    :map    :equal    :dup      :typep :list
    :type :reduce)
  (:use :common-lisp :eager-future2 :named-readtables)
  (:export
    :syntax
    :seq*
    :empty
    :is-empty
    :make
    :make*
    :cursor
    :cursor-p
    :cursor-for
    :cursor-back
    :cursor-forward
    :cursor-up
    :cursor-at
    :push
    :add
    :first
    :last
    :rest
    :list
    :length
    :dup
    :typep
    :type    
    :concat
    :map
    :elt
    :compare
    :equal
    :reduce
    ))
