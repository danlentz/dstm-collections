;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-user)


    
(defpackage :quad
  (:use :common-lisp :lparallel :named-readtables)
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
  (:use :common-lisp :lparallel :named-readtables)
  (:shadow :read :write)
  (:export
    :var
    :dstm-var
    :*transaction*
    :transaction
    :create-var
    :read
    :write    
    :atomic
    :orelse
    :rollback
    :with-update
    :updatef
    :safe-value
    :value
    :check
    :reset))


(defpackage :ord
  (:use :common-lisp :lparallel :named-readtables)
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
    :proper-list
    :proper-list-p
    :association-list
    :association-list-p
    ))


(defpackage :tree
  (:use :common-lisp :quad :lparallel :named-readtables)
  (:shadow :merge :typep :type  :min  :max)
  (:export
    :rb-tree
    :make-rb-tree
    :make-cursor
    :with-cursor
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
  (:use :common-lisp :lparallel :named-readtables)
  (:import-from :tree :create :bal :join :concat :cons-enum ;;:not-found :invalid-argument
    :lr :lvr :lvrh :make-rb-tree :rb-tree-p :rb-tree-l :rb-tree-v :rb-tree-r :rb-tree-h
    :height :add :remove-min :remove-max :make-cursor: :with-cursor)
  (:shadowing-import-from :tree :merge  :max :min)
  (:shadow :equal :remove :union :typep :type :set)
  (:export
    :syntax
    :set
    :set*
    :height
    :empty
    :is-empty
    :make
    :make*
    :make-cursor
    :with-cursor
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
  (:use :common-lisp :lparallel :named-readtables)
  (:shadow :find :equal :map :remove :typep :type)
  (:import-from :set :dup)                                                    
  (:import-from :tree :lr :lvr :lvrh :cons-enum :make-cursor :with-cursor)
  (:export
    :syntax
    :map
    :map*
    :empty
    :empty*
    :is-empty
    :make
    :make*
    :make-cursor
    :with-cursor
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
    :dup
    ))


(defpackage :seq
  (:shadow  :push  :pop    :first    :second    :third    :elt :butlast
    :last  :rest  :length    :map    :equal    :dup      :typep :list
    :type :reduce)
  (:import-from :tree :make-cursor :with-cursor)
  (:use :common-lisp :lparallel :named-readtables)
  (:export
    :syntax
    :seq
    :seq*
    :empty
    :is-empty
    :make
    :make*
    :make-cursor
    :with-cursor
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


(defpackage :dstm-collections
  (:nicknames :dclx)
  (:use :common-lisp :named-readtables)
  (:shadowing-import-from :set :set :set*)
  (:shadowing-import-from :map :map :map*)
  (:shadowing-import-from :seq :seq :seq*)
  (:export
    :set
    :set*
    :map
    :map*
    :seq
    :seq*
    :ensure-kernel
    :standard-syntax
    :*default-syntax*
    :enable-syntax
    :disable-syntax
    :*set-reader-macro-char*
    :*seq-reader-macro-char*
    :*value-reader-macro-char*
    :*parallel-execution-enabled*
    :*default-syntax-startup-enabled*
    :*print-collections-readably*
   ))

