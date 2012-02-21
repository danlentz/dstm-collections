;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-user)


(defpackage :dstm-collections
  (:nicknames :dclx)
  (:use :common-lisp :named-readtables)
  (:export
    :?
    :?+
    :printv
    :set
    :set*
    :map
    :map*
    :seq
    :seq*
    :var
    :value
    :with-gensyms
    :make-gensym-list

    :once-only
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
    
(defpackage :quad
  (:use :common-lisp :lparallel :named-readtables)
  (:import-from :dclx :? :?+ :printv)
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


(defpackage :cstm
  (:shadowing-import-from :closer-mop :standard-generic-function :defgeneric :defmethod)
  (:use :common-lisp :contextl :closer-mop :lparallel :named-readtables)
  (:import-from :dclx :? :?+ :printv :var :value)
  (:export
    :*tries*
    :*timeout*
    :*current-transaction*
    :deferred-update-mode
    :direct-update-mode
    :transaction
    :define-transactional-class
    :transactional-class
    :transactional-variable
    :value
    :var
    :create-var
    :call-atomic
    :atomic
    :roll-back
    :commit-transaction
    :retry-transaction
    :most-recent-transaction
    :transaction-status))


(defpackage :dstm
  (:use :common-lisp :lparallel :named-readtables)
  (:import-from :dclx :? :?+ :printv :var :value)
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
  (:import-from :dclx :? :?+ :printv)
  (:export
    :compare|>|
    :compare|<|
    :compare|<=|
    :compare|=|
    :compare|>=|
    :compare
    :make-ci-char
    :make-ci-string
    :slots-to-compare
    :writing-readably
    :of-type
    :proper-list
    :proper-list-p
    :association-list
    :association-list-p
    ))


(defpackage :tree
  (:use :common-lisp :quad :lparallel :named-readtables)
  (:import-from :dclx :? :?+ :printv :value :var)
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
    :weight
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
  (:import-from :dclx :? :?+ :printv :value :var :with-gensyms :make-gensym-list :once-only)
  (:import-from :tree :create :bal :join :concat :cons-enum 
    :lr :lvr :lvrh :make-rb-tree :rb-tree-p :rb-tree-l :rb-tree-v :rb-tree-r :rb-tree-h
    :height :add :remove-min :remove-max :make-cursor :with-cursor)
  (:shadowing-import-from :tree :merge  :max :min)
;;  (:shadowing-import-from :dclx :coerce)
  (:shadow :equal :remove :union :typep :type :set :do)
  (:export
    :syntax
    :set           :set*
    :empty         :empty*
    :make          :make*    
    :add           :add*
    :singleton     :singleton*
    :remove        :remove*
    :remove-min    :remove-min*
    :remove-max    :remove-max*
    :union         :union*
    :inter         :inter*
    :filter        :filter*    
    :height
    :emptyp
    :make-cursor
    :with-cursor
    :dup
    :mem
    :min
    :max
    :diff
    :compare
    :equal
    :subset
    :iter
    :do
    :fold
    :for-all
    :exists
    :split
    :partition
    :cardinal
    :elements
    :typep
    :type
    ))


(defpackage :map
  (:use :common-lisp :lparallel :named-readtables)
  (:shadow :find :equal :map :remove :typep :type :values :do)
  (:import-from :set :dup)
  (:import-from :dclx :? :var :value)
;;  (:shadowing-import-from :dclx :coerce)
 (:import-from :tree :lr :lvr :lvrh :cons-enum :make-cursor :with-cursor)
  (:export
    :map          :map*
    :empty        :empty*
    :make         :make*
    :add          :add*
    :ensure-find  :ensure-find*      
    :remove       :remove*
    :emptyp
    :find
    :mem
    :iter
    :keys
    :values
    :map
    :mapi
    :keymap
    :do
    :fold
    :compare
    :equal
    :typep
    :type
    :dup
    :make-cursor
    :with-cursor
    :syntax
;;    :coerce
    ))


(defpackage :seq
  (:shadow  :push  :pop    :first    :second    :third    :elt :butlast
    :last  :rest  :length    :map    :equal    :dup      :typep :list :vector
    :type :reduce :do :reverse :subseq)
  (:import-from :tree :make-cursor :with-cursor)
  (:import-from :dclx :? :?+ :printv :var :value :with-gensyms :once-only :make-gensym-list)
;;  (:shadowing-import-from :dclx :coerce)
  (:use :common-lisp :lparallel :named-readtables)
  (:export
    :syntax
    :seq
    :seq*
    :empty
    :empty*
    :emptyp
    :make
    :make*
    :make-cursor
    :with-cursor
    :add
    :push
    :first
    :last
    :rest
    :butlast
    :list
    :vector
    :length
    :subseq
    :dup
    :typep
    :type    
    :concat
    :do
    :map
    :elt
    :compare
    :equal
    :reduce
    :reverse
    :reverse*
    ))



  (shadowing-import '(set:set set:set*) :dclx)
  (shadowing-import '(map:map map:map*) :dclx)
  (shadowing-import '(seq:seq seq:seq*) :dclx)
  (export '(dclx::set dclx::set* dclx::map dclx::map* dclx::seq dclx::seq*) :dclx)
