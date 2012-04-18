;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-user)

(defpackage :dstm-collections (:nicknames :cxn :dclx :collex)
  (:documentation "")
  (:use :common-lisp :contextl :named-readtables)
  (:export
    :standard-syntax
    :*default-syntax*
    :enable-syntax
    :disable-syntax
    :*set-reader-macro-char*
    :*seq-reader-macro-char*
    :*default-syntax-startup-enabled*
    :*print-collections-readably*
    :?
    :?+
    :printv
    :with-gensyms
    :make-gensym-list
    :make-keyword
    :symbolicate
    :keywordicate
    :once-only
    :nlet
    :symbolic
    :scond
    :scase
    :swhen
    :anaphoric
    :acase
    :atypecase
    :awhen
    :aprog1
    :conc1f
    :reuse-cons
    :ensure-function
    :compose
    :multiple-value-compose
    :ensure-list
    :ppmx
#+()    :ensure-kernel
#+()    :*value-reader-macro-char*
#+()    :*parallel-execution-enabled*
    :timing-info
    :real-time
    :user-time
    :system-time
    :gc-time
    :page-faults
    :bytes-consed
    :collect-timing
    :pprint-bytes
    :pprint-milliseconds
    :with-profiling-enabled
    :execution-time
    :pprint-to-string
    :report-and-ignore-errors
    :class-proto
    :drop
    :using))


(defpackage :tree
  (:documentation "")
  (:shadowing-import-from :closer-mop :standard-generic-function :defgeneric :defmethod)
  (:use :common-lisp :collex :contextl :closer-mop)
  (:export
    :balanced
    :height-balanced
    :weight-balanced
    :merge-direction
    :leaf
    :empty
    :empty?
    :node
    :kv
    :lr
    :kvlr
    :kvlrx
    :kvlrs
    :kvlrh
    :node/k
    :node/v
    :node/l
    :node/r
    :node/x
    :node/s
    :node/h
    :node/kv
    :node/lr
    :node/kvlr
    :node/kvlrx
    :node/kvlrs
    :node/kvlrh
    :node/constituents
    :node/values
    :node/call
    :node/size
    :node/weight
    :node/height
    :node/create
    :node/join
    :node/singleton
    :node/least
    :node/greatest
    :node/remove-least
    :node/remove-greatest
    :node/concat2
    :node/inorder-fold
    :node/iter
    :node/at-index
    :node/find
    :node/add
    :node/remove
    :node/concat3
    :node/concat
    :node/split-lesser
    :node/split-greater
    :node/split
    :node/union
    :node/union-merge
    :node/intersection
    :node/difference
    :node/subset?
    :node/from
    :node/member?
    :node/cons-enum
    :node/contains?
    :node/rank
    :node/for-all
    :+delta+
    :+gamma+
    :node/empty?
    :layer
    :allocation))


(defpackage :set
  (:shadowing-import-from :closer-mop :standard-generic-function :defgeneric :defmethod)
  (:use :common-lisp :closer-mop :contextl :collex :tree :named-readtables)
  (:shadow :equal :remove :union :typep :type :set :do)
  (:export
    :set
    :set*
    :typep
    :type
    :add
    :least
    :greatest
    :remove-least
    :remove-greatest
    :isolated-transaction
    :direct-update-tranaction
    :deferred-update-tranaction
    :direct-update-tranaction/height-balanced 
    :direct-update-tranaction/weight-balanced
    :deferred-update-tranaction/weight-balanced    
    :deferred-update-tranaction/height-balanced
    :isolated-tranaction/height-balanced
    :isolated-tranaction/weight-balanced
    :istm/rb
    :istm/wb
    :cstm/rb    
    :cstm/wb
    :dstm/wb
    :dstm/rb))   


(defpackage :map
  (:shadowing-import-from :closer-mop :standard-generic-function :defgeneric :defmethod)
  (:use :common-lisp :closer-mop :contextl :collections :tree :named-readtables)
  (:export
    :map          :map*))

;;   (:use :common-lisp :lparallel :named-readtables)
;;   (:shadow :find :equal :map :remove :typep :type :values :do)
;;   (:import-from :set :dup)
;;   (:import-from :dclx :? :var :value)
;; ;;  (:shadowing-import-from :dclx :coerce)
;;  (:import-from :tree :lr :lvr :lvrh :cons-enum :make-cursor :with-cursor)
;;   (:export
;;     :map          :map*
;;     :empty        :empty*
;;     :make         :make*
;;     :add          :add*
;;     :ensure-find  :ensure-find*      
;;     :remove       :remove*
;;     :emptyp
;;     :find
;;     :mem
;;     :iter
;;     :keys
;;     :values
;;     :map
;;     :mapi
;;     :keymap
;;     :do
;;     :fold
;;     :compare
;;     :equal
;;     :typep
;;     :type
;;     :dup
;;     :make-cursor
;;     :with-cursor
;;     :syntax
;; ;;    :coerce
;;     ))


(defpackage :seq
  (:shadowing-import-from :closer-mop :standard-generic-function :defgeneric :defmethod)
  (:use :common-lisp :closer-mop :contextl :collections :tree :named-readtables)
  (:shadow  :push  :pop    :first    :second    :third    :elt :butlast
    :last  :rest  :length    :map    :equal    :dup      :typep :list :vector
    :type :reduce :do :reverse :subseq)
  (:export
    :seq     :seq*))

  
  ;; (:import-from :tree :make-cursor :with-cursor)
  ;; (:import-from :dclx :? :?+ :printv :var :value :with-gensyms :once-only :make-gensym-list)
  ;; (:use :common-lisp :lparallel :named-readtables)
  ;; (:export
  ;;   :reverse :reverse*
  ;;   :seq     :seq*
  ;;   :empty   :empty*
  ;;   :make    :make*
  ;;   :add     :add*
  ;;   :push    :push*
  ;;   :emptyp
  ;;   :first
  ;;   :last
  ;;   :rest
  ;;   :butlast
  ;;   :butfirst
  ;;   :list
  ;;   :vector
  ;;   :length
  ;;   :subseq
  ;;   :dup
  ;;   :typep
  ;;   :type    
  ;;   :concat
  ;;   :do
  ;;   :map
  ;;   :elt
  ;;   :compare
  ;;   :equal
  ;;   :reduce
  ;;   :syntax
  ;;   :make-cursor
  ;;   :with-cursor
  ;;   ))


;; (shadowing-import '(;set:set
;;                      set:set*
;;                      ;map:map
;;                      map:map*
;;                      ;seq:seq 
;;                      seq:seq*) :dclx)
;; (export '(;dclx::set
;;            dclx::set*
;;            ;dclx::map
;;            dclx::map*
;;            ;dclx::seq
;;            dclx::seq*) :dclx)
