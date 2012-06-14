;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-user)

(defpackage :persistent-transactional-collections
  (:nicknames :collex)
  (:documentation "")
  (:use :closer-common-lisp :contextl :named-readtables :cl-syntax :lisp-unit)
  (:export
    :standard-syntax
    :*default-syntax*
    :enable-syntax
    :disable-syntax
    :*set-reader-macro-char*
    :*seq-reader-macro-char*
    :*default-syntax-startup-enabled*
    :*print-collections-readably*
    :with-gensyms
    :make-gensym-list
    :make-keyword
    :symbolicate
    :keywordicate
    :once-only
    :nlet
    :nlet-tail
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
    :get-place
    :proto
    :drop
    :using
    :defmacro!
    :object->sexp))


#+()
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
#|

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
|#
