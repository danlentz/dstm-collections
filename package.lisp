;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-user)


(defpackage :quad
  (:use #:common-lisp)
  (:export
    #:tuple
    #:abstract-quad
    #:qar
    #:qbr
    #:qcr
    #:qdr
    #:a
    #:b
    #:c
    #:d))
   
(defpackage :ord
  (:use #:common-lisp)
  (:export
    #:|COMPARE|
    #:|COMPARE<|
    #:|COMPARE<=|
    #:|COMPARE=|
    #:|COMPARE>=|
    #:|COMPARE>|
    #:|MAKE-CI-CHAR|
    #:|MAKE-CI-STRING|))


(defpackage :tree
  (:use #:common-lisp #:quad)
  (:shadow #:merge)
  (:export
    #:rb-tree
    #:make-rb-tree
    #:rb-tree-p
    #:rb-tree-l
    #:rb-tree-v
    #:rb-tree-r
    #:rb-tree-h
    #:create
    #:height
    #:add
    #:min-elt
    #:remove-min-elt
    #:bal
    #:join
    #:merge
    #:concat
    #:cons-enum
    #:not-found
    #:invalid-argument
    #:lr
    #:lvr
    #:lvrh))


(defpackage :set
  (:use #:common-lisp)
  (:import-from #:tree #:create #:bal #:join #:concat #:cons-enum #:not-found #:invalid-argument
    #:lr #:lvr #:lvrh #:make-rb-tree #:rb-tree-p #:rb-tree-l #:rb-tree-v #:rb-tree-r #:rb-tree-h
    #:height #:add #:min-elt #:remove-min-elt)
  (:shadowing-import-from #:tree #:merge)
  (:shadow #:equal #:remove #:union)
  (:export
    #:height
    #:empty
    #:is-empty
    #:mem
    #:add
    #:singleton
    #:remove
    #:remove-min-elt
    #:union
    #:inter
    #:diff
    #:compare
    #:equal
    #:subset
    #:iter
    #:fold
    #:for-all
    #:exists
    #:filter
    #:split
    #:partition
    #:cardinal
    #:elements
    #:min-elt
    #:max-elt
    #:choose))


(defpackage :map
  (:use #:common-lisp)
  (:shadow #:find #:equal #:map #:remove)
  (:import-from #:tree #:lr #:lvr #:lvrh)
  (:export
    #:empty
    #:is-empty
    #:add
    #:find
    #:remove
    #:mem
    #:iter
    #:map
    #:mapi
    #:fold
    #:compare
    #:equal
    ))


(defpackage :dstm
  (:use #:common-lisp)
  (:export
    #:var
    #:transaction
    #:create-var
    #:read-var
    #:write-var
    #:write-vars
    #:atomic
    #:orelse
    #:rollback
    #:rmw
    #:check
    #:reset))

