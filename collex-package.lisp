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

