;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :mmap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Header/Footer Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcstruct* unique-id
  (bytes :uint8 :count 16))

(defcstruct* standard-indexed-data-file-header
  (cookie              :uint)
  (unique-id           :uint8 :count 16)
  (version-major       :uint)
  (version-minor       :uint)
  (free-space-start    :uint)
  (footer-offset       :uint))

(defcenum action
  :none
  :create
  :merge
  :gc
  :upgrade
  :clear)

(defcstruct* standard-indexed-data-file-footer
  (cookie                 :uint)
  (serial                 :uint)
  (action                 action)
  (previous-footer-offset :uint)
  (root-offset            :uint))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typed-Pointer Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defctype box :uint8)

(cffi:defcenum pointer-type
  :null
  :free
  :pointer
  :typed-pointer
  :uint
  :uint8
  :uint16
  :uint32
  :uint64
  :int
  :int8
  :int16
  :int32
  :int64
  :pcons
  :char
  :string
  :symbol
  :box)
    
(defcstruct* typed-pointer
  (element-type   pointer-type)
  (element-count  :uint)
  (element-offset :uint))
