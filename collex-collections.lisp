;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :dstm-collections)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collections Layer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deflayer collections ()
  ((registry :initform (make-hash-table :test #'cl:equal) :reader registry)
    (locator :initform (make-hash-table :test #'cl:equal) :reader locator)))

(ensure-active-layer 'collections)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-layered-class named-collection :in-layer collections ()
  ((name :initarg :name :reader name :reader name-of))
  (:default-initargs :name (unicly:uuid-princ-to-string (unicly:make-v4-uuid))))


(defmethod initialize-instance :after ((self named-collection) &key &allow-other-keys)
  (let ((layer (find-layer 'collections)))
    (setf (gethash self (registry layer)) (name self))
    (setf (gethash (name self) (locator layer)) self))
  (log:info "~%registered ~A as a ~S" (name self) (class-of self)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predefined Default Contextal Environments 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter weight-balanced
  (with-active-layers (tree:weight-balanced)
    (with-inactive-layers (tree:height-balanced)
      (capture-dynamic-environment))))


(defparameter height-balanced
  (with-active-layers (tree:height-balanced)
    (with-inactive-layers (tree:weight-balanced)
      (capture-dynamic-environment))))


(defparameter isolated-transaction
  (with-active-layers (cstm:stm-mode)
    (with-inactive-layers (cstm:direct-update-mode cstm:deferred-update-mode)
      (capture-dynamic-environment))))


(defparameter direct-update-tranaction
  (with-active-layers (cstm:direct-update-mode)
    (with-inactive-layers (cstm:deferred-update-mode)
      (capture-dynamic-environment))))


(defparameter deferred-update-tranaction
  (with-active-layers (cstm:deferred-update-mode)
    (with-inactive-layers (cstm:direct-update-mode)
      (capture-dynamic-environment))))


(defparameter isolated-tranaction/weight-balanced
  (with-dynamic-environment (weight-balanced)
    (with-dynamic-environment (isolated-transaction)
      (capture-dynamic-environment))))


(defparameter isolated-tranaction/height-balanced
  (with-dynamic-environment (height-balanced)
    (with-dynamic-environment (isolated-transaction)
      (capture-dynamic-environment))))


(defparameter direct-update-tranaction/height-balanced
  (with-dynamic-environment (height-balanced)
    (with-dynamic-environment (direct-update-tranaction)
      (capture-dynamic-environment))))


(defparameter direct-update-tranaction/weight-balanced
  (with-dynamic-environment (weight-balanced)
    (with-dynamic-environment (direct-update-tranaction)
      (capture-dynamic-environment))))


(defparameter deferred-update-tranaction/height-balanced
  (with-dynamic-environment (height-balanced)
    (with-dynamic-environment (deferred-update-tranaction)
      (capture-dynamic-environment))))


(defparameter deferred-update-tranaction/weight-balanced
  (with-dynamic-environment (weight-balanced)
    (with-dynamic-environment (deferred-update-tranaction)
      (capture-dynamic-environment))))


(defparameter istm/wb isolated-tranaction/weight-balanced)
(defparameter istm/rb isolated-tranaction/height-balanced)
(defparameter cstm/wb direct-update-tranaction/weight-balanced)
(defparameter cstm/rb direct-update-tranaction/height-balanced)
(defparameter dstm/wb deferred-update-tranaction/weight-balanced)
(defparameter dstm/rb deferred-update-tranaction/height-balanced)

(defparameter *default-context* weight-balanced)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set collection classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-layered-class abstract-collection ()
  ())

