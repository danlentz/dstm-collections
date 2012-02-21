;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :dclx)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *default-syntax*                 'dclx:standard-syntax))


(defvar *default-syntax-startup-enabled* t)                                                         
(defvar *print-collections-readably*     t)
(defvar *set-reader-macro-char*          #\{)
(defvar *seq-reader-macro-char*          #\[)
(defvar *value-reader-macro-char*        #\$)
(defvar *prior-readtable*                nil)
(defvar *parallel-execution-enabled*     t)
(defvar *default-kernel-parallelism*     16)
(defvar *default-kernel-startup-enabled* t)
(defvar *kernel*                         nil)


(defun default-syntax ()
  (find-readtable *default-syntax*))


(defun enable-syntax ()
  (unless (eq *readtable* (default-syntax))
    (setf *prior-readtable* *readtable*)
    (funcall #'in-readtable (default-syntax))))


(defun disable-syntax ()
  (in-readtable *prior-readtable*))

#+()
(defun kernel ()
  *kernel*)


(defun ensure-kernel ()
  (unless *kernel*
    (setf *kernel*
      (lparallel:make-kernel *default-kernel-parallelism*
        :bindings '((dstm:*transaction* . nil))
        :name "dclx-worker")))
  (when (null lparallel:*kernel*)
    (setf lparallel:*kernel* *kernel*))
  (unless (eq lparallel:*kernel* *kernel*)
    (warn "lparallel:*kernel* already running. dclx:*kernel* started but is not global default"))
  :kernel-started)
    

(defclass   var ()())

(defgeneric value (thing)
  (:documentation "Atomically read the value of a transactional variable"))

(defgeneric (setf value) (new-value var))

(defmethod  value ((thing t))
  thing)

(defmethod (setf value) (new-value (var t))
  (error "Attempt to write the VALUE of the NON-TRANSACTIONAL place ~S" var))

