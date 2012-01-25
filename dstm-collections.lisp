;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :dclx)


(defvar *default-syntax*                 'dclx:standard-syntax)
(defvar *default-syntax-startup-enabled* t)                                                         
(defvar *print-collections-readably*     t)
(defvar *set-reader-macro-char*          #\{)
(defvar *seq-reader-macro-char*          #\[)
(defvar *value-reader-macro-char*        #\$)
(defvar *prior-readtable*                nil)
(defvar *parallel-execution-enabled*     t)
(defvar *default-kernel-parallelism*     10)
(defvar *default-kernel-startup-enabled* t)
(defvar *kernel*                         nil)


(defun default-syntax ()
  (find-readtable *default-syntax*))


(defun enable-syntax (&optional (which-syntax (default-syntax)))
  (unless (eq *readtable* (find-readtable which-syntax))
    (setf *prior-readtable* *readtable*)
    (in-readtable which-syntax)))


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
    
