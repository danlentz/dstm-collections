;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :collex)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *default-syntax*                 'standard-syntax))

(defvar *prior-readtable*                nil)
(defvar *default-syntax-startup-enabled* t)                                                         
(defvar *print-collections-readably*     t)
(defvar *set-reader-macro-char*          #\{)
(defvar *seq-reader-macro-char*          #\[)
#+() (defvar *value-reader-macro-char*        #\$)


#+() (defvar *parallel-execution-enabled*     t)
#+() (defvar *default-kernel-parallelism*     16)
#+() (defvar *default-kernel-startup-enabled* t)
#+() (defvar *kernel*                         nil)


(defun default-syntax ()
  (find-readtable *default-syntax*))


(defun enable-syntax ()
  (unless (eq *readtable* (default-syntax))
    (setf *prior-readtable* *readtable*)
    (setf *readtable* (default-syntax))))


(defun disable-syntax ()
  (unless (null *prior-readtable*)
    (setf *readtable* *prior-readtable*)
    (setf *prior-readtable* nil)))

#+()
(defun kernel ()
  *kernel*)

#+()
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
    
