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

(defvar *default-kernel-parallelism*     10)
(defvar *default-kernel-startup-enabled* nil)
(defvar *kernel*                         nil)


(defun default-syntax ()
  (find-readtable *default-syntax*))


(defun enable-syntax (&optional (which-syntax (default-syntax)))
  (unless (eq *readtable* (find-readtable which-syntax))
    (setf *prior-readtable* *readtable*)
    (setf *readtable* (find-readtable which-syntax))))


(defun disable-syntax ()
  (in-readtable *prior-readtable*))


(defun kernel ()
  *kernel*)

#+()
(defun ensure-kernel ()
  (and 
    (if lparallel:*kernel*
      (error "multiple kernels not yet supported")
      (unless *kernel*
        (setf *kernel*
          (lparallel:make-kernel *default-kernel-parallelism*
            :bindings '((dstm:*transaction* . nil))
            :name "dclx-worker"))))
    (setf lparallel:*kernel* *kernel*)))
    
