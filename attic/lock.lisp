;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;

(defpackage :lock
  (:use :common-lisp)
  (:export
    :make-spinlock
    :lock-spinlock
    :unlock-spinlock
    :with-spinlock
    :make-recursive-spinlock
    :with-recursive-spinlock))

(in-package :lock)

#-sbcl (error "~A Not yet supported" (lisp-implementation-type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common idioms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop repeat (length names) collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names
                   collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names for g in gensyms
                     collect `(,n ,g)))
             ,@body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spinlock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-spinlock ()
  (cons nil nil))

(defun lock-spinlock (spinlock)
  (loop :while (sb-ext:compare-and-swap (car spinlock) nil t)))

(defun unlock-spinlock (spinlock)
  (setf (car spinlock) nil))

(defmacro with-spinlock ((spinlock) &body body)
  (once-only (spinlock)
    `(progn
       (lock-spinlock ,spinlock)
       (unwind-protect (progn ,@body)
         (unlock-spinlock ,spinlock)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recursive-spinlock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-recursive-spinlock ()
  (cons nil 0))

(defun lock-recursive-spinlock (recursive-spinlock)
  (loop
    :with    self = sb-thread:*current-thread*
    :for     ret  = (sb-ext:compare-and-swap (car recursive-spinlock) nil self)
    :until   (or (null ret) (eq ret self))
    :finally (incf (cdr recursive-spinlock))))

(defun unlock-recursive-spinlock (recursive-spinlock)
  (when (decf (cdr recursive-spinlock))
    (setf (car recursive-spinlock) nil)))

(defmacro with-recursive-spinlock ((recursive-spinlock) &body body)
  (once-only (recursive-spinlock)
    `(progn
       (lock-recursive-spinlock ,recursive-spinlock)
       (unwind-protect (progn ,@body)
         (unlock-recursive-spinlock ,recursive-spinlock)))))
