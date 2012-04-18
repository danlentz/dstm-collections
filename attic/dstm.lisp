;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;
;;;;; Lock Free DSTM -- Software Transactional Memory after Herlihy, et. al.
;;;;;
;;;;; based on:
;;;;;  http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.59.8787
;;;;;
;;;;; original implementation by: Dr. David McClain
;;;;; updates, tests, and sbcl compatibility by: Dan Lentz
;;;;;
;;;;; portions Copyright (C) 2008-2010 by SpectroDynamics, LLC
;;;;;

(defpackage :dstm
  (:documentation "")
  (:use :common-lisp  :named-readtables)
  (:import-from :dclx :? :?+ :printv :var :value)
  (:shadow :read :write)
  (:export
    :var
    :dstm-var
    :*transaction*
    :transaction
    :create-var
    :read
    :write    
    :atomic
    :orelse
    :rollback
    :with-update
    :updatef
    :safe-value
    :value
    :check
    :reset))

(in-package :dstm)

#-(or sbcl lispworks)
(error "unsupported lisp implementation.")


(defvar *transaction* nil)


(defclass transaction ()
  ((state   :accessor transaction-state  :initform :active :initarg :state)
    (root   :reader   transaction-root   :initform nil)
    (reads  :accessor transaction-reads  :initform nil)
    (subs   :accessor transaction-subs   :initform nil)
    (parent :accessor transaction-parent :initform (current-transaction))))


(defun current-transaction ()
  (unless (bt:current-thread) 
    (warn "Starting Multiprocessing")
    (bt:start-multiprocessing))
  #+lispworks (mp:process-private-property 'dstm:*transaction*)
  #+sbcl (sb-thread:symbol-value-in-thread 'dstm:*transaction* sb-thread:*current-thread* nil))


(defun (setf current-transaction) (trans)
  (unless (bt:current-thread) 
    (warn "Starting Multiprocessing")
    (bt:start-multiprocessing))
  (setf
    #+lispworks (mp:process-private-property 'dstm:*transaction*) 
    #+sbcl (sb-thread:symbol-value-in-thread 'dstm:*transaction* sb-thread:*current-thread* nil)
    trans))


(defmethod initialize-instance :after ((trans transaction) &key &allow-other-keys)
  (setf (slot-value trans 'root)
    (let ((curtrans (current-transaction)))
      (if curtrans
        (transaction-root curtrans)
        trans))))


(defun equivalentp (trans1 trans2)
   (eq (transaction-root trans1) (transaction-root trans2)))


(when (not (find-class 'var nil))
  (defclass var ()
    ()
    (:documentation "a consistent top-level export representing transactional variables")))


(defclass dstm-var (var)
  ((new
     :accessor dstm-var-new
     :initform nil
     :initarg :new)
    (old
      :accessor dstm-var-old
      :initform nil)
    (trans
      :accessor dstm-var-trans
      :initform (load-time-value (make-instance 'transaction :state :committed)))))



(defun create-var (&optional val (class-name 'dstm-var))
   (make-instance class-name :new val))



(define-condition rollback-exn ()
   ())


(defun reclaim-lists (trans)
  (setf (transaction-reads  trans) nil
    (transaction-subs   trans) nil))


(defparameter *nrolls*
  #+sbcl      (make-array '(1) :element-type 'sb-ext:word :initial-element #x0)
  #+lispworks (make-array '(1) :initial-element 0))


(defparameter *ntrans*
  #+sbcl      (make-array '(1) :element-type 'sb-ext:word :initial-element #x0)
  #+lispworks (make-array '(1) :initial-element 0))


(defun rollback-trans-and-subs (trans)
   (dolist (sub (transaction-subs trans))
     (rollback-trans-and-subs sub))
   (setf (transaction-state trans) :ABORTED)
   (reclaim-lists trans))


(defun rollback ()
  (let ((trans (current-transaction)))
    ;; rollbacks outside of transactions are permitted but meaningless
    (when trans
      #+sbcl      (sb-ext:atomic-incf (aref *nrolls* 0))
      #+lispworks (sys:atomic-incf (aref *nrolls* 0))
      (rollback-trans-and-subs trans)
      (error (load-time-value (make-condition 'rollback-exn) t)))))


(defun check-reads (trans)
  #+lispworks  (sys:ensure-memory-after-store)
  (dolist (pair (transaction-reads trans))
    (destructuring-bind (var . vtrans) pair
      (let ((vnow (dstm-var-trans var)))
        (unless (or (eq vnow vtrans)          ;; unchanged?
                  (equivalentp trans vnow))   ;; ...or changed by us...
          (rollback))))))


(defun commit (final)
   (commit-with-transaction (current-transaction) final))


(defun commit-with-transaction (trans final)
   (dolist (sub (transaction-subs trans))
     (commit-with-transaction sub final))
   (check-reads trans)
   (let ((parent (shiftf (transaction-parent trans) nil)))
     (when parent
       ;; trans succeeded for now, add to parents subs-list, but leave
       ;; in :ACTIVE state -- happens only on the first commit attempt
       (push trans (transaction-subs parent))))
   (when final
     (setf (transaction-state trans) :COMMITTED)
     (reclaim-lists trans)))


;; (defun read-var (var)
;;   (warn "read-var deprecated")
;;   (dstm:read var))


(defun read (var)
  (let ((trans (current-transaction)))
    (loop
      #+lispworks  (sys:ensure-memory-after-store)
      (let* ((vtrans (dstm-var-trans var))
              (vstate (transaction-state vtrans)))
        (when (or (not (eq :ACTIVE vstate))
                (and trans (equivalentp trans vtrans)))
          (when trans
            (push (cons var vtrans) (transaction-reads trans)))
          (return (if (eq :ABORTED vstate)
                    (dstm-var-old var)
                    (dstm-var-new var))))))))


;; (defun write-var (var val)
;;   (warn "write-var deprecated")
;;   (dstm:write var val))


(defun write (var val)
   (let* ((trans   (current-transaction))
          (wtrans  (or trans (make-instance 'transaction))))
     (prog1
         (write-var-with-transaction wtrans var val)
       (unless trans
         (setf (transaction-state wtrans) :COMMITTED)))))


(defun write-vars (&rest pairs)
   (do ((pairs pairs (cddr pairs)))
       ((null pairs))
     (dstm:write (car pairs) (cdar pairs))))


(defun write-var-with-transaction (trans var val)
  "trans is the current transaction for a thread"
  (loop
    #+lispworks  (sys:ensure-memory-after-store)
    (let* ((vtrans (dstm-var-trans var))
            (vstate (transaction-state vtrans)))
      (cond
        ((not (eq :ACTIVE vstate))
          (when (#+sbcl       sb-ext:compare-and-swap
                  #+lispworks sys:compare-and-swap
                  (slot-value var 'trans) vtrans trans)
            (when (eq :COMMITTED vstate)
              (setf (dstm-var-old var) (dstm-var-new var)))
            (return (setf (dstm-var-new var) val)) ))
        ((equivalentp trans vtrans)
          (return (setf (dstm-var-new var) val)))))))


(defun do-orelse (&rest fns)
  "Perform one of the functions in the list fns.  The list is
   examined in order, front to back.  The first one to succeed is
   the sub-transaction accepted.  If none succeed, or the overall
   transaction fails, the the whole thing is restarted."
  (let ((ct-save (current-transaction)))
    (loop
      (dolist (fn fns)
        (setf (current-transaction)
          (make-instance 'transaction))
        (#+sbcl sb-ext:atomic-incf
          #+lispworks sys:atomic-incf
          (aref *ntrans* 0))
        (unwind-protect
          (handler-case
            (return-from do-orelse
              (prog1
                (funcall fn)
                (commit (null ct-save))))
            (rollback-exn (exn)
              (declare (ignore exn)))
            (error (exn)
              (rollback-trans-and-subs (current-transaction))
              (error exn)))
          ;; unwind
          (setf (current-transaction) ct-save)))
      ;; end of list
      (when ct-save
        (rollback)))))


(defmacro orelse (&rest clauses)
   `(apply #'do-orelse
     (list ,@(mapcar (lambda (clause)
                       `(lambda ()
                          ,clause))
                     clauses))))


(defmacro atomic (&body body)
   `(orelse
     (progn
       ,@body)))


(defmacro check (&body body)
   ;; Transaction checking
   `(unless (atomic ,@body)
      (rollback)))


;; (defun do-rmw (place fn)
;;   "RMW = read / modify / write"
;;   (warn "do-rmw is deprecated, use execute-transaction instead")
;;   (atomic (dstm:write place (funcall fn (dstm:read place)))))


;; (defmacro rmw ((var-name place) &body body)
;;   (warn "rmw is deprecated, use with--update instead")
;;    `(do-rmw ,place (lambda (,var-name)
;;                      ,@body)))


(defun execute-transaction (place fn)
  "read / if modify, then write"
  (atomic
    (let* ((original-value (dstm:read place))
            (updated-value (funcall fn original-value)))
      (if (eq updated-value :secret-unchanged-value-flag)
        original-value
        (dstm:write place updated-value)))))


(defmacro with-update ((var-name place) &body body)
  `(let ((place ,place))
     (if (cl:typep place 'dstm:var)
       (execute-transaction ,place (lambda (,var-name) (declare (ignorable ,var-name))
                                     :secret-unchanged-value-flag ,@body))
       (error "~S is not a transactional variable" place))))
     
#+()
(defgeneric value (var)
  (:documentation "Atomically read the value of a DSTM transactional variable"))
#+()
(defmethod  value ((var t))
  (error "Attempt to read the VALUE of the NON-TRANSACTIONAL place ~S. Try safe-value instead."
    var))

(defmethod  value ((var dstm-var))
  (atomic (dstm:read var)))


(defun safe-value (thing)
  "If thing is a DSTM variable, read and return its value within an atomic transaction.
   If thing is not a DSTM variable, simply return its value."
  (if (cl:typep thing 'dstm:var)
    (value thing)
    thing))

#+()
(defgeneric (setf value) (new-value var))
#+()
(defmethod (setf value) (new-value (var t))
  (error "Attempt to write the VALUE of the NON-TRANSACTIONAL place ~S" var))

(defmethod (setf value) (new-value (var dstm-var))
  (atomic (dstm:write var new-value)))

#+()
(defmethod (setf value) ((new-value var) (var dstm-var))
  (error "The VALUE of a transactional place should not, itself, be a transactional variable."))


(define-modify-macro updatef (new-value)
  "Execute an atomic DSTM transaction to update the VALUE of a DSTM variable to NEW-VALUE."
  (lambda (place new-value)
    (setf (value place) new-value)))


(defun reset ()
  (setf (current-transaction) nil)
  (setf (aref  *nrolls* 0) 0)
  (setf (aref  *ntrans* 0) 0))





;; (let ((x #{1 2 3}))
;;   (setf (value x) {0})
;;   x)

;; (let ((x #{1 2 3}))
;;   (updatef x {0})
;;   x)
