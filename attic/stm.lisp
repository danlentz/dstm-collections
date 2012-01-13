;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;
;;;;; Lock Free DTSM -- Software Transactional Memory after Herlihy, et. al.
;;;;;
;;;;; based on:
;;;;;  http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.59.8787
;;;;;
;;;;; original implementation by: Dr. David McClain
;;;;; portions Copyright (C) 2008-2010 by SpectroDynamics, LLC
;;;;;

(defpackage :dstm
   (:use #:common-lisp)
   (:export
    #:var
    #:create-var
    #:read-var
    #:write-var
    #:write-vars
    #:atomic
    #:orelse
    #:rollback
    #:rmw
    #:check))

(in-package :dstm)



(defclass transaction ()
  ((state   :accessor transaction-state  :initform :active  :initarg :state)
    (root   :reader   transaction-root   :initform nil)
    (reads  :accessor transaction-reads  :initform nil)
    (subs   :accessor transaction-subs   :initform nil)
    (parent :accessor transaction-parent :initform (current-transaction))))


(defun current-transaction ()
  (unless sb-thread:*current-thread*
    (error "Multiprocessing not running"))
  (sb-thread:symbol-value-in-thread 'transaction sb-thread:*current-thread* nil ))


(defun (setf current-transaction) (trans)
  (setf (sb-thread:symbol-value-in-thread 'transaction sb-thread:*current-thread*) trans))


(defmethod initialize-instance :after ((trans transaction) &key &allow-other-keys)
  (setf (slot-value trans 'root)
    (let ((curtrans (current-transaction)))
      (if curtrans
        (transaction-root curtrans)
        trans))))


(defun equivalentp (trans1 trans2)
   (eq (transaction-root trans1) (transaction-root trans2)))


(defclass var ()
   ;; make a consistent top-level export representing STM vars
   ())


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
      :initform (load-time-value
                  (make-instance 'transaction :state :committed)))))


(defun create-var (&optional val)
   (make-instance 'dstm-var :new val))


(define-condition rollback-exn ()
   ())


(defun reclaim-lists (trans)
  (setf (transaction-reads  trans) nil
    (transaction-subs   trans) nil))

(defparameter *nrolls* (make-array '(1) :element-type 'sb-ext:word :initial-element #x0))
(defparameter *ntrans* (make-array '(1) :element-type 'sb-ext:word :initial-element #x0))
;; (defvar *ntrans* (make-array '(1) :element-type 'sb-ext:word :initial-element #x0))

;; (defparameter *nrolls* 0)
;; (defparameter *ntrans* 0)

(defun rollback-trans-and-subs (trans)
   (dolist (sub (transaction-subs trans))
     (rollback-trans-and-subs sub))
   (setf (transaction-state trans) :ABORTED)
   (reclaim-lists trans))


(defun rollback ()
  (let ((trans (current-transaction)))
    ;; rollbacks outside of transactions are permitted but meaningless
    (when trans      
      (sb-ext:atomic-incf (aref *nrolls* 0)) ;; *nrolls*)
      (rollback-trans-and-subs trans)
      (error (load-time-value
               (make-condition 'rollback-exn)
               t)))))


(defun check-reads (trans)
  #+lispworks
  (sys:ensure-memory-after-store)
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



(defun read-var (var)
  (let ((trans (current-transaction)))
    (loop
      #+lispworks
      (sys:ensure-memory-after-store)
      (let* ((vtrans (dstm-var-trans var))
              (vstate (transaction-state vtrans)))
        (when (or (not (eq :ACTIVE vstate))
                (and trans (equivalentp trans vtrans)))
          (when trans
            (push (cons var vtrans) (transaction-reads trans)))
          (return (if (eq :ABORTED vstate)
                    (dstm-var-old var)
                    (dstm-var-new var))))
        ))))



(defun write-var (var val)
   (let* ((trans   (current-transaction))
          (wtrans  (or trans (make-instance 'transaction))))
     (prog1
         (write-var-with-transaction wtrans var val)
       (unless trans
         (setf (transaction-state wtrans) :COMMITTED)))))


(defun write-vars (&rest pairs)
   (do ((pairs pairs (cddr pairs)))
       ((null pairs))
     (write-var (car pairs) (cdar pairs))))


(defun write-var-with-transaction (trans var val)
  "trans is the current transaction for a thread"
  (loop
    #+lispworks
    (sys:ensure-memory-after-store)
    (let* ((vtrans (dstm-var-trans var))
            (vstate (transaction-state vtrans)))
      (cond
        ((not (eq :ACTIVE vstate))
          (when (sb-ext:compare-and-swap (slot-value var 'trans) vtrans trans)
            (when (eq :COMMITTED vstate)
              (setf (dstm-var-old var) (dstm-var-new var)))
            (return (setf (dstm-var-new var) val)) ))
        ((equivalentp trans vtrans)
          (return (setf (dstm-var-new var) val)))
        ))))


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
        (sb-ext:atomic-incf (aref *ntrans* 0))
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
              (error exn)) )
          ;; unwind
          (setf (current-transaction) ct-save)) )
      ;; end of list
      (when ct-save
        (rollback)) )))


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


(defun do-rmw (place fn)
  "RMW = read / modify / write"
  (atomic (write-var place (funcall fn (read-var place)))))


(defmacro rmw ((var-name place) &body body)
   `(do-rmw ,place (lambda (,var-name)
                     ,@body)))


;; ---------------------------------------------------
;; Test it out... hopefully lots of contention... yep!


(defun show-rolls (&optional (duration 1))
   (let ((pcnt (/ (aref *nrolls* 0) (aref *ntrans* 0) 0.01))
         (rate (/ (aref *ntrans* 0) duration)))
     (list :rollbacks (aref *nrolls* 0)
           :transactions (aref *ntrans* 0)
           :percent-rollbacks pcnt
           :trans-per-roll (if (zerop pcnt) :infinite (* 100 (/ pcnt)))
           :duration duration
           :trans-per-sec  rate)))

(defun reset ()
   (setf (current-transaction) nil)
   (setf (aref  *nrolls* 0) 0)
   (setf (aref  *ntrans* 0) 0))

(defvar *a* (create-var 0))
(defvar *b* (create-var 0))

(defun check-invariant (&aux a b)
  (atomic
    (setf a (read-var *a*)
      b (read-var *b*)))
  (unless (= b (* 2 a))    
    (format *trace-output* "Invariant broken: A = ~A, B = ~A" a  b)))


(defun common-code (delta)
  (atomic
    (let ((a (+ delta (read-var *a*))))
      (write-var *a* a)
      (write-var *b* (* 2 a)))))

(defun count-up ()
   (loop repeat 5000000 do (common-code 1)))

(defun count-down ()
   (loop repeat 5000000 do (common-code -1)))

#+()
(defun checker (&rest procs)
  (let ((start (local-time:now)))
     (loop while (some #'sb-thread:thread-alive-p procs)
           do (check-invariant))
    (let ((stop (local-time:now)))    ;;usec:get-time-usec)))
      (princ (show-rolls (* 1e-6 (local-time:timestamp-difference  stop start))))) ))

(defun test-dstm ()

  ;; so as not to impose the several dependencies required only for testing on the
  ;; general user population, we check for their presence and load the packages
  ;; (only if necessary) as part of the initial unit-test setup process.
  
  (or (find-package :local-time)      (ql:quickload :local-time))
  (or (find-package :bordeaux-threads (ql:quickload :bordeaux-threads)))

  ;; (re)initialization of fixtures required for test
  
  (princ "Start DSTM Test..." *trace-output*)
  (setf *a* (create-var 0))
  (setf *b* (create-var 0))
  (reset)

  ;; here begins the actual unit test
  
  (let ((start (local-time:now))
         (procs
           (list
             (bt:make-thread #'count-down :name "down" :initial-bindings '((transaction . nil)))
             (bt:make-thread #'count-up   :name "up"   :initial-bindings '((transaction . nil))))))
    (loop while (some #'sb-thread:thread-alive-p procs)  do (check-invariant))
    (let ((stop (local-time:now)))
      (princ (show-rolls (* 1e-6 (local-time:timestamp-difference stop start)))
        *trace-output*))))

;; a: 500000 b: 500000
;; Start DSTM Test...
;; (ROLLBACKS 23 TRANSACTIONS 1439589 PERCENT-ROLLBACKS 0.0015976782 TRANS-PER-ROLL
;;  62590.83 DURATION 4.6898001881593924d-5 TRANS-PER-SEC 3.06961691808238d10)

;; a: 1000000 b: 1000000
;; Start DSTM Test...
;; (ROLLBACKS 44 TRANSACTIONS 2221568 PERCENT-ROLLBACKS 0.0019805832 TRANS-PER-ROLL
;;  50490.18 DURATION 7.452033281185425d-5 TRANS-PER-SEC 2.9811568415950584d10)

;; a: 5000000 b: 5000000
;; Start DSTM Test...
;; (ROLLBACKS 261 TRANSACTIONS 13883984 PERCENT-ROLLBACKS 0.0018798639
;;  TRANS-PER-ROLL 53195.344 DURATION 4.5344716185515596d-4 TRANS-PER-SEC 3.061874716162617d10)
