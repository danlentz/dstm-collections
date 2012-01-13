----------------------------------------------------------------------------------------------------
;; dstm-no-locking.lisp -- Software Transactional Memory after  
Herlihy, et. al.
;;
;; See paper: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.59.8787&rep=rep1&type=pdf
;;
;; Copyright (C) 2008-2010 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/RAL  01/10
;;  
-------------------------------------------------------------------------

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
    #:check
    ))

;; ---------------------------------------------------------------------
(in-package :dstm)
;; ---------------------------------------------------------------------

(defclass transaction ()
   ((state  :accessor transaction- 
state  :initform :active  :initarg :state)
    (root   :reader   transaction-root   :initform nil)
    (reads  :accessor transaction-reads  :initform nil)
    (subs   :accessor transaction-subs   :initform nil)
    (parent :accessor transaction-parent :initform (current- 
transaction))
    ))

;; ----------------------------------------------------

(defun current-transaction ()
   (unless mp:*current-process*
     (error "Multiprocessing not running") )
   (mp:process-private-property 'transaction))

(defun (setf current-transaction) (trans)
   (setf (mp:process-private-property 'transaction) trans))

;; ----------------------------------------------------

(defmethod initialize-instance :after ((trans transaction)
                                        &key &allow-other-keys)
   (setf (slot-value trans 'root)
         (let ((curtrans (current-transaction)))
           (if curtrans
               (transaction-root curtrans)
             trans))))

(defun equivalentp (trans1 trans2)
   (eq (transaction-root trans1) (transaction-root trans2)))

;; ----------------------------------------------------

(defclass var ()
   ;; make a consistent top-level export representing STM vars
   ())

;; ----------------------------------------------------

(defclass dstm-var (var)
   ((new   :accessor dstm-var-new   :initform nil  :initarg :new)
    (old   :accessor dstm-var-old   :initform nil)
    (trans :accessor dstm-var-trans :initform (load-time-value
                                               (make-instance  
'transaction
                                                              :state 
  :committed)))
    ))

(defun create-var (&optional val)
   (make-instance 'dstm-var :new val))

;; ----------------------------------------------------

(define-condition rollback-exn ()
   ())

(defun reclaim-lists (trans)
   (setf (transaction-reads  trans) nil
         (transaction-subs   trans) nil))

(defvar *nrolls* 0)

(defun rollback-trans-and-subs (trans)
   (dolist (sub (transaction-subs trans))
     (rollback-trans-and-subs sub))
   (setf (transaction-state trans) :ABORTED)
   (reclaim-lists trans))

(defun rollback ()
   (let ((trans (current-transaction)))
     (when trans
       ;; rollbacks outside of transactions are permitted but
       ;; meaningless
       (sys:atomic-incf *nrolls*)
       (rollback-trans-and-subs trans)
       (error (load-time-value
               (make-condition 'rollback-exn)
               t)))))

(defun check-reads (trans)
   (sys:ensure-memory-after-store)
   (dolist (pair (transaction-reads trans))
     (destructuring-bind (var . vtrans) pair
       (let ((vnow (dstm-var-trans var)))
         (unless (or (eq vnow vtrans)          ;; unchanged?
                     (equivalentp trans vnow)) ;; ...or changed by us...
           (rollback)) ))))

(defun commit (final)
   (commit-with-transaction (current-transaction) final))

(defun commit-with-transaction (trans final)
   (dolist (sub (transaction-subs trans))
     (commit-with-transaction sub final))
   (check-reads trans)
   (let ((parent (shiftf (transaction-parent trans) nil)))
     (when parent
       ;; trans succeeded for now, add to parents subs-list, but
       ;; leave in :ACTIVE state
       ;; -- happens only on the first commit attempt
       (push trans (transaction-subs parent))))
   (when final
     (setf (transaction-state trans) :COMMITTED)
     (reclaim-lists trans)))

;; ----------------------------------------------------

(defun read-var (var)
   (let ((trans (current-transaction)))
     (loop
      (sys:ensure-memory-after-store)
      (let* ((vtrans (dstm-var-trans var))
             (vstate (transaction-state vtrans)))
        (when (or (not (eq :ACTIVE vstate))
                  (and trans
                       (equivalentp trans vtrans)))
          (when trans
            (push (cons var vtrans) (transaction-reads trans)))
          (return (if (eq :ABORTED vstate)
                      (dstm-var-old var)
                    (dstm-var-new var))) )
        ))))

;; ----------------------------------------------------------

(defun write-var (var val)
   (let* ((trans   (current-transaction))
          (wtrans  (or trans
                       (make-instance 'transaction))))
     (prog1
         (write-var-with-transaction wtrans var val)
       (unless trans
         (setf (transaction-state wtrans) :COMMITTED)))))

(defun write-vars (&rest pairs)
   (do ((pairs pairs (cddr pairs)))
       ((null pairs))
     (write-var (car pairs) (cdar pairs))))

(defun write-var-with-transaction (trans var val)
   ;; trans is the current transaction for a thread
   (loop
    (sys:ensure-memory-after-store)
    (let* ((vtrans (dstm-var-trans var))
           (vstate (transaction-state vtrans)))
      (cond
       ((not (eq :ACTIVE vstate))
        (when (sys:compare-and-swap (slot-value var 'trans) vtrans  
trans)
          (when (eq :COMMITTED vstate)
            (setf (dstm-var-old var) (dstm-var-new var)))
          (return (setf (dstm-var-new var) val)) ))

       ((equivalentp trans vtrans)
        (return (setf (dstm-var-new var) val)))
       ))))

;; --------------------------------------------------

(defvar *ntrans* 0)

(defun do-orelse (&rest fns)
   ;; Perform one of the functions in the list fns.  The list is
   ;; examined in order, front to back.  The first one to succeed is
   ;; the sub-transaction accepted.  If none succeed, or the overall
   ;; transaction fails, the the whole thing is restarted.
   (let ((ct-save (current-transaction)))
     (loop
      (dolist (fn fns)
        (setf (current-transaction)
         (make-instance 'transaction))
        (sys:atomic-incf *ntrans*)
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

;; ---------------------------------------------------------------

;; common idiom... RMW = read / modify / write
(defun do-rmw (place fn)
   (atomic
     (write-var place (funcall fn (read-var place)))))

(defmacro rmw ((var-name place) &body body)
   `(do-rmw ,place (lambda (,var-name)
                     ,@body)))

;; ---------------------------------------------------
;; Test it out... hopefully lots of contention... yep!
#|

(defun show-rolls (&optional (duration 1))
   (let ((pcnt (/ *nrolls* *ntrans* 0.01))
         (rate (/ *ntrans* duration)))
     (list :rollbacks *nrolls*
           :transactions *ntrans*
           :percent-rollbacks pcnt
           :trans-per-roll (if (zerop pcnt) :infinite (* 100 (/ pcnt)))
           :duration duration
           :trans-per-sec  rate)))

(defun reset ()
   (setf (current-transaction) nil)
   (setf *nrolls* 0)
   (setf *ntrans* 0))

(defvar *a* (create-var 0))
(defvar *b* (create-var 0))

(defun check-invariant (&aux a b)
   (atomic
     (setf a (read-var *a*)
           b (read-var *b*)
           ))
     (unless (= b (* 2 a))
       (bfly:log-info :SYSTEM-LOG "Invariant broken: A = ~A, B = ~A" a  
b)))

(defun common-code (delta)
   (atomic
     (let ((a (+ delta (read-var *a*))))
       (write-var *a* a)
       (write-var *b* (* 2 a))
       ;; (check (= (read-var *b*) (* 2 (read-var *a*))))
       )))

(defun count-up ()
   (loop repeat 5000000 do (common-code 1)))

(defun count-down ()
   (loop repeat 5000000 do (common-code -1)))

(defun checker (&rest procs)
   (let ((start (usec:get-time-usec)))
     (loop while (some #'mp:process-alive-p procs)
           do (check-invariant))
     (let ((stop (usec:get-time-usec)))
       (bfly:log-info :SYSTEM-LOG (show-rolls (* 1e-6 (- stop  
start))))) ))

(progn
   (bfly:log-info :SYSTEM-LOG "Start HDSTM Test...")
   (setf *a* (create-var 0)
         *b* (create-var 0))
   (reset)
   (bfly:spawn #'checker
               :name :checker
               :args (mapcar #'bfly:pid-proc
                             (list (bfly:spawn #'count-down
                                               :name :up-counter)
                                   (bfly:spawn #'count-up
                                               :name :down-counter))))
   )

|#

Dr. David McClain
dbm <at> refined-audiometrics.com
