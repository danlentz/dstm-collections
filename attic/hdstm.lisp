----------------------------------------
;; herlihy-dstm.lisp -- Software Transactional Memory after Herlify, et. al.
;;
;; See paper: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.59.8787&rep=rep1&type=pdf
;;
;; DM/RAL  01/10
;; -------------------------------------------------------------------------

(defpackage :hdstm
  (:use :common-lisp))


(in-package :hdstm)


(defclass dstm-ref ()
  ((cell  :accessor dstm-ref-cell  :initform nil  :initarg :cell)
   ;; (lock  :reader   dstm-ref-lock  :initform (mp:make-lock))
   ))

(defclass dstm-val ()
  ((old   :reader   dstm-val-old   :initform nil  :initarg :old)
   (new   :accessor dstm-val-new   :initform nil  :initarg :new)
   (trans :reader   dstm-val-trans :initform nil  :initarg :trans)
    ))

(defclass transaction ()
  ((state  :accessor transaction-state :initform :active)
   (reads  :accessor transaction-reads :initform nil)
   ))

(defun create-var (&optional val)
  (make-instance 'dstm-ref
                 :cell (make-instance 'dstm-val
                                      :new val)))
;; ----------------------------------------------------

(defmacro volatile (&body body)
  `(progn
     (ensure-memory-after-store)
     , <at> body))

(defmacro flush-volatile (&body body)
  `(prog1
       (progn
         , <at> body)
     (sys:ensure-memory-after-store)))

;; ----------------------------------------------------

(defun current-transaction ()
  (unless (mpcompat:current-process)
    (error "Multiprocessing not running") )
  (mpcompat:process-private-property 'transaction))

(defun set-current-transaction (trans)
  (setf (mpcompat:process-private-property 'transaction) trans))
  
;; ----------------------------------------------------

(define-condition rollback-exn ()
  ())

(defun set-state (trans new-state)
  (um:nlet-tail iter ()
    (let ((old-state (volatile (transaction-state trans))))
      (unless (or (eq old-state new-state)
                  (flush-volatile
                    (sys:compare-and-swap (slot-value trans 'state)
                                          old-state new-state)))
        (iter)) )) )

(defun abort-transaction (trans)
  (set-state trans :ABORTED)
  (setf (transaction-reads trans) nil))

(defvar *nrolls* 0)

(defun rollback (trans)
  (when trans
    (sys:atomic-incf *nrolls*)
    ;; (bfly:log-info :SYSTEM-LOG "Rollback")
    (abort-transaction trans)
    (when (eq trans (current-transaction))
      (error (make-condition 'rollback-exn)))))

(defun check-reads (trans)
  (dolist (pair (shiftf (transaction-reads trans) nil))
    (destructuring-bind (var . vref) pair
      (let ((vnow (volatile (dstm-ref-cell var))))
        (unless (or (eq vref vnow)
                    (eq trans (dstm-val-trans vnow)))
          (rollback trans)) ))))

(defun commit ()
  (let ((trans (current-transaction)))
    (unless (eq :ACTIVE (volatile (transaction-state trans)))
      (rollback trans))
    ;; (bfly:log-info :SYSTEM-LOG "Committing")
    (check-reads trans)
    (set-state trans :COMMITTED)))

;; ----------------------------------------------------

(defun current-value (vref)
  (um:if-let (vtrans (dstm-val-trans vref))
      (if (eq :aborted (volatile (transaction-state vtrans)))
          (dstm-val-old vref)
        (dstm-val-new vref))
    ;; else
    (dstm-val-new vref)))

;; ----------------------------------------------------

(defun conflict-manager-for-read (trans1 trans2)
  (declare (ignore trans1 trans2))
  ;; either rollback trans1 - the reader and don't return
  ;; or else rollback trans2 - the writer and do return
  ;; or... just wait and return
  ;; (sleep 0.01)
  )

(defun conflict-manager-for-write (trans1 trans2)
  (declare (ignore trans2))
  ;; either rollback trans1 - the new writer and don't return
  ;; or else rollback trans2 - the old writer and do return
  ;; or... just wait and return
  (rollback trans1))

;; ----------------------------------------------------

(defun read-var (var)
  (let ((trans (current-transaction)))
    (um:nlet-tail iter ()
      (progn ;; mpcompat:with-spinlock ((dstm-ref-lock var))
        (let* ((vref   (volatile (dstm-ref-cell var)))
               (vtrans (dstm-val-trans vref)))
          
          (cond
           ((null trans)
            (dstm-val-new vref))
           
            ((eq :ABORTED (volatile (transaction-state trans)))
             (rollback trans))

           ((eq trans vtrans)
            (push (cons var vref) (transaction-reads trans))
            (dstm-val-new vref))
           
           ((and vtrans
                 (eq :ACTIVE (volatile (transaction-state vtrans))))
            (conflict-manager-for-read trans vtrans)
            (iter)) ;; spin-wait for writer to finish
           
           (t 
            (let ((val (current-value vref)))
              (push (cons var vref) (transaction-reads trans))
              val))
           ))))))

(defun write-var (var val)
  (let ((trans  (current-transaction)))
    (um:nlet-tail iter ()
      (progn ;; mpcompat:with-spinlock ((dstm-ref-lock var))
        (let* ((vref   (volatile (dstm-ref-cell var)))
               (vtrans (dstm-val-trans vref)))
          (cond
           ((or (null trans)
                (eq trans vtrans))
            (flush-volatile (setf (dstm-val-new vref) val)))
           
           ((eq :ABORTED (volatile (transaction-state trans)))
            (rollback trans))
           
           ((and vtrans
                 (eq :active (volatile (transaction-state vtrans))))
            (conflict-manager-for-write trans vtrans)
            (iter))
           
           (t
            (let ((oldv (current-value vref)))
              (unless (flush-volatile
                        (volatile
                          (sys:compare-and-swap (slot-value var 'cell) vref
                                                (make-instance 'dstm-val
                                                               :old   oldv
                                                               :new   val
                                                               :trans trans))))
                (iter)
                ;; (rollback trans)
                )))
           ))))))
    
;; --------------------------------------------------

(defvar *ntrans* 0)

(defun do-atomic (fn)
  ;; Perform the function fn under a transaction, allowing restart
  ;; when needed
  (if (current-transaction) ;; nested transaction?
      (funcall fn)
    
    ;; else - fresh transaction
    (loop
     (set-current-transaction (progn
                                (sys:atomic-incf *ntrans*)
                                (make-instance 'transaction)))
     (handler-case
         (return-from do-atomic
           (prog1
               (funcall fn)
             (commit)
             (set-current-transaction nil)))
         
         (rollback-exn (exn)
           (declare (ignore exn)))

         #| |#
         (error (exn)
           (abort-transaction (current-transaction))
           (set-current-transaction nil)
           (error exn))
         #| |#
         ))))

(defmacro atomic (&body body)
  `(do-atomic (lambda () , <at> body)))

(defun show-rolls ()
  (list *nrolls* *ntrans* (/ *nrolls* *ntrans* 0.01)))

;; ---------------------------------------------------------------

(defun do-orelse (&rest fns)
  ;; Perform one of the functions in the list fns.  The list is
  ;; examined in order, front to back.  The first one to succeed is
  ;; the sub-transaction accepted.  If none succeed, or the overall
  ;; transaction fails, the the whole thing is restarted.
  ;;
  ;; Each level of ORELSE nesting pushes a new hashtable onto the
  ;; write cache list of hashtables. The last one is the one belonging
  ;; to the outermost ATOMICALLY transaction.
  (if (current-transaction)
      (progn
        (dolist (fn fns)
          (handler-case
              (return-from do-orelse
                (funcall fn))
            
            (rollback-exn (exn)
              (declare (ignore exn)))

            (error (exn)
              (abort-transaction (current-transaction))
              (set-current-transaction nil)
              (error exn))))
        
        (rollback (current-transaction)))
    
    ;; else - fresh transaction
    (loop
     (set-current-transaction (make-instance 'transaction))
     (dolist (fn fns)
       (handler-case
           (return-from do-orelse
             (prog1
                 (funcall fn)
               (commit)
               (set-current-transaction nil)))
           
           (rollback-exn (exn)
             (declare (ignore exn)))

           (error (exn)
             (abort-transaction (current-transaction))
             (set-current-transaction nil)
             (error exn))
           )) )) )

(defmacro orelse (&rest clauses)
  `(apply #'do-orelse
    ,(mapcar (lambda (clause)
               `(lambda ()
                  ,clause))
             clauses)))

;; ---------------------------------------------------
;; Test it out... hopefully lots of contention... yep!
#|

(defvar *a* (create-var 0))
(defvar *b* (create-var 0))

(defun check-invariant (&aux a b)
  (atomic
    (setf a (read-var *a*)
          b (read-var *b*)))
    (unless (= b (* 2 a))
      (bfly:log-info :SYSTEM-LOG "Invariant broken: A = ~A, B = ~A" a b))))

(defun common-code (delta)
  (atomic
    (let ((a (+ delta (read-var *a*))))
      (write-var *a* a)
      (write-var *b* (* 2 a))))
  (check-invariant))

(defun count-up ()
  (loop repeat 5000000 do (common-code 1)))

(defun count-down ()
  (loop repeat 5000000 do (common-code -1)))

(progn
  (setf *a* (create-var 0)
        *b* (create-var 0))
  (setf *nrolls* 0)
  (setf *ntrans* 0)
  (set-current-transaction nil)
  ;; (common-code 1)
  ;; (count-up)
  (bfly:spawn #'count-up)
  ;; (common-code 1)
  (bfly:spawn #'count-down)
  )

|#
