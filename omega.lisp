;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(defpackage :omega
  (:use :closer-common-lisp)
  (:export
    :timing-info
    :real-time
    :user-time
    :system-time
    :gc-time
    :page-faults
    :bytes-consed
    :collect-timing
    :pprint-bytes
    :pprint-milliseconds
    :with-profiling-enabled
    :execution-time
    :group
    :partion-by
    :format-relative-time
    :time-difference))
    
(in-package :omega)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Metric Specification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass timing-info ()
  ((real-time
     :accessor real-time
     :initarg  :real-time
     :initform :not-available
     :documentation "Real time, in milliseconds.")
    (user-time
      :accessor user-time
      :initarg  :user-time
      :initform :not-available
      :documentation "User time, in milliseconds.")
    (system-time
      :accessor system-time
      :initarg  :system-time
      :initform :not-available
      :documentation "System time, in milliseconds.")
    (gc-time
      :accessor gc-time
      :initarg  :gc-time
      :initform :not-available
      :documentation "GC time, in milliseconds.")
    (page-faults
      :accessor page-faults
      :initarg  :page-faults
      :initform :not-available
      :documentation "Number of page faults.")
    (bytes-consed
      :accessor bytes-consed
      :initarg  :bytes-consed
      :initform :not-available
      :documentation "Number of bytes allocated.")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Report and Correlation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun group (list &key (test #'eql) (key #'identity))
  "Divides LIST by TEST and returns a list of groups.
  TEST ia a function that takes two arguments and returns true if both
  belong to same group, as shown in the following examples.
  ;;;    (group '(1 1 2 3 3 3))
  ;;;          =>  ((1 1) (2) (3 3 3))
  ;;;    (group '(:a a b c) :test #'string=)
  ;;;          =>  ((:A A) (B) (C))
  ;;;    (group '((:a 1) (:a 2) (:b 3)) :key #'car)
  ;;;          =>  (((:A 1) (:A 2)) ((:B 3)))"
  (loop
    :with     group
    :with     groups
    :for      x = (first list) :then y
    :for      y :in list
    :if       (funcall test (funcall key x) (funcall key y)) :do (push y group)
    :else :do (push (nreverse group) groups)  (setq group (list y))
    :finally  (return (nreverse (if group
                                  (cons group groups)
                                  groups)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun span (list pfun &key (key #'identity))
  (let ((current (funcall key (first list))))
    (labels ((split (sublist list)
	       (cond
		 ((null list) (list sublist nil))
		 ((funcall pfun (funcall key (first list)) current)
		  (split (cons (first list) sublist) (rest list)))
		 (t (list (nreverse sublist) list)))))
      (split nil list))))

(defun partion-by (list pfun &key (key #'identity))
  (labels ((join (list result)
	     (destructuring-bind 
	      (partion rest) (span list pfun :key key)
	       (cond
		 ((null rest) (nreverse (cons partion result)))
		 (t (join rest (cons partion result)))))))
    (join list nil)))
   
(defparameter *test-list* '((3 5) (3 1) (3 3) (1 2) (1 7) (1 1) (2 3)))

(defun ex (list)
  (mapcan (lambda (sublist) (sort sublist #'< :key #'second))
    (partion-by list #'eql :key #'first)))

;; list at start
;; '((3 5) (3 1) (3 3) (1 2) (1 7) (1 1) (2 3)))
;; 1. partion list by identical first element
;; result: '(((3 5) (3 1) (3 3)) ((1 2) (1 7) (1 1)) ((2 3)))
;; 2. sort sublists by second element
;; result: '(((3 1) (3 3) (3 5)) ((1 1) (1 2) (1 7)) ((2 3)))
;; 3. concatenate the sublists
;; result: '((3 1) (3 3) (3 5) (1 1) (1 2) (1 7) (2 3))


(defun plist-to-alist (plist)
  (loop
    for (key value) on plist by #'cddr
    collect (cons key value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Format and Presentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((info timing-info) stream)
  (print-unreadable-object (info stream :type t :identity t)
    (format stream "~A/~A"
      (pprint-milliseconds (real-time info))
      (pprint-bytes (bytes-consed info)))))


(defun pprint-milliseconds (milliseconds &optional stream)
  (cond
    ((< milliseconds 1000)        (format stream "~D ms" milliseconds))
    ((= milliseconds 1000)        (format stream "1.00 second"))
    ((< milliseconds (* 60 1000)) (format stream "~,2F seconds" (/ milliseconds 1000)))
    ((= milliseconds (* 60 1000)) (format stream "1.00 minute"))
    (t                            (format stream "~,2F minutes"
                                    (/ milliseconds (* 60 1000))))))


(defun pprint-bytes (num-bytes &optional stream)
  "Writes NUM-BYTES to stream, rounds num-bytes and appends a suffix
  depending on the size of num-bytes."
  (cond
    ((< num-bytes (expt 2 10))   (format stream "~D B" num-bytes))
    ((< num-bytes (expt 2 20))   (format stream "~,2F Kb" (/ num-bytes (expt 2 10))))
    ((< num-bytes (expt 2 30))   (format stream "~,2F Mb" (/ num-bytes (expt 2 20))))
    ((< num-bytes (expt 2 40))   (format stream "~,2F Gb" (/ num-bytes (expt 2 30))))
    (t                           (format stream "~,2F Tb" (/ num-bytes (expt 2 40))))))


(defun pprint-to-string (struct &optional (right-margin *print-right-margin*))
  (with-output-to-string (stream)
    (let ((*print-pretty* t)
          (*print-right-margin* right-margin))
      (write struct :stream stream))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Format Relative Time --  http://paste.lisp.org/display/127561
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun time-difference (a b &optional (count-weeks-p t))
  (unless (local-time:timestamp<= a b)
    (cerror "Swap the timestamps." "Expected ~A to be before ~A" a b)(rotatef a b))
  (let ((result ())
         (carry 0))
    (flet ((collect (delta base unit)
             (decf delta carry)
             (cond
               ((minusp delta) (setf carry 1) (incf delta base))
               (t              (setf carry 0)))
             (push delta result)
             (push unit result)))
      (multiple-value-bind (nsec1 sec1 min1 hour1 day1 month1 year1)
        (local-time:decode-timestamp a :timezone local-time:+utc-zone+)
        (multiple-value-bind (nsec2 sec2 min2 hour2 day2 month2 year2)
          (local-time:decode-timestamp b :timezone local-time:+utc-zone+)
          (collect (- nsec2 nsec1) 1000000000 :nanosecond)
          (collect (- sec2 sec1) 60 :second)
          (collect (- min2 min1) 60 :minute)
          (collect (- hour2 hour1) 24 :hour)
          ;; If the end-day is larger than start-day then keep the
          ;; month, and days are the difference between end-day and
          ;; start-day (the do-nothing-special case). If the end-day
          ;; is smaller than start-day, days is the number of days
          ;; left in start-month plus the number of days in end monht,
          ;; and month is decresed by 1.
          (let ((adjusted-day2 (- day2 carry)))
            (setf carry 0)
            (multiple-value-bind (days months)
              (cond
                ((<= day1 adjusted-day2) (values (- adjusted-day2 day1) (- month2 month1)))
                (t (let ((sdays (- (local-time:days-in-month month1 year1) day1)))
                     (values (+ sdays adjusted-day2)
                       (- month2 month1 1)))))
              (if count-weeks-p
                (multiple-value-bind (weeks days) (floor days 7)
                  (collect days nil :day)
                  (collect weeks nil :week))
                (collect days nil :day))
              (collect months 12 :month)))
          (collect (- year2 year1) 0 :year))))
    result))


(defun format-relative-time (time &key (base (local-time:now)) (min-unit :minute) (resolution 1)
                              (include-empty nil) (past-format "~@(~A~) ago")
                              (future-format "In ~A") (stream nil))
  (check-type min-unit (member :nanosecond :second :minute :hour :day :week :month :year))
  (when (local-time:timestamp= time base)
    (return-from format-relative-time "Now"))
  (multiple-value-bind (pastp relative-time) (if (local-time:timestamp< time base)
                                               (values t   (time-difference time base))
                                               (values nil (time-difference base time)))
    (setf relative-time (reverse (plist-to-alist relative-time)))
    (setf relative-time (member min-unit relative-time :key #'car))
    (unless include-empty
      (setf relative-time (remove 0 relative-time :key #'cdr)))
    (when (numberp resolution) (setf relative-time (last relative-time resolution)))
    (let* ((fragments (mapcar (lambda (cons)
                                (format nil "~D ~(~A~)~P" (cdr cons) (car cons)(cdr cons)))
                        relative-time))
            (time-string (if (null fragments)
                           (format nil "less than ~(~A~)" min-unit)
                           (format nil "~{~A~#[~; and ~:;, ~]~}" (reverse fragments)))))
      (format stream (if pastp past-format future-format) time-string))))


;; (format-relative-time (local-time:today)) => "19 hours ago"

(assert (equal "In 7 months" (format-relative-time (local-time:universal-to-timestamp
                                       (+ (get-universal-time) 20480000)))))

(assert (equal "3 years ago" (format-relative-time (local-time:universal-to-timestamp
                                                     (- (get-universal-time) 99990000)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Measurement and Instrumentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+sbcl
(defmacro with-profiling-enabled (&body body)
  `(progn (require :sb-sprof)
     (sb-sprof:with-profiling ()
       ,@body)))


(defmacro execution-time (&body body)
  "Return the number of milliseconds it takes to execute BODY as the second value"
  (let ((tm  (gensym))
         (res (gensym)))
    `(let* ((,tm (get-internal-real-time))
             (,res (progn ,@body))
             (,tm (floor (* 1000 (- (get-internal-real-time) ,tm))
                    internal-time-units-per-second)))
       (values ,res ,tm))))


#+sbcl
(defun collect-timing (thunk)
  "Executes THUNK and returns a timing-info object specifying how long
  execution took and how much memory was used. Implementation of
  collect-timing for SBCL. This code is a cut and paste adoption from
  from sbcl/src/code/time.lisp"
  (declare (type function thunk))
  
  (let (old-run-utime       new-run-utime       old-run-stime
         new-run-stime       old-real-time       new-real-time
         old-page-faults     new-page-faults     real-time-overhead
         run-utime-overhead  run-stime-overhead  page-faults-overhead
         old-bytes-consed    new-bytes-consed    cons-overhead)
    
    (multiple-value-setq                ;; Calculate the overhead...
      (old-run-utime old-run-stime old-page-faults old-bytes-consed)
      (sb-impl::time-get-sys-info))

    (multiple-value-setq                ;; Do it a second time
      (old-run-utime old-run-stime old-page-faults old-bytes-consed)
      (sb-impl::time-get-sys-info))    

    (multiple-value-setq
      (new-run-utime new-run-stime new-page-faults new-bytes-consed)
      (sb-impl::time-get-sys-info))
    
    (setq run-utime-overhead   (- new-run-utime old-run-utime))
    (setq run-stime-overhead   (- new-run-stime old-run-stime))
    (setq page-faults-overhead (- new-page-faults old-page-faults))
    (setq old-real-time        (get-internal-real-time))
    (setq old-real-time        (get-internal-real-time))
    (setq new-real-time        (get-internal-real-time))
    (setq real-time-overhead   (- new-real-time old-real-time))
    (setq cons-overhead        (- new-bytes-consed old-bytes-consed))

    (multiple-value-setq                ;; Now get the initial times.
      (old-run-utime old-run-stime old-page-faults old-bytes-consed)
      (sb-impl::time-get-sys-info))    
    (setq old-real-time (get-internal-real-time))
    
    (let ((start-gc-run-time sb-impl::*gc-run-time*)) ;; Now Execute.
      (progn (funcall thunk)
        (multiple-value-setq
          (new-run-utime new-run-stime new-page-faults new-bytes-consed)
          (sb-impl::time-get-sys-info))        
        (setq new-real-time (- (get-internal-real-time) real-time-overhead))
        
        (let ((gc-run-time (max (- sb-impl::*gc-run-time* start-gc-run-time) 0)))          
          (make-instance 'timing-info
            :real-time     (max (- new-real-time old-real-time) 0.0)
            :user-time     (max (/ (- new-run-utime old-run-utime) 1000.0) 0.0)
            :system-time   (max (/ (- new-run-stime old-run-stime) 1000.0) 0.0)
            :gc-time       (float gc-run-time)
            :page-faults   (max (- new-page-faults old-page-faults) 0)
            :bytes-consed  (max (- new-bytes-consed old-bytes-consed) 0)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(describe (collect-timing (lambda ()
                            (reduce #'+
                              (loop
                                for i from 1 to 1024000
                                for j from 2048000 downto 1024000
                                collect (sqrt (/ (* i j) (+ i j))))))))

#<TIMING-INFO 1.55 seconds/247.90 MiB {10054A3D83}>
  [standard-object]

Slots with :INSTANCE allocation:
  REAL-TIME     = 1548
  USER-TIME     = 1364.296
  SYSTEM-TIME   = 181.218
  GC-TIME       = 699.0
  PAGE-FAULTS   = 7
  BYTES-CONSED  = 259941136
|#
