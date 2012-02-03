;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :dclx)


(defun symbolicate (&rest things)
  (let* ((length (reduce #'+ things
                   :key (lambda (x) (length (string x)))))
          (name (make-array length :element-type 'character)))
    (let ((index 0))
      (dolist (thing things (values (intern name)))
        (let* ((x (string thing))
                (len (length x)))
          (replace name x :start1 index)
          (incf index len))))))


(defun keywordicate (&rest things)
  (let ((*package* (find-package :keyword)))
    (apply #'symbolicate things)))


(defun ensure-list (thing)
  (if (atom thing)
    (list thing)
    thing))


(defmacro with-gensyms (symbols &body body)
  `(let ,(mapcar (lambda (symbol)
                   (let* ((symbol-name (symbol-name symbol))
                           (stem (if (every #'alpha-char-p symbol-name)
                                   symbol-name
                                   (concatenate 'string symbol-name "-"))))
                     `(,symbol (gensym ,stem))))
           symbols)
     ,@body))


(defun make-gensym-list (n &optional name)
  (when (eq t name)
    (break))
  (if name
      (loop repeat n collect (gensym (string name)))
      (loop repeat n collect (gensym))))


(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop repeat (length names) collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names
                   collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names for g in gensyms
                     collect `(,n ,g)))
             ,@body)))))


;; Example:
;; (defmacro do-primes ((var start end) &body body)
;;   (ONCE-ONLY (start end)
;;     `(do ((,var (next-prime ,start)
;;             (next-prime (1+ ,var))))
;;        ((> ,var ,end))
;;        ,@body)))


(defun :break (name &rest values)
  (break "~S = ~{~S~^, ~}" name values)
  (values-list values))


(defmacro execution-time (&body body)
  "Return the number of milliseconds it takes to execute BODY as the second value"
  (let ((tm (gensym))
        (res (gensym)))
    `(let* ((,tm (get-internal-real-time))
            (,res (progn ,@body))
            (,tm (floor (* 1000 (- (get-internal-real-time) ,tm))
                        internal-time-units-per-second)))
       (values ,res ,tm))))


#+sbcl
(defmacro with-profiling-enabled (&body body)
  `(progn
     (require :sb-sprof)
     (sb-sprof:with-profiling ()
       ,@body)))
