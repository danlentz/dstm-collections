;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :dclx)


(defmethod print-object ((tx dstm:transaction) stream)
  (print-unreadable-object (tx stream :type t :identity t)
    (with-slots (dstm::state dstm::root dstm::reads dstm::subs) tx
      (format stream "[~S] / ~A (~d subtx, ~d reads)" dstm::state
        (if (eq dstm::root tx) :ROOT-TX :sub-tx)
        (cl:length dstm::subs) (cl:length dstm::reads))))) 


(defun print-unreadable-var (thing stream)
  (check-type thing dstm::dstm-var)
  (print-unreadable-object (thing stream :type t :identity t)
    (format stream "~%  New: ~S~%  Old: ~S~%  Txn: ~S"
      (dstm::dstm-var-new thing)
      (dstm::dstm-var-old thing)
      (dstm::dstm-var-trans thing))))


(defmethod print-object ((thing dstm::dstm-var) stream)
  (print-unreadable-var thing stream))


(defmethod print-object ((thing tree:rb-tree) stream)
  (if (and dclx:*print-collections-readably* (set:typep thing))
    (typecase thing
      (seq:type (prog1 thing
                  (ord:writing-readably 
                    (format stream "[ ~{~s ~}]" (seq:list thing)))))
      (map:type (prog1 thing
                  (ord:writing-readably
                    (format stream "{| ~{~s ~}|}" (set:elements thing)))))
      (set:type (prog1 thing
                  (ord:writing-readably 
                    (format stream "{ ~{~s ~}}" (set:elements thing))))))
    (print-unreadable-object (thing stream :type t :identity t)
      (format stream "Node Value: ~s, Height: ~d"
        (quad:qbr thing)
        (quad:qdr thing)))))


(defmethod print-object ((s dclx:set*) stream)
  (if dclx:*print-collections-readably*
    (let ((val (dstm:value s)))
      (princ "#" stream)
      (print-object val stream))
    (print-unreadable-var s stream)))
    

(defmethod print-object ((s dclx:map*) stream)
  (if dclx:*print-collections-readably*
    (let ((val (dstm:value s)))
      (princ "#" stream)
      (print-object val stream))
    (print-unreadable-var s stream)))


(defmethod print-object ((s dclx:seq*) stream)
  (if dclx:*print-collections-readably*
    (let ((val (dstm:value s)))
      (princ "#" stream)
      (print-object val stream))
    (print-unreadable-var s stream)))


(defmethod print-object ((mc map::map-cell) stream)
  (prog1 mc
    (if dclx:*print-collections-readably*
      (ord:writing-readably 
        (format stream "~s" (cons (map::map-cell-key mc) (map::map-cell-val mc))))
      (print-unreadable-object (mc stream :type t :identity t)
        (format stream "key: ~s, val: ~s" (map::map-cell-key mc) (map::map-cell-val mc))))))
     


