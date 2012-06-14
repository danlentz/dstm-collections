;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :mmap)

(defcstruct* mmap-tuple
  (mmap-tuple-x :uint)
  (mmap-tuple-l :uint)
  (mmap-tuple-r :uint)
  (mmap-tuple-k :uint)
  (mmap-tuple-v :uint))


(defun allocate-mmap-tuple (place &key (k nil) (v nil) (l nil) (r nil) (x 0) &aux tuple-mmptr)
  (flet ((node-ref (thing)
          (typecase thing
            (null    0)
            (integer thing)
            (mmptr   (mmptr-offset thing))))
          (value-ref (thing)
            (if (null thing)
              0
              (mmptr-offset (ref thing)))))
    (prog1 (setf tuple-mmptr (mmalloc place 'mmap-tuple))
      (let ((tuple (mem-ref (mmptr->ptr tuple-mmptr) 'mmap-tuple)))
        (setf (mmap-tuple-x tuple) x)
        (setf (mmap-tuple-k tuple) (value-ref k))
        (setf (mmap-tuple-v tuple) (value-ref v))
        (setf (mmap-tuple-l tuple) (node-ref  l))
        (setf (mmap-tuple-r tuple) (node-ref  r))))))


(defun null-mmap-tuple (place)
  (allocate-mmap-tuple place))


(defgeneric null-mmap-tuple-p (thing)
  (:method (default)
    (zerop (pointer-address default)))
  (:method ((val null))
    t)
  (:method ((tuple mmap-tuple))
    (zerop (mmap-tuple-x tuple)))
  (:method ((mmptr mmptr))
    (or (zerop (mmptr-offset mmptr))
      (and (eq (mmptr-type mmptr) 'mmap-tuple)
        (null-mmap-tuple-p (mem-ref (mmptr->ptr mmptr) 'mmap-tuple))))))


