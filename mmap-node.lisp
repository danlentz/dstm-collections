;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :tree)


(deflayer storage ())

(deflayer memory  (storage))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manardb MMAP/MOP Persistence Layer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deflayer manardb (storage)
  ((base-directory
     :initarg  :base-directory
     :reader   base-directory
     :initform #p"/tmp/mmlayer/"))) 

(ensure-active-layer 'manardb)

(manardb:use-mmap-dir
  (base-directory (find-layer 'manardb))
  :if-does-not-exist :create)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Memory Mapped Vector persistent node 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(manardb:defmmclass mmap-node (manardb:marray)
  ()
  (manardb::walker manardb::walk-array))


(defmethod pointer:deref ((node mmap-node) &optional type &rest args)
  (declare (ignore type args))
  (let ((retrieved-content (manardb:marray-to-list node)))
    (coerce retrieved-content 'vector)))


(define-layered-method make-node :in-layer manardb (k v l r x &optional
                                                     (allocator 'mmap-node) &rest args)
  (declare (ignore args))
  (manardb:make-marray 6 :initial-contents (list 'node k v l r x) :marray-class allocator))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Memory Mapped Vector with Lazy Cache
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deflayer manardb/cache (manardb))


(manardb:defmmclass mmap/cache-node (mmap-node)
  ((content :reader node-content :persistent nil))
  (manardb::walker manardb::walk-array))


(defmethod slot-unbound (class (instance mmap/cache-node) (slot-name (eql 'content)))
  ;;  (log:debug "caching [%ptr ~D]" (mptr instance)) (printv "caching" instance)
  (let ((retrieved-content (manardb:marray-to-list instance)))
    (setf (slot-value instance slot-name) (coerce retrieved-content 'vector))))


(defmethod pointer:deref ((node mmap/cache-node) &optional type &rest args)
  (declare (ignore type args))
  (node-content node))


(define-layered-method make-node :in-layer manardb/cache (k v l r x &optional
                                                           (allocator 'mmap/cache-node) &rest args)
  (declare (ignore args))
  (manardb:make-marray 6 :initial-contents (list 'node k v l r x) :marray-class allocator))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

(flet ((ok (m)
         (assert (equalp (pointer:deref m) (vector 'node :k :v :l :r :x)))
         (assert (eq :k (node/k m)))
         (assert (eq :v (node/v m)))
         (assert (eq :l (node/l m)))
         (assert (eq :r (node/r m)))
         (assert (eq :x (node/x m)))
         (assert (equal '(:k :v) (node/kv m)))
         (assert (equal '(:l :r) (node/lr m)))
         (assert (equal '(:k :v :l :r) (node/kvlr m)))
         (assert (equal '(:k :v :l :r :x) (node/kvlrx m)))
         (assert (equal '(:k :v :l :r :x) (node/constituents m)))
         (assert (equal '(:k :v :l :r :x) (multiple-value-list (node/values m))))))
  (ok (vector 'node :k :v :l :r :x))
  (ok (make-array 6
        :initial-contents (list 'node :k :v :l :r :x)))
  (ok (with-inactive-layers (manardb)
        (make-node :k :v :l :r :x)))
  (ok (manardb:make-marray 6
        :marray-class 'mmap-node
        :initial-contents (list 'node :k :v :l :r :x)))
  (ok (with-active-layers (manardb)
        (with-inactive-layers (manardb/cache)
          (make-node :k :v :l :r :x))))
  (ok (manardb:make-marray 6
        :marray-class 'mmap/cache-node
        :initial-contents (list 'node :k :v :l :r :x)))
  (ok (with-active-layers (manardb/cache)
        (make-node :k :v :l :r :x))))
  
|#
