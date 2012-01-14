;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(defpackage :dstm-collections-test
  (:shadow :set :map)
  (:use :common-lisp
    :dstm
    :bordeaux-threads
    :hu.dwim.stefil
    :hu.dwim.def
    :hu.dwim.defclass-star)
  (:export :dstm-collections :quad :ord :tree :set :map :dstm))

(in-package :dstm-collections-test)

(def (suite* e) (dstm-collections :in root-suite))

(def test check-dstm-featurep ()
  (is (find :dstm *features*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; QUAD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def (suite* e) (quad :in dstm-collections))

(def test check-protocol-class-constitutents ()
  (let ((x (make-instance 'quad:tuple)))
    (signals error (quad:qar x))
    (signals error (quad:qbr x))
    (signals error (quad:qcr x))
    (signals error (quad:qdr x))))

(def test check-sequence-type-assertions ()
  (signals error (quad:qar (list 1 2 3 4 5)))
  (signals error (quad:qar (list 1 2 3)))
  (signals error (quad:qbr #(1 2 3 4 5)))
  (signals error (quad:qbr #(1 2 3)))
  (signals error (quad:qcr "abcde"))
  (signals error (quad:qcr "abc"))
  (signals error (quad:qdr (list '(1 2) 3 4)))
  (signals error (quad:qdr (list '(1 2 3 4)))))

(def test check-list-constituent-accessors ()
  (is (eql 1 (quad:qar '(1 2 3 4))))
  (is (eql 2 (quad:qbr '(1 2 3 4))))
  (is (eql 3 (quad:qcr '(1 2 3 4))))
  (is (eql 4 (quad:qdr '(1 2 3 4)))))

(def test check-vector-constituent-accessors ()
  (is (eql 1 (quad:qar #(1 2 3 4))))
  (is (eql 2 (quad:qbr #(1 2 3 4))))
  (is (eql 3 (quad:qcr #(1 2 3 4))))
  (is (eql 4 (quad:qdr #(1 2 3 4)))))

(def test check-string-constituent-accessors ()
  (is (eql #\a (quad:qar "abcd")))
  (is (eql #\b (quad:qbr "abcd")))
  (is (eql #\c (quad:qcr "abcd")))
  (is (eql #\d (quad:qdr "abcd"))))

(def test check-abstract-quad-class-precidence ()
  (let ((x (make-instance 'quad:abstract-quad)))
    (is (typep x 'standard-object))
    (is (typep x 'quad:tuple))))

(def test check-abstract-quad-constituent-accessors ()
  (let ((x (make-instance 'quad:abstract-quad :a 1 :b 2 :c 3 :d 4)))
    (is (eql 1 (quad:qar x)))
    (is (eql 2 (quad:qbr x)))
    (is (eql 3 (quad:qcr x)))
    (is (eql 4 (quad:qdr x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def (suite* e) (ord :in dstm-collections))

(def (test :auto-call nil) check-compare-ordinality (lesser greater)
  (is (eql (ord:compare lesser  greater) -1))
  (is (eql (ord:compare greater lesser)   1))
  (is (eql (ord:compare lesser  lesser)   0))
  (is (eql (ord:compare greater greater)  0))
  (is (ord:compare< lesser greater))
  (is (not (ord:compare< greater lesser)))
  (is (ord:compare<= lesser greater))
  (is (not (ord:compare<= greater lesser)))
  (is (not (ord:|COMPARE>| lesser greater)))
  (is (ord:|COMPARE>| greater lesser))
  (is (not (ord:compare>= lesser greater)))
  (is (ord:compare>= greater lesser))
  (is (ord:compare= lesser lesser))
  (is (ord:compare= greater greater))
  (is (ord:compare<= lesser lesser))
  (is (ord:compare>= lesser lesser))
  (is (ord:compare<= greater greater))
  (is (ord:compare>= greater greater))
  (is (not (ord:compare= lesser greater)))
  (is (not (ord:compare= greater lesser))))
  
(def test check-compare-ordinality-types ()
  (progn 
    (check-compare-ordinality #\a  #\z)
    (check-compare-ordinality 1    2)
    (check-compare-ordinality 10   20)
    (check-compare-ordinality 10.0 20.0)
    (check-compare-ordinality 10   20.0)
    (check-compare-ordinality 10.0 20)
    (check-compare-ordinality  0   :x)
    (check-compare-ordinality 'x   :x)
    (check-compare-ordinality 10.0 'x)
    (check-compare-ordinality :x   10.0)
    (check-compare-ordinality "aardvark" "zebra")
    (check-compare-ordinality :aardvark :zebra)
    (check-compare-ordinality '#:aardvark '#:zebra)
    (check-compare-ordinality 'aardvark 'zebra)
    (check-compare-ordinality
      (make-instance 'standard-object)
      (make-instance 'standard-object))
    (check-compare-ordinality
      (make-instance 'quad:abstract-quad)
      (make-instance 'standard-object))
    (check-compare-ordinality
      (make-instance 'quad:abstract-quad :a 0)
      (make-instance 'quad:abstract-quad :a 10))
    (check-compare-ordinality
      (make-instance 'quad:abstract-quad :a 10 :b 10 :c 0)
      (make-instance 'quad:abstract-quad :a 10 :b 10 :c 10))
    (check-compare-ordinality
      (make-instance 'quad:abstract-quad :a 10 :b 10 :c 10 :d 0)
      (make-instance 'quad:abstract-quad :a 10 :b 10 :c 10 :d 10))
    (check-compare-ordinality
      (make-instance 'quad:abstract-quad :a 10 :b 0  :c 10 :d 10)
      (make-instance 'quad:abstract-quad :a 10 :b 10 :c 10 :d 10))  
    (check-compare-ordinality
      (make-instance 'quad:abstract-quad :a 1)
      (make-instance 'quad:abstract-quad))
    (check-compare-ordinality (find-package :common-lisp) (find-package :keyword))
    (check-compare-ordinality #p"/tmp/aardvark" #p"/tmp/zebra")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TREE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def (suite* e) (tree :in dstm-collections))

(def test check-rb-tree-class-precidence ()
  (let ((x (make-instance 'tree:rb-tree)))
    (is (typep x 'quad:abstract-quad))
    (is (typep x 'quad:tuple))
    (is (typep x 'standard-object))))

(def test check-rb-tree-type-predicate ()
  (is (tree:rb-tree-p (tree:make-rb-tree)))
  (is (not (tree:rb-tree-p (make-instance 'quad:abstract-quad))))
  (is (not (tree:rb-tree-p :x)))
  (is (tree:rb-tree-p (change-class (make-instance 'quad:abstract-quad) 'tree:rb-tree))))
  
(def test check-rb-tree-class-constituent-initargs ()
  (let ((x (tree:make-rb-tree :l :a :v :b :r :c :h :d))
         (y (tree:make-rb-tree :a :a :b :b :c :c :d :d)))
    (is (eql (slot-value x 'quad:a) (slot-value y 'quad:a)))
    (is (eql (slot-value x 'quad:b) (slot-value y 'quad:b)))
    (is (eql (slot-value x 'quad:c) (slot-value y 'quad:c)))
    (is (eql (slot-value x 'quad:d) (slot-value y 'quad:d)))))

(def test check-rb-tree-class-constituent-accessors ()
  (let ((x (tree:make-rb-tree :l :a :v :b :r :c :h :d)))
    (is (eql (tree:rb-tree-l x) :a))
    (is (eql (tree:rb-tree-v x) :b))
    (is (eql (tree:rb-tree-r x) :c))
    (is (eql (tree:rb-tree-h x) :d))))

(def test check-height-create-tree-with-subtrees ()
  (is (eql 1 (tree:height (tree:create nil 0 nil))))
  (is (eql 2 (tree:height (tree:create (tree:create nil 4 nil) 8 (tree:create nil 16 nil)))))
  (is (eql 3 (tree:height (tree:create
                            (tree:create
                              (tree:create nil 1 nil) 4 nil)
                            8
                            (tree:create nil 16
                              (tree:create nil 32 nil)))))))

(def test check-destructuring-macro-lr ()
  (tree:lr (l r) (tree:create nil 1 nil)
    (is (null l))
    (is (null r)))
  (tree:lr (l0 r0) (tree:create (tree:create nil 4 nil) 8 (tree:create nil 16 nil))
    (is (eql (tree:height l0) 1))
    (is (eql (tree:height r0) 1))
    (tree:lr (l1 r1) l0
      (is (null l1))
      (is (null r1)))
    (tree:lr (l1 r1) r0
      (is (null l1))
      (is (null r1)))))      
  
(def test check-destructuring-macro-lvr ()
  (tree:lvr (l v r) (tree:create nil 1 nil)
    (is (eql 1 v))
    (is (null l))
    (is (null r)))
  (tree:lvr (l0 v0 r0) (tree:create (tree:create nil 4 nil) 8 (tree:create nil 16 nil))
    (is (eql 8 v0))
    (is (eql (tree:height l0) 1))
    (is (eql (tree:height r0) 1))
    (tree:lvr (l1 v1 r1) l0
      (is (eql 4 v1))
      (is (null l1))
      (is (null r1)))
    (tree:lvr (l1 v1 r1) r0
      (is (eql 16 v1))
      (is (null l1))
      (is (null r1)))))

(def test check-destructuring-macro-lvrh ()
  (tree:lvrh (l v r h) (tree:create nil 1 nil)
    (is (eql 1 h))
    (is (eql 1 v))
    (is (null l))
    (is (null r)))
  (tree:lvrh (l0 v0 r0 h0) (tree:create (tree:create nil 4 nil) 8 (tree:create nil 16 nil))
    (is (eql 2 h0))
    (is (eql 8 v0))
    (is (eql (tree:height l0) 1))
    (is (eql (tree:height r0) 1))
    (tree:lvrh (l1 v1 r1 h1) l0
      (is (eql 1 h1))
      (is (eql 4 v1))
      (is (null l1))
      (is (null r1)))
    (tree:lvrh (l1 v1 r1 h1) r0
      (is (eql 1 h1))
      (is (eql 16 v1))
      (is (null l1))
      (is (null r1)))))


(def test check-balance-one-step-bal ()
  (let ((x (tree:create
             (tree:create
               (tree:create nil 1 nil) 4 nil)
             8
             (tree:create
               (tree:create nil 12 nil)
               16
               (tree:create (tree:create nil 24 nil) 32
                 (tree:create (tree:create nil 48 nil) 64
                   (tree:create nil 128 (tree:create nil 256 nil))))))))
    (is (eql 6 (tree:rb-tree-h x)))
    (is (eql 2 (tree:rb-tree-h (tree:rb-tree-l x))))
    (is (eql 5 (tree:rb-tree-h (tree:rb-tree-r x))))
    (let ((y (tree:lvr (l v r) x
               (tree:bal l v r))))
      (is (eql 16 (tree:rb-tree-v y)))
      (is (eql 5  (tree:rb-tree-h y)))
      (tree:lr (l r) y
        (is (eql 3  (tree:rb-tree-h l)))
        (is (eql 4  (tree:rb-tree-h r)))
        (is (eql 8  (tree:rb-tree-v l)))
        (is (eql 32 (tree:rb-tree-v r)))))))


(def test check-balance-join-subtrees ()
  (let* ((a    (tree:create nil 1 nil))
          (b   (tree:create nil 3 nil))
          (c   (tree:create nil 5 nil))
          (d   (tree:create nil 7 nil))
          (e   (tree:create nil 9 nil))
          (f   (tree:create c   6   d))
          (g   (tree:create f   8   e))
          (h   (tree:create b   4   g))
          (i   (tree:create a   2   h)) 
          (j   (tree:join   a   2   h)))
    (tree:lvrh (l v r h) i
      (is (eql 2 v))
      (is (eql 5 h))
      (is (eql (tree:rb-tree-v l) 1))
      (is (eql (tree:rb-tree-h l) 1))
      (is (eql (tree:rb-tree-v r) 4))
      (is (eql (tree:rb-tree-h r) 4)))
    (tree:lvrh (l v r h) j
      (is (eql 4 v))
      (is (eql 4 h))
      (tree:lvrh (l v r h) l
        (is (eql 2 v))
        (is (eql 2 h))
        (is (eql (tree:rb-tree-v l) 1))
        (is (eql (tree:rb-tree-v r) 3)))
      (tree:lvrh (l v r h) r
        (is (eql 8 v))
        (is (eql 3 h))
        (is (eql (tree:rb-tree-v r) 9))
        (is (eql (tree:rb-tree-h r) 1))
        (tree:lvrh (l v r h) l
          (is (eql 6 v))
          (is (eql 2 h))
          (is (eql (tree:rb-tree-v l) 5))
          (is (eql (tree:rb-tree-h l) 1))
          (is (eql (tree:rb-tree-v r) 7))
          (is (eql (tree:rb-tree-h r) 1)))))))
      

(def test check-structure-merged-trees ()
  (let* ((a    (tree:create nil 1  nil))
          (b   (tree:create nil 3  nil))
          (c   (tree:create a   2    b))
          (d   (tree:create nil 5  nil))
          (e   (tree:create nil 7  nil))
          (f   (tree:create nil 9  nil))
          (g   (tree:create nil 11 nil))
          (h   (tree:create d   6    e))
          (i   (tree:create f   10   g))
          (j   (tree:create h   8    i))
          (k   (tree:merge  c nil))
          (l   (tree:merge  c j)))
    (is (eql (tree:rb-tree-h k) 2))
    (is (eql (tree:rb-tree-v k) 2))
    (tree:lvrh (l v r h) l
      (is (eql 5 v))
      (is (eql 4 h))
      (tree:lvrh (l v r h) l
        (is (eql 2 v))
        (is (eql 2 h))
        (is (eql (tree:rb-tree-v l) 1))
        (is (eql (tree:rb-tree-h l) 1))
        (is (eql (tree:rb-tree-v r) 3))
        (is (eql (tree:rb-tree-h r) 1)))
      (tree:lvrh (l v r h) r
        (is (eql 8 v))
        (is (eql 3 h))
        (tree:lvrh (l v r h) l
          (is (eql 6 v))
          (is (eql 2 h))
          (is (null l))
          (is (eql (tree:rb-tree-v r) 7))
          (is (eql (tree:rb-tree-h r) 1)))
        (tree:lvrh (l v r h) r
          (is (eql 10 v))
          (is (eql 2 h))
          (is (eql (tree:rb-tree-v l) 9))
          (is (eql (tree:rb-tree-h l) 1))
          (is (eql (tree:rb-tree-v r) 11))
          (is (eql (tree:rb-tree-h r) 1)))))))

(def test check-structure-concat-trees ()
  (let* ((a    (tree:create nil 1  nil))
          (b   (tree:create nil 2  nil))
          (c   (tree:create nil 3  nil))
          (d   (tree:create nil 7  nil))
          (e   (tree:create nil 9  nil))
          (f   (tree:create nil 11 nil))
          (g   (tree:create nil 13 nil))
          (h   (tree:create b   4    c))
          (i   (tree:create h   6    d))
          (j   (tree:create f   12   g))
          (k   (tree:create e   10   j))
          (l   (tree:create i   8    k))
          (m   (tree:concat a l)))
    (is (eql (tree:rb-tree-v l) 8))
    (is (eql (tree:rb-tree-h l) 4))
    (tree:lvrh (l v r h) m
      (is (eql 8 v))
      (is (eql 5 h))
      (tree:lvrh (l v r h) l
        (is (eql 2 v))
        (is (eql 4 h))
        (is (eql (tree:rb-tree-v l) 1))
        (is (eql (tree:rb-tree-h l) 1))
        (tree:lvrh (l v r h) r
          (is (eql 6 v))
          (is (eql 3 h))
          (is (eql (tree:rb-tree-v r) 7))
          (is (eql (tree:rb-tree-h r) 1))
          (tree:lvrh (l v r h) l
            (is (eql 4 v))
            (is (eql 2 h))
            (is (null l))
            (is (eql (tree:rb-tree-v r) 3))
            (is (eql (tree:rb-tree-h r) 1)))))
      (tree:lvrh (l v r h) r
        (is (eql 10 v))
        (is (eql 3 h))
        (is (eql (tree:rb-tree-v l) 9))
        (is (eql (tree:rb-tree-h l) 1))
        (tree:lvrh (l v r h) r
          (is (eql 12 v))
          (is (eql 2 h))
          (is (eql (tree:rb-tree-v l) 11))
          (is (eql (tree:rb-tree-h l) 1))
          (is (eql (tree:rb-tree-v r) 13))
          (is (eql (tree:rb-tree-h r) 1)))))))
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def (suite* e) (set :in dstm-collections))


(def test check-construct-set-sequentially ()
  (let (integer-set)
    (loop
      :for i :from 1 :to 64
      :do (progn
            (setf integer-set (set:add i integer-set))
            (is (eql (set:min-elt  integer-set) 1))
            (is (eql (set:max-elt  integer-set) i))
            (is (eql (set:cardinal integer-set) i))))))


(def function make-integer-set (size)
  (let (integer-set)
    (loop
      :for i :from 1 :to size
      :do (progn
            (setf integer-set (set:add i integer-set))))
    integer-set))


(def test check-destruct-set-sequentially ()
  (let (integer-set)
    (loop
      :for i :from 1 :to 64
      :do (setf integer-set (set:add i integer-set)))
    (loop
      :for i :from 1 :to 64
      :do (progn
            (setf integer-set (set:remove-min-elt integer-set))
            (is (eql (set:cardinal integer-set) (- 64 i)))
            (is (not (set:mem i integer-set)))))
    (is (null integer-set))))


(def test check-construct-set-randomly ()
  (let (random-set random-list)
    (loop
      :for i :from 1 :to 256
      :do (progn
            (let ((x (random 16356)))
              (when (not (find x random-list))
                (push x random-list)
                (setf random-set (set:add x random-set))
                (is (eql (length random-list) (set:cardinal random-set)))))))
    (dolist (elem random-list)
      (is (set:mem elem random-set)))))

(def test check-destruct-set-randomly ()
  (let (random-set random-list)
    (loop
      :for i :from 1 :to 256
      :do (progn
            (let ((x (random 16356)))
              (when (not (find x random-list))
                (push x random-list)
                (setf random-set (set:add x random-set))))))
    (is (eql (length random-list) (set:cardinal random-set)))
    (dolist (elem (reverse random-list))
      (setf random-set (set:remove elem random-set))
      (is (not (set:mem elem random-set))))
    (is (eql (set:empty) random-set))))


(def function make-random-set (size)
  (let (random-set random-list)
    (loop
      :for i :from 1 :to size
      :do (progn
            (let ((x (random 16356)))
              (when (not (find x random-list))
                (push x random-list)
                (setf random-set (set:add x random-set))))))
    random-set))


(def test check-union-disjoint-integer-sets ()
  (let (set0 set1)
    (loop
      :for i :from 1  :to 64
      :for j :from 65 :to 128
      :do (progn
            (setf set0 (set:add i set0))
            (setf set1 (set:add j set1))))
    (is (eql 64 (set:cardinal set0)))
    (is (eql 64 (set:cardinal set1)))
    (loop
      :for i :from 1 :to 128
      :do (is (set:mem i (set:union set0 set1))))))


(def test check-union-identical-integer-sets ()
  (let (set0 set1)
    (loop
      :for i :from 1 :to 128
      :do (progn
            (setf set0 (set:add i set0))
            (setf set1 (set:add i set1))))
    (is (eql 128 (set:cardinal set0)))
    (is (eql 128 (set:cardinal set1)))
    (is (eql 128 (set:cardinal (set:union set0 set1))))
    (loop
      :for i :from 1 :to 128
      :do (is (set:mem i (set:union set0 set1))))))
    

(def test check-union-conjoint-integer-sets ()
  (let (set0 set1)
    (loop
      :for i :from 1 :to 96
      :do (setf set0 (set:add i set0)))
    (is (eql 96 (set:cardinal set0)))
    (loop
      :for i :from 1 :to 128
      :do (setf set1 (set:add i set1)))
    (is (eql 128 (set:cardinal set1)))
    (is (eql 128 (set:cardinal (set:union set0 set1))))
    (loop
      :for i :from 1 :to 128
      :do (is (set:mem i (set:union set0 set1))))))


(def test check-inter-disjoint-integer-sets ()
  (let (set0 set1)
    (loop
      :for i :from 1  :to 64
      :for j :from 65 :to 128
      :do (progn
            (setf set0 (set:add i set0))
            (setf set1 (set:add j set1))))
    (is (eql 64 (set:cardinal set0)))
    (is (eql 64 (set:cardinal set1)))
    (is (null (set:inter set0 set1)))))


(def test check-inter-identical-integer-sets ()
  (let (set0 set1)
    (loop
      :for i :from 1 :to 128
      :do (progn
            (setf set0 (set:add i set0))
            (setf set1 (set:add i set1))))
    (is (eql 128 (set:cardinal set0)))
    (is (eql 128 (set:cardinal set1)))
    (is (eql 128 (set:cardinal (set:inter set0 set1))))
    (loop
      :for i :from 1 :to 128
      :do (is (set:mem i (set:inter set0 set1))))))


(def test check-inter-conjoint-integer-sets ()
  (let (set0 set1)
    (loop
      :for i :from 1 :to 96
      :do (setf set0 (set:add i set0)))
    (is (eql 96 (set:cardinal set0)))
    (loop
      :for i :from 1 :to 128
      :do (setf set1 (set:add i set1)))
    (is (eql 128 (set:cardinal set1)))
    (is (eql 96 (set:cardinal (set:inter set0 set1))))
    (loop
      :for i :from 1 :to 96
      :do (is (set:mem i (set:inter set0 set1))))))


(def test check-iter-random-set ()
  (let (new-list random-set random-list)
    (loop
      :for i :from 1 :to 128
      :do (progn
            (let ((x (random 16356)))
              (when (not (find x random-list))
                (push x random-list)
                (setf random-set (set:add x random-set))))))    
    (set:iter #'(lambda (x) (push x new-list)) random-set)
    (is (eql (length new-list) (length random-list)))
    (is (eql (length new-list) (set:cardinal random-set)))                                        
    (dolist (x new-list)
      (is (not (null (find x random-list))))
      (is (not (null (set:mem x random-set)))))))

(def test check-subset-true-integer-sets ()
  (let (integer-set0 integer-set1)
    (loop
      :for i :from 1 :to 128
      :do (progn
            (setf integer-set0 (set:add i integer-set0))
            (setf integer-set1 (make-integer-set 128))
            (is (set:subset integer-set0 integer-set1))))))


(def test check-subset-false-integer-sets ()
  (let (integer-set0 integer-set1)
    (loop
      :for i :from 129 :to 192
      :do (progn
            (setf integer-set0 (set:add i integer-set0))
            (setf integer-set1 (make-integer-set 128))
            (is (not (set:subset integer-set0 integer-set1)))))
    (loop
      :for i :from -129 :to 0
      :do (progn
            (setf integer-set0 (set:add i integer-set0))
            (setf integer-set1 (make-integer-set 128))
            (is (not (set:subset integer-set0 integer-set1)))))

    ))

(def test check-subset-true-random-sets ()
  (let (random-set0 random-set1 random-list length)
    (loop
      :for i :from 1 :to 128
      :do (progn
            (let ((x (random 16356)))
              (when (not (find x random-list))
                (push x random-list)
                (setf random-set0 (set:add x random-set0))))
            (setf length (length random-list))
            (loop
              :for j :from 1 :to (mod length 8)
              :do (let ((sseq (subseq random-list 0 j)))
                    (loop
                      :for k :in sseq
                      :do (progn
                            (setf random-set1 (set:add k random-set1))
                            (is (not (null (set:subset random-set1 random-set0))))))))))))
               

(def test check-for-all-some ()
  (let (integer-list (integer-set (make-integer-set 128)))
    (set:for-all #'oddp integer-set
      #'(lambda (x)
          (push x integer-list)))
    (loop
      :for i :from  1 :to 128
      :do (when (oddp i)
            (is (not (null (find i integer-list))))))))


(def test check-for-all-every ()
  (let (integer-list (integer-set (make-integer-set 128)))
    (set:for-all #'integerp integer-set
      #'(lambda (x)
          (push x integer-list)))
    (loop
      :for i :from  1 :to 128
      :do (when (integerp i)
            (is (not (null (find i integer-list))))))))


(def test check-exists-every ()
  (let ((set0 (make-integer-set 16)))
    (is (set:exists #'integerp set0))))


(def test check-exists-some ()
  (let* ((set0 (make-integer-set 16))
          (set0 (set:add 0 set0)))
    (is (set:exists #'zerop set0))))


(def test check-exists-none ()
  (let ((set0 (make-integer-set 16)))
    (is (not (set:exists #'zerop set0)))))


(def test check-filter-some ()
  (let* ((set0 (make-integer-set 128))
          (set1 (set:filter #'oddp set0)))
    (loop
      :for i :from 1 :to 128
      :do (when (oddp i)
            (is (set:mem i set1))))))


(def test check-partition-some ()
  (let* ((set0 (make-integer-set 128))
          (set-list1 (set:partition #'oddp set0)))
    (loop
      :for i :from 1 :to 128
      :do (when (oddp i)
            (is (set:mem i (first set-list1)))
            (is (null (set:mem i (second set-list1))))
            ))))


(def test check-partition-none ()
  (let* ((set0 (make-integer-set 128))
          (set-list1 (set:partition #'zerop set0)))
    (loop
      :for i :from 1 :to 128
      :do (when (zerop i)
            (is (set:mem i (first set-list1)))
            (is (null (set:mem i (second set-list1))))
            ))))


(def test check-partition-every ()
  (let* ((set0 (make-integer-set 128))
          (set-list1 (set:partition #'integerp set0)))
    (loop
      :for i :from 1 :to 128
      :do (when (integerp i)
            (is (set:mem i (first set-list1)))
            (is (null (set:mem i (second set-list1))))
            ))))


(def test check-equal-randomly-constructed-sets ()
  (let (random-set0 random-set1 random-list)
    (loop
      :for i :from 1 :to 256
      :do (progn
            (let ((x (random 16356)))
              (when (not (find x random-list))
                (push x random-list)
                (setf random-set0 (set:add x random-set0))))))
    (is (eql (length random-list) (set:cardinal random-set0)))
    (dolist (elem random-list)
      (setf random-set1 (set:add elem random-set1)))    
    (is (set:equal random-set0 random-set1))
    (dolist (elem random-list)
      (setf random-set0 (set:remove elem random-set0))
      (setf random-set1 (set:remove elem random-set1))   
      (is (set:equal random-set0 random-set1)))
    (is (null (or random-set0 random-set1)))))
    

(def test check-compare-set-ordinality ()
  (let ((random-set (make-random-set 128))
         (integer-set (make-integer-set 128)))
    (is (eql  0 (set:compare random-set  random-set)))
    (is (eql  0 (set:compare integer-set  integer-set)))
    (is (eql  0 (set:compare nil         nil)))
    (check-compare-ordinality (set:empty)                       random-set)
    (check-compare-ordinality random-set  (set:remove-min-elt   random-set))
    (check-compare-ordinality integer-set (set:remove-min-elt   integer-set))
    (check-compare-ordinality (set:remove-max-elt random-set)   random-set)
    (check-compare-ordinality (set:remove-max-elt integer-set)  integer-set)
    (check-compare-ordinality (set:singleton 0) integer-set)
    (check-compare-ordinality (set:singleton 1) integer-set)
    (tree:lr (l r) integer-set
      (is (eql  0 (set:compare l l)))
      (is (eql  0 (set:compare r r)))
      (check-compare-ordinality l r)
      (check-compare-ordinality l integer-set)
      (check-compare-ordinality integer-set r))
    (tree:lr (l r) random-set
      (is (eql  0 (set:compare l l)))
      (is (eql  0 (set:compare r r)))
      (check-compare-ordinality l r)
      (check-compare-ordinality l random-set)
      (check-compare-ordinality random-set r))))     
  

(def test check-elements-random-set ()
  (let (random-set random-list)
    (loop
      :for i :from 1 :to 128
      :do (progn
            (let ((x (random 16356)))
              (when (not (find x random-list))
                (push x random-list)
                (setf random-set (set:add x random-set))))))
    (dolist (x random-list)
      (is (find x (set:elements random-set))))))




#|
#+xxxxx
(def test check-diff-disjoint-integer-sets ()
  (let (set0 set1)
    (loop
      :for i :from 1  :to 128
      :for j :from 65 :to 128
      :do (progn
            (setf set0 (set:add i set0))
            (setf set1 (set:add j set1))))
    (is (eql 64 (set:cardinal set0)))
    (is (eql 64 (set:cardinal set1)))
    (is (eql 128 (set:cardinal (set:diff set1 set0)))))
|#   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def (suite* e) (map :in dstm-collections))

(def test check-integer-map-contents ()
  (let (integer-map)
    (loop
      :for i :from 1 :to 128
      :do (progn
            (setf integer-map (map:add i (* 2 i) integer-map))))
    (loop
      :for i :from 1 :to 128
      :do (is (eql (* 2 i) (map:find i integer-map))))))


(def test check-symbol-map-contents ()
  (let* (symbol-map
          (symbol-list (list :one :two :three :four :five :six :seven :eight)))
    (dolist (sym symbol-list)
      (setf symbol-map (map:add sym sym symbol-map)))
    (dolist (sym symbol-list)
      (is (map:find sym symbol-map)))))


(def test check-map-integer-map ()
  (let (integer-map)
    (loop
      :for i :from 1 :to 128
      :do (progn
            (setf integer-map (map:add i (* 2 i) integer-map))))
    (loop
      :for i :from 1 :to 128
      :do (is (eql (* 4 i) (map:find i (map:map #'(lambda (x) (* 2 x)) integer-map)))))))

  
(def test check-mapi-integer-map ()
  (let (integer-map)
    (loop
      :for i :from 1 :to 128
      :do (progn
            (setf integer-map (map:add i (* 2 i) integer-map))))
    (loop
      :for i :from 1 :to 128
      :do (is (eql (* 2 i) (map:find (* 2 i) (map:mapi #'(lambda (x) (* 2 x)) integer-map)))))))


(def test check-iter-integer-map ()
  (let (integer-map (hash-table (make-hash-table)))
    (loop
      :for i :from 1 :to 128
      :do (progn
            (setf integer-map (map:add i (* 2 i) integer-map))))
    (map:iter #'(lambda (k v) (setf (gethash k hash-table) v)) integer-map )
    (dolist (cell (set:elements integer-map))
      (is (eql (gethash (map::map-cell-key cell) hash-table) (map::map-cell-val cell))))))
      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SEQ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def (suite* e) (seq :in dstm-collections))


(def test check-collection-types-and-type-predicates ()
  (is  (seq:typep  (set:singleton (seq::make-seq-cell))))
  (is  (tree:typep (set:singleton (seq::make-seq-cell))))
  (is  (set:typep  (set:singleton (seq::make-seq-cell))))
  (is  (map:typep  (set:singleton (seq::make-seq-cell))))

  (is  (cl:typep (set:singleton (seq::make-seq-cell))   'seq:type))
  (is  (cl:typep (set:singleton (seq::make-seq-cell))   'tree:type))
  (is  (cl:typep (set:singleton (seq::make-seq-cell))   'map:type))
  (is  (cl:typep (set:singleton (seq::make-seq-cell))   'set:type))

  (is  (not (seq:typep  (set:singleton (map::make-map-cell)))))
  (is  (tree:typep (set:singleton (map::make-map-cell))))
  (is  (set:typep  (set:singleton (map::make-map-cell))))
  (is  (map:typep  (set:singleton (map::make-map-cell))))

  (is  (not (cl:typep (set:singleton (map::make-map-cell))   'seq:type)))
  (is  (cl:typep (set:singleton (map::make-map-cell))        'tree:type))
  (is  (cl:typep (set:singleton (map::make-map-cell))        'map:type))
  (is  (cl:typep (set:singleton (map::make-map-cell))        'set:type))

  (is  (set:typep       nil))
  (is  (map:typep       nil))
  (is  (seq:typep       nil))
  (is  (not (tree:typep nil)))

  (is  (tree:typep      (set:singleton 0)))
  (is  (set:typep       (set:singleton 0)))
  (is  (not (map:typep  (set:singleton 0))))
  (is  (not (seq:typep  (set:singleton 0))))

  (is  (not (cl:typep (set:singleton 5)            'seq:type)))
  (is  (not (cl:typep (set:singleton 5)            'map:type)))
  (is  (cl:typep (set:singleton 5)                 'tree:type))
  (is  (cl:typep (set:singleton 5)                 'set:type)))


#|

(seq:create)
(seq:list (seq:create t))
(seq:list (seq:create '(1 2 3 4 5)))
(seq:list (seq:create (seq:create '(1 2 3 4 5))))
(seq:list (seq:create (seq:create #(1 2 3 4 5))))
(seq:list (seq:create (seq:create "bababooey")))
(seq:list (seq:create (set:add 5 (set:add 9 nil))))
(seq:list (seq:concat
            (push 1 (push 2 (push 3)))
            (seq:map #'- (push 1 (push 2 (push 3))))
            (seq:dup (seq:map #'princ-to-string (push 1 (push 2 (push 3)))))))



(is (eql 0 (seq:compare (seq:create '(1 2 3 4 5)) (seq:create '(1 2 3 4 5)))))
(is (seq:equal (seq:create '(1 2 3 4 5)) (seq:create '(1 2 3 4 5))))
(is (eql 1 (seq:compare (seq:create '(2 3 4 5)) (seq:create '(1 2 3 4 5)))))
(is (eql -1 (seq:compare (seq:create '(1 2 3 4 5)) (seq:create '(2 3 4 5)))))
(is (eql 1 (seq:compare (seq:create '(10 20 30 40 50)) (seq:create '(1 2 3 4 5)))))
(is (eql -1 (seq:compare  (seq:create '(1 2 3 4 5)) (seq:create '(10 20 30 40 50)))))


|#
                

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DSTM 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def (suite* e) (dstm :in dstm-collections))

(defun show-rolls (&optional (duration 1))
   (let ((pcnt (/ (aref dstm::*nrolls* 0) (aref dstm::*ntrans* 0) 0.01))
         (rate (/ (aref dstm::*ntrans* 0) duration)))
     (list :rollbacks (aref dstm::*nrolls* 0)
           :transactions (aref dstm::*ntrans* 0)
           :percent-rollbacks pcnt
           :trans-per-roll (if (zerop pcnt) :infinite (* 100 (/ pcnt)))
           :duration duration
           :trans-per-sec  rate)))


(defvar *a* nil)
(defvar *b* nil)


#+()
(defixture tx-var-a
  (let ((*a* (create-var 0)))
    (-body-)))
#+()
(defixture tx-var-b
  (let ((*b* (create-var 0)))
    (-body-)))


(defun check-invariant (&aux a b)
  (atomic
    (setf
      a (read-var *a*)
      b (read-var *b*)))
  (let ((result (= b (* 2 a))))
    (unless result
      (error  "Invariant broken: A = ~A, B = ~A" a b))))



(defun common-code (delta)
  (atomic
    (let ((a (+ delta (read-var *a*))))
      (write-var *a* a)
      (write-var *b* (* 2 a)))))


(defun count-up (n)
  (lambda ()
    (loop :for i :from 1 :to n
      :do (progn
            (when (zerop (mod i 10000))
              (princ "+" *trace-output*))
            (common-code 1)))))


(defun count-down (n)
  (lambda ()
    (loop :for i :from 1 :to n
      :do (progn
            (when (zerop (mod i 10000))
              (princ "-" *trace-output*))
            (common-code -1)))))


(defun test-dstm-contention (iterations)
  (format *trace-output* "~%Start DSTM Test/~A...~%" iterations)

  (setf *a* (create-var 0))
  (setf *b* (create-var 0))
  (reset)

  (let ((start (local-time:now))
         (procs
           (list
             (bt:make-thread (funcall #'count-down iterations) :name "down")
             (bt:make-thread (funcall #'count-up   iterations) :name "up"))))

    (loop
      :while (some #'sb-thread:thread-alive-p procs)
      :do    (check-invariant))

    
    (let ((stop (local-time:now)))
      (princ (show-rolls (* 1e-6 (local-time:timestamp-difference stop start)))
        *trace-output*)))

  (values))


(def test check-dstm/1-million-transactions ()
  (finishes
    (test-dstm-contention 500000)))


(def test check-dstm/2-million-transactions ()
  (finishes
    (test-dstm-contention 1000000)))


(def test check-dstm/10-million-transactions ()
  (finishes
    (test-dstm-contention 5000000)))


;; Example Test Run:

;; Start DSTM Test/500000...
;; (ROLLBACKS 20 TRANSACTIONS 1136254 PERCENT-ROLLBACKS 0.0017601699 TRANS-PER-ROLL 56812.703
;;  DURATION 3.8240682903451554d-5 TRANS-PER-SEC 2.971322460084632d10).
;; #<test-run: 1 test, 1 assertion, 0 failures in 38.179 sec>

;; Start DSTM Test/1000000...
;; (ROLLBACKS 66 TRANSACTIONS 2822540 PERCENT-ROLLBACKS 0.0023383193 TRANS-PER-ROLL 42765.758
;;  DURATION 9.261683476616496d-5 TRANS-PER-SEC 3.0475453054795372d10).
;; #<test-run: 1 test, 1 assertion, 0 failures in 92.612 sec>

;; Start DSTM Test/5000000...
;; (ROLLBACKS 271 TRANSACTIONS 14450804 PERCENT-ROLLBACKS 0.0018753281 TRANS-PER-ROLL 53324.0 
;;  DURATION 4.699111058135885d-4 TRANS-PER-SEC 3.075220785637819d10).
;; #<test-run: 1 test, 1 assertion, 0 failures in 471.979 sec>

