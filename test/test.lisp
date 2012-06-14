;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(defpackage :dstm-collections-test
;  (:shadow :set :map)
  (:shadowing-import-from :closer-mop :standard-generic-function :defgeneric :defmethod)
  (:use :common-lisp :collex :tree :contextl :closer-mop
    :named-readtables
    :hu.dwim.stefil
    :hu.dwim.def
    :hu.dwim.defclass-star))


(in-package :dstm-collections-test)
;;(in-readtable dclx:standard-syntax)

(def (suite* e) (dstm-collections :in root-suite))

;; (def test check-dstm-featurep ()
;;   (is (find :dstm *features*)))

(def test check-cstm-featurep ()
  (is (find :cstm *features*)))

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
#+()    (check-compare-ordinality
      (make-instance 'standard-object)
      (make-instance 'standard-object))
    (check-compare-ordinality (find-package :common-lisp) (find-package :keyword))
    (check-compare-ordinality #p"/tmp/aardvark" #p"/tmp/zebra"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TREE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def (suite* e) (tree :in dstm-collections))


(def test check-simple-constituent-accessors ()
  (let ((x (node :k :v :l :r)))
    (is (eql (node/k x) :k))
    (is (eql (node/v x) :v))
    (is (eql (node/l x) :l))
    (is (eql (node/r x) :r)))
  (with-active-layers (balanced)
    (let ((x (node :k :v :l :r :x)))
      (is (eql (node/k x) :k))
      (is (eql (node/v x) :v))
      (is (eql (node/l x) :l))
      (is (eql (node/r x) :r))  
      (is (eql (node/x x) :x))))
  (with-active-layers (weight-balanced)
    (let ((x (node :k :v :l :r :s)))
      (is (eql (node/k x) :k))
      (is (eql (node/v x) :v))
      (is (eql (node/l x) :l))
      (is (eql (node/r x) :r))  
      (is (eql (node/s x) :s))))
  (with-active-layers (height-balanced)
    (let ((x (node :k :v :l :r :h)))
      (is (eql (node/k x) :k))
      (is (eql (node/v x) :v))
      (is (eql (node/l x) :l))
      (is (eql (node/r x) :r))  
      (is (eql (node/h x) :h)))))


(def test check-compound-constituent-accessors ()
  (with-active-layers (t)
    (let ((x (node :k :v :l :r :x)))
      (is (equalp (node/kv x)            (list :k :v)))
      (is (equalp (node/lr x)            (list :l :r)))
      (is (equalp (node/kvlr x)          (list :k :v :l :r)))
      (is (equalp (node/kvlrx x)         (list :k :v :l :r :x)))
      (is (equalp (node/constituents x)  (list :k :v :l :r :x)))))
  (with-active-layers (weight-balanced)
    (let ((x (node :k :v :l :r :s)))
      (is (equalp (node/kv x)            (list :k :v)))
      (is (equalp (node/lr x)            (list :l :r)))
      (is (equalp (node/kvlr x)          (list :k :v :l :r)))
      (is (equalp (node/kvlrs x)         (list :k :v :l :r :s)))
      (is (equalp (node/constituents x)  (list :k :v :l :r :s)))))
  (with-active-layers (height-balanced)
    (let ((x (node :k :v :l :r :h)))
      (is (equalp (node/kv x)            (list :k :v)))
      (is (equalp (node/lr x)            (list :l :r)))
      (is (equalp (node/kvlr x)          (list :k :v :l :r)))
      (is (equalp (node/kvlrh x)         (list :k :v :l :r :h)))
      (is (equalp (node/constituents x)  (list :k :v :l :r :h))))))


(defmacro with-instances/wb (&body body)
  `(with-active-layers (weight-balanced)
    (symbol-macrolet ((_1    (node/singleton (gensym)))
                       (_3   (node/create    (gensym) t _1  _1))
                       (_5   (node/create    (gensym) t _3  _1))
                       (_7   (node/create    (gensym) t _3  _3))
                       (_11  (node/create    (gensym) t _3  _7)) 
                       (_15  (node/create    (gensym) t _7  _7))
                       (_23  (node/create    (gensym) t _15 _7))
                       (_27  (node/create    (gensym) t _15 _11))
                       (_31  (node/create    (gensym) t _15 _15))
                       (_39  (node/create    (gensym) t _15 _23))
                       (_51  (node/create    (gensym) t _23 _27))
                       (_63  (node/create    (gensym) t _31 _31))
                       (_127 (node/create    (gensym) t _63 _63)))
      ,@body)))


(def test check-simple-constructors/wb ()
  (with-active-layers (weight-balanced)
    (with-instances/wb ()
      (is (= 0 (node/size   (empty))))
      (is (= 1 (node/weight (empty))))
      (is (= 1 (node/size   (node/singleton :k :v))))
      (is (= 2 (node/weight (node/singleton :k :v))))
      (is (= 1 (node/size   (node/create :k :v nil nil))))
      (is (= 2 (node/weight (node/create :k :v nil nil))))
      (is (= 3  (node/size   _3)))
      (is (= 4  (node/weight _3)))
      (is (= 7  (node/size   _7)))
      (is (= 8  (node/weight _7)))
      (is (= 15 (node/size   _15)))
      (is (= 16 (node/weight _15)))
      (is (= 31 (node/size   _31)))
      (is (= 32 (node/weight _31)))
      (is (= 63 (node/size   _63)))
      (is (= 64 (node/weight _63))))))


(def test check-simple-constructors/rb ()
  (with-active-layers (height-balanced)
    (is (= 0 (node/height (empty))))
    (is (= 1 (node/height (node/singleton :k :v))))
    (is (= 1 (node/height (node/create :k :v nil nil))))
    (symbol-macrolet ((_1   (node/singleton (gensym)))
                       (_3  (node/create  (gensym) t _1 _1))
                       (_7  (node/create  (gensym) t _3 _3))
                       (_15 (node/create  (gensym) t _7 _7)))
      (is (= 2  (node/height _3)))
      (is (= 3  (node/height _7)))
      (is (= 4  (node/height _15))))))


(def test check-rotation-wb/single-l ()
  (with-active-layers (weight-balanced)
    (labels ((matches (n1 n2)
               (or (if (empty? n1) (is (empty? n2)))
                 (progn
                   (is (eq  (node/k n1) (node/k n2)))
                   (is (eq  (node/v n1) (node/v n2)))
                   (is (eql (node/s n1) (node/s n2)))
                   (matches (node/l n1) (node/l n2))
                   (matches (node/r n1) (node/r n2))))))
      (matches (tree::single-l :AK :AV
                 (node :XK :XV () () 1)
                 (node :BK :BV (node :YK :YV () () 1) (node :ZK :XZ () () 1) 3))
        (node :BK :BV
          (node :AK :AV (node :XK :XV () () 1) (node :YK :YV () () 1) 3)
          (node :ZK :XZ () () 1) 5)))))


(def test check-rotation-wb/double-l ()
  (with-active-layers (weight-balanced)
    (labels ((matches (n1 n2)
               (or (if (empty? n1) (is (empty? n2)))
                 (progn
                   (is (eq  (node/k n1) (node/k n2)))
                   (is (eq  (node/v n1) (node/v n2)))
                   (is (eql (node/s n1) (node/s n2)))
                   (matches (node/l n1) (node/l n2))
                   (matches (node/r n1) (node/r n2))))))
      (matches (tree::double-l :AK :AV
                 (node :XK :XV () () 1)
                 (node :CK :CV
                   (node :BK :BV (node :Y1K :Y1V () () 1) (node :Y2K :Y2V () () 1) 3)
                   (node :ZK :ZV () () 1) 5))
        (node :BK :BV
          (node :AK :AV (node :XK :XV () () 1) (node :Y1K :Y1V () () 1) 3)
          (node :CK :CV (node :Y2K :Y2V () () 1) (node :ZK :ZV () () 1) 3) 7)))))


(def test check-rotation-wb/single-r ()
  (with-active-layers (weight-balanced)
    (labels ((matches (n1 n2)
               (or (if (empty? n1) (is (empty? n2)))
                 (progn
                   (is (eq  (node/k n1) (node/k n2)))
                   (is (eq  (node/v n1) (node/v n2)))
                   (is (eql (node/s n1) (node/s n2)))
                   (matches (node/l n1) (node/l n2))
                   (matches (node/r n1) (node/r n2))))))
      (matches (tree::single-r :BK :BV
                 (node :AK :AV (node :XK :XV () () 1) (node :YK :YV () () 1) 3)
                 (node :ZK :XZ () () 1))
        (node :AK :AV
          (node :XK :XV () () 1)
          (node :BK :BV (node :YK :YV () () 1) (node :ZK :XZ () () 1) 3) 5)))))


(def test check-rotation-wb/double-r ()
  (with-active-layers (weight-balanced)
    (labels ((matches (n1 n2)
               (or (if (empty? n1) (is (empty? n2)))
                 (progn
                   (is (eq  (node/k n1) (node/k n2)))
                   (is (eq  (node/v n1) (node/v n2)))
                   (is (eql (node/s n1) (node/s n2)))
                   (matches (node/l n1) (node/l n2))
                   (matches (node/r n1) (node/r n2))))))
      (matches (tree::double-r :CK :CV
                 (node :aK :aV
                   (node :XK :XV () () 1)
                   (node :BK :BV (node :Y1K :Y1V () () 1) (node :Y2K :Y2V () () 1) 3) 5)
                 (node :ZK :ZV () () 1))
        (node :BK :BV
          (node :AK :AV (node :XK :XV () () 1) (node :Y1K :Y1V () () 1) 3)
          (node :CK :CV (node :Y2K :Y2V () () 1) (node :ZK :ZV () () 1) 3) 7)))))


(def test check-join-wb/single-l ()
  (with-active-layers (weight-balanced)
    (with-instances/wb ()
      (let ((rot/1l (node/join :root t _1 _7)))
        (is (eql 9 (node/size rot/1l)))
        (is (eq :root (node/k (node/l rot/1l))))
        (is (eql 5 (node/size (node/l rot/1l))))
        (is (eql 3 (node/size (node/r rot/1l))))
        (kvlr (k v l r) rot/1l
          (is (eq k (node/k (node/join k v l r)))))))))


(def test check-join-wb/single-r ()
  (with-active-layers (weight-balanced)
    (with-instances/wb ()
      (let ((rot/1r (node/join :root t _7 _1)))
        (is (eql 9 (node/size rot/1r)))
        (is (eq :root (node/k (node/r rot/1r))))
        (is (eql 5 (node/size (node/r rot/1r))))
        (is (eql 3 (node/size (node/l rot/1r))))
        (kvlr (k v l r) rot/1r
          (is (eq k (node/k (node/join k v l r)))))))))


(def test check-join-wb/double-l ()
  (with-active-layers (weight-balanced)
    (labels ((matches (n1 n2)
               (or (if (empty? n1) (is (empty? n2)))
                 (progn
                   (is (eq  (node/k n1) (node/k n2)))
                   (is (eq  (node/v n1) (node/v n2)))
                   (is (eql (node/s n1) (node/s n2)))
                   (matches (node/l n1) (node/l n2))
                   (matches (node/r n1) (node/r n2))))))
      (matches (node/join :AK :AV
                 (node :XK :XV () () 1)
                 (node :CK :CV
                   (node :BK :BV
                     (node :Y1K :Y1V (node :q1k :q1v () () 1) () 2)
                     (node :Y2K :Y2V (node :q2k :q2v () () 1) () 2) 5) 
                   (node :ZK :ZV () () 1) 7))
        (node :BK :BV
          (node :AK :AV
            (node :XK :XV NIL NIL 1)
            (node :Y1K :Y1V (node :Q1K :Q1V NIL NIL 1) NIL 2) 4)
          (node :CK :CV
            (node :Y2K :Y2V (node :Q2K :Q2V NIL NIL 1) NIL 2)
            (node :ZK :ZV NIL NIL 1) 4) 9)))))


(def test check-join-wb/double-r ()
  (with-active-layers (weight-balanced)
    (labels ((matches (n1 n2)
               (or (if (empty? n1) (is (empty? n2)))
                 (progn
                   (is (eq  (node/k n1) (node/k n2)))
                   (is (eq  (node/v n1) (node/v n2)))
                   (is (eql (node/s n1) (node/s n2)))
                   (matches (node/l n1) (node/l n2))
                   (matches (node/r n1) (node/r n2))))))
      (matches (node/join :CK :CV
                 (node :AK :AV
                   (node :XK :XV () () 1)
                   (node :BK :BV
                     (node :Y1K :Y1V (node :Q1K :Q1V () () 1) () 2)
                     (node :Y2K :Y2V (node :Q2K :Q2V () () 1) () 2) 5) 7)
                 (node :ZK :ZV () () 1))
        (node :BK :BV
          (node :AK :AV
            (node :XK :XV NIL NIL 1)
            (node :Y1K :Y1V (node :Q1K :Q1V NIL NIL 1) NIL 2) 4)
          (node :CK :CV
            (node :Y2K :Y2V (node :Q2K :Q2V NIL NIL 1) NIL 2)
            (node :ZK :ZV NIL NIL 1) 4) 9)))))


(def test check-concat3-wb ()
  (with-active-layers (weight-balanced)
    (with-instances/wb ()
      (is
        (eql #\A
          (elt
            (symbol-name
              (node/k
                (node/least
                  (node/concat3
                    (gensym "A") t () _5)))) 0)))
      (is
        (eql #\Z
          (elt
            (symbol-name
              (node/k
                (node/greatest
                  (node/concat3
                    (gensym "Z") t _5 ())))) 0)))
      (is
        (eql #\A
          (elt
            (symbol-name
              (node/k
                (node/least
                  (node/concat3
                    (gensym "C") t
                    (node/singleton (gensym "A") t) _7)))) 0))))))



(def test check-join-rb/single ()
  (with-active-layers (height-balanced)
    (let ((x (node/create 8 8
               (node/create 4 4
                 (node/create 1 1 nil nil) nil)
               (node/create 16 16
                 (node/create 12 12 nil nil)             
                 (node/create 32 32
                   (node/create 24 24 nil nil)
                   (node/create 64 64
                     (node/create 48 48 nil nil)
                     (node/create 128 128 nil
                       (node/create 256 256 nil nil))))))))   
      (is (eql 6 (node/height x)))
      (is (eql 2 (node/height (node/l x))))
      (is (eql 5 (node/height (node/r x))))
      (let ((y (kvlr (k v l r) x
                 (node/join k v l r))))
        (is (eql 16 (node/v y)))
        (is (eql 5  (node/h y)))
        (lr (l r) y
          (is (eql 3  (node/h l)))
          (is (eql 4  (node/h r)))
          (is (eql 8  (node/v l)))
          (is (eql 32 (node/v r))))))))


(def test check-concat3-rb ()
  (with-active-layers (height-balanced)
    (let* ((a    (node/create   1 1 nil nil))
            (b   (node/create   3 3 nil nil))
            (c   (node/create   5 5 nil nil))
            (d   (node/create   7 7 nil nil))
            (e   (node/create   9 9 nil nil))
            (f   (node/create   6 6 c   d))
            (g   (node/create   8 8 f   e))
            (h   (node/create   4 4 b   g))
            (i   (node/create   2 2 a   h)) 
            (j   (node/concat3  2 2 a   h)))
      (tree:kvlrh (k v l r h) i
        (is (eql 2 k))
        (is (eql 2 v))
        (is (eql 5 h))
        (is (eql (node/v l) 1))
        (is (eql (node/h l) 1))
        (is (eql (node/v r) 4))
        (is (eql (node/h r) 4)))
      (tree:kvlrh (k v l r h) j
        (is (eql 4 k))
        (is (eql 4 v))
        (is (eql 4 h))
        (tree:kvlrh (k v l r h) l
          (is (eql 2 k))
          (is (eql 2 v))
          (is (eql 2 h))
          (is (eql (node/v l) 1))
          (is (eql (node/v r) 3)))
        (tree:kvlrh (k v l r h) r
          (is (eql 8 k))
          (is (eql 8 v))
          (is (eql 3 h))
          (is (eql (node/v r) 9))
          (is (eql (node/h r) 1))
          (tree:kvlrh (k v l r h) l
            (is (eql 6 k))
            (is (eql 6 v))
            (is (eql 2 h))
            (is (eql (node/v l) 5))
            (is (eql (node/h l) 1))
            (is (eql (node/v r) 7))
            (is (eql (node/h r) 1))))))))
      
#+()
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

#+()
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
            (is (eql (set:min  integer-set) 1))
            (is (eql (set:max  integer-set) i))
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
            (setf integer-set (set:remove-min integer-set))
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
            (let ((x (random most-positive-fixnum)))
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
    (check-compare-ordinality random-set  (set:remove-min   random-set))
    (check-compare-ordinality integer-set (set:remove-min   integer-set))
    (check-compare-ordinality (set:remove-max random-set)   random-set)
    (check-compare-ordinality (set:remove-max integer-set)  integer-set)
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

(seq:make)
(seq:list (seq:make t))
(seq:list (seq:make '(1 2 3 4 5)))
(seq:list (seq:make (seq:make '(1 2 3 4 5))))
(seq:list (seq:make (seq:make #(1 2 3 4 5))))
(seq:list (seq:make (seq:make "bababooey")))
(seq:list (seq:make (set:add 5 (set:add 9 nil))))
(seq:list (seq:concat
            (push 1 (push 2 (push 3)))
            (seq:map #'- (push 1 (push 2 (push 3))))
            (seq:dup (seq:map #'princ-to-string (push 1 (push 2 (push 3)))))))



(is (eql 0 (seq:compare (seq:make '(1 2 3 4 5)) (seq:make '(1 2 3 4 5)))))
(is (seq:equal (seq:make '(1 2 3 4 5)) (seq:make '(1 2 3 4 5))))
(is (eql 1 (seq:compare (seq:make '(2 3 4 5)) (seq:make '(1 2 3 4 5)))))
(is (eql -1 (seq:compare (seq:make '(1 2 3 4 5)) (seq:make '(2 3 4 5)))))
(is (eql 1 (seq:compare (seq:make '(10 20 30 40 50)) (seq:make '(1 2 3 4 5)))))
(is (eql -1 (seq:compare  (seq:make '(1 2 3 4 5)) (seq:make '(10 20 30 40 50)))))


|#
                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSTM 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def (suite* e) (cstm :in dstm-collections))

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

(defun check-invariant (&aux a b)
  (dstm:atomic
    (setf
      a (dstm:read *a*)
      b (dstm:read *b*)))
  (let ((result (= b (* 2 a))))
    (unless result
      (error  "Invariant broken: A = ~A, B = ~A" a b))))



(defun common-code (delta)
  (dstm:atomic
    (let ((a (+ delta (dstm:read *a*))))
      (dstm:write *a* a)
      (dstm:write *b* (* 2 a)))))


(defun count-up (n)
  (lambda ()
    (loop :for i :from 1 :to n
      :do (progn
            (when (zerop (mod i 100000))
              (princ "+" *trace-output*))
            (common-code 1)))))


(defun count-down (n)
  (lambda ()
    (loop :for i :from 1 :to n
      :do (progn
            (when (zerop (mod i 100000))
              (princ "-" *trace-output*))
            (common-code -1)))))


(defun test-dstm-contention (iterations)
  (format *trace-output* "~%Start DSTM Test/~A...~%" iterations)

  (setf *a* (dstm:create-var 0))
  (setf *b* (dstm:create-var 0))
  (dstm:reset)

  (let ((start (local-time:now))
         (procs
           (list
             (bt:make-thread (funcall #'count-down iterations)
               :name "down"
               :initial-bindings '((dstm:*transaction* . nil)))
             (bt:make-thread (funcall #'count-up   iterations)
               :name "up"
               :initial-bindings '((dstm:*transaction* . nil))))))

    (loop
      :while (some #'sb-thread:thread-alive-p procs)
      :do    (check-invariant))

    
    (let ((stop (local-time:now)))
      (princ (show-rolls (* 1e-6 (local-time:timestamp-difference stop start)))
        *trace-output*)))

  (values))

#+()
(def test check-dstm/1-million-transactions ()
  (finishes
    (test-dstm-contention 500000)))

#+()
(def test check-dstm/2-million-transactions ()
  (finishes
    (test-dstm-contention 1000000)))

#+()
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

(defmacro are (test &rest tests)
  "Evaluate a list of tests with (is)"
  `(progn
     (is ,test)
     (unless (null ',tests)
       (are ,@tests))))

(deftest test-mult ()
  (is (= 1 (multiply 1 1)))
  (is (= 6 (multiply:multiply 2 3)))
  (is (= 100 (multiply:multiply 10 10)))
  (is (= 0 (multiply:multiply 3 0)))
  (is (= 0 (multiply:multiply 0 3))))

(deftest test-mult ()
  (are (= 1 (multiply 1 1))
       (= 6 (multiply:multiply 2 3))
       (= 100 (multiply:multiply 10 10))
       (= 0 (multiply:multiply 3 0))
       (= 0 (multiply:multiply 0 3))))
