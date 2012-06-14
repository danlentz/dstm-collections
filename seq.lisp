;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

;; seq's are somewhat of a comprimise -- to provide  the most efficient and capable
;; implementation necessitates a weight-balanced tree, but this would cause
;; a significant hit to performance over red-black tree based collections, in the
;; general case.  As, at the moment, a homogeneous underlying tree implementation
;; for all collection types is desirable, and since seq has proven somewhat useful to,
;; have, a red-black tree based implementation is provided, with the adviso that this may
;; be subject to significant change in the future.


(in-package :seq)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; seq collection classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass mutable-seq/dstm (map::mutable-map/dstm)
  ())

(defclass mutable-seq/cstm (map::mutable-map/cstm)
  ())

(defclass seq* (map:map* mutable-seq/cstm)
  ())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; seq element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass seq-cell (map::map-cell)
  ((map::key
     :accessor seq-cell-key
     :initform 0
     :initarg :key
     :documentation "position constituent")
    (map::val
      :accessor seq-cell-val
      :initform nil
      :initarg :val
      :documentation "item constituent"))
  (:documentation "storage cell for implementation of tree:rb-tree based sequences"))


(defun make-seq-cell (&rest args)
  "convenience api routine for map-cell initialization"
  (apply #'make-instance 'seq-cell args))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; seq:type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun seq:typep (thing)
  "seq collection type-predicate"
  (or
    (null thing)
    (cl:typep thing 'mutable-seq/dstm)
    (cl:typep thing 'mutable-seq/cstm) 
    (and (tree:typep thing) (cl:typep (tree:rb-tree-v thing) 'seq-cell))))


(deftype seq:type ()
  "collection of arbitrary, possibly non-unique, elements in some specific lexical order"
  `(satisfies seq:typep))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; seq api
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun seq::first-node (seq &aux (actual-seq (value seq)))
  (cond ((null actual-seq) nil)
    ((null (tree:rb-tree-l actual-seq)) actual-seq)
    (t     (seq::first-node (tree:rb-tree-l actual-seq)))))


(defun seq::last-node (seq &aux (actual-seq (value seq)))
  (cond ((null actual-seq) nil)
    ((null (tree:rb-tree-r actual-seq)) actual-seq)
    (t     (seq::last-node (tree:rb-tree-r actual-seq)))))


(defun seq:empty ()
  "create empty seq"
  nil)


(defun seq:empty* ()
  "create empty seq"
  (seq:make* (seq:empty)))


(defun seq:emptyp (seq &aux (actual-seq (value seq)))
  "return true if SEQ contains no elements, otherwise false"
  (null actual-seq))

;; (seq:emptyp (seq:empty))
;; T

;; (seq:emptyp (seq:empty*))
;; T
  
;; (seq:emptyp [7 8 9])
;; NIL

;; (seq:emptyp #[7 8 9])
;; NIL


(defun seq:length (seq &aux (actual-seq (value seq)))
  "return the number of elements in SEQ, similar to (cl:length list)"
  (set:cardinal actual-seq))

;; (seq:length (seq:empty))
;; 0

;; (seq:length (seq:empty*))
;; 0

;; (seq:length [1 2 3 4 5])
;; 5

;; (seq:length #[1 2 3 4 5])
;; 5

(defun seq:first (seq &aux (actual-seq (value seq)))
  "return the first element in SEQ, similar to (cl:first list)"
  (when actual-seq (seq-cell-val (set:min actual-seq))))

;; (seq:first [])
;; NIL

;; (seq:first #[])
;; NIL

;; (seq:first [1 2 3 4 5])
;; 1

;; (seq:first #[1 2 3 4 5])
;; 1


(defun seq:last (seq &aux (actual-seq (value seq)))
  "return the last element in SEQ, similar to (cl:car (cl:last list))"
  (when actual-seq (seq-cell-val (set:max actual-seq))))

;; (seq:last [])
;; NIL

;; (seq:last #[])
;; NIL

;; (seq:last [1 2 3 4 5])
;; 5

;; (seq:last #[1 2 3 4 5])
;; 5


(defun seq::min-key (seq)
  "return the key of the first element of SEQ"
  (let ((seq (value seq)))
    (if (null seq)
      0
      (seq-cell-key (set:min seq)))))


(defun seq::max-key (seq)
  "return the key of the last element of SEQ"
  (let ((seq (value seq)))
    (if (null seq)
      0
      (seq-cell-key (set:max seq)))))


(defun seq:rest (seq)
  "return a new seq containing all but the first element of SEQ"
  (let ((seq (value seq)))
    (if (null seq)
      nil
      (set:remove-min seq))))


(defun seq:butfirst (seq &optional (n 1))
  "return a new seq containing all but the first n elements of SEQ"
  (let ((seq (value seq)))
    (cond
      ((null  seq) nil)
      ((zerop n)   seq)
      ((plusp n)   (seq:butfirst (set:remove-min seq) (- n 1)))
      (t           (error "invalid argument n: ~s" n)))))

;; (butfirst [1 2 3 4 5])
;; [ 2 3 4 5 ]

;; (butfirst [1 2 3 4 5] 0)
;; [ 1 2 3 4 5 ]

;; (butfirst [1 2 3 4 5] 1)
;; [ 2 3 4 5 ]

;; (butfirst [1 2 3 4 5] 2)
;; [ 3 4 5 ]

;; (butfirst [1 2 3 4 5] 3)
;; [ 4 5 ]

;; (butfirst [1 2 3 4 5] 4)
;; [ 5 ]

;; (butfirst [1 2 3 4 5] 5)
;; NIL

;; (butfirst [1 2 3 4 5] 6)
;; NIL

(defun seq:butlast (seq &optional (n 1))
  "return a new seq containing all but the last n elements of SEQ"
  (let ((seq (value seq)))
    (cond
      ((null  seq) nil)
      ((zerop n)   seq)
      ((plusp n)   (seq:butlast (set:remove-max seq) (- n 1)))
      (t           (error "invalid argument n: ~s" n)))))

;; (butlast [1 2 3 4 5])
;; [ 1 2 3 4 ]

;; (butlast [1 2 3 4 5] 0)
;; [ 1 2 3 4 5 ]

;; (butlast [1 2 3 4 5] 1)
;; [ 1 2 3 4 ]

;; (butlast [1 2 3 4 5] 2)
;; [ 1 2 3 ]

;; (butlast [1 2 3 4 5] 3)
;; [ 1 2 ]

;; (butlast [1 2 3 4 5] 4)
;; [ 1 ]

;; (butlast [1 2 3 4 5] 5)
;; NIL

;; (butlast [1 2 3 4 5] 6)
;; NIL


(defun seq:push (elem &optional seq)
  "return a new seq identical to SEQ but with elem prepended"
  (set:add
    (make-seq-cell :key (- (min-key seq) 1) :val elem) (value seq)))

;; (seq:push :x)
;; [ :X ]

;; (seq:push 3 [:x 2])
;; [ 3 :X 2 ]

;; (seq:push 'x (seq:push :x (seq:push #\x)))
;; [ SEQ::X :X #\x ]


(defun seq:push* (elem &optional (seq (seq:make* elem) seq-provided-p))
  "prepend elem to the mutable seq SEQ, if seq-provided-p, othewise create a new mutable
   seq containing only elem"
  (check-type seq var)
  (prog1 seq
    (when seq-provided-p
      (setf (value seq) (seq:push elem (value seq))))))

;; (seq:push* :x)
;; #[ :X ]

;; (seq:push* 3 #[:x 2])
;; #[ 3 :X 2 ]

;; (seq:push* 'x (seq:push* :x (seq:push* #\x)))
;; #[ SEQ::X :X #\x ]


(defun seq:add (elem &optional seq)
  "return a new seq identical to SEQ but with elem appended"
  (set:add
    (make-seq-cell :key (+ (max-key seq) 1) :val elem) (value seq)))

;; (seq:add :x)
;; [ :X ]

;; (seq:add :x ["x" #\x 'x])
;; [ "x" #\x 'SEQ::X :X ]

;; (seq:add 1 (seq:add 5 (seq:add 2 (seq:add 4 [3]))))
;; [ 3 4 2 5 1 ]


(defun seq:add* (elem &optional (seq (seq:make* elem) seq-provided-p))
  "append elem to the mutable seq SEQ, if seq-provided-p, othewise create a new mutable
   seq containing only elem"
  (check-type seq var)
  (prog1 seq
    (when seq-provided-p
      (setf (value seq) (seq:add elem (value seq))))))

;; (seq:add* :x)
;; #[ :X ]

;; (seq:add* :x #["x" #\x 'x])
;; #[ "x" #\x 'SEQ::X :X ]

;; (seq:add* 1 (seq:add* 5 (seq:add* 2 (seq:add* 4 #[3]))))
;; #[ 3 4 2 5 1 ]


(defun seq:list (seq)
  "return the list-based equivalent to SEQ, containing all elements and in the same order"
  (mapcar #'seq-cell-val (set:elements (value seq))))

;; (seq:list [1 2 3 4 5])
;; (1 2 3 4 5)


(defun seq:vector (seq)
  "return the vector-based equivalent to SEQ, containing all elements and in the same order"
  (cl:map 'cl:vector #'seq-cell-val (set:elements (value seq))))

;; (seq:vector [1 2 3 4 5])
;; #(1 2 3 4 5)


(defun seq:dup (seq)
  "return a new seq that is seq:equal to the original SEQ"
  (let ((seq (value seq)))
    (cond
      ((null seq)  nil)
      (t         (let* ((min-cell (set:min seq))
                         (new-cell (make-seq-cell
                                     :key (seq-cell-key min-cell)
                                     :val (seq-cell-val min-cell))))
                   (set:add new-cell (seq:dup (set:remove-min seq))))))))


(defun seq::map-indices (f seq &aux new-seq)
  "seq::map-indices is functionally identical to map:keymap, with the exception that a new seq is
   constructed and returned rather than a map.  seq:map-indices, however, is not exported as part
   of the public interface, as the keys of a seq are managed internally in order to
   manipulate the lexical order of the elements of a seq, and are not user-visible.  That
   said, it is extremely useful when implementing new types of sequence operations
   that may themselves be exported in the public interface as an extension to this library"
  (let ((seq (value seq)))
    (cond
      ((null seq) nil)
      (t         (dolist (elem (set:elements seq))
                   (setf new-seq (set:add  (make-seq-cell
                                       :key (funcall f (seq-cell-key elem))
                                       :val (seq-cell-val elem)) new-seq)))
                 new-seq))))


(defun seq::reindex (seq &key (offset 0) (increasing t) &aux (idx offset))
  (let ((seq (value seq)))
    (map-indices #'(lambda (key)
                     (declare (ignore key))
                     (prog1 idx
                       (if increasing (incf idx) (decf idx))))
      seq)))


(defun seq::reindex* (seq &key (offset 0) (increasing t))
  (check-type seq var)
  (setf (value seq) (reindex seq :offset offset :increasing increasing))
  seq)
  

;; (progn
;;   (printv (map:keys #1=(push 9 (push :a [1 2 3 4 5]))))
;;   (printv (map:keys (reindex #1#))))
;;
;; (MAP:KEYS          (PUSH 9 (PUSH :A [ 1 2 3 4 5 ])))
;;      => (-1 0 1 2 3 4 5)
;; (MAP:KEYS (REINDEX (PUSH 9 (PUSH :A [ 1 2 3 4 5 ]))))
;;      => (0 1 2 3 4 5 6)

;; (progn
;;   (printv (map:keys #1=(push 9 (push :a [1 2 3 4 5]))))
;;   (printv (map:keys (reindex #1# :offset 10 :increasing nil))))
;;
;;   (MAP:KEYS (PUSH 9 (PUSH :A [ 1 2 3 4 5 ])))
;;      => (-1 0 1 2 3 4 5)
;;   (MAP:KEYS (REINDEX (PUSH 9 (PUSH :A [ 1 2 3 4 5 ])) :OFFSET 10 :INCREASING NIL))
;;      => (4 5 6 7 8 9 10)


(defun seq:reverse (seq)
  ""
  (let ((seq (value seq)))
    (reindex seq :increasing nil :offset (max-key seq))))

;; (printv (seq:reverse (seq:make (make-gensym-list 10))))
;;      => [ #:G3836 #:G3835 #:G3834 #:G3833 #:G3832 #:G3831 #:G3830 #:G3829 #:G3828 #:G3827 ]


(defun seq:reverse* (seq)
  (check-type seq var)
  (reindex* seq :increasing nil :offset (max-key seq)))

;; (printv (reverse* #[1 2 3 4 5 6 7 8]))
;;      => #[ 8 7 6 5 4 3 2 1 ]


(defun seq:concat (s0 &rest args)
  "return a new sequence that consists of all elements of s0 followed by all elements of
   each of the supplied additional sequences, in order, from left to right.  Any number
   of sequences may be concatenated in a single call."
  (let* ((s0 (value s0))
          (s1 (when args (value (car args))))
          (max-key-s0 (max-key s0))
          (min-key-s1 (if s1 (min-key s1) 0)))        
    (cond
      ((null args) s0)
      (t (apply #'seq:concat
           (set:union s0
             (seq::map-indices
               #'(lambda (old-index)
                   (+ old-index 1
                     (if (minusp max-key-s0) 1 max-key-s0)
                     (if (minusp min-key-s1) (abs min-key-s1) 1)))
               s1))
           (cl:rest args))))))


(defun seq:map (f seq)
  "return a new seq that contains the result of applying f to each element of seq,
   in the same lexical order.  The underlying structure of the collection rb-tree
   is unaffected.  Operates in a manner analogous to cl:mapcar."
  (let ((seq (value seq)))
    (cond ((null seq) nil)
      (t (let (new-seq)
           (dolist (elem (set:elements seq))
             (setf new-seq (set:add (make-seq-cell
                                      :key (seq-cell-key elem)
                                      :val (funcall f (seq-cell-val elem)))
                             new-seq)))
           new-seq)))))


;; (seq:equal (seq:map #'string          [:a :b :c :d]) ["A" "B" "C" "D"])
;; (seq:equal (seq:map #'princ-to-string [5 4 3 2 1])   ["5" "4" "3" "2" "1"])
  

(defun seq:make (&optional (from (seq:empty)))
  "end-user api for construction of a new seq optionally initialized to contain
   elements derived from various types of source data, with preservation of the
   original order in such cases where the concept of lexical ordering applies."
  (etypecase from
    (null     (seq:empty))
    (var      (seq:make (value from)))
    (cl:list  (let (seq) (dolist (elem from) (setq seq (seq:add elem seq))) seq))
    (seq:type (seq:dup from))
    (map:type (error "at this point seqs cannot be created from ordinary maps"))
    (set:type (seq:make (set:elements from)))
    (string   (seq:add from (seq:empty)))
    (sequence (seq:make (cl:coerce from 'cl:list)))
    (atom     (seq:add from (seq:empty)))))


(defun seq:make* (&optional (from (seq:empty)))
  ;; selects stm implementation based on class of seq*
  (if (find (find-class 'mutable-seq/cstm)
        (c2mop:class-direct-superclasses (find-class 'dclx:seq*)))
    (cstm:create-var (seq:make from) 'seq*)
    (dstm:create-var (seq:make from) 'seq*)))


(defun seq:elt (seq index)
  "return the element of seq at position index, which should be a positive integer
   as would apply in a manner analogous to using cl:elt on a built-in sequence type."
  (when (minusp index) (error "dereference index: ~d before start of seq: ~d" index 0))
  (let* ((seq (value seq))
          (enum (tree:cons-enum seq nil))
          (node (progn
                  (when (plusp index)
                    (dotimes (i index)
                      (when (every #'null (cl:rest enum))
                        (error (make-condition 'sb-kernel:index-too-large-error
                                 :expected-type `(integer 0 ,(- (seq:length seq) 1))
                                 :datum index)))
                      (setq enum (tree:cons-enum (cl:second enum) (cl:third enum)))))
                  (cl:first enum))))
    (if node
      (values (seq-cell-val node) t)
      (values nil t))))


(defun seq::elt-impl2 (seq index)
  (let* ((seq (value seq))
          (list (seq:list seq)))
    (cl:elt list index)))


;; (elt  [1 2 3 4 5] -1) (elt-impl2  #[1 2 3 4 5] -1)
;; (elt  [1 2 3 4 5]  0) (elt-impl2  #[1 2 3 4 5]  0)
;; (elt  [1 2 3 4 5]  1) (elt-impl2  #[1 2 3 4 5]  1)
;; (elt  [1 2 3 4 5]  2) (elt-impl2  #[1 2 3 4 5]  2)
;; (elt  [1 2 3 4 5]  3) (elt-impl2  #[1 2 3 4 5]  3)
;; (elt  [1 2 3 4 5]  4) (elt-impl2  #[1 2 3 4 5]  4)
;; (elt  [1 2 3 4 5]  5) (elt-impl2  #[1 2 3 4 5]  5)
;; (elt  [1 2 3 4 5]  6) (elt-impl2  #[1 2 3 4 5]  6)


(defun seq:reduce (fn seq &rest args &key key from-end (start 0) end initial-value)
  "combines the elements of seq according to function and returns the result.
   for example, a sequence of numbers may be reduced using function #'+ which
   would return the result of adding them all together.  reduction using #'cl:max
   would return the largest.  In the simplest type of invocation, if the elements
   of seq are #[a b c d] calling reduce is equivalent to (fn (fn (fn a b) c) d).
   All options of #'cl:reduce are supported as it is what is used in the underlying
   implementation of #'seq:reduce"
  (declare (ignorable key from-end start end initial-value))
  (apply #'cl:reduce (cl:append (cl:list fn (seq:list (value seq))) args)))


;; (seq:reduce #'cl:+   [1 2 3 4 5])
;; (seq:reduce #'cl:max [1 2 3 4 5])
;; (seq:reduce #'cl:min [1 2 3 4 5])
;; (seq:reduce #'cl:*   [1 2 3 4 5])


(defun seq:compare (seq1 seq2 &optional (cmp #'ord:compare))
  "ordinal comparison of two seqs"
  (let*  ((seq1 (value seq1))
          (seq2 (value seq2))
          (e1   (tree:cons-enum seq1 nil))
          (e2   (tree:cons-enum seq2 nil)))
    (tagbody again
      (return-from compare
        (cond
          ((and (null e1) (null e2))   0)
          ((null e1)                  -1)
          ((null e2)                   1)
          (t (destructuring-bind (v1 r1 ee1) e1
               (destructuring-bind (v2 r2 ee2) e2
                 (let ((c (funcall cmp (seq-cell-val v1) (seq-cell-val v2))))
                   (cond
                     ((not (zerop c)) c)
                     (t               (progn
                                        (setf e1 (tree::cons-enum r1 ee1)
                                              e2 (tree::cons-enum r2 ee2))
                                        (go again)))))))))))))


(defun seq:equal (seq1 seq2 &optional (cmp #'ord:compare))
  "compare two seqs for element-wise equality using cmp"
  (zerop (seq:compare (value seq1) (value seq2) cmp)))


;; (seq:equal [1 2 3 4 5]   [1 2 3 4 5])
;; (seq:equal [1 2 3 4 5]   [1 2 3 4])
;; (seq:equal [1 :b 3 :c 5] [1 :b 3 :c 5])

;; (not (eq #1=[1 2 3 4 5] (seq:dup #1#)))
;; (eq #1=[1 2 3 4 5] #1#)


(defun seq:subseq (seq start &optional end)
  (let ((seq (value seq)))
    (with-cursor (@ seq)
      (when (null end) (setq end (@ :remain))) 
      (when (or (< start 0) (> end (@ :remain)))
        (error "indices out of bound: ~D ~D" start end))
      (when (> start end)
        (error "indices specify negative interval: ~D ~D" start end))
      (@ :elt start)
      (@ :collect (1+ (- end start))))))



;; (subseq {0 1 2 3 4 5 6 7 8 9} 0 )
;; (0 1 2 3 4 5 6 7 8 9)
;;
;; (subseq {0 1 2 3 4 5 6 7 8 9} 5 )
;; (5 6 7 8 9)
;;
;;
;; (subseq {0 1 2 3 4 5 6 7 8 9} 5 7)
;; (5 6 7)
;;
;; (subseq {0 1 2 3 4 5 6 7 8 9} 5 5)
;; (5)



#+()
(defmethod sequence:length ((s tree:rb-tree))
  (set:cardinal s))
