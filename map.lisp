;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; map collection classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass mutable-map/dstm (set::mutable-set/dstm)
  ())

(defclass mutable-map/cstm (set::mutable-set/cstm)
  ())

(defclass map* (set:set* mutable-map/cstm)
  ()
  (:documentation "A transactional variant of the base functional data structure
   map implementation which may be used in a manner similar to 'regular' mutable 
   dictionary constructs, such as a hash-table."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mapped pair
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass map-cell ()
  ((key
     :accessor map-cell-key
     :initform nil
     :initarg :key
     :documentation "domain constituent")
    (val
      :accessor map-cell-val
      :initform nil
      :initarg :val
      :documentation "range constituent"))
  (:documentation "storage cell for implementation of tree:rb-tree based key-value maps"))


(defun make-map-cell (&rest args)
  "convenience api routine for map-cell initialization"
  (apply #'make-instance 'map-cell args))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; map:type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun map:typep (thing)
  "type predicate responsible for aggregate classification  of the various map subtypes" 
  (or
    (null thing)
    (cl:typep thing 'mutable-map/dstm)
    (cl:typep thing 'mutable-map/cstm)
    (and (tree:typep thing) (cl:typep (tree:rb-tree-v thing) 'map-cell))))


(deftype map:type ()
  "key-value associative set"
  `(satisfies map:typep))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; map ordinality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod ord:compare ((a map-cell) (b map-cell))
  "ordinal comparison of two map cells -- used by set:add"
  (ord:compare (map-cell-key a) (map-cell-key b)))


(defmethod ord:compare (a (b map-cell))
  "ordinal comparison of keys against map-cells"
  (ord:compare a (map-cell-key b)))


(defmethod ord:compare ((a map-cell) b)
  "ordinal comparison of map-cells against keys"
  (ord:compare (map-cell-key a) b))

(defmethod ord:compare ((a tree:rb-tree) (b tree:rb-tree))
  "ordinal comparison of two collections of indeterminant type"
  (cond
    ((and (null a) (null b))  0)
    ((null a)                -1)
    ((null b)                 1)
    (t                        (let ((a-elem (tree:rb-tree-v a))
                                     (b-elem (tree:rb-tree-v b)))
                                (if (and
                                      (cl:typep a-elem 'map-cell)
                                      (cl:typep b-elem 'map-cell))
                                  (map:compare a b)
                                  (set:compare a b))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; map api
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun map:empty ()
  "create empty map"
  nil)


(defun map:empty* ()
  "create empty map"
  (map:make* (map:empty)))


(defun map:emptyp (map)
  "return true if map contains no elements, otherwise false"
  (null (value map)))


(defun map:add (key val map)
  "retun map with key-val association added or replaced if already present
   as a second return value, return true if actually added"
  (set:add (make-map-cell :key key :val val) (value map)))


(defun map:add* (key val map)
  "destructively map:add or replace KEY-VAL association to a mutable MAP
   as a second return value, return true if actually added"
  (check-type map var)
  (multiple-value-bind (new-map added?) (map:add key val map)
    (setf (value map) new-map)
    (cl:values map added?)))


(defun find-cell (key map &optional default)
  "return cell-object with key in map, or default if not present"
  (let ((map (value map)))
    (cond
      ((null map) (cl:values default nil))
      (t            (lvr (l v r) map
                      (let ((c (ord:compare key (map-cell-key v))))
                        (cond
                          ((zerop c) (cl:values v t))
                          (t         (find-cell key (if (minusp c) l r)))
                          )))))))


(defun map:mem (key map)
  "return true if map contains mapping 'key' -- key may be key or cell object"
  (let ((map (value map)))
    (typecase key
      (map-cell (set:mem key map))
      (t        (if (find-cell (map-cell-key key) map) t nil)))))


(defun map:remove (key map)
  "construct a new map identical to MAP, but with the KEY-VALUE association for KEY
   removed, if present"
  (let ((map (value map)))
    (typecase key
      (map-cell (set:remove key map))
      (t        (set:remove (find-cell key map) map)))))


(defun map:remove* (key map)
  "destructively map:remove KEY-VALUE association from a mutable MAP"
  (check-type map var)
  (setf (value map) (map:remove key map))
  map)

  
(defun map:find (key map &optional default)
  "return value mapped to key in map, or default if not present"
  (let ((map (value map)))
    (cond
      ((null map) (cl:values default nil))
      (t           (lvr (l v r) map
                     (let ((c (ord:compare key (map-cell-key v))))
                       (cond ((zerop c) (cl:values (map-cell-val v) t))
                         (t         (find key (if (minusp c) l r))))))))))


(defun map:ensure-find (key map &optional default)
  "Like FIND, but if KEY is not found in the MAP constructs a new map
   with association of DEFAULT under key and returns it. Secondary return
   value is true if key was already in the table."
  (let ((map (value map)))
    (multiple-value-bind (val ok) (map:find key map)
      (if ok
        (cl:values map t val)
        (cl:values (map:add key default map) nil default)))))


(defun map:ensure-find* (key map &optional default)
  "Operates as map:ensure-find, but destructively modifies mutable MAP."
  (check-type map var)
  (multiple-value-bind (val ok) (map:find key map)
    (when (not ok) (setf (value map) (map:add key default map)))
    (cl:values map ok (or val default))))


(defun map:compare (map1 map2 &optional (cmp #'ord:compare))
  "ordinal comparison of two maps"
  (let* ((map1  (value map1))
          (map2 (value map2))
          (e1 (cons-enum map1 nil))
          (e2 (cons-enum map2 nil)))
    (tagbody again
      (return-from compare
        (cond
          ((and (null e1) (null e2))   0)
          ((null e1)                  -1)
          ((null e2)                   1)
          (t (destructuring-bind (v1 r1 ee1) e1
               (destructuring-bind (v2 r2 ee2) e2
                 (let ((c (funcall cmp (map-cell-key v1) (map-cell-key v2))))
                   (cond
                     ((not (zerop c)) c)
                     (t              (let ((c (funcall cmp (map-cell-val v1) (map-cell-val v2))))
                                       (cond
                                         ((not (zerop c)) c)
                                         (t               (progn
                                                            (setf e1 (cons-enum r1 ee1)
                                                              e2 (cons-enum r2 ee2))
                                                            (go again)) ))) ))))) ))))))


(defun map:equal (map1 map2 &optional (cmp #'ord:compare))
  "compare two maps for equality using cmp"
  (let ((map1 (value map1))
         (map2 (value map2)))
    (zerop (compare map1 map2 cmp))))


(defun map:fold (f map accu)
  "similar to reduce, takes three argument function f as in: (f key value accumulator)"
  (let ((map (value map)))
    (cond
      ((null map) accu)
      (t          (lvr (l v r) map
                    (fold f r
                      (funcall f
                        (map-cell-key v)
                        (map-cell-val v)
                        (fold f l accu))))))))


(defun map:iter (f map)
  "funcall a function f as (funcall #'f k v) on each key-value pair contained in map"
  (let ((map (value map)))
    (cond ((null map) nil)
      (t  (lvr (l v r) map
            (iter f l)
            (funcall f (map-cell-key v) (map-cell-val v))
            (iter f r))))))


(defmacro map:do ((key value map) &body body)
  "Iterate over (key . value) from map in the manner of the dolist and dotimes macros"
  `(map:iter #'(lambda (,key ,value)
                 ,@body)
     ,map))


;;(map:do (k v {| (9 . 0) (8 . 5) |})
;;  (format t "~&~S => ~S~%" k v))


(defun map:keys (map)
  "Return a list containing all keys present in  MAP"
  (let ((map (value map)) keys)
    (map:iter #'(lambda (k v) (declare (ignore v)) (push k keys)) map)
    (nreverse keys)))


(defun map:values (map)
  "Return a list containing all values present in MAP"
  (let ((map (value map)) values)
    (map:iter #'(lambda (k v) (declare (ignore k)) (push v values)) map)
    values))


(defun map::map-sequential (f map)
  "return a newly created map where the keys of 'map' are associated with function f applied
   to the corresponding map cl:values"
  (let ((map (value map)))
    (cond ((null map) nil)
      (t (let (new-map)
           (dolist (elem (set:elements map))
             (setf new-map (map:add (map-cell-key elem) (funcall f (map-cell-val elem)) new-map)))
           new-map)))))


(defun map:map (f map)
  "return a newly created map where the keys of 'map' are associated with function f applied
   to the corresponding map cl:values"
  (let ((map (value map)))
    (cond ((null map) nil)
      (t (let (new-map)
           (map:iter #'(lambda (k v) (setf new-map (map:add k (funcall f v) new-map)))
             map)
           new-map)))))


(defun map:keymap (f map)
  "return a newly created map where the keys of 'map' are replaced by the result of
   applying the function f to each key.  Note that unless f is a properly 'invertable'
   function, the resulting map will not be very useful.  This is included in the external
   interface mainly because it is extremely useful for manipulating SEQ type collections,
   where we rely on it heavily"
  (let ((map (value map)))
    (cond
      ((null map) nil)
      (t (let (new-map)
           (map:iter #'(lambda (k v) (setf new-map (map:add (funcall f k) v new-map))) map)
           new-map)))))


(defun map::mapi-sequential (f map)
  "return a newly created map from the existing set of keys to values computed as
   dyadic function f(k, v)" 
  (let ((map (value map)))
    (cond
      ((null map) nil)
      (t (let (new-map)
           (dolist (elem (set:elements map))
             (setf new-map (map:add (map-cell-key elem)
                             (funcall f (map-cell-key elem) (map-cell-val elem)) new-map)))
           new-map)))))


(defun map:mapi (f map)
  "return a newly created map from the existing set of keys to values computed as
   dyadic function f(k, v)" 
  (let ((map (value map)))
    (cond ((null map) nil)
      (t (let (new-map)
           (map:iter #'(lambda (k v) (setf new-map (map:add k (funcall f k v) new-map)))
             map)
           new-map)))))


(defun map:make (&optional (from (map:empty)))
  (etypecase from
    (null                  (map:empty))
    (var                   (map:make (value from)))
    (ord:association-list  (loop
                               :with  map
                               :for  (k . v) :in from
                               :do   (setf map (map:add k v map))
                               :finally (return map)))
    (hash-table            (let (map)
                             (maphash #'(lambda (k v) (setf map (map:add k v map))) from)
                             map))
    (seq:type              (error "maps cannot be created from seqs"))
    (map:type              (map:dup from))
    (set:type              (error "maps cannot be created from sets"))))
                

(defun map:make* (&optional (from (map:empty)))
  ;; selects stm implementation based on class of map*
  (if (cl:find (find-class 'mutable-map/cstm)
        (c2mop:class-direct-superclasses (find-class 'map*)))
    (cstm:create-var (map:make from) 'map*)
    (dstm:create-var (map:make from) 'map*)))


;; (map:make '((1 . 2)(3 . 4)(:x . 8)("one" . 1)))
;; =>   {| ("one" . 1) (1 . 2) (3 . 4) (:X . 8) |}

;; (map:make* '((1 . 2)(3 . 4)(:x . 8)("one" . 1)))

