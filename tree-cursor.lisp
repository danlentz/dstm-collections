;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :tree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dlambda: anaphoric destructuring lexical closure dispatch function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))


(defmacro dlambda (&rest dispatch-table)
  (let* ((arglist (gensym "ARGS")))
    `(lambda (&rest ,arglist)
       (case (car ,arglist)
         ,@(mapcar (lambda (d)
                     `(,(if (eq t (car d))
                          t
                          (list (car d)))
                        (apply (lambda ,@(cdr d))
                          ,(if (eq t (car d))
                             arglist
                             `(cdr ,arglist)))))
             dispatch-table)))))


(defmacro alet (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     (lambda (&rest params)
       (apply this params))))


(defmacro fbind ((name form) &body body)
  (let ((gname (gensym (string name))))
    `(let ((,gname ,form))
       (declare (function ,gname))
       (flet ((,name (&rest args) (apply ,gname args)))
         ,@body))))


;; (fbind (foo (lambda (x) (print (list 'foo x))))
;;   (foo 1)
;;   (foo 2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the structure of cons-enum
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; the following structure is an example of what we would like to traverse with
;; our cursor (or "iterator"). The right-subtrees (below in { }) are expanded
;; lazily as we traverse the structure.
;;
;;
;; (tree:cons-enum (make-integer-set 32) nil)
;;                         
;; (1  NIL
;;  (2   { 3 }
;;   (4   { 5 6 7 }
;;    (8   { 9 10 11 12 13 14 15 }
;;     (16  { 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 } NIL)))))
;;
;; Once "unfolded" it will accumulate in reverse (for backtracking) as a structure
;; that looks like this when the sequence has been fully traversed:
;;
;; (13 NIL
;;   (12 NIL
;;     (11 NIL
;;       (10 NIL
;;         (9 NIL (8 NIL (7 NIL (6 NIL (5 NIL (4 NIL (3 NIL (2 NIL (1 NIL NIL)))))))))))))
;;
;; The "end" of the tree can always be recognised for a stop condition  as a 3 element
;; list matching (+end+ nil nil)
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; with-cursor convenience macro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro with-cursor ((name collection) &body body)
  "Within the scope of BODY, Bind a stateful closure to a function called NAME
   that may then be called repeatedly with various optional parameters in order
   to effect a number of useful operations on the given collection that may be
   both conceptually simpler to implement and possibly more efficient than
   it would be to accomplish the equivalent using a functional approach such
   as mapping and reducing.  In a basic sense a cursor is similar in spirit to
   the 'iterator' facilities present in common-lisp (package iterator, sequence
   iterator, ...) however there are a number of additional capabilities beyond
   just simple traversal provided by those iterator facilities.  Most significantly,
   the cursor implementation is based on the pandoric closure techniques described
   in Doug Hoyte's 'Let Over Lambda', and, as such, is fully extensible -- not
   only in terms of further development, but also at 'runtime' a new dispatching
   dlambda can be hotpatched into a running cursor instance, offering pretty much
   unlimited ways in which one can dynamically effect behavior and implement
   interesting functionality.  The following table describes the basic cursor
   functions currently provided by the default cursor implementation:

   (<cursor> :whole)
  
   Return the full and exact (eq) collection that this cursor
   was originally invoked on, as the closure maintains a reference
   to it for the duration of its lifetime, unless acted upon; see :reset, below.

   (<cursor> :reset)
   (<cursor> :reset NEW-COLLECTION)
                      
   On initial construction  and when invoked by the user, the :reset dispatch
   clears and initializes all internal state maintained by the cursor and performs
   the initial housekeeping required to prepare for traversal of NEW-COLLECTION.
   As a special case, on :RESET invocations subsequent to the first, if called 
   with no NEW-COLLECTION specified, it will by default prepare for a fresh
   traversal of the original collection. 

   (<cursor> :state)

   The various parameters that comprise the cursor's internal state may be
   inspected by calling the :STATE dispatch, which will return a plist that
   explicitly describes its current dynamic state.
   
   (<cursor> :at)

   Return the collection constituent that is directly 'under' the cursor at
   the present, without advancing the cursor afterwards. Ie, the cursor will
   remain at the same point and if :AT were to be invoked again, the same
   constituent would again be returned.
  
   (<cursor> :elt N)

   Move the cursor directly to the Nth element and return that value.  This
   index is based on a 0-origin to maintain the same semantics as the corresponding
   common-lisp sequence lib.

   (<cursor> :collect)
   (<cursor> :collect N)

   Collect provides a convenient means of aggregating specific elements of the
   collection, similarly to other familiar collect macros, such as used with loop.
   Thus, when used with the cursor, one can browse the collection in any order,
   backwards and forwards to pick and choose elements and collect them as you see fit.
   In addition, if an argument N is provided, :collect will attempt to collect that
   many elements, starting from the initial collection position.

   (<cursor> :clear)

   The contents of the :COLLECT accumulator as described above are cleared and the
   cursor state will be as having collected no elements.t

   (<cursor> :next)
   (<cursor> :prev)

   Advance the cursor one step in the respective direction, returning the value of
   the element positioned there.  

   (<cursor> :nextp)
   (<cursor> :prevp)

   Boolean predicate which may be invoked in order to determine if any further
   elements exist in the corresponding direction.  Thus at the beginning of
   a sequence prevp is nil, at the end nextp is nil.

   (<cursor> :remain)

   The :remain dispatch returns the total number of elements which remain between
   the current position of the cursor and the end of the sequence.  Thus, the
   'rest' of a sequence may be easily obtained by executing a form like
   (cursor :collect (cursor :remain)).  Note, however that since the cursor is
   always free to backtrack as well, the :remain will increase again as one moves
   back towards the 'front' of the sequence.

   (<cursor> :forward N)
   (<cursor> :backward N)

   
   (<cursor> :format STREAM CONTROL-STRING)


   (<cursor> :?)"
  
  (let ((gcolx (gensym "collection"))
         (gcur (gensym "cursor")))
    `(let* ((,gcolx ,collection)
            (,gcur (make-cursor ,gcolx)))
       (flet ((,name (&rest args)
                (apply ,gcur args)))
         ,@body))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dlambda cursor implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar +end+ (cons t t))

(defun make-cursor (collection)
  (check-type collection (or set:type map:type seq:type))
  (alet (enum index head elt tail accum size whole tree)
    (funcall this :reset collection)
    (dlambda
      (:?      () (describe this))
      (:reset  (&optional (collection whole))
        (setf
          whole  collection
          tree   (value collection)
          enum  (tree:cons-enum tree (list +end+ nil nil))
          tail  enum  
          elt   +end+ 
          head  (list +end+ nil nil)
          index -1
          size   (set:cardinal tree)
          accum (list nil nil))
        this)
      (:whole  ()  whole)
      (:at     ()
        (if (minusp index)
          (funcall this :next)
          (values elt index)))
      (:elt    (position)
        (when (minusp index)
          (funcall this :next))
        (loop :repeat (abs (- position index)) :do
          (if (> position index)
            (funcall this :next)
            (funcall this :prev))))
      (:format (&optional (stream t) (control " ~S "))
        (format stream control elt))
      (:clear ()
        (setf accum (list nil nil)))
      (:collect (&optional (count 1))
        (when (minusp index) (funcall this :next))
        (loop
          :for i :from 1 :to count           
          :do (progn
                (pushnew elt   (first  accum))
                (pushnew index (second accum))
                (funcall this :next))
          :finally (return (reverse (first accum))))) 
      (:accum  ()
        (reverse (first accum)))        
      (:state  ()
        (list :head head :element elt :tail tail :position index 
          :accum accum :remain (funcall this :remain)
          :nextp (funcall this :nextp) :prevp (funcall this :prevp)))
      (:remain () (- size index 1)) 
      (:nextp  () (not (eq +end+ (first tail))))
      (:prevp  () (not (eq +end+ (first head))))
      (:forward  (&optional (count 1))
        (dotimes (i count)
          (funcall this :next)))
      (:backward (&optional (count 1))
        (dotimes (i count)
          (funcall this :prev)))
      (:next   ()
        (if (funcall this :nextp)
          (progn
            (incf index)
            (setf
              elt   (first tail)
              tail  (tree:cons-enum (second tail) (third tail))
              head  (list elt nil head))
            (values elt index))
          (values nil nil)))
      (:prev   ()
        (if (funcall this :prevp)
          (progn
            (decf index)
            (setf
              elt   (first head)
              head  (tree:cons-enum (second head) (third head))
              tail  (list elt nil tail))
            (values elt index))
          (values nil nil)))
      (t       ()
        (funcall this :next)))))






#|

(with-cursor (@ {9 8 7 6 5})
  (print (@ :state))
  (print (@ :whole))
  (@ :reset #{ :a :b :c :d :e :f :g})
  (print (@ :state))
  (print (@ :whole))
  (print (@ :collect 7))
  (@))

|#

#|
(defparameter csr  (make-cursor (dstm-collections-test::make-integer-set 23)))
(describe csr)
(setf (fdefinition '@) csr)
(@ :?)
(progn
  (@ :next)
  (print (@ :state)))
(@ :collect 5)
(@ :elt)
(@ :whole)
(@ :reset)
(@ :reset {9 8 7 6 5 4 3 2 1})
(@ :format)
(@)

(funcall (make-cursor {:a :b :c :d}) :state)
(funcall (make-cursor {3 9 8 7}) :whole)
(funcall (make-cursor {3 9 8 7}) :next)

(let* ((x (dstm-collections-test::make-integer-set 23))
        (cursor (make-cursor x)))
  (print (funcall cursor :state))
  (loop :repeat (funcall cursor :remain)
    :do (print (funcall cursor :next)))
  (print (funcall cursor :state)))

(let* ((x (dstm-collections-test::make-integer-set 13))
        (cursor (make-cursor x)))
  (print (funcall cursor :state))
  (loop :while (funcall cursor :nextp)
    :do (print (funcall cursor :collect))
    :finally (print (funcall cursor :collect)))
  (print (funcall cursor :state))
  (funcall cursor)
  (loop :while (funcall cursor :prevp)
    :do (print (funcall cursor :prev)))
  (print (funcall cursor :state))
  (print (funcall cursor :next))
  (print (funcall cursor :state)))

|#


#|

(defparameter *rset1* (time (dstm-collections-test::make-random-set   300000)))
(defparameter *rset2* (time (dstm-collections-test::make-random-set   100000)))
(defparameter *rset3* (time (dstm-collections-test::make-random-set    50000)))
(defparameter *rset4* (time (dstm-collections-test::make-random-set    50000)))
(defparameter *iset*  (time (dstm-collections-test::make-integer-set 1000000)))

(time (print
        (preduce #'set:union 
          (pmapcar #'set:make
            (set:with-cursor (@ *rset1*)
              (let (acc) 
                (loop 
                  :while (@ :nextp)
                  :do (progn (push (@ :collect 50) acc) (@ :clear)))
                acc))))))

(time (set:union
        (set:make (subseq (set:elements *rset1*) 0 150000))
        (set:make (subseq (set:elements *rset1*) 150001))))

|#


;;(values (reverse (first accum)) (reverse (second accum))))))
