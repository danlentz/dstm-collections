;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :dclx)

(defvar *page-size* userial::+default-buffer-capacity+)

(defun make-buffer ()
  (userial:make-buffer)) 


(defclass page-buffer ()
  ((page-list
     :accessor  page-list
     :initform  nil
     :initarg   :page-list)
    (storage-vector
      :reader   storage-vector
      :initform (userial:make-buffer))))


(defclass disk-page ()
  ((next    :accessor disk-page-next   :initarg :next) 
    (prev   :accessor disk-page-prev   :initarg :prev) 
    (id     :accessor disk-page-id     :initarg :id) 
    (dirty  :accessor disk-page-dirty  :initform nil :initarg :dirty)
    (buffer :accessor disk-page-buffer :initform (make-instance 'page-buffer))
    (stream :accessor disk-page-stream :initform nil :initarg :stream)))


(defgeneric drop (what &key &allow-other-keys))


(defmethod drop :before ((page-buffer page-buffer) &key)
  (setf (page-list page-buffer) nil))


(defmethod drop :before ((disk-page disk-page) &key)
  (with-slots (prev next buffer) disk-page
    (setf (disk-page-next prev) next)
    (setf (disk-page-prev next) prev)
    (drop buffer)))


(defmethod initialize-instance :after ((disk-page disk-page) &rest initargs)
  (declare (ignorable initargs))
  (prog1 disk-page
    (unless (slot-boundp disk-page 'id)
      (setf (disk-page-id disk-page) (unicly:make-v4-uuid))
      (log:user1 "Allocating new disk page: ~A" (disk-page-id disk-page)))
    (unless (slot-boundp disk-page 'next)
      (setf (disk-page-next disk-page) disk-page))
    (unless (slot-boundp disk-page 'prev)
      (setf (slot-value disk-page 'prev) (disk-page-next disk-page)))
    (setf (page-list (disk-page-buffer disk-page)) disk-page)))


(defgeneric allocate (what &rest args))

(defmethod allocate ((what (eql (find-class 'disk-page))) &rest args)
  (apply #'make-instance what args))

(defmethod allocate ((what (eql 'disk-page)) &rest args)
  (apply #'make-instance what args))



