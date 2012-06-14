;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

;;;
;;; For the moment this is implemented via  an indirection in order to evaluate the various
;;; backend alternatives as well as to permit selection of backend on a class by
;;; class basis, since there is considerable difference represented among these options
;;; with regards to depth, completeness, versatility, and speed.  For example,
;;; for serialization of  PACKAGE, Rucksack signals an error, DWIM serializes the
;;; string (package-name PACKAGE) and cl-store serializes a complete representation
;;; including exports, imports, and the whole nine yards. The way I see it at the moment,
;;; the basic functionality matrix out-of-the box, without further customizations or
;;; enhancement is:
;;;
;;;           BACKEND  | DEPTH | COMPLETENESS     | SPEED
;;;          --------------------------------------------
;;;           cl-store | :full | 10 (everything)  | :slow
;;;           hu.dwim  | :full | 8  (most things) | :med
;;;           rucksack |   1   | 6  (many things) | :med-fast
;;;           userial  |   0   | 3  (few things)  | :fast
;;;
;;; I think the ideal solution is to spend some time with userial and put together exactly
;;; the right thing, but that will have to wait until other priorities are addressed, as
;;; the capabilities among the other options seem to offer a reasonable 90% solution, and
;;; the ideal hand-tooled userial solution can be snapped in at any time with 2 lines of
;;; code and altogether invisibly to the higher-level code depending on the uniform
;;; serialize/deserialize API provided.
;;;


(in-package :mmap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface to various serializer backends
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +dwim+           0)
(defconstant +rucksack+       1)
(defconstant +clstore+        2)
(defconstant +userial+        3)


(defun serialize-dwim (thing &rest args)
  (apply #'hu.dwim.serializer:serialize thing args))

(defun deserialize-dwim (vector &rest args)
  (apply #'hu.dwim.serializer:deserialize vector args))

(defun serialize-rucksack (thing)
  (flex:with-output-to-sequence (out)
    (rs::serialize thing (make-instance 'rs::serializer :stream out))))

(defun deserialize-rucksack (vector)
  (flex:with-input-from-sequence (in vector)
    (rs::deserialize (make-instance 'rs::serializer :stream in))))

(defun serialize-clstore (thing)
  (flex:with-output-to-sequence (out)
    (cl-store:store thing out)))

(defun deserialize-clstore (vector)
  (flex:with-input-from-sequence (in vector)
    (cl-store:restore in)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Uniform API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun serialize-using (backend thing)
  (serialize-dwim (cons backend (ecase backend
                                  (+dwim+     (serialize-dwim     thing))
                                  (+rucksack+ (serialize-rucksack thing))
                                  (+cl-store+ (serialize-clstore  thing))))))

(defgeneric serialize (thing)
  (:method ((thing t))
    (serialize-using +dwim+ thing)))

(defgeneric deserialize (thing)
  (:method ((thing vector))
    (destructuring-bind (backend vector) (deserialize-dwim thing)
      (ecase backend
        (+dwim+     (deserialize-dwim     vector))
        (+rucksack+ (deserialize-rucksack vector))
        (+cl-store+ (deserialize-clstore  vector))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc Utility
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(with-output-to-string (s)
  (defun set-hu-serializer-debug-logger (&optional (new-value s))
    (prog1 new-value
      (warn "manually setting serializer debug log; New value is ~A"
        (setf hu.dwim.serializer::*DEBUG-LOG-ENABLED* new-value))))
  (defun get-hu-serializer-debug-stream ()
    s)
  (defun get-hu-serializer-logs ()
    (prog1 (get-output-stream-string s)
      (with-output-to-string (new-s)
        (setf s new-s)
        (set-hu-serializer-debug-logger new-s)))))
  


#|
(defun time-serialize/deserialize (thing)
  (let* ((prior-debug hu.dwim.serializer::*debug-log-enabled*)
         (result (list
                   (time
                     
                    (ignore-errors (warn "testing rucksack")
                      (flex:with-input-from-sequence (in (flex:with-output-to-sequence (s)
                                                           (rs::serialize thing
                                                             (make-instance 'rs::serializer
                                                               :stream s))))
                        (rs::deserialize (make-instance 'rs::serializer :stream in)))))
                   (time
                    
                     (ignore-errors  (warn "testing hu.dwim")
                       (unwind-protect (progn
                                         (setf hu.dwim.serializer::*debug-log-enabled* nil)
                                         (hu.dwim.serializer:deserialize
                                           (hu.dwim.serializer:serialize thing)))
                         (setf hu.dwim.serializer::*debug-log-enabled* prior-debug))))
                   (time
                    
                    (ignore-errors  (warn "testing cl-store")
                      (flex:with-input-from-sequence (in (flex:with-output-to-sequence (s)
                                                           (cl-store:store thing s)))
                        (cl-store:restore in)))))))
    (values result (mapcar (lambda (x) (equalp x thing)) result))
    ))

(defun describe-all (things)
  (mapc #'describe (alexandria:ensure-list things)))

|#
