;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;


;; TODO: lift dependencies to encompassing asdf system once stabilized

(unless (every #'find-package
          #1='(:cffi-objects :cffi-grovel :osicat :closer-mop :contextl :alexandria :puri
                :hu.dwim.serializer :flexi-streams :rucksack :cl-store))
  (warn "Manually quick-loading required systems: ~{~A ~}"
    (loop for sys in #1# when (not (find-package sys)) collect sys))
  (:ql #1#))


(unless (every #'identity #1=(mapcar #'find-package #2='(:io :pointer)))
  (error "required packages not found: ~{~S ~}"
     (loop with remaining = #2#
       for pkg in (remove-if #'null #1#)
       do (setf remaining (delete (package-name pkg) remaining :test #'string=))
       finally (return remaining))))


(defpackage :mmap
  (:shadow :pointer-address :incf-pointer :decf-pointer)
  (:use :closer-common-lisp :closer-mop :contextl :cffi-objects))

