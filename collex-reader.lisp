;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :dclx)


(defun delimiter-for (char)
  (ecase char
    (#\{ #\})
    (#\[ #\])
    (#\< #\>)
    (#\( #\))))


(defreadtable dclx:standard-syntax (:merge :standard)
  (:syntax-from :standard #\) #\})
  (:syntax-from :standard #\) #\])
  
  (:macro-char #\~
    ;; this is of dubious value 
    #'(lambda (stream char)
        (declare (ignore char))
        (let ((form (read stream)))
          (if (find :debugging *features*)
            (dclx:printv form)
            form))))

  (:macro-char dclx:*value-reader-macro-char*
    #'(lambda (stream char)
        (declare (ignore char))
        (value (read stream))))

  (:macro-char dclx:*set-reader-macro-char*
    #'(lambda (stream char)
        (declare (ignore char))
        (case (peek-char nil stream t nil t)
          (#\| (progn
                 (read-char stream t nil t)
                 (let ((contents (read-delimited-list #\| stream t)))
                   (if (eql (read-char stream) #\})
                     (map:make contents)
                     (error "Invalid map collection syntax.")))))
          (t    (set:make
                  (read-delimited-list
                    (delimiter-for dclx:*set-reader-macro-char*)
                    stream))))))

  (:macro-char dclx:*seq-reader-macro-char*
    #'(lambda (stream char)
        (declare (ignore char))
        (seq:make
          (read-delimited-list
            (delimiter-for dclx:*seq-reader-macro-char*)
            stream))))

  (:dispatch-macro-char #\# dclx:*set-reader-macro-char*
    #'(lambda (stream char arg)
        (declare (ignore char arg))
        (case (peek-char nil stream t nil t)
          (#\| (progn
                 (read-char stream t nil t)
                 (let ((contents (read-delimited-list #\| stream t)))
                   (if (eql (read-char stream) #\})
                     (map:make* contents)
                     (error "Invalid map collection syntax.")))))
          (t    (set:make*
                  (read-delimited-list
                    (delimiter-for dclx:*set-reader-macro-char*)
                    stream))))))

  (:dispatch-macro-char #\# dclx:*seq-reader-macro-char*
    #'(lambda (stream char arg)
        (declare (ignore char arg))
        (seq:make*
          (read-delimited-list
            (delimiter-for dclx:*seq-reader-macro-char*)
            stream)))))






