(in-package :checkmate)
(in-readtable :fn.reader)

;;------------------------------------------------------------
;; My first checker

(defchecker exact-types xtype
  lisp-type)

(defmethod print-object ((fact xtype) stream)
  (with-slots (lisp-type) fact
    (format stream "<XTYPE ~a>" lisp-type)))

(defmethod infer (code (env exact-types))
  (format t "~%my infer ~s" code)
  (if (numberp code)
      (values :lisp-type 'number)
      (values :lisp-type (type-of code))))

(defmethod check-call (func-fact arg-facts (env exact-types))
  (format t "~%my check-call ~s ~{~s~^ ~}" func-fact arg-facts)
  (make-instance 'xtype))

;;------------------------------------------------------------
