(in-package :checkmate)
(in-readtable :fn.reader)

;;------------------------------------------------------------

(defmacro defchecker (name fact-type-name &body fact-slots)
  (assert (every #'symbolp fact-slots))
  (gen-checker name fact-type-name fact-slots))

(defun gen-checker (name fact-type-name fact-slots)
  (let ((slot-keys (mapcar λ(intern (symbol-name _) :keyword)
                           fact-slots)))
    `(progn
       (defclass ,name (check-environment) ())
       (defclass ,fact-type-name (fact)
         ,(mapcar λ`(,_ :initform nil :initarg ,_1)
                  fact-slots slot-keys))
       (defmethod %fact-for ((env ,name) &key ,@fact-slots)
         (make-instance
          ',fact-type-name
          ,@(mapcan #'list slot-keys fact-slots)))
       (setf (gethash ',name *env-roots*) (make-instance ',name)))))

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
