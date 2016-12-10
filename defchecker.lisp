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
