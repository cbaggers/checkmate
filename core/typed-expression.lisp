(in-package :checkmate)

;;------------------------------------------------------------

(defun type-of-typed-expression (expression)
  (assert (and (listp expression)
               (eq (first expression) 'truly-the)
               (typep (second expression) 'type-ref))
          ()
          "The following is not a typed expression:~%~s"
          expression)
  (second expression))

;;------------------------------------------------------------
