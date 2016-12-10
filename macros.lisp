(in-package :checkmate)

;;------------------------------------------------------------

(defparameter *known-macros* nil)

(defmacro def-static-macro (name args &body body)
  (let ((args (replace-all args '&body '&rest)))
    `(pushnew (cons ',name (lambda ,args ,@body))
              *known-macros*)))

(defun static-macro-name-p (name)
  (not (null (assoc name *known-macros*))))

(defmethod expand-static-macro (name code)
  (let ((f (cdr (assoc name *known-macros*))))
    (apply f (rest code))))

;;------------------------------------------------------------

(def-static-macro let* (bindings &body body)
  (cond
    ((> (length bindings) 1)
     `(let (,(first bindings))
        (let* ,(rest bindings)
          ,@body)))
    ((= (length bindings) 1)
     `(let ,bindings
        ,@body))
    (t `(progn ,@body))))

;;------------------------------------------------------------
