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

(defvar *ignored* nil)

(defun make-known-to-ignore (name)
  (pushnew name *ignored*)
  (remove name *known-macros*))

(defun checkmate-ignored-p (name)
  (not (null (member name *ignored*))))

;;------------------------------------------------------------

(make-known-to-ignore 'progn)
(make-known-to-ignore 'let)

(def-static-macro let* (bindings &body body)
  (if bindings
      `(let (,(first bindings))
         (let* ,(rest bindings)
           ,@body))
      `(progn ,@body)))

;;------------------------------------------------------------
