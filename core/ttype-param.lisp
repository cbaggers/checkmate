(in-package :checkmate)

;;------------------------------------------------------------

(defun ttype-designator-to-param (spec val)
  (declare (ignore spec))
  (designator->type val))

(register-parameter-type
 (make-instance 'ttype-parameter-spec
                :name 'ttype
                :unify #'unify
                :to-param 'ttype-designator-to-param))

;;------------------------------------------------------------
