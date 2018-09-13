(in-package :checkmate)

;;------------------------------------------------------------

(unless (boundp '*ttype-param-spec*)
  (setf *ttype-param-spec*
        (make-instance 'ttype-parameter-spec
                       :name 'ttype
                       :unify #'unify)))

;;------------------------------------------------------------
