(in-package :checkmate)

;;------------------------------------------------------------

(unless (boundp '*ttype-param-spec*)
  (setf *ttype-param-spec*
        (make-instance 'ttype-parameter-spec
                       :name 'ttype
                       :equal-name nil
                       :equal nil
                       :valid-p-name nil
                       :valid-p nil)))

;;------------------------------------------------------------
