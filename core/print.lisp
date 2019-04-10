(in-package :checkmate)

;;------------------------------------------------------------

(defmethod print-object ((obj ttype) stream)
  (format stream "#<NAKED-TYPE ~a>" (type-of obj)))

(defmethod print-object ((obj ttype-parameter) stream)
  (format stream "#<NAKED-PARAMETER ~a>" (slot-value obj 'name)))

(defmethod print-object ((obj type-ref) stream)
  (force-output t)
  (let ((desig (ttype-of obj)))
    (format stream "#T~a" desig)))

(defgeneric print-type (type stream)
  (:method ((obj ttype) stream)
    (format stream "#T~a" (designator-from-type obj)))
  (:method ((obj ttype-parameter) stream)
    (if (eq (slot-value obj 'name) 'ttype)
        (print-type (deref (slot-value obj 'value)))
        (format stream "#P~a"
                (if (typep obj 'unknown-param)
                    (slot-value obj 'name)
                    (slot-value obj 'value))))))

;;------------------------------------------------------------
