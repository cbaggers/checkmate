(in-package :checkmate)

;;------------------------------------------------------------

(defgeneric print-type (type stream))

(defmethod print-object ((obj ttype) stream)
  (format stream "#<NAKED-TYPE ~a>" (type-of obj)))

(defmethod print-object ((obj ttype-parameter) stream)
  (format stream "#<NAKED-PARAMETER ~a>" (slot-value obj 'name)))

(defmethod print-object ((obj type-ref) stream)
  (print-type (deref obj) stream))

(defmethod print-type ((obj ttype) stream)
  (format stream "#T~a"
          (etypecase obj
            (unknown
             (slot-value obj 'name))
            (tfunction
             (designator-from-type obj))
            (user-ttype
             (designator-from-type obj)))))

(defmethod print-type ((obj ttype-parameter) stream)
  (if (eq (slot-value obj 'name) 'ttype)
      (print-type (deref (slot-value obj 'value)))
      (format stream "#P~a"
              (if (typep obj 'unknown-param)
                  (slot-value obj 'name)
                  (slot-value obj 'value)))))

;;------------------------------------------------------------
