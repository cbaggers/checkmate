(in-package :checkmate)

;;------------------------------------------------------------

(declaim (inline type-system-designator->context))
(defun type-system-designator->context (type-system-designator)
  (etypecase type-system-designator
    (check-context
     type-system-designator)
    (type-system
     (make-check-context type-system-designator))
    (symbol
     (make-check-context
      (find-type-system type-system-designator)))))

;;------------------------------------------------------------

(defun make-check-context (type-system-designator)
  (let ((tsys
         (etypecase type-system-designator
           (symbol (find-type-system type-system-designator))
           (type-system type-system-designator))))
    (make-instance 'check-context
                   :type-system tsys)))

(defun get-function-type (context name arg-types-provided-p arg-types)
  (labels ((find-in-context (context)
             (with-slots (function-types parent type-system) context
               (or (when function-types (gethash name function-types))
                   (when parent (find-in-context parent))
                   (with-slots (get-top-level-function-type)
                       type-system
                     (funcall get-top-level-function-type
                              context
                              name
                              arg-types-provided-p
                              arg-types))
                   (error "Could not get function type for ~a"
                          name)))))
    (let ((ftype (find-in-context context)))
      (etypecase ftype
        (generalized-function-type
         (instantiate-function-type ftype))
        (tfunction ftype)))))

(defun get-binding (context name)
  (labels ((inner (context)
             (with-slots (variable-bindings parent) context
               (or (when variable-bindings (gethash name variable-bindings))
                   (when parent (inner parent))))))
    (inner context)))

(defun add-binding (context name type)
  (check-type type type-ref)
  (let ((bindings (make-hash-table)))
    (setf (gethash name bindings) type)
    (make-instance 'check-context
                   :type-system (slot-value context 'type-system)
                   :variable-bindings bindings
                   :parent context)))

(defun add-bindings (context name-type-pairs)
  (let ((bindings (make-hash-table)))
    (loop
       :for (name type) :in name-type-pairs
       :do
         (assert (typep type 'type-ref))
         (setf (gethash name bindings) type))
    (make-instance 'check-context
                   :type-system (slot-value context 'type-system)
                   :variable-bindings bindings
                   :parent context)))

;;------------------------------------------------------------
