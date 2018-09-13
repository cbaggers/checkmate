(in-package :checkmate)

;;------------------------------------------------------------

(defun make-check-context (type-system-designator)
  (let ((tsys (find-type-system type-system-designator)))
    (make-instance 'check-context
                   :type-system tsys)))

(defun get-function-type (context name)
  (labels ((inner (context)
             (with-slots (function-types parent) context
               (or (when function-types (gethash name function-types))
                   (when parent (inner parent))))))
    (inner context)))

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
                   :variable-bindings bindings
                   :parent context)))

;;------------------------------------------------------------
