(in-package :checkmate)

;;------------------------------------------------------------

(defun generalize (function-type-ref)
  (check-type function-type-ref type-ref)
  (let ((type (deref function-type-ref)))
    (check-type type tfunction)
    (make-instance 'generalized-function-type
                   :type type)))

(defun instantiate-function-type (generalized-type &key named-unknowns)
  (check-type generalized-type generalized-function-type)
  (check-type named-unknowns (or null hash-table))
  (let ((unknowns (or named-unknowns (make-hash-table))))
    (%copy-type (slot-value generalized-type 'type)
                unknowns)))

;;------------------------------------------------------------
