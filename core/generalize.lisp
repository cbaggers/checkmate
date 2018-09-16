(in-package :checkmate)

;;------------------------------------------------------------

(defun generalize (function-type-ref)
  (check-type function-type-ref type-ref)
  (let ((type (deref function-type-ref)))
    (check-type type tfunction)
    (make-instance 'generalized-function-type
                   :type type)))

(defun instantiate-function-type (generalized-type)
  (check-type generalized-type generalized-function-type)
  (%copy-type (slot-value generalized-type 'type)
              (make-hash-table)))

;;------------------------------------------------------------
