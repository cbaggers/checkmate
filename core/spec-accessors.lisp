(in-package :checkmate)

;;------------------------------------------------------------

(defun spec-name (spec)
  (check-type spec checkmate-spec)
  (slot-value spec 'name))

(defun ttype-custom-data (type-ref)
  (with-slots (target) type-ref
    (with-slots (spec) target
      (slot-value spec 'custom-data))))

(defun spec-custom-data (spec)
  (slot-value spec 'custom-data))

;;------------------------------------------------------------
