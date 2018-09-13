(in-package :checkmate)

;;------------------------------------------------------------

(defun custom-data (type-ref)
  (with-slots (target) type-ref
    (with-slots (spec) target
      (slot-value spec 'custom-data))))

;;------------------------------------------------------------
