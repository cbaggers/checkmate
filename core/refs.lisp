(in-package :checkmate)

;;------------------------------------------------------------

(defun naked-type-p (x)
  (typep x 'ttype))

(defun naked-param-p (x)
  (typep x 'ttype-parameter))

(defun naked-constraint-p (x)
  (typep x 'constraint))

(defun deref (ref)
  (slot-value ref 'target))

(defun (setf deref) (value ref)
  (setf (slot-value ref 'target) value))

(defun take-ref (type/param)
  (let ((ref
         (cond
           ((naked-type-p type/param)
            (make-instance 'type-ref :target type/param))
           ((naked-param-p type/param)
            (make-instance 'param-ref :target type/param))
           (t (error "BUG: cant take ref to ~a" type/param)))))
    (with-slots (refs) type/param
      (pushnew ref refs)
      ref)))

(defun retarget-ref (x-ref new)
  (let* ((old (deref x-ref)))
    (with-slots (refs) old
      (loop :for ref :in refs :do
           (setf (deref ref) new)
           (pushnew ref (slot-value new 'refs)))
      (setf refs nil))
    x-ref))

;;------------------------------------------------------------
