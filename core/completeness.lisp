(in-package :checkmate)

;;------------------------------------------------------------

(defgeneric complete-p (type)
  (:method ((type unknown))
    (declare (ignore type))
    nil)
  (:method ((type unknown-param))
    (declare (ignore type))
    nil)
  (:method ((param ttype-parameter))
    (declare (ignore param))
    t)
  (:method ((type ttype))
    (with-slots (known-complete) type
      (or known-complete
          (let ((complete (check-type-complete type)))
            (when complete
              (setf known-complete t))
            complete))))
  (:method ((type-ref type-ref))
    (complete-p (deref type-ref))))

(defun check-type-complete (type)
  (etypecase type
    (unknown nil)
    (unknown-param nil)
    (tfunction
     (with-slots (arg-types return-type) type
       (and (every (lambda (x) (complete-p (deref x))) arg-types)
            (complete-p (deref return-type)))))
    (user-ttype
     (let ((arg-vals (slot-value type 'arg-vals)))
       (assert (> (length arg-vals) 0) ()
               "BUG: this type should have been known-complete ~a"
               type)
       (every (lambda (x) (complete-p (deref x)))
              arg-vals)))
    (ttype-parameter t) ;; type params become type-refs not param-refs
    (ttype t)))

;;------------------------------------------------------------
