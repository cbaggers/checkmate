(in-package :checkmate)

;;------------------------------------------------------------

(defun unify (type-a type-b mutate-p)
  ;; The only case when mutate-p is nil is when you are trying
  ;; to check constraints as there you dont want the type to aquire
  ;; information from the type
  (check-type type-a type-ref)
  (check-type type-b type-ref)
  (let* ((a (deref type-a))
         (b (deref type-b))
         (a-is-user-type-p (typep a 'user-ttype))
         (b-is-user-type-p (typep b 'user-ttype)))
    (unless (eq type-a type-b)
      (cond
        ((and a-is-user-type-p
              b-is-user-type-p
              (eq (slot-value a 'name)
                  (slot-value b 'name))
              (unify-user-type type-a type-b mutate-p))
         t)
        ((and (typep a 'tfunction) (typep b 'tfunction))
         (mapcar (lambda (x y) (unify x y mutate-p))
                 (slot-value a 'arg-types)
                 (slot-value b 'arg-types))
         (unify (slot-value a 'return-type)
                (slot-value b 'return-type)
                mutate-p))
        (t
         (let* ((a-unknown (typep a 'unknown))
                (b-unknown (typep b 'unknown))
                (a-constraints
                 (when a-unknown
                   (slot-value a 'constraints)))
                (b-constraints
                 (when b-unknown
                   (slot-value b 'constraints))))
           (cond
             ((and a-unknown b-unknown)
              (let ((new (make-naked-unknown
                          (append a-constraints
                                  b-constraints))))
                (retarget-ref type-a new)
                (retarget-ref type-b new)))
             (a-unknown
              (check-constraints type-b a-constraints)
              (when mutate-p
                (retarget-ref type-a b)))
             (b-unknown
              (check-constraints type-a b-constraints)
              (when mutate-p
                (retarget-ref type-b a)))
             (t (error "No way to unify ~a and ~a" type-a type-b))))))))
  (values))

;;------------------------------------------------------------

(defun unify-user-type (type-a type-b mutate-p)
  (let* ((a (deref type-a))
         (b (deref type-b)))
    (assert (eq (slot-value a 'name)
                (slot-value b 'name)))
    (loop
       :for aparam :across (slot-value a 'arg-vals)
       :for bparam :across (slot-value b 'arg-vals)
       :for a-is-type := (typep aparam 'type-ref)
       :for b-is-type := (typep bparam 'type-ref)
       :do (if (or a-is-type b-is-type)
               (progn
                 (assert (and a-is-type b-is-type))
                 (unify aparam bparam mutate-p))
               (unify-params aparam bparam t)))
    t))

;;------------------------------------------------------------

(defun unify-params (param-a param-b mutate-p)
  (check-type param-a param-ref)
  (check-type param-b param-ref)
  (let* ((a (deref param-a))
         (b (deref param-b))
         (a-unknown (typep a 'unknown-param))
         (b-unknown (typep b 'unknown-param))
         (name-a (slot-value a 'name))
         (name-b (slot-value b 'name))
         (primary-name-matches (eq name-a name-b)))
    (cond
      ((and primary-name-matches
            (funcall (slot-value (slot-value a 'spec) 'equal)
                     (slot-value a 'value)
                     (slot-value b 'value)))
       t)
      (a-unknown
       (when mutate-p
         (retarget-ref param-a b)))
      (b-unknown
       (when mutate-p
         (retarget-ref param-b a)))
      (t (error "No way to unify params:~%~a: ~a~%~a: ~a"
                (slot-value a 'name)
                (slot-value a 'value)
                (slot-value b 'name)
                (slot-value b 'value)))))
  (values))

;;------------------------------------------------------------
