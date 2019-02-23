(in-package :checkmate)

;;------------------------------------------------------------

(defun unifies-p (type-a type-b)
  (handler-case
      (let ((type-a (copy-type type-a))
            (type-b (copy-type type-b)))
        (unify type-a type-b)
        t)
    (error () nil)))

(defun unify (type-a type-b)
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
              (unify-user-type a b))
         t)
        ((and (typep a 'tfunction) (typep b 'tfunction))
         (map 'list
              (lambda (x y) (unify x y))
              (slot-value a 'arg-types)
              (slot-value b 'arg-types))
         (unify (slot-value a 'return-type)
                (slot-value b 'return-type)))
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
              (retarget-ref type-a b))
             (b-unknown
              (check-constraints type-a b-constraints)
              (retarget-ref type-b a))
             (t (error "No way to unify ~a and ~a" type-a type-b))))))))
  (values))

;;------------------------------------------------------------

(defun unify-user-type (a b)
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
               (unify aparam bparam))
             (unify-params aparam bparam)))
  t)

;;------------------------------------------------------------

(defun unify-params (param-a param-b)
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
       (retarget-ref param-a b))
      (b-unknown
       (retarget-ref param-b a))
      (t (error "No way to unify params:~%~a: ~a~%~a: ~a"
                (slot-value a 'name)
                (slot-value a 'value)
                (slot-value b 'name)
                (slot-value b 'value)))))
  (values))

;;------------------------------------------------------------
