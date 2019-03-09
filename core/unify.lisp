(in-package :checkmate)

;;------------------------------------------------------------

(defun unifies-p (type-a type-b)
  (let* ((type-a (copy-type type-a))
         (type-b (copy-type type-b))
         (err (unify type-a type-b)))
    (not err)))

;;------------------------------------------------------------

;; The rest of the function in this file return nil is unification
;; is successfull and return an instance of an error object if it
;; failed

(defun unify (type-a type-b)
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
                  (slot-value b 'name)))
         (unify-user-type a b))
        ((and (typep a 'tfunction) (typep b 'tfunction))
         (let ((args-a (slot-value a 'arg-types))
               (args-b (slot-value b 'arg-types)))
           (if (= (length args-a) (length args-b))
               (loop
                  :for x :across args-a
                  :for y :across args-b
                  :for r := (unify x y)
                  :when r :do (return r)
                  :finally (return
                             (unify (slot-value a 'return-type)
                                    (slot-value b 'return-type))))
               (make-instance 'cannot-unify-types
                              :type-a type-a
                              :type-b type-b))))
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
                (retarget-ref type-b new)
                nil))
             (a-unknown
              (or (check-constraints type-b a-constraints)
                  (progn
                    (retarget-ref type-a b)
                    nil)))
             (b-unknown
              (or (check-constraints type-a b-constraints)
                  (progn
                    (retarget-ref type-b a)
                    nil)))
             (t (make-instance 'cannot-unify-types
                               :type-a type-a
                               :type-b type-b)))))))))

;;------------------------------------------------------------

(defun unify-user-type (a b)
  ;; Assumes called has already checked 'name's match
  (loop
     :for aparam :across (slot-value a 'arg-vals)
     :for bparam :across (slot-value b 'arg-vals)
     :for a-is-type := (typep aparam 'type-ref)
     :for b-is-type := (typep bparam 'type-ref)
     :for r := (if (or a-is-type b-is-type)
                   (if (and a-is-type b-is-type)
                       (unify aparam bparam)
                       (make-instance 'cannot-unify-types
                                      :type-a aparam
                                      :type-b bparam))
                   (unify-params aparam bparam))
       :when r :do (return r)))

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
       nil)
      (a-unknown
       (retarget-ref param-a b)
       nil)
      (b-unknown
       (retarget-ref param-b a)
       nil)
      (t (make-instance 'cannot-unify-params
                        :a-name (slot-value a 'name)
                        :a-val (slot-value a 'value)
                        :b-name (slot-value b 'name)
                        :b-val (slot-value b 'value)))))
  (values))

;;------------------------------------------------------------
