(in-package :checkmate)

;;------------------------------------------------------------

(defun populate-constraint (constraint-ref named-unknowns)
  (let ((designator (slot-value constraint-ref 'designator)))
    (destructuring-bind (principle-name . args)
        (uiop:ensure-list designator)
      (assert (not (unknown-designator-name-p designator)) ()
              "Constraint cannot be unknown")
      (let ((spec (get-constraint-spec designator)))
        (setf (deref constraint-ref)
              (funcall (slot-value spec 'desig-to-constraint)
                       spec
                       named-unknowns
                       args))))))

(defmacro define-constraint (designator
                             &body rest
                             &key where satifies-this-p
                               custom-spec-data)
  (declare (ignore rest))
  (destructuring-bind (name . designator-args)
      (uiop:ensure-list designator)
    (destructuring-bind (req-args key-forms)
        (parse-ttype-lambda-list designator-args)
      (let* ((req-len (length req-args))
             (key-len (length key-forms))
             (args-len (+ req-len key-len))
             (key-args (mapcar #'first key-forms))
             (args (append req-args key-args))
             (where (loop :for arg :in args
                       :collect (or (find arg where :key #'first)
                                    (list arg 'ttype))))
             (arg-param-types (mapcar #'second where)))
        (alexandria:with-gensyms (gconstraint-spec)
          `(let ((satisfies ,satifies-this-p))
             (labels ((destructure-args (args)
                        (destructuring-bind (,@req-args &key ,@key-forms)
                            args
                          (list ,@req-args ,@key-args)))
                      (to-constraint (,gconstraint-spec
                                      named-unknowns
                                      args)
                        (assert (eq (slot-value ,gconstraint-spec 'name)
                                    ',name))
                        (let ((args (destructure-args args)))
                          (let ((vals (make-array
                                       ,args-len
                                       :initial-contents
                                       (construct-designator-args ,gconstraint-spec
                                                                 named-unknowns
                                                                 nil
                                                                 args))))
                            (make-instance 'constraint
                                           :spec ,gconstraint-spec
                                           :name ',name
                                           :arg-vals vals)))))
               (register-constraint
                (let ((arg-param-specs
                       (make-array ,(length arg-param-types)
                                   :initial-contents
                                   (mapcar #'get-parameter-type-spec
                                           ',arg-param-types))))
                  (make-instance 'constraint-spec
                                 :name ',name
                                 :satisfies satisfies
                                 :arg-param-specs arg-param-specs
                                 :desig-to-constraint #'to-constraint
                                 :custom-data ',custom-spec-data)))
               ',name)))))))

;;------------------------------------------------------------

(defun check-constraints (type-ref constraints)
  ;; We can safely use unify here as we tell it not to mutate the types.
  ;; This means we check everything can work but we dont allow any
  ;; modification to a type's references.
  (check-type type-ref type-ref)
  (when constraints
    (let ((type (deref type-ref)))
      (labels ((unifies-with-constraint (constraint)
                 (with-slots (satisfies)
                     (slot-value (deref constraint) 'spec)
                   (unless(typep type 'unknown)
                     (handler-case
                         (funcall satisfies constraint type-ref)
                       (error () nil))))))
        (let ((failed
               (loop
                  :for constraint :in constraints
                  :unless (unifies-with-constraint constraint)
                  :collect (slot-value constraint 'name))))
          (when failed
            (error "Type ~a failed to satisfy the following constraints:~%~{~a~}"
                   type-ref failed))))))
  t)

;;------------------------------------------------------------
