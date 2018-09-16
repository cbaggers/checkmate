(in-package :checkmate)

;;------------------------------------------------------------

(defun populate-constraint (type-system constraint-ref named-unknowns)
  (let ((designator (slot-value constraint-ref 'designator)))
    (destructuring-bind (principle-name . args)
        (uiop:ensure-list designator)
      (declare (ignore principle-name))
      (assert (not (unknown-designator-name-p designator)) ()
              "Constraint cannot be unknown")
      (let ((spec (get-constraint-spec type-system designator)))
        (setf (deref constraint-ref)
              (to-constraint type-system
                             spec
                             named-unknowns
                             args))))))

(defun late-initialize-constraint-spec (spec)
  (unless (slot-boundp spec 'satisfies)
    (setf (slot-value spec 'satisfies)
          (symbol-function (slot-value spec 'satisfies-name)))))

(defun to-constraint (type-system spec named-unknowns args)
  (late-initialize-constraint-spec spec)
  (with-slots (arg-param-specs) spec
    (let* ((constructed
            (construct-designator-args type-system
                                       spec
                                       named-unknowns
                                       nil
                                       args))
           (vals
            (make-array
             (length constructed)
             :initial-contents constructed)))
      (make-instance 'constraint
                     :spec spec
                     :name (slot-value spec 'name)
                     :arg-vals vals))))

(defun make-constraint-spec (type-system
                             designator
                             where
                             satisfies-this-p
                             custom-spec-data)
  (assert (symbolp satisfies-this-p))
  (destructuring-bind (name . designator-args)
      (uiop:ensure-list designator)
    (let* ((req-args
            (parse-ttype-lambda-list designator-args))
           (params
            (loop
               :for arg :in req-args
               :collect (%get-parameter-spec
                         type-system
                         (or (second (find arg where :key #'first))
                             'ttype)))))
      (make-instance
       'constraint-spec
       :name name
       :satisfies-name satisfies-this-p
       :custom-data custom-spec-data
       :arg-param-specs (make-array (length params)
                                    :initial-contents params)))))

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
                  :collect (slot-value (deref constraint) 'name))))
          (when failed
            (error "Type ~a failed to satisfy the following constraints:~%~{~a~}"
                   type-ref failed))))))
  t)

;;------------------------------------------------------------
