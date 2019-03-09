(in-package :checkmate)

;; {TODO} disallow names to start with ~ or ?

;;------------------------------------------------------------

(defun populate-constraint (context constraint-ref named-unknowns)
  (let ((designator (slot-value constraint-ref 'designator)))
    (destructuring-bind (principle-name . args)
        (uiop:ensure-list designator)
      (declare (ignore principle-name))
      (assert (not (unknown-designator-name-p designator)) ()
              "Constraint cannot be unknown")
      (with-slots (get-constraint-spec)
          (slot-value context 'type-system)
        (let ((spec (or (funcall get-constraint-spec
                                 context
                                 designator)
                        (error "Could not infer type of constraint ~a"
                               designator))))
          (setf (deref constraint-ref)
                (to-constraint context
                               spec
                               named-unknowns
                               args)))))))

(defun late-initialize-constraint-spec (spec)
  (unless (slot-boundp spec 'satisfies)
    (setf (slot-value spec 'satisfies)
          (symbol-function (slot-value spec 'satisfies-name)))))

(defun to-constraint (context spec named-unknowns args)
  (late-initialize-constraint-spec spec)
  (with-slots (arg-param-specs) spec
    (let* ((constructed
            (construct-designator-args context
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

(defun make-constraint-spec (context
                             designator
                             where
                             satisfies-this-p
                             custom-spec-data)
  (assert (symbolp satisfies-this-p))
  (destructuring-bind (name . designator-args)
      (uiop:ensure-list designator)
    (assert (every #'unknown-designator-name-p designator-args))
    (let* ((req-args
            (parse-ttype-lambda-list designator-args))
           (params
            (loop
               :for arg :in req-args
               :collect (%get-parameter-spec
                         context
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
  (check-type type-ref type-ref)
  (when constraints
    (let ((type (deref type-ref)))
      (labels ((unifies-with-constraint (constraint)
                 (with-slots (satisfies)
                     (slot-value (deref constraint) 'spec)
                   (unless (typep type 'unknown)
                     (handler-case
                         (funcall satisfies constraint type-ref)
                       (error () nil))))))
        (let ((failed
               (loop
                  :for constraint :in constraints
                  :unless (unifies-with-constraint constraint)
                  :collect (slot-value (deref constraint) 'name))))
          (when failed
            (make-instance 'failed-to-satisfy-constraints
                           :type-ref type-ref
                           :failed failed)))))))

;;------------------------------------------------------------
