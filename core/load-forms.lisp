(in-package :checkmate)

;;------------------------------------------------------------

(defmethod make-load-form ((obj type-system)
                           &optional environment)
  (declare (ignore environment))
  (with-slots (name infer-atom-name
                    infer-special-form-name
                    type-expander-name
                    get-type-spec-name
                    get-constraint-spec-name
                    get-parameter-spec-name
                    get-top-level-function-type-name
                    get-top-level-var-type-name)
      obj
    `(make-instance
      'type-system
      :name ',name
      :infer-atom-name ',infer-atom-name
      :infer-atom #',infer-atom-name
      :infer-special-form-name ',infer-special-form-name
      :infer-special-form #',infer-special-form-name
      :type-expander-name ',type-expander-name
      :type-expander #',type-expander-name
      :get-type-spec-name ',get-type-spec-name
      :get-type-spec #',get-type-spec-name
      :get-constraint-spec-name ',get-constraint-spec-name
      :get-constraint-spec #',get-constraint-spec-name
      :get-parameter-spec-name ',get-parameter-spec-name
      :get-parameter-spec #',get-parameter-spec-name
      :get-top-level-function-type-name ',get-top-level-function-type-name
      :get-top-level-function-type #',get-top-level-function-type-name
      :get-top-level-var-type-name ',get-top-level-var-type-name
      :get-top-level-var-type #',get-top-level-var-type-name)))

(defmethod make-load-form ((obj user-ttype-spec)
                           &optional environment)
  (declare (ignore environment))
  (with-slots (name arg-param-specs custom-data) obj
    `(make-instance 'user-ttype-spec
                    :name ',name
                    :arg-param-specs ,arg-param-specs
                    :custom-data ',custom-data)))

(defmethod make-load-form ((obj user-ttype)
                           &optional environment)
  (declare (ignore environment))
  (with-slots (spec name arg-vals) obj
    `(make-instance 'user-ttype
                    :spec ,spec
                    :name ',name
                    :arg-vals ,arg-vals)))

(defmethod make-load-form ((obj type-ref)
                           &optional environment)
  (declare (ignore environment))
  (with-slots (target) obj
    `(make-instance 'type-ref :target ,target)))

(defmethod make-load-form ((obj unknown)
                           &optional environment)
  (declare (ignore environment))
  (with-slots (constraints) obj
    `(make-instance 'unknown
                    :constraints ,(cons 'list constraints))))

(defmethod make-load-form ((obj tfunction)
                           &optional environment)
  (declare (ignore environment))
  (with-slots (arg-types return-type) obj
    `(make-instance 'tfunction
                    :arg-types ,(cons 'vector arg-types)
                    :return-type ,return-type)))

(defmethod make-load-form ((obj ttype-parameter-spec)
                           &optional environment)
  (declare (ignore environment))
  (with-slots (name equal-name valid-p-name) obj
    `(make-instance 'ttype-parameter-spec
                    :name ',name
                    :equal-name ',equal-name
                    :equal ,(when equal-name `(function ,equal-name))
                    :valid-p-name ',valid-p-name
                    :valid-p ,(when valid-p-name `(function ,valid-p-name)))))

(defmethod make-load-form ((obj ttype-parameter)
                           &optional environment)
  (declare (ignore environment))
  (with-slots (name spec value refs) obj
    (values
     `(make-instance 'ttype-parameter
                     :name ',name
                     :spec ,spec
                     :value ',value)
     `(setf (slot-value ,obj 'refs) ,(cons 'list refs)))))

(defmethod make-load-form ((obj param-ref)
                           &optional environment)
  (declare (ignore environment))
  (with-slots (target) obj
    `(make-instance 'param-ref :target ,target)))

(defmethod make-load-form ((obj unknown-param)
                           &optional environment)
  (declare (ignore environment))
  (with-slots (value) obj
    `(make-instance 'unknown-param
                    :value ',value)))

(defmethod make-load-form ((obj constraint-spec)
                           &optional environment)
  (declare (ignore environment))
  (with-slots (name satisfies-name arg-param-specs custom-data) obj
    `(make-instance
      'constraint-spec
      :name ',name
      :satisfies ,(when satisfies-name `(function ,satisfies-name))
      :satisfies-name ',satisfies-name
      :arg-param-specs ,arg-param-specs
      :custom-data ',custom-data)))

(defmethod make-load-form ((obj constraint)
                           &optional environment)
  (declare (ignore environment))
  (with-slots (spec name arg-vals) obj
    `(make-instance 'constraint
                    :spec ,spec
                    :name ',name
                    :arg-vals ,arg-vals)))

(defmethod make-load-form ((obj constraint)
                           &optional environment)
  (declare (ignore environment))
  (with-slots (spec name arg-vals) obj
    `(make-instance 'constraint
                    :name ',name
                    :spec ,spec
                    :arg-vals ,arg-vals)))

(defmethod make-load-form ((obj constraint-ref)
                           &optional environment)
  (declare (ignore environment))
  (with-slots (target designator) obj
    `(make-instance 'constraint-ref
                    :target ,target
                    :designator ',designator)))


(defmethod make-load-form ((obj generalized-function-type)
                           &optional environment)
  (declare (ignore environment))
  (with-slots (type) obj
    `(make-instance 'generalized-function-type :type ,type)))

;;------------------------------------------------------------
